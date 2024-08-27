create_index_geom <- function(
    conn, table_name, index_name = paste0(table_name, "_geom_idx")) {
  sql <- glue_sql("
      CREATE INDEX {`index_name`}
      ON {`table_name`}
      USING GIST(geom)
    ", .con = conn)
  dbExecute(conn, sql)
}

create_table_edges <- function(conn, table_name, parent_table, parent_id) {
  sql <- glue_sql("
      CREATE TABLE {`table_name`} (
        id integer PRIMARY KEY
      , source integer
      , target integer
      , oneway boolean
      , length double precision
      , parent integer NOT NULL
      , geom geometry(LINESTRING, 4326) NOT NULL
      , FOREIGN KEY ( parent ) REFERENCES {`parent_table`} ( {`parent_id`} )
      )
    ", .con = conn)
  dbExecute(conn, sql)
}

create_table_nodes <- function(conn, table_name) {
  sql <- glue_sql("
      CREATE TABLE {`table_name`} (
        id integer PRIMARY KEY
      , geom geometry(POINT, 4326) NOT NULL
      )
    ", .con = conn)
  dbExecute(conn, sql)
}

.create_table_osm_roads <- function(conn) {
  sql <- "
      CREATE TABLE osm_roads (
        osm_id integer PRIMARY KEY
      , road_type text NOT NULL
      , road_name text NULL
      , layer integer NOT NULL
      , oneway boolean NOT NULL
      , geom geometry(LINESTRING, 4326) NOT NULL
      )
    "
  dbExecute(conn, sql)
}

create_table_temp_edges <- function(conn) {
  sql <- "
      CREATE TABLE temp_edges (
        id integer PRIMARY KEY
      , source integer
      , target integer
      , oneway boolean
      , parent integer NOT NULL
      , geom geometry(LINESTRING, 3857) NOT NULL
      )
    "
  dbExecute(conn, sql)
}

create_table_temp_roads <- function(conn) {
  sql <- "
      CREATE TEMPORARY TABLE temp_roads (
        LIKE osm_roads
      )
    "
  dbExecute(conn, sql)
}

select_osm_roads <- function(conn) {
  sql <- "
      SELECT
        osm_id
      , road_type
      , road_name
      , layer
      , oneway
      , geom
      FROM
        osm_roads
      WHERE
        ST_Intersects(geom, (SELECT geom FROM temp_area))
    "
  dbGetQuery(conn, sql)
}

insert_nodes <- function(conn, target) {
  sql <- glue_sql("
      INSERT INTO {`target`} (id, geom)
      SELECT
        id
      , ST_Transform(the_geom, 4326)
      FROM
        temp_edges_vertices_pgr
    ", .con = conn)
  dbExecute(conn, sql)
}

insert_edges <- function(conn, target) {
  sql <- glue_sql("
      INSERT INTO {`target`} (id, source, target, oneway, parent, length, geom)
      SELECT
        id
      , source
      , target
      , oneway
      , parent
      , ST_Length(geom)
      , ST_Transform(geom, 4326)
      FROM
        temp_edges
    ", .con = conn)
  dbExecute(conn, sql)
}

generate_road_edges <- function(conn) {
  sql <- "
      INSERT INTO temp_edges (id, oneway, parent, geom)
      WITH split_roads AS (
        SELECT
          osm_id
        , (ST_Dump(ST_Split(geom, ST_Union(split_point)))).geom
        FROM
        osm_roads r1
        LEFT JOIN LATERAL (
          SELECT
            (ST_Dump(ST_Intersection(r1.geom, r2.geom))).geom AS split_point
          FROM
            osm_roads r2
          WHERE
            r1.osm_id <> r2.osm_id
            AND ST_Intersects(r1.geom, r2.geom)
            AND r1.layer = r2.layer
        ) sp
        ON ST_GeometryType(split_point) = 'ST_Point'
        AND NOT ST_Equals(ST_StartPoint(geom), split_point)
        AND NOT ST_Equals(ST_ENDPoint(geom), split_point)
        AND ST_Intersects(geom, (SELECT geom FROM temp_area))
        GROUP BY
          osm_id
        , geom
      )
      SELECT
        ROW_NUMBER() OVER (ORDER BY r1.osm_id)
      , oneway
      , r1.osm_id
      , ST_Transform(COALESCE(r2.geom, r1.geom), 3857) AS geom
      FROM
        osm_roads AS r1
        LEFT JOIN split_roads AS r2
        ON r1.osm_id = r2.osm_id
      WHERE
        ST_Intersects(r1.geom, (SELECT geom FROM temp_area))
    "
  dbExecute(conn, sql)
}

generate_segment_edges <- function(conn, source, length) {
  sql <- glue_sql("
      INSERT INTO temp_edges (id, oneway, parent, geom)
      SELECT
        ROW_NUMBER() OVER (ORDER BY id)
      , oneway
      , id
      , ST_LineSubstring(geom, startfrac, LEAST( endfrac, 1 ))
      FROM
        (
      	  SELECT
      	    id
      	  , oneway
      		, ST_Length(ST_Transform(geom, 3857)) AS len
      		, {length} AS sublen
      		, ST_Transform(geom, 3857) AS geom
      	  FROM
      		  {`source`}
        ) AS base CROSS JOIN LATERAL (
      	  SELECT
      		  i
      		, (sublen * i) / len AS startfrac
      		, (sublen * (i + 1)) / len AS endfrac
      	  FROM
      		  generate_series(0, floor( len / sublen )::integer) AS t(i)
      	  WHERE
      		  (sublen * i) / len <> 1.0
        ) AS subs
    ", .con = conn)
  dbExecute(conn, sql)
}

create_topology <- function(conn) {
  sql <- "
      SELECT
        pgr_createTopology('temp_edges', 1, 'geom')
    "
  suppressMessages(dbExecute(conn, sql))
}

upsert_osm_roads <- function(conn) {
  sql <- "
      INSERT INTO osm_roads
      SELECT * FROM temp_roads
      ON CONFLICT ON CONSTRAINT osm_roads_pkey
      DO UPDATE SET
        road_type = EXCLUDED.road_type,
        road_name = EXCLUDED.road_name,
        layer = EXCLUDED.layer,
        oneway = EXCLUDED.oneway,
        geom = EXCLUDED.geom
    "
  dbExecute(conn, sql)
}
