CREATE TABLE osm_roads (
  osm_id integer PRIMARY KEY
, road_type text NOT NULL
, road_name text NULL
, layer integer NOT NULL
, oneway boolean NOT NULL
, geom geometry(LINESTRING, 4326) NOT NULL
)
