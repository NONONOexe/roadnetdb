CREATE TABLE temp_edges (
  id integer PRIMARY KEY
, source integer
, target integer
, oneway boolean
, parent integer NOT NULL
, geom geometry(LINESTRING, 3857) NOT NULL
)
