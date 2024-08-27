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
