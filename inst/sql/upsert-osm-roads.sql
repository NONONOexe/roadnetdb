INSERT INTO osm_roads
SELECT * FROM temp_roads
ON CONFLICT ON CONSTRAINT osm_roads_pkey
DO UPDATE SET
  road_type = EXCLUDED.road_type,
  road_name = EXCLUDED.road_name,
  layer = EXCLUDED.layer,
  oneway = EXCLUDED.oneway,
  geom = EXCLUDED.geom
