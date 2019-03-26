
--Postgre SQL

-- between clause is inclusive
BETWEEN…AND… 



-- concatenation operator
SELECT 'Current time is ' ||  now()
-- Current time is 2018-11-28 20:57:53.024957+01



-- like
-- LIKE: takes into account letter case
-- ILIKE: doesn't 


------------------------------------------------
-- Date and Time


-- Select at Time Zone
SELECT
  signup_timestamp AT TIME ZONE 'America/Chicago'
FROM dim_client

-- casting between date and timestamp
SELECT
  '2016-05-14 23:49:23'::date,  -- '2016-05-14'
  '2016-03-14'::timestamp  -- '2016-03-14 00:00:00'



 -- Time Interval
SELECT '2015-02-15 01:09'::timestamp + interval '7 days'
-- interval keywords: day, days, week, weeks, month, year

-- PostgreSQL: 1 month as calendar month
-- Vertica: 1 month = 30 days

-- Bounds of Intervals
WHERE timestamp BETWEEN '2012-01-01'::timestamp
  and '2012-01-01'::date + interval '1 month'
-- selects from 2012-01-01 00:00:00 to 2012-02-01 00:00:00 (postgreSQL)
WHERE timestamp BETWEEN '2012-01-01'
  and '2012-01-31'
-- selects from 2012-01-01 00:00:00 to 2012-01-31 00:00:00 (postgreSQL)



-- now()
WHERE timestamp BETWEEN now() – interval '7 days' AND now()

-- datediff()
SELECT datediff('day', '2016-01-01', '2016-01-05') -- 4


-- date_trunc()
SELECT date_trunc('hour', '2010-04-20 18:36:43'::timestamp). -- '2010-04-20 18:00:00'
     , date_trunc('quarter', '2010-04-20 18:36:43'::timestamp). --'2010-04-01 00:00:00'
-- possible keywords: second, minute, hour, day, week, onth, quarter, year




-- group by 1 : by the first column
SELECT
  date_trunc('week', begintrip_timestamp_local) AS week,
  count(uuid)
FROM fact_trip
GROUP BY 1
ORDER BY 1


--------------------------------------------------------------------
-- window functions

-- rank
SELECT city_name
FROM (
SELECT
  RANK() over (order by launch_date) as rank,
  city_name
FROM dim_city) AS a


-- group average
SELECT depname, empno, salary, avg(salary) OVER (PARTITION BY depname) FROM empsalary
--   depname  | empno | salary |          avg          
-- -----------+-------+--------+-----------------------
--  develop   |    11 |   5200 | 5020.0000000000000000
--  develop   |     7 |   4200 | 5020.0000000000000000
--  personnel |     5 |   3500 | 3700.0000000000000000
--  personnel |     2 |   3900 | 3700.0000000000000000
--  sales     |     3 |   4800 | 4866.6666666666666667
--  sales     |     1 |   5000 | 4866.6666666666666667



-- cumulative sum
SELECT salary, sum(salary) OVER (ORDER BY salary) FROM empsalary
--  salary |  sum  
-- --------+-------
--    3500 |  3500
--    3900 |  7400
--    4200 | 11600
--    4500 | 16100
--    4800 | 25700
--    4800 | 25700


----------------------------------------------------------------------
-- Geospartial Functions


-- points
SELECT
   dropoff_lat
  ,dropoff_lng
  ,st_astext(dropoff_point) geometry
FROM fact_trip
-- POINT(dropoff_lat, dropoff_lng)


-- polygons
SELECT city_name
      ,st_astext(shape)
FROM dim_city
-- Polygon((Point1, Point2, Point3, Point1), (Point4, Point5, Point6, Point4))
-- triangle of point 1,2,3, with a triangular hole of point 4,5,6

-- MultiPolygon((Point1, Point2, Point3, Point1), (Point4, Point5, Point6, Point4), (Point7, Point8, Point9, Point7))
-- above plus another triangle by point 7,8,9



-- from text to geom

--geom and geography: difference in precision
--geography: to meter
SELECT
  'POINT(' || signup_lng || ' ' || signup_lat || ')'
 , ST_GeomFromText('POINT(' || signup_lng || ' ' || signup_lat || ')')
 , ST_GeographyFromText('POINT(' || signup_lng || ' ' || signup_lat || ')')
 , st_astext(ST_GeographyFromText('POINT(' || signup_lng || ' ' || signup_lat || ')'))
FROM dim_client
--                      ?column?                     |              st_geomfromtext               |                st_geographyfromtext                |              st_astext               |
-- --------------------------------------------------+--------------------------------------------+----------------------------------------------------+--------------------------------------+
--  POINT(-46.6429767999999996 -23.548321399999999)  | 01010000001A0B54104D5247C06AB290CA5E8C37C0 | 0101000020E61000001A0B54104D5247C06AB290CA5E8C37C0 | POINT(-46.6429768 -23.5483214)       |
--  POINT(151.194545827599995 -33.8707424486999997)  | 0101000000B0E32BB839E66240E633127D74EF40C0 | 0101000020E6100000B0E32BB839E66240E633127D74EF40C0 | POINT(151.1945458276 -33.8707424487) |
--  POINT(-43.1302999999999983 -22.9021000000000008) | 01010000009B559FABAD9045C0BBB88D06F0E636C0 | 0101000020E61000009B559FABAD9045C0BBB88D06F0E636C0 | POINT(-43.1303 -22.9021)             |
--  POINT(-77.6534923353000011 39.9468693417000011)  | 0101000000361384D1D26953C0E117BC0333F94340 | 0101000020E6100000361384D1D26953C0E117BC0333F94340 | POINT(-77.6534923353 39.9468693417)  |
--  POINT(-46.6358000000000033 -23.547699999999999)  | 0101000000FE65F7E4615147C0EA043411368C37C0 | 0101000020E6100000FE65F7E4615147C0EA043411368C37C0 | POINT(-46.6358 -23.5477)             |



-- calculate area
SELECT
  st_area(shape)
FROM dim_city
WHERE city_id=1



-- contains
SELECT *
FROM fact_trip ft
JOIN dim_city dc
  ON ft.city_id = dc.city_id
WHERE st_contains(shape, begintrip_point) IS TRUE



-- check 2 points within distance
st_dwithin(
  ST_GeographyFromText('POINT('|| request_lng ||' '|| request_lat ||')'),
  ST_GeographyFromText('POINT(-86.778471 36.161248)'),
  1000)


-- distance between two points
st_distance(
  ST_GeographyFromText('POINT('|| begintrip_lng ||' '|| begintrip_lat ||')'),
  ST_GeographyFromText('POINT(2.294694 48.858093)'))









--------------------------------------------------------------------------------------------
-- unnest() in Presto




-- select a nested field
select  user_tags._column_key
from rawdata.schemaless_userstore_udr_entities_rows tb
where 1 = 1
and datestr = '2018-01-01' 



-- unnest a structure: 
-- need to transform into an array first 
-- then exploded table has one column: the array itself
select datestr, x.lalala, x.lalala._column_key
from  rawdata.schemaless_userstore_udr_entities_rows
cross join unnest(array[user_tags]) as x(lalala)
where datestr = '2019-01-01'





-- unnest a map
-- exploded table has 2 columns: key | value
-- where in this example and the next, value is also an array
select datestr, x.wtf, x.lalala
from  rawdata.schemaless_userstore_udr_entities_rows
cross join unnest(user_tags.user_tags) as x(wtf, lalala)
where datestr = '2019-01-01'



-- example of unnesting a map
SELECT
  tags.name,
  tags.notes.name,
  tags.notes.notes,
  tags.notes.note,
  tags.notes.deleted_at,
  tags.notes.created_at
FROM
  rawdata.schemaless_userstore_udr_entities_rows AS us
  CROSS JOIN UNNEST (USER_TAGS.user_tags) AS tags (name, notes)
where
  1 = 1
  and datestr = '2019-01-01'
  and tags.name like '%su_treatment%'






