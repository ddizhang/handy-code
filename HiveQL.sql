
--Hive 

-- Logical Operators: AND, OR, A+NOT
-- Comparison operators: =, >, <, >=, <=, <>, BETWEEN
-- Arithmetical operators: +, -, *, /

-- strings
-- 'string' || 'concatenation'
-- concat('string col 1', 'string col 2')


-- wildcards
-- string LIKE '%abc%'
-- lower(string) LIKE 'pattern


-- boolean columns
WHERE bool_column = false
WHERE !bool_column


-- Null Values
WHERE column IS NOT NULL


-- distinct
SELECT DISTINCT col




-- joins: only Equal join in HQL
-- JOIN optimization: start from the smallest table and finish with the biggest



-- CASE WHEN (Doesn't have to have a ELSE NULL)
SELECT COUNT(CASE WHEN is_partner = false THEN uuid END) 


-- GROUP BY with column positions: turned OFF by default
set hive.groupby.orderby.position.alias = true;
-- HAVING still works
GROUP BY 1,2
ORDER BY 1,2



-- ORDER BY is turned off by default
-- need to use with a LIMIT
-- or use SORT BY (partially sorted only)



-- Subqueries
-- use Common Table Expressions (CTE): WITH clause instead
WITH distinct_client_roles AS (
	SELECT DISTINCT roles FROM clients
)

SELECT 
  COUNT(role) 
FROM distinct_client_roles



-- Optimization: Join goes before Where
-- so use CTE and subqueries to shrink the joining tables


--------------------------------------------------------------------------------
-- datatype: Structure

-- A structure is a compound data type. 
-- It consists of a number of named data fields of various types. 
-- For example, there might be a structure called person, 
-- which contains a string firstname, a string lastname, a date dob, and numerical height.

SELECT *
FROM table
LIMIT 1
-- it's a nested structure
-- col1: int | col2: string | col3: structure
--  1000     | 'asdfds'     | {"key1": value,  
--                             "_key2": {"nest_key1": val21, "nest_key2":val22},  
--                             "key3":[val31, val32, val33], 
--                             "key4":[structure1, structure2]}

-- structure1 & 2: {'stkey1':val}


SELECT col3.key
     , col3.`_key2` -- fields starting with `_` need to be surrounded by backticks
     , col3.`_key2`.nest_key21  -- select nested field
     , col3.key3[0] -- first item in key3 array. value beyond array size will be NULL
     , size(col3.key3) -- array size
FROM table
WHERE array_contains(col3.key3, 'value_to_check')   -- see if a value is contained in a array



-- querying for a specific field within a list of sub-structure 
SELECT key4
FROM table
-- key4
-- --------------------------
-- record1 [structure1, structure2]
-- record2 [structure1, structure2]
-- ...


SELECT key4.stkey1
FROM table
-- key4.stkey1
-- ------------------------
-- record1 [structure1.stkey1, structure12.stkey1]
-- record2 [structure1.stkey1, structure12.stkey1]
-- ...



-- array function explode(): turns an array into rows. Can't use with other columns
-- USE LATERAL VIEWS

SELECT desired_columns
      ,array_element
FROM table_name
LATERAL VIEW explode(array_column) exploded_part AS array_element
WHERE blah blah




-- practical use of lateral view:
-- to summarize an array
SELECT 
  SUM(charges) 
FROM rawdata.schemaless_mezzanine_trips_rows 
LATERAL VIEW explode(fare.breakdown.charges.amount) x as charges
WHERE base.uuid = 'ae4258be-9b0f-4ca5-a584-1dcb3be684e0' 
AND datestr='2016-01-05'





-- use join with lateral view
-- this would fail
SELECT
  base.uuid, 
  client_uuid,
  firstname,
  lastname
FROM rawdata.schemaless_mezzanine_trips_rows
LATERAL VIEW explode(base.all_client_uuids) x AS client_uuid
JOIN dwh.dim_client c
ON c.user_uuid = client_uuid
WHERE datestr = '2016-01-01'


-- instead, use CTE
WITH trip_client_uuids AS (
  SELECT
    base.uuid AS base_uuid,
    client_uuid
  FROM rawdata.schemaless_mezzanine_trips_rows
  LATERAL VIEW explode(base.all_client_uuids) x AS client_uuid
  WHERE datestr = '2016-01-01'
)

SELECT
  tc.base_uuid,
  client_uuid,
  firstname,
  lastname
FROM trip_client_uuids tc
JOIN dwh.dim_client c
ON c.user_uuid = tc.client_uuid


---------------------------------------------------------------------------




-- Dates and Times

1. Timestamps 'yyyy-MM-dd hh:mm:ss.s', 'yyyy-MM-ddTHH:mm:ss'
2. Dates as strings
3. Epoch: seconds since Jan.1st, 1970


-- from utc timestamp and to utc timestamp
SELECT from_utc_timestamp(request_timestamp_utc,'America/Montreal'),
       to_utc_timestamp(request_timestamp_local, time_zone_column)
FROM table
WHERE base.request_at BETWEEN "2016-02-01'T'00:00:00" 
  AND "2016-05-01'T'23:59:59"



-- converting timestamp to date strings
to_date(timestamp)
-- epoch to timestamp
from_unixtime(epoch)
-- timestamp to epoch
unix_timestamp(timestamp)
-- epoch to date
from_unixtime(epoch,'yyyy-MM-dd') 
-- date to epoch
unix_timestamp(datestr,'yyyy-MM-dd')

-- between timestamp formats
from_unixtime(
    unix_timestamp(
      'a yyyy-MM-dd hh:mm:ss.s timestamp',
      "yyyy-MM-dd'T'HH:mm:ss"))



-- date arithmetic
-- only works with dates!
date_add(date, n)
date_sub(date, n)
date_add(to_date(timestamp), 7)

-- select current date
SELECT current_date

-- last day of the month given in the date
last_day(date)

-- difference in time
-- accepts both datestr and timestamp
datediff(timea, timeb)




-- date part
-- works both with timestamps and dates
year(), month(), day(), weekofyear()




---------------------------------------------------------------------------
-- Query Optimization


1. Partitions

2. Join smallest table first

3. Map Join

-- map join. Set up these parameters
set hive.auto.convert.join = true;
set hive.auto.convert.join.noconditionaltask = true;
set hive.auto.convert.join.noconditionaltask.size = 50000000;
--Map JOIN will work only if all tables except for the last one fit into the limit set by the hive.auto.convert.join.noconditionaltask.size flag.

4. Create subset of tables before joining


5. Move conditions from WHERE clause to JOIN clause













----------------------------------------
-- hashing function
hash(column) -- will hash the column into a number









