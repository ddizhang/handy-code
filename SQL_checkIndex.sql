/*
There's a general consensus that you should reorganize ("defragment") your indices 
as soon as index fragmentation reaches more than 5 (sometimes 10%), 
and you should rebuild them completely when it goes beyond 30% 
(at least that's the numbers I've heard advocated in a lot of places).

Michelle Ufford (a.k.a. "SQL Fool") has an automated index defrag script, 
which uses those exact limits for deciding when to reorganize or rebuild an index.

Also see Brad McGehee's tips on rebuild indexes with some good thoughts and tips on how to deal with index rebuilding.
*/

USE _dizhang
GO

SELECT 
    t.NAME 'Table name',
    i.NAME 'Index name',
    ips.index_type_desc,
    ips.alloc_unit_type_desc,
    ips.index_depth,
    ips.index_level,
    ips.avg_fragmentation_in_percent,
    ips.fragment_count,
    ips.avg_fragment_size_in_pages,
    ips.page_count,
    ips.avg_page_space_used_in_percent,
    ips.record_count,
    ips.ghost_record_count,
    ips.Version_ghost_record_count,
    ips.min_record_size_in_bytes,
    ips.max_record_size_in_bytes,
    ips.avg_record_size_in_bytes,
    ips.forwarded_record_count
FROM 
    sys.dm_db_index_physical_stats(DB_ID(), NULL, NULL, NULL, 'DETAILED') ips
INNER JOIN  
    sys.tables t ON ips.OBJECT_ID = t.Object_ID
INNER JOIN  
    sys.indexes i ON ips.index_id = i.index_id AND ips.OBJECT_ID = i.object_id
WHERE
    AVG_FRAGMENTATION_IN_PERCENT > 0.0
ORDER BY
    AVG_FRAGMENTATION_IN_PERCENT, fragment_count