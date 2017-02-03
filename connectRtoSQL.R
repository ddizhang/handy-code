


library(RODBC)
dbhandle <- odbcDriverConnect('driver={SQL Server}; server=dc1sql27; trusted_connection=true')

#you can select from a permenant table, or a global temp table
sqlQuery(dbhandle, "select top 10 * from tempDB..##globalTempTbName")

#or make a temp table through the connection
sqlQuery(dbhandle, 'select top 10 * into #testTable from [_Boris].[dbo].[AC_2013_Models]')
res <- sqlQuery(dbhandle, 'select * from #testTable')


odbcClose(dbhandle)




