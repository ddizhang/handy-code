


library(RODBC)
#this will connect to your default database
dbhandle <- odbcDriverConnect('driver={SQL Server}; server=dc1sql27; trusted_connection=true')

# you can connect to other databases (and write to otherdatabases) this way
con <- odbcDriverConnect('driver={SQL Server}; server=DC1SQL27; database=_Analytics;trusted_connection=true')
sqlSave(con, table_to_save, tablename = 'schema.tablename', rownames = FALSE, append = FALSE)

#you can select from a permenant table, or a global temp table
sqlQuery(dbhandle, "select top 10 * from tempDB..##globalTempTbName")

#or make a temp table through the connection
sqlQuery(dbhandle, 'select top 10 * into #testTable from [_Boris].[dbo].[AC_2013_Models]')
res <- sqlQuery(dbhandle, 'select * from #testTable')

# when dealing with timestamps, you need to factorize it in R before upload to SQL, otherwise it will throw an error.
table_to_save$dateLogged = as.factor(Sys.Date())


odbcClose(dbhandle)






