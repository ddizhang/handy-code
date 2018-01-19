


library(RODBC)
# this will connect to your default database. 
dbhandle <- odbcDriverConnect('driver={SQL Server}; server=dc1sql27; trusted_connection=true')

# you can connect to other databases (and write to other databases) this way
con <- odbcDriverConnect('driver={SQL Server};server=DC1SQL27;database=_Analytics;trusted_connection=true')
sqlSave(con, combination, tablename = 'mkt.CampaignReason', rownames = FALSE, append = FALSE)


#you can select from a permenant table, or a global temp table
sqlQuery(dbhandle, "select top 10 * from tempDB..##globalTempTbName")

#or make a temp table through the connection
sqlQuery(dbhandle, 'select top 10 * into #testTable from [_Boris].[dbo].[AC_2013_Models]')
res <- sqlQuery(dbhandle, 'select * from #testTable')

#save back to sql database
sqlSave(dbhandle, res, tablename = "optMultipliers")



odbcClose(dbhandle)



