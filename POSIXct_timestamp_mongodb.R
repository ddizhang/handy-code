# manually set timestamps in R and mongoDB
library(mongolite)

#specify timezone (or use default Sys.time() time zone.)
m <- mongo(collection="test1", db="testdb",
           url="mongodb://localhost:27017")
df = data.frame(a = rnorm(10), b = rbinom(10, 20, prob = 0.2))
df$timestamp = as.POSIXlt(Sys.time(),tz="America/Los_Angeles")
m$insert(df)

#insert some records with another timestamp
df = data.frame(a = rnorm(10, mean = 15), b = rbinom(10, 3, prob = 0.2))
df$timestamp = as.POSIXlt(Sys.time(),tz="America/Los_Angeles")
m$insert(df)

#another document of records with a timestamp without timezone specified
m <- mongo(collection="test2", db="testdb",
           url="mongodb://localhost:27017")
df = data.frame(a = rnorm(10, mean = 15), b = rbinom(10, 3, prob = 0.2))
df$timestamp = format(Sys.time(),tz="America/Los_Angeles")
m$insert(df)



#' This function returns the nth closest timestamp from now
getTimeStamp = function(periodFromNow, collection = "test1", url="mongodb://localhost:27017")
{
  m <- mongo(collection=collection, db="testdb", url = url)
  timestamps = m$distinct(key = "timestamp")
  periodFromNow = periodFromNow[periodFromNow < length(timestamps)]
  DTlist = lapply(periodFromNow, function(t) timestamps[order(timestamps, decreasing = TRUE) == (t + 1)])
  do.call(c, DTlist)
}


#-----------------------------------------------------------
#this works if timeStamp doesn't have time zone specified
timeStamp = getTimeStamp(0, collection = "test2", url)
m <- mongo(collection="test2", db="testdb",url="mongodb://localhost:27017")
res <- m$find(sprintf('{"timestamp": "%s"}', timeStamp))  

#but doesn't work if time zone is specified..
timeStamp = getTimeStamp(0, collection = "test1", url)
m <- mongo(collection="test1", db="testdb",url="mongodb://localhost:27017")
res <- m$find(sprintf('{"timestamp": "%s"}', timeStamp))  


as.POSIXct(timeStamp, tz='America/Los_Angeles')


#-----------------------------------------------------------
#This works with character-like format of timeStamp, with time zone
m <- mongo(collection="test1", db="testdb",url="mongodb://localhost:27017")
timeStamp = getTimeStamp(0, collection = "test1", url); timeStamp

#change it into a numeric format
dateNum <- as.numeric(as.POSIXct(timeStamp, tz='America/Los_Angeles'))*1000; as.character(dateNum)

#round/floor/ceiling it, still works
dateNum <- round(as.numeric(as.POSIXct(timeStamp, tz='America/Los_Angeles'))*1000)
test <- m$find(sprintf('{"timestamp": {"$eq":{"$date": {"$numberLong": "%s"}}}}', dateNum))

#and it doesn't work with timestamps without a time zone
m <- mongo(collection="test2", db="testdb",url="mongodb://localhost:27017")
timeStamp = getTimeStamp(0, collection = "test2", url); timeStamp
dateNum <- as.numeric(as.POSIXct(timeStamp))*1000; as.character(dateNum)

test <- m$find(sprintf('{"timestamp": {"$eq":{"$date": {"$numberLong": "%s"}}}}', dateNum))


#-----------------------------------------------------------


#doesn't work
dateInt = as.integer(as.POSIXct(timeStamps, tz='America/Los_Angeles', origin = "1970-1-1"))*1000

#works with numeric output of timeStamp
test <- m$find(sprintf('{"timestamp": {"$eq":{"$date": {"$numberLong": "%s"}}}}', timeStamps*1000))




