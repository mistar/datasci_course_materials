listS3Objects <- function(awsregion,s3Bucket){
  ## awsregion: string
  ## s3Bucket: string
  ## Return: list: all objects in the aws reagion stored in the s3 bucket
  
  sandboxyr::setCredentials(iamrole="analyticsSandboxServerRole")
  s3List <- sandboxyr::getS3Bucket(awsregion=awsregion,
                                   s3bucket=s3Bucket) 
  s3List <- lapply(s3List[names(s3List)=="Contents"],function(x){x$Key})
  l <- as.list(NA)
  for (i in 1:length(s3List)){
    l <- c(l,s3List[[i]])
  }
  l <- l[grep("*.csv",l)]
}


getS3Data <- function(awsregion,s3Bucket,s3Object) {
  ## awsregion: string
  ## s3Bucket: string
  ## s3Object: string
  ## Return: list: all data in the aws reagion stored in the s3 bucket and collection s3 Object

  sandboxyr::setCredentials(iamrole="analyticsSandboxServerRole")
  selection <- sandboxyr::getS3Object(awsregion=awsregion,
                                      s3bucket=s3Bucket,
                                      s3object=s3Object)
}


getMongoDBDatabase <- function(host){
  ## host: string: IP adress
  ## Return: list: databases available for the host
  
  mongost <- rmongodb::mongo.create(host = host)
  if (rmongodb::mongo.is.connected(mongost)) {
    rmongodb::mongo.get.databases(mongost)
  }
}

getMongoDBCollection <- function(host, db){
  ## host: string: IP adress
  ## db: string: db name
  ## Return: list: collections available for the host and database
  
  mongost <- rmongodb::mongo.create(host = host)
  if (rmongodb::mongo.is.connected(mongost)) {
    rmongodb::mongo.get.database.collections(mongost, db = db)
  }
}

getMongoDBData <- function(host, collection,query,limit){
  ## host: string: IP adress
  ## collection: string: collection name
  ## query: string: query in JSON format
  ## limit: integer: number of records to be returned
  ## Return: data.frame: data for the host and collection that match query criteria
  ##         the result will returm maximum "limit" number of records

  mongost <- rmongodb::mongo.create(host = host)
  if (rmongodb::mongo.is.connected(mongost)) {
    if (limit == "" & query == ""){
      cursor <- rmongodb::mongo.find(mongost,collection)
    } else if (limit == ""){
      cursor <- rmongodb::mongo.find(mongost,collection, query=query)
    } else if (query == ""){
      limit <- as.integer(limit)
      cursor <- rmongodb::mongo.find(mongost,collection, limit=limit)
    } else {
      limit <- as.integer(limit)
      cursor <- rmongodb::mongo.find(mongost,collection, query=query,limit=limit)
    }
    rmongodb::mongo.cursor.to.data.frame(cursor, nullToNA = TRUE)
  }
}

# initiate dataSet
dataSet <- data.frame(NA)