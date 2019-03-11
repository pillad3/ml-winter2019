taxiData <- function(numberOfRows, startIndex = 0, cleanData = TRUE) {
  dataFolder <- "../Data"
  dataFile <- paste0(dataFolder, "/taxiData.rds")
  
  # Use cached data if we have any. Only use the cache when the start index
  # is at 0 to kep the cache reusable for all values of numberOfRows
  if (startIndex == 0 && file.exists(dataFile)) {
    cachedData <- readRDS(dataFile)
    cachedCount <- cachedData$count
    
    # If we've already loaded enough rows, just return the requested # of rows
    if (cachedCount >= numberOfRows) {
      message("All taxi data loaded from cache")
      
      cachedData$rawData <- cachedData$rawData[1:numberOfRows,]
      cachedData$count <- numberOfRows
      return(cachedData)
    }
    message(paste0("Need to load ", numberOfRows-cachedCount, " additional rows"))
    
    # We need to load more rows. Only load what we don't already have from the cache
    remainingCount <- numberOfRows - cachedCount
    startIndex <- cachedCount
    remainingData <- taxiData(remainingCount, startIndex)
    
    # Combine the cached data with the new data
    data = list()
    data$rawData <- rbind(cachedData$rawData, remainingData$rawData)
    data$count <- numberOfRows
    
    # Write the new object to the cache
    saveRDS(data, dataFile)
    
    return(data)
  }
  
  # Create the cache folder if it doesn't exist
  if (!dir.exists(dataFolder)) {
    dir.create(dataFolder)
  }
  
  # Load the data from the web in batches
  limit <- 50000
  batches <- ceiling(numberOfRows/limit)
  rawData <- data.frame()
  
  for (i in 0:(batches-1)) {
    dataOffset <- i*limit
    message(paste0("Loading rows ", dataOffset, " to ", dataOffset + limit))
    
    # Build the URL
    dataUri <- paste0("http://data.cityofchicago.org/resource/wrvz-psew.json?",
                      "$limit=", as.integer(limit), "&",
                      "$offset=", as.integer(dataOffset), "&",
                      "$order=:id")
    
    # Load the data and close the connection
    urlCxn <- url(dataUri)
    batchStr <- readLines(urlCxn)
    close(urlCxn)
    
    # Load the JSON into the data frame
    json <- jsonlite::fromJSON(batchStr, flatten = TRUE)
    rownames(json) <- as.numeric(rownames(json)) + dataOffset
    rawData <- rbind(rawData, json)
  }
  
  rawData <- tibble::as_tibble(rawData)
  if (cleanData) {
    # Clean the data types and make it a tibble
    rawData = rawData %>%
      mutate(
        company = as.factor(company),
        pickup_community_area = as.factor(pickup_community_area),
        dropoff_community_area = as.factor(dropoff_community_area),
        payment_type = as.factor(payment_type),
        taxi_id = as.factor(taxi_id),
        trip_id = as.factor(trip_id),
        trip_miles = as.numeric(trip_miles),
        trip_seconds = as.numeric(trip_seconds),
        tips = as.numeric(tips),
        fare = as.numeric(fare)
      )
  }
  
  data <- list(
    rawData=rawData,
    count=numberOfRows
  )
  
  class(data) <- "taxiData"
  
  # Only write to the cache if we're starting at 0
  if (startIndex == 0) {
    saveRDS(data, dataFile)
  }
  
  return(data)
}