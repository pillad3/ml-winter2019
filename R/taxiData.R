taxiData <- function(n) {
  dataFolder <- "../Data"
  dataFile <- paste0(dataFolder, "/taxiData_", n, ".rds")
  
  # Create the cache folder if it doesn't exist
  if (!dir.exists(dataFolder)) {
    dir.create(dataFolder)
  }
  
  if (file.exists(dataFile)) {
    return(readRDS(dataFile))
  }
  
  # Load the data from the web in batches
  limit <- 50000
  batches <- ceiling(n/limit)
  rawData <- data.frame()
  
  for (i in 0:(batches-1)) {
    dataOffset <- i*limit
    message(paste0("Loading batch ", i+1, " of ", batches))
    
    # Build the URL
    dataUri <- paste0("http://data.cityofchicago.org/resource/wrvz-psew.json?",
                      "$limit=", as.integer(limit), "&",
                      "$offset=", as.integer(dataOffset), "&",
                      "$order=:id")
    
    # Load the data and close the connection
    urlCxn <- url(dataUri)
    batchStr <- readLines(urlCxn)
    close(urlCxn)
    
    # Write the data to the cache
    fileCxn <- file(dataFile)
    writeLines(batchStr, fileCxn)
    close(fileCxn)
    
    # Load the JSON into the data frame
    json <- jsonlite::fromJSON(batchStr, flatten = TRUE)
    rownames(json) <- as.numeric(rownames(json)) + dataOffset
    rawData <- rbind(rawData, json)
  }
  
  # Clean the data types and make it a tibble
  rawData = tibble::as_tibble(rawData) %>%
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
  
  data <- list(
    rawData=rawData,
    count=n,
    offset=dataOffset
  )
  
  class(data) <- "taxiData"
  saveRDS(data, dataFile)
  
  return(data)
}