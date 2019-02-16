taxiData <- function(n) {
  limit <- 50000
  batches <- ceiling(n/limit)
  rawData <- data.frame()
  
  for (i in 0:(batches-1)) {
    dataOffset <- i*limit
    
    url <- paste0("https://data.cityofchicago.org/resource/wrvz-psew.json?",
                  "$limit=", limit, "&",
                  "$offset=", dataOffset, "&",
                  "$order=:id")
    cat(url, "\n")
    json <- jsonlite::fromJSON(url, flatten = TRUE)
    rownames(json) <- as.numeric(rownames(json)) + dataOffset
    rawData <- rbind(rawData, json)
  }
  
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
  data
}