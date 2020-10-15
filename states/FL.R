library(httr, xml2)
library(rvest, dplyr)

raw <- read.csv("~/Desktop/Landfills/FL_raw.csv", stringsAsFactors = FALSE)
size <- nrow(raw)

arcgis <- read.csv("~/Desktop/Landfills/arcgis.csv", stringsAsFactors = FALSE)

insert <- 253
for (i in seq_len(nrow(arcgis))) {
  if ( !(arcgis[i, 'Name'] %in% raw[,'Name']) ) {
    raw[insert,] <- arcgis[i,]
    insert <- insert + 1
  }
}

# GET PLACE IDS

place_ids <- c()
for (i in 335:size) {
  response <- GET(url="https://maps.googleapis.com/maps/api/place/findplacefromtext/json",
                  query = list (
                    key = "",
                    input = paste(raw[i,"Name"], raw[i,"City"], "Florida"),
                    inputtype = "textquery",
                    fields = "place_id")
  )
  content <- content(response)

  if (length(content$candidates) == 0)
    place_ids[i] <- NA

  else
    place_ids[i] <- content$candidates[[1]]$place_id
}

details <- cbind (raw, place_ids)
# GET DETAILS

for (i in 335:size) {
  if (!is.na(place_ids[i])) {
    response <- GET(url="https://maps.googleapis.com/maps/api/place/details/json",
                    query = list(
                      key = "",
                      place_id = place_ids[i],
                      fields = "website,formatted_phone_number,opening_hours,formatted_address"
                    ))
    content <- content(response)

    if ( !is.null(content$result$formatted_address) ) {
      details[i, "Street.Address"] <- content$result$formatted_address
    }
    if ( !is.null(content$result$website) ) {
      details[i, "Website"] <- content$result$website
    }
    if (!is.null(content$result$formatted_phone_number)) {
      details[i,"Phone.Number"] <-content$result$formatted_phone_number
    }
    # Getting hours
    if (!is.null(content$result$opening_hours$weekday_text[[1]])) {
      str <- content$result$opening_hours$weekday_text[[1]]
      str <- gsub("Monday: ", "", str)
      str <- gsub(" ", "", str)
      str <- gsub("Closed", "No", str)
      details[i, 12] <- str
    }
    if (!is.null(content$result$opening_hours$weekday_text[[2]])) {
      str <- content$result$opening_hours$weekday_text[[2]]
      str <- gsub("Tuesday: ", "", str)
      str <- gsub(" ", "", str)
      str <- gsub("Closed", "No", str)
      details[i, 13] <- str
    }
    if (!is.null(content$result$opening_hours$weekday_text[[3]])) {
      str <- content$result$opening_hours$weekday_text[[3]]
      str <- gsub("Wednesday: ", "", str)
      str <- gsub(" ", "", str)
      str <- gsub("Closed", "No", str)
      details[i, 14] <- str
    }
    if (!is.null(content$result$opening_hours$weekday_text[[4]])) {
      str <- content$result$opening_hours$weekday_text[[4]]
      str <- gsub("Thursday:", "", str)
      str <- gsub(" ", "", str)
      vstr <- gsub("Closed", "No", str)
      details[i, 15] <- str
    }
    if (!is.null(content$result$opening_hours$weekday_text[[5]])) {
      str <- content$result$opening_hours$weekday_text[[5]]
      str <- gsub("Friday: ", "", str)
      str <- gsub(" ", "", str)
      str <- gsub("Closed", "No", str)
      details[i, 16] <- str
    }
    if (!is.null(content$result$opening_hours$weekday_text[[6]])) {
      str <- content$result$opening_hours$weekday_text[[6]]
      str <- gsub("Saturday: ", "", str)
      str <- gsub(" ", "", str)
      str <- gsub("Closed", "No", str)
      details[i, 17] <- str
    }
    if (!is.null(content$result$opening_hours$weekday_text[[7]])) {
      str <- content$result$opening_hours$weekday_text[[7]]
      str <- gsub("Sunday: ", "", str)
      str <- gsub(" ", "", str)
      str <- gsub("Closed", "No", str)
      details[i, 11] <- str
    }
  }
}

write.csv(details, "~/Desktop/Landfills/FL_details.csv")
