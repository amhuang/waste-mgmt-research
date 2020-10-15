library(httr, xml2)
library(rvest, dplyr)

raw <- read.csv("~/Desktop/Landfills/MN_details.csv", stringsAsFactors = FALSE)
size <- nrow(raw)


# GET PLACE IDS

place_ids <- c()
cities <- c()

for (i in seq_len(size)) {
  response <- GET(url="https://maps.googleapis.com/maps/api/place/findplacefromtext/json",
                  query = list (
                    key = "",
                    input = paste(raw[i,"City"], raw[i,"City"], "Minnesota"),
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


# GET MISSING COUNTIES

for (i in seq_len(size)) {
  response <- GET(url="https://maps.googleapis.com/maps/api/place/details/json",
                  query = list(
                    key = "",
                    place_id = cities[i],
                    fields = "address_component"
                  ))
  content <- content(response)

  for (j in seq_along(content$result$address_components)) {
    level <- content$result$address_components[[j]]$types[[1]]
    if (level == "administrative_area_level_2") {
      details[i,"County"] <- content$result$address_components[[j]]$short_name
    }
    if (level == "postal_code") {
      # details[i,"Zip"] <- content$result$address_components[[j]]$short_name
    }
  }
}


# GET DETAILS

for (i in seq_len(size)) {
  if (!is.na(place_ids[i])) {
    response <- GET(url="https://maps.googleapis.com/maps/api/place/details/json",
                    query = list(
                      key = "",
                      place_id = place_ids[i],
                      fields = "website,formatted_phone_number,opening_hours"
                    ))
    content <- content(response)

    if ( !is.null(content$result$formatted_address) ) {
      details[i, "Street.Address"] <- content$result$formatted_address
    }
    if ( !is.null(content$result$website) ) {
      details[i, "Website"] <- content$result$website
    }
    if (!is.null(content$result$formatted_phone_number) ) {
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
      str <- gsub("Thursday: ", "", str)
      str <- gsub(" ", "", str)
      str <- gsub("Closed", "No", str)
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

write.csv(details, "~/Desktop/Landfills/MN_details.csv")
