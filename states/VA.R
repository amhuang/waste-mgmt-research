library(httr)

raw <- read.csv("~/Downloads/Landfills/VA_raw.csv")
size <- nrow(details)

# GET PLACE IDS

place_ids <- c()

for (i in seq_len(size)) {
  response <- GET(url="https://maps.googleapis.com/maps/api/place/findplacefromtext/json",
                  query = list (
                    key = "",
                    input = paste(raw[i,2], "VA"),
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
write.csv(details, "~/Downloads/Landfills/VA_details.csv")
details <- details[1:50]

# GET DETAILS
# hours, website, and phone

details <- read.csv("~/Downloads/Landfills/VA_details.csv")
place_ids <- details[1:96,"place_id"]

for (i in seq_along(place_ids)) {
  if (!is.na(place_ids[i])) {
    response <- GET(url="https://maps.googleapis.com/maps/api/place/details/json",
                    query = list(
                      key = "",
                      place_id = place_ids[i],
                      fields = "website,opening_hours,formatted_phone_number,formatted_address,address_components"
                    ))
    content <- content(response)

    for (j in seq_along(content$result$address_components)) {
      level <- content$result$address_components[[j]]$types[[1]]
      if (level == "locality") {
        details[i,"City"] <- content$result$address_components[[j]]$short_name
      }
      if (level == "administrative_area_level_2") {
        details[i,"County"] <- content$result$address_components[[j]]$short_name
      }
      if (level == "postal_code") {
        details[i,"Zip"] <- content$result$address_components[[j]]$short_name
      }
    }

    if (!is.null(content$result$formatted_address)) {
      details[i, "Street.Address"] <- content$result$formatted_address
    }
    if (!is.null(content$result$website)) {
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
      str <- gsub("Thursday: ", "", str)
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
write.csv(details, "~/Downloads/Landfills/VA_details.csv")



# SEARCH CONVENIENCE CENTERS

counties <- read.csv("~/Downloads/Landfills/VA_counties.csv")
counties <- counties[,1]
counties <- unique(counties)

places <- details[,"Name"]

for (i in seq_along(counties)) {
    response <- GET(url="https://maps.googleapis.com/maps/api/place/textsearch/json",
                    query = list(
                      key = "",
                      query = paste(counties[i], "VA Convenience Center")
                    ))
    content <- content(response)

    for (j in seq_along(content$results)) {
      if (!(content$results[[j]]$formatted_address %in% details[,"Street.Address"])) {
        details[173+i,"Name"] <- content$results[[j]]$name
        details[173+i,"Street.Address"] <- content$results[[j]]$formatted_address
        details[173+i,"place_id"] <- content$results[[j]]$place_id
        details[173+i,"County"] <- counties[i]
      }
    }
}
