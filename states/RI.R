
details <- read.csv("~/Downloads/Landfills/RI_raw.csv", stringsAsFactors = FALSE)[1:61,1:49]

# Formatting addresses

for (i in seq_len(nrow(details))) {
  # splitting address into 3 components
  if (!is.na(details[i, 4])) {
    components <- strsplit(details[i, 4], split=",")
    components <- components[[1]]

    if (length(components) == 3) {
      details[i,4] <- components[1]
      details[i,5] <- components[2]
      details[i,6] <- "RI"
      details[i,7] <- components[3]
    }
  }
}

# Getting website + opening hrs

library(httr)

place_ids <- c()
size <- nrow(details)

for (i in seq_len(size)) {
  response <- GET(url="https://maps.googleapis.com/maps/api/place/findplacefromtext/json",
                  query = list (
                    key = "",
                    input = paste(details[i,2], details[i,5], "RI"),
                    inputtype = "textquery",
                    fields = "place_id")
  )
  content <- content(response)

  if (length(content$candidates) == 0)
    place_ids[i] <- NA

  if (length(content$candidates) != 0)
    place_ids[i] <- content$candidates[[1]]$place_id
}

dim(details)
length(unique(place_ids))

details <- cbind(details, place_ids, stringsAsFactors=FALSE)
write.csv(details, "~/Downloads/Landfills/RI_details.csv")

# Obtain details with place IDs

details <- read.csv("~/Downloads/Landfills/RI_details.csv", stringsAsFactors = FALSE)[,2:51]
place_ids <- details[ ,50]

for (i in seq_len(size)) {
  if (!is.na(place_ids[i])) {
    response <- GET(url="https://maps.googleapis.com/maps/api/place/details/json",
                    query = list(
                      key = "",
                      place_id = place_ids[i],
                      fields = "address_component,website,opening_hours,formatted_phone_number"
                    ))
    content <- content(response)

    if (!is.null(content$result$address_components)) {
      for (j in seq_along(content$result$address_components)) {
        if (content$result$address_components[[j]]$types[[1]] == "administrative_area_level_2") {
          county <- content$result$address_components[[j]]$long_name
          county <- gsub("County", "", county)
          details[i, "County"] <- county
          print(content$result$address_components[[j]]$long_name)
        }
      }
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

names(details)
dups <- details[which(duplicated(details[,4])),]
details[36,] <- NA
details[25,] <- NA
details[15,] <- NA
details[3,] <- NA
details[11,] <- NA
details[13,] <- NA


write.csv(details, "~/Downloads/Landfills/RI_details.csv")
