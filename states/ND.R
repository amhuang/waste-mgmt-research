library(httr)
original <- read.csv("~/Downloads/Landfills/ND_raw.csv", stringsAsFactors = FALSE)
raw <- original[1:279,]
size <- nrow(raw)

# MISSING ADDRESSES FROM COORDINATES
# Coordinates
coords <- rep(NA, size)
for (i in seq_len(size)) {
  if (!is.na(raw[i,"Lat"]) && raw[i,"Lat"] != 0) {
    coords[i] <- paste(raw[i,"Lat"], raw[i,"Long"], sep=",")
  }
}
length(unique(coords))
length(which(duplicated(coords)))
dups <- which(duplicated(coords))
raw[dups,] <- NA

# Getting addresses
for (i in seq_len(size)) {
  if ( (!is.na(coords[i])) && (raw[i,"Street.Address"] == "") ) {
    response <- GET(url="https://maps.googleapis.com/maps/api/place/textsearch/json",
                    query = list (
                      query=paste(raw[i,"Lat"], raw[i,"Long"], sep=","),
                      key = "",
                      location = paste(raw[i,"Lat"], raw[i,"Long"], sep=","),
                      radius = 10
                    )
    )
    content <- content(response)
    raw[i,4] <- content$results[[1]]$formatted_address
  }
}

# Formatting new addresses into df
for (i in seq_len(size)) {
  split <- strsplit(raw[i,"Street.Address"], ", ")
  if (length(split[[1]]) > 1) {
    raw[i,"Street.Address"] <- split[[1]][1]
    raw[i,"City"] <- split[[1]][2]
    raw[i,"Zip"] <- gsub("[^0-9]", "", split[[1]][3])
  }
}


# GET PLACE IDS

place_ids <- c()
for (i in seq_len(size)) {
  response <- GET(url="https://maps.googleapis.com/maps/api/place/findplacefromtext/json",
                  query = list (
                    key = "",
                    input = paste(raw[i,"Street.Address"], raw[i,"City"], "ND"),
                    inputtype = "textquery",
                    fields = "place_id")
  )
  content <- content(response)

  if (length(content$candidates) == 0)
    place_ids[i] <- ""

  else
    place_ids[i] <- content$candidates[[1]]$place_id
}
length(place_ids)
length(unique(place_ids))
details <- cbind (raw, place_ids)
write.csv(details, "~/Downloads/Landfills/ND_details.csv")


# GET DETAILS
# hours, website

details <- read.csv("~/Downloads/Landfills/ND_details.csv", stringsAsFactors = FALSE)[,2:53]
place_ids <- details[,52]
size <- nrow(details)

for (i in seq_len(size)) {
  if (!is.na(place_ids[i])) {
    response <- GET(url="https://maps.googleapis.com/maps/api/place/details/json",
                    query = list(
                      key = "",
                      place_id = place_ids[i],
                      fields = "website,opening_hours,formatted_phone_number"
                    ))
    content <- content(response)

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

write.csv(raw, "~/Downloads/Landfills/ND_details.csv")
