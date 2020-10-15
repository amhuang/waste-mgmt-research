
details <- read.csv("~/Downloads/Landfills/VT_raw.csv", stringsAsFactors = FALSE)[1:190,1:49]

# Getting website + opening hrs
library(httr)

place_ids <- c()
size <- nrow(details)

for (i in seq_len(size)) {
  response <- GET(url="https://maps.googleapis.com/maps/api/place/findplacefromtext/json",
                  query = list (
                    key = "",
                    input = paste(details[i,2], details[i,5], "VT"),
                    inputtype = "textquery",
                    fields = "place_id")
  )
  content <- content(response)

  if (length(content$candidates) == 0)
    place_ids[i] <- NA

  if (length(content$candidates) != 0)
    place_ids[i] <- content$candidates[[1]]$place_id
}

length(unique(place_ids))
details <- cbind(details, place_ids, stringsAsFactors=FALSE)
write.csv(details, "~/Downloads/Landfills/VT_details.csv")

# Obtain details with place IDs

details <- read.csv("~/Downloads/Landfills/VT_details.csv", stringsAsFactors = FALSE)
place_ids <- details[ ,50]

for (i in seq_len(size)) {
  if (!is.na(place_ids[i])) {

    response <- GET(url="https://maps.googleapis.com/maps/api/place/details/json",
                    query = list(
                      key = "",
                      place_id = place_ids[i],
                      fields = "website,opening_hours"
                    ))
    content <- content(response)

    if (!is.null(content$result$website)) {
      details[i, "Website"] <- content$result$website
    }
    if (!is.null(content$result$opening_hours$weekday_text[[1]])) {
      str <- content$result$opening_hours$weekday_text[[1]]
      str <- gsub("Monday: ", "", str)
      str <- gsub(" ", "", str)
      details[i, 12] <- str
    }
    if (!is.null(content$result$opening_hours$weekday_text[[2]])) {
      str <- content$result$opening_hours$weekday_text[[2]]
      str <- gsub("Tuesday: ", "", str)
      str <- gsub(" ", "", str)
      details[i, 13] <- str
    }
    if (!is.null(content$result$opening_hours$weekday_text[[3]])) {
      str <- content$result$opening_hours$weekday_text[[3]]
      str <- gsub("Wednesday: ", "", str)
      str <- gsub(" ", "", str)
      details[i, 14] <- str
    }
    if (!is.null(content$result$opening_hours$weekday_text[[4]])) {
      str <- content$result$opening_hours$weekday_text[[4]]
      str <- gsub("Thursday: ", "", str)
      str <- gsub(" ", "", str)
      details[i, 15] <- str
    }
    if (!is.null(content$result$opening_hours$weekday_text[[5]])) {
      str <- content$result$opening_hours$weekday_text[[5]]
      str <- gsub("Friday: ", "", str)
      str <- gsub(" ", "", str)
      details[i, 16] <- str
    }
    if (!is.null(content$result$opening_hours$weekday_text[[6]])) {
      str <- content$result$opening_hours$weekday_text[[6]]
      str <- gsub("Saturday: ", "", str)
      str <- gsub(" ", "", str)
      details[i, 17] <- str
    }
    if (!is.null(content$result$opening_hours$weekday_text[[7]])) {
      str <- content$result$opening_hours$weekday_text[[7]]
      str <- gsub("Sunday: ", "", str)
      str <- gsub(" ", "", str)
      details[i, 11] <- str
    }
  }
}

which(duplicated(details[,4]))
details[173,] <- NA
write.csv(details, "~/Downloads/Landfills/VT_details.csv")
