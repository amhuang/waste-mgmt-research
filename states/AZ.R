library(httr, xml2, rvest)
library(dplyr)

raw <- read.csv("~/Desktop/Landfills/AZ_raw.csv", stringsAsFactors = FALSE)
size <- nrow(raw)

# Web scrape missing facilities in "All Landfills" XLSX

url3 <- "https://legacy.azdeq.gov/environ/waste/solid/non_municipal.html"
url2 <- "https://legacy.azdeq.gov/environ/waste/solid/active.html"
url1 <- "https://legacy.azdeq.gov/environ/waste/solid/transfer_station.html"

ts_names <- url1 %>% read_html() %>%
  html_nodes(".middle_content span b") %>%
  html_text()
lf_names <- url2 %>% read_html() %>%
  html_nodes(".middle_content span b") %>%
  html_text()
cd_names <- url3 %>% read_html() %>%
  html_nodes(".middle_content span b") %>%
  html_text()

raw[,"Name"] <- c(raw[,"Name"], ts_names, lf_names, cd_names) %>%
  unique()


# GET PLACE IDS

place_ids <- c()
for (i in seq_len(size)) {
  response <- GET(url="https://maps.googleapis.com/maps/api/place/findplacefromtext/json",
                  query = list (
                    key = "",
                    input = paste(raw[i,"Name"], raw[i,"City"], "AZ"),
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
write.csv(details, "~/Desktop/Landfills/AZ_details.csv")


# GET MISSING COUNTIES AND ZIP CODES

for (i in 168:190) {
  response <- GET(url="https://maps.googleapis.com/maps/api/place/details/json",
                  query = list(
                    key = "",
                    place_id = place_ids[i],
                    fields = "address_component"
                  ))
  content <- content(response)

  for (j in seq_along(content$result$address_components)) {
    level <- content$result$address_components[[j]]$types[[1]]
    if (level == "administrative_area_level_2") {
      details[i,"County"] <- content$result$address_components[[j]]$short_name
    }
    if (level == "postal_code") {
      details[i,"Zip"] <- content$result$address_components[[j]]$short_name
    }
  }
}


# GET DETAILS
details <- read.csv("~/Desktop/Landfills/AZ_details.csv", stringsAsFactors = FALSE)

for (i in seq_len(size)) {
  if (!is.na(place_ids[i])) {
    response <- GET(url="https://maps.googleapis.com/maps/api/place/details/json",
                    query = list(
                      key = "",
                      place_id = place_ids[i],
                      fields = "formatted_address,formatted_phone_number"
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

write.csv(details, "~/Desktop/Landfills/AZ_details.csv")
