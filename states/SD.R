library(httr)
original <- read.csv("~/Downloads/Landfills/SD_details.csv", stringsAsFactors = FALSE)
raw <- original[1:245,]
size <- nrow(raw)


# FORMATTING RECYCLING WASTE TYPES

recycling <- raw[66:245,]
types <- recycling[,18]

for (i in seq_along(types)) {
  line <- strsplit(types[i], split=",")

  #recycling[i,18] <- accepts("msw", line)
  recycling[i,19] <- accepts("(electronics|phones|computer|TV)", line)
  recycling[i,20] <- accepts("(white good|whitegood|appliances|mattress)", line)
  recycling[i,21] <- accepts("(yard|trees)", line)
  recycling[i,22] <- accepts("msw", line)
  recycling[i,23] <- accepts("(construction|demolition)", line)
  recycling[i,24] <- accepts("(metal|car parts)", line)
  recycling[i,25] <- accepts("(non-ferrous|tin|steel)", line)
  recycling[i,26] <- accepts("(aluminum|ferrous|foil)", line)
  recycling[i,27] <- accepts("(plastic)", line)
  recycling[i,28] <- accepts("(glass)", line)
  recycling[i,29] <- accepts("(paper|news)", line)
  recycling[i,30] <- accepts("(cardboard|box board)", line)
  recycling[i,31] <- accepts("oil", line)
  recycling[i,32] <- accepts("oil", line)
  recycling[i,33] <- accepts("(Automotive batteries|lead.*acid)", line)
  recycling[i,34] <- accepts("(paint|solvent)", line)
  recycling[i,35] <- accepts("(antifreeze)", line)
  recycling[i,36] <- accepts("(tires)", line)
}

# Function - whether waste type is accepted
accepts <- function(pattern, line) {
  matches <- grepl(pattern, line[[1]], ignore.case=TRUE)
  if (sum(matches) != 0) {
    return("Yes")
  }
  else return("No")
}

# Mark recycling centers with all No's as NA
is_useful <- c()
for (i in seq_along(types)) {
  num_nos <- sum (recycling[i,19:36] == "No")
  if (num_nos==18) {
    is_useful[i] <- FALSE
  }
  else is_useful[i] <- TRUE
}
recycling[!is_useful,] <- NA

raw[66:245,] <- recycling
write.csv(raw, "~/Downloads/Landfills/SD_details.csv")


# GET ZIP CODES & COUNTIES

for (i in seq_len(size)) {
  response <- GET(url="https://maps.googleapis.com/maps/api/place/findplacefromtext/json",
                  query = list (
                    key = "",
                    input = paste(raw[i,"City"], "SD", sep=", "),
                    inputtype = "textquery",
                    fields = "place_id")
  )
  content <- content(response)

  response <- GET(url="https://maps.googleapis.com/maps/api/place/details/json",
                  query = list (
                    key = "",
                    place_id = content$candidates[[1]]$place_id,
                    fields = "address_component")
  )
  content <- content(response)

  for (j in seq_along(content$result$address_components)) {
    level <- content$result$address_components[[j]]$types[[1]]
    if (level == "administrative_area_level_2") {
      raw[i,"County"] <- content$result$address_components[[j]]$short_name
    }
    if (level == "postal_code") {
      raw[i,"Zip"] <- content$result$address_components[[j]]$short_name
    }
  }
}
write.csv(raw, "~/Downloads/Landfills/SD_details.csv")


# GET PLACE IDS

place_ids <- c()
for (i in seq_len(size)) {
  response <- GET(url="https://maps.googleapis.com/maps/api/place/findplacefromtext/json",
                  query = list (
                    key = "",
                    input = paste("+1", raw[i,"Phone.Number"]),
                    inputtype = "phonenumber",
                    fields = "place_id,formatted_address")
  )
  content <- content(response)

  if (length(content$candidates) == 0)
    place_ids[i] <- ""

  else {
    place_ids[i] <- content$candidates[[1]]$place_id
    raw[i, "Street.Address"] <- content$candidates[[1]]$formatted_address
  }
}
length(unique(place_ids))

details <- cbind (raw, place_ids)
write.csv(details, "~/Downloads/Landfills/SD_details.csv")


# GET DETAILS
# hours, website

details <- read.csv("~/Downloads/Landfills/SD_details.csv", stringsAsFactors = FALSE)[,2:51]
place_ids <- details[,50]

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
    if ( !is.null(content$result$formatted_phone_number) ) {
      details[i, "Phone.Number"] <- content$result$formatted_phone_number
    }
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
write.csv(details, "~/Downloads/Landfills/SD_details.csv")
