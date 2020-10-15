library(httr)

raw <- read.csv("~/Desktop/Landfills/CA_raw.csv", stringsAsFactors = FALSE)
raw <- raw[1:1136,]
size <- nrow(raw)

# Get data from other spreadsheets
owners <- read.csv("~/Desktop/Landfills/CA owners.csv", stringsAsFactors = FALSE)
geodata <- read.csv("~/Desktop/Landfills/CA site geodata.csv", stringsAsFactors = FALSE)

for (i in seq_len(size)) {

  if (raw[i,"SWIS"] %in% owners[,"SwisNo"]) {
    match <- grep(raw[i,"SWIS"], owners[,"SwisNo"])[1]
    owner <- owners[match,"Owner"]

    if (grepl("(county of|city of|calif dept|US |california dept|bureau of)", owner, ignore.case=TRUE)) {
      raw[i, 3] <- "Government"
    }
    else {
      raw[i, 3] <- owner
    }
  }

  if (raw[i,"SWIS"] %in% geodata[,"SwisNo"]) {
    match <- grep(raw[i,"SWIS"], geodata[,"SwisNo"])[1]

    raw[i,"Name"] <- geodata[match,"Name"]
    raw[i,"Street.Address"] <- geodata[match,"Location"]
    raw[i,"City"] <- geodata[match,"Place"]
    raw[i,"State"] <- "CA"
    raw[i,"County"] <- geodata[match,"County"]
    raw[i,"Phone.Number"] <- geodata[match,"Op_Phone"]
    raw[i,"Lat"] <- geodata[match,"Latitude"]
    raw[i,"Long"] <- geodata[match,"Longitude"]
  }
}


# GET FORMATTED ADDRESSES FROM LAT LONG

place_ids <- c()
for (i in seq_len(size)) {
  response <- GET(url="https://maps.googleapis.com/maps/api/place/textsearch/json",
                  query = list (
                    query = paste(raw[i,"Lat"], raw[i,"Long"], sep=","),
                    key = "",
                    location = paste(raw[i,"Lat"], raw[i,"Long"], sep=","),
                    radius = 5
                  )
  )
  content <- content(response)
  if (!is.null(content$results[[1]]$formatted_address)) {
    raw[i,"Street.Address"] <- content$results[[1]]$formatted_address
  }
}


# GET PLACE IDS
place_ids <- c()
for (i in seq_len(size)) {
  response <- GET(url="https://maps.googleapis.com/maps/api/place/findplacefromtext/json",
                  query = list (
                    key = "",
                    input = paste(details[i,"Name"], details[i,"City"], "CA"),
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
write.csv(details, "~/Desktop/Landfills/CA_details.csv")


# GET DETAILS
# hours, website, phone
details <- read.csv("~/Desktop/Landfills/CA_details.csv", stringsAsFactors = FALSE)
place_ids <- details[,"place_ids"]

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


# FORMAT WASTE TYPES
names(details)
types <- details[,19]

for (i in seq_along(types)) {
  line <- strsplit(types[i], split=",")

  details[i,18] <- accepts("municipal", line)
  #details[i,19] <- accepts("electronic", line)
  details[i,20] <- accepts("municipal", line)
  details[i,21] <- accepts("(green|wood)", line)
  details[i,22] <- accepts("(municipal|inert)", line)
  details[i,23] <- accepts("(construction|demolition|inert)", line)
  details[i,24] <- accepts("(metal|green)", line)
  details[i,25] <- accepts("(green)", line)
  details[i,26] <- accepts("(green)", line)
  details[i,27] <- accepts("(green)", line)
  details[i,28] <- accepts("(green)", line)
  details[i,29] <- accepts("(green)", line)
  details[i,30] <- accepts("(green)", line)
  details[i,31] <- accepts("waste oil", line)
  details[i,32] <- accepts("waste oil", line)
  #details[i,33] <- accepts("batteries", line)
  details[i,36] <- accepts("tires", line)
}

# Function - whether waste type is accepted
accepts <- function(pattern, line) {
  matches <- grepl(pattern, line[[1]], ignore.case=TRUE)
  if (sum(matches) != 0) {
    return("Yes")
  }
  else return("No")
}

write.csv(details, "~/Desktop/Landfills/CA_details.csv")
