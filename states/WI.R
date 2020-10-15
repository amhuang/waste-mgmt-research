library(httr)
original <- read.csv("~/Downloads/Landfills/WI_raw.csv", stringsAsFactors = FALSE)[1:219,1:49]
raw <- original[1:219,1:49]
size <- nrow(raw)

# GET ZIP CODES

for (i in seq_len(size)) {
  response <- GET(url="https://maps.googleapis.com/maps/api/place/findplacefromtext/json",
                  query = list (
                    key = "",
                    input = paste(raw[i,5], "WI", sep=", "),
                    inputtype = "textquery",
                    fields = "formatted_address")
  )
  content <- content(response)

  if (length(content$candidates) != 0)
    addr <- content$candidates[[1]]$formatted_address
  addr <- strsplit(addr, ",")[[1]]
  raw[i,"Zip"] <- gsub("[^0-9]", "", addr[2])
}

write.csv(details, "~/Downloads/Landfills/WI_details.csv")
length(unique(raw[,"Zip"]))
df<- as.data.frame(raw[,"Zip"])

# GET PLACE IDS

place_ids <- c()
for (i in seq_len(size)) {
  response <- GET(url="https://maps.googleapis.com/maps/api/place/findplacefromtext/json",
                  query = list (
                    key = "",
                    input = paste(raw[i,2], raw[i,5], "WI"),
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
write.csv(details, "~/Downloads/Landfills/WI_details.csv")


# GET DETAILS
# hours, website, and phone

for (i in seq_len(size)) {
  if (!is.na(place_ids[i])) {
    response <- GET(url="https://maps.googleapis.com/maps/api/place/details/json",
                    query = list(
                      key = "",
                      place_id = place_ids[i],
                      fields = "website,opening_hours,formatted_phone_number"
                    ))
    content <- content(response)

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
write.csv(details, "~/Downloads/Landfills/WI_details.csv")


# TYPES OF WASTE

types <- original[,18]

details[,18:36] <- ""
for (i in seq_along(types)) {
  if (types[i] != "") {
    line <- strsplit(types[i], split=";")

    details[i,18] <- accepts("GARBAGE", line)
    details[i,19] <- accepts("electronic", line)
    details[i,20] <- accepts("APPLIANCES", line)
    details[i,21] <- accepts("(BRUSH|YARD|WOOD)", line)
    details[i,22] <- accepts("GARBAGE", line)
    details[i,23] <- accepts("(CONSTRUCTION|DEMOLITION)", line)
    details[i,24] <- accepts("METAL|REFUSE", line)
    details[i,25] <- accepts("(non-ferrous|RECYCLABLE|REFUSE)", line)
    details[i,26] <- accepts("(aluminum|ferrous|RECYCLABLE|REFUSE)", line)
    details[i,27] <- accepts("(plastic|RECYCLABLE|REFUSE)", line)
    details[i,28] <- accepts("(glass|RECYCLABLE|REFUSE)", line)
    details[i,29] <- accepts("(PAPER|RECYCLABLE|REFUSE)", line)
    details[i,30] <- accepts("(CARDBOARD|RECYCLABLE|REFUSE)", line)
    details[i,31] <- accepts("OIL", line)
    details[i,32] <- accepts("OIL", line)
    details[i,33] <- accepts("batteries", line)
    details[i,36] <- accepts("TIRES", line)
  }
}
# Function - whether waste type is accepted
accepts <- function(pattern, line) {
  matches <- grepl(pattern, line[[1]], ignore.case=TRUE)
  if (sum(matches) != 0) {
    return("Yes")
  }
  else return("No")
}
