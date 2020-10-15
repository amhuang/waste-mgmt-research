library(httr)
library(xml2)
library(rvest)
library(dplyr)
library(pander)
library(stringr)

# WEB SCRAPING
# Get links from main website
raw <- read.csv("~/Downloads/Landfills/UT_raw.csv")[1:311,]
size <- nrow(raw)
write.csv(raw, "~/Downloads/Landfills/UT_raw.csv")

target_url <- "https://deq.utah.gov/waste-management-and-radiation-control/solid-waste-facilities-permits-and-permitting"
main <- read_html(target_url)

links <- main %>%
  html_nodes("body") %>%
  html_nodes(".entry-content li") %>%
  html_nodes("a") %>%
  html_attr("href")

# Scrape individual links

df <- as.data.frame(matrix(rep(NA), ncol = 5, nrow = length(links)))
names(df) <- c("Name", "Location", "Waste Accepted",
               "Waste Excluded", "Facility Contact")

for (i in seq_along(links)) {
  read <- read_html(links[i])

  title <- read %>%
    html_nodes(".article-header") %>%
    html_nodes("h1") %>%
    html_text()

  title <- gsub(" Fact Sheet.*", "" , title)
  df[i, "Name"] <- title

  parsed <- read %>%
    html_nodes(".entry-content") %>%
    html_nodes("h3,p")

  for (j in seq_along(parsed)) {
    if (xml_text(parsed[[j]]) == "Facility Location") {
      df[i, "Location"] <- xml_text(parsed[[j+1]])
    }
    if (xml_text(parsed[[j]]) == "Waste Accepted") {
      df[i, "Waste Accepted"] <- xml_text(parsed[[j+1]])
    }
    if (xml_text(parsed[[j]]) == "Waste Excluded") {
      df[i, "Waste Excluded"] <- xml_text(parsed[[j+1]])
    }
    if (xml_text(parsed[[j]]) == "Facility Contact") {
      df[i, "Facility Contact"] <- xml_text(parsed[[j+1]])
    }
  }
}

raw[,"Residential.Household.Garbage"] <- NA
raw[,"Ownership..Government.Private"] <- df[,"Waste Accepted"]


# COORDINATES TO GET COUNTIES
# Getting coords from geodata lined up w raw
raw <- read.csv("~/Downloads/Landfills/UT_raw.csv")[1:309,]
geodata <- read.csv("~/Downloads/Landfills/UT geodata.csv")[1:283,]

for (i in seq_len(nrow(raw))) {
  where <- grep(raw[i,"Name"], geodata[,"FACILITY_NAME"], ignore.case = TRUE)
  if(length(where) != 0) {
    raw[i,"Lat"] <- geodata[where, "LATITUDE"][1]
    raw[i,"Long"] <- geodata[where, "LONGITUDE"][1]
  }
}

# Using lat long to retrieve
for (i in seq_len(nrow(raw))) {
  if (!is.na(raw[i,"Lat"])) {
    response <- GET(url="https://maps.googleapis.com/maps/api/place/textsearch/json",
                    query = list (

                      key = "",
                      location = paste(raw[i,"Lat"], raw[i,"Long"], sep=","),
                      radius = 100
                    )
    )
    content <- content(response)

    for (j in seq_along(content$result$address_components)) {
      level <- content$result$address_components[[j]]$types[[1]]
      if (level == "locality") {
        raw[i,"City"] <- content$result$address_components[[j]]$short_name
      }
      if (level == "administrative_area_level_2") {
        raw[i,"County"] <- content$result$address_components[[j]]$short_name
      }
      if (level == "postal_code") {
        raw[i,"Zip"] <- content$result$address_components[[j]]$short_name
      }
    }
  }
}

# GET PLACE IDS

place_ids <- c()

for (i in seq_len(size)) {
  response <- GET(url="https://maps.googleapis.com/maps/api/place/findplacefromtext/json",
                  query = list (
                    key = "",
                    input = paste(raw[i,3], raw[i,5], "Utah"),
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
write.csv(details, "~/Downloads/Landfills/UT_details.csv")
details <- details[1:50]

# GET DETAILS
# hours, website, and phone

details <- read.csv("~/Downloads/Landfills/UT_raw.csv")[,2:50]
place_ids <- details[,"place_ids"]

for (i in seq_len(size)) {
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
write.csv(details, "~/Downloads/Landfills/UT_details.csv")


# TYPES OF WASTE

types <- details[,3]

details[,18:36] <- ""
for (i in seq_along(types)) {
  if (types[i] != "") {
    line <- strsplit(types[i], split=";")

    details[i,18] <- accepts("municipal", line)
    details[i,19] <- accepts("special", line)
    details[i,20] <- accepts("(commercial|municipal)", line)
    details[i,21] <- accepts("(yard)", line)
    details[i,22] <- accepts("construction", line)
    details[i,23] <- accepts("(construction|demolition)", line)
    #details[i,24] <- accepts("METAL|REFUSE", line)
    #details[i,25] <- accepts("(non-ferrous|RECYCLABLE|REFUSE)", line)
    #details[i,26] <- accepts("(aluminum|ferrous|RECYCLABLE|REFUSE)", line)
    #details[i,27] <- accepts("(plastic|RECYCLABLE|REFUSE)", line)
    #details[i,28] <- accepts("(glass|RECYCLABLE|REFUSE)", line)
    #details[i,29] <- accepts("(PAPER|RECYCLABLE|REFUSE)", line)
    #details[i,30] <- accepts("(CARDBOARD|RECYCLABLE|REFUSE)", line)
    details[i,31] <- accepts("oil", line)
    details[i,32] <- accepts("oil", line)
    details[i,33] <- accepts("special", line)
    details[i,34] <- accepts("special", line)
    details[i,35] <- accepts("special", line)
    details[i,36] <- accepts("tire", line)
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
