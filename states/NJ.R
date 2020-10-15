# Formatting waste types
original <- read.csv("~/Downloads/Landfills/NJ_unformatted.csv")[1:64,1:20]

types <- original[,7]
size <- length(types)
output <- as.data.frame(matrix(data="No",ncol=12,nrow=size),
                        stringsAsFactors=FALSE)
colnames <- names(original)[9:20]
names(output) <- colnames

for (i in seq_len(size)) {
  str <- as.character(types[i])
  split <- strsplit(str, split=",")[[1]]
  split <- as.vector(split)

  # Convert numbers to yes/nos in output
  for (j in seq_along(split)) {
    if (split[j] == "10") {
      output[i, 1] <- "Yes" # household
    }
    if (split[j] == "13") {
      output[i, 3] <- "Yes" # furniture
      output[i, 5] <- "Yes" # res constr
      output[i, 12] <- "Yes" # tires
    }
    if (split[j] == "13C") {
      output[i,6] <- "Yes" # c&d
    }
    if (split[j] == "23") {
      output[i, 4] <- "Yes" # brush
    }
    if (split[j] == "27") {
      output[i, 10] <- "Yes" # antifreeze
      output[i, 11] <- "Yes" # paint
    }
  }
}
original[1:64,9:20] <- output
write.csv(original, "~/Downloads/Landfills/NJ_unformatted.csv")


# Getting place IDs

library(httr)

place_ids <- c()
details <- read.csv("~/Downloads/Landfills/NJ_raw.csv")[1:173,1:6]

for (i in seq_len(nrow(details))) {
  places <- GET(url="https://maps.googleapis.com/maps/api/place/findplacefromtext/json",
                query = list (
                  key = "",
                  input = paste(details[i,1], details[i,3], "NJ"),
                  inputtype = "textquery",
                  fields = "place_id")
  )
  content <- content(places)

  if (length(content$candidates) == 0)
    place_ids[i] <- NA

  if (length(content$candidates) != 0)
    place_ids[i] <- content$candidates[[1]]$place_id
}

# Obtain details with place IDs
size <- length(place_ids)
col_names <- c("Name", "Address","City","Zip","State","Phone","Website","Sunday", "Monday","Tuesday",
               "Wednesday","Thursday","Friday","Saturday","Place ID")
output = as.data.frame(matrix(data=NA, ncol=length(col_names), nrow=size))
names(output) <- col_names
output[,"Place ID"] <- place_ids

for (i in seq_along(place_ids)) {

  if (!is.na(place_ids[i])) {

    details <- GET(url="https://maps.googleapis.com/maps/api/place/details/json",
                   query = list(
                     key = "",
                     place_id = place_ids[i],
                     fields = "name,formatted_address,formatted_phone_number,website,opening_hours"
                   ))
    content <- content(details)

    if (!is.null(content$result$name))
      output[i, "Name"] <- content$result$name
    if (!is.null(content$result$formatted_address))
      output[i, "Address"] <- content$result$formatted_address
    if (!is.null(content$result$formatted_phone_number))
      output[i, "Phone"] <- content$result$formatted_phone_number
    if (!is.null(content$result$website))
      output[i, "Website"] <- content$result$website
    if (!is.null(content$result$opening_hours$weekday_text[[1]]))
      output[i, "Monday"] <- content$result$opening_hours$weekday_text[[1]]
    if (!is.null(content$result$opening_hours$weekday_text[[2]]))
      output[i, "Tuesday"] <- content$result$opening_hours$weekday_text[[2]]
    if (!is.null(content$result$opening_hours$weekday_text[[3]]))
      output[i, "Wednesday"] <- content$result$opening_hours$weekday_text[[3]]
    if (!is.null(content$result$opening_hours$weekday_text[[4]]))
      output[i, "Thursday"] <- content$result$opening_hours$weekday_text[[4]]
    if (!is.null(content$result$opening_hours$weekday_text[[5]]))
      output[i, "Friday"] <- content$result$opening_hours$weekday_text[[5]]
    if (!is.null(content$result$opening_hours$weekday_text[[6]]))
      output[i, "Saturday"] <- content$result$opening_hours$weekday_text[[6]]
    if (!is.null(content$result$opening_hours$weekday_text[[7]]))
      output[i, "Sunday"] <- content$result$opening_hours$weekday_text[[7]]
  }
}

write.csv(output, "~/Downloads/Landfills/NJ_details.csv")


# Splitting address into components

details <- read.csv("~/Downloads/Landfills/NJ_details.csv")[2:15]
details[,2] <- as.character(details[,2])

for (i in seq_len(nrow(details))) {
  # splitting address into 3 components

  if (!is.na(details[i, 2])) {
    format_adr <- as.character(details[i, 2])
    components <- strsplit(format_adr, split=", ")
    components <- components[[1]]

    if (length(components) == 4) {
      details[i,2] <- components[1]
      details[i,3] <- components[2]
      details[i,4] <- components[3]
    }

    if (length(components) < 4) {
      details[i,3] <- components[1]
      details[i,4] <- components[2]
    }

    # splitting zip from state
    state_zip <- details[i,4]
    zip_split <- strsplit(state_zip, split=" ")
    zip_split <- zip_split[[1]]

    details[i,4] <- zip_split[1]
    details[i,5] <- zip_split[2]
  }
}

# Append separated addresses to details
write.csv(details, "~/Downloads/Landfills/NJ_details.csv")
