original <- read.csv("~/Downloads/Landfills/NH.csv")
names(original)

# Get rid of unused landfills from downloaded data
for (i in seq_len(nrow(original))) {

  nos <- original[i, 29:46]
  is_no <- nos == "No"

  if (sum(is_no) == 18) {
    original[i,48] <- FALSE
    next
  }
  original[i,48] <- TRUE
}

clean <- original[ original[,48], ]
clean <- clean[1:312, 2:50]
write.csv(clean, "~/Downloads/Landfills/NH.csv")

# Get place ids by searching names, city, and NH

library(httr)

place_ids <- c()
clean <- read.csv("~/Downloads/Landfills/NH.csv")

for (i in seq_len(nrow(clean))) {
  places <- GET(url="https://maps.googleapis.com/maps/api/place/findplacefromtext/json",
                query = list (
                  key = "",
                  input = paste(clean[i,1], clean[i,4], clean[i,5]),
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
col_names <- c("name", "address","phone","website","Monday","Tuesday",
               "Wednesday","Thursday","Friday","Saturday","Sunday")
output = as.data.frame(matrix(data=NA, ncol=length(col_names), nrow=size))
names(output) <- col_names

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
      output[i, 1] <- content$result$name
    if (!is.null(content$result$formatted_address))
      output[i, 2] <- content$result$formatted_address
    if (!is.null(content$result$formatted_phone_number))
      output[i, 3] <- content$result$formatted_phone_number
    if (!is.null(content$result$website))
      output[i, 4] <- content$result$website
    if (!is.null(content$result$opening_hours$weekday_text[[1]]))
      output[i, 5] <- content$result$opening_hours$weekday_text[[1]]
    if (!is.null(content$result$opening_hours$weekday_text[[2]]))
      output[i, 6] <- content$result$opening_hours$weekday_text[[2]]
    if (!is.null(content$result$opening_hours$weekday_text[[3]]))
      output[i, 7] <- content$result$opening_hours$weekday_text[[3]]
    if (!is.null(content$result$opening_hours$weekday_text[[4]]))
      output[i, 8] <- content$result$opening_hours$weekday_text[[4]]
    if (!is.null(content$result$opening_hours$weekday_text[[5]]))
      output[i, 9] <- content$result$opening_hours$weekday_text[[5]]
    if (!is.null(content$result$opening_hours$weekday_text[[6]]))
      output[i, 10] <- content$result$opening_hours$weekday_text[[6]]
    if (!is.null(content$result$opening_hours$weekday_text[[7]]))
      output[i, 11] <- content$result$opening_hours$weekday_text[[7]]
  }
}

write.csv(output, "~/Downloads/Landfills/NH_details.csv")


# Splitting address into components

details <- read.csv("~/Downloads/Landfills/NH_details.csv")[2:12]

col_names <- c("street", "city", "state", "zip")
split_adr <- as.data.frame(matrix(data=NA, ncol=length(col_names), nrow=size))
names(split_adr) <- col_names

for (i in seq_len(nrow(details))) {
  # splitting address into 3 components
  if (!is.na(details[i, 2])) {
    format_adr <- as.character(details[i, 2])
    components <- strsplit(format_adr, split=", ")
    components <- components[[1]]

    if (length(components) == 4) {
      split_adr[i,1] <- components[1]
      split_adr[i,2] <- components[2]
      split_adr[i,3] <- components[3]
    }

    if (length(components) < 4) {
      split_adr[i,2] <- components[1]
      split_adr[i,3] <- components[2]
    }

    # splitting zip from state
    state_zip <- split_adr[i,3]
    zip_split <- strsplit(state_zip, split=" ")
    zip_split <- zip_split[[1]]

    split_adr[i,3] <- zip_split[1]
    split_adr[i,4] <- zip_split[2]
  }
}

# Append separated addresses to details
final <- cbind(details, split_adr)
write.csv(final, "~/Downloads/Landfills/NH_details.csv")
