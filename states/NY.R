# FORMATTING WASTE TYPES

original <- read.csv("~/Downloads/Landfills/NY_raw.csv", stringsAsFactors = FALSE)[1:671,]
# waste types are 18:36
# current waste data in "website" [10]
names(original)
original[,18:36] <- "No"

types <- original[,10]

for (i in seq_along(types)) {
  line <- strsplit(types[i], split=";")

  original[i,18] <- accepts("msw", line)
  original[i,19] <- accepts("electronic", line)
  original[i,20] <- accepts("white good", line)
  original[i,21] <- accepts("(yard waste|tree|brush|branches|leaves|stumps)", line)
  original[i,22] <- accepts("msw", line)
  original[i,23] <- accepts("(construction|demolition)", line)
  original[i,24] <- accepts("metal", line)
  original[i,25] <- accepts("(non-ferrous|single stream|source separated)", line)
  original[i,26] <- accepts("(aluminum|ferrous|single stream|source separated)", line)
  original[i,27] <- accepts("(plastic|single stream|source separated)", line)
  original[i,28] <- accepts("(glass|single stream|source separated)", line)
  original[i,29] <- accepts("(paper|single stream|source separated)", line)
  original[i,30] <- accepts("(cardboard|paperboard|single stream|source separated)", line)
  original[i,31] <- accepts("waste oil", line)
  original[i,32] <- accepts("waste oil", line)
  original[i,33] <- accepts("batteries", line)
  original[i,36] <- accepts("tire", line)
}

# Mark landfills with nothing as NA
is_useful <- c()
for (i in seq_along(types)) {
  num_nos <- sum (original[i,18:36] == "No")
  if (num_nos == 19) {
    is_useful[i] <- FALSE
  }
  else is_useful[i] <- TRUE
}
sum(is_useful)
original[!is_useful, 18:36] <- NA

# Function - whether waste type is accepted
accepts <- function(pattern, line) {
  matches <- grepl(pattern, line[[1]], ignore.case=TRUE)
  if (sum(matches) != 0) {
    return("Yes")
  }
  else return("No")
}

write.csv(original, "~/Downloads/Landfills/NY_wastetypes.csv")


# GETTING OPENING HOURS

# Getting place IDs

library(httr)

place_ids <- c()
details <- read.csv("~/Downloads/Landfills/NY_details.csv", stringsAsFactors = FALSE)[1:595,3:50]
size <- nrow(details)

for (i in seq_len(size)) {
  response <- GET(url="https://maps.googleapis.com/maps/api/place/findplacefromtext/json",
                query = list (
                  key = "",
                  input = paste(details[i,1], details[i,4], "NY"),
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
write.csv(details, "~/Downloads/Landfills/NY_details.csv")

# Obtain details with place IDs

details <- read.csv("~/Downloads/Landfills/NY_details.csv", stringsAsFactors = FALSE)
place_ids <- details[ ,50]

for (i in seq_len(size)) {
  if (!is.na(place_ids[i])) {

    response <- GET(url="https://maps.googleapis.com/maps/api/place/details/json",
                   query = list(
                     key = "",
                     place_id = place_ids[i],
                     fields = "name,website,opening_hours"
                   ))
    content <- content(response)

    if (!is.null(content$result$website))
      details[i, "Website"] <- content$result$website
    if (!is.null(content$result$opening_hours$weekday_text[[1]]))
      details[i, 11] <- content$result$opening_hours$weekday_text[[1]]
    if (!is.null(content$result$opening_hours$weekday_text[[2]]))
      details[i, 12] <- content$result$opening_hours$weekday_text[[2]]
    if (!is.null(content$result$opening_hours$weekday_text[[3]]))
      details[i, 13] <- content$result$opening_hours$weekday_text[[3]]
    if (!is.null(content$result$opening_hours$weekday_text[[4]]))
      details[i, 14] <- content$result$opening_hours$weekday_text[[4]]
    if (!is.null(content$result$opening_hours$weekday_text[[5]]))
      details[i, 15] <- content$result$opening_hours$weekday_text[[5]]
    if (!is.null(content$result$opening_hours$weekday_text[[6]]))
      details[i, 16] <- content$result$opening_hours$weekday_text[[6]]
    if (!is.null(content$result$opening_hours$weekday_text[[7]]))
      details[i, 10] <- content$result$opening_hours$weekday_text[[7]]
  }
}
write.csv(details, "~/Downloads/Landfills/NY_details.csv")
