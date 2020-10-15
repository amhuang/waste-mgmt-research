library(httr, xml2)
library(rvest, dplyr)

details <- read.csv("~/Desktop/Landfills/MN_raw.csv", stringsAsFactors = FALSE)
details <- details[,]


# SCRAPING

# Get links from each page
links <- lapply(paste0("https://wastebits.com/locator/region/mississippi/p", 1:10), 
                function(url) {
                  url %>% read_html() %>%
                    html_nodes(".badge-target") %>%
                    html_attr("href")
                })

# Get waste types for each facility
wastetypes <- list()
for (i in seq_along(links)) {
  page <- lapply(links[[i]], 
                 function(link) {
                   link %>% read_html() %>%
                     html_nodes(".location-materials a") %>%
                     html_text()
                 })
  wastetypes <- append(wastetypes, page)
}

# Get names of each facility from wastebits
headernames <- list()
for (i in seq_along(links)) {
  page <- lapply(links[[i]], 
                 function(link) {
                   link %>% read_html() %>%
                     html_nodes(".location-header h1") %>%
                     html_text()
                 })
  headernames <- append(headernames, page)
}

# Get addresses from wastebits
addresses <- list()
for (i in seq_along(links)) {
  page <- lapply(links[[i]], 
                 function(link) {
                   link %>% read_html() %>%
                     html_nodes(".location-header") %>%
                     html_nodes(".address") %>%
                     html_text()
                 })
  addresses <- append(addresses, page)
}

# Check lengths
length(unlist(links))
length(wastetypes)
length(headernames)
length(addresses)


# SORT DETAILS INTO TEXT

for (i in seq_len(size)) {
  
  match <- grepl(headernames[[i]], details[,"Name"], ignore.case=TRUE)
  
  wb_name <- headernames[[i]] %>%
    strsplit(split=" ") %>% 
    unlist() %>%
    toString() %>%
    gsub(pattern=", ", replacement="|")
  
  wb_name_len <- wb_name %>% 
    strsplit(split="\\|") %>% 
    unlist() %>%
    length()
  
  # regex = "^(.*? )?(words) .*?(words) .*?(words)( .*)?$"
  if (wb_name_len > 4)
    pattern <- paste0("^((.*? )?(", wb_name ,")( |$)){", wb_name_len-3, "}.*$")
  else if (wb_name_len <= 4 && wb_name_len > 2)
    pattern <- paste0("^((.*? )?(", wb_name ,")( |$)){", wb_name_len-2, "}.*$")
  else if (wb_name_len <= 2)
    pattern <- paste0("^((.*? )?(", wb_name ,")( |$)){", wb_name_len, "}.*$")
  
  matchN <- grepl(pattern, details[,"Name"], ignore.case=TRUE)
  
  city <- addresses[[i]]
  city <- gsub(", MS.*", "", city)
  city <- gsub(".*, ", "", city)
    
  matchC <- grepl(city, details[,"City"], ignore.case=TRUE)
  match2 <- matchN & matchC
  
  if (sum(match2) > 0) {
    line <- wastetypes[[i]]
    details[match2,18] <- accepts("Municipal", line)
    details[match2,19] <- accepts("(electronics|e waste|television|monitor)", line)
    details[match2,20] <- accepts("(white goods|bulty waste)", line)
    details[match2,21] <- accepts("(yard|wood|tree)", line)
    details[match2,22] <- accepts("(construction|concrete|asbestos)", line)
    details[match2,23] <- accepts("(construction|demolition|asbestos|concrete|industrial)", line)
    details[match2,24] <- accepts("recyclables", line)
    details[match2,25] <- accepts("recyclables", line)
    details[match2,26] <- accepts("recyclables", line)
    details[match2,27] <- accepts("recyclables", line)
    details[match2,28] <- accepts("recyclables", line)
    details[match2,29] <- accepts("recyclables", line)
    details[match2,30] <- accepts("recyclables", line)
    details[match2,31] <- accepts("oil", line)
    details[match2,32] <- accepts("oil", line)
    details[match2,33] <- accepts("(batteries|special)", line)
    details[match2,34] <- accepts("(hhw|household hazardous|special)", line)
    details[match2,35] <- accepts("(hhw|household hazardous|special)", line)
    details[match2,36] <- accepts("tires", line)
  }
  
  if (sum(match) > 0) {
    line <- wastetypes[[i]]
    details[match,18] <- accepts("Municipal", line)
    details[match,19] <- accepts("(electronics|e waste|television|monitor)", line)
    details[match,20] <- accepts("(white goods|bulty waste)", line)
    details[match,21] <- accepts("(yard|wood|tree)", line)
    details[match,22] <- accepts("(construction|concrete|asbestos)", line)
    details[match,23] <- accepts("(construction|demolition|asbestos|concrete|industrial)", line)
    details[match,24] <- accepts("recyclables", line)
    details[match,25] <- accepts("recyclables", line)
    details[match,26] <- accepts("recyclables", line)
    details[match,27] <- accepts("recyclables", line)
    details[match,28] <- accepts("recyclables", line)
    details[match,29] <- accepts("recyclables", line)
    details[match,30] <- accepts("recyclables", line)
    details[match,31] <- accepts("oil", line)
    details[match,32] <- accepts("oil", line)
    details[match,33] <- accepts("(batteries|special)", line)
    details[match,34] <- accepts("(hhw|household hazardous|special)", line)
    details[match,35] <- accepts("(hhw|household hazardous|special)", line)
    details[match,36] <- accepts("tires", line)
  }
}


# Get transfer stations & landfills from Wastebits
match <- 102
for (i in seq_along(headernames)) {
  
  if( grepl("(Transfer|Landfill|Recycling|Convenience|Dropoff|Drop off|drop-off)", headernames[[i]], ignore.case=TRUE)[1] 
      && !(headernames[[i]][1] %in% details[,"Name"]) ) {
    details[match,"Name"] <- headernames[[i]]
    
    add <- addresses[[i]]
    details[match, "Street.Address"] <- gsub(" •.*", "", add)
    
    line <- wastetypes[[i]]
    details[match,18] <- accepts("Municipal", line)
    details[match,19] <- accepts("(electronics|e waste|television|monitor)", line)
    details[match,20] <- accepts("(white goods|bulty waste)", line)
    details[match,21] <- accepts("(yard|wood|tree)", line)
    details[match,22] <- accepts("(construction|concrete|asbestos)", line)
    details[match,23] <- accepts("(construction|demolition|asbestos|concrete|industrial)", line)
    details[match,24] <- accepts("recyclables", line)
    details[match,25] <- accepts("recyclables", line)
    details[match,26] <- accepts("recyclables", line)
    details[match,27] <- accepts("recyclables", line)
    details[match,28] <- accepts("recyclables", line)
    details[match,29] <- accepts("recyclables", line)
    details[match,30] <- accepts("recyclables", line)
    details[match,31] <- accepts("oil", line)
    details[match,32] <- accepts("oil", line)
    details[match,33] <- accepts("(batteries|special)", line)
    details[match,34] <- accepts("(hhw|household hazardous|special)", line)
    details[match,35] <- accepts("(hhw|household hazardous|special)", line)
    details[match,36] <- accepts("tires", line) 
    
    match = match +1
  }
}

# Function - whether waste type is accepted
accepts <- function(pattern, line) {
  matches <- grepl(pattern, line, ignore.case=TRUE)
  if (sum(matches) != 0) {
    return("Yes")
  }
  else return("No")
}

sum(details[,25] != "", na.rm=TRUE)

write.csv(details, "~/Desktop/Landfills/MS_details.csv")

