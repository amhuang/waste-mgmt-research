library(httr)
library(xml2)
library(dplyr)
library(rvest)
details <- read.csv("~/Desktop/Landfills/PA_raw.csv", stringsAsFactors = FALSE)

# SCRAPING - links, names, and addresses of facilities

pages <- paste0("https://recyclesearch.com/profile/padep-facility-directory/places?page=", 1:2, "&q=Solid%20Waste")

rec_addr <- lapply(pages, function(url) {
  url %>% read_html() %>%
    html_nodes(".address") %>% html_text() %>% 
    gsub(pattern=", ", replacement=",",) %>%
    gsub(pattern=",USA", replacement="")
}) %>% unlist()

rec_links <- lapply(pages, function(url) {
  prof <- url %>% read_html() %>%
    html_nodes(".container .result_name a") %>%
     html_attr("href") %>% unlist()
     return(paste0("https://recyclesearch.com",prof)) 
  }) %>% unlist()

rec_names <- lapply(pages, function(url) {
  url %>% read_html() %>%
    html_nodes(".container .result_name span") %>%
    html_text()
  }) %>% unlist()

state <- grepl("PA", rec_addr)
recaddr <- rec_addr[state]
reclinks <- rec_links[state]
recnames <- rec_names[state]


# INSERT STATE FACILITY DATA

insert <- 343
for (i in seq_along(reclinks)) {
  split <- strsplit(recaddr[i], split=",")[[1]]
  cityzip <- toString(split[2:length(split)]) %>% 
    gsub(pattern=", ", replacement=",")
  
  match <- grepl(split[1], details[,"Street.Address"], ignore.case=TRUE)
  if (sum(match) == 0) {
    details[insert, "Website"] <- reclinks[i]
    details[insert, "Name"] <- recnames[i]
    details[insert, "Street.Address"] <- split[1]
    details[insert, "City"] <- cityzip
    insert <- insert + 1
  }
  else if (sum(match) > 0) {
    details[match, "Website"] <- reclinks[i]
  }
}


# SCRAPE & PARSE WASTE DETAILS

links <- details[, "Website"]
for (i in seq_along(links)) {
  if (grepl("recyclesearch.com", links[i])) {
    details[i, 18:26] <- "No"
    
    line <- links[i] %>% read_html() %>% 
      html_nodes(".tab-content .material_category_list p") %>%
      html_text(trim=TRUE)
    
    details[i,18] <- accepts("(msw|solid waste)", line)
    details[i,19] <- accepts("(electronic)", line)
    details[i,20] <- accepts("(furniture)", line)
    details[i,21] <- accepts("(wood|clippings|brush|branches)", line)
    details[i,22] <- accepts("(construction|concrete|asbestos)", line)
    details[i,23] <- accepts("(construction|demolition|asbestos|concrete|industrial)", line)
    details[i,24] <- accepts("(metal|All Residential Recyclables)", line)
    details[i,25] <- accepts("(cans|All Residential Recyclables)", line)
    details[i,26] <- accepts("(aluminum|All Residential Recyclables)", line)
    details[i,27] <- accepts("(plastic|All Residential Recyclables)", line)
    details[i,28] <- accepts("(glass|All Residential Recyclables)", line)
    details[i,29] <- accepts("(paper|All Residential Recyclables)", line)
    details[i,30] <- accepts("(cardboard|All Residential Recyclables)", line)
    details[i,31] <- accepts("motor oil", line)
    details[i,32] <- accepts("(used oil|cooking oil)", line)
    details[i,33] <- accepts("(Batteries - Automotive)", line)
    details[i,34] <- accepts("(paint)", line)
    details[i,35] <- accepts("(antifreeze)", line)
    details[i,36] <- accepts("tires", line)
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

write.csv(details, "~/Desktop/Landfills/PA_details.csv")

