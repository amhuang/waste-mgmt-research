library(magrittr)
library(dplyr)

census <- read.csv("~/Desktop/Landfills/-General/census.csv")

states <- census[,"state_id"] %>% unique

# Facilities by County
county_fac_list <- lapply(states, by_county)
county_fac_count <- do.call(rbind, county_fac_list)

county_fac_prop <- county_fac_count %>%
  mutate(LandfillProp = Population/Landfills,
         TransferProp = Population/Transfer,
         RecyclingProp = Population/Recycling,
         OtherProp = Population/Other)
  
county_fac_prop[county_fac_prop == Inf] <- NA

names(county_fac_prop) <- c('State','County','Population','Landfills',
                           'Transfer Stations/Convenience Centers','Recycling Centers',
                           'Other', 'Persons per Landfill',
                           'Persons per Smaller facility','Persons per Recycling Center',
                           'Persons per Other facility')

write.csv(county_fac_prop, "~/Desktop/Landfills/-General/Facilities by County.csv")

# Facilities by State
state_fac_list <- lapply(states, by_state)
state_fac_count <- do.call(rbind, state_fac_list)

state_fac_prop <- state_fac_count %>%
  mutate(LandfillProp = Population/Landfills,
         TransferProp = Population/Transfer,
         RecyclingProp = Population/Recycling,
         OtherProp = Population/Other)

state_fac_prop[state_fac_prop == Inf] <- NA

names(state_fac_prop) <- c('State','Population','Landfills',
                           'Transfer Stations/Convenience Centers','Recycling Centers',
                           'Other', 'Persons per Landfill',
                           'Persons per Smaller facility','Persons per Recycling Center',
                           'Persons per Other facility')

write.csv(state_fac_prop, "~/Desktop/Landfills/-General/Facilities by State.csv")


# FUNCTIONS 

# Returns df with facility counts by county
by_state <- function(id) {
  # Calculate population by state
  census_state <- census %>%
    select(state_id, county_name, city, population, zips) %>%
    filter(state_id == id)
  state_pop <- aggregate(census_state$population, census_state['state_id'], sum, na.rm=TRUE)
  
  # New df (state_dat) with pop and facility counts
  blank <- data.frame(matrix(data=NA, ncol=4, nrow=1))
  state_dat <- cbind(state_pop, blank)
  
  names(state_dat) <- c('State','Population','Landfills',
                        'Transfer','Recycling',
                        'Other')
  
  waste_dat <- read.csv(paste("~/Desktop/Landfills/-General/csvbystate/", id, ".csv", sep=""))
  
  types <- c('Landfill','(Transfer|Collection|Convenience)',
             'Recycling', '^(?!(Landfill|Transfer|Collection|Convenience|Recycling)+|^$)')
  
  # Counts how many and inserts into state_dat
  colnum <- 3
  for (type in types) {
    state_dat[,colnum] <- count_types(waste_dat, type)
    colnum = colnum + 1
  }
  return(state_dat)
}

# Returns df with facility counts by county
by_county <- function(id) {
  # Calculate population per county
  census_state <- census %>%
    select(state_id, county_name, city, population,zips) %>%
    filter(state_id == id)
  county_pop <- aggregate(census_state$population, census_state['county_name'], sum, na.rm=TRUE)
  
  # New df (state_dat) with pop and facility counts
  state_dat <- cbind(rep(id), county_pop)
  blank <- data.frame(matrix(data=NA, ncol=4, nrow=nrow(state_dat)))
  state_dat <- cbind(state_dat, blank)
  
  names(state_dat) <- c('State','County','Population','Landfills',
                        'Transfer','Recycling',
                        'Other')
  
  waste_dat <- read.csv(paste0("~/Desktop/Landfills/-General/csvbystate/", id, ".csv"))
  
  types <- c('Landfill','(Transfer|Collection|Convenience)',
             'Recycling', '^(?!(Landfill|Transfer|Collection|Convenience|Recycling)+|^$)')

  for (i in seq_along(state_dat$County)) {
    # Gets info on all facilities in a county
    county_fac <- waste_dat %>%
      filter(County==state_dat$County[i])
    
    # Counts how many and inserts into state_dat
    colnum <- 4
    for (type in types) {
      state_dat[i,colnum] <- count_types(county_fac, type)
      colnum = colnum + 1
    }
  }
  return(state_dat)
}

# Counts how many of a certain type of facility
count_types <- function(waste_df, type) {
  count <- waste_df %>%
    filter(grepl(type, waste_df$Category, ignore.case=TRUE, perl=TRUE)) %>%
    nrow
  return(count)
}

