library(tidyverse)
####Data Preprocessing####0
setwd("/media/yiqiang/D/USF/622DataViz/final project/finalproject/dataviz/")
## Part 1: parse data to get map data
df <- read_csv("data/operations.csv")
map_data <- df %>% filter(is.na(Country) == FALSE & is.na(`Target Longitude`) == FALSE & is.na(`Takeoff Longitude`) == FALSE) %>%
        filter(`Takeoff Latitude` != '4248', `Takeoff Longitude` != 1355) %>%
          filter(`Target Longitude` > 0, `Takeoff Longitude` > 0, `Target Latitude` > 0, `Target Longitude` <=360) %>%
            mutate(id=1:n())
map_data$`Mission Date` <- as.Date(anytime::anydate(map_data$`Mission Date`))
map_data$start_week <- cut(map_data$`Mission Date`, "week")
flight_route <- gcIntermediate(map_data[ , c("Takeoff Longitude", "Takeoff Latitude")],
                               map_data[ , c("Target Longitude", "Target Latitude")],
                               n=20,
                               addStartEnd=TRUE,
                               sp=TRUE)
write.csv(map_data, file = "data/simplified_data.csv", row.names = FALSE)
save(flight_route, file="data/flight_route.rda")

## Part 2: parse data to get obs count ts data
Mission <-  df %>% select(`Mission Date`, `Mission ID`, `Theater of Operations`, `Aircraft Series`) %>%
  filter(is.na(`Mission Date`) == F, is.na(`Theater of Operations`) != TRUE, `Theater of Operations` != 'MADAGASCAR')
Mission$`Mission Date` <- as.Date(anytime::anydate(Mission$`Mission Date`))
Mission$start_month <- cut(Mission$`Mission Date`, "month")
Mission$start_month = as.Date(Mission$start_month)

Mission[Mission$`Theater of Operations` == 'ETO', "Theater of Operations"] <- 'Euro'
Mission[Mission$`Theater of Operations` == 'PTO', "Theater of Operations"] <- 'Pacific'
Mission[Mission$`Theater of Operations` == 'MTO', "Theater of Operations"] <- 'Mediterranean'
Mission[Mission$`Theater of Operations` == 'CBI', "Theater of Operations"] <- 'Pacific'
Mission[Mission$`Theater of Operations` == 'EAST AFRICA', "Theater of Operations"] <- 'Africa'
monthly_ct <- Mission %>% group_by(start_month, `Theater of Operations`) %>% summarise('ct' = n())
total_monthly_ct <- Mission %>% group_by(start_month) %>% summarise('ct' = n())
total_monthly_ct$`Theater of Operations` <- 'Total'
monthly_ct <- bind_rows(monthly_ct, total_monthly_ct)

write.csv(monthly_ct, file = "data/monthly_obs_count.csv", row.names = FALSE)

## Part 3: chorddiag data
diag_data <- df %>% select(`Target Country`, Country) %>% group_by(., `Target Country`, Country) %>% 
  count() %>% filter(., is.na(Country) == FALSE, is.na(`Target Country`) == FALSE, n>100, 
                     `Target Country` != 'UNKNOWN OR NOT INDICATED')
library(igraph)
diag_data <- as.data.frame(diag_data)
diag_data[, 1] <- as.character(diag_data[, 1])
diag_data[, 2] <- as.character(diag_data[, 2])
diag_data[, 3] <- as.numeric(diag_data[, 3])
diag_data <- as.matrix(diag_data)
diag_net <- igraph::graph.edgelist(diag_data[,1:2])
E(diag_net)$weights <- as.numeric(diag_data[,3])
ajmatrix <- get.adjacency(diag_net,attr='weights', sparse = FALSE)
Occupied <- toupper(c("Albania", "Belgium", "Czechoslovakia", "Denmark", "Estonia", 
                      "Ethiopia", "France", "Greece", "Luxemburg", "Netherlands",
                      "Norway", "Philippine Islands", "Poland", "Yugoslavia"))
Axis <- toupper(c("Bulgaria", "Finland", "Germany", "Hungary", "Italy", "Japan", "Romania"))
Allies <- toupper(c("Argentina", "Australia", "Bolivia", "Brazil", "Canada", "China",
                    "Chile", "Columbia", "Costa Rica", "Cuba", "France", "India", "Iraq", 
                    "Lebanon", "Mexico", "New Zealand", "Paraguay", "South Africa", "Soviet Union",
                    "Great Britain", "USA"))
Neutral <- toupper(c("Andorra", "Ireland", "Liechtenstein", "Portugal", "Spain", "Sweden",
                     "Switzerland", "Turkey", "Uruguay", "Vatican City"))
groupColors = c()
for(country in dimnames(ajmatrix)[[1]]){
  if(country %in% Occupied){
    groupColors = c(groupColors, "#636363")
  }else if(country %in% Axis){
    groupColors = c(groupColors, "#25338a")
  }else if (country %in% Allies){
    groupColors = c(groupColors, "#9ecae1")
  }else if (country %in% Neutral){
    groupColors = c(groupColors, "#bdbdbd")
  }else{
    groupColors = c(groupColors, "#bdbdbd")
  }
}
save(ajmatrix, file = "data/ajmatrix.rda")
save(groupColors, file="data/groupColors.rda")

## Part 4: Country - Target Country
attack_data <- df %>% filter(is.na(Country) != T, is.na(`Target Country`) != T, is.na(`Target City`) != T) %>% 
  select(Country, `Target Country`, `Target City`)
write.csv(attack_data, file = "data/attack_data.csv", row.names = FALSE)

## Part 5: Aircraft Series Count
as_ct <- Mission %>% filter(., is.na(`Aircraft Series`) != T, `Aircraft Series` %in% c('B24', 'B17', 'B25', 'B26', 'A20', 'GB17')) %>% 
  group_by(`Aircraft Series`, `Theater of Operations`) %>% 
  count() %>% reshape2::dcast(., `Aircraft Series` ~ `Theater of Operations`, value.var = "n")
as_ct[is.na(as_ct)] <- 0
write.csv(as_ct, file = "data/aircraft_data.csv", row.names = FALSE)
