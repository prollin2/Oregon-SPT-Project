library(tidyverse)
library(ggplot2)

## Cleaning Win/Loss into 2 columns
d21 <- read.csv("C:/Users/proll/OneDrive/Documents/data_wrangling/College Football/cfb21.csv")
d22 <- read.csv("C:/Users/proll/OneDrive/Documents/data_wrangling/College Football/cfb22.csv")

d21 <- d21 %>% 
  separate(Win.Loss,c('Win','Loss'),sep= "-")

d22 <- d22 %>% 
  separate(Win.Loss,c('Win','Loss'),sep= "-") 

write.csv(d21,"C:/Users/proll/OneDrive/Documents/data_wrangling/College Football//cfb21.csv",row.names = FALSE)
write.csv(d22,"C:/Users/proll/OneDrive/Documents/data_wrangling/College Football//cfb22.csv",row.names = FALSE)



## Read in data files

data_files <- list.files(
  "C:/Users/proll/OneDrive/Documents/data_wrangling/College Football",
  pattern = ".csv", full.names = TRUE)

## Season Numbers
results <- lapply(data_files, function(x) {
  data_in <- read.csv(x)
  season_name <- stringr::str_extract(x, "[0-9]+")
  data_in$season <- season_name
  return(data_in)
}
)

## Bind data files
all_results <- data.table::rbindlist(results, fill = TRUE)




## Make Conference into its own column
all_results$Conference <- stringr::str_extract(all_results$Team, "(?<=\\().*(?=\\))")
all_results$Team <- stringr::str_extract(all_results$Team, ".*(?= \\()")


## Creating Special Teams dataframe
spt_data <- all_results[,c('season','Team','Conference','Win','Loss','Kickoff.Touchbacks','Avg.Yards.per.Kickoff.Return.Allowed','Avg.Yard.per.Kickoff.Return','Opp.Net.Punt.Return.Yards','Avg.Yards.Per.Punt.Return','Opp.Feild.Goals.Made','Opp.Field.Goals.Made','Feild.Goals','Field.Goals','PAT','Points.Per.Game','Avg.Points.per.Game.Allowed')]

## Merging two columns
spt_data$field_goals <- dplyr::coalesce(spt_data$Feild.Goals, spt_data$Field.Goals)
spt_data$opp_field_goals <- dplyr::coalesce(spt_data$Opp.Feild.Goals.Made, spt_data$Opp.Field.Goals.Made)


## Remove FG columns
spt_data <- subset(spt_data, select = -c(Opp.Feild.Goals.Made,Opp.Field.Goals.Made,Feild.Goals,Field.Goals))
spt_data <- spt_data[spt_data$season >= 16]

## Make columns Numeric
spt_data$Win <- as.numeric(spt_data$Win)
spt_data$Loss <- as.numeric(spt_data$Loss)
spt_data$Kickoff.Touchbacks <- as.numeric(spt_data$Kickoff.Touchbacks)
spt_data$Opp.Net.Punt.Return.Yards <- as.numeric(spt_data$Opp.Net.Punt.Return.Yards)
spt_data$PAT <- as.numeric(spt_data$PAT)
spt_data$field_goals <- as.numeric(spt_data$field_goals)
spt_data$opp_field_goals <- as.numeric(spt_data$opp_field_goals)

## Find Oregon specific summary stats
oregon_spt <- spt_data[spt_data$Team == 'Oregon']

## Find conference summary stats
pac_12 <- spt_data[spt_data$Conference == 'Pac-12']

power_5 <- spt_data[spt_data$Conference == 'Pac-12'| spt_data$Conference == 'SEC'| spt_data$Conference == 'Big Ten'| spt_data$Conference == 'ACC'| spt_data$Conference == 'Big-12']




## 2022 Season Data

twenty_two <- pac_12[pac_12$season == '22']
twenty_two[2,2] = 'USC'

## FBS Correlation
library(ggplot2)
library(ggcorrplot)
FBS_corr <- subset(spt_data, select = -c(season, Team, Conference))
fbs_corr <- round(cor(FBS_corr),1)
ggcorrplot(fbs_corr, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of FBS SPT Stats", 
           ggtheme=theme_bw)

## Graphing Comparisons
touchbacks <- ggplot(data=twenty_two, mapping = aes(x= reorder(Team, -Kickoff.Touchbacks), y = Kickoff.Touchbacks, fill = Team)) +
  geom_bar(stat = "identity", width = 0.5)+
  geom_text(aes(label = Kickoff.Touchbacks), vjust = -0.25)+
  theme_minimal()+
  labs(x = "Pac-12 Team", y = "# of Touchbacks")+
  scale_fill_manual(values = c("Arizona" = "grey80",
                               "Arizona St." = "grey80",
                               "California" = "grey80",
                               "Colorado" = "grey80",
                               "Oregon" = "green",
                               "Oregon St." = "grey80",
                               "USC" = "grey80",
                               "Stanford" = "grey80",
                               "UCLA" = "grey80",
                               "Utah" = "grey80",
                               "Washington" = "grey80",
                               "Washington St." = "grey80"))
print(touchbacks)

##
opp_punt_return <- ggplot(data=twenty_two, mapping = aes(x= reorder(Team, +Avg.Yards.Per.Punt.Return), y = Avg.Yards.Per.Punt.Return, fill = Team)) +
  geom_bar(stat = "identity", width = 0.5)+
  geom_text(aes(label = Avg.Yards.Per.Punt.Return), vjust = -0.25)+
  theme_minimal()+
  labs(x = "Pac-12 Team", y = "AVG Yards per Punt Return")+
  scale_fill_manual(values = c("Arizona" = "grey80",
                               "Arizona St." = "grey80",
                               "California" = "grey80",
                               "Colorado" = "grey80",
                               "Oregon" = "green",
                               "Oregon St." = "grey80",
                               "USC" = "grey80",
                               "Stanford" = "grey80",
                               "UCLA" = "grey80",
                               "Utah" = "grey80",
                               "Washington" = "grey80",
                               "Washington St." = "grey80"))
print(opp_punt_return)

## Ranking Averages
## Create sequence fom greatest to smallest for touchbacks
spt_rankings <- all_results[, c('Team','season' ,'Kickoff.Return.Rank', 'Punt.Return.Rank', 'Kickoff.Return.Def.Rank', 'Kickoff.Touchbacks')]
spt_rankings <- spt_rankings[spt_rankings$season == 22]
spt_rankings$Touchbacks_Rank <- round(rank(-spt_rankings$Kickoff.Touchbacks),0)
spt_rankings$Touchbacks_Rank <- as.numeric(spt_rankings$Touchbacks_Rank)
spt_rankings <- subset(spt_rankings, select = -c(Kickoff.Touchbacks))
spt_rankings$Avg_Rank <- round(rowMeans(spt_rankings[, c(3,6)], na.rm = TRUE),0)
spt_rankings$SPT_Rank <- round(rank(spt_rankings$Avg_Rank),0)


## Top 10 teams & Oregon's ranking

top_10 <- spt_rankings[spt_rankings$SPT_Rank < 10 | spt_rankings$Team == 'Oregon']

p5_plot <- ggplot(data=top_10, aes(x= reorder(Team,-SPT_Rank), y=SPT_Rank, fill = Team))+
  geom_bar(stat = 'identity', width = 0.5)+
  geom_text(aes(label = SPT_Rank), hjust = -0.25)+
  labs(x = "FBS Team", y = "SPT Rank")+
  coord_flip()+
  theme_minimal()+
  scale_fill_manual(values = c("Minnesota" = "grey80",
                               "South Carolina" = "grey80",
                               "North Carolina" = "grey80",
                               "Clemson" = "grey80",
                               "Florida St." = "grey80",
                               "Kansas St." = "grey80",
                               "Syracuse" = "grey80",
                               "Tulane" = "grey80",
                               "Louisville" = "grey80",
                               "Oregon" = "green"))
print(p5_plot)
