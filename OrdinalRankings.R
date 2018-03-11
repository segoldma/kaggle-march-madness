library(dplyr)
library(skimr)
library(readr)
library(corrplot)
library(ggplot2)

cities <- read_csv("Cities.csv")
conferences <- read_csv("Conferences.csv")
conf_tourney_games <- read_csv("ConferenceTourneyGames.csv")
game_cities <- read_csv("GameCities.csv")
ncaa_tourney_comact_results <- read_csv("NCAATourneyCompactResults.csv")
ncaa_tourney_detail_results <- read_csv("NCAATourneyDetailedResults.csv")
regular_season_compact_results <- read_csv("RegularSeasonCompactResults.csv")
regular_reason_detail_results <- read_csv("RegularSeasonDetailedResults.csv")
ncaa_tourney_seeds <- read_csv("NCAATourneySeeds.csv")
ncaa_tourney_slots <- read_csv("NCAATourneySlots.csv")
massey_ordinals <- read_csv("MasseyOrdinals.csv")
team_names <- read_csv("Teams.csv") %>% 
  select(TeamID, TeamName)


# How many systems are tracked by the Massey ordinals dataset
massey_ordinals %>% 
  group_by(SystemName) %>% 
  tally() %>% 
  arrange(desc(n))

# Add team names to the Ordinals dataset
massey_ordinals <- massey_ordinals %>% 
  left_join(team_names, by = "TeamID")

# Examine kenpom's 2017 data
pomeroy_ordinals_2017 <- massey_ordinals %>% 
  filter(Season == "2017" & SystemName == "POM")


pomeroy_ordinals_2017 %>% 
  filter(TeamID %in% ncaa_tourney_teams_2017$TeamID) %>% 
  ggplot(aes(x = RankingDayNum, y = OrdinalRank))+
  geom_line() +
  facet_wrap(~ TeamName) + 
  ggtitle("Kenpom ordinals among NCAA Tournament teams (2017)")+
  xlab("Time (days)")+
  ylab("Rank")

# Examine rankings throughout the year among teams that played in the ncaa_tournament
ncaa_tourney_teams_2017 <- ncaa_tourney_seeds %>% 
  filter(Season == "2017") %>% 
  select(TeamID)

# Create faceted linegraphs of rankings over time, among teams that played in the 2017 ncaa tournament

pomeroy_ordinals_2017 %>% 
  filter(TeamID %in% ncaa_tourney_teams_2017$TeamID) %>% 
  ggplot(aes(x = reorder(TeamName, desc(OrdinalRank)), y = OrdinalRank))+
  geom_boxplot()+
  coord_flip()

# Same as above, but will all ordinals data - inclusive of poor models
massey_ordinals%>% 
  filter(TeamID %in% ncaa_tourney_teams_2017$TeamID) %>% 
  ggplot(aes(x = reorder(TeamName, desc(OrdinalRank)), y = OrdinalRank))+
  geom_boxplot()+
  coord_flip()+
  ggtitle("Ordinal Rankings by 2017 Tournament Teams")


