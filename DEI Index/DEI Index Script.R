#step 1: import libraries needed

library(tidyverse)

#step 2: import databases and merge them

gamedata <- read.csv("Clean PWHPA Secret Dream Gap Tour - SportLogiq - PWHPA Secret Dream Gap Tour.csv")

colordata <- read.csv("PWHPA Team Colors.csv")

data <- merge(gamedata, colordata, by = "team", all =TRUE)


#step 3: make the relevant dataset

sample <- select(data, c("player", "team", "goals", "player_goals_against", "x_gf_woi", "x_ga_woi", "primary", "secondary")) %>% 
  group_by(player, team, primary, secondary) %>% 
  summarize(goals = sum(goals, na.rm=TRUE), 
            player_goals_against = sum(player_goals_against, na.rm=TRUE), 
            x_gf_woi = sum(x_gf_woi, na.rm=TRUE), 
            x_ga_woi = sum(x_ga_woi, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(DEI = goals - x_gf_woi + x_ga_woi - player_goals_against)


MTL <- select(sample, c())

#Step 4: Make us a graph

graph <- ggplot(data = sample, 
                mapping = aes(x = reorder(player, +DEI), y = DEI, fill = team)) + geom_bar(stat = "identity") + 
              coord_flip() + 
              labs(title = "Over- and Under-Achievers in PWHPA Secret Dream Gap Tour by DEI",
                   subtitle = "DEI: Defied Expectations Index, calculated by actual performance subtracted\n from predicted performance. Positive values indicate overperformance.",
                   caption = "Viz by Brian Yngve and Matt Roepke\nData from PWHPA and Sportlogiq",
                   x = "Player Name",
                   y = "Defied Expectations Index (DEI)",
                   fill = "Team") +
              scale_fill_manual(values = c("#ee0b19", "#cbd443", "#32364f", "#141517", "#6dd5e1"))
graph

ggsave("beginner_brian_yngve.png", width = 10, height = 12)
  