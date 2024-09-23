## Attempts at visualizing the baseball data
## Want to create a percent bar chart where each bar is 
## an outcome and coverage within bars represents percentage of 
## each age group with that outcome. 

# Creates several plots and writes them to a subdirectory called
# Presentation. Need to create this subdirectory or 
# change the png function path argument.

library(tidyverse)
library(RColorBrewer)
library(gganimate)
library(gifski)

batters.combo <- read.csv(file="../Data/batters.combo.rhp.csv", header=T)
batters.combo <- batters.combo[,-1]

## how many batters played multiple seasons?
length(which(table(batters.combo$PlayerID)==1))
length(which(table(batters.combo$PlayerID)==2))
length(which(table(batters.combo$PlayerID)==3))
length(which(table(batters.combo$PlayerID)==4))
length(which(table(batters.combo$PlayerID)==5))
length(which(table(batters.combo$PlayerID)==6))
length(which(table(batters.combo$PlayerID)==7))
length(which(table(batters.combo$PlayerID)==8))
length(which(table(batters.combo$PlayerID)==9))
length(which(table(batters.combo$PlayerID)==10))
length(which(table(batters.combo$PlayerID)==11))
playerYears <- data.frame(Years=1:11,
    Count=c(466,311,194,163,144,113,99,79,63,49,87))
png("../Presentation/NumberSeasonsPlayed.png")
xx <- barplot(Count~Years, data=playerYears, ylim=range(0,500),col="royalblue", xlab="Number of Seasons Played", ylab="")
text(x = xx, y = playerYears$Count, label = playerYears$Count, pos = 3, cex = 0.8, font = 2, col = "red")
dev.off()

## Create "long" format file for purposes of animated bar charts
handAgeData <- batters.combo %>% select(1:4,7) 
View(handAgeData)
handAgeData %>% ggplot(aes(x=ageGroup)) + geom_bar() + 
  facet_wrap(~PitcherHand) +
  labs(title = 'Season: 2001-2010', x = 'Age Group', y = 'Count')
### Same plot animated
p1 <- handAgeData %>% ggplot(aes(x=ageGroup)) + geom_bar() + 
  facet_wrap(~PitcherHand) +
  labs(title = 'Season: {frame_time}', x = 'Age Group', y = 'Count') +
  transition_time(as.integer(Season)) +
  ease_aes('linear')
animate(p1)

## Create a bar chart for all seasons. First, pivot_longer
batter.long <- batters.combo %>% 
  pivot_longer(cols=7:12,names_to="Outcome", values_to="Count")

## Column chart
png(file="../Presentation/OverallOutcomes.png")
batter.long %>% group_by(PlayerID) %>% 
  mutate(Outcome = fct_relevel(Outcome, "HomeRun", "Triple", "Double", "Single", "Other", "Out")) %>%
  ggplot(aes(Outcome, Count)) + geom_col(color=cbPalette[4]) + 
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  labs(x = 'At Plate Outcomes', y = 'Count of Outcome')
dev.off()
# scale_y_continuous(labels = scales::percent_format(accuracy = 1))

## reorder factors and change color.
# Color blind friendly palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# line chart of age group for all data by seasons
png(file="../Presentation/playerCountByAge.png")
playerCount <- table(batter.combo$ageGroup,batter.combo$Season)
plot(2000:2010, playerCount[1,], col="darkred",type="b", lwd=2, lty=1, ylim=c(150, 300), xlab = "", ylab="Number of Players")
lines(2000:2010, playerCount[2,], lwd=2, type="b", col="steelblue", lty=2)
lines(2000:2010, playerCount[3,], lwd=2, type="b", col="darkgoldenrod", lty=3)
legend("bottomleft", col=c("darkgoldenrod","steelblue","darkred"), lty=1:3, lwd = 2, legend = c("Young", "Middle", "Experienced"), inset=c(0,1), xpd=TRUE, horiz=TRUE, bty="n")
dev.off()
# Season = 2000
png(file="../Presentation/Season2000OutcomesByAge.png")
batter.long %>% subset(Season==2000) %>% group_by(PlayerID) %>% 
  mutate(Outcome = fct_relevel(Outcome, "HomeRun", "Triple", "Double", "Single", "Other", "Out")) %>%
  ggplot(aes(Outcome, Count)) + geom_col(aes(fill = ageGroup), position = "dodge") + 
  ylim(0, 400) + scale_fill_manual(values=cbPalette[c(3,8,6)]) +
  guides(fill=guide_legend(title='Age Group')) +  
  theme(legend.position=c(0.25,0.8), panel.background = element_rect(fill = "white", colour = "grey50")) + 
  labs(x = 'At Plate Outcome for 2000 Season', y = 'Count of Outcome')
dev.off()
# Season = 2005
png(file="../Presentation/Season2005OutcomesByAge.png")
batter.long %>% subset(Season==2005) %>% group_by(PlayerID) %>% 
  mutate(Outcome = fct_relevel(Outcome, "HomeRun", "Triple", "Double", "Single", "Other", "Out")) %>%
  ggplot(aes(Outcome, Count)) + geom_col(aes(fill = ageGroup), position = "dodge") + 
  scale_fill_manual(values=cbPalette[c(3,8,6)]) +
  guides(fill=guide_legend(title='Age Group')) +  
  theme(legend.position=c(0.25,0.8), panel.background = element_rect(fill = "white", colour = "grey50")) + 
  labs(x = 'At Plate Outcome for 2005 Season', y = 'Count of Outcome')
dev.off()
# Season = 2010
png(file="../Presentation/Season2010OutcomesByAge.png")
batter.long %>% subset(Season==2010) %>% group_by(PlayerID) %>% 
  mutate(Outcome = fct_relevel(Outcome, "HomeRun", "Triple", "Double", "Single", "Other", "Out")) %>%
  ggplot(aes(Outcome, Count)) + geom_col(aes(fill = ageGroup), position = "dodge") + 
  ylim(0, 400) + scale_fill_manual(values=cbPalette[c(3,8,6)]) +
  guides(fill=guide_legend(title='Age Group')) +  
  theme(legend.position=c(0.25,0.8), panel.background = element_rect(fill = "white", colour = "grey50")) + 
  labs(x = 'At Plate Outcome for 2010 Season', y = 'Count of Outcome')
dev.off()

# Create animated GIF file for outcomes by season - not included in paper.
library(gganimate) 
p2 <- batter.long %>% group_by(PlayerID) %>% 
  mutate(Outcome = fct_relevel(Outcome, "HomeRun", "Triple", "Double", "Single", "Other", "Out")) %>%
  ggplot(aes(Outcome, Count)) + geom_col(aes(fill = ageGroup), position = "dodge") + 
  ylim(0, 400) + scale_fill_manual(values=cbPalette[c(3,8,6)]) +
  guides(fill=guide_legend(title='Age Group')) +  
  theme(legend.position=c(0.25,0.8), panel.background = element_rect(fill = "white", colour = "grey50")) + 
  labs(title = 'Season: {frame_time}', x = 'At Plate Outcome', y = 'Count') +
  transition_time(as.integer(Season)) +
  ease_aes('linear')
animate(p2)
gifski(p2, gif_file="SeasonOutcomes.gif", delay=2,loop=3)

