################################################################################
## Analysis of the Baseball Data Using Nested Dirichlet Techniques
## Monnie McGee's updates to Bianca Luedeker's code
## Originally written June 11, 2022
## Updated by BL:  9/19/2022. The update makes better ternary diagrams.
## Updated by MM: 04/29/2023 to perform the analysis for each season.
################################################################################

##################### Packages Required ################################
library(tidyverse)    ## For Data management.
library(xtable)       ## Creates tables for latex.
library(gtools)       ## Generates all possible permutations.  
library(sirt)         ## Estimates Dirichlet alpha parameters
library(compositions) ## Ternary Diagrams.
library(gganimate)    ## For animated images
library(gifski)       ## To create GIFs from animated images
################################################################################

######################## Organize and Clean the Data ###########################

## Read in Data For all batters as provided by Brad Null
## Same data as were used in his 2009 JQAS paper
## Code assumes that files are accessible from R's current working directory
batters.all.data <- read.table("batters_by_year.txt", header=FALSE, sep=",")
# variables as given in Null(2009)
outcomes <- c("PlayerID","Season","BirthYear","BatterHand","PitcherHand","TotalPlate","Int","HBP","BB","K","FBHR","FB3B","FB2B","FB1B","FBO","GBHR","GB3B","GB2B","GB1B","GBO")
names(batters.all.data) = outcomes
View(batters.all.data)

## Calculate current age and create age group variable
batters.all.data <- batters.all.data %>% mutate(currAge = Season - BirthYear)
str(batters.all.data)
dim(batters.all.data) # should be one more column now
batters.all.data <- batters.all.data %>% mutate(ageGroup = case_when(
  `currAge` < 27 ~ "Young",
  `currAge` >= 27 & `currAge` < 32 ~ "Middle",
  `currAge` >= 32 ~ "Experienced"
))
batters.all.data$ageGroup <- as.factor(batters.all.data$ageGroup)
str(batters.all.data) # ageGroup should be a factor
dim(batters.all.data) # should be one more column now

## Handedness as Factors with labels #######
batters.all.data <- batters.all.data %>%   mutate(BatterHand = case_when(
  `BatterHand` == 1 ~ "Left",
  `BatterHand` == 2 ~ "Right",
))
batters.all.data <- batters.all.data %>%   mutate(PitcherHand = case_when(
  `PitcherHand` == 1 ~ "Left",
  `PitcherHand` == 2 ~ "Right",
))
batters.all.data$BatterHand <- as.factor(batters.all.data$BatterHand)
batters.all.data$PitcherHand <- as.factor(batters.all.data$PitcherHand)
str(batters.all.data)
## Just some tables comparing handedness, season, and age
table(batters.all.data$BatterHand, batters.all.data$Season)
table(batters.all.data$PitcherHand, batters.all.data$Season)
table(batters.all.data$BatterHand, batters.all.data$ageGroup)
table(batters.all.data$PitcherHand, batters.all.data$ageGroup)

## Write out entire data set with variable names
write.csv(batters.all.data,file="allBattersAllSeasons.csv")

## For some batters, we have counts of outcomes from both right and left handed pitchers
## These are paired data, and should be treated differently from two-group data.
## Separate left and right handed pitchers. Use this later for two-group analysis  
batters.rhp.data <- subset(batters.all.data, PitcherHand=="Right")                                   
batters.lhp.data <- subset(batters.all.data, PitcherHand=="Left")
#### Write files
write.csv(batters.rhp.data,file="allBattersRhp.csv")
write.csv(batters.lhp.data,file="allBattersLhp.csv")

## Now we have two data sets, one each for RHP and LHP.
## Use right handed pitchers only.
batters.data <- read.csv(file="../Data/allBattersRhp.csv", header=T)
batters.data <- batters.data[,-1]
## Some descriptive statistics
summary(batters.data$currAge)
# Get ages of batters 
batterAge <- batters.data %>% group_by(Season) %>% 
  summarise(n=n(), 
            min = min(currAge),
            Q1 = fivenum(currAge)[2],
            Med = median(currAge),
            Q3 = fivenum(currAge)[4],
            max = max(currAge))
batterAge
summary(batters.data$PlayerID)
## Doesn't really help. Lines start at different years, 
## and are all straight lines.
selPlayers <- sample(2:3136,50)
batters.sel <- batters.data %>% group_by(PlayerID) %>% 
  filter(PlayerID == selPlayers)
ggplot(batters.sel, aes(x=Season, y=currAge)) + 
  geom_line(aes(color=as.factor(PlayerID))) 
## Plot of player ages
ageDF <- data.frame(table(batters.data$currAge))
names(ageDF) <- c("Age","Count")
# Plain old bar chart where height of the bar is frequency for each age
barplot(Count~Age,data=ageDF)
# facet plots - each facet is a season
ageSeasonDF <- data.frame(table(batters.data$currAge, batters.data$Season))
names(ageSeasonDF) <- c("Age","Season","Count")
## to convert factor to numeric while keeping Age values
## first convert to character and then to factor
## https://www.datacademy.co/r/how-to/converting-factors-to-numbers/
ageSeasonDF$Age <- as.numeric(as.character(ageSeasonDF$Age))
ggplot(ageSeasonDF, aes(x=Age, y=Count, fill=Season))+
  geom_bar(stat='identity', fill="forest green")+
  facet_wrap(~Season,  ncol=2) + theme_minimal()
## ridgeline plots - age on horizontal, each ridge is a season
## Not used in paper.
library(ggridges)
library(hrbrthemes)
ggplot(ageSeasonDF, aes(x = Age, y = Season, fill=Season)) +
         geom_density_ridges() + theme_ridges() + 
         theme(legend.position = "none") + 
         labs(title = 'Distribution of Batter Ages by Season', y="")
## later for LHP data - not done
## batters.data <- read.csv(file="allBattersLhp.csv", header=T)
## batters.data <- batters.data[,-1]

################# Combine ground outs (GB) and fly outs (FB) #################
## batters.data.combo is the same as Bianca's data3 with age group added.
batters.allRHP.combo <- batters.data %>% 
  mutate("HomeRun" = FBHR + GBHR, "Triple" = FB3B + GB3B, 
         "Double" = FB2B + GB2B,"Single" = FB1B + GB1B, 
         "Out" = K + FBO + GBO, "Other" = Int + HBP + BB, 
         "Total" = Int+HBP+BB+K+FBHR+FB3B+FB2B+FB1B+FBO+GBHR+GB3B+
                                                GB2B+GB1B+GBO) 
write.csv(batters.allRHP.combo, file="allBattersRhpSums.csv")

# For LHP - not done
# write.csv(batters.allLHP.combo, file="allBattersLhpSums.csv")

### Select the combined variables plus some covariates
batters.combo <- batters.allRHP.combo %>% select(1:2,4:5,21:29)
View(batters.combo)
write.csv(batters.combo,file="batters.combo.rhp.csv")

## For LHPs - not done
# batters.combo <- batters.allLHP.combo %>% select(1:2,4:5,21:29)
# View(batters.combo)
# write.csv(batters.combo,file="batters.combo.lhp.csv")

# Read in file already created by cleaning
# Can be done similarly for LHPs
batters.combo <- read.csv(file="batters.combo.rhp.csv", header=T)
batters.combo <- batters.combo[,-1]

# For RHP only. Can be done for LHPs with appropriate data.
batters.percent <- batters.combo %>% 
  dplyr::mutate( "HR" = HomeRun/Total, "Triple" = Triple/Total, 
                 "Double" =Double/Total, "Single" = Single/Total, "Out" = Out/Total, 
                 "Other" = Other/Total) %>% select(14,8:12,6,3,2,1) 
# %>% group_by(ageGroup,Season) %>% summarise(n = n())
View(batters.percent) # looks  good.
write.csv(batters.percent,file="battersByPercentRhp.csv")

## This just sets up the data for the tree finder algorithm.
## Go to baseball_analysis.R for to get tree