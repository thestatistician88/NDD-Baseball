### SDSS analysis for RHPs 
## make sure you're in the correct directory
## Run baseball_cleaning.R first.

## load libraries
library(tidyverse)
library(xtable)
library(gtools)
library(sirt)
library(compositions)
library(reshape)

## Start here for tree
## Read in data
batter.dat <- read.csv("../Data/battersByPercentRhp.csv", stringsAsFactors = TRUE)
batter.dat <- batter.dat[,-1]
# Number of unique players
length(unique(batter.dat$PlayerID))

batterAge1 <- subset(batter.dat, ageGroup=="Young")
batterAge2 <- subset(batter.dat, ageGroup=="Middle")
batterAge3 <- subset(batter.dat, ageGroup=="Experienced")

#### Create a pie chart
batters.combo <- read.csv(file="../Data/batters.combo.rhp.csv", header=T)
batters.combo <- batters.combo[,-1]
batter.long <- batters.combo %>% 
  pivot_longer(cols=7:12,names_to="Outcome", values_to="Count")
batter.long$Outcome <- factor(batter.long$Outcome, levels = c("HomeRun", "Triple", "Double", "Single", "Other", "Out"))
test <- batter.long %>% uncount(Count) %>% mutate(Count=1)
batter.perc <- test %>% group_by(Outcome) %>% 
  count() %>% ungroup() %>%
  mutate(Percent = `n` / sum(`n`)) %>% 
  arrange(Percent) %>%
  mutate(labels = scales::percent(Percent))
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
## Needs tweaking with labels, but this mostly works.
# df2 calculates the positions for the text labels
library(ggrepel)
df2 <- batter.perc %>% 
  mutate(csum = rev(cumsum(rev(Percent))), 
         pos = Percent/2 + lead(csum, 1),
         pos = if_else(is.na(pos), Percent/2, pos))

png(file="../Presentation/PieTotalOutcome.png")
  batter.perc %>% 
    ggplot(aes(x="", y=Percent, fill=fct_inorder(Outcome))) + 
    geom_col() +    scale_fill_brewer(palette = "Pastel1") +
    coord_polar(theta = "y") + 
     scale_y_continuous(breaks = df2$pos, labels = batter.perc$Outcome) +
    geom_label_repel(data = df2, aes(y = pos, label = labels),
                     size = 4.5, nudge_x = .75, show.legend = FALSE) +
    guides(fill = guide_legend(title = "Outcome")) +
#    theme(axis.ticks = element_blank(),
#          axis.title = element_blank(),
 #         axis.text = element_text(size = 15), 
  #        legend.position = "none", # Removes the legend
   #       panel.background = element_rect(fill = "white"))
    theme_void() 
  #+ theme(legend.position = "top")
dev.off()

#### 2010 season
batter2010.dat <- batter.dat %>% subset(Season==2010)
## Subset into age groups
batter2010Age1 <- subset(batter.dat, ageGroup=="Young") %>% 
  subset(Season==2010)
batter2010Age2 <- subset(batter.dat, ageGroup=="Middle") %>% 
  subset(Season==2010)
batter2010Age3 <- subset(batter.dat, ageGroup=="Experienced") %>% 
  subset(Season==2010)

# Correlation matrices
cc <- round(cor(batter2010.dat[,1:6]),4) # All ages for 2010 season
c1 <- round(cor(batter2010Age1[,1:6]),4) # Youngsters
c2 <- round(cor(batter2010Age2[,1:6]),4) # Middle aged
c3 <- round(cor(batter2010Age3[,1:6]),4) # Chronologically gifted

## 2005 season
batter2005.dat <- batter.dat %>% subset(Season==2005)
## Subset into age groups
batter2005Age1 <- subset(batter.dat, ageGroup=="Young") %>% 
  subset(Season==2005)
batter2005Age2 <- subset(batter.dat, ageGroup=="Middle") %>% 
  subset(Season==2005)
batter2005Age3 <- subset(batter.dat, ageGroup=="Experienced") %>% 
  subset(Season==2005)
# Correlation matricies
bb <- round(cor(batter2005.dat[,1:6]),4) # All ages for 2010 season
b1 <- round(cor(batter2005Age1[,1:6]),4) # Youngsters
b2 <- round(cor(batter2005Age2[,1:6]),4) # Middle aged
b3 <- round(cor(batter2005Age3[,1:6]),4) # Chronologically gifted

## 2000 season
batter2000.dat <- batter.dat %>% subset(Season==2000)
## Subset into age groups
batter2000Age1 <- subset(batter.dat, ageGroup=="Young") %>% 
  subset(Season==2000)
batter2000Age2 <- subset(batter.dat, ageGroup=="Middle") %>% 
  subset(Season==2000)
batter2000Age3 <- subset(batter.dat, ageGroup=="Experienced") %>% 
  subset(Season==2000)
# Correlation matricies
aa <- round(cor(batter2000.dat[,1:6]),4) # All ages for 2010 season
a1 <- round(cor(batter2000Age1[,1:6]),4) # Youngsters
a2 <- round(cor(batter2000Age2[,1:6]),4) # Middle aged
a3 <- round(cor(batter2000Age3[,1:6]),4) # Chronologically gifted

# Overall correlation for all seasons and ages
oall <- round(cor(batter.dat[,1:6]),4) # All ages for 2010 season
o1 <- round(cor(batterAge1[,1:6]),4) # Youngsters
o2 <- round(cor(batterAge2[,1:6]),4) # Middle aged
o3 <- round(cor(batterAge3[,1:6]),4) # Chronologically gifted

# We can get a correlation table, but why?
xtable(aa, type=latex, digits=3)


# Reorder the correlation matrix
# change according to age
# oall = all ages and all seasons
# o1 = all seasons young players
# o2 = all seasons middle players
# o3 = all seasons mature players

# aa = all ages for 2000 season
# a1 = youngsters for 2000
# a2 = middle for 2000
# a3 = mature players for 2000

# bb = all ages for 2005 season
# b1 = youngsters for 2005
# b2 = middle for 2005
# b3 = mature players for 2005

# cc = all ages for 2010 season
# c1 = youngsters for 2010
# c2 = middle for 2010
# c3 = mature players for 2010
source("getHeatmap.R")
cormat <- b3 
cormat <- reorder_cormat(cormat) 
upper_tri <- get_upper_tri(cormat)
upper_tri <- as.data.frame.table(upper_tri)
melted_cormat <- reshape::melt(upper_tri, na.rm = TRUE)
########## Generic function for Heatmap ############
# Melt the correlation matrix
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# Add correlation values to heatmap and delete axis labels
# Files created
# png(file="../Presentation/overallHmap2010.png")
# png(file="../Presentation/youngHmap2010.png")
# png(file="../Presentation/middleHmap2010.png")
# png(file="../Presentation/matureHmap2010.png")
# png(file="../Presentation/overallHmap.png")
# png(file="../Presentation/overallHmapYoung.png")
# png(file="../Presentation/overallHmapMiddle.png")
# png(file="../Presentation/overallHmapMature.png")
# png(file="../Presentation/overallHmap2000.png")
# png(file="../Presentation/youngHmap2000.png")
# png(file="../Presentation/middleHmap2000.png")
# png(file="../Presentation/matureHmap2000.png")
# png(file="../Presentation/overallHmap2005.png")
# png(file="../Presentation/youngHmap2005.png")
# png(file="../Presentation/middleHmap2005.png")
# png(file="../Presentation/matureHmap2005.png")
ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))
dev.off()


## Compositional data analysis a la CoDa
# All ratios can be accessed with the "aplus" function
# Problems with 0's in the data
batter2010 <- batter.dat %>% subset(Season == 2010)
compOut <- aplus(batter2010[,1:6])
# alr has -Inf and -NaN in the results
# clr does not. 
batter2010$comp <- compOut
codalm <- lm(clr(comp)~ageGroup, data=batter2010)
compInv <- clrInv(predict(codalm), orig=batter2010[,1:6])
pred2010 <- as.data.frame(compInv)
pred2010$age <- batter2010$ageGroup
pred2010 <- pred2010 %>% group_by(age) %>% summarise(across(HR:Other, mean))
round(mean(compInv), 4)
# summary(rowSums(as.data.frame(compInv))) # all sums are 1.
summary(codalm)
xtable(codalm$coefficients, digits=4)
# How are coda LM coefficients intrepreted?

# 2000 Season
batter2000 <- batter.dat %>% subset(Season == 2000)
compOut <- aplus(batter2000[,1:6])
# alr has -Inf and -NaN in the results
# clr does not. 
batter2000$comp <- compOut
# head(batterSeason)
codalm <- lm(clr(comp)~ageGroup, data=batter2000)
compInv <- clrInv(predict(codalm), orig=batter2000[,1:6])
pred2000 <- as.data.frame(compInv)
pred2000$age <- batter2000$ageGroup
pred2000 <- pred2000 %>% group_by(age) %>% summarise(across(HR:Other, mean))
round(mean(compInv), 4)
# summary(rowSums(as.data.frame(compInv))) - all sums are 1.
summary(codalm)
xtable(codalm$coefficients)

batter2005 <- batter.dat %>% subset(Season == 2005)
compOut <- aplus(batter2005[,1:6])
# alr has -Inf and -NaN in the results
# clr does not. 
batter2005$comp <- compOut
# head(batterSeason)
codalm <- lm(clr(comp)~ageGroup, data=batter2005)
compInv <- clrInv(predict(codalm), orig=batter2005[,1:6])
pred2005 <- as.data.frame(compInv)
pred2005$age <- batter2005$ageGroup
pred2005 <- pred2005 %>% group_by(age) %>% summarise(across(HR:Other, mean))
round(mean(compInv), 4)
# summary(rowSums(as.data.frame(compInv))) - all sums are 1.
summary(codalm)
xtable(codalm$coefficients)
xtable(round(rbind(pred2000,pred2005,pred2010)),4)

### Next - run tree and Dirichlet analysis
batter2010 <- batter.dat %>% subset(Season==2010)
batter2009 <- batter.dat %>% subset(Season==2009)
batter2008 <- batter.dat %>% subset(Season==2008)
batter2007 <- batter.dat %>% subset(Season==2007)
batter2006 <- batter.dat %>% subset(Season==2006)
batter2005 <- batter.dat %>% subset(Season==2005)
batter2004 <- batter.dat %>% subset(Season==2004)
batter2003 <- batter.dat %>% subset(Season==2003)
batter2002 <- batter.dat %>% subset(Season==2002)
batter2001 <- batter.dat %>% subset(Season==2001)
batter2000 <- batter.dat %>% subset(Season==2000)

#### Finding the correct tree for the baseball data ####
source("treeFinder.R")
treeAll <- complete.bin.tree(batter.dat[,1:6])

tree2010 <- complete.bin.tree(batter2010[,1:6])
tree2009 <- complete.bin.tree(batter2009[,1:6])
tree2008 <- complete.bin.tree(batter2008[,1:6])
tree2007 <- complete.bin.tree(batter2007[,1:6])
tree2006 <- complete.bin.tree(batter2006[,1:6])
tree2005 <- complete.bin.tree(batter2005[,1:6])
tree2004 <- complete.bin.tree(batter2004[,1:6])
tree2003 <- complete.bin.tree(batter2003[,1:6])
tree2002 <- complete.bin.tree(batter2002[,1:6])
tree2001 <- complete.bin.tree(batter2001[,1:6])
tree2000 <- complete.bin.tree(batter2000[,1:6])

# Don't need these
# age1tree <- complete.bin.tree(batterAge1[,1:6])
# age2tree <- complete.bin.tree(batterAge2[,1:6])
# age3tree <- complete.bin.tree(batterAge3[,1:6])
## Tree results are in the plain text file ../Data/treesByYear.txt