## first, we need to install a few packages.
## to do this, first run install.packages()
## and put the package name in quotes

install.packages("Lahman")
install.packages("dplyr")
install.packages("ggplot2")

## With these libraries installed, we can now call them

library(Lahman)
library(dplyr)
library(ggplot2)

## looking at some data: 
## the Lahmann library comes with several in-built 
## datasets -- let's look at whats available:

data(package = "Lahman")

# lots of interesting stuff here.  
# we'll work with the Batting data.
# let's create a copy of this called df -- 
# this will make it easier to quickly refer to the 
# data later on:

df = Batting
# note: make sure to match the case!


# ok.  We've got data!

# first question -- how much data do we have? 
nrow(df)

# whoah.  there's a ton!  We have 102816 observations.

# what variables do we observe? 
names(df)

# player code
# what year
# "stint"
# league ID (e.g. AL / NL etc.)
# Games
# At bats
# Runs
# Hits
# Doubles
# Triples
# Home Runs
# Runs Batted in
# etc.

# we can gather a bit more information using
?Batting



## looking at the data: 
head(Batting)

# whoa.  This goes back to 1871!


## ok.  let's browse the data a bit -- 

# in general, we'll use the summarize() command
# to conduct calculations. this works for 
# many functions of the data:

# what is the average number of home runs?
df %>% summarize(mn.hr = mean(HR, na.rm = TRUE))

# what is the median number of home runs?
df %>% summarize(med.hr = median(HR, na.rm = TRUE))
# ha.

# what is the max number of home runs?
df %>% summarize(max.hr = max(HR, na.rm = TRUE))
# whoa.

# who are these HR-hitting phenoms?

# we can sort the data using the arrange() command:
df %>% arrange(desc(HR)) %>% head(.)

# we can look at a little more data by passing 
# another attribute to the head() command: 

df %>% arrange(desc(HR)) %>% head(.,20)

# does anyone remember 1998?

# why was 1998 so special? I'll tell you!

# let's calculate the maximal number of home runs
# hit in each year in the data. 

# to do this, we can think about splitting our data
# up by year -- collecting all the 1871 observations 
# together, 
# and all the 1872 observations, etc. 
# next, we're going to run the same function on 
# each of the groups -- that function will ask what 
# the max of the HR variable is.  

# example: 
df %>% group_by(yearID) %>% summarize(max.hr = max(HR, na.rm = TRUE))

# we can also write it like this: 

df %>% 
  group_by(yearID) %>% 
  summarize(max.hr = max(HR, na.rm = TRUE))

## ok.  That's sort of intersting... can 
# we look at these values on a plot? 


df %>% 
  group_by(yearID) %>% 
  summarize(max.hr = max(HR, na.rm = TRUE)) %>%
  ggplot(.,aes(x = yearID, y = max.hr)) + geom_point() + 
  theme_bw()

# if I want to save this plot (insert your own file path here)
# ggsave("C:/users/mpatters/desktop/HRs_by_year.png")

# what's interesting about this plot?
# 1920 is crazy!

## ok - back to the question about 1998
# let's highlight this point a bit: 


df %>% 
  group_by(yearID) %>% 
  summarize(max.hr = max(HR, na.rm = TRUE)) %>%
  ggplot(.,aes(x = yearID, y = max.hr)) + geom_point() + 
  theme_bw() + geom_vline(aes(xintercept = 1998), col = "red")

# Let's look at what the record books were like
# in 1998 (note: MP was 13 years old):

df %>% filter(yearID < 1998) %>%
  select(playerID,yearID,HR) %>%
  arrange(desc(HR)) %>% 
  head(.)

# the HR record had been the same since 1961
# (Roger Maris hit 61), and before that, it was Babe Ruth
# in 1927 with 60!
# 
# There was speculation about whether anyone would 
# ever pass roger maris...
#
# but let's look at the data from 1998:

df %>% filter(yearID == 1998) %>%
  select(playerID,yearID,HR) %>%
  arrange(desc(HR)) %>% 
  head(.)

# what was crazy?  

# what about 1999?
df %>% filter(yearID == 1999) %>%
  select(playerID,yearID,HR) %>%
  arrange(desc(HR)) %>% 
  head(.)

# what about 2000?
df %>% filter(yearID == 2001) %>%
  select(playerID,yearID,HR) %>%
  arrange(desc(HR)) %>% 
  head(.)

# what about 2001?
df %>% filter(yearID == 2001) %>%
  select(playerID,yearID,HR) %>%
  arrange(desc(HR)) %>% 
  head(.)

# what about 2002?
df %>% filter(yearID == 2002) %>%
  select(playerID,yearID,HR) %>%
  arrange(desc(HR)) %>% 
  head(.)

# Poor Sammy :(


## ok.  A little more on HRs: 

# we can modify the code above to look at the 
# total number of homeruns hit by year: 

df %>% 
  group_by(yearID) %>% 
  summarize(tot.hr = sum(HR, na.rm = TRUE)) %>%
  ggplot(.,aes(x = yearID, y = tot.hr)) + geom_point() + 
  theme_bw() 

# We can also look at the number of strikeouts:
df %>% 
  group_by(yearID) %>% 
  summarize(tot.so = sum(SO, na.rm = TRUE)) %>%
  ggplot(.,aes(x = yearID, y = tot.so)) + geom_point() + 
  theme_bw() 
# woah.  (what's going on in 1900?? -- I have no idea) 

# what about walks?

df %>% 
  group_by(yearID) %>% 
  summarize(tot.bb = sum(BB, na.rm = TRUE)) %>%
  ggplot(.,aes(x = yearID, y = tot.bb)) + geom_point() + 
  theme_bw() 


## all of that is sort of interesting, but I'd 
# like to know about how the likelihoods of 
# walks and strikes (maybe hits too?) are changing
# over time.  
# to do this, let's do this: 
# for each player, I'll create a new variable
# which is ABs + BBs:

df = df %>% 
  mutate(PA = BB + AB)

df %>% head(.)

# this isn't perfect, but it'll give us a 
# starting point. 

# next, let's find the total BBs divided by plate
# appearances: 

df = df %>%
  mutate(BB.prob = BB/PA)

# let's do the same for strike outs and hits:

df = df %>%
  mutate(SO.prob = SO/PA) %>%
  mutate(H.prob = H/PA) 

# ok.  Some of the players only have a small 
# number of plate appearances.. we'll remove these
# in our analysis using filter()...

df %>% filter(PA > 120) %>% head(.)


# ok.  This won't be perfect, but I'd like to 
# look at how the median BB, SO, and H probabilities
# are changing over time.

df %>% filter(PA > 120) %>%
  group_by(yearID) %>%
  summarize(med.BB.prob = median(BB.prob, na.rm = TRUE),
            med.SO.prob = median(SO.prob,na.rm = TRUE),
            med.H.prob = median(H.prob,na.rm = TRUE)) %>%
  ggplot(.,aes(x = yearID, y = med.BB.prob)) + geom_point(col = "blue") + 
  theme_bw() + geom_point(aes(x = yearID, y = med.SO.prob), col = "red") + 
  geom_point(aes(x = yearID, y = med.H.prob), col = "purple")

#huh. interesting.  


### Ok -- switching datasets a little bit --
#  let's look at Fielding.

# set df to be the Fielding data: 
df = Fielding

# look at the beginning of the data:
head(df)

# interesting that we have position information!

# another interesting thing -- we have repeated entries
# in the data.  hmm.. it looks like players have seperate
# entries when they play multiple positions. 
# is this common?

# first, let's count how many positions each player had 
# in a given year:

df %>%
  group_by(yearID,playerID) %>%
  summarize(tot.pos = length(unique(POS))) %>%
  head(.)

# woah. 5 positions for ansonca01!

# let's add a variable which indicates whether the number
# of positions is more than 1:

df %>%
  group_by(yearID,playerID) %>%
  summarize(tot.pos = length(unique(POS)),
            mult.pos = as.numeric(tot.pos > 1)) %>%
  head(.)


# ok.  Now, we can figure out what the average number of 
# players who play multiple positions in each year is by taking
# the average of our mult.pos variable in each year:

df %>%
  group_by(yearID,playerID) %>%
  summarize(tot.pos = length(unique(POS)),
            mult.pos = as.numeric(tot.pos > 1)) %>%
  group_by(yearID) %>%
  summarize(share.mult = mean(mult.pos, na.rm = TRUE)) %>%
  ggplot(.,aes(x = yearID, y = share.mult)) + geom_point()

#ggsave("C:/Users/mpatters/desktop/multipos_by_year.png")

# more than half of the players played multiple positions circa 1880!
# fewer than 1 in 5 do now!
