

# Doing the laundry (load and clean)  ---------------------------------------------------------

## Note: This data set is offense only. Will have to use the what I had for defense previously ##

library(nflreadr)
library(data.table)

dat = data.table(load_player_stats()) # Load all the player stats for the season so far
colnames(dat)[1] = "gsis_id" # change the name of the column to make merging later easy
week = subset(dat, dat$week == 1) # Filter for current week
week = week[-c(271), ] # Remove Henry Ruggs because ya know..... 

weekly_players = unique(week$gsis_id) # Get the unique IDs of the players for the week

names = data.table(load_rosters(2021)) # Load in the complete roster

players = names[names$gsis_id %in% weekly_players, ] # Filter players so only the ones who played count
all_dat = merge(week, players) # Merge player stats with roster data so we have player positions




# The Brady Bunch ---------------------------------------------------------
qb = subset(all_dat, all_dat$position == "QB")


# Lights,  Kamara,  Action! -----------------------------------------------
rb = subset(all_dat, all_dat$position == "RB")


# More Than A. Theilen ----------------------------------------------------
wr = subset(all_dat, all_dat$position == "WR")




# Things to do ------------------------------------------------------------

# - Best way to chose all the columns that I want to use to create rankings
# - Put in a format that will allow me to track players over each week
# - Add in total for the season to date as well as weekly
# - Validate my rankings vs ESPN or something
# - Do more simulations and try to incorporate them in as something useful
# - Explore the effects of random factors like salary, weather, college, twitter mentions, day of the week




