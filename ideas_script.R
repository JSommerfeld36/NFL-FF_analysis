
# library(data.table)
# library(tictoc)
# 
# tic()
# for (i in 1:1000) {
#   
#   if (i == 1) {
#     #y = data.table(rnorm(1000))
#     y = data.table(matrix(data = rexp(200, rate = 10), nrow = 1000, ncol = 1000))    
#     fwrite(y, "test.csv")
#   }else {
#     fread("test.csv")
#     #y = data.table(rnorm(1000))
#     y = data.table(matrix(data = rexp(200, rate = 10), nrow = 1000, ncol = 1000))    
#     fwrite(y, "test.csv")
#   }
# }
# 
# file.remove("test.csv")
# toc()


player_name = c("Aaron Rodgers", "Justin Herbert", "Stefon Diggs", "Cooper Kupp", "Alvin Kamara", "Nick Chubb")
player_pos = c("QB", "QB", "WR", "WR", "RB", "RB")
xFP = c(24, 23, 14, 18, 17, 15)
rFP = c(28, 26, 15, 15, 12, 18)
o = c(1, 1, 2, 2, 3, 3)

dat = data.table::data.table(player_name, player_pos, xFP, rFP, o)
d = data.table::data.table(player_name, player_pos, xFP, rFP)
dat$player_pos = as.factor(dat$player_pos)
dat$player_name = as.factor(dat$player_name)
dat$performance = dat$xFP - dat$rFP
dat$o = as.numeric(dat$o)


library(ggplot2)

xp = ggplot(dat, aes(x = reorder(player_name, o), xFP, fill = player_pos)) + geom_col() + 
  geom_line(data = dat, aes(reorder(player_name, o), rFP, group = 1)) +
  labs(title = ("Fantasy Points (Bars) & Expected Fantasy Points (Line)")) + 
  ylab("Fantasy Points") + 
  theme(axis.title.x = element_blank())
xp
# autoregression on the actual scores over the weeks to see if there is a trend




library(nflreadr)
library(data.table)

dat = data.table(load_player_stats()) # Load all the player stats for the season so far

colnames(dat)[1] = "gsis_id" # change the name of the column to make merging later easy
#week = subset(dat, dat$week == 1) # Filter for current week
#week = week[-c(271), ] # Remove Henry Ruggs because ya know..... 

week = dat[player_name != "H.Ruggs"]

weekly_players = unique(week$gsis_id) # Get the unique IDs of the players for the week

names = data.table(load_rosters(2021)) # Load in the complete roster

players = names[names$gsis_id %in% weekly_players, ] # Filter players so only the ones who played count
all_dat = merge(week, players) # Merge player stats with roster data so we have player positions

qb = subset(all_dat, all_dat$position == "QB")

print("Who are your QBs?")
qb_list = readline("Enter their last name only with no spaces and separated by a comma:")
qb_names = unlist(strsplit(qb_list, ','))

current_week = readline("What game week do you want to look at?")
qb = qb[week == current_week]

league_max = NULL 
league_min = NULL 
for (i in 7:48) {
  league_max[i] = max(qb[[i]])
  league_min[i] = min(qb[[i]])
}

qb1 = qb[qb$player_name %like% qb_names[1],]
qb2 = qb[qb$player_name %like% qb_names[2],]
qb3 = qb[qb$player_name %like% qb_names[3],]

# Could do the radar chart but for it to be useful I would need to have the max and min for the league and 
# then the specific player would be mapped using that as the "normalisation"
# Not sure I can do comparisons unless I make the app because the legend is cut off

qbs = rbind.data.frame(league_max[7:48], league_min[7:48], qb1[1,7:48], qb2[1,7:48], qb3[1,7:48])
colnames(qbs) = colnames(qb1[,7:48])

library(fmsb)
colors = c("#00AFBB", "#E7B800", "#FC4E07")
radarchart(qbs[,1:12],axistype = 1,
           # Customize the polygon
           pcol = colors, pfcol = scales::alpha(colors, 0.3), plwd = 2, plty = 1,
           # Customize the grid
           cglcol = "grey", cglty = 1, cglwd = 0.8,
           # Customize the axis
           axislabcol = "grey", 
           vlcex = 0.7,
           title = paste0("QB Comparison - Week ", current_week, ", 2021"))

legend(-1.5,1.2, legend=c(qb_names), seg.len=0.5, title="QB", pch=1, 
       bty="n" ,lwd=3, y.intersp=0.5, horiz=FALSE, col=colors, cex = 0.8)

name = "T.Brady"
week = 4

plot_dat %>%
  filter(plot_dat$player_name %in% name) %>%
  ggplot(plot_dat, mapping = aes(week, completions)) + 
  geom_line(aes(y = completions, color = "completions")) +       
  geom_line(aes(y = attempts, color = "attempts")) + 
  scale_color_manual("", breaks = c("completions","attempts"), 
                  values = c("red", "blue")) +
  labs(title = paste(name, "completions over the season")) + 
  ylab("# Completions") + xlab('Game Week') + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())
  


league_max = data.frame(lapply(plot_dat[,7:48], max))
league_min = data.frame(lapply(plot_dat[,7:48], min))



plot_dat %>%
  filter(plot_dat$player_name %in% name) 