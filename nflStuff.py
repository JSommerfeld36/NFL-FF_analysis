#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Aug 11 20:52:37 2022

@author: joelsommerfeld
"""

import nfl_data_py as nfl
import plotly.express as px
import plotly.io as pio
pio.renderers.default = "svg"


pbp = nfl.import_pbp_data([2021]) # Play by play
ngs = nfl.import_ngs_data("passing", [2021]) # Next Gen Stats

# Who is the fastest passer??
p1 = ngs[ngs["week"] == 1]
test = p1.sort_values(['avg_time_to_throw'], ascending=[True])

fig1 = px.bar(test, x = 'player_display_name', y = "avg_time_to_throw", title = "Average time to throw. Week 1, 2021")
fig1.show()

fig2 = px.bar(test, x = 'player_display_name', y = "max_completed_air_distance", title = "Max completed air distance. Week 1, 2021")
fig2.show()

fig3 = px.bar(test, x = 'player_display_name', y = "pass_yards", title = "Passing yards. Week 1, 2021")
fig3.show()

# Let's do it with ggplot now
# import plotnine
# from plotnine import ggplot, aes, geom_bar

# ggplot(test) + aes("player_display_name", "avg_time_to_throw") + geom_bar(stat = "identity")


# This is to save all the graphs in one html doc
with open('p_graph.html', 'a') as f:
    f.write(fig1.to_html(full_html=False, include_plotlyjs='cdn'))
    f.write(fig2.to_html(full_html=False, include_plotlyjs='False'))
    f.write(fig3.to_html(full_html=False, include_plotlyjs='False'))



# I want to rank each player, weekly, based on the below categories. The rank will be their average position across all categories. 

import nfl_data_py as nfl
import pandas as pd

currentWeek = 1

dat = nfl.import_weekly_data([2021])

pos = nfl.import_rosters([2021], ["position", "player_id"])
this = dat.merge(pos, on = "player_id")

#weekly_players = unique(week$gsis_id)
#players = dat.player_id.unique()

#players = names[names$gsis_id %in% weekly_players, ]
#test = dat.player_id.isin(pos)


dat = dat[dat["week"] == currentWeek] # This should probably still come later


# Need to work out a way to filter for only ids in dat and count games played

# df.team.value_counts() --> can use this to filter for games played
#gamesPlayed = dat.player_id.value_counts()


#if currentWeek > 5:
    # remove players with less than 5 total games
#toRemove = gamesPlayed < 5
#temp = toRemove == "False"

 
#else :
        


# For QB: Passing yards, passing TDs and interceptions
qb1 = this[this["position"] == "QB"]
qb1 = qb1[qb1["week"] == currentWeek]

cols = ["player_name", "week", "passing_yards", "passing_tds", "interceptions"]
qb1 = qb1.filter(cols)


# Need to sort the names alphabetically, that way all the numbers will match up.
# Week also won't matter if it is in long format because the trace will just appear.

for col in 2:4:
    if col == 4:
        temp = qb1.sort_values(cols[col], ascending=True)
    

    else: 
        temp = qb1.sort_values(cols[col], ascending=False)
        res = pd.DataFrame(data = temp.player_name)

# For WR: Recieving yards, recieving TDs and rushing TDs

# For TE: Recieving yards and recieving TDs

# For RB: Rushing TDs, rushing yards, recieving TDs and recieving yards





# Plot Rankings each week

import pandas as pd

dat = pd.read_csv('ranks.csv')
top_players = dat[dat["week"] == dat["week"].max()].nsmallest(5, "rank")

import matplotlib.pyplot as plt
from matplotlib.ticker import MultipleLocator, FixedFormatter, FixedLocator

fig, ax = plt.subplots(figsize=(8, 5), subplot_kw=dict(ylim=(0.5, 0.5 + len(top_players))))

ax.xaxis.set_major_locator(MultipleLocator(1))
ax.yaxis.set_major_locator(MultipleLocator(1))

yax2 = ax.secondary_yaxis("right")
yax2.yaxis.set_major_locator(FixedLocator(top_players["rank"].to_list()))
yax2.yaxis.set_major_formatter(FixedFormatter(top_players["player"].to_list()))

for i, j in dat.groupby("player"):
    ax.plot("week", "rank", "o-", data=j, mfc="w")

ax.invert_yaxis()
ax.set(xlabel="Week", ylabel="Rank", title="Rank of players")
ax.grid(axis="x")
plt.tight_layout()

