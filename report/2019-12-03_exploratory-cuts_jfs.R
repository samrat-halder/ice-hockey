#### Loading in data and packages ####

library(data.table)
library(ggplot2)
library(stringr)

load('./data/2019-11-26_nhl-cleaned-data.RData')

#### Cuts of play data ####

vF_game_plays[,year := as.integer(substr(game.id,1,4))]

vF_game_plays[,games_in_season := length(unique(game.id)),by=.(year)]

tmp <- vF_game_plays[,.(.N, num_games = min(games_in_season)),by=.(year,result.eventTypeId)]
tmp[,rate := N/num_games]

### Occurrence of these events
ggplot(tmp[result.eventTypeId=="SHOT"]) +
  geom_line(aes(year,rate)) +
  scale_y_continuous(limits = c(0,60))

ggplot(tmp[result.eventTypeId=="HIT"]) +
  geom_line(aes(year,rate)) +
  scale_y_continuous(limits = c(0,60))

ggplot(tmp[result.eventTypeId=="GOAL"]) +
  geom_line(aes(year,rate)) +
  scale_y_continuous(limits = c(0,6))

### Ratio of H vs A

tmp2 <- vF_game_plays[,.(.N),by=.(year,result.eventTypeId,HoA)]

tmp2 <- dcast(tmp2, year + result.eventTypeId ~ HoA, drop=F)[,ratio := H/A]

# ggplot(tmp2[result.eventTypeId=="SHOT"]) +
#   geom_line(aes(year,ratio)) +
#   scale_y_continuous(limits = c(.9,1.1))
# 
# ggplot(tmp2[result.eventTypeId=="HIT"]) +
#   geom_line(aes(year,ratio)) +
#   scale_y_continuous(limits = c(.9,1.1))
# 
# ggplot(tmp2[result.eventTypeId=="GOAL"]) +
#   geom_line(aes(year,ratio)) +
#   scale_y_continuous(limits = c(.8,1.2))

ggplot(tmp2) +
  geom_line(aes(year,ratio,color=result.eventTypeId)) +
  scale_y_continuous(limits = c(.8,1.2))

### H vs. A by period

tmp3 <- vF_game_plays[,.(.N),by=.(result.eventTypeId,HoA,about.period)]

tmp3 <- dcast(tmp3, result.eventTypeId + about.period ~ HoA)[,ratio := H/A]

ggplot(tmp3) +
  geom_line(aes(about.period,ratio,color=result.eventTypeId)) +
  scale_y_continuous(limits = c(.8,1.2)) +
  scale_x_continuous(limits = c(1,3), breaks = scales::pretty_breaks(n=3))

#### Game info cuts ####
vF_game_info[away.goals>home.goals, `:=` (away.win = 1, home.win = 0)]
vF_game_info[away.goals<home.goals, `:=` (away.win = 0, home.win = 1)]

tmp4 <- vF_game_info[,.(ratio=sum(home.win)/sum(away.win)),by=season]

ggplot(tmp4) +
  geom_col(aes(season,ratio))
