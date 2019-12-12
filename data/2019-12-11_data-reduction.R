library(data.table)

load("./data/2019-12-07_nhl-cleaned-data.RData")

not_needed <- c("vF_all_player_ids", "vF_game_goalie_stats", "vF_game_periods", "vF_game_shootout_status",
                "vF_game_skater_stats", "vF_game_types", "vF_no_play_data", "vF_play_types", "vF_seasons_DT",
                "vF_stat_types", "vF_team_rosters")

rm(list=not_needed)
rm(not_needed)

# Reducing size of the following
drop_cols <- c('type', 'settled.in', 'dateTime')
vF_game_info <- vF_game_info[,!(names(vF_game_info) %in% drop_cols),with=F]

drop_cols <- c('result.penaltySeverity', 'result.strength.code', 'about.periodType', 'about.periodTime',
               'about.dateTime', 'about.goals.away', 'about.goals.home', 'coordinates.x', 'coordinates.y',
               's.x', 's.y', 'p.x', 'p.y')
vF_game_plays <- vF_game_plays[result.eventTypeId %in% c("SHOT","GOAL"), !(names(vF_game_plays) %in% drop_cols),with=F]

vF_game_plays_players <- vF_game_plays_players[playerType %in% c('Shooter','Scorer')]

drop_cols <- c('powerPlayPercentage', 'powerPlayGoals', 'powerPlayOpportunities', 'faceOffWinPercentage')
vF_game_teams_stats <- vF_game_teams_stats[,!(names(vF_game_teams_stats) %in% drop_cols),with=F]

drop_cols <- c('primaryNumber', 'birthDate', 'nationality', 'height', 'weight', 'active', 'shootsCatches',
               'rosterStatus', 'currentTeam.id')
vF_player_info <- vF_player_info[,!(names(vF_player_info) %in% drop_cols),with=F]

drop_cols <- c('sequenceNumber', 'stat.powerPlayPoints', 'stat.powerPlayGoals', 'stat.gameWinningGoals','stat.overTimeGoals',
               'stat.shortHandedGoals','stat.shortHandedPoints','stat.powerPlayTimeOnIce', 'stat.evenTimeOnIce', 
               'stat.shortHandedTimeOnIce', 'stat.faceOffPct', 'stat.shifts', 'team.link', 'stat.ties', 'stat.saves',
               'stat.powerPlaySaves', 'stat.shortHandedSaves', 'stat.evenSaves', 'stat.shortHandedShots', 'stat.evenShots',
               'stat.powerPlayShots', 'stat.gamesStarted', 'stat.shotsAgainst', 'stat.powerPlaySavePercentage',
               'stat.evenStrengthSavePercentage', 'stat.ot', 'stat.shortHandedSavePercentage')
vF_player_season_data <- vF_player_season_data[season >= 2014, !(names(vF_player_season_data) %in% drop_cols),with=F]

rm(drop_cols)
save.image('./data/2019-12-11_app-data.RData')

### Aligning name of Montreal's and Tampa's arena across data frames
load('./data/2019-12-11_app-data.RData')

# vF_teams_DT # For the names to change them to 
vF_game_info[home.teamID == 8, name := 'Bell Centre'] #Centre Bell before
vF_game_info[home.teamID == 14, name := 'AMALIE Arena'] #Amalie Arena before

save.image('./data/2019-12-11_app-data.RData')
