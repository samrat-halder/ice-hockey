#### Notes ####
# Major reference for this: https://gitlab.com/dword4/nhlapi/blob/master/stats-api.md#game-ids
# A list of tables created by this script are located at the bottom of the file
# The script will automatically Save data to an .RData file in your working directory

#### User inputs ####

setwd('./data')
seasons_to_pull_year <- 2014:2018

#### Loading packages ####
library(httr)
library(jsonlite)
library(data.table)
library(stringr)
library(lubridate)
library(readr)

#### Example api call ####
# base <- "https://statsapi.web.nhl.com/api/v1/"
# endpoint <- "teams"
# modifier <- "?expand=team.roster"
# call <- paste0(base,endpoint,modifier)
# 
# get_request <- GET(call)
# get_request_text <- content(get_request, "text")
# get_request_json <- fromJSON(get_request_text, flatten=TRUE)

#### Static pulls ####

base <- "https://statsapi.web.nhl.com/api/v1/"

### List of teams
endpoint <- "teams"
call <- paste0(base,endpoint)

get_request <- GET(call)
get_request_text <- content(get_request, "text")
get_request_json <- fromJSON(get_request_text, flatten=TRUE)

teams_DT <- setDT(get_request_json$teams)

vF_teams_DT <- teams_DT[,.(team.id=id
                           ,team.abbrev=abbreviation
                           ,short.name=teamName
                           ,long.name=name
                           ,firstYearOfPlay=as.integer(firstYearOfPlay)
                           ,locationName,franchiseId
                           ,division.name,conference.name
                           ,venue.name,venue.city)]

### Get game types
endpoint <- "gameTypes"
call <- paste0(base,endpoint)

get_request <- GET(call)
get_request_text <- content(get_request, "text")
get_request_json <- fromJSON(get_request_text, flatten=TRUE)

game_types <- setDT(get_request_json)

vF_game_types <- game_types

### Get play types
endpoint <- "playTypes"
call <- paste0(base,endpoint)

get_request <- GET(call)
get_request_text <- content(get_request, "text")
get_request_json <- fromJSON(get_request_text, flatten=TRUE)

play_types <- setDT(get_request_json)

vF_play_types <- play_types[,.(play.id=id,play.name=name)] # Don't really need the other columns
ids_we_care_about <- c('FACEOFF','HIT','GIVEAWAY','GOAL','SHOT','MISSED_SHOT','PENALTY','FIGHT','TAKEAWAY','BLOCKED_SHOT')
vF_play_types[,important := 0]
vF_play_types[play.id %in% ids_we_care_about ,important := 1]

### Get stat types
endpoint <- "statTypes"
call <- paste0(base,endpoint)

get_request <- GET(call)
get_request_text <- content(get_request, "text")
get_request_json <- fromJSON(get_request_text, flatten=TRUE)

stat_types <- setDT(get_request_json)

vF_stat_types <- stat_types

### Get seasons
endpoint <- "seasons"
call <- paste0(base,endpoint)

get_request <- GET(call)
get_request_text <- content(get_request, "text")
get_request_json <- fromJSON(get_request_text, flatten=TRUE)

seasons_DT <- setDT(get_request_json$seasons)
date_cols <- str_subset(names(seasons_DT),"Date")
seasons_DT[, (date_cols) := lapply(.SD,as.Date),.SDcols= date_cols]

vF_seasons_DT <- seasons_DT

#### Roster and player info by season ####

base <- "https://statsapi.web.nhl.com/api/v1/"

seasons_to_pull_both_years <-str_c(seasons_to_pull_year,seasons_to_pull_year+1)

### Team rosters
team_seasons <- data.table(full.season= rep(seasons_to_pull_both_years,each=31)
                        ,team.id= rep(vF_teams_DT$team.id,length(seasons_to_pull_both_years))
                        ,season= rep(seasons_to_pull_year,each=31))
team_seasons[,id.col := 1:team_seasons[,.N]]

team_roster_pull <- function(i, to_pull){
  season <- to_pull[i,1]
  team <- to_pull[i,2]
  modifier <- str_c('?expand=team.roster&season=',season)
  endpoint <- paste0('teams/',team,'/roster')
  call <- paste0(base,endpoint,modifier)
  
  get_request <- GET(call)
  get_request_text <- content(get_request, "text")
  get_request_json <- fromJSON(get_request_text, flatten=TRUE)
  return(get_request_json$roster)
}

roster_list <- lapply(team_seasons$id.col, team_roster_pull, to_pull = team_seasons)

vF_team_rosters <- rbindlist(roster_list,idcol = 'id.col')
vF_team_rosters <- merge(vF_team_rosters, team_seasons[,.(season,team.id,id.col)], by='id.col')
vF_team_rosters[, c('id.col', 'person.link', 'position.name', 'position.type', 'position.abbreviation') := NULL]
vF_team_rosters <- type_convert(vF_team_rosters)

### Get list of player IDs
vF_all_player_ids <- vF_team_rosters[,.N,by=person.id][,person.id]

### Extracting player season info and player info
modifier <- '?stats=yearByYear'
collected_player_data <- list()
player_info_list <- list()
for (i in vF_all_player_ids){
  endpoint <- paste0('people/',i)
  call <- paste0(base,endpoint)
  
  get_request <- GET(call)
  get_request_text <- content(get_request, "text")
  get_request_json <- fromJSON(get_request_text, flatten=T)
  player_info_list[[toString(i)]] <- as.data.table(get_request_json$people)
  
  endpoint <- paste0(endpoint,'/stats')
  call <- paste0(base,endpoint,modifier)
  
  get_request <- GET(call)
  get_request_text <- content(get_request, "text")
  get_request_json <- fromJSON(get_request_text, flatten=T)
  collected_player_data[[toString(i)]] <- as.data.table(get_request_json$stats$splits[[1]])
}

player_season_data <- rbindlist(collected_player_data, use.names=T, idcol="player.id", fill=T)
col_sel <- str_subset(names(player_season_data),'league.',negate=T)
vF_player_season_data <- type_convert(player_season_data[league.name == 'National Hockey League',..col_sel])
vF_player_season_data[,season := substr(season,1,4)]

player_info <- rbindlist(player_info_list, use.names=T, idcol="player.id", fill=T)
col_sel <- c('player.id','firstName','lastName','primaryNumber','birthDate','nationality','height','weight','active','shootsCatches','rosterStatus','currentTeam.id','primaryPosition.code')
vF_player_info <- type_convert(player_info[,..col_sel])

#### Pulling game data by season ####

### Writing function to pull game data
pull_game_data <- function(season, type, game){
  # Setting up and making call
  game_id <- paste0(season,type,game)
  base <- "https://statsapi.web.nhl.com/api/v1/"
  endpoint <- paste0("game/",game_id,"/feed/live")
  call <- paste0(base,endpoint)
  
  get_request <- GET(call)
  get_request_text <- content(get_request, "text")
  get_request_json <- fromJSON(get_request_text, flatten=TRUE)
  
  # Pulling out other basic info
  period_summary <- setDT(get_request_json$liveData$linescore$periods)
  shootout_status <- get_request_json$liveData$linescore$hasShootout
  venue <- setDT(get_request_json$gameData$venue)
  timing <- setDT(get_request_json$gameData$datetime)
  
  if(is.null(shootout_status)){return()}
  
  # Pulling out plays info if available
  plays <- setDT(get_request_json$liveData$plays$allPlays)
  plays_players <- rbindlist(plays$players, use.names=T,idcol="eventIdx",fill=T)
  
  if(plays[,.N] == 0){
    no_play_data <- TRUE
  } else {
    no_play_data <- FALSE
    plays_players <- rbindlist(plays$players, use.names=T,idcol="eventIdx",fill=T)[,eventIdx := eventIdx-1]
    plays <- plays[,!"players"]
    setnames(plays,'team.id','team.id.for')
    
    home_team <- get_request_json$liveData$boxscore$teams$home$team$id
    away_team <- get_request_json$liveData$boxscore$teams$away$team$id
    
    plays[team.id.for == home_team,c('team.id.against','HoA') := list(away_team,'H')]
    plays[team.id.for == away_team,c('team.id.against','HoA'):= list(home_team,'A')]
    
    try({
      plays <- merge(plays,period_summary[,.(num,home.rinkSide,away.rinkSide)],by.x='about.period',by.y='num')
      
      plays[, `:=` (coordinates.x = as.integer(coordinates.x)
                    ,coordinates.y = as.integer(coordinates.y))]
      
      plays[HoA == 'H', `:=` (s.x = ifelse(home.rinkSide=='left',coordinates.x,-coordinates.x)
                              ,s.y = ifelse(home.rinkSide=='left',coordinates.y,-coordinates.y))]

      plays[HoA == 'A', `:=` (s.x = ifelse(away.rinkSide=='right',coordinates.x,-coordinates.x)
                        ,s.y = ifelse(away.rinkSide=='right',coordinates.y,-coordinates.y))]

      plays[, `:=` (r.x = abs(coordinates.x)
                    ,l.x = -abs(coordinates.x))]
      
      plays[, `:=` (l.y = ifelse(coordinates.x<0,coordinates.y,-coordinates.y)
                    ,r.y = ifelse(coordinates.x>0,coordinates.y,-coordinates.y))]
    },silent=T)
  }
  
  # Determining who won
  home_win <- 0
  if(shootout_status){
    if(get_request_json$liveData$linescore$shootoutInfo$home$scores > get_request_json$liveData$linescore$shootoutInfo$away$scores){
      home_win <- 1
    }
  } else if(period_summary[,sum(home.goals)] > period_summary[,sum(away.goals)]){
    home_win <- 1
  }
  num_periods <- period_summary[,.N] + shootout_status # 3=finished in regular time, 4=OT, 5=SO
  
  # Creating game info table
  game_info <- data.table(season=season,type=type, home.win=home_win, away.win=1-home_win, settled.in=num_periods)
  game_info <- cbind(game_info,timing,venue)
   
  # Setting up lists 
  game_teams_stats_list <- list()
  game_skater_stats_list <- list()
  game_goalie_stats_list <- list()
  
  for (i in c("away","home")){
    # Determining what team it is
    team_id <- get_request_json$liveData$boxscore$teams[[i]]$team$id
    did_they_win <- ifelse(i=="home",home_win,1-home_win)
    
    # Pulling out some basic team info
    game_teams_stats_list[[i]] <- setDT(get_request_json$liveData$boxscore$teams[[i]]$teamStats$teamSkaterStats)[,c('team.id','won','settled.in') := list(team_id,did_they_win,num_periods)]
    game_info[, paste0(i,c('.teamID','.goals')) := list(team_id, game_teams_stats_list[[i]][,goals])]
    
    # Pulling data on all skaters and goalies
    skaters <- get_request_json$liveData$boxscore$teams[[i]]$skaters
    goalies <- get_request_json$liveData$boxscore$teams[[i]]$goalies
    team_skater_stat_list <- list()
    team_goalie_stat_list <- list()
    
    for (j in skaters){
      skater_id <- paste0("ID",j)
      team_skater_stat_list[[toString(j)]] <- setDT(get_request_json$liveData$boxscore$teams[[i]]$players[[skater_id]]$stats$skaterStats)
    }
    for (j in goalies){
      goalie_id <- paste0("ID",j)
      team_goalie_stat_list[[toString(j)]] <- setDT(get_request_json$liveData$boxscore$teams[[i]]$players[[goalie_id]]$stats$goalieStats)
    }
    
    # Merging player data together 
    game_skater_stats_list[[i]] <- rbindlist(team_skater_stat_list, use.names=T, idcol="player.id", fill=T)[,team.id := team_id]
    game_goalie_stats_list[[i]] <- rbindlist(team_goalie_stat_list, use.names=T, idcol="player.id", fill=T)[,team.id := team_id]

  }
  
  # Merging home/away data together
  game_teams_stats <- rbindlist(game_teams_stats_list, use.names=T,idcol="HoA",fill=T)
  game_skater_stats <- rbindlist(game_skater_stats_list, use.names=T,idcol="HoA",fill=T)
  game_goalie_stats <- rbindlist(game_goalie_stats_list, use.names=T,idcol="HoA",fill=T)
  
  return(list(plays, plays_players, period_summary, shootout_status, game_teams_stats, game_skater_stats, game_goalie_stats, game_info, no_play_data))
} 

### Pulling plays for all games in 2017

### Creating a vector of the potential game numbers
games_played_31 <- 31*82/2 #2017 onwards
games_played_30 <- 30*82/2 #Before 2017

game_nums_31 <- str_pad(seq(games_played_31), width=4,side="left",pad="0")
game_nums_30 <- str_pad(seq(games_played_30), width=4,side="left",pad="0")

game_plays_l <- list()
game_plays_players_l <- list()
game_periods_l <- list()
game_shootout_status_v <- list()
game_teams_stats_l <- list()
game_skater_stats_l <- list()
game_goalie_stats_l <- list()
game_info_l <- list()
no_play_data_l <- list()

sel_type <- "02"
for (sel_season in seasons_to_pull_year){
  if (sel_season<2017){game_nums <- game_nums_30} else {game_nums <- game_nums_31}
  for (i in game_nums){
    game_id <- paste0(sel_season, sel_type, i)
    g <- pull_game_data(sel_season, sel_type, i)
    
    game_plays_l[[game_id]] <- g[[1]]
    game_plays_players_l[[game_id]] <- g[[2]]
    game_periods_l[[game_id]] <- g[[3]]
    game_shootout_status_v[[game_id]] <- g[[4]]
    game_teams_stats_l[[game_id]] <- g[[5]]
    game_skater_stats_l[[game_id]] <- g[[6]]
    game_goalie_stats_l[[game_id]] <- g[[7]]
    game_info_l[[game_id]] <- g[[8]]
    no_play_data_l[[game_id]] <- g[[9]]
    print(game_id)
  }
}

game_plays <- rbindlist(game_plays_l, use.names=T, idcol="game.id", fill=T)
game_plays_players <- rbindlist(game_plays_players_l, use.names=T, idcol="game.id", fill=T)
game_periods <- rbindlist(game_periods_l, use.names=T, idcol="game.id", fill=T)
game_shootout_status <- data.table(game.id=names(game_shootout_status_v)
                                        ,shootout=unlist(game_shootout_status_v))
game_teams_stats <- rbindlist(game_teams_stats_l, use.names=T, idcol="game.id", fill=T)
game_skater_stats <- rbindlist(game_skater_stats_l, use.names=T, idcol="game.id", fill=T)
game_goalie_stats <- rbindlist(game_goalie_stats_l, use.names=T, idcol="game.id", fill=T)
game_info <- rbindlist(game_info_l, use.names=T, idcol="game.id", fill=T)
no_play_data <- data.table(game.id=names(no_play_data_l)
                                ,no.data=unlist(no_play_data_l))

#### Cleaning up play tables ####

col_sel <- c('game.id','about.period','result.eventTypeId','result.secondaryType','result.penaltySeverity','result.strength.code','about.eventIdx','about.periodType','about.periodTime','about.dateTime','about.goals.away','about.goals.home','coordinates.x','coordinates.y','team.id.for','team.id.against','HoA','s.x','s.y','r.x','r.y','l.x','l.y')
vF_game_plays <- merge(game_plays,vF_play_types[,c(1,3)],by.x='result.eventTypeId',by.y='play.id')[important==1,..col_sel]
vF_game_plays <- type_convert(vF_game_plays,col_types = cols(about.periodTime=col_time("%M:%S")))
vF_game_plays[HoA=="A", `:=` (p.x=-s.x, p.y=-s.y)]
vF_game_plays[HoA=="H", `:=` (p.x=s.x, p.y=s.y)]

vF_game_plays_players <- game_plays_players[,.(game.id,eventIdx,playerType,player.id=str_conv(player.id,'UTF8'))]

col_ex <- c('home.rinkSide','away.rinkSide')
col_sel <- setdiff(names(game_periods),col_ex)
vF_game_periods <- type_convert(game_periods[,..col_sel])

vF_game_teams_stats <- type_convert(game_teams_stats,col_types = cols(game.id=col_character()))

vF_game_skater_stats <- game_skater_stats
col_sel <- str_subset(names(vF_game_skater_stats),'OnIce')
vF_game_skater_stats[, (col_sel) := lapply(.SD, function(x){ifelse(str_length(x)==4,str_c('0',x),x)}), .SDcols=col_sel]
vF_game_skater_stats <- type_convert(vF_game_skater_stats, col_types = cols(
  game.id = col_character(),
  HoA = col_character(),
  player.id = col_character(),
  timeOnIce = col_time(format = "%M:%S"),
  evenTimeOnIce = col_time(format = "%M:%S"),
  powerPlayTimeOnIce = col_time(format = "%M:%S"),
  shortHandedTimeOnIce = col_time(format = "%M:%S")
))

vF_game_goalie_stats <- type_convert(game_goalie_stats, col_types = cols(
  game.id = col_character(),
  HoA = col_character(),
  player.id = col_character(),
  timeOnIce = col_character(),
  decision = col_character()
))

col_sel <- c('game.id','season','type','home.win','away.win','settled.in','dateTime','name','away.teamID','away.goals','home.teamID','home.goals')
vF_game_info <- type_convert(game_info[,..col_sel])

### No changes
vF_game_shootout_status <- game_shootout_status
vF_no_play_data <- no_play_data

#### Cleaned data output for project ####
# "Background info"
# vF_game_types # Data.table of game types
# vF_play_types # Data.table of play types
# vF_stat_types # Data.table of stat types that can be pulled from the api
# vF_seasons_DT # Data.table with each season that's been recorded (only the recent ones will have play level data)
# vF_all_player_ids # Vector of player ids extracted from the roster list
# 
# "Matching the relational database from that person on Kaggle"
# vF_teams_DT # Data.table of teams
# vF_game_plays # Play level data
# vF_game_plays_players # Players involved in plays
# vF_game_periods # Summary of periods in each game
# vF_game_shootout_status # Whether there was a shootout or not
# vF_game_teams_stats # Overall team stats
# vF_game_skater_stats # Player stats for each team
# vF_game_goalie_stats # Goalie stats for each team
# vF_game_info # Some game information
# vF_no_play_data # Tells which games did not have play data available
# 
# "Other info"
# vF_player_info # Information on every player (like age, nationality, etc.)
# vF_team_rosters # List of data.tables with players on each team for all seasons
# vF_player_season_data # Data.table with info on all seasons for each player in the players vector (includes non NHL seasons too)

# Cleaning up workspace
sel <- str_subset(ls(),"vF_",negate = T)
rm(list=sel)
rm(sel)

# Saving impage
save.image(paste0(Sys.Date(),"_nhl-cleaned-data.RData"))