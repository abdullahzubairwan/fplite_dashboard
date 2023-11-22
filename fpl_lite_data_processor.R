# Archived for reference
library(fplscrapR)
library(tidyverse)

fpl_bootstrap = jsonlite::fromJSON("https://fantasy.premierleague.com/api/bootstrap-static/")
pl_teams_id = fpl_bootstrap[["teams"]]
fpl_contender_league_id = 255511
# current_gw = 4

#
round_info = get_round_info()
# Getting League Details
fpl_league_query = function(league_id,league_type){
  if(!league_type %in% c("classic","h2h")){
    stop("Incorrect League Type; classic or h2h")
  }
  league <- jsonlite::fromJSON(curl::curl(paste("https://fantasy.premierleague.com/api/leagues-", 
                                                league_type, "/", league_id, "/standings/", sep = "")))
  return(league)
}


fpl_contender_participant = fpl_league_query(fpl_contender_league_id,"classic")
participant_df = fpl_contender_participant[["standings"]][["results"]]

# 0.0 Wall of Fame
# Run Once
# participant_historical = get_entry_hist(entryid=c(participant_df$entry))
# saveRDS(participant_historical,"docs/data/1_participant_historical.RDS")

# 1.0 Gameweek Points
entry_season = get_entry_season(participant_df$entry) %>% 
  left_join(.,round_info %>% select("event" = id,average_entry_score))
current_gw = max(entry_season$event)

# current_gw = max(entry_season$event)

gw_weekly_scores = entry_season %>%  
  mutate(above_avg = ifelse(points >= average_entry_score,"Above Average","Below Average"))
saveRDS(gw_weekly_scores,"docs/data/1_gw_weekly_scores.RDS")

# 2.0 Get Participant's Teams

######## Run once to get previous GW Teams #####

# ppn_team_df = data.frame()
# for(j in 1:current_gw){
#   for(i in 1:nrow(participant_df)){
#     participant_name = participant_df$player_name[i]
#     thisgw = get_entry_player_picks(entryid = participant_df[["entry"]][[i]],gw = j)
#     thisgw = thisgw %>% mutate(participant_name = participant_name)
#     ppn_team_df = bind_rows(ppn_team_df,thisgw)
# 
#   }
# }
# 
# saveRDS(ppn_team_df,"docs/data/2_participant_team_df.RDS")
# write_csv(ppn_team_df,"docs/data/2_participant_team_df.csv")

##### Run Weekly to get Latest Teams ####

ppn_team_df = readRDS("docs/data/2_participant_team_df.RDS")

for(i in 1:nrow(participant_df)){
      participant_name = participant_df$player_name[i]
      thisgw = get_entry_player_picks(entryid = participant_df[["entry"]][[i]],gw = current_gw)
      thisgw = thisgw %>% mutate(participant_name = participant_name)
      ppn_team_df = bind_rows(ppn_team_df,thisgw)

}
saveRDS(ppn_team_df,"docs/data/2_participant_team_df.RDS")
rm(participant_name,thisgw)

# 3.0 Basic Player Data 
######## Run once to get previous GW Teams #####
# Will take a long time to run!
# By right should be updated weekly
player_data_fpl = get_player_details()
player_data_fpl_detailed = get_player_info()

player_data_fpl_detailed_names = player_data_fpl_detailed %>% select(id,web_name,element_type) %>% mutate(position = case_when(
  element_type == 1 ~ "GKP",
  element_type == 2 ~ "DEF",
  element_type == 3 ~ "MID",
  element_type == 4 ~ "FWD"
)) %>% select(-element_type)
player_data_fpl_2 = left_join(player_data_fpl,player_data_fpl_detailed_names, by = c('element'='id'))
# Read Old and Update
readr::write_csv(player_data_fpl_2,"docs/data/2_fpl_player_stats_2324.csv")
saveRDS(player_data_fpl_2,"docs/data/2_fpl_player_stats_2324.RDS")

# 1.1 Extensive Player Data

# 4.0 Chip

chip_obtainer = function(id){
  require(dplyr)
  require(jsonlite)
  if (is.null(id)){
    stop("Please provide 1 or more ID")
  } 
  chip_df = data.frame()
  for (i in 1:length(id)) {
    entry_detail = jsonlite::fromJSON(paste0("https://fantasy.premierleague.com/api/entry/",id[i],"/"))
    entr_hist = jsonlite::fromJSON(paste0("https://fantasy.premierleague.com/api/entry/",id[i],"/history/"))
    chip_usage = entr_hist[["chips"]]
    if(length(chip_usage) > 0){
      chip_usage = chip_usage %>% dplyr::mutate(id = id[i],manager_name = paste(entry_detail$player_first_name,entry_detail$player_last_name))
    }else{
      next
    }
    chip_df = bind_rows(chip_df,chip_usage)
  }
  if(length(chip_df) == 0){
    txt = "No chip has been used by the selected Manager"
    return(txt)
  }else{
    return(chip_df)
  }
}

chip_tracker = chip_obtainer(participant_df$entry)
chip_tracker = chip_tracker %>% mutate(val = 1,
                                       chip = case_when(
                                         name != 'wildcard' ~ stringr::str_to_title(name),
                                         name == 'wildcard' & event %in% c(1:19) ~ "Wildcard 1",
                                         name == 'wildcard' & event %in% c(20:38) ~ "Wildcard 2"
                                       ),
                                       event = paste("GW",event))

saveRDS(chip_tracker,"docs/data/3_chip_tracker.RDS")

# 4.0 Transfer
# Get Weeks where Manager made transfers
transfer_check = entry_season %>% filter(event_transfers>0) %>% arrange(event)

league_player_transfers = data.frame()
for(i in 1:nrow(transfer_check)){
  current_gw = transfer_check$event[i]
  participant = transfer_check$name[i]
  prev_week = ppn_team_df %>% filter(participant_name == participant, event == current_gw-1) %>% arrange(position)
  current_week = ppn_team_df %>% filter(participant_name == participant, event == current_gw) %>% arrange(position)
  transferred_out = anti_join(prev_week,current_week, by = "id") %>% select(participant_name,"id_out" = id,"player_out" = playername,"prev_gw" = event)
  transferred_in = anti_join(current_week,prev_week, by = "id") %>% select("id_in" = id,"player_in" = playername,"gw" = event)
  
  newtable = cbind(transferred_out,transferred_in)
  league_player_transfers = bind_rows(league_player_transfers,newtable)
}

saveRDS(league_player_transfers,"docs/data/4_fpl_transfers.RDS")
