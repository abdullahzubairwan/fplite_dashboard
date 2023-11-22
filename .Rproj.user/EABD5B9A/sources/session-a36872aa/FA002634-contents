# Due to my inability to install fplscrapr via github actions yaml file due to lack of understanding on linux workarounds
# I copy pasted the necessary codes from fplscrapr package here
# All codes here belongs to the package maintainer/developer of FPLSCRAP

# 1.0 Get Round Info
get_round_info = function (round = NULL, season = NULL) 
{
  ifelse(!is.null(season), {
    events <- read.csv(paste0("https://raw.githubusercontent.com/wiscostret/histfpldata/master/getroundinfo", 
                              season, ".csv"), encoding = "UTF-8")
  }, {
    events <- jsonlite::fromJSON("https://fantasy.premierleague.com/api/bootstrap-static/")$events
  })
  ifelse(is.null(round), {
    return(events)
  }, {
    return(events %>% dplyr::filter(id %in% round))
  })
}

#2.0 Get Player Name
get_player_name = function (playerid = NULL) 
{
  ifelse(is.null(playerid), {
    elements <- jsonlite::fromJSON("https://fantasy.premierleague.com/api/bootstrap-static/")$elements
    elements$playername <- paste(elements$first_name, elements$second_name)
    return(elements %>% dplyr::select(playername, id))
  }, {
    elements <- jsonlite::fromJSON("https://fantasy.premierleague.com/api/bootstrap-static/")$elements
    elements$playername <- paste(elements$first_name, elements$second_name)
    return(elements %>% dplyr::filter(id %in% playerid) %>% 
             dplyr::select(playername, id))
  })
}

#3.0 Get Player Info
get_player_info = function (name = NULL, season = NULL) 
{
  ifelse(is.null(season), {
    elements <- jsonlite::fromJSON("https://fantasy.premierleague.com/api/bootstrap-static/")$elements
    elements$playername <- paste(elements$first_name, elements$second_name)
  }, {
    elements <- read.csv(paste0("https://raw.githubusercontent.com/wiscostret/histfpldata/master/getplayerinfo", 
                                season, ".csv"), encoding = "UTF-8")
  })
  ifelse(is.null(name), return(elements), return(elements %>% 
                                                   dplyr::filter(playername %in% name)))
}

#4.0 Get player details
get_player_details = function (playerid = NULL, name = NULL, season = NULL) 
{
  if (!is.null(playerid) & !is.null(name)) 
    stop("Please only supply playerid OR name, not both.")
  ifelse(!is.null(season), {
    detinfo <- read.csv(paste0("https://raw.githubusercontent.com/wiscostret/histfpldata/master/getplayerdetails", 
                               season, ".csv"), encoding = "UTF-8")
    ifelse(is.null(playerid), ifelse(is.null(name), return(detinfo), 
                                     return(detinfo %>% dplyr::filter(playername %in% 
                                                                        name))), return(detinfo %>% dplyr::filter(playerid %in% 
                                                                                                                    id)))
  }, {
    elements <- jsonlite::fromJSON("https://fantasy.premierleague.com/api/bootstrap-static/")$elements
    elements$playername <- paste(elements$first_name, elements$second_name)
    ifelse(is.null(playerid), ifelse(is.null(name), {
      detinfo <- data.frame()
      for (i in 1:nrow(elements)) {
        fplboot <- jsonlite::fromJSON(url(paste("https://fantasy.premierleague.com/api/element-summary/", 
                                                elements$id[i], "/", sep = "")))$history
        detinfo <- plyr::rbind.fill(detinfo, data.frame(cbind(fplboot, 
                                                              playername = elements$playername[which(elements$id == 
                                                                                                       elements$id[i])])))
      }
      return(detinfo)
    }, {
      detinfo <- data.frame()
      selection <- elements %>% dplyr::filter(playername %in% 
                                                name) %>% dplyr::select(id) %>% unlist() %>% 
        as.numeric()
      for (i in 1:length(selection)) {
        fplboot <- jsonlite::fromJSON(url(paste("https://fantasy.premierleague.com/api/element-summary/", 
                                                selection[i], "/", sep = "")))$history
        detinfo <- plyr::rbind.fill(detinfo, data.frame(cbind(fplboot, 
                                                              playername = elements$playername[which(elements$id == 
                                                                                                       selection[i])])))
      }
      return(detinfo)
    }), {
      detinfo <- data.frame()
      for (i in 1:length(playerid)) {
        fplboot <- jsonlite::fromJSON(url(paste("https://fantasy.premierleague.com/api/element-summary/", 
                                                playerid[i], "/", sep = "")))$history
        detinfo <- plyr::rbind.fill(detinfo, data.frame(cbind(fplboot, 
                                                              playername = elements$playername[which(elements$id == 
                                                                                                       playerid[i])])))
      }
      return(detinfo)
    })
  })
}

# 5.0 Get Entry Season
get_entry_season = function (entryid = NULL) 
{
  if (is.null(entryid)) 
    stop("You'll need to input at least one entry ID, mate.")
  {
    entryseason2 <- data.frame()
    for (i in 1:length(entryid)) {
      entry <- jsonlite::fromJSON(paste("https://fantasy.premierleague.com/api/entry/", 
                                        entryid[i], "/", sep = ""))
      entryseason <- jsonlite::fromJSON(paste("https://fantasy.premierleague.com/api/entry/", 
                                              entryid[i], "/history", "/", sep = ""))
      entryseason2 <- rbind(entryseason2, data.frame(entryseason$current, 
                                                     name = paste(entry$player_first_name, entry$player_last_name)))
    }
    return(entryseason2)
    }
}

# 6.0 Get Entry Player Picks
get_entry = function (entryid = NULL) 
{
  if (is.null(entryid)) 
    stop("You'll need to input at least one entry ID, mate.")
  if (length(entryid) != 1) 
    stop("One at a time, please")
  {
    entry <- jsonlite::fromJSON(paste("https://fantasy.premierleague.com/api/entry/", 
                                      entryid, "/", sep = ""))
    return(entry)
    }
}




get_entry_player_picks = function (entryid = NULL, gw = NULL) 
{
  if (is.null(entryid)) 
    stop("You'll need to input an entry ID, mate.")
  if (is.null(gw)) 
    stop("You'll need to input a gameweek, mate.")
  picks3 <- data.frame()
  for (j in 1:length(entryid)) {
    picks2 <- data.frame()
    gw_started <- get_entry(entryid[j])[["started_event"]]
    gw_thisentry <- gw[gw >= gw_started]
    for (i in 1:length(gw_thisentry)) {
      picks_list <- jsonlite::fromJSON(paste("https://fantasy.premierleague.com/api/entry/", 
                                             entryid[j], "/event/", gw_thisentry[i], "/picks", 
                                             "/", sep = ""))
      picks <- picks_list$picks
      picks$event <- gw_thisentry[i]
      picks2 <- rbind(picks2, picks)
    }
    picks2$entry <- entryid[j]
    picks3 <- rbind(picks3, picks2)
  }
  player_names <- get_player_name(unique(picks3$element))
  picks3 <- merge(player_names, picks3, by.x = "id", by.y = "element")
  names(picks3) <- c("id", "playername", "position", "multiplier", 
                     "is_captain", "is_vice_captain", "event", "entry")
  picks3 <- picks3[order(picks3$entry, picks3$event), ]
  return(picks3)
}
