## Preparing for Worlds 2023: Team/Player-Based Data Analysis
## Author: Olivia Giandrea

library(tidyverse)
library(rvest)
library(dplyr)
library(stringr)
library(jsonlite)

# function to scrape list of team names from wiki
scrape_team_names <- function(url) {
  # an empty list to store team names and URLs
  team_links <- list()
  
  # find all page links for teams going to worlds
  webpage <- read_html(url)
  teams_table <- webpage %>% html_nodes(".wikitable .tWAN")
  
  # append each team name and URL to the results list as a list
  for (team in teams_table) {
    team_name <- team %>% html_attr("title")
    team_url <- paste0("https://lol.fandom.com", team %>% html_attr("href"))  # Make sure to add the base URL
    team_links <- append(team_links, list(list(team_name, team_url)))
  }
  
  return(team_links)
}

team_links <- scrape_team_names("https://lol.fandom.com/wiki/2023_Season_World_Championship")

# Convert results into a data frame
teams_df <- data.frame(do.call(rbind, team_links))
colnames(teams_df) <- c("name", "url")
glimpse(teams_df)

# function to scrape team data from wiki
scrape_team_data <- function(teams_df) {
  # create empty vectors to store scraped data
  team_names <- character(0)
  opponents <- character(0)
  dates <- character(0)
  tournaments <- character(0)
  win_losses <- character(0)
  sides <- character(0)
  bans <- list()
  opp_bans <- list()
  picks <- list()
  opp_picks <- list()
  players <- list()
  player_links <- list()
  
  for (i in 1:nrow(teams_df)) {
    print(i)
    team_name <- gsub("\\.", " ", teams_df[i, "name"])
    print(team_name)
    team_url <- teams_df[i, "url"]
    
    # navigate to, then scrape data from team matches table
    url <- paste0(team_url, "/Match_History")
    webpage <- read_html(url)
    
    # find all rows in matches table
    matches <- webpage %>% html_nodes(".multirow-highlighter")
    
    list_indexer = length(bans)
    
    for (j in 1:length(matches)) {
      match = matches[j]
      
      if (length(match %>% html_nodes("td:nth-child(7) span")) < 5) {
        break
      }
      
      # find team name, opponent, date, tournament, win/loss, side
      opponent <- match %>% html_node("td:nth-child(6) a") %>% html_attr("data-to-id")
      opponent <- gsub("_", " ", opponent)
      opponent <- gsub("_2e-", " ", opponent)
      opponent <- gsub("KOI__28-Spanish_Team_29-", "KOI (Spanish)", opponent)
      opponent <- gsub("Team_Secret__28-Vietnamese_Team_29-", "Team Secret (Vietnamese)", opponent)
      date <- match %>% html_node("td:nth-child(1)") %>% html_text()
      tournament <- match %>% html_node("td:nth-child(2) a") %>% html_text()
      win_loss <- match %>% html_node("td:nth-child(4)") %>% html_text()
      side <- match %>% html_node("td:nth-child(5)") %>% html_text()
      
      # append other data to vectors
      team_names <- c(team_names, team_name)
      opponents <- c(opponents, opponent)
      dates <- c(dates, date)
      tournaments <- c(tournaments, tournament)
      win_losses <- c(win_losses, win_loss)
      sides <- c(sides, side)
      
      # iterate over bans, opponent bans, picks, opponent picks, players, player links to build vectors
      match_bans <- character(5)
      ban_list <- match %>% html_nodes("td:nth-child(7) span")
      for (k in 1:5) {
        match_bans[k] <- ban_list[k] %>% html_attr("title") 
      }
      bans[[j+list_indexer]] <- match_bans

      match_opp_bans <- character(5)
      opp_ban_list <- match %>% html_nodes("td:nth-child(8) span")
      for (k in 1:5) {
        match_opp_bans[k] <- opp_ban_list[k] %>% html_attr("title") 
      }
      opp_bans[[j+list_indexer]] <- match_opp_bans
      
      match_picks <- character(5)
      pick_list <- match %>% html_nodes("td:nth-child(9) span")
      for (k in 1:5) {
        match_picks[k] <- pick_list[k] %>% html_attr("title") 
      }
      picks[[j+list_indexer]] <- match_picks
      
      match_opp_picks <- character(5)
      opp_pick_list <- match %>% html_nodes("td:nth-child(10) span")
      for (k in 1:5) {
        match_opp_picks[k] <- opp_pick_list[k] %>% html_attr("title") 
      }
      opp_picks[[j+list_indexer]] <- match_opp_picks
      
      match_players <- character(5)
      match_player_links <- character(5)
      player_list <- match %>% html_nodes("td:nth-child(11) a")
      for (k in 1:5) {
        match_players[k] <- player_list[k] %>% html_attr("title") 
        match_player_links[k] <- player_list[k] %>% html_attr("href") 
      }
      players[[j+list_indexer]] <- match_players
      player_links[[j+list_indexer]] <- match_player_links
    }
  }
  
  # create a data frame from the vectors
  team_data <- data.frame(
    TeamName = team_names,
    Opponent = opponents,
    Date = dates,
    Tournament = tournaments,
    WinLoss = win_losses,
    Side = sides,
    Bans = I(bans),
    OpponentBans = I(opp_bans),
    Picks = I(picks),
    OpponentPicks = I(opp_picks),
    Players = I(players),
    PlayerLinks = I(player_links)
  )
  
  return(team_data)
}

# Call the function and store the results in a data frame
results <- scrape_team_data(teams_df)
glimpse(results)

json_data <- toJSON(results)
file_path <- "C:/Users/olivi/repos/data/lol_worlds_team_data.json"
write(json_data, file_path)

player_links_df <- data.frame(link = unlist(teams_data$PlayerLinks))
glimpse(player_links)

# function to scrape team data from wiki
scrape_player_champ_data <- function(player_links) {
  # create empty vectors to store scraped data
  names <- character(0)
  positions <- character(0)
  teams <- character(0)
  tournaments <- character(0)
  champions <- character(0)
  num_gamess <- integer(0)
  winss <- integer(0)
  lossess <- integer(0)
  win_rates <- numeric(0)
  killss <- numeric(0)
  deathss <- numeric(0)
  assistss <- numeric(0)
  kdas <- numeric(0)
  css <- numeric(0)
  golds <- integer(0)
  damages <- integer(0)
  kill_participations <- numeric(0)
  kill_shares <- numeric(0)
  gold_shares <- numeric(0)
  
  for (i in 1:nrow(player_links)) {
    print(i)
    team <- player_links[i, "team"]
    
    # find player's role and team in home page
    url <- paste0("https://lol.fandom.com", player_links[i, "link"])
    home_page <- read_html(url)
    position <- home_page %>% html_node(".role-sprite+ .markup-object-name") %>% html_text()
    position <- gsub(" Laner", "", position)
    team <- home_page %>% html_node("#infoboxPlayer span .teamname a") %>% html_text() 

    # navigate to, then scrape data from team matches table
    url <- paste0("https://lol.fandom.com", player_links[i, "link"], "/Statistics/2023")
    print(url)
    
    tryCatch({
      webpage <- read_html(url)
      name <- webpage %>% html_node("a.to_hasTooltip") %>% html_attr("title")
      
      # find all tables (each table is a different tournament)
      tournament_tables <- webpage %>% html_nodes(".wikitable")
      
      for (table in tournament_tables) {
        tournament <- table %>% html_node("th[colspan='18'] a.to_hasTooltip[title*='Season']") %>% html_text()
        rows <- table %>% html_nodes("tbody tr")
        rows <- rows[-c(1:3, (length(rows) - 1):length(rows))]
        
        for (row in rows) {
          champion <- row %>% html_node("td:nth-child(1) span .champion-sprite") %>% html_attr("title")
          num_games <- row %>% html_node("td:nth-child(2) a") %>% html_text()
          wins <- row %>% html_node("td:nth-child(3)") %>% html_text()
          losses <- row %>% html_node("td:nth-child(4)") %>% html_text()
          win_rate <- row %>% html_node("td:nth-child(5)") %>% html_text()
          kills <- row %>% html_node("td:nth-child(6)") %>% html_text()
          deaths <- row %>% html_node("td:nth-child(7)") %>% html_text()
          assists <- row %>% html_node("td:nth-child(8)") %>% html_text()
          kda <- row %>% html_node("td:nth-child(9)") %>% html_text()
          cs <- row %>% html_node("td:nth-child(10)") %>% html_text()
          gold <- row %>% html_node("td:nth-child(12)") %>% html_text()
          damage <- row %>% html_node("td:nth-child(14)") %>% html_text()
          kill_participation <- row %>% html_node("td:nth-child(16)") %>% html_text()
          kill_share <- row %>% html_node("td:nth-child(17)") %>% html_text()
          gold_share <- row %>% html_node("td:nth-child(18)") %>% html_text()
          
          names <- c(names, name)
          positions <- c(positions, position)
          teams <- c(teams, team)
          tournaments <- c(tournaments, tournament)
          champions <- c(champions, champion)
          num_gamess <- c(num_gamess, as.integer(num_games))
          winss <- c(winss, as.integer(wins))
          lossess <- c(lossess, as.integer(losses))
          win_rates <- c(win_rates, as.numeric(sub("%", "", win_rate)))
          killss <- c(killss, as.numeric(kills))
          deathss <- c(deathss, as.numeric(deaths))
          assistss <- c(assistss, as.numeric(assists))
          kdas <- c(kdas, as.numeric(kda))
          css <- c(css, as.numeric(cs))
          golds <- c(golds, as.integer(as.numeric(gold) * 1000))
          damages <- c(damages, as.integer(as.numeric(sub("k", "", damage)) * 1000))
          kill_participations <- c(kill_participations, as.numeric(sub("%", "", kill_participation)))
          kill_shares <- c(kill_shares, as.numeric(sub("%", "", kill_share)))
          gold_shares <- c(gold_shares, as.numeric(sub("%", "", gold_share)))
        }
      }
    }, error = function(e) {
      # Handle HTTP 404 error (or other errors) here
      if (grepl("HTTP 404", e$message)) {
        cat(paste(team, "stats not found\n"))
        # skip team and continue the loop on error
        next
      } else {
        # Handle other errors here if needed
        cat("Error occurred for team:", team, "\n")
        cat("Error message:", e$message, "\n")
      }
    })
  }
  
  # create a data frame from the vectors
  player_champ_data <- data.frame(
    Name = names,
    Position = positions, 
    Team = teams,
    Tournament = tournaments,
    Champion = champions,
    NumGames = num_gamess,
    Wins = winss,
    Losses = lossess,
    WinRate = win_rates,
    Kills = killss,
    Deaths = deathss,
    Assists = assistss,
    KDA = kdas,
    CS = css,
    Gold = golds,
    Damage = damages,
    KillParticipation = kill_participations,
    KillShare = kill_shares,
    GoldShare = gold_shares
  )
  
  return(player_champ_data)
}

# Call the function and store the results in a data frame
player_champ_data <- scrape_player_champ_data(unique(player_links_df))
player_champ_data <- player_champ_data[!is.na(player_champ_data$Tournament), ]
glimpse(player_champ_data)

json_data <- toJSON(player_champ_data)
file_path <- "C:/Users/olivi/repos/data/lol_worlds_player_champ_data.json"
write(json_data, file_path)

# function to scrape player match data from wiki
scrape_player_match_data <- function(player_links) {
  # create empty vectors to store scraped data
  names <- character(0)
  dates <- character(0)
  tournaments <- character(0)
  win_losses <- character(0)
  sides <- character(0)
  teams <- character(0)
  opponents <- character(0)
  lengths <- character(0)
  champions <- character(0)
  killss <- integer(0)
  deathss <- integer(0)
  assistss <- integer(0)
  kdas <- numeric(0)
  
  for (i in 1:nrow(player_links)) {
    print(i)
    
    # navigate to, then scrape data from team matches table
    url <- paste0("https://lol.fandom.com", player_links[i, "link"], "/Match_History")
    print(url)
    
    tryCatch({
      webpage <- read_html(url)
      name <- webpage %>% html_node("a.to_hasTooltip") %>% html_attr("title")
      
      # find all rows in match table
      rows <- webpage %>% html_nodes(".wikitable tr")
      rows <- rows[-c(1:4, (length(rows) - 9):length(rows))]
      
      for (row in rows) {
        date <- row %>% html_node("td:nth-child(1)") %>% html_text()
        tournament <- row %>% html_node("td:nth-child(2) a") %>% html_text()
        win_loss <- row %>% html_node("td:nth-child(4)") %>% html_text()
        side <- row %>% html_node("td:nth-child(5)") %>% html_text()
        team <- row %>% html_node("td:nth-child(6) a") %>% html_attr("title")
        opponent <- row %>% html_node("td:nth-child(7) a") %>% html_attr("title")
        length <- row %>% html_node("td:nth-child(8)") %>% html_text()
        champion <- row %>% html_node("td:nth-child(9) span") %>% html_attr("title")
        kills <- row %>% html_node(".mhplayer-k") %>% html_text()
        deaths <- row %>% html_node(".mhplayer-d") %>% html_text()
        assists <- row %>% html_node(".mhplayer-a") %>% html_text()
        kda <- row %>% html_node(".mhplayer-a+ td") %>% html_text()
        
        names <- c(names, name)
        dates <- c(dates, date)
        tournaments <- c(tournaments, tournament)
        win_losses <- c(win_losses, win_loss)
        sides <- c(sides, side)
        teams <- c(teams, team)
        opponents <- c(opponents, opponent)
        lengths <- c(lengths, length)
        champions <- c(champions, champion)
        killss <- c(killss, as.integer(kills))
        deathss <- c(deathss, as.integer(deaths))
        assistss <- c(assistss, as.integer(assists))
        kdas <- c(kdas, as.numeric(kda))
      }
    }, error = function(e) {
      # Handle HTTP 404 error (or other errors) here
      if (grepl("HTTP 404", e$message)) {
        cat(paste(name, "stats not found\n"))
        # skip team and continue the loop on error
        next
      } else {
        # Handle other errors here if needed
        cat("Error occurred for", name, "\n")
        cat("Error message:", e$message, "\n")
      }
    })
  }
  
  # create a data frame from the vectors
  player_match_data <- data.frame(
    Name = names,
    Date = dates, 
    Tournament = tournaments,
    WinLoss = win_losses,
    Side = sides,
    Team = teams,
    Opponent = opponents,
    Length = lengths,
    Champion = champions,
    Kills = killss,
    Deaths = deathss,
    Assists = assistss,
    KDA = kdas
  )
  
  return(player_match_data)
}

# Call the function and store the results in a data frame
player_match_data <- scrape_player_match_data(unique(player_links_df))
glimpse(player_match_data)

json_data <- toJSON(player_match_data)
file_path <- "C:/Users/olivi/repos/data/lol_worlds_player_match_data.json"
write(json_data, file_path)

# function to scrape pentakill data from wiki
scrape_pentakill_data <- function() {
  # create empty vectors to store scraped data
  dates <- character(0)
  players <- character(0)
  regions <- character(0)
  tournaments <- character(0)
  teams <- character(0)
  opponents <- character(0)
  champions <- character(0)
  positions <- character(0)
  win_losses <- character(0)
  kdas <- character(0)
  
  webpage <- read_html("https://lol.fandom.com/wiki/Pentakills/2023")

  # find all pentakill table rows 
  rows <- webpage %>% html_nodes("tr")
  filtered_rows <- Filter(function(tr) {
    !any(html_name(html_children(tr)) == "th")
  }, rows)

  
  for (i in 1:length(filtered_rows)) {
    print(i)
    row = filtered_rows[i]
    date <- row %>% html_node("td:nth-child(1)") %>% html_text()
    player <- row %>% html_node("td:nth-child(2) a") %>% html_text()
    region <- row %>% html_node("td:nth-child(3) span .markup-object-name") %>% html_text()
    tournament <- row %>% html_node("td:nth-child(4) a") %>% html_text()
    team <- row %>% html_node("td:nth-child(5) a") %>% html_attr("data-to-id")
    team <- gsub("_2e-", " ", opponent)
    team <- gsub("_", " ", opponent)
    opponent <- row %>% html_node("td:nth-child(6) a") %>% html_attr("data-to-id")
    opponent <- gsub("_2e-", " ", opponent)
    opponent <- gsub("_", " ", opponent)
    champion <- row %>% html_node("td:nth-child(7) span .champion-sprite") %>% html_attr("title")
    position <- row %>% html_node("td:nth-child(8)") %>% html_text()
    win_loss <- row %>% html_node("td:nth-child(9)") %>% html_text()
    kda <- row %>% html_node("td:nth-child(10)") %>% html_text()
    
    dates <- c(dates, date)
    players <- c(players, player)
    regions <- c(regions, region)
    tournaments <- c(tournaments, tournament)
    teams <- c(teams, team)
    opponents <- c(opponents, opponent)
    champions <- c(champions, champion)
    positions <- c(positions, position)
    win_losses <- c(win_losses, win_loss)
    kdas <- c(kdas, kda)
  }
  
  # create a data frame from the vectors
  pentakill_data <- data.frame(
    Date = dates,
    Player = players,
    Region = regions,
    Tournament = tournaments,
    Team = teams,
    Opponent = opponents,
    Champion = champions,
    Position = positions,
    WinLoss = win_losses,
    KDA = kdas
  )
  
  return(pentakill_data)
}

# Call the function and store the results in a data frame
pentakill_data <- scrape_pentakill_data()
glimpse(pentakill_data)

json_data <- toJSON(pentakill_data)
file_path <- "C:/Users/olivi/repos/data/lol_worlds_pentakill_data.json"
write(json_data, file_path)



############################################################################
## Analyzing Data                                                         ##
############################################################################

base_path = "C:/Users/olivi/repos/data/"

player_champ_data <- read_json(paste0(base_path, "lol_worlds_player_champ_data.json"), simplifyVector = TRUE)
glimpse(player_champ_data)

player_match_data <- read_json(paste0(base_path, "lol_worlds_player_match_data.json"), simplifyVector = TRUE)
glimpse(player_match_data)

team_data <- read_json(paste0(base_path, "lol_worlds_team_data.json"), simplifyVector = TRUE)
glimpse(team_data)

pentakill_data <- read_json(paste0(base_path, "lol_worlds_pentakill_data.json"), simplifyVector = TRUE)
glimpse(pentakill_data)



############################################################################
##       Which champion will be played in the most different Roles?       ##
############################################################################

find_most_varied_champ <- function(player_data) {
  # Count the unique combinations of champions and positions
  filtered_player_data <- player_data %>%
    filter(Position != "Substitute")
  
  # Count the unique combinations of champions and positions
  champion_position_counts <- filtered_player_data %>%
    group_by(Champion) %>%
    summarize(NumPositions = n_distinct(Position))
  
  # Find the top 3 champions played in the most different positions
  most_played_champions <- champion_position_counts %>%
    filter(NumPositions == 4)
  
  # Loop through the top champions
  cat("There were", paste(nrow(most_played_champions)), "Champions played in 4 roles during the 2023 season:\n")
  for (champion_name in most_played_champions$Champion) {
    # Filter the player_data dataframe for the champion
    champion_data <- filtered_player_data %>%
      filter(Champion == champion_name)
    
    # Get the unique positions for the champion
    num_positions <- unique(champion_data$Position)
    
    # Print information about the champion
    cat(paste0("\t", champion_name, " (", paste(num_positions, collapse = ", "), ")\n"))
  }
  cat("\n")
}
find_most_varied_champ(player_champ_data)

############################################################################
##                Which champion will be Picked the most?                 ##
############################################################################

find_most_picked_champ <- function(player_data) {
  # Group the data by champion and calculate the count
  champion_counts <- player_data %>%
    group_by(Champion) %>%
    summarize(NumPicks = n())
  
  # Find the champion with the highest pick count
  most_picked_champion <- champion_counts %>%
    arrange(desc(NumPicks)) %>%
    head(3)
  
  # Loop through the top champions
  cat("The top 3 picked champions in the 2023 season were:\n")
  for (i in 1:nrow(most_picked_champion)) {
    row <- most_picked_champion[i, ]
    cat(paste0("\t", row$Champion, " (", row$NumPicks, "times )\n"))
  }
  cat("\n")
}
find_most_picked_champ(player_champ_data)

############################################################################
##                Which champion will be Banned the most?                 ##
############################################################################

find_most_banned_champ <- function(team_data) {
  # Combine all Bans and OpponentBans lists into a single list
  all_bans <- unlist(team_data$Bans)
  all_opponent_bans <- unlist(team_data$OpponentBans)
  combined_bans <- c(all_bans, all_opponent_bans)
  
  # Create a data frame with the combined bans
  bans_df <- data.frame(Ban = combined_bans)
  
  # Find the top 3 most frequent bans
  most_banned_champion <- bans_df %>%
    count(Ban) %>%
    arrange(desc(n)) %>%
    head(3)
  
  # Print the most frequent bans
  cat("The top 3 banned champions in the 2023 season were:\n")
  for (i in 1:nrow(most_banned_champion)) {
    row <- most_banned_champion[i, ]
    cat(paste0("\t", row$Ban, " (", row$n, ")\n"))
  }
  cat("\n")
}
find_most_banned_champ(team_data)

############################################################################
##            Which champion will have the most total Deaths?             ##
############################################################################

find_most_dead_champ <- function(player_data) {
  # Group the data by champion and calculate the total deaths for each champion
  champion_deaths <- player_data %>%
    group_by(Champion) %>%
    summarize(TotalDeaths = as.integer(sum(Deaths, na.rm = TRUE)))
  
  # Find top 3 champions with the most deaths
  most_deaths_champions <- champion_deaths %>%
    arrange(desc(TotalDeaths)) %>%
    head(3)
  
  # Print the most frequent bans
  cat("The top 3 champions with the most Deaths in the 2023 season were:\n")
  for (i in 1:nrow(most_deaths_champions)) {
    row <- most_deaths_champions[i, ]
    cat(paste0("\t", row$Champion, " (", row$TotalDeaths, ")\n"))
  }
  cat("\n")
}
find_most_dead_champ(player_champ_data)

############################################################################
##   Which champion will have the highest win rate? (>= 5 games played)   ##
############################################################################

find_highest_win_rate_champ <- function(player_data) {
  # Group the data by champion and calculate the mean win rate for each champion
  champion_winrates <- player_data %>%
    group_by(Champion) %>%
    summarize(AvgWinRate = mean(WinRate, na.rm = TRUE))
  
  # Find the top 10 champions with the highest average win rate
  highest_winrate_champions <- champion_winrates %>%
    arrange(desc(AvgWinRate)) %>%
    head(10)
  
  # Print the most frequent bans
  cat("The top 10 champions with the highest win rate in the 2023 season were:\n")
  for (i in 1:nrow(highest_winrate_champions)) {
    row <- highest_winrate_champions[i, ]
    cat(paste0("\t", row$Champion, " (", row$AvgWinRate, ")\n"))
  }
  cat("\n")
}
find_highest_win_rate_champ(player_champ_data)



############################################################################
##          Which player will play the most different Champions?          ##
############################################################################

find_most_champions_player <- function(player_data) {
  # Group the data by player and calculate the number of unique champions played by each player
  player_unique_champions <- player_data %>%
    group_by(Name, Team) %>%
    summarize(UniqueChampions = n_distinct(Champion))
  
  # Find the top 10 players with the most unique champions
  most_unique_champions_player <- player_unique_champions %>%
    arrange(desc(UniqueChampions)) %>%
    head(10)
  
  # Print the most frequent bans
  cat("The top 10 players with the most different played champions in the 2023 season were:\n")
  for (i in 1:nrow(most_unique_champions_player)) {
    row <- most_unique_champions_player[i, ]
    cat(paste0("\t", row$Name, " (", row$Team, "): ", row$UniqueChampions, "\n"))
  }
  cat("\n")
}
find_most_champions_player(player_champ_data)

############################################################################
##         Which player will get the most Kills in a single game?         ##
############################################################################

find_most_kills_player <- function(player_match_data) {
  # Filter the data to find players with the maximum number of kills
  kills_desc <- player_match_data %>% arrange(desc(Kills)) 
  players_with_most_kills <- unique(kills_desc) %>% head(10) 
  
  # Print the most frequent bans
  cat("The top 10 players with the most kills in a single game in the 2023 season were:\n")
  for (i in 1:nrow(players_with_most_kills)) {
    row <- players_with_most_kills[i, ]
    cat(paste0("\t", row$Name, " (", row$Team, "): ", row$Kills, " kills against ", row$Opponent, "\n"))
  }
  cat("\n")
}
find_most_kills_player(player_match_data)

############################################################################
##               Which player will have the highest KDA?                 ##
############################################################################

find_highest_kda_player <- function(player_champ_data) {
  # Filter the data to find players with the highest average season KDA
  players_with_highest_kda <- player_champ_data %>%
    group_by(Name, Team) %>%
    summarize(AvgKDA = mean(KDA, na.rm = TRUE)) %>%
    arrange(desc(AvgKDA)) %>%
    head(10)
  
  # Print the data
  cat("The top 10 players with the highest average KDA in the 2023 season were:\n")
  for (i in 1:nrow(players_with_highest_kda)) {
    row <- players_with_highest_kda[i, ]
    cat(paste0("\t", row$Name, " (", row$Team, "): ", round(row$AvgKDA, 2), "\n"))
  }
  cat("\n")
}
find_highest_kda_player(player_champ_data)

############################################################################
##                   How many Pentakills will there be?                   ##
##             Which player will get at least one Pentakill?              ##
############################################################################

find_pentakills_player <- function(pentakill_data) {
  # Define list of teams going to worlds
  selected_teams <- c("Gen G", "T1", "KT Rolster", "Dplus KIA", "JD Gaming", 
                      "Bilibili Gaming", "LNG Esports", "Weibo Gaming", 
                      "G2 Esports", "Fnatic", "MAD Lions", "NRG", "Cloud9", 
                      "Team Liquid", "PSG Talon", "CTBC Flying Oyster",
                      "GAM Esports", "Team Whales", "DetonatioN FocusMe", 
                      "LOUD", "Movistar R7")
  
  # Filter the data for pentakills in 2023 and by a member of a worlds team
  filtered_pentakill_data <- pentakill_data %>%
    filter(substr(Date, 1, 4) == "2023" & Team %in% selected_teams & !is.na(Player))
  
  # Count the entries for each player
  players_with_pentakills <- filtered_pentakill_data %>%
    group_by(Player) %>%
    summarize(NumPentakills = n()) %>%
    arrange(desc(NumPentakills))
    
  # Print the data
  cat("The players with pentakills in the 2023 season were:\n")
  for (i in 1:nrow(players_with_pentakills)) {
    row <- players_with_pentakills[i, ]
    cat(paste0("\t", row$Player, ": ", row$NumPentakills, "\n"))
  }
  cat("\n")
}
find_pentakills_player(pentakill_data)

############################################################################
##  Which team from a region with <= 2 seeds will advance the furthest?   ##
############################################################################

find_underdog <- function(team_data) {
  # Define list of underdog teams going to worlds
  selected_teams <- c("PSG Talon", "CTBC Flying Oyster", "GAM Esports", 
                      "Team Whales", "DetonatioN FocusMe", "LOUD", "Movistar R7")
  
  # Group the data by TeamName and calculate the sum of wins for each team
  team_wins <- team_data %>%
    filter(TeamName %in% selected_teams) %>%
    group_by(TeamName) %>%
    summarize(TotalWins = sum(ifelse(WinLoss == "Win", 1, 0)))
  
  # Find the teams with the most overall wins
  teams_with_most_wins <- team_wins %>%
    arrange(desc(TotalWins)) %>%
    head(10)
  
  # Print the data
  cat("The underdog teams win counts in the 2023 season were:\n")
  for (i in 1:nrow(teams_with_most_wins)) {
    row <- teams_with_most_wins[i, ]
    cat(paste0("\t", row$TeamName, ": ", row$TotalWins, " wins\n"))
  }
  cat("\n")
}
find_underdog(team_data)

############################################################################
##           Which team will win the shortest game (duration)?            ##
############################################################################

find_shortest_wins <- function(player_match_data) {
  # Define list of teams going to worlds
  selected_teams <- c("Gen G", "T1", "KT Rolster", "Dplus KIA", "JD Gaming", 
                      "Bilibili Gaming", "LNG Esports", "Weibo Gaming", 
                      "G2 Esports", "Fnatic", "MAD Lions", "NRG", "Cloud9", 
                      "Team Liquid", "PSG Talon", "CTBC Flying Oyster",
                      "GAM Esports", "Team Whales", "DetonatioN FocusMe", 
                      "LOUD", "Movistar R7")
  
  # Filter for games where the team won
  shortest_games <- player_match_data %>%
    filter(Team %in% selected_teams) %>%
    filter(WinLoss == "Win") %>%
    group_by(Team, Length) %>%
    summarize(min_length = min(as.numeric(gsub("[:]", "", Length)))) %>%
    arrange(min_length) %>%
    head(10)
  
  # Print the data
  cat("The teams with the shortest wins in the 2023 season were:\n")
  for (i in 1:nrow(shortest_games)) {
    row <- shortest_games[i, ]
    cat(paste0("\t", row$Team, ": ", row$Length, " minutes\n"))
  }
  cat("\n")
}
find_shortest_wins(player_match_data)

############################################################################
##                      Which team will win Worlds?                       ##
############################################################################



############################################################################
##           Which team will play the most different Champions?           ##
############################################################################

find_most_champions_team <- function(player_champ_data) {
  most_unique_champions <- player_champ_data %>%
    group_by(Team) %>%
    summarise(UniqueChampions = n_distinct(Champion)) %>%
    arrange(desc(UniqueChampions)) %>%
    head(10)
  
  cat("The teams that played the most champions in the 2023 season were:\n")
  for (i in 1:nrow(most_unique_champions)) {
    row <- most_unique_champions[i, ]
    cat(paste0("\t", row$Team, ": ", row$UniqueChampions, "\n"))
  }
  cat("\n")
}
find_most_champions_team(player_champ_data)

############################################################################
##  All together now  ##
############################################################################

base_path = "C:/Users/olivi/repos/data/"

player_champ_data <- read_json(paste0(base_path, "lol_worlds_player_champ_data.json"), simplifyVector = TRUE)
glimpse(player_champ_data)

player_match_data <- read_json(paste0(base_path, "lol_worlds_player_match_data.json"), simplifyVector = TRUE)
glimpse(player_match_data)

team_data <- read_json(paste0(base_path, "lol_worlds_team_data.json"), simplifyVector = TRUE)
glimpse(team_data)

pentakill_data <- read_json(paste0(base_path, "lol_worlds_pentakill_data.json"), simplifyVector = TRUE)
glimpse(pentakill_data)

lets_get_it <- function() {
  find_most_varied_champ(player_champ_data)
  find_most_picked_champ(player_champ_data)
  find_most_banned_champ(team_data)
  find_most_dead_champ(player_champ_data)
  find_highest_win_rate_champ(player_champ_data)
  find_most_champions_player(player_champ_data)
  find_most_kills_player(player_match_data)
  find_highest_kda_player(player_champ_data)
  find_pentakills_player(pentakill_data)
  find_underdog(team_data)
  find_shortest_wins(player_match_data)
  find_most_champions_team(player_champ_data)
}






