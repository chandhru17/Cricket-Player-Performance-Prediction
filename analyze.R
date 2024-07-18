#An R project by Tarun S 

color_picker <- function(opposing_team){
  team_color <- 'black'
  team_color <- switch(
    opposing_team,
    'Chennai Super Kings' = 'yellow',
    'Delhi Capitals' = 'darkblue',
    'Gujarat Titans' = 'navyblue',
    'Kolkata Knight Riders' = 'purple',
    'Lucknow Super Giants' = 'cyan',
    'Mumbai Indians' = 'lightblue',
    'Punjab Kings' = 'darkred',
    'Rajasthan Royals' = 'pink',
    'Royal Challengers Banglore' = 'red',
    'Sunrisers Hyderabad' = 'orange'
  )
  return(team_color)
}

perf_predict_batsman<-function(player_name,opposing_team){
  data <- read.csv("deliveries.csv")
  # Specify the player name and opposing team
  data <- read.csv("deliveries.csv")
  
  # Filter the data for the specified player and opposing team
  filtered_data <- data[data$batsman == player_name & data$bowling_team == opposing_team, ]
  
  # Create a data frame to store runs and balls played for each match
  player_stats <- data.frame(Match_ID = unique(filtered_data$match_id),
                             Runs = integer(length(unique(filtered_data$match_id))),
                             Balls_Played = integer(length(unique(filtered_data$match_id))))
  
  # Iterate over each match and calculate runs and balls played
  for (i in 1:length(player_stats$Match_ID)) {
    match_id <- player_stats$Match_ID[i]
    match_data <- filtered_data[filtered_data$match_id == match_id, ]
    player_stats$Runs[i] <- sum(match_data$total_runs)
    player_stats$Balls_Played[i] <- length(match_data$ball)
  }
  
  color <- color_picker(team)
  
  # Plotting the data
  plot(player_stats$Balls_Played, player_stats$Runs, 
       xlab = "Balls Played", ylab = "Score", 
       main = paste("Player Performance -", player_name, "vs", opposing_team),
       pch = 16, col = color)
  
  # Adding a trend line
  trend_line <- lm(Runs ~ Balls_Played, data = player_stats)
  abline(trend_line, col = "red")
  
  # Adding a legend
  legend("bottomright", legend = c("Data Points", "Trend Line"), 
         col = c(color, "red"), pch = c(16, NA), lty = c(NA, 1))
  
  
  
  
  plot_file <- paste(player_name,'.pdf',sep='')
  pdf(file = plot_file)
  dev.off()
  
}


perf_predict_bowler <- function(player_name,opposing_team){
  data <- read.csv('deliveries.csv')
  
  filtered_data <- data[data$bowler == player_name & data$batting_team == opposing_team,]
  
  player_stats <- data.frame(Match_ID = unique(filtered_data$match_id),
                             Runs = integer(length(unique(filtered_data$match_id))),
                             Balls_Played = integer(length(unique(filtered_data$match_id))))
  
  # Iterate over each match and calculate runs and balls played
  for (i in 1:length(player_stats$Match_ID)) {
    match_id <- player_stats$Match_ID[i]
    match_data <- filtered_data[filtered_data$match_id == match_id, ]
    player_stats$Runs[i] <- sum(match_data$total_runs)
    player_stats$Balls_Played[i] <- length(match_data$ball)
  }
  
  color <- color_picker(team)
  
  # Plotting the data
  plot(player_stats$Balls_Played, player_stats$Runs, 
       xlab = "Balls Bowled", ylab = "Score", 
       main = paste("Player Performance -", player_name, "vs", opposing_team),
       pch = 16, col = color)
  
  # Adding a trend line
  trend_line <- lm(Runs ~ Balls_Played, data = player_stats)
  abline(trend_line, col = "red")
  
  # Adding a legend
  legend("bottomright", legend = c("Data Points", "Trend Line"), 
         col = c(color, "red"), pch = c(16, NA), lty = c(NA, 1))
  
  plot_file <- paste(player_name,'.pdf',sep='')
  pdf(file = plot_file)
  dev.off()
  
}


batsman_stat<-function(name){
  
  predictions <- data.frame(Player = character(), Score = numeric(), Balls = numeric(), stringsAsFactors = FALSE)
  
  player_data <- subset(source, batsman == name)
  
  player_stats <- aggregate(total_runs ~ match_id, player_data, FUN = function(x) c(no_of_balls = length(x) - sum(x == 0), total_runs = sum(x)))
  
  player_stats <- as.data.frame(player_stats[, -1])
  
  regression_model <- lm(total_runs ~ no_of_balls, data = player_stats)
  
  balls_to_predict <- 24
  
  new_data <- data.frame(no_of_balls = balls_to_predict)
  
  predicted_score <- predict(regression_model, newdata = new_data)
  
  predictions <- rbind(predictions, data.frame(Player = name, Score = predicted_score, Balls = balls_to_predict))
  
  popular_players <- c('JC Buttler','S Dhawan','V Kohli')
  
  colors <- c('pink','darkred','red')
  
  for (player in popular_players) {
    if(player == name){
      already_present <- TRUE
      next
    }
    player_data <- subset(source, batsman == player)
    
    player_stats <- aggregate(total_runs ~ match_id, player_data, FUN = function(x) c(no_of_balls = length(x) - sum(x == 0), total_runs = sum(x)))
    
    player_stats <- as.data.frame(player_stats[, -1])
    
    regression_model <- lm(total_runs ~ no_of_balls, data = player_stats)
    
    balls_to_predict <- 30
    
    new_data <- data.frame(no_of_balls = balls_to_predict)
    
    predicted_score <- predict(regression_model, newdata = new_data)
    
    predictions <- rbind(predictions, data.frame(Player = player, Score = predicted_score, Balls = balls_to_predict))
  }
  
  colors <- c(color_picker(team),colors)
  
  barplot(predictions$Score, names.arg = predictions$Player,
          xlab = "Player Name", ylab = "Score",
          main = "Player Name vs Score", col = colors)
}

bowler_stat<-function(name){
  
  predictions <- data.frame(Player = character(), Runs = numeric(), Balls = numeric(), stringsAsFactors = FALSE)
  
  bowler_name <- name
  
  bowler_data <- subset(source, bowler == bowler_name)
  
  bowler_stats <- aggregate(total_runs ~ match_id, bowler_data, FUN = function(x) c(no_of_balls = length(x), total_runs = sum(x)))
  
  bowler_stats <- as.data.frame(bowler_stats[, -1])
  
  regression_model <- lm(total_runs ~ no_of_balls, data = bowler_stats)
  
  balls_to_predict <- 24
  
  new_data <- data.frame(no_of_balls = balls_to_predict)
  
  predicted_runs <- predict(regression_model, newdata = new_data)
  
  predictions <- rbind(predictions, data.frame(Player = name, Runs = predicted_runs, Balls = balls_to_predict))
  
  popular_players <- c('Mohammed Siraj','R Ashwin','Mohammed Shami','JJ Bumrah')
  
  colors <- c('red','pink','navyblue','lightblue')
  
  for (player in popular_players) {
    if(player == name){
      next
    }
    bowler_data <- subset(source, bowler == player)
    
    bowler_stats <- aggregate(total_runs ~ match_id, bowler_data, FUN = function(x) c(no_of_balls = length(x), total_runs = sum(x)))
    
    bowler_stats <- as.data.frame(bowler_stats[, -1])
    
    regression_model <- lm(total_runs ~ no_of_balls, data = bowler_stats)
    
    balls_to_predict <- 24
    
    new_data <- data.frame(no_of_balls = balls_to_predict)
    
    predicted_runs <- predict(regression_model, newdata = new_data)
    
    predictions <- rbind(predictions, data.frame(Player = player, Runs = predicted_runs, Balls = balls_to_predict))
  }
  
  colors <- c(color_picker(team),colors)
  
  barplot(predictions$Runs, names.arg = predictions$Player,
          xlab = "Player Name", ylab = "Runs",
          main = "Predicted runs to be conceded every 4 overs", col = colors)
  
  
}

#Driver Code
#Loading necessary files

source<-read.csv("deliveries.csv")
squad<-read.csv("squad.csv")

#Initializing variables

is_present<-FALSE
player_type<-"NONE"
list_name<-""
team<-"NONE"
row<-1
invalid <- FALSE
pp <- 0

args <- commandArgs(trailingOnly = TRUE)

args <- as.list(args)

#Prompting user input

if(args[3] == '--'){
  player_vector <- c(args[4])
  player <- paste(args[4])
}else{
  player_vector <- c(args[4])
  player <- paste(args[3],args[4],sep = ' ')
}


#Searching for player in current squad

for(i in squad$Player.s.List){
  if(is_present){
    break
  }
  if(grepl(tolower(player),tolower(i))){
    player_type<-squad[row,4]
    is_present<-TRUE
    team<-squad[row,8]
  }
  row<-row+1
}


#Determining name as in deliveries.csv

if(player_type=="WICKETKEEPER" || player_type=="BATSMAN"){
  for(i in source$batsman){
    if(grepl(tolower(player_vector[1]),tolower(i))){
      list_name<-i
      break
    }
  }
}else{
  for(i in source$bowler){
    if(grepl(tolower(player_vector[1]),tolower(i))){
      list_name<-i
      break
    }
  }
}

#print(paste(list_name,player_type,team))

if(length(list_name) == 0){
  invalid <- TRUE
}


if(args[1] == 2){
  pp <- as.numeric(args[7])
  team_name <- c(args[5],args[6])
  team_name <- paste(team_name,collapse = ' ')
  if(args[2] == 1){
    style <- args[8]
  }
}else if(args[1] == 3){
  pp <- as.numeric(args[8])
  team_name <- c(args[5],args[6],args[7])
  team_name <- paste(team_name,collapse = ' ')
  if(args[2] == 1){
    style <- 9
  }
}

if(args[2] == 1){
  if(pp == 1){
    if(style == 'Batting'){
      perf_predict_batsman(list_name,team_name)
    }else{
      perf_predict_bowler(list_name,team_name)
    }
    }
  else if(pp == 0){
    if(style == 'Batting'){
      batsman_stat(list_name)
    }else{
      bowler_stat(list_name)
    }
  }
}else{
  
  if(pp == 1){
    
    
    if(player_type == 'BATSMAN' || player_type == 'WICKETKEEPER'){
      perf_predict_batsman(list_name,team_name)
    }else if(player_type == 'BOWLER'){
      perf_predict_bowler(list_name,team_name)
    }
    
  }else{
    #Performing regression and plotting stats among other famous players
    
    if(player_type == "BATSMAN" || player_type == "WICKETKEEPER"){
      
      batsman_stat(list_name)
    }else if(player_type == "BOWLER"){
      bowler_stat(list_name)
    }
  }
}







