library(rvest)
library(dplyr)
library(stringr)
library(lubridate)
week = 13
url = "https://www.espn.com/college-football/schedule/_/week/14"
getGameIds <- function(week){
    #Build empty output frame
  output_df <- data.frame(
    Date = NA,
    Away = NA,
    Home = NA
  )[0,]
  
  #Create html connection
  connection <- paste("https://www.espn.com/college-football/schedule/_/week/", week, sep = "") %>%
    html_session
  
  #Pull game links for complete games
  links <- html_nodes(connection, "[name='&lpos=college-football:schedule:score']") %>%
    html_attr("href")
  
  #Pull game links for incomplete games if present
  links <- c(links, html_nodes(connection, "[name='&lpos=college-football:schedule:time']") %>%
    html_attr("href"))
  
  #Format the gameIDs
  gameIDs <- str_remove(links, "/college-football/game/_/gameId/")
  
  #Get dates and format them
  dates <- html_nodes(connection, "[class='table-caption']") %>%
    html_text %>%
    paste("2019")
  dates <- format(mdy(dates), "%m/%d/%y")
  
  #Get games table
  games <- html_nodes(connection, "[class='schedule has-team-logos align-left']") %>%
    html_table(fill = TRUE)
  
  #Compile the output frame
  #This works by the games table from above, it is one df per gameday
  for(i in 1:length(games)){
    day_df <- data.frame(games[i])
    colnames(day_df) = c("Away", "Home", "Result", "Passing.Leader", "Rushing.Leader", "Receiving.Leader")

    day_df <- day_df[,c("Away", "Home")]
    
    #Add date for games by day
    day_df$Date <- dates[i]

    #Pull team abbreviations
    day_df$Away <- word(day_df$Away, -1)
    day_df$Home <- word(day_df$Home, -1)
      
    output_df <- rbind(output_df, day_df)
  }
  
  output_df$Links <- links
  output_df <- cbind(output_df, gameIDs)
  output_df$Week <- week
  output_df$Season <- "2019"
  return(output_df)
}
