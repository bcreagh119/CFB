library(rvest)
library(dplyr)
library(stringr)
getGameIds <- function(){
  #Scrapes all URLs containing "gameId" in the following URL
  connection <- "https://www.espn.com/college-football/schedule" %>%
    html_session
  links <- html_nodes(connection, "[name='&lpos=college-football:schedule:score']") %>%
    html_attr("href")
  return(str_remove(links, "/college-football/game/_/gameId/"))
}