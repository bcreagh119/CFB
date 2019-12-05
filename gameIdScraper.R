library(rvest)
library(dplyr)
library(stringr)
getGameIds <- function(url){
  #Scrapes all URLs containing "gameId" in the following URL
  connection <- url %>%
    html_session
  links <- html_nodes(connection, "[name='&lpos=college-football:schedule:score']") %>%
    html_attr("href")
  links <- c(links, html_nodes(connection, "[name='&lpos=college-football:schedule:time']") %>%
    html_attr("href"))
  return(str_remove(links, "/college-football/game/_/gameId/"))
}