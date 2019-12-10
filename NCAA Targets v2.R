library(rvest)
library(magrittr)
library(data.table)
library(stringr)

source('gameIdScraper.R')

#Create list of relevant ESPN Game IDs
gameids <- getGameIds(13)

playbyplayagg <- data.frame("plays"=NA,
                              "downText"=NA,
                              "target"=NA,
                              "WRtarget"=NA,
                              "time"=NA,
                              "QBtarget"=NA,
                              "Rush"=NA,
                              "RushAtt"=NA,
                              "gameID"=NA,
                              "field"=NA,
                              "teamfield"=NA)[0,]


#function to scrape plays from ESPN
for (i in gameids$gameIDs){
connection <- read_html(paste("http://www.espn.com/college-football/playbyplay?gameId=",i, sep = ''))

#Get plays  
plays <- connection %>%
  html_nodes("h3, .post-play") %>%
  html_text()

#Convert to dataframe, trim whitespace 
plays <- trimws(plays)
playbyplay <- as.data.frame(plays)

#Duplicate plays into downs, shift downs down one, join the rows
playbyplay$downText <- playbyplay$plays
playbyplay$downText <- shift(playbyplay$downText, n=1, fill = NA, type = "lag")
playbyplay <- subset(playbyplay, grepl('\\(',playbyplay$plays))

#Find play target
playbyplay$target <- ifelse( grepl('complete', playbyplay$plays) , as.character(playbyplay$plays),ifelse(grepl('pass from', playbyplay$plays) , as.character(playbyplay$plays),ifelse(grepl('intercepted', playbyplay$plays) , as.character(playbyplay$plays),'')))

#Find wide receiver target
playbyplay$WRtarget <- ifelse(grepl('pass from', playbyplay$target), 
                              substr(playbyplay$target,regexpr(pattern = '\\)',playbyplay$target)+2,regexpr(pattern = 'pass from',playbyplay$target)-7 ), 
                              substr(playbyplay$target,regexpr(pattern = 'complete to',playbyplay$target) , 99) )

playbyplay$WRtarget <- sub("complete to ","",playbyplay$WRtarget)


playbyplay$WRtarget <- ifelse(grepl(pattern = 'Jr.', playbyplay$WRtarget),
                                  sub("^(\\S*\\s+\\S+\\s+\\S+).*", "\\1", playbyplay$WRtarget),
                              ifelse(grepl(pattern = 'David Sills V', 
                                           playbyplay$WRtarget),
                                     sub("^(\\S*\\s+\\S+\\s+\\S+).*", "\\1", 
                                         playbyplay$WRtarget),
                                     ifelse(grepl(pattern = ' IV', 
                                                  playbyplay$WRtarget),
                                            sub("^(\\S*\\s+\\S+\\s+\\S+).*", "\\1", 
                                                playbyplay$WRtarget),
                                        ifelse(grepl(pattern = ' St. ', 
                                                         playbyplay$WRtarget),
                                                   sub("^(\\S*\\s+\\S+\\s+\\S+).*", "\\1", 
                                                       playbyplay$WRtarget),
                                  ifelse(grepl(pattern = ' III', 
                                               playbyplay$WRtarget),
                                         sub("^(\\S*\\s+\\S+\\s+\\S+).*", "\\1", 
                                             playbyplay$WRtarget), 
                                         ifelse(grepl(pattern = ' II', 
                                                      playbyplay$WRtarget),
                                                sub("^(\\S*\\s+\\S+\\s+\\S+).*", "\\1", playbyplay$WRtarget), 
                                                sub("^(\\S*\\s+\\S+).*", "\\1", playbyplay$WRtarget)))))))


playbyplay$WRtarget <- sub(",","",playbyplay$WRtarget)


playbyplay$time <- substr(playbyplay$plays,0,21)


playbyplay$QBtarget <- ifelse(grepl('pass from', playbyplay$plays), substr(playbyplay$target,regexpr(pattern = 'pass from',playbyplay$target)+10,regexpr(pattern = ' \\(',playbyplay$target)-1) ,substr(playbyplay$target,regexpr(pattern = ') ',playbyplay$target)+2, regexpr(pattern = 'pass', playbyplay$target) - 2))


playbyplay$Rush <- ifelse( grepl('run for', playbyplay$plays),as.character(playbyplay$plays),ifelse(grepl(' Yd Run', playbyplay$plays) , as.character(playbyplay$plays),ifelse(grepl('sacked', playbyplay$plays) , as.character(playbyplay$plays),'')))


playbyplay$RushAtt <- substr(playbyplay$Rush, regexpr(pattern = ') ',playbyplay$Rush)+2, 350)


playbyplay$gameID <- i


playbyplay <- subset(playbyplay, grepl('\\(',playbyplay$plays))

#Assign a unique play ID, as ESPN game idea plus a number
playbyplay$playID <- paste(i, 1:length(playbyplay$plays), sep = '')

#Add the games plays to the aggregate dataframe
playbyplayagg <- rbind(playbyplay, playbyplayagg)


}

playbyplayagg$field <- substr(playbyplayagg$downText, regexpr(pattern = 'at',playbyplayagg$downText)+3,99)


playbyplayagg$teamfield <- substr(playbyplayagg$field,1,regexpr(pattern = ' ',playbyplayagg$field)-1)

#Clean data
playbyplayout <- playbyplayagg


playbyplayout <- playbyplayout[,c(9,1,2,10,11,12,5,4,6,8)]


playbyplayout$Down <- substr(playbyplayout$downText,0,3)


playbyplayout$Distance <- substr(playbyplayout$downText,regexpr(pattern = '& ',playbyplayout$downText)+2,regexpr(pattern = '& ',playbyplayout$downText)+3)


playbyplayout$Distance <- str_trim(playbyplayout$Distance)


playbyplayout$time <- substr(playbyplayout$plays,0,21)


playbyplayout$Time_Left <- substr(playbyplayout$time,1,regexpr(pattern=' -',playbyplayout$time))


playbyplayout$Time_Left <- sub('\\(','',playbyplayout$Time_Left)


playbyplayout$Time_Left <- str_trim(playbyplayout$Time_Left)


playbyplayout$Quarter <- ifelse(grepl('OT',playbyplayout$time),'OT',sub('\\(','',playbyplayout$time))


playbyplayout$Quarter <- sub('\\)','',playbyplayout$Quarter)


playbyplayout$Quarter <- substr(playbyplayout$Quarter,nchar(playbyplayout$Quarter)-3,nchar(playbyplayout$Quarter))


playbyplayout$Rusher <- substr(playbyplayout$RushAtt,1,regexpr(pattern = ' Yd Run',playbyplayout$RushAtt)-3)


playbyplayout$Rusher <- ifelse(grepl(' sacked',playbyplayout$RushAtt),substr(playbyplayout$RushAtt,1,regexpr(pattern = ' sacked',playbyplayout$RushAtt)),
                                ifelse(grepl(' Yd Run',playbyplayout$RushAtt),substr(playbyplayout$RushAtt,1,regexpr(pattern = ' Yd Run',playbyplayout$RushAtt)-3),
                                       substr(playbyplayout$RushAtt,1,regexpr(pattern = ' run for',playbyplayout$RushAtt))))


playbyplayout$Rusher <- str_trim(playbyplayout$Rusher)


playbyplayout$PassYds <- ifelse(grepl(' pass from',playbyplayout$plays),substr(playbyplayout$plays,regexpr(pattern = ' Yd pass',playbyplayout$plays)-2,regexpr(pattern = ' Yd pass',playbyplayout$plays)),
                                 ifelse(grepl(' complete',playbyplayout$plays)&grepl(' fumble',playbyplayout$plays),
                                        substr(playbyplayout$plays,regexpr(pattern = ' yd',playbyplayout$plays)-2,regexpr(pattern = ' yd',playbyplayout$plays)),
                                        ifelse(grepl(' a loss of',playbyplayout$plays)&grepl(' complete',playbyplayout$plays),paste0('-',substr(playbyplayout$plays,regexpr(pattern = ' a loss of ',playbyplayout$plays)+11
                                                                                              ,regexpr(pattern = ' a loss of ',playbyplayout$plays)+13)),
                                               ifelse(grepl(' complete',playbyplayout$plays),substr(playbyplayout$plays,regexpr(pattern = ' for ',playbyplayout$plays)+4
                                                                                                     ,regexpr(pattern = ' for ',playbyplayout$plays)+6),''))))


playbyplayout$PassYds <- sub(pattern = ' y','', playbyplayout$PassYds)


playbyplayout$PassYds <- sub(pattern = 'no','0',playbyplayout$PassYds)


playbyplayout$PassYds <- str_trim(playbyplayout$PassYds)


playbyplayout$RecYds <- ifelse(grepl(' pass from',playbyplayout$plays),substr(playbyplayout$plays,regexpr(pattern = ' Yd pass',playbyplayout$plays)-2,regexpr(pattern = ' Yd pass',playbyplayout$plays)),
                                ifelse(grepl(' complete',playbyplayout$plays)&grepl(' fumble',playbyplayout$plays),
                                       substr(playbyplayout$plays,regexpr(pattern = ' yd',playbyplayout$plays)-2,regexpr(pattern = ' yd',playbyplayout$plays)),
                                       ifelse(grepl(' a loss of',playbyplayout$plays)&grepl(' complete',playbyplayout$plays),paste0('-',substr(playbyplayout$plays,regexpr(pattern = ' a loss of ',playbyplayout$plays)+11
                                              ,regexpr(pattern = ' a loss of ',playbyplayout$plays)+13)),
                                              ifelse(grepl(' complete',playbyplayout$plays),substr(playbyplayout$plays,regexpr(pattern = ' for ',playbyplayout$plays)+4
                                                                                                    ,regexpr(pattern = ' for ',playbyplayout$plays)+6),''))))


playbyplayout$RecYds <- sub(pattern = ' y','', playbyplayout$RecYds)


playbyplayout$RecYds <- sub(pattern = 'no','0',playbyplayout$RecYds)


playbyplayout$RecYds <- str_trim(playbyplayout$RecYds)


playbyplayout$RushYds <-  ifelse(grepl('NO PLAY',playbyplayout$RushAtt),0,
                            ifelse(grepl(' sacked',playbyplayout$RushAtt)&(grepl(' for 0 yards to',playbyplayout$RushAtt)|grepl(' for 0 yds by',playbyplayout$RushAtt)),0,
                                 ifelse(grepl(' sacked',playbyplayout$RushAtt)&grepl(' and ',playbyplayout$RushAtt),
                                 paste0('-',substr(playbyplayout$RushAtt,regexpr(pattern = ' a loss of',playbyplayout$RushAtt)+11,regexpr(pattern = ' a loss of',playbyplayout$RushAtt)+13)),
                                 ifelse(grepl(' sacked',playbyplayout$RushAtt)&(grepl('safety',playbyplayout$RushAtt)|grepl('Safety',playbyplayout$RushAtt)),0,
                                ifelse(grepl(' sacked',playbyplayout$RushAtt),
                                paste0('-',substr(playbyplayout$RushAtt,regexpr(pattern = ' loss of',playbyplayout$RushAtt)+9,regexpr(pattern = ' loss of',playbyplayout$RushAtt)+11)),
                                ifelse(grepl(' Yd Run',playbyplayout$RushAtt),substr(playbyplayout$RushAtt,regexpr(pattern = ' Yd Run',playbyplayout$RushAtt)-2,regexpr(pattern = ' Yd Run',playbyplayout$RushAtt)),
                                ifelse(grepl(' a loss of',playbyplayout$RushAtt),paste0('-',substr(playbyplayout$RushAtt,regexpr(pattern = ' a loss of',playbyplayout$RushAtt)+11,regexpr(pattern = ' a loss of',playbyplayout$RushAtt)+13)),
                                ifelse(grepl(' run for no gain',playbyplayout$RushAtt),0,
                                ifelse(grepl(' run for',playbyplayout$RushAtt),substr(playbyplayout$RushAtt,regexpr(pattern = ' run for ',playbyplayout$RushAtt)+9,regexpr(pattern = ' run for ',playbyplayout$RushAtt)+11),'')))))))))


playbyplayout$RushYds <- sub(' y','',playbyplayout$RushYds)


playbyplayout$RushYds <- str_trim(playbyplayout$RushYds)


playbyplayout$RushYds <- as.numeric(playbyplayout$RushYds)


playbyplayout$PassTD <- ifelse(grepl('intercept',playbyplayout$plays),0,ifelse(
  (grepl(' TD',playbyplayout$plays)|grepl('pass from',playbyplayout$plays))&playbyplayout$QBtarget!='',1,0))


playbyplayout$INT <- ifelse(grepl('intercepted',playbyplayout$plays)&!grepl('intercepted )',playbyplayout$plays),1,0)


playbyplayout$RushTD <- ifelse((grepl(' TD',playbyplayout$plays)|grepl('Yd Run',playbyplayout$plays))&playbyplayout$Rusher!='',1,0)


playbyplayout$RecTD <- playbyplayout$PassTD


playbyplayout$Fumble <- ifelse(grepl('fumble',playbyplayout$plays),1,0)


playbyplayout$PassAtt <- ifelse(playbyplayout$QBtarget!='',1,0)


playbyplayout$PassComp <- ifelse(grepl(' complete',playbyplayout$plays)|grepl('pass from',playbyplayout$plays),1,0)


playbyplayout$Rush <- ifelse(playbyplayout$Rusher!='',1,0)


playbyplayout$Target <- ifelse(playbyplayout$WRtarget!='',1,0)


playbyplayout$FirstDown <- ifelse(grepl('1ST down',playbyplayout$plays),1,0)

#Set season
playbyplayout$Season <- "2019"

#Write play by play
write.csv(playbyplayout, paste0("../data/playbyplay",Sys.Date(),"_", format(Sys.time(), "%h%m%s"), ".csv"), row.names = FALSE)



#build player_schedule table
  rosters <- read.csv("Rosters - Rosters.csv")
  schedule <- read.csv(file = "NCAA Targets - Copy of Game IDs.csv", sep = ',')
  rosters19 <- subset(rosters,rosters$Year=='2019')
  schedule19 <- subset(schedule,schedule$Season=='2019')
  away19 <- merge(x=rosters19, y=schedule19, by.x = "Team",by.y = "Away")
  away19 <- away19[,c(1,4:12,17)]
  home19 <- merge(x=rosters19, y=schedule19, by.x = "Team",by.y = "Home")
  home19 <- home19[,c(1,4:12,17)]
  player_schedule19 <- rbind(away19, home19)
  write.csv(player_schedule19, paste0("../data/player_schedule",Sys.Date(),"_", format(Sys.time(), "%h%m%s"), ".csv"), row.names = FALSE)


  
#Dump this on the AWS rdb instance
library(RMySQL)
aws = dbConnect(MySQL(), user='admin', password='expandtheboxscore', dbname='new_xtb_cfb', 
                 host='database-1.chaj45z3rtec.us-east-2.rds.amazonaws.com')

#Write tables (This needs work, it will add duplicates)
dbWriteTable(aws, name = "playbyplay", value = playbyplayout, row.names = FALSE, overwrite = FALSE, append = TRUE)
#Coming soon...
#dbWriteTable(aws, name = "rosters", value = player_schedule19, row.names = FALSE, overwrite = FALSE, append = TRUE)
dbWriteTable(aws, name = "games", value = gameids, row.names = FALSE, overwrite = FALSE, append = TRUE)


#Close connections
lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)

  