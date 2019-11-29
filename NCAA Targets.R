library(rvest)
library(magrittr)
library(data.table)
library(stringr)
setwd("/Users/briancreagh/XTB/NCAAF/Data/Production")

#Create list of relevant ESPN Game IDs
gameids <- c("401119305",
             "401119306",
             "401119307",
             "401119308",
             "401112513",
             "401117550",
             "401110859",
             "401110857",
             "401114162",
             "401114173",
             "401112120",
             "401112083",
             "401112263",
             "401112167",
             "401112113",
             "401112236",
             "401112155",
             "401112515",
             "401117907",
             "401117909",
             "401112128",
             "401117551",
             "401117906",
             "401117908",
             "401117910",
             "401119309",
             "401121977",
             "401112516",
             "401114315",
             "401112518",
             "401112519",
             "401112520",
             "401114182",
             "401114191",
             "401114206",
             "401114217",
             "401110861",
             "401112094",
             "401112210",
             "401112220",
             "401112256",
             "401110855",
             "401110856",
             "401114358",
             "401183793",
             "401121976",
             "401112514",
             "401121973",
             "401121974",
             "401117911",
             "401114317",
             "401114318",
             "401114313",
             "401110858",
             "401110862",
             "401114319",
             "401117553",
             "401114316",
             "401121975",
             "401114314",
             "401112517",
             "401110860",
             "401117549",
             "401117552")

playbyplayagg19 <- playbyplayagg2
playbyplayagg19 <- playbyplayagg19[0,]

#function to scrape plays from ESPN
for (i in gameids){
url <- read_html(paste("http://www.espn.com/college-football/playbyplay?gameId=",i, sep = ''))
  
plays <- url %>%
  html_nodes("h3 , .post-play") %>%
  html_text()
playbyplay <- as.data.frame(plays)
playbyplay$col2 <- playbyplay$plays
playbyplay$col2 <- shift(playbyplay$col2, n=1, fill = NA, type = "lag")
playbyplay <- subset(playbyplay, grepl('\\(',playbyplay$plays))
playbyplay$target <- ifelse( grepl('complete', playbyplay$plays) , as.character(playbyplay$plays),ifelse(grepl('pass from', playbyplay$plays) , as.character(playbyplay$plays),ifelse(grepl('intercepted', playbyplay$plays) , as.character(playbyplay$plays),'')))
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
playbyplayagg19 <- rbind(playbyplay, playbyplayagg19)
}
playbyplayagg192 <- unique(playbyplayagg19)
playbyplayagg192$field <- substr(playbyplayagg192$col2, regexpr(pattern = 'at',playbyplayagg192$col2)+3,99)
playbyplayagg192$teamfield <- substr(playbyplayagg192$field,1,regexpr(pattern = ' ',playbyplayagg192$field)-1)

#Clean data
playbyplaytest <- playbyplayagg192
playbyplaytest <- playbyplaytest[,c(9,1,2,10,11,5,4,6,8)]
playbyplaytest$Down <- substr(playbyplaytest$col2,0,3)
playbyplaytest$Distance <- substr(playbyplaytest$col2,regexpr(pattern = '& ',playbyplaytest$col2)+2,regexpr(pattern = '& ',playbyplaytest$col2)+3)
playbyplaytest$Distance <- str_trim(playbyplaytest$Distance)
playbyplaytest$time <- substr(playbyplaytest$plays,0,21)
playbyplaytest$Time_Left <- substr(playbyplaytest$time,1,regexpr(pattern=' -',playbyplaytest$time))
playbyplaytest$Time_Left <- sub('\\(','',playbyplaytest$Time_Left)
playbyplaytest$Time_Left <- str_trim(playbyplaytest$Time_Left)
playbyplaytest$Quarter <- ifelse(grepl('OT',playbyplaytest$time),'OT',sub('\\(','',playbyplaytest$time))
playbyplaytest$Quarter <- sub('\\)','',playbyplaytest$Quarter)
playbyplaytest$Quarter <- substr(playbyplaytest$Quarter,nchar(playbyplaytest$Quarter)-3,nchar(playbyplaytest$Quarter))
playbyplaytest$Rusher <- substr(playbyplaytest$RushAtt,1,regexpr(pattern = ' Yd Run',playbyplaytest$RushAtt)-3)
playbyplaytest$Rusher <- ifelse(grepl(' sacked',playbyplaytest$RushAtt),substr(playbyplaytest$RushAtt,1,regexpr(pattern = ' sacked',playbyplaytest$RushAtt)),
                                ifelse(grepl(' Yd Run',playbyplaytest$RushAtt),substr(playbyplaytest$RushAtt,1,regexpr(pattern = ' Yd Run',playbyplaytest$RushAtt)-3),
                                       substr(playbyplaytest$RushAtt,1,regexpr(pattern = ' run for',playbyplaytest$RushAtt))))
playbyplaytest$Rusher <- str_trim(playbyplaytest$Rusher)
playbyplaytest$PassYds <- ifelse(grepl(' pass from',playbyplaytest$plays),substr(playbyplaytest$plays,regexpr(pattern = ' Yd pass',playbyplaytest$plays)-2,regexpr(pattern = ' Yd pass',playbyplaytest$plays)),
                                 ifelse(grepl(' complete',playbyplaytest$plays)&grepl(' fumble',playbyplaytest$plays),
                                        substr(playbyplaytest$plays,regexpr(pattern = ' yd',playbyplaytest$plays)-2,regexpr(pattern = ' yd',playbyplaytest$plays)),
                                        ifelse(grepl(' a loss of',playbyplaytest$plays)&grepl(' complete',playbyplaytest$plays),paste0('-',substr(playbyplaytest$plays,regexpr(pattern = ' a loss of ',playbyplaytest$plays)+11
                                                                                              ,regexpr(pattern = ' a loss of ',playbyplaytest$plays)+13)),
                                               ifelse(grepl(' complete',playbyplaytest$plays),substr(playbyplaytest$plays,regexpr(pattern = ' for ',playbyplaytest$plays)+4
                                                                                                     ,regexpr(pattern = ' for ',playbyplaytest$plays)+6),''))))
playbyplaytest$PassYds <- sub(pattern = ' y','', playbyplaytest$PassYds)
playbyplaytest$PassYds <- sub(pattern = 'no','0',playbyplaytest$PassYds)
playbyplaytest$PassYds <- str_trim(playbyplaytest$PassYds)
playbyplaytest$RecYds <- ifelse(grepl(' pass from',playbyplaytest$plays),substr(playbyplaytest$plays,regexpr(pattern = ' Yd pass',playbyplaytest$plays)-2,regexpr(pattern = ' Yd pass',playbyplaytest$plays)),
                                ifelse(grepl(' complete',playbyplaytest$plays)&grepl(' fumble',playbyplaytest$plays),
                                       substr(playbyplaytest$plays,regexpr(pattern = ' yd',playbyplaytest$plays)-2,regexpr(pattern = ' yd',playbyplaytest$plays)),
                                       ifelse(grepl(' a loss of',playbyplaytest$plays)&grepl(' complete',playbyplaytest$plays),paste0('-',substr(playbyplaytest$plays,regexpr(pattern = ' a loss of ',playbyplaytest$plays)+11
                                              ,regexpr(pattern = ' a loss of ',playbyplaytest$plays)+13)),
                                              ifelse(grepl(' complete',playbyplaytest$plays),substr(playbyplaytest$plays,regexpr(pattern = ' for ',playbyplaytest$plays)+4
                                                                                                    ,regexpr(pattern = ' for ',playbyplaytest$plays)+6),''))))
playbyplaytest$RecYds <- sub(pattern = ' y','', playbyplaytest$RecYds)
playbyplaytest$RecYds <- sub(pattern = 'no','0',playbyplaytest$RecYds)
playbyplaytest$RecYds <- str_trim(playbyplaytest$RecYds)
playbyplaytest$RushYds <-  ifelse(grepl('NO PLAY',playbyplaytest$RushAtt),0,
                            ifelse(grepl(' sacked',playbyplaytest$RushAtt)&(grepl(' for 0 yards to',playbyplaytest$RushAtt)|grepl(' for 0 yds by',playbyplaytest$RushAtt)),0,
                                 ifelse(grepl(' sacked',playbyplaytest$RushAtt)&grepl(' and ',playbyplaytest$RushAtt),
                                 paste0('-',substr(playbyplaytest$RushAtt,regexpr(pattern = ' a loss of',playbyplaytest$RushAtt)+11,regexpr(pattern = ' a loss of',playbyplaytest$RushAtt)+13)),
                                 ifelse(grepl(' sacked',playbyplaytest$RushAtt)&(grepl('safety',playbyplaytest$RushAtt)|grepl('Safety',playbyplaytest$RushAtt)),0,
                                ifelse(grepl(' sacked',playbyplaytest$RushAtt),
                                paste0('-',substr(playbyplaytest$RushAtt,regexpr(pattern = ' loss of',playbyplaytest$RushAtt)+9,regexpr(pattern = ' loss of',playbyplaytest$RushAtt)+11)),
                                ifelse(grepl(' Yd Run',playbyplaytest$RushAtt),substr(playbyplaytest$RushAtt,regexpr(pattern = ' Yd Run',playbyplaytest$RushAtt)-2,regexpr(pattern = ' Yd Run',playbyplaytest$RushAtt)),
                                ifelse(grepl(' a loss of',playbyplaytest$RushAtt),paste0('-',substr(playbyplaytest$RushAtt,regexpr(pattern = ' a loss of',playbyplaytest$RushAtt)+11,regexpr(pattern = ' a loss of',playbyplaytest$RushAtt)+13)),
                                ifelse(grepl(' run for no gain',playbyplaytest$RushAtt),0,
                                ifelse(grepl(' run for',playbyplaytest$RushAtt),substr(playbyplaytest$RushAtt,regexpr(pattern = ' run for ',playbyplaytest$RushAtt)+9,regexpr(pattern = ' run for ',playbyplaytest$RushAtt)+11),'')))))))))
playbyplaytest$RushYds <- sub(' y','',playbyplaytest$RushYds)
playbyplaytest$RushYds <- str_trim(playbyplaytest$RushYds)
playbyplaytest$RushYds <- as.numeric(playbyplaytest$RushYds)
playbyplaytest$PassTD <- ifelse(grepl('intercept',playbyplaytest$plays),0,ifelse(
  (grepl(' TD',playbyplaytest$plays)|grepl('pass from',playbyplaytest$plays))&playbyplaytest$QBtarget!='',1,0))
playbyplaytest$INT <- ifelse(grepl('intercepted',playbyplaytest$plays)&!grepl('intercepted )',playbyplaytest$plays),1,0)
playbyplaytest$RushTD <- ifelse((grepl(' TD',playbyplaytest$plays)|grepl('Yd Run',playbyplaytest$plays))&playbyplaytest$Rusher!='',1,0)
playbyplaytest$RecTD <- playbyplaytest$PassTD
playbyplaytest$Fumble <- ifelse(grepl('fumble',playbyplaytest$plays),1,0)
playbyplaytest$PassAtt <- ifelse(playbyplaytest$QBtarget!='',1,0)
playbyplaytest$PassComp <- ifelse(grepl(' complete',playbyplaytest$plays)|grepl('pass from',playbyplaytest$plays),1,0)
playbyplaytest$Rush <- ifelse(playbyplaytest$Rusher!='',1,0)
playbyplaytest$Target <- ifelse(playbyplaytest$WRtarget!='',1,0)
playbyplaytest$FirstDown <- ifelse(grepl('1ST down',playbyplaytest$plays),1,0)
playbyplaytest$Season <- "2019"

write.csv(playbyplaytest, paste0("playbyplaytest",Sys.Date(),".csv"))



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
  write.csv(player_schedule19, paste0("player_schedule",Sys.Date(),".csv"))


