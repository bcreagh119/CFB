library(RMySQL)
pullPlayByPlayFromAWS <- function(aws = 'dbconnect to whatever sql server'){
  query = dbSendQuery(aws, "select * from new_xtb_cfb.playbyplay")
  playbyplay2019 = fetch(query, n=-1)
  
  playbyplay2019["MyUnknownColumn"] = ""
  playbyplay2019["MyUnknownColumn_[0]"] = ""
  playbyplay2019 <- playbyplay2019[c("MyUnknownColumn", "MyUnknownColumn_[0]", "gameID", "plays", "downText", "field", "teamfield", "time", "WRtarget", "QBtarget",
                                     "RushAtt", "Down", "Distance", "Time_Left", "Quarter", "Rusher", "PassYds", "RecYds", "RushYds", "PassTd", "INT", "RushTD",
                                     "RecTD", "Fumble", "PassAtt", "PassComp", "Rush", "Target", "FirstDown", "Season")]
  
  colnames(playbyplay2019) <- c("MyUnknownColumn", "MyUnknownColumn_[0]", "gameID", "plays", "col2", "field", "teamfield", "time", "WRtarget", "QBtarget",
                                "RushAtt", "Down", "Distance", "Time_Left", "Quarter", "Rusher", "PassYds", "RecYds", "RushYds", "PassTD", "INT", "RushTD",
                                "RecTD", "Fumble", "PassAtt", "PassComp", "Rush", "Target", "FirstDown", "Season")
  
  write.csv(playbyplay2019, paste0("../data/playbyplay",Sys.Date(),"_", format(Sys.time(), "%h%m%s"), ".csv"), row.names = FALSE)
  lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
}