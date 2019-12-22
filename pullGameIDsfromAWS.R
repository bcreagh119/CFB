library(RMySQL)
pullGameIDsfromAWS <- function(aws = 'dbconnect to whatever sql server'){
  query = dbSendQuery(aws, "select * from new_xtb_cfb.games")
  playbyplay2019 = fetch(query, n=-1)
  
  write.csv(playbyplay2019, paste0("../data/gameIDs",Sys.Date(),"_", format(Sys.time(), "%h%m%s"), ".csv"), row.names = FALSE)
  lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
}