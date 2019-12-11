library(RMySQL)
aws = dbConnect(MySQL(), user='admin', password='expandtheboxscore', dbname='new_xtb_cfb', 
                host='database-1.chaj45z3rtec.us-east-2.rds.amazonaws.com')
query = dbSendQuery(aws, "select * from new_xtb_cfb.playbyplay")
playbyplay2019 = fetch(query, n=-1)
write.csv(playbyplay2019, paste0("../data/playbyplay",Sys.Date(),"_", format(Sys.time(), "%h%m%s"), ".csv"), row.names = FALSE)
lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
