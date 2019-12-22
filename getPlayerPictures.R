library(httr)
library(jsonlite)
connection <- GET("http://site.api.espn.com/apis/common/v3/sports/football/college-football/statistics/byathlete?category=offense%3Apassing&conference=80&contentorigin=espn&isqualified=false&lang=en&region=us&sort=passing.passingYards%3Adesc&limit=1000", timeout(30))
response_text <- content(connection, as='text')
df <- fromJSON(response_text)

out <- data.frame(names <- df$athletes$athlete$displayName,
                  playerIDs <- df$athletes$athlete$id,
                  playerPictureLink <- df$athletes$athlete$headshot$href
)

colnames(out) <- c("name", "playerID", "playerPictureLink")

if (!file.exists("data/pics")){
  dir.create(paste(getwd(), "/data/pics", sep=''), recursive = TRUE)
}

for(i in 1:nrow(out)){
  if(!is.na(out$playerPictureLink[i])){
    download.file(as.character(out$playerPictureLink[i]), destfile = paste("data/pics/", out$name[i], ".png", sep = ""), method = "libcurl", quiet = TRUE, mode = "wb")
  }
}
