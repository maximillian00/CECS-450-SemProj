
MoviesOnStreamingPlatforms <- read.csv("~/GitHub/CECS-450-SemProj/dataSet/MoviesOnStreamingPlatforms.csv")

tv_shows <- read.csv("~/GitHub/CECS-450-SemProj/dataSet/tv_shows.csv")

dt <- merge(MoviesOnStreamingPlatforms, tv_shows)
