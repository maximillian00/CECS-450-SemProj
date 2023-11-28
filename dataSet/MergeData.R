library(dplyr)
dt <- read.csv("~/GitHub/CECS-450-SemProj/dataSet/movies_and_tv - Sheet1.csv")
show <- read.csv("~/GitHub/CECS-450-SemProj/dataSet/tv_shows.csv")
movie <- read.csv("~/GitHub/CECS-450-SemProj/dataSet/MoviesOnStreamingPlatforms.csv")

#dt$group <- (mean(RottenTomatoes))


####################


#converts char column to double
score <- dt$RottenTomatoes #gets column of Rotten Tomatoe from dataset
decimal_list <- lapply(score, function(x) eval(parse(text = x))) 
dt$RottenTomatoes <- lapply(decimal_list, function(x) as.numeric(x))


score <- show$RottenTomatoes #gets column of Rotten Tomatoe from dataset
decimal_list <- lapply(score, function(x) eval(parse(text = x))) 
show$RottenTomatoes <- lapply(decimal_list, function(x) as.numeric(x))


score <- movie$RottenTomatoes #gets column of Rotten Tomatoe from dataset
decimal_list <- lapply(score, function(x) eval(parse(text = x))) 
movie$RottenTomatoes <- lapply(decimal_list, function(x) as.numeric(x))

###################################

decimal_list <- unlist(decimal_list) #converts column to list (2d array -> array)
print(decimal_list)

# More random testing
avg <- mean(unlist(decimal_list))
print(avg)
###############################

# Plots Netflix average Rotten Tomato score for show and movies
# 0 is another streaming service and 1 is Netflix's score
net <- dplyr::filter(dt,Netflix == 1)

boxplot(decimal_list ~ dt$Netflix,
        names = c("Other","Netflix"),
        main = "Average score for Netflix V.S. Other Streaming Platforms",
        xlab = "streaming services",
        ylab = "Average score" 
)

# Plots Disney average Rotten Tomato score for show and movies
# 0 is another streaming service and 1 is Disney's score
boxplot(decimal_list ~ dt$Disney,
        names = c("Other","Disney"),
        main = "Average score for Netflix V.S. Other Streaming Platforms",
        xlab = "streaming services",
        ylab = "Average score" 
)
# Plots Hulu average Rotten Tomato score for show and movies
# 0 is another streaming service and 1 is Hulu's score
boxplot(decimal_list ~ dt$Hulu,
        names = c("Other", "Hulu"),
        main = "Average score for Netflix V.S. Other Streaming Platforms",
        xlab = "streaming services",
        ylab = "Average score" 
)
# Plots Prime average Rotten Tomato score for show and movies
# 0 is another streaming service and 1 is Prime's score
boxplot(decimal_list ~ dt$PrimeVideo,
        names = c("Other", "Prime"),
        main = "Average score for Netflix V.S. Other Streaming Platforms",
        xlab = "streaming services",
        ylab = "Average score" 
)

boxplot(unlist(RottenTomatoes) ~ Netflix + PrimeVideo + Disney + Hulu, data = dt,
        names = c("Other","Netflix", "Other", "Disney", "", ""),
        main = "Average score for Netflix V.S. Other Streaming Platforms",
        xlab = "streaming services",
        ylab = "Average score" 
)



#Plots average score shows and movies on Netflix
# 0 is movies and 1 is shows 
boxplot(unlist(RottenTomatoes) ~ Type, data = net,
        names = c("Movie","Show"),
        main = "Show V.S. Movies Average Score on Netflix",
        xlab = "Netflix",
        ylab = "Average score" 
)


#Plots average score shows and movies on Disney
# 0 is movies and 1 is shows 

dis <- dplyr::filter(dt,Disney == 1)

boxplot(unlist(RottenTomatoes) ~ Type, data = dis,
        names = c("Movie","Show"),
        main = "Show V.S. Movies Average Score on Disney",
        xlab = "Disney+",
        ylab = "Average score" 
)


#Plots average score shows and movies on Hulu
# 0 is movies and 1 is shows 
hulu <- dplyr::filter(dt,Hulu == 1)

boxplot(unlist(RottenTomatoes) ~ Type, data = hulu,
        names = c("Movie","Show"),
        main = "Show V.S. Movies Average Score on Hulu",
        xlab = "Hulu",
        ylab = "Average score" 
)

#Plots average score shows and movies on Prime Video
# 0 is movies and 1 is shows 
prime <- dplyr::filter(dt,PrimeVideo == 1)


boxplot(unlist(RottenTomatoes) ~ Type, data = prime,
        names = c("Movie","Show"),
        main = "Show V.S. Movies Average Score on Prime Video",
        xlab = "Prime Video",
        ylab = "Average score" 
)

