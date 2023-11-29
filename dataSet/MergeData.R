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




#########################################
# Plots Netflix average Rotten Tomato score for show and movies
# 0 is another streaming service and 1 is Netflix's score

boxplot(unlist(RottenTomatoes) ~ Netflix,data = dt,
        names = c("Other","Netflix"),
        main = "Average score for Netflix V.S. Other Streaming Platforms",
        xlab = "streaming services",
        ylab = "Average score" 
)

# Plots Disney average Rotten Tomato score for show and movies
# 0 is another streaming service and 1 is Disney's score
boxplot(unlist(RottenTomatoes) ~ dt$Disney, data = dt,
        names = c("Other","Disney"),
        main = "Average score for Netflix V.S. Other Streaming Platforms",
        xlab = "streaming services",
        ylab = "Average score" 
)
# Plots Hulu average Rotten Tomato score for show and movies
# 0 is another streaming service and 1 is Hulu's score
boxplot(unlist(RottenTomatoes) ~ dt$Hulu, data = dt,
        names = c("Other", "Hulu"),
        main = "Average score for Netflix V.S. Other Streaming Platforms",
        xlab = "streaming services",
        ylab = "Average score" 
)
# Plots Prime average Rotten Tomato score for show and movies
# 0 is another streaming service and 1 is Prime's score
boxplot(unlist(RottenTomatoes) ~ dt$PrimeVideo, data = dt,
        names = c("Other", "Prime"),
        main = "Average score for Netflix V.S. Other Streaming Platforms",
        xlab = "streaming services",
        ylab = "Average score" 
)




#Plots average score shows and movies on Netflix
# 0 is movies and 1 is shows 
net <- dplyr::filter(dt,Netflix == 1)

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




#




dt$StreamingService <- ifelse(dt$Netflix == 1, "Netflix",
                              ifelse(dt$Hulu == 1, "Hulu",
                                     ifelse(dt$Disney == 1, "Disney",
                                            ifelse(dt$PrimeVideo == 1, "PrimeVideo", "Other"))))

# Create a boxplot for each platform


#boxplot(unlist(RottenTomatoes) ~ Netflix, data = net, col = "blue",at = 1, width = 0.2, main = "Rotten Tomatoes Scores for Streaming Services (Value = 1)", ylab = "Rotten Tomatoes Score")
#boxplot(unlist(RottenTomatoes) ~ Hulu, data = hulu, col = "red",at = 1.75, width = 0.2, add = TRUE)
#boxplot(unlist(RottenTomatoes) ~ Disney, data = dis, col = "green",at = 2.5, width = 0.2, add = TRUE)
#boxplot(unlist(RottenTomatoes) ~ PrimeVideo, data = prime, col = "purple", at = 3.5, width = 0.2, add = TRUE)
#legend("topright", legend = c("Netflix", "Hulu", "Disney", "Prime"), fill = c("blue", "red", "green", "purple"))
#pdf("boxplot.pdf", width = 10, height = 6)
##################################

#Get the length of titles in each streaming platform
n <- as.numeric(length(unlist(net$RottenTomatoes))) #netflix
h <- as.numeric(length(unlist(hulu$RottenTomatoes))) #hulu
d <- as.numeric(length(unlist(dis$RottenTomatoes))) #disney
p <- as.numeric(length(unlist(prime$RottenTomatoes))) #prime video

combined_data <- data.frame(
  RottenTomatoes = c(unlist(net$RottenTomatoes), unlist(hulu$RottenTomatoes), 
                     unlist(dis$RottenTomatoes), unlist(prime$RottenTomatoes)),
  StreamingService = rep(c("Netflix", "Hulu", "Disney", "Prime"),
                         times = c(n,h,d,p))
)






# Create side-by-side boxplots for each streaming service
#will show the average score of the streaming service
boxplot(RottenTomatoes ~ StreamingService, data = combined_data,
        col = c("blue", "red", "green", "purple"),
        main = "Average Rotten Tomatoes Score for Streaming Services",
        ylab = "Rotten Tomatoes Score",
        par(mar = c(5, 4, 4, 8)))  # Adjust the margin if needed






