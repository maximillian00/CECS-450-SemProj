
library(dplyr)
dt <- read.csv("~/GitHub/CECS-450-SemProj/dataSet/movies_and_tv - Sheet1.csv")
#dt$group <- (mean(RottenTomatoes))


####################


#converts char column to double
score <- dt$RottenTomatoes #gets column of Rotten Tomatoe from dataset
decimal_list <- lapply(score, function(x) eval(parse(text = x))) 
dt$RottenTomatoes <- lapply(decimal_list, function(x) as.numeric(x))


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

#BOTH MOVIES AND SHOWS

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
        col = c("blue", "green", "red", "purple"),
        main = "Rotten Tomatoes Scores for Streaming Services (Movies and Shows)",
        ylab = "Rotten Tomatoes Score"

        )
#Adds labels on y axis
axis(2, at = seq(0.05, 0.95, by = 0.05), labels = seq(0.05, 0.95, by = 0.05))



#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#FOR SHOWS ONLY



#Uses subset data of already filtered to specific Streaming Services
#This will further filter the data to only include titles with the value 1 for Type
#When Type == 1 this means it is a show

netShow <- dplyr::filter(net,Type == 1) #Netflix shows
huluShow <- dplyr::filter(hulu,Type == 1) # Hulu shows
disShow <- dplyr::filter(dis,Type == 1) # Disney shows
primeShow <- dplyr::filter(prime,Type == 1) #Prime shows



nS <- as.numeric(length(unlist(netShow$RottenTomatoes))) #netflix
hS <- as.numeric(length(unlist(huluShow$RottenTomatoes))) #hulu
dS <- as.numeric(length(unlist(disShow$RottenTomatoes))) #disney
pS <- as.numeric(length(unlist(primeShow$RottenTomatoes))) #prime video

show <- data.frame(
  RottenTomatoes = c(unlist(netShow$RottenTomatoes), unlist(huluShow$RottenTomatoes), 
                     unlist(disShow$RottenTomatoes), unlist(primeShow$RottenTomatoes)),
  StreamingService = rep(c("Netflix", "Hulu", "Disney", "Prime"),
                         times = c(nS,hS,dS,pS))
)

# Create side-by-side boxplots for each streaming service
#will show the average score of the streaming service
boxplot(RottenTomatoes ~ StreamingService, data = show,
        col = c("blue", "green", "red", "purple"),
        main = "Rotten Tomatoes Scores for Streaming Services (Shows Only)",
        ylab = "Rotten Tomatoes Score"

        )
#Adds labels on y axis
axis(2, at = seq(0.05, 0.95, by = 0.05), labels = seq(0.05, 0.95, by = 0.05))


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


#FOR MOVIES ONLY


#Uses subset data of already filtered to specific Streaming Services
#This will further filter the data to only include titles with the value 0 for Type
#When Type == 0 this means it is a show

netMovie <- dplyr::filter(net,Type == 0) #Netflix shows
huluMovie <- dplyr::filter(hulu,Type == 0) # Hulu shows
disMovie <- dplyr::filter(dis,Type == 0) # Disney shows
primeMovie <- dplyr::filter(prime,Type == 0) #Prime shows



nM <- as.numeric(length(unlist(netMovie$RottenTomatoes))) #netflix
hM <- as.numeric(length(unlist(huluMovie$RottenTomatoes))) #hulu
dM <- as.numeric(length(unlist(disMovie$RottenTomatoes))) #disney
pM <- as.numeric(length(unlist(primeMovie$RottenTomatoes))) #prime video

movie <- data.frame(
  RottenTomatoes = c(unlist(netMovie$RottenTomatoes), unlist(huluMovie$RottenTomatoes), 
                     unlist(disMovie$RottenTomatoes), unlist(primeMovie$RottenTomatoes)),
  StreamingService = rep(c("Netflix", "Hulu", "Disney", "Prime"),
                         times = c(nM,hM,dM,pM))
)

# Create side-by-side boxplots for each streaming service
#will show the average score of the streaming service
boxplot(RottenTomatoes ~ StreamingService, data = movie,
        col = c("blue", "green", "red", "purple"),
        main = "Rotten Tomatoes Scores for Streaming Services (Movies Only)",
        ylab = "Rotten Tomatoes Score",
        par(bg = "#FFF8C1")

)
#Adds labels on y axis
axis(2, at = seq(0.05, 0.95, by = 0.05), labels = seq(0.05, 0.95, by = 0.05))


