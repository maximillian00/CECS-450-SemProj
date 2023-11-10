
dt <- read.csv("~/GitHub/CECS-450-SemProj/dataSet/movies_and_tv - Sheet1.csv")

#dt$group <- (mean(RottenTomatoes))



# random testing
temp <- c(80/100, 100/100)
print(temp)
####################

score <- dt$RottenTomatoes #gets column of Rotten Tomatoe from dataset

#converts char column to double
decimal_list <- lapply(score, function(x) eval(parse(text = x))) 
decimal_list <- lapply(decimal_list, function(x) as.numeric(x))
###################################


decimal_list <- unlist(decimal_list) #converts column to list (2d array -> array)
print(decimal_list)

# More random testing
avg <- mean(unlist(decimal_list))
print(avg)
###############################
Netflix <-

# Plots Netflix average Rotten Tomato score for show and movies
# 0 is another streaming service and 1 is Netflix's score
boxplot(decimal_list ~ dt$[Netflix==1] + dt$Hulu + dt$PrimeVideo + dt$Disney,
        names = c("Netflix", "Hulu", "Prime", "Disney"),
        main = "Average score for Netflix V.S. Other Streaming Platforms",
        xlab = "streaming services",
        ylab = "Average score" 
        )

#Plots average score shows and movies on Netflix
# 0 is movies and 1 is shows
boxplot(decimal_list ~ dt$Type,
        main = "Show V.S. Movies Average Score on Netflix",
        xlab = "Netflix",
        ylab = "Average score" 
        )

