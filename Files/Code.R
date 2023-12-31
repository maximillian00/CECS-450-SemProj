
library(dplyr)
dt <- read.csv("~/GitHub/CECS-450-SemProj/dataSet/movies_and_tv - Sheet1.csv")


####################


#converts char column to double
score <- dt$RottenTomatoes #gets column of Rotten Tomatoe from dataset
decimal_list <- lapply(score, function(x) eval(parse(text = x))) 
dt$RottenTomatoes <- lapply(decimal_list, function(x) as.numeric(x))


#########################################
# Plots Netflix average Rotten Tomato score for show and movies
# 0 is another streaming service and 1 is Netflix's score
par(bg = "#FFFEDB")


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
        col = c("#E1A7F5", "#BBF5A7"),
        main = "Show V.S. Movies Average Score on Netflix",
        xlab = "Netflix",
        ylab = "Average score" 
)
axis(2, at = seq(0.05, 1, by = 0.05), labels = seq(0.05, 1, by = 0.05))


#Plots average score shows and movies on Disney
# 0 is movies and 1 is shows 

dis <- dplyr::filter(dt,Disney == 1)

boxplot(unlist(RottenTomatoes) ~ Type, data = dis,
        names = c("Movie","Show"),
        col = c("#E1A7F5", "#BBF5A7"),
        main = "Show V.S. Movies Average Score on Disney",
        xlab = "Disney+",
        ylab = "Average score" 
)
axis(2, at = seq(0.05, 1, by = 0.05), labels = seq(0.05, 1, by = 0.05))


#Plots average score shows and movies on Hulu
# 0 is movies and 1 is shows 
hulu <- dplyr::filter(dt,Hulu == 1)

boxplot(unlist(RottenTomatoes) ~ Type, data = hulu,
        names = c("Movie","Show"),
        col = c("#E1A7F5", "#BBF5A7"),
        main = "Show V.S. Movies Average Score on Hulu",
        xlab = "Hulu",
        ylab = "Average score" 
)
axis(2, at = seq(0.05, 0.95, by = 0.05), labels = seq(0.05, 0.95, by = 0.05))


#Plots average score shows and movies on Prime Video
# 0 is movies and 1 is shows 
prime <- dplyr::filter(dt,PrimeVideo == 1)


boxplot(unlist(RottenTomatoes) ~ Type, data = prime,
        names = c("Movie","Show"),
        col = c("#E1A7F5", "#BBF5A7"),
        main = "Show V.S. Movies Average Score on Prime Video",
        xlab = "Prime Video",
        ylab = "Average score" 
)
axis(2, at = seq(0.05, 1, by = 0.05), labels = seq(0.05, 1, by = 0.05))





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
        col = c("#16A5E4", "green", "red", "purple"),
        main = "Rotten Tomatoes Scores for Streaming Services (Movies and Shows)",
        ylab = "Rotten Tomatoes Score"

        )
#Adds labels on y axis
axis(2, at = seq(0.05, 0.95, by = 0.05), labels = seq(0.05, 0.95, by = 0.05))


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


# Filters subsets to only get titles when type ==1



netShow <- dplyr::filter(net,Type == 1) #Netflix shows
huluShow <- dplyr::filter(hulu,Type == 1) # Hulu shows
disShow <- dplyr::filter(dis,Type == 1) # Disney shows
primeShow <- dplyr::filter(prime,Type == 1) #Prime shows






#FOR SHOWS ONLY



#Uses subset data of already filtered to specific Streaming Services
#This will further filter the data to only include titles with the value 1 for Type
#When Type == 1 this means it is a show


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
        col = c("#16A5E4", "green", "red", "purple"),
        main = "Rotten Tomatoes Scores for Streaming Services (Shows Only)",
        ylab = "Rotten Tomatoes Score"

        )
#Adds labels on y axis
axis(2, at = seq(0.05, 0.95, by = 0.05), labels = seq(0.05, 0.95, by = 0.05))


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Filters subsets to only get titles when type ==0

netMovie <- dplyr::filter(net,Type == 0) #Netflix shows
huluMovie <- dplyr::filter(hulu,Type == 0) # Hulu shows
disMovie <- dplyr::filter(dis,Type == 0) # Disney shows
primeMovie <- dplyr::filter(prime,Type == 0) #Prime shows


#FOR MOVIES ONLY


#Uses subset data of already filtered to specific Streaming Services
#This will further filter the data to only include titles with the value 0 for Type
#When Type == 0 this means it is a show




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
#will show the average score for movies for each streaming service
boxplot(RottenTomatoes ~ StreamingService, data = movie,
        col = c("#16A5E4", "green", "red", "purple"),
        main = "Rotten Tomatoes Scores for Streaming Services (Movies Only)",
        ylab = "Rotten Tomatoes Score"
)
#Adds labels on y axis
axis(2, at = seq(0.05, 0.95, by = 0.05), labels = seq(0.05, 0.95, by = 0.05))






#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


#Best AVG SCORE for Age category for Movies/Shows to watch are 13+
boxplot(unlist(RottenTomatoes) ~ Age, data = dt,
        col = c("#16A5E4", "green", "red", "purple"),
        main = "Rotten Tomatoes Scores for Age (Movies and Shows)",
        ylab = "Rotten Tomatoes Score"

)
#Adds labels on y axis
axis(2, at = seq(0.05, 0.95, by = 0.05), labels = seq(0.05, 0.95, by = 0.05))



#Age rating subsets for both movies and shows

net13 <- dplyr::filter(net,Age == "13+") #Netflix shows
hulu13 <- dplyr::filter(hulu,Age == "13+") # Hulu shows
dis13 <- dplyr::filter(dis,Age == "13+") # Disney shows
prime13 <- dplyr::filter(prime,Age == "13+") #Prime shows

n13 <- as.numeric(length(unlist(net13$RottenTomatoes))) #netflix
h13 <- as.numeric(length(unlist(hulu13$RottenTomatoes))) #hulu
d13 <- as.numeric(length(unlist(dis13$RottenTomatoes))) #disney
p13 <- as.numeric(length(unlist(prime13$RottenTomatoes))) #prime video

combine13 <- data.frame(
  RottenTomatoes = c(unlist(net13$RottenTomatoes), unlist(hulu13$RottenTomatoes), 
                     unlist(dis13$RottenTomatoes), unlist(prime13$RottenTomatoes)),
  StreamingService = rep(c("Netflix", "Hulu", "Disney", "Prime"),
                         times = c(n13,h13,d13,p13)))
  
boxplot(RottenTomatoes ~ StreamingService, data = combine13,
        col = c("#16A5E4", "green", "red", "purple"),
        main = "Rotten Tomatoes Scores for Age 13+ (Movies and Shows)",
        ylab = "Rotten Tomatoes Score"

)
#Adds labels on y axis
axis(2, at = seq(0.05, 0.95, by = 0.05), labels = seq(0.05, 0.95, by = 0.05))

##############################################
#16+


net16 <- dplyr::filter(net,Age == "16+") #Netflix shows
hulu16 <- dplyr::filter(hulu,Age == "16+") # Hulu shows
dis16 <- dplyr::filter(dis,Age == "16+") # Disney shows
prime16 <- dplyr::filter(prime,Age == "16+") #Prime shows

n16 <- as.numeric(length(unlist(net16$RottenTomatoes))) #netflix
h16 <- as.numeric(length(unlist(hulu16$RottenTomatoes))) #hulu
d16 <- as.numeric(length(unlist(dis16$RottenTomatoes))) #disney
p16 <- as.numeric(length(unlist(prime16$RottenTomatoes))) #prime video

combine16 <- data.frame(
  RottenTomatoes = c(unlist(net16$RottenTomatoes), unlist(hulu16$RottenTomatoes), 
                     unlist(dis16$RottenTomatoes), unlist(prime16$RottenTomatoes)),
  StreamingService = rep(c("Netflix", "Hulu", "Disney", "Prime"),
                         times = c(n16,h16,d16,p16)))

boxplot(RottenTomatoes ~ StreamingService, data = combine16,
        col = c("#16A5E4", "green", "red", "purple"),
        main = "Rotten Tomatoes Scores for Age 16+ (Movies and Shows)",
        ylab = "Rotten Tomatoes Score"

)
#Adds labels on y axis
axis(2, at = seq(0.05, 0.95, by = 0.05), labels = seq(0.05, 0.95, by = 0.05))

#########################################
#18+
net18 <- dplyr::filter(net,Age == "18+") #Netflix shows
hulu18 <- dplyr::filter(hulu,Age == "18+") # Hulu shows
dis18 <- dplyr::filter(dis,Age == "18+") # Disney shows
prime18 <- dplyr::filter(prime,Age == "18+") #Prime shows

n18 <- as.numeric(length(unlist(net18$RottenTomatoes))) #netflix
h18 <- as.numeric(length(unlist(hulu18$RottenTomatoes))) #hulu
d18 <- as.numeric(length(unlist(dis18$RottenTomatoes))) #disney
p18 <- as.numeric(length(unlist(prime18$RottenTomatoes))) #prime video

combine18 <- data.frame(
  RottenTomatoes = c(unlist(net18$RottenTomatoes), unlist(hulu18$RottenTomatoes), 
                     unlist(dis18$RottenTomatoes), unlist(prime18$RottenTomatoes)),
  StreamingService = rep(c("Netflix", "Hulu", "Disney", "Prime"),
                         times = c(n18,h18,d18,p18)))

boxplot(RottenTomatoes ~ StreamingService, data = combine18,
        col = c("#16A5E4", "green", "red", "purple"),
        main = "Rotten Tomatoes Scores for Age 18+ (Movies and Shows)",
        ylab = "Rotten Tomatoes Score"

)
#Adds labels on y axis
axis(2, at = seq(0.05, 0.95, by = 0.05), labels = seq(0.05, 0.95, by = 0.05))
#####################################################




netAll <- dplyr::filter(net,Age == "all") #Netflix shows
huluAll <- dplyr::filter(hulu,Age == "all") # Hulu shows
disAll <- dplyr::filter(dis,Age == "all") # Disney shows
primeAll <- dplyr::filter(prime,Age == "all") #Prime shows


nAll <- as.numeric(length(unlist(netAll$RottenTomatoes))) #netflix
hAll <- as.numeric(length(unlist(huluAll$RottenTomatoes))) #hulu
dAll <- as.numeric(length(unlist(disAll$RottenTomatoes))) #disney
pAll <- as.numeric(length(unlist(primeAll$RottenTomatoes))) #prime video

combineAll <- data.frame(
  RottenTomatoes = c(unlist(netAll$RottenTomatoes), unlist(huluAll$RottenTomatoes), 
                     unlist(disAll$RottenTomatoes), unlist(primeAll$RottenTomatoes)),
  StreamingService = rep(c("Netflix", "Hulu", "Disney", "Prime"),
                         times = c(nAll,hAll,dAll,pAll)))

boxplot(RottenTomatoes ~ StreamingService, data = combineAll,
        col = c("#16A5E4", "green", "red", "purple"),
        main = "Rotten Tomatoes Scores for Age All (Movies and Shows)",
        ylab = "Rotten Tomatoes Score"

)
#Adds labels on y axis
axis(2, at = seq(0.05, 0.95, by = 0.05), labels = seq(0.05, 0.95, by = 0.05))
#####################################################



net7 <- dplyr::filter(net,Age == "7+") #Netflix shows
hulu7 <- dplyr::filter(hulu,Age == "7+") # Hulu shows
dis7 <- dplyr::filter(dis,Age == "7+") # Disney shows
prime7 <- dplyr::filter(prime,Age == "7+") #Prime shows


n7 <- as.numeric(length(unlist(net7$RottenTomatoes))) #netflix
h7 <- as.numeric(length(unlist(hulu7$RottenTomatoes))) #hulu
d7 <- as.numeric(length(unlist(dis7$RottenTomatoes))) #disney
p7 <- as.numeric(length(unlist(prime7$RottenTomatoes))) #prime video


combine7 <- data.frame(
  RottenTomatoes = c(unlist(net7$RottenTomatoes), unlist(hulu7$RottenTomatoes), 
                     unlist(dis7$RottenTomatoes), unlist(prime7$RottenTomatoes)),
  StreamingService = rep(c("Netflix", "Hulu", "Disney", "Prime"),
                         times = c(n7,h7,d7,p7)))

boxplot(RottenTomatoes ~ StreamingService, data = combine7,
        col = c("#16A5E4", "green", "red", "purple"),
        main = "Rotten Tomatoes Scores for Age 7 (Movies and Shows)",
        ylab = "Rotten Tomatoes Score"

)
#Adds labels on y axis
axis(2, at = seq(0.05, 0.95, by = 0.05), labels = seq(0.05, 0.95, by = 0.05))





netNR <- dplyr::filter(net,Age == "NR") #Netflix shows
huluNR <- dplyr::filter(hulu,Age == "NR") # Hulu shows
disNR <- dplyr::filter(dis,Age == "NR") # Disney shows
primeNR <- dplyr::filter(prime,Age == "NR") #Prime shows


nNR <- as.numeric(length(unlist(netNR$RottenTomatoes))) #netflix
hNR <- as.numeric(length(unlist(huluNR$RottenTomatoes))) #hulu
dNR <- as.numeric(length(unlist(disNR$RottenTomatoes))) #disney
pNR <- as.numeric(length(unlist(primeNR$RottenTomatoes))) #prime video


combineNR <- data.frame(
  RottenTomatoes = c(unlist(netNR$RottenTomatoes), unlist(huluNR$RottenTomatoes), 
                     unlist(disNR$RottenTomatoes), unlist(primeNR$RottenTomatoes)),
  StreamingService = rep(c("Netflix", "Hulu", "Disney", "Prime"),
                         times = c(nNR,hNR,dNR,pNR)))

boxplot(RottenTomatoes ~ StreamingService, data = combineNR,
        col = c("#16A5E4", "green", "red", "purple"),
        main = "Rotten Tomatoes Scores for Age NR (Movies and Shows)",
        ylab = "Rotten Tomatoes Score"

)
#Adds labels on y axis
axis(2, at = seq(0.05, 0.95, by = 0.05), labels = seq(0.05, 0.95, by = 0.05))
                            

#Labels and values to create pie chart
labels = c("Prime","Netflix", "Hulu", "Disney")
amount_on_netflix <- mean(dt$Netflix, na.rm =TRUE)
amount_on_prime <- mean(dt$PrimeVideo, na.rm =TRUE)
amount_on_hulu<- mean(dt$Hulu, na.rm =TRUE)
amount_on_dis <- mean(dt$Disney, na.rm = TRUE)
percentages <- round((list_of_avgs/sum(list_of_avgs))*100,1)
list_of_avgs <-c(amount_on_prime, amount_on_netflix, amount_on_hulu, amount_on_dis)

#Pie chart creation
pie(list_of_avgs, labels = paste(labels, "\n", percentages, "%"), main = "Streaming Service with the Most Titles", col =  c("purple", "red", "green", "blue"))

#Prices of services with and without ads
prime_price <-9

netflix_ads <-7
hulu_ads <-8
disney_ads<- 9

netflix_no_ads<-10
hulu_no_ads<-15
disney_no_ads<-14

#vector for prices with ads
prices_ads <-c(prime_price,netflix_ads, hulu_ads,disney_ads)
barplot(prices_ads, names.arg = labels, col = c("purple", "red", "green", "blue"), main = "Monthly Price of Streaming Services with Ads", ylab = "Prices(USD)", xlab = "Services", ylim = c(0,10))

#vector for prices without ads
prices_no_ads <- c(prime_price, netflix_no_ads, hulu_no_ads, disney_no_ads )
barplot(prices_no_ads, names.arg = labels, col = c("purple", "red", "green", "blue"), main = "Monthly Price of Streaming Services without Ads", ylab = "Prices(USD)", xlab = "Services", ylim = c(0,16))

#Create Barplots to compare Average Rotten Tomato Score over Price 
#per Streaming Service (IE the Value per Streaming Service)

#Average Rotten Tomato Score per Streaming Service
prime_avg <- mean(unlist(prime$RottenTomatoes))
net_avg <- mean(unlist(net$RottenTomatoes))
hulu_avg <- mean(unlist(hulu$RottenTomatoes))
dis_avg <- mean(unlist(dis$RottenTomatoes))

prime_avg <- as.numeric(prime_avg)
net_avg <- as.numeric(net_avg)
hulu_avg <- as.numeric(hulu_avg)
dis_avg <- as.numeric(dis_avg)

# Calculate the average score per each streaming service's price with ads
prime_value<- (prime_avg/prime_price) 
net_value_ads<-(net_avg/netflix_ads) 
hulu_value_ads<- (hulu_avg/hulu_ads) 
disney_value_ads<- (dis_avg/disney_ads) 

# Do the same for prices without ads 
net_value<- (net_avg/netflix_no_ads) 
hulu_value<- (hulu_avg/hulu_no_ads) 
disney_value<- (dis_avg/disney_no_ads) 

# Create the bar plot for average price per score with ads
value_ads <-c(prime_value, net_value_ads, hulu_value_ads, disney_value_ads)
barplot(value_ads, names.arg = labels, 
        col = c("purple", "red", "green", "blue"), 
        main = "Value per Streaming Service with Ads", 
        ylab = "Average Score over Price", 
        xlab = "Streaming Service",
        ylim = c(0,0.08))

# Create the bar plot for average price per score without ads
value_no_ads <-c(prime_value, net_value, hulu_value, disney_value)
barplot(value_no_ads, names.arg = labels,  
        col = c("purple", "red", "green", "blue"), 
        main = "Value per Streaming Service without Ads", 
        ylab = "Average Score over Price", 
        xlab = "Streaming Service",
        ylim = c(0,0.08))
