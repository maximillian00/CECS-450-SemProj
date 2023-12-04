library(dplyr)
dt <- read.csv("~/GitHub/CECS-450-SemProj/dataSet/movies_and_tv - Sheet1.csv")

#dt$group <- (mean(RottenTomatoes))



# random testing
temp <- c(80/100, 100/100)
print(temp)
####################

score <- dt$RottenTomatoes #gets column of Rotten Tomatoe from dataset

#converts char column to double
decimal_list <- lapply(score, function(x) eval(parse(text = x))) 
dt$RottenTomatoes <- lapply(decimal_list, function(x) as.numeric(x))
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
boxplot(unlist(RottenTomatoes) ~ stack(Netflix + PrimeVideo + Disney + Hulu) , data = dt,
        main = "Rotten Tomatoes Score for Streaming Platforms",
        xlab = "Streaming Services",
        ylab = "Rotten Tomatoes Score"
)


# Subset data where Netflix, Hulu, Disney, or PrimeVideo is 1
subset_data <- dt[dt$Netflix == 1 | dt$Hulu == 1 | dt$Disney == 1 | dt$PrimeVideo == 1, ]

# Create a boxplot
boxplot(unlist(RottenTomatoes) ~ interaction(Netflix, Hulu, Disney, PrimeVideo), data = subset_data,
        main = "Rotten Tomatoes Score for Streaming Platforms (when value is 1)",
        xlab = "Streaming Services",
        ylab = "Rotten Tomatoes Score"
)


#Plots average score shows and movies on Netflix
# 0 is movies and 1 is shows
boxplot(decimal_list ~ dt$Type,
        main = "Show V.S. Movies Average Score on Netflix",
        xlab = "Netflix",
        ylab = "Average score" 
)

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

#price to movie ratio

#price to show ratio