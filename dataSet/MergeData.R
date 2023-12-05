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
        ylab = "Price per Average Score", 
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

