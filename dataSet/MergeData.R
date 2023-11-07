
dt <- read.csv("~/GitHub/CECS-450-SemProj/dataSet/movies_and_tv - Sheet1.csv")

#dt$group <- (mean(RottenTomatoes))


temp <- c("80/100", "100/100")
decimal_list <- lapply(temp, function(x) eval(parse(text = x)))
decimal_list <- lapply(decimal_list, function(x) as.numeric(x))

print(decimal_list)

score <- dt$RottenTomatoes
decimal_list <- lapply(score, function(x) eval(parse(text = x)))
decimal_list <- lapply(decimal_list, function(x) as.numeric(x))
print(decimal_list)

avg <- mean(unlist(decimal_list))
print(avg)

net <- unique(dt$Netflix)

data <- data.frame(Average = avg, Netflix = net)


boxplot(data,
        main = "Average score for Netflix",
        xlab = "Netflix",
        ylab = "Average Rotten Tomatoe score" 
        )

