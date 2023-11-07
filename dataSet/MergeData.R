
dt <- read.csv("~/GitHub/CECS-450-SemProj/dataSet/movies_and_tv - Sheet1.csv")

#dt$group <- (mean(RottenTomatoes))


temp <- c("80/100", "100/100")
decimal_list <- lapply(temp, function(x) eval(parse(text = x)))
decimal_list <- lapply(decimal_list, function(x) as.numeric(x))

print(decimal_list)

score <- dt$RottenTomatoes

avg <- lapply(score, mean, na.rm = TRUE)


boxplot(avg ~ Netflix, data = dt,
        main = "Average score for Netflix",
        xlab = "Netflix",
        ylab = "Average Rotten Tomatoe score" 
        )

