
dt <- read.csv("~/GitHub/CECS-450-SemProj/dataSet/movies_and_tv - Sheet1.csv")

#dt$group <- (mean(RottenTomatoes))
temp <- as.integer(c("100/100", "100/100"))

score <- as.numeric(dt$RottenTomatoes)

avg <- lapply(score, mean, na.rm = TRUE)


boxplot(avg ~ Netflix, data = dt,
        main = "Average score for Netflix",
        xlab = "Netflix",
        ylab = "Average Rotten Tomatoe score" 
        )
