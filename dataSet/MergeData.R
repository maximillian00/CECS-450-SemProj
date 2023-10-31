
dt <- read.csv("~/GitHub/CECS-450-SemProj/dataSet/movies_and_tv - Sheet1.csv")

#dt$group <- (mean(RottenTomatoes))

boxplot(mean(RottenTomatoes) ~ Year, data = dt,
        main = "Average score for Netflix",
        xlab = "Netflix",
        ylab = "Average Rotten Tomatoe score" 
        )

