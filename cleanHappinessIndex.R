a = read.csv("./data/Happiness Report/worldwide_happiness_report15.csv")
a <- subset(a, select = c("Country", "Happiness.Rank", "Happiness.Score", "Economy..GDP.per.Capita.",
                          "Health..Life.Expectancy.", "Freedom", "Generosity"))
names(a) <- c("Country", "Overall.Rank", "Score", "GDP.per.capita", 
              "Healthy.Life.Expectancy", "Freedom.to.make.life.choices", "Generosity")
a$Year <- "2015"
a$Social.support <- NA

b = read.csv("./data/Happiness Report/worldwide_happiness_report16.csv")
b <- subset(b, select = c("Country", "Happiness.Rank", "Happiness.Score", "Economy..GDP.per.Capita.", 
                          "Health..Life.Expectancy.", "Freedom", "Generosity"))
names(b) <- c("Country", "Overall.Rank", "Score", "GDP.per.capita", 
              "Healthy.Life.Expectancy", "Freedom.to.make.life.choices", "Generosity")
b$Year <- "2016"
b$Social.support <- NA

c = read.csv("./data/Happiness Report/worldwide_happiness_report17.csv")
c <- subset(c, select = c("Country", "Happiness.Rank", "Happiness.Score", "Economy..GDP.per.Capita.", 
                          "Health..Life.Expectancy.", "Freedom", "Generosity"))
names(c) <- c("Country", "Overall.Rank", "Score", "GDP.per.capita", 
              "Healthy.Life.Expectancy", "Freedom.to.make.life.choices", "Generosity")
c$Year <- "2017"
c$Social.support <- NA

d = read.csv("./data/Happiness Report/worldwide_happiness_report18.csv")
d <- subset(d, select = c("Country.or.region", "Overall.rank", "Score", "GDP.per.capita", 
                          "Social.support", "Healthy.life.expectancy", "Freedom.to.make.life.choices",
                          "Generosity"))
names(d) <- c("Country", "Overall.Rank", "Score", "GDP.per.capita", "Social.support",
              "Healthy.Life.Expectancy", "Freedom.to.make.life.choices", "Generosity")
d$Year <- "2018"

e = read.csv("./data/Happiness Report/worldwide_happiness_report19.csv")
e <- subset(e, select = c("Country.or.region", "Overall.rank", "Score", "GDP.per.capita", 
                          "Social.support", "Healthy.life.expectancy", "Freedom.to.make.life.choices",
                          "Generosity"))
names(e) <- c("Country", "Overall.Rank", "Score", "GDP.per.capita", "Social.support",
              "Healthy.Life.Expectancy", "Freedom.to.make.life.choices", "Generosity")
e$Year <- "2019"

f = read.csv("./data/Happiness Report/worldwide_happiness_report20.csv")
f$Overall.Rank <- c(1:nrow(f))
f <- subset(f, select = c("country", "Overall.Rank", "happiness_score", "gdp_per_capita", 
                          "social_support", "health", "freedom",
                          "generosity"))
names(f) <- c("Country", "Overall.Rank", "Score", "GDP.per.capita", "Social.support",
              "Healthy.Life.Expectancy", "Freedom.to.make.life.choices", "Generosity")
f$Year <- "2020"

hri <- rbind.data.frame(a,b,c,d,e,f)
hicountries <- levels(factor(hri$Country))
save(hri, file = "./app/data/Happiness Report/hri.RDS")
save(hicountries, file = "./app/data/Happiness Report/hicountries.RDS")
