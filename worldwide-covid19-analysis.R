require(ggplot2)
require(plotly)
require(tidyr)

# Worldwide CoronaVirus cases
worldwideAgg <- read.csv("./data/worldwideAggregated.csv")
head(worldwideAgg)

plot_ly(x = as.Date(worldwideAgg$Date), y = worldwideAgg$Confirmed, type = "scatter", mode = "lines" )

fig <- plot_ly(worldwideAgg, x = as.Date(worldwideAgg$Date))
fig <- add_trace(fig, y = ~worldwideAgg$Confirmed, name = "Confirmed", mode = "lines", color = I("salmon"))
fig <- add_trace(fig, y = ~worldwideAgg$Recovered, name = "Recovered", mode = "lines", color = I("cyan"))
fig <- add_trace(fig, y = ~worldwideAgg$Deaths, name = "Deaths", mode = "lines", color = I("orangered"))
fig <- layout(fig, title = "CoronaVirus Cases arould the World",
              xaxis = list(title = "Date"),
              yaxis = list(title = "Number of Patients"))#, hovermode = "compare")
fig


#Plot WorldWide CoronaVirus Cases on Map
countriesAgg <- read.csv("./data/countriesAggregated.csv")
v1 <- tapply(countriesAgg$Confirmed, factor(countriesAgg$Country), max)
v2 <- tapply(countriesAgg$Recovered, factor(countriesAgg$Country), max)
v3 <- tapply(countriesAgg$Deaths, factor(countriesAgg$Country), max)
plotWorldDF <- data.frame("Country" = names(v1), "Confirmed" = v1, "Recovered" = v2, "Deaths" = v3)
temp <- subset(INDIA_States, subset = (is.na(INDIA_States$Province_State) == "TRUE"))
temp <- temp[,c("iso3", "Country_Region")]
plotWorldDF <- merge.data.frame(plotWorldDF, temp, by.x = "Country", by.y = "Country_Region")

plotWorldDF$hover <- with(plotWorldDF, paste(Country, '<br>', 
                                             "<b>Confirmed: </b>", Confirmed, '<br>',
                                             "<b>Recovered: </b>", Recovered, '<br>',
                                             "<b>Deaths: </b>", Deaths))

fig <- plot_ly(plotWorldDF, type = "choropleth", 
               locations = plotWorldDF$iso3, z = plotWorldDF$Confirmed,
               text = plotWorldDF$hover, colorscale = "Bluered")
fig


# Daily change in number of cases
worldwideAgg <- read.csv("./data/worldwideAggregated.csv")
head(worldwideAgg)

plot_ly(x = as.Date(worldwideAgg$Date), y = c(0, diff(worldwideAgg$Confirmed)), type = "scatter", mode = "lines" )

fig <- plot_ly(worldwideAgg, x = as.Date(worldwideAgg$Date))
fig <- add_trace(fig, y = ~c(0, diff(worldwideAgg$Confirmed)), name = "Daily Change in Cases", mode = "lines")
fig <- layout(fig, title = "Daily Change in Number of CoronaVirus Cases arould the World",
              xaxis = list(title = "Date"),
              yaxis = list(title = "Number of Confirmed Cases"))
fig


# Happiness Report Global
happiness_report20 <- read.csv("./data/Happiness Report/worldwide_happiness_report.csv")

# Subset important columns
happiness_report20sub <- subset(happiness_report20, select = -c(Overall.rank, Score, Generosity, Perceptions.of.corruption))

#Cleaning Required
countriesAgg <- read.csv("./data/countriesAggregated.csv")
v1 <- tapply(countriesAgg$Confirmed, factor(countriesAgg$Country), max)
countries <- levels(factor(countriesAgg$Country))
v2 = c()
for (x in countries) {
    countriesAggsub <- countriesAgg[countriesAgg$Country == x,]
    v2 <- append(v2, max(diff(countriesAggsub$Confirmed)))
}
plotWorldDF <- data.frame("Country" = names(v1), "Confirmed" = v1, "Max.Infection.Rate" = v2)

#Cleaning Countries
## Czech Republic
plotWorldDF[plotWorldDF$Country=="Czechia",]$Country <- "Czech Republic"
## South Korea
plotWorldDF[plotWorldDF$Country=="Korea, South",]$Country <- "South Korea"
## Eswatini
happiness_report20[happiness_report20$Country.or.region=="Swaziland",]$Country.or.region <- "Eswatini"
## Taiwan
plotWorldDF[plotWorldDF$Country=="Taiwan*",]$Country <- "Taiwan"
## Trinidad and Tobago
happiness_report20sub[happiness_report20sub$Country.or.region=="Trinidad & Tobago",]$Country.or.region <- "Trinidad and Tobago"
## United States
plotWorldDF[plotWorldDF$Country=="US",]$Country <- "United States"

# MERGE Data
data = merge.data.frame(plotWorldDF, happiness_report20sub, by.x = "Country", by.y = "Country.or.region")

head(data)

# Plot GDP per capita versus Number of Cases

fig1 <- plot_ly(data = data, x = data$GDP.per.capita, y = log10(data$Confirmed),
                name = "Confirmed Cases", type = "scatter", mode = "markers")
fig1 <- layout(fig1, title = "GDP per capita versus CoronaVirus Cases",
               xaxis = list(title = "GDP per capita"),
               yaxis = list(title = "Log of Number of Confirmed Cases"))
fig2 <- plot_ly(data = data, x = data$GDP.per.capita, y = log10(data$Max.Infection.Rate), 
                name = "Maximum Infection Rate", type = "scatter", mode = "markers")
fig2 <- layout(fig2, title = "GDP per capita versus Maximum Infection Rate",
               xaxis = list(title = "GDP per capita"),
               yaxis = list(title = "Log of Maximum Infection Rate"))
fig <- subplot(fig1, fig2, shareY = TRUE, titleX = TRUE)
fig <- layout(fig, yaxis = list(title = "Log of Cases"))
fig

# Plot Social Support versus Number of Cases

fig1 <- plot_ly(data = data, x = data$Social.support, y = log10(data$Confirmed),
                name = "Confirmed Cases", type = "scatter", mode = "markers")
fig1 <- layout(fig1, title = "Social Support versus CoronaVirus Cases",
               xaxis = list(title = "Social Support"),
               yaxis = list(title = "Log of Number of Confirmed Cases"))
fig2 <- plot_ly(data = data, x = data$Social.support, y = log10(data$Max.Infection.Rate), 
                name = "Maximum Infection Rate", type = "scatter", mode = "markers")
fig2 <- layout(fig2, title = "Social Support versus Maximum Infection Rate",
               xaxis = list(title = "Social Support"),
               yaxis = list(title = "Log of Maximum Infection Rate"))
fig <- subplot(fig1, fig2, shareY = TRUE, titleX = TRUE)
fig <- layout(fig, yaxis = list(title = "Log of Cases"))
fig

# Plot Healthy Life Expectancy versus Number of Cases

fig1 <- plot_ly(data = data, x = data$Healthy.life.expectancy, y = log10(data$Confirmed),
                name = "Confirmed Cases", type = "scatter", mode = "markers")
fig1 <- layout(fig1, title = "Healthy Life Expectancy versus CoronaVirus Cases",
               xaxis = list(title = "Healthy Life Expectancy"),
               yaxis = list(title = "Log of Number of Confirmed Cases"))
fig2 <- plot_ly(data = data, x = data$Healthy.life.expectancy, y = log10(data$Max.Infection.Rate), 
                name = "Maximum Infection Rate", type = "scatter", mode = "markers")
fig2 <- layout(fig2, title = "Healthy Life Expectancy versus Maximum Infection Rate",
               xaxis = list(title = "Healthy Life Expectancy"),
               yaxis = list(title = "Log of Maximum Infection Rate"))
fig <- subplot(fig1, fig2, shareY = TRUE, titleX = TRUE)
fig <- layout(fig, yaxis = list(title = "Log of Cases"))
fig

# Plot Freedom to make Life Choices versus Number of Cases

fig1 <- plot_ly(data = data, x = data$Freedom.to.make.life.choices, y = log10(data$Confirmed),
                name = "Confirmed Cases", type = "scatter", mode = "markers")
fit <- lm(log10(data$Confirmed) ~ data$Freedom.to.make.life.choices, data = data)
summ <- summary(fit)
pred <- predict(fit)
ll.df <- data.frame(data$Freedom.to.make.life.choices, fit = pred, 
                    lb = pred - (1 * summ$sigma), 
                    ub = pred + (1 * summ$sigma))
ll.df <-ll.df[order(ll.df$data.Freedom.to.make.life.choices),]
fig1 <- add_lines(fig1, 
                  x = data$Freedom.to.make.life.choices, 
                  y = predict(fit), mode = "lines", name = "Linear Regression",
                  line=list(color="orangered", width=2))
fig1 <- add_ribbons(fig1, 
                    x=ll.df$data.Freedom.to.make.life.choices,
                    ymin=ll.df$lb, ymax=ll.df$ub, name="95% CI",
                    line=list(opacity=0.4, width=0, color="salmon"))
fig1 <- layout(fig1, title = "Freedom to make Life Choices versus CoronaVirus Cases",
               xaxis = list(title = "Freedom to make Life Choices"),
               yaxis = list(title = "Log of Number of Confirmed Cases"))

fig2 <- plot_ly(data = data, x = data$Freedom.to.make.life.choices, 
                y = log10(data$Max.Infection.Rate), 
                name = "Maximum Infection Rate", type = "scatter", mode = "markers")
fit <- lm(log10(data$Max.Infection.Rate) ~ data$Freedom.to.make.life.choices, data = data)
summ <- summary(fit)
pred <- predict(fit)
ll.df <- data.frame(data$Freedom.to.make.life.choices, fit = pred, 
                    lb = pred - (1 * summ$sigma), 
                    ub = pred + (1 * summ$sigma))
ll.df <-ll.df[order(ll.df$data.Freedom.to.make.life.choices),]
fig2 <- add_lines(fig2, 
                  x = data$Freedom.to.make.life.choices, 
                  y = predict(fit), mode = "lines", name = "Linear Regression",
                  line=list(color="#366092", width=2))
fig2 <- add_ribbons(fig2, 
                    x=ll.df$data.Freedom.to.make.life.choices,
                    ymin=ll.df$lb, ymax=ll.df$ub, name="95% CI", 
                    line=list(opacity=0.4, width=0, color="#366092"))
fig2 <- layout(fig2, title = "Freedom to make Life Choices versus Maximum Infection Rate",
               xaxis = list(title = "Freedom to make Life Choices"),
               yaxis = list(title = "Log of Maximum Infection Rate"))


fig <- subplot(fig1, fig2, shareY = TRUE, titleX = TRUE)
fig <- layout(fig, yaxis = list(title = "Log of Cases"))
fig





