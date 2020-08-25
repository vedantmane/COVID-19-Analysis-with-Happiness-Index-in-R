
library(shiny)
library(plotly)
library(dplyr)

#Create Data Directory
if(!dir.exists("./data")){
    dir.create("./data/")
}

#Download Countries Aggregated Data
countriesAggURL <- "https://raw.githubusercontent.com/datasets/covid-19/master/data/countries-aggregated.csv"
download.file(countriesAggURL, "./data/countriesAggregated.csv")

#Download Worldwide Aggregated Data
worldwideAggURL <- "https://raw.githubusercontent.com/datasets/covid-19/master/data/worldwide-aggregated.csv"
download.file(worldwideAggURL, "./data/worldwideAggregated.csv")
# 
# #Time-series Covid-19 combined data
# timeSeriesURL <- "https://raw.githubusercontent.com/datasets/covid-19/master/data/time-series-19-covid-combined.csv"
# download.file(timeSeriesURL, "./data/timeSeriesCOVID19.csv")

#Reference Covid-19 combined data
referenceURL <- "https://raw.githubusercontent.com/datasets/covid-19/master/data/reference.csv"
download.file(referenceURL, "./data/subsetINDIAStates.csv")
INDIA_States <- read.csv("./data/subsetINDIAStates.csv", na.strings = c("NA", "", " "))

# #Subset India and territories
# INDIA_States <- subset(INDIA_States, subset = (INDIA_States$Country_Region == "India"))
# write.csv(INDIA_States, "./data/INDIAStates.csv")

print("Downloading done...")

# #Reading Files
countriesAgg <- read.csv("./data/countriesAggregated.csv")
worldwideAgg <- read.csv("./data/worldwideAggregated.csv")
# timeSeriesCOVID19 <- read.csv("./data/timeSeriesCOVID19.csv")
# indiaStates <- read.csv("./data/INDIAStates.csv")
happiness_report20 <- read.csv("./data/Happiness Report/worldwide_happiness_report.csv")
load("./data/Happiness Report/hri.RDS")

print("Reading done...")

## Server Code

shinyServer(function(input, output, session) {
    
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
    happiness_report20sub[happiness_report20sub$Country.or.region=="Swaziland",]$Country.or.region <- "Eswatini"
    ## Taiwan
    plotWorldDF[plotWorldDF$Country=="Taiwan*",]$Country <- "Taiwan"
    ## Trinidad and Tobago
    happiness_report20sub[happiness_report20sub$Country.or.region=="Trinidad & Tobago",]$Country.or.region <- "Trinidad and Tobago"
    ## United States
    plotWorldDF[plotWorldDF$Country=="US",]$Country <- "United States"
    
    # MERGE Data
    data = merge.data.frame(plotWorldDF, happiness_report20sub, by.x = "Country", by.y = "Country.or.region")
    
    m <- list(
        l = 20,
        r = 20,
        b = 70,
        t = 70
    )
    
    filteredCountries <- reactive({
        countriesAgg[countriesAgg$Country %in% input$countrychoice, ]
    })
    
    filterIndia <- reactive({
        countriesAgg[countriesAgg$Country %in% "India", ]
    })
    
    happiness_index <- reactive({
        hri[hri$Country %in% input$hicountrychoice, ]
    })

    happiness_index_India <- reactive({
        hri[hri$Country %in% "India", ]
    })
    
    output$mapWorld <- renderPlotly({
        #Plot WorldWide CoronaVirus Cases on Map
        vc1 <- tapply(countriesAgg$Confirmed, factor(countriesAgg$Country), max)
        vc2 <- tapply(countriesAgg$Recovered, factor(countriesAgg$Country), max)
        vc3 <- tapply(countriesAgg$Deaths, factor(countriesAgg$Country), max)
        mapWorldDF <- data.frame("Country" = names(vc1), "Confirmed" = vc1, "Recovered" = vc2, "Deaths" = vc3)
        temp <- subset(INDIA_States, subset = (is.na(INDIA_States$Province_State) == "TRUE"))
        temp <- temp[,c("iso3", "Country_Region")]
        mapWorldDF <- merge.data.frame(mapWorldDF, temp, by.x = "Country", by.y = "Country_Region")
        mapWorldDF$hover <- with(mapWorldDF, paste(Country, '<br>', 
                                                   "<b>Confirmed: </b>", Confirmed, '<br>',
                                                   "<b>Recovered: </b>", Recovered, '<br>',
                                                   "<b>Deaths: </b>", Deaths))
        
        fig <- plot_ly(mapWorldDF, type = "choropleth", 
                       locations = mapWorldDF$iso3, z = mapWorldDF$Confirmed,
                       text = mapWorldDF$hover, colorscale = "Reds") %>%
            layout(title = "CoronaVirus Cases arould the World")
        fig
    })
    
    output$casesWorld <- renderPlotly({
        fig <- plot_ly(worldwideAgg, x = as.Date(worldwideAgg$Date)) %>%
            add_trace(y = ~worldwideAgg$Confirmed,
                      name = "Confirmed", mode = "lines", color = I("salmon")) %>%
            add_trace(y = ~worldwideAgg$Recovered,
                      name = "Recovered", mode = "lines", color = I("cyan")) %>%
            add_trace(y = ~worldwideAgg$Deaths, 
                         name = "Deaths", mode = "lines", color = I("orangered")) %>%
            layout(title = "CoronaVirus Cases arould the World",
                   xaxis = list(title = "Date"),
                   yaxis = list(title = "Number of Patients"), margin = m,
                   legend = list(x = 0.1, y = 1, 
                                 bgcolor = "#F2F2F2",
                                 bordercolor = "darkblue",
                                 borderwidth = 2,
                                 title=list(text='<b> Trend </b>')))
        fig
    })
    
    output$casesWorldDaily <- renderPlotly({
        fig <- plot_ly(worldwideAgg, x = as.Date(worldwideAgg$Date), fill = "tozeroy") %>%
            add_trace(y = ~c(0, diff(worldwideAgg$Confirmed)), 
                         name = "Daily Change in Cases", mode = "lines") %>%
            layout(title = "Daily Change in Number of CoronaVirus Cases arould the World",
                   xaxis = list(title = "Date"),
                   yaxis = list(title = "Number of Confirmed Cases"), margin = m)
        fig
    })

    output$casesGDPpc <- renderPlotly({
        
        fit <- lm(log10(data$Confirmed) ~ data$GDP.per.capita, data = data)
        summ <- summary(fit)
        pred <- predict(fit)
        ll.df <- data.frame(data$GDP.per.capita, fit = pred, 
                            lb = pred - (1 * summ$sigma), ub = pred + (1 * summ$sigma))
        ll.df <- ll.df[order(ll.df$data.GDP.per.capita),]
        
        fig1 <- plot_ly(data = data, 
                        x = data$GDP.per.capita, y = log10(data$Confirmed),
                        name = "Confirmed Cases", type = "scatter", mode = "markers") %>%
            add_lines(x = data$GDP.per.capita,
                      y = predict(fit), 
                      mode = "lines", name = "Linear Regression",
                      line=list(color="salmon", width=2), showlegend = FALSE) %>%
            add_ribbons(x=ll.df$data.GDP.per.capita,
                        ymin=ll.df$lb, ymax=ll.df$ub, name="68% CI", 
                        opacity=0.2,
                        fillcolor = "orangered",
                        line=list(width=0, color="salmon"), showlegend = FALSE) %>%
            layout(title = "GDP per capita versus CoronaVirus Cases",
                   xaxis = list(title = "GDP per capita"),
                   yaxis = list(title = "Log of Number of Confirmed Cases"))
        
        fit <- lm(log10(data$Max.Infection.Rate) ~ data$GDP.per.capita, data = data)
        summ <- summary(fit)
        pred <- predict(fit)
        ll.df <- data.frame(data$GDP.per.capita, fit = pred, 
                            lb = pred - (1 * summ$sigma), 
                            ub = pred + (1 * summ$sigma))
        ll.df <-ll.df[order(ll.df$data.GDP.per.capita),]

        fig2 <- plot_ly(data = data, 
                        x = data$GDP.per.capita, y = log10(data$Max.Infection.Rate), 
                        name = "Maximum Infection Rate", type = "scatter", mode = "markers") %>%
            add_lines(x = data$GDP.per.capita, 
                      y = predict(fit), 
                      mode = "lines", name = "Linear Regression",
                      line=list(color="blue", width=2), showlegend = FALSE) %>%
            add_ribbons(x=ll.df$data.GDP.per.capita,
                        ymin=ll.df$lb, ymax=ll.df$ub, name="68% CI", 
                        opacity=0.2, fillcolor="lightblue", 
                        line=list(color="blue", width=0), showlegend = FALSE) %>%
            layout(title = "GDP per capita versus Maximum Infection Rate",
                       xaxis = list(title = "GDP per capita"),
                       yaxis = list(title = "Log of Maximum Infection Rate"))
        
        fig <- subplot(fig1, fig2, shareY = TRUE, titleX = TRUE) %>%
            layout(yaxis = list(title = "Log of Cases"), margin = m,
                   legend = list(x = 0.4, y = 1, orientation = "v", 
                                 bgcolor = "#F2F2F2",
                                 bordercolor = "darkblue",
                                 borderwidth = 2,
                                 title=list(text='<b> Trend </b>')))
        fig
    })
    
    output$casesSS <- renderPlotly({

        fit <- lm(log10(data$Confirmed) ~ data$Social.support, data = data)
        summ <- summary(fit)
        pred <- predict(fit)
        ll.df <- data.frame(data$Social.support, fit = pred, 
                            lb = pred - (1 * summ$sigma), ub = pred + (1 * summ$sigma))
        ll.df <-ll.df[order(ll.df$data.Social.support),]

        fig1 <- plot_ly(data = data, 
                        x = data$Social.support, y = log10(data$Confirmed),
                        name = "Confirmed Cases", type = "scatter", mode = "markers") %>%
            add_lines(x = data$Social.support,
                      y = predict(fit), 
                      mode = "lines", name = "Linear Regression",
                      line=list(color="salmon", width=2), showlegend = FALSE) %>%
            add_ribbons(x=ll.df$data.Social.support,
                        ymin=ll.df$lb, ymax=ll.df$ub, name="68% CI",
                        opacity=0.2, fillcolor = "orangered", 
                        line=list(width=0, color="salmon"), showlegend = FALSE) %>%
            layout(title = "Social Support versus CoronaVirus Cases",
                   xaxis = list(title = "Social Support"),
                   yaxis = list(title = "Log of Number of Confirmed Cases"))
        
        fit <- lm(log10(data$Max.Infection.Rate) ~ data$Social.support, data = data)
        summ <- summary(fit)
        pred <- predict(fit)
        ll.df <- data.frame(data$Social.support, fit = pred, 
                            lb = pred - (1 * summ$sigma), 
                            ub = pred + (1 * summ$sigma))
        ll.df <-ll.df[order(ll.df$data.Social.support),]
        
        fig2 <- plot_ly(data = data, 
                        x = data$Social.support, y = log10(data$Max.Infection.Rate), 
                        name = "Maximum Infection Rate", type = "scatter", mode = "markers") %>%
            add_lines(x = data$Social.support, 
                      y = predict(fit), 
                      mode = "lines", name = "Linear Regression",
                      line=list(color="blue", width=2), showlegend = FALSE) %>%
            add_ribbons(x=ll.df$data.Social.support,
                        ymin=ll.df$lb, ymax=ll.df$ub, name="68% CI", 
                        opacity=0.2, fillcolor = "lightblue",
                        line=list(width=0, color="#366092"), showlegend = FALSE) %>%
            layout(title = "Social Support versus Maximum Infection Rate",
                   xaxis = list(title = "Social Support"),
                   yaxis = list(title = "Log of Maximum Infection Rate"))
    
        fig <- subplot(fig1, fig2, shareY = TRUE, titleX = TRUE) %>%
            layout(yaxis = list(title = "Log of Cases"), margin = m,
                   legend = list(x = 0.4, y = 1, orientation = "v", 
                                 bgcolor = "#F2F2F2",
                                 bordercolor = "darkblue",
                                 borderwidth = 2,
                                 title=list(text='<b> Trend </b>')))
        fig
        
    })
    
    output$casesHLE <- renderPlotly({

        fit <- lm(log10(data$Confirmed) ~ data$Healthy.life.expectancy, data = data)
        summ <- summary(fit)
        pred <- predict(fit)
        ll.df <- data.frame(data$Healthy.life.expectancy, fit = pred, 
                            lb = pred - (1 * summ$sigma), ub = pred + (1 * summ$sigma))
        ll.df <-ll.df[order(ll.df$data.Healthy.life.expectancy),]
        
        fig1 <- plot_ly(data = data, 
                        x = data$Healthy.life.expectancy, y = log10(data$Confirmed),
                        name = "Confirmed Cases", type = "scatter", mode = "markers") %>%
            add_lines(x = data$Healthy.life.expectancy, 
                      y = predict(fit), 
                      mode = "lines", name = "Linear Regression",
                      line=list(color="salmon", width=2), showlegend = FALSE) %>%
            add_ribbons(x=ll.df$data.Healthy.life.expectancy,
                        ymin=ll.df$lb, ymax=ll.df$ub, name="68% CI",
                        opacity=0.2, fillcolor = "orangered",
                        line=list(width=0, color="salmon"), showlegend = FALSE) %>%
            layout(title = "Healthy Life Expectancy versus CoronaVirus Cases",
                   xaxis = list(title = "Healthy Life Expectancy"),
                   yaxis = list(title = "Log of Number of Confirmed Cases"))
        
        fit <- lm(log10(data$Max.Infection.Rate) ~ data$Healthy.life.expectancy, data = data)
        summ <- summary(fit)
        pred <- predict(fit)
        ll.df <- data.frame(data$Healthy.life.expectancy, fit = pred, 
                            lb = pred - (1 * summ$sigma), 
                            ub = pred + (1 * summ$sigma))
        ll.df <-ll.df[order(ll.df$data.Healthy.life.expectancy),]
        
        fig2 <- plot_ly(data = data, 
                        x = data$Healthy.life.expectancy, y = log10(data$Max.Infection.Rate), 
                        name = "Maximum Infection Rate", type = "scatter", mode = "markers") %>%
            add_lines(x = data$Healthy.life.expectancy, 
                      y = predict(fit), 
                      mode = "lines", name = "Linear Regression",
                      line=list(color="blue", width=2), showlegend = FALSE) %>% 
            add_ribbons(x=ll.df$data.Healthy.life.expectancy,
                        ymin=ll.df$lb, ymax=ll.df$ub, name="68% CI", 
                        opacity=0.2, fillcolor = "lightblue",
                        line=list(width=0, color="blue"), showlegend = FALSE) %>%
            layout(title = "Healthy Life Expectancy versus Maximum Infection Rate",
                   xaxis = list(title = "Healthy Life Expectancy"),
                   yaxis = list(title = "Log of Maximum Infection Rate"))

        fig <- subplot(fig1, fig2, shareY = TRUE, titleX = TRUE) %>%
            layout(yaxis = list(title = "Log of Cases"), margin = m,
                   legend = list(x = 0.4, y = 1, orientation = "v", 
                                 bgcolor = "#F2F2F2",
                                 bordercolor = "darkblue",
                                 borderwidth = 2,
                                 title=list(text='<b> Trend </b>')))
        fig
        
    })
    
    output$casesFLC <- renderPlotly({

        fit <- lm(log10(data$Confirmed) ~ data$Freedom.to.make.life.choices, data = data)
        summ <- summary(fit)
        pred <- predict(fit)
        ll.df <- data.frame(data$Freedom.to.make.life.choices, fit = pred, 
                            lb = pred - (1 * summ$sigma), ub = pred + (1 * summ$sigma))
        ll.df <-ll.df[order(ll.df$data.Freedom.to.make.life.choices),]

        fig1 <- plot_ly(data = data, 
                        x = data$Freedom.to.make.life.choices, 
                        y = log10(data$Confirmed), name = "Confirmed Cases", 
                        type = "scatter", mode = "markers") %>%
            add_lines(x = data$Freedom.to.make.life.choices, 
                      y = predict(fit), 
                      mode = "lines", name = "Linear Regression",
                      line=list(color="salmon", width=2), showlegend = FALSE) %>%
            add_ribbons(x=ll.df$data.Freedom.to.make.life.choices,
                        ymin=ll.df$lb, ymax=ll.df$ub, name="68% CI",
                        opacity=0.2, fillcolor = "orangered",
                        line=list(width=0, color="salmon"), showlegend = FALSE) %>%
            layout(title = "Freedom to make Life Choices versus CoronaVirus Cases",
                   xaxis = list(title = "Freedom to make Life Choices"),
                   yaxis = list(title = "Log of Number of Confirmed Cases"))

        fit <- lm(log10(data$Max.Infection.Rate) ~ data$Freedom.to.make.life.choices, data = data)
        summ <- summary(fit)
        pred <- predict(fit)
        ll.df <- data.frame(data$Freedom.to.make.life.choices, fit = pred, 
                            lb = pred - (1 * summ$sigma), 
                            ub = pred + (1 * summ$sigma))
        ll.df <-ll.df[order(ll.df$data.Freedom.to.make.life.choices),]
        
        fig2 <- plot_ly(data = data, 
                        x = data$Freedom.to.make.life.choices, 
                        y = log10(data$Max.Infection.Rate), name = "Maximum Infection Rate", 
                        type = "scatter", mode = "markers") %>%
            add_lines(x = data$Freedom.to.make.life.choices, 
                      y = predict(fit), 
                      mode = "lines", name = "Linear Regression",
                      line=list(color="blue", width=2), showlegend = FALSE) %>%
            add_ribbons(x=ll.df$data.Freedom.to.make.life.choices,
                        ymin=ll.df$lb, ymax=ll.df$ub, name="68% CI",
                        opacity=0.2, fillcolor = "lightblue",
                        line=list(width=0, color="blue"), showlegend = FALSE) %>%
            layout(title = "Freedom to make Life Choices versus Maximum Infection Rate",
                   xaxis = list(title = "Freedom to make Life Choices"),
                   yaxis = list(title = "Log of Maximum Infection Rate"))
        
        fig <- subplot(fig1, fig2, shareY = TRUE, titleX = TRUE) %>%
            layout(yaxis = list(title = "Log of Cases"), margin = m,
                   legend = list(x = 0.4, y = 1, orientation = "v", 
                                 bgcolor = "#F2F2F2",
                                 bordercolor = "darkblue",
                                 borderwidth = 2,
                                 title=list(text='<b> Trend </b>')))
        fig
        
    })

    output$mapCountry <- renderPlotly({
        #Plot Countries CoronaVirus Cases on Map
        vcsub1 <- tapply(filteredCountries()$Confirmed, factor(filteredCountries()$Country), max)
        vcsub2 <- tapply(filteredCountries()$Recovered, factor(filteredCountries()$Country), max)
        vcsub3 <- tapply(filteredCountries()$Deaths, factor(filteredCountries()$Country), max)
        mapCountryDF <- data.frame("Country" = names(vcsub1), "Confirmed" = vcsub1, "Recovered" = vcsub2, "Deaths" = vcsub3)
        temp <- subset(INDIA_States, subset = (is.na(INDIA_States$Province_State) == "TRUE"))
        temp <- temp[,c("iso3", "Country_Region")]
        mapCountryDF <- merge.data.frame(mapCountryDF, temp, by.x = "Country", by.y = "Country_Region")
        mapCountryDF$hover <- with(mapCountryDF, paste(Country, '<br>', 
                                                   "<b>Confirmed: </b>", Confirmed, '<br>',
                                                   "<b>Recovered: </b>", Recovered, '<br>',
                                                   "<b>Deaths: </b>", Deaths))
        
        fig <- plot_ly(mapCountryDF, type = "choropleth", 
                       locations = mapCountryDF$iso3, z = mapCountryDF$Confirmed,
                       text = mapCountryDF$hover, colorscale = "Reds") %>%
            layout(title = "CoronaVirus Cases arould the World")
        fig
    })
        
    output$multipleC <- renderPlotly({
        plot_ly(filteredCountries(), x = ~as.Date(Date), y = ~Confirmed, 
                type = "scatter", mode = "lines", color = ~Country) %>%
            layout(title = "CoronaVirus Cases",
                   xaxis = list(title = "Date"),
                   yaxis = list(title = "Number of Patients"), margin = m,
                   legend = list(x = 0.05, y = 1, 
                                 bgcolor = "#F2F2F2",
                                 bordercolor = "darkblue",
                                 borderwidth = 2,
                                 title=list(text='<b> Trend </b>')))
        })

    output$multipleClog <- renderPlotly({
        plot_ly(filteredCountries(), x = ~as.Date(Date), y = ~log(Confirmed), 
                type = "scatter", mode = "lines", color = ~Country) %>%
            layout(title = "CoronaVirus Cases",
                   xaxis = list(title = "Date"),
                   yaxis = list(title = "Number of Patients"), margin = m,
                   legend = list(x = 0.05, y = 1, 
                                 bgcolor = "#F2F2F2",
                                 bordercolor = "darkblue",
                                 borderwidth = 2,
                                 title=list(text='<b> Trend </b>')))
    })
    
    output$multipleCdiff <- renderPlotly({
        fig <- plot_ly()
        for (ci in levels(factor(filteredCountries()$Country))){
            dt <- filteredCountries()[filteredCountries()$Country == ci,]
            fig <- add_trace(fig, data = dt, x = ~as.Date(Date), y = c(0, diff(dt$Confirmed)),
                             name = ci, type = "scatter", mode = "lines", 
                             opacity = 0.7)
        }
        fig %>%
            layout(title = "Daily CoronaVirus Cases",
                   xaxis = list(title = "Date"),
                   yaxis = list(title = "Number of Patients"), margin = m,
                   legend = list(x = 0.05, y = 1, 
                                 bgcolor = "#F2F2F2",
                                 bordercolor = "darkblue",
                                 borderwidth = 2,
                                 title=list(text='<b> Trend </b>')))
    })
    
    output$multipleRank <- renderPlotly({
        hisub <- happiness_index()
        plot_ly(data = hisub, x = ~factor(Year), y = ~Overall.Rank, color = ~Country,
                type = "scatter", mode = "lines+markers") %>%
            layout(yaxis = list(autorange = "reversed", rangemode = "tozero")) %>%
            layout(title = "Happiness Index Ranks",
                   xaxis = list(title = "Year"),
                   yaxis = list(title = "Overall Rank"), margin = m,
                   legend = list(bgcolor = "#F2F2F2",
                                 bordercolor = "darkblue",
                                 borderwidth = 2,
                                 title=list(text='<b> Trend </b>')))
    })
    
    output$multipleScore <- renderPlotly({
        hisub <- happiness_index()
        plot_ly(data = hisub, x = ~factor(Year), y = ~Score, color = ~Country,
                type = "scatter", mode = "lines+markers") %>%
            layout(yaxis = list(rangemode = "tozero")) %>%
            layout(title = "Happiness Index Score",
                   xaxis = list(title = "Year"),
                   yaxis = list(title = "Happiness Score"), margin = m,
                   legend = list(bgcolor = "#F2F2F2",
                                 bordercolor = "darkblue",
                                 borderwidth = 2,
                                 title=list(text='<b> Trend </b>')))
        
        
    })
    
    output$multipleGDP <- renderPlotly({
        hisub <- happiness_index()
        plot_ly(data = hisub, x = ~factor(Year), y = ~GDP.per.capita, color = ~Country,
                type = "scatter", mode = "lines+markers") %>%
            layout(yaxis = list(rangemode = "tozero")) %>%
            layout(title = "Impact of GDP per capita",
                   xaxis = list(title = "Year"),
                   yaxis = list(title = "GDP per capita"), margin = m,
                   legend = list(bgcolor = "#F2F2F2",
                                 bordercolor = "darkblue",
                                 borderwidth = 2,
                                 title=list(text='<b> Trend </b>')))
        
        
    })
    
    output$multipleHLE <- renderPlotly({
        hisub <- happiness_index()
        plot_ly(data = hisub, x = ~factor(Year), y = ~Healthy.Life.Expectancy, color = ~Country,
                type = "scatter", mode = "lines+markers") %>%
            layout(yaxis = list(rangemode = "tozero")) %>%
            layout(title = "Impact on Healthy Life Expectancy",
                   xaxis = list(title = "Year"),
                   yaxis = list(title = "Life Expectancy"), margin = m,
                   legend = list(bgcolor = "#F2F2F2",
                                 bordercolor = "darkblue",
                                 borderwidth = 2,
                                 title=list(text='<b> Trend </b>')))
        
    })
    
    output$multipleFLC <- renderPlotly({
        hisub <- happiness_index()
        plot_ly(data = hisub, x = ~factor(Year), y = ~Freedom.to.make.life.choices, color = ~Country,
                type = "scatter", mode = "lines+markers") %>%
            layout(yaxis = list(rangemode = "tozero")) %>%
            layout(title = "Impact on Freedom to make Life Choices",
                   xaxis = list(title = "Year"),
                   yaxis = list(title = "Freedom"), margin = m,
                   legend = list(bgcolor = "#F2F2F2",
                                 bordercolor = "darkblue",
                                 borderwidth = 2,
                                 title=list(text='<b> Trend </b>')))
        
            
        
    })
    
    output$multipleGen <- renderPlotly({
        hisub <- happiness_index()
        plot_ly(data = hisub, x = ~factor(Year), y = ~Generosity, color = ~Country,
                type = "scatter", mode = "lines+markers") %>%
            layout(yaxis = list(rangemode = "tozero")) %>%
            layout(title = "Impact on Generosity",
                   xaxis = list(title = "Year"),
                   yaxis = list(title = "Generosity"), margin = m,
                   legend = list(bgcolor = "#F2F2F2",
                                 bordercolor = "darkblue",
                                 borderwidth = 2,
                                 title=list(text='<b> Trend </b>')))
        
        
        
    })
    
    output$multipleSS <- renderPlotly({
        hisub <- happiness_index()
        plot_ly(data = hisub, x = ~factor(Year), y = ~Social.support, color = ~Country,
                type = "scatter", mode = "lines+markers") %>%
            layout(yaxis = list(rangemode = "tozero")) %>%
            layout(title = "Impact on Social Support",
                   xaxis = list(title = "Year"),
                   yaxis = list(title = "Social Support"), margin = m,
                   legend = list(bgcolor = "#F2F2F2",
                                 bordercolor = "darkblue",
                                 borderwidth = 2,
                                 title=list(text='<b> Trend </b>')))
    })
    
    output$indiaC <- renderPlotly({
        plot_ly(filterIndia(), x = ~as.Date(Date), y = ~Confirmed, 
                type = "scatter", mode = "lines", color = ~Country) %>%
            layout(title = "CoronaVirus Cases",
                   xaxis = list(title = "Date"),
                   yaxis = list(title = "Number of Patients"), margin = m,
                   legend = list(x = 0.05, y = 1, 
                                 bgcolor = "#F2F2F2",
                                 bordercolor = "darkblue",
                                 borderwidth = 2,
                                 title=list(text='<b> Trend </b>')))
    })
    
    output$indiaClog <- renderPlotly({
        plot_ly(filterIndia(), x = ~as.Date(Date), y = ~log(Confirmed), 
                type = "scatter", mode = "lines", color = ~Country) %>%
            layout(title = "CoronaVirus Cases",
                   xaxis = list(title = "Date"),
                   yaxis = list(title = "Number of Patients"), margin = m,
                   legend = list(x = 0.05, y = 1, 
                                 bgcolor = "#F2F2F2",
                                 bordercolor = "darkblue",
                                 borderwidth = 2,
                                 title=list(text='<b> Trend </b>')))
    })
    
    output$indiaCdiff <- renderPlotly({
        fig <- plot_ly()
        for (ci in levels(factor(filterIndia()$Country))){
            dt <- filterIndia()[filterIndia()$Country == ci,]
            fig <- add_trace(fig, data = dt, x = ~as.Date(Date), y = c(0, diff(dt$Confirmed)),
                             name = ci, type = "scatter", mode = "lines", 
                             opacity = 0.7)
        }
        fig %>%
            layout(title = "Daily CoronaVirus Cases",
                   xaxis = list(title = "Date"),
                   yaxis = list(title = "Number of Patients"), margin = m,
                   legend = list(x = 0.05, y = 1, 
                                 bgcolor = "#F2F2F2",
                                 bordercolor = "darkblue",
                                 borderwidth = 2,
                                 title=list(text='<b> Trend </b>')))
    })
    
    output$indiaRank <- renderPlotly({
        hisubIndia <- happiness_index_India()
        plot_ly(data = hisubIndia, x = ~factor(Year), y = ~Overall.Rank, color = ~Country,
                type = "scatter", mode = "lines+markers") %>%
            layout(yaxis = list(autorange = "reversed", rangemode = "tozero")) %>%
            layout(title = "Happiness Index Ranks",
                   xaxis = list(title = "Year"),
                   yaxis = list(title = "Overall Rank"), margin = m,
                   legend = list(bgcolor = "#F2F2F2",
                                 bordercolor = "darkblue",
                                 borderwidth = 2,
                                 title=list(text='<b> Trend </b>')))
    })
    
    output$indiaScore <- renderPlotly({
        hisubIndia <- happiness_index_India()
        plot_ly(data = hisubIndia, x = ~factor(Year), y = ~Score, color = ~Country,
                type = "scatter", mode = "lines+markers") %>%
            layout(yaxis = list(rangemode = "tozero")) %>%
            layout(title = "Happiness Index Score",
                   xaxis = list(title = "Year"),
                   yaxis = list(title = "Happiness Score"), margin = m,
                   legend = list(bgcolor = "#F2F2F2",
                                 bordercolor = "darkblue",
                                 borderwidth = 2,
                                 title=list(text='<b> Trend </b>')))
    })
    
    output$indiaGDP <- renderPlotly({
        hisubIndia <- happiness_index_India()
        plot_ly(data = hisubIndia, x = ~factor(Year), y = ~GDP.per.capita, color = ~Country,
                type = "scatter", mode = "lines+markers") %>%
            layout(yaxis = list(rangemode = "tozero")) %>%
            layout(title = "Impact of GDP per capita",
                   xaxis = list(title = "Year"),
                   yaxis = list(title = "GDP per capita"), margin = m,
                   legend = list(bgcolor = "#F2F2F2",
                                 bordercolor = "darkblue",
                                 borderwidth = 2,
                                 title=list(text='<b> Trend </b>')))
    })
    
    output$indiaHLE <- renderPlotly({
        hisubIndia <- happiness_index_India()
        plot_ly(data = hisubIndia, x = ~factor(Year), y = ~Healthy.Life.Expectancy, color = ~Country,
                type = "scatter", mode = "lines+markers") %>%
            layout(yaxis = list(rangemode = "tozero")) %>%
            layout(title = "Impact on Healthy Life Expectancy",
                   xaxis = list(title = "Year"),
                   yaxis = list(title = "Life Expectancy"), margin = m,
                   legend = list(bgcolor = "#F2F2F2",
                                 bordercolor = "darkblue",
                                 borderwidth = 2,
                                 title=list(text='<b> Trend </b>')))
    })
    
    output$indiaFLC <- renderPlotly({
        hisubIndia <- happiness_index_India()
        plot_ly(data = hisubIndia, x = ~factor(Year), y = ~Freedom.to.make.life.choices, color = ~Country,
                type = "scatter", mode = "lines+markers") %>%
            layout(yaxis = list(rangemode = "tozero")) %>%
            layout(title = "Impact on Freedom to make Life Choices",
                   xaxis = list(title = "Year"),
                   yaxis = list(title = "Freedom"), margin = m,
                   legend = list(bgcolor = "#F2F2F2",
                                 bordercolor = "darkblue",
                                 borderwidth = 2,
                                 title=list(text='<b> Trend </b>')))
    })
    
    output$indiaGen <- renderPlotly({
        hisubIndia <- happiness_index_India()
        plot_ly(data = hisubIndia, x = ~factor(Year), y = ~Generosity, color = ~Country,
                type = "scatter", mode = "lines+markers") %>%
            layout(yaxis = list(rangemode = "tozero")) %>%
            layout(title = "Impact on Generosity",
                   xaxis = list(title = "Year"),
                   yaxis = list(title = "Generosity"), margin = m,
                   legend = list(bgcolor = "#F2F2F2",
                                 bordercolor = "darkblue",
                                 borderwidth = 2,
                                 title=list(text='<b> Trend </b>')))
    })
    
    output$indiaSS <- renderPlotly({
        hisubIndia <- happiness_index_India()
        plot_ly(data = hisubIndia, x = ~factor(Year), y = ~Social.support, color = ~Country,
                type = "scatter", mode = "lines+markers") %>%
            layout(yaxis = list(rangemode = "tozero")) %>%
            layout(title = "Impact on Social Support",
                   xaxis = list(title = "Year"),
                   yaxis = list(title = "Social Support"), margin = m,
                   legend = list(bgcolor = "#F2F2F2",
                                 bordercolor = "darkblue",
                                 borderwidth = 2,
                                 title=list(text='<b> Trend </b>')))
    })
    
    
})
