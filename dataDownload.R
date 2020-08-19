#Create Data Directory
if(!dir.exists("./data")){
    dir.create("./data/")
}

#Download Countries Aggregated Data
countriesAggURL <- "https://raw.githubusercontent.com/datasets/covid-19/master/data/countries-aggregated.csv"
download.file(countriesAggURL, "./data/countriesAggregated.csv")
countriesAgg <- read.csv("./data/countriesAggregated.csv")

#Download Worldwide Aggregated Data
worldwideAggURL <- "https://raw.githubusercontent.com/datasets/covid-19/master/data/worldwide-aggregated.csv"
download.file(worldwideAggURL, "./data/worldwideAggregated.csv")
worldwideAgg <- read.csv("./data/worldwideAggregated.csv")

#Time-series Covid-19 combined data
timeSeriesURL <- "https://raw.githubusercontent.com/datasets/covid-19/master/data/time-series-19-covid-combined.csv"
download.file(timeSeriesURL, "./data/timeSeriesCOVID19.csv")
timeSeriesCOVID19 <- read.csv("./data/timeSeriesCOVID19.csv")

#Reference Covid-19 combined data >> Subset INDIA
referenceURL <- "https://raw.githubusercontent.com/datasets/covid-19/master/data/reference.csv"
download.file(referenceURL, "./data/subsetINDIAStates.csv")
INDIA_States <- read.csv("./data/subsetINDIAStates.csv")
