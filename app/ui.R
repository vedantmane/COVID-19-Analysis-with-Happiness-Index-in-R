
library(shiny)
library(shinythemes)
library(plotly)

load("./data/country.RDS")
load("./data/Happiness Report/hicountries.RDS")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    #shinythemes::themeSelector(),
    #theme = shinytheme("flatly")
    #theme = shinytheme("cosmo")
    #theme = shinytheme("yeti")
    #theme = shinytheme("superhero")
    #theme = shinytheme("slate")
    theme = shinytheme("flatly"),
    # Application title
    titlePanel("COVID-19 Data Analysis with Happniess Index"),
    br(),
    #Tabs
    navbarPage("CoronaVirus",
        tabPanel("Home",
                 sidebarLayout(
                     sidebarPanel(
                         h4(strong("Novel CoronaVirus Disease (COVID-19)")),
                         hr(),
                         a(p("View Presentation"), href="", target = "_blank"),
                         a(p("Click here to jump to the Github Repository."), href="https://github.com/vedantmane/COVID-19-Analysis-with-Happiness-Index-in-R", target = "_blank")
                     ),
                     mainPanel(
                         
                     )
                 )
        ),
        tabPanel("WorldWide Data Analysis",
                 h3(strong("Impact of CoronaVirus around the Globe")),
                 hr(),
                 div(plotlyOutput(outputId = "casesWorld", width = "80%", height = "500px"), align = "center", width = "100%"),
                 br(),
                 h3(strong("Daily Change in Number of CoronaVirus Cases arould the Globe")),
                 hr(),
                 div(plotlyOutput(outputId = "casesWorldDaily", width = "80%", height = "60%"), align = "center"),
                 br(),
                 h3(strong("Map of CoronaVirus Cases around the World")),
                 hr(),
                 div(plotlyOutput(outputId = "mapWorld", width = "80%", height = "100%"), align = "center"),
                 br(),
                 h3(strong("Impact based on GDP per capita")),
                 hr(),
                 div(plotlyOutput(outputId = "casesGDPpc", width = "100%", height = "60%"), align = "center"),
                 br(),
                 h3(strong("Impact based on Social Support")),
                 hr(),
                 div(plotlyOutput(outputId = "casesSS", width = "100%", height = "60%"), align = "center"),
                 br(),
                 h3(strong("Impact based on Healthy Life Expectancy")),
                 hr(),
                 div(plotlyOutput(outputId = "casesHLE", width = "100%", height = "60%"), align = "center"),
                 br(),
                 h3(strong("Impact based on Freedom of Life Choices")),
                 hr(),
                 div(plotlyOutput(outputId = "casesFLC", width = "100%", height = "60%"), align = "center")
        ),
        tabPanel("CoronaVirus Data Analysis",
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("countrychoice", label = "Country", 
                                     selected = c("India", "US", "Italy", "Brazil"), choices = country, 
                                     width = "100%", multiple = TRUE),
                         submitButton("Generate", width = "100%"),
                         width = 3
                     ),
                     mainPanel(
                         h3(strong("Map of CoronaVirus Impact")),
                         hr(),
                         div(plotlyOutput(outputId = "mapCountry", width = "100%", height = "60%"), align = "center"),
                         h3(strong("Impact on the Selected Country")),
                         hr(),
                         div(plotlyOutput(outputId = "multipleC", width = "100%", height = "60%"), align = "center"),
                         h3(strong("Log Transformation of Impact on the Selected Country")),
                         hr(),
                         div(plotlyOutput(outputId = "multipleClog", width = "100%", height = "60%"), align = "center"),
                         h3(strong("Daily Change in CoronaVirus Cases in Selected Country")),
                         hr(),
                         div(plotlyOutput(outputId = "multipleCdiff", width = "100%", height = "60%"), align = "center")
                     )
                     
                 )
        ),
        tabPanel("Happiness Index",
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("hicountrychoice", label = "Country", 
                                     selected = c("India", "United States", "Italy", "Brazil"), choices = hicountries, 
                                     width = "100%", multiple = TRUE),
                         submitButton("Generate", width = "100%"),
                         width = 3
                     ),
                     mainPanel(
                         h3(strong("Impact on Happiness Rank in Selected Country")),
                         hr(),
                         div(plotlyOutput(outputId = "multipleRank", width = "100%", height = "60%"), align = "center"),
                         
                         h3(strong("Impact on Happiness Score in Selected Country")),
                         hr(),
                         div(plotlyOutput(outputId = "multipleScore", width = "100%", height = "60%"), align = "center"),
                         
                         h3(strong("Impact on GDP per capita in Selected Country")),
                         hr(),
                         div(plotlyOutput(outputId = "multipleGDP", width = "100%", height = "60%"), align = "center"),
                         
                         h3(strong("Impact on Life Expectancy in Selected Country")),
                         hr(),
                         div(plotlyOutput(outputId = "multipleHLE", width = "100%", height = "60%"), align = "center"),
                         
                         h3(strong("Impact on Freedom in Selected Country")),
                         hr(),
                         div(plotlyOutput(outputId = "multipleFLC", width = "100%", height = "60%"), align = "center"),
                         
                         h3(strong("Impact on Generosity in Selected Country")),
                         hr(),
                         div(plotlyOutput(outputId = "multipleGen", width = "100%", height = "60%"), align = "center"),
                         
                         h3(strong("Impact on Social Support in Selected Country")),
                         hr(),
                         div(plotlyOutput(outputId = "multipleSS", width = "100%", height = "60%"), align = "center"),
                     )
                 )),
        tabPanel("India CoronaVirus Analysis",
                 h3(strong("Impact of CoronaVirus in India")),
                 hr(),
                 div(plotlyOutput(outputId = "indiaC", width = "80%", height = "60%"), align = "center"),
                 h3(strong("Log Transformation of impact in India")),
                 hr(),
                 div(plotlyOutput(outputId = "indiaClog", width = "80%", height = "60%"), align = "center"),
                 h3(strong("Daily Change in CoronaVirus Cases in India")),
                 hr(),
                 div(plotlyOutput(outputId = "indiaCdiff", width = "80%", height = "60%"), align = "center"),
                 
                 h3(strong("Impact on Happiness Rank in India")),
                 hr(),
                 div(plotlyOutput(outputId = "indiaRank", width = "80%", height = "60%"), align = "center"),
                 
                 h3(strong("Impact on Happiness Score in India")),
                 hr(),
                 div(plotlyOutput(outputId = "indiaScore", width = "80%", height = "60%"), align = "center"),
                 
                 h3(strong("Impact on GDP per capita in India")),
                 hr(),
                 div(plotlyOutput(outputId = "indiaGDP", width = "80%", height = "60%"), align = "center"),
                 
                 h3(strong("Impact on Life Expectancy in India")),
                 hr(),
                 div(plotlyOutput(outputId = "indiaHLE", width = "80%", height = "60%"), align = "center"),
                 
                 h3(strong("Impact on Freedom in India")),
                 hr(),
                 div(plotlyOutput(outputId = "indiaFLC", width = "80%", height = "60%"), align = "center"),
                 
                 h3(strong("Impact on Generosity in India")),
                 hr(),
                 div(plotlyOutput(outputId = "indiaGen", width = "80%", height = "60%"), align = "center"),
                 
                 h3(strong("Impact on Social Support in India")),
                 hr(),
                 div(plotlyOutput(outputId = "indiaSS", width = "80%", height = "60%"), align = "center"),
                 
        ),
        tabPanel("About")
    ),
    br(),
    hr(),
    h4("Vedant Mane :-)", 
       br(),
       a(p("Click here to jump to the Github Repository."), href="https://github.com/vedantmane/COVID-19-Analysis-with-Happiness-Index-in-R", target = "_blank")
    ))
)
