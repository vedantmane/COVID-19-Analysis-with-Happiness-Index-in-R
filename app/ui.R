
library(shiny)
library(shinythemes)
library(plotly)

load("./data/country.RDS")

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
                         a(p("View Presentation"), href="https://rpubs.com/vedantmane/swiftkeyPres", target = "_blank"),
                         a(p("Click here to jump to the Github Repository."), href="https://github.com/vedantmane/COVID-19-Analysis-with-Happiness-Index-in-R", target = "_blank")
                     ),
                     mainPanel(
                         
                     )
                 )
        ),
        tabPanel("WorldWide Data Analysis",
                 br(),
                 h3(strong("CoronaVirus Cases around the World")),
                 hr(),
                 div(plotlyOutput(outputId = "casesWorld", width = "80%", height = "100%"), align = "center"),
                 br(),
                 h3(strong("Impact of CoronaVirus around the Globe")),
                 hr(),
                 div(plotlyOutput(outputId = "mapWorld", width = "80%", height = "500px"), align = "center", width = "100%"),
                 br(),
                 h3(strong("Daily Change in Number of CoronaVirus Cases arould the World")),
                 hr(),
                 div(plotlyOutput(outputId = "casesWorldDaily", width = "80%", height = "60%"), align = "center"),
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
                                     selected = "India", choices = country, 
                                     width = "200", multiple = TRUE)
                     ),
                     mainPanel(
                     )
                 )
        ),
        tabPanel("Happiness Index"),
        tabPanel("India CoronaVirus Analysis")
    ),
    br(),
    hr(),
    h4("Vedant Mane :-)", 
       br(),
       a(p("Click here to jump to the Github Repository."), href="https://github.com/vedantmane/COVID-19-Analysis-with-Happiness-Index-in-R", target = "_blank")
    ))
)
