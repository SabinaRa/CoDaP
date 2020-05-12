#
# CoDaP (Covid Data And Perception) is an application developed for CovidR contest 
# organized as a part of e-Rum 2020 conference https://2020.erum.io/covidr-contest/.
# 
# Data sources: 
#
# European Centre for Disease Prevention and Control.Data on the geographic distribution of COVID-19 cases worldwide. 
# https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide
# 
# Fetzer, T., Witte, M., Hensel, L., Jachimowicz, J.M., Haushofer, J., Ivchenko, A., 
# Caria, C., Reutskaja, E., Roth, C., Fiorin, F., Gomez, M., Kraft-Todd, G., Goetz, F., & Yoeli, E.. 
# Global Behaviors and Perceptions in the COVID-19 Pandemic. https://doi.org/10.31234/osf.io/3kfmh
# International Survey on Coronavirus, GlobalBehaviorsPerceptions_Dataset_Apr22_2020, https://osf.io/k5b6u/, https://covid19-survey.org/results.html 
#
# Dashboard created by: Sabina Rako, sabina.rako1[at]gmail.com
#


header <- dashboardHeader( 
            title = "CoDaP"
             )

sidebar <- dashboardSidebar( 
            sidebarMenu(
            menuItem("My country",tabName = "mycountry", icon = icon("flag")),
            menuItem("My continent",tabName = "mycontinent", icon = icon("globe")),
            menuItem("About", tabName = "about", icon = icon("info"))
             ))

body <- dashboardBody(
   
tabItems(
  
  tabItem(tabName = "mycountry",
  
    selectInput("country", "Select country:", choices=bycountry$CountryofLiving, selected="Italy"),
    helpText("Note: Only countries with more than 50 responses included."),
        
      
    fluidRow(
      column(width=6,        
        valueBoxOutput(width=12, "numberparticipants"),
        valueBoxOutput(width = 6,"datefirst"),
        valueBoxOutput(width = 6, "datelast"),

        tabBox( width=12,
          title = "Survery results",
          id = "tabsurvey", 
          tabPanel("Past behaviors", plotOutput("pastbeh") ),
          tabPanel("Future behaviors", plotOutput("futurebeh")),
          tabPanel("Perceptions of government response", plotOutput("govresponse")),
          tabPanel("Worries",plotOutput("worries"))
          )
        ),
    
     box(width=6, leafletOutput("mapcorona"), status = "primary", title="Map"),
     valueBoxOutput(width=6,"populationcountry2018")
        ),
    
     fluidRow(
      column(width=12,
             
             box(width=10, title="Cumulative COVID data",status = "primary", plotOutput("covidcasescum")),
             valueBoxOutput(width=2,"nocases"),
             valueBoxOutput(width=2,"nocasesPerPop"),
             valueBoxOutput(width=2,"nodeathsPercases")
             )
             ),
    fluidRow(
     box(width = 12,title = "Survey participants per country", status = "primary", plotOutput("treemapcountry"))
      )

  ),


tabItem(tabName = "mycontinent", 
selectInput("continent", "Select continent:", choices=bycontinent$continentExp, selected="Europe"),
fluidRow(
  column(width=6,        
         valueBoxOutput(width=12, "numberparticipantscon"),
         valueBoxOutput(width = 6,"datefirstcon"),
         valueBoxOutput(width = 6, "datelastcon"),
         tabBox(width = 12,
           title = "Survery results",
           id = "tabsurvey", 
           tabPanel("Past behaviors", plotOutput("pastbehcon")),
           tabPanel("Future behaviors", plotOutput("futurebehcon")),
           tabPanel("Perceptions of government response", plotOutput("govresponsecon")),
           tabPanel("Worries",plotOutput("worriescon"))
         )
  ),
 
box(width=6, leafletOutput("mapcoronacon"), status = "primary", title="Map with confirmed number of COVID-19 cases"),
valueBoxOutput(width=6,"populationcontinent2018")
  ),

fluidRow(
  column(width=12,
         
         box(width=10, title="Cumulative COVID data",status = "primary", plotOutput("covidcasescumcon")),
         valueBoxOutput(width=2,"nocasescon"),
         valueBoxOutput(width=2,"nocasesPerPopcon"),
         valueBoxOutput(width=2,"nodeathsPercasescon")
  )),
fluidRow(
  box(width = 12, title = "Survey participants per continent", status = "primary", plotOutput("treemapcontinent")),
)
),
tabItem(tabName = "about",   
        
        fluidRow(
        boxPlus(width= 12, title="Data sources",closable= FALSE, status="warning", solidHeader= FALSE, collapsible=TRUE,
                                     p("European Centre for Disease Prevention and Control.Data on the geographic distribution of COVID-19 cases worldwide.",
                                       tags$a(href="https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide", "https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide"),br(),br(),
                                       
                                       "Fetzer, T., Witte, M., Hensel, L., Jachimowicz, J.M., Haushofer, J., Ivchenko, A., 
                                        Caria, C., Reutskaja, E., Roth, C., Fiorin, F., Gomez, M., Kraft-Todd, G., Goetz, F., & Yoeli, E.. 
                                        Global Behaviors and Perceptions in the COVID-19 Pandemic. https://doi.org/10.31234/osf.io/3kfmh
                                        International Survey on Coronavirus, GlobalBehaviorsPerceptions_Dataset_Apr22_2020,", tags$a(href="https://osf.io/k5b6u/", "https://osf.io/k5b6u/"),",",tags$a(href="https://covid19-survey.org/results.html", "https://covid19-survey.org/results.html") 
                                     )),
        boxPlus(width=12, title="Data update", closable= FALSE, status="warning", solidHeader = FALSE, collapsible = TRUE, p("ECDC Covid-19 Data, last update: 11.5.2020",br(),"International Survey on Coronavirus, last update: 22.4.2020",br(), "Dashboard v1.1, last update 11.5.2020")),
        boxPlus(width=12, title="Contact", closable= FALSE, status="warning", solidHeader = FALSE, collapsible = TRUE, p("Dashboard autor: Sabina Rako, E-mail: sabina.rako1[at]gmail.com", br(), "Dashboard R code available at", tags$a(href="https://github.com/SabinaRa/CoDaP", "GitHub page")))
        
        )
        )

)
)


footer <-  dashboardFooter(
  left_text = "Our humanity rests upon a series of learned behaviors, woven together into patterns that are infinitely fragile and never directly inherited. --Margaret Mead",
  right_text = ""
)
  
dashboardPagePlus(skin = "blue", header, sidebar, body, footer)

