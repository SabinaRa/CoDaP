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



shinyServer(function(input, output, session) {
 
  #Countries
   
    filteredcountry <- reactive({

        statadata %>% select(CountryofLiving) %>% dplyr::filter(CountryofLiving == input$country)  %>% add_tally() %>% select(n) %>% distinct(n) 
    })
    
    
    output$numberparticipants <- renderValueBox({
      if(filteredcountry()<200) { 
      valueBox(filteredcountry(),"Number of International Survey on Coronavirus participants is less than 200. Please, interpret survey results carefully!", icon = icon("user-alt"), color="red") }
      else({
        valueBox(filteredcountry(),"Number of International Survey on Coronavirus participants", icon = icon("user-alt"), color="blue")
      })
    })
    

    
    filtereddatefirst <- reactive({
      statadata %>% dplyr::filter(CountryofLiving == input$country) %>% select(StartDate) %>% dplyr::summarise_all(funs(min)) %>% mutate(StartDate = format(as.Date(StartDate), "%d.%m.%Y"))
      
         })
    
    output$datefirst <- renderValueBox({
        valueBox(filtereddatefirst(),"Date of first response", icon = icon("calendar"), color="orange")
    })
    

    
    filtereddatelast <- reactive({
      statadata %>% dplyr::filter(CountryofLiving == input$country) %>% select(StartDate)  %>% dplyr::summarise_all(funs(max)) %>% mutate(StartDate = format(as.Date(StartDate), "%d.%m.%Y"))
    })
    
    output$datelast <- renderValueBox({
      valueBox(filtereddatelast(),"Date of last response", icon = icon("calendar"), color="orange")
      
    })
    
  #Continents  
    
    filteredcontinent <- reactive({
      
      statadata %>% select(continent) %>% dplyr::filter(continent == input$continent)  %>% add_tally() %>% select(n) %>% distinct(n)
    })
    
    output$numberparticipantscon <- renderValueBox({
      if(filteredcontinent()<200) { 
        valueBox(filteredcontinent(),"Number of International Survey on Coronavirus participants is less than 200. Please, interpret survey results carefully!", icon = icon("user-alt"), color="red") }
      else({
        valueBox(filteredcontinent(),"Number of International Survey on Coronavirus participants", icon = icon("user-alt"), color="navy")
      })
    })
    
    filtereddatefirstcon <- reactive({
      statadata %>% dplyr::filter(continent == input$continent) %>% select(StartDate) %>% dplyr::summarise_all(funs(min)) %>% mutate(StartDate = format(as.Date(StartDate), "%d.%m.%Y"))
      
    })
    
    output$datefirstcon <- renderValueBox({
      valueBox(filtereddatefirstcon(),"Date of first response", icon = icon("calendar"), color="orange")
    })
    
    
    filtereddatelastcon <- reactive({
      statadata %>% dplyr::filter(continent == input$continent) %>% select(StartDate)  %>% dplyr::summarise_all(funs(max)) %>% mutate(StartDate = format(as.Date(StartDate), "%d.%m.%Y"))
    })
    
    output$datelastcon <- renderValueBox({
      valueBox(filtereddatelastcon(),"Date of last response", icon = icon("calendar"), color="orange")
      
    })
    
    
    populationofcountry2018 <- reactive({
      data %>% select(countriesAndTerritories,popData2018) %>% dplyr::filter(countriesAndTerritories == input$country) %>% select(popData2018) %>% distinct(popData2018) %>% prettyNum(big.mark = " ")
    })
    
    
    
    output$populationcountry2018 <- renderValueBox({
      valueBox(populationofcountry2018(),"County population (census 2018)", icon = icon("users"), color="blue")
      
    })
    
    

#    output$dataecdc <- renderDataTable({
#        DT::datatable(data)
#    })
    
#    output$countriesgeo <- renderDataTable({
#        DT::datatable(countries)
#    })
    
#    output$dtadata <- renderDataTable({
#        DT::datatable(statadata)
#    })
    
    output$pastbeh <- renderPlot({
      
      df <- statadata %>% select(CountryofLiving, beh_stayhome, beh_socgathering, beh_distance, beh_tellsymp,beh_handwash) %>% dplyr::filter(CountryofLiving == input$country) %>% group_by(CountryofLiving)
      df1<- df %>% dplyr::summarise_all(mean) %>% select(beh_stayhome, beh_socgathering, beh_distance, beh_tellsymp,beh_handwash) 
      q <- c("I stayed at home.", "I did not attend social gatherings", "I kept a distance of at least two meters to other people.", "If I had exhibited symptoms of sickness, I would have immediately informed the people around me.", "I washed my hands more frequently than the month before.")
      var <- t(df1)
      df3 <- data.frame(var,q) 
      df4 <- df3 %>% select(var,q)
      
      ggplot(data=df4, aes(stringr::str_wrap(q, 30), var)) +
      geom_bar(stat="identity", fill="steelblue") + xlab("Questions") + ylab("Responses (mean)") +coord_flip() + theme_minimal() + theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12) , plot.title =element_text(size=18, face='bold')) + ggtitle("To what extent do the following statements describe \nyour behavior for the past week?") 
      
    })
    
  
    
    output$futurebeh <- renderPlot({
      
      
      df <- statadata %>% select(CountryofLiving,leavehome) %>% dplyr::filter(CountryofLiving == input$country) %>% group_by(leavehome) %>% dplyr::summarise (n = n()) %>% mutate(per = (n / sum(n)*100))
      
      res <- c("No","Yes")
      df1 <- data.frame(df,res)
      ggplot(data=df1, aes(per,res)) +
        geom_bar(stat="identity", fill="steelblue") + xlab("Percentage") + ylab("Responses") + xlim(0, 100) + theme_minimal() + theme(axis.text.y = element_text(size = 20), axis.text.x = element_text(size = 12),  plot.title =element_text(size=18, face='bold')) + ggtitle("Do you need to leave your home in the next 5 days?") 
      
    })
    
    

    
    output$govresponse <- renderPlot({
      
      df <- statadata %>% select(CountryofLiving,perceivedreaction) %>% dplyr::filter(CountryofLiving == input$country & perceivedreaction <=5 ) %>% group_by(perceivedreaction) %>% dplyr::summarise(n = n()) %>% mutate(per = (n / sum(n)*100))
      
      res <- c("The reaction is much too extreme","The reaction is somewhat too extreme","The reaction is appropriate", "The reaction is somewhat insufficient","The reaction is not at all sufficient")
      num <- c(1,2,3,4,5)
      res <- data.frame(num,res)
      
      df1 <- df %>% full_join(res, by=c("perceivedreaction"="num"))
      
      df1[is.na(df1)] <- 0
      df1$res <- factor(df1$res, levels = c("The reaction is much too extreme","The reaction is somewhat too extreme","The reaction is appropriate", "The reaction is somewhat insufficient","The reaction is not at all sufficient"))
      
      ggplot(data=df1, aes(per,res)) +
      geom_bar(stat="identity", fill="steelblue") + xlab("Percentage") + xlim(0, 100) + ylab("Responses")  + theme_minimal() + theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),  plot.title= element_text(size=18, face='bold')) + ggtitle("Do you think the reaction of your country\'s government \nto the current coronavirus outbreak is appropriate, \ntoo extreme, or not sufficient?")  
      
    })
    
    
    output$worries <- renderPlot({
      
      df <- statadata %>% select(CountryofLiving,mh_anxiety_1) %>% dplyr::filter(CountryofLiving == input$country) %>% group_by(mh_anxiety_1) %>% dplyr::summarise(n = n()) %>% mutate(per = (n / sum(n)*100))
      
      res <- c("Does not apply at all","Somewhat does not apply","Neither applies nor does not apply", "Somewhat applies","Strongly applies")
      num <- c(1,2,3,4,5)
      res <- data.frame(num,res)
      
      df1 <- df %>% full_join(res, by=c("mh_anxiety_1"="num"))
      
      df1[is.na(df1)] <- 0
     
      df1$res <- factor(df1$res, levels = c("Does not apply at all","Somewhat does not apply","Neither applies nor does not apply", "Somewhat applies","Strongly applies"))
    
      ggplot(data=df1, aes(per,res)) +
        geom_bar(stat="identity", fill="steelblue") + xlab("Percentage") + xlim(0, 100) + ylab("Responses")  + theme_minimal() + theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),  plot.title =element_text(size=18, face='bold')) + ggtitle("I am nervous when I think about current circumstances.")
      
    })
    
    output$pastbehcon <- renderPlot({
      
      df <- statadata %>% select(continent, beh_stayhome, beh_socgathering, beh_distance, beh_tellsymp,beh_handwash) %>% dplyr::filter(continent == input$continent) %>% group_by(continent)
      df1<- df %>% dplyr::summarise_all(mean) %>% select(beh_stayhome, beh_socgathering, beh_distance, beh_tellsymp,beh_handwash) 
      q <- c("I stayed at home.", "I did not attend social gatherings", "I kept a distance of at least two meters to other people.", "If I had exhibited symptoms of sickness, I would have immediately informed the people around me.", "I washed my hands more frequently than the month before.")
      var <- t(df1)
      df3 <- data.frame(var,q) 
      df4 <- df3 %>% select(var,q)
      
      ggplot(data=df4, aes(stringr::str_wrap(q, 30), var)) +
        geom_bar(stat="identity", fill="steelblue") + xlab("Questions") + ylab("Responses (mean)") +coord_flip() + theme_minimal() + theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12) , plot.title =element_text(size=18, face='bold')) + ggtitle("To what extent do the following statements describe \nyour behavior for the past week?") 
      
    })
    
    
    output$futurebehcon <- renderPlot({
      
      
      df <- statadata %>% select(continent,leavehome) %>% dplyr::filter(continent == input$continent) %>% group_by(leavehome) %>% dplyr::summarise (n = n()) %>% mutate(per = (n / sum(n)*100))
      
      res <- c("No","Yes")
      df1 <- data.frame(df,res)
      ggplot(data=df1, aes(per,res)) +
        geom_bar(stat="identity", fill="steelblue") + xlab("Percentage") + ylab("Responses") + xlim(0, 100) + theme_minimal() + theme(axis.text.y = element_text(size = 20), axis.text.x = element_text(size = 12),  plot.title =element_text(size=18, face='bold')) + ggtitle("Do you need to leave your home in the next 5 days?") 
      
    })
    
    
    output$govresponsecon <- renderPlot({
      
      df <- statadata %>% select(continent,perceivedreaction) %>% dplyr::filter(continent == input$continent & perceivedreaction <=5 ) %>% group_by(perceivedreaction) %>% dplyr::summarise(n = n()) %>% mutate(per = (n / sum(n)*100))
      
      res <- c("The reaction is much too extreme","The reaction is somewhat too extreme","The reaction is appropriate", "The reaction is somewhat insufficient","The reaction is not at all sufficient")
      num <- c(1,2,3,4,5)
      res <- data.frame(num,res)
      
      df1 <- df %>% full_join(res, by=c("perceivedreaction"="num"))
      
      df1[is.na(df1)] <- 0
      df1$res <- factor(df1$res, levels = c("The reaction is much too extreme","The reaction is somewhat too extreme","The reaction is appropriate", "The reaction is somewhat insufficient","The reaction is not at all sufficient"))
      
      ggplot(data=df1, aes(per,res)) +
        geom_bar(stat="identity", fill="steelblue") + xlab("Percentage") + xlim(0, 100) + ylab("Responses")  + theme_minimal() + theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),  plot.title= element_text(size=18, face='bold')) + ggtitle("Do you think the reaction of your country\'s government \nto the current coronavirus outbreak is appropriate, \ntoo extreme, or not sufficient?") 
      
    })
    
    
    
    output$worriescon <- renderPlot({
      
      df <- statadata %>% select(continent,mh_anxiety_1) %>% dplyr::filter(continent == input$continent) %>% group_by(mh_anxiety_1) %>% dplyr::summarise(n = n()) %>% mutate(per = (n / sum(n)*100))
      
      res <- c("Does not apply at all","Somewhat does not apply","Neither applies nor does not apply", "Somewhat applies","Strongly applies")
      num <- c(1,2,3,4,5)
      res <- data.frame(num,res)
      
      df1 <- df %>% full_join(res, by=c("mh_anxiety_1"="num"))
      
      df1[is.na(df1)] <- 0
      
      df1$res <- factor(df1$res, levels = c("Does not apply at all","Somewhat does not apply","Neither applies nor does not apply", "Somewhat applies","Strongly applies"))
      #  stringr::str_wrap(res, 30)
      ggplot(data=df1, aes(per,res)) +
        geom_bar(stat="identity", fill="steelblue") + xlab("Percentage") + xlim(0, 100) + ylab("Responses")  + theme_minimal() + theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),  plot.title =element_text(size=18, face='bold')) + ggtitle("I am nervous when I think about current circumstances.")
      
    })
    
    
    
    output$covidcasescum <- renderPlot({
      

      surveyfirstdate <- statadata %>% dplyr::filter(CountryofLiving == input$country) %>% select(StartDate)  %>% dplyr::summarise_all(funs(min))
      surveylastdate <- statadata %>% dplyr::filter(CountryofLiving == input$country) %>% select(StartDate)  %>% dplyr::summarise_all(funs(max))
      
      
      datacountrycov <- data %>% select(dateRep, countriesAndTerritories, cases, deaths) %>% arrange(dateRep) %>% dplyr::filter(countriesAndTerritories == input$country)  %>% mutate(casecum = cumsum(cases)) %>% mutate(deathscum = cumsum(deaths)) %>% select(dateRep,casecum,deathscum)
      datacountrycov_reshaped <- reshape2::melt(datacountrycov, id.var='dateRep')
      
      
      
      pl <- ggplot(datacountrycov_reshaped, aes(x=dateRep,y=value, col=variable))   +  geom_line(size=1.2)  + scale_x_datetime(date_breaks = "2 weeks", labels = date_format("%d.%m.%Y")) + theme_few() + xlab("Date") +ylab("Number") + labs(colour = NULL, subtitle = "Yellow block represents the period when the survey was conducted.") +
        scale_color_manual(labels = c("Cases", "Deaths"), values = c("#1C1C1C", "#BB000E")) + theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) + scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE))
      pl  + annotate("rect", xmin = as.POSIXct(as.Date(surveyfirstdate$StartDate)), xmax = as.POSIXct(as.Date(surveylastdate$StartDate)), ymin = 0, ymax = max(datacountrycov$casecum), alpha = .2, fill= "orange")
      
      
    })
    
    
    output$covidcasescumcon <- renderPlot({
      
      
      surveyfirstdate <- statadata %>% dplyr::filter(continent == input$continent) %>% select(StartDate)  %>% dplyr::summarise_all(funs(min))
      surveylastdate <- statadata %>% dplyr::filter(continent == input$continent) %>% select(StartDate)  %>% dplyr::summarise_all(funs(max))
      
      datacontinentcov <- data %>% select(dateRep, continentExp, cases, deaths) %>% arrange(dateRep, continentExp) %>% dplyr::filter(continentExp == input$continent) %>%group_by(dateRep) %>% summarise( sumcases= sum(cases), sumdeaths = sum(deaths)) %>% mutate(casecum = cumsum(sumcases)) %>% mutate(deathscum = cumsum(sumdeaths)) %>% select(dateRep,casecum,deathscum) 
      datacontinentcov_reshaped <- reshape2::melt(datacontinentcov , id.var='dateRep')
      
      
      
      pl <- ggplot(datacontinentcov_reshaped, aes(x=dateRep,y=value, col=variable))   +  geom_line(size=1.2)  + scale_x_datetime(date_breaks = "2 weeks", labels = date_format("%d.%m.%Y")) + theme_few() + xlab("Date") +ylab("Number") + labs(colour = NULL, subtitle = "Yellow block represents the period when the survey was conducted.") +
        scale_color_manual(labels = c("Cases", "Deaths"), values = c("#1C1C1C", "#BB000E")) + theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) + scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE))
      pl  + annotate("rect", xmin = as.POSIXct(as.Date(surveyfirstdate$StartDate)), xmax = as.POSIXct(as.Date(surveylastdate$StartDate)), ymin = 0, ymax = max(datacontinentcov$casecum), alpha = .2, fill= "orange")
      
      
    })
    
    output$mapcorona <- renderLeaflet({
      
      lnglatcountry <- countries %>% select(latitude, longitude, name) %>% dplyr::filter(name == input$country)
      
      leaflet() %>% setView(lng=lnglatcountry$longitude, lat=lnglatcountry$latitude, zoom=4) %>% addTiles()  %>% addMarkers(lng=lnglatcountry$longitude, lat=lnglatcountry$latitude, popup=lnglatcountry$name)
    })    
    
    output$mapcoronacon <- renderLeaflet({
      
      lnglatcontinent <- continents %>% select(latitude, longitude, name) %>% dplyr::filter(name == input$continent)
      
      lnglatcountry <- countries %>% select(latitude, longitude, name) 
      datacountrycovmapcon <- data %>% select(dateRep, countriesAndTerritories, cases, deaths) %>% group_by(countriesAndTerritories) %>% summarise(sumcases=sum(cases), sumdeaths =sum(deaths))
      
      datapercountrycorona <- datacountrycovmapcon %>% inner_join(lnglatcountry, by=c("countriesAndTerritories" = "name" ))
      
      leaflet() %>% setView(lng=lnglatcontinent$longitude, lat=lnglatcontinent$latitude, zoom=3) %>% addTiles() %>% addMinicharts(lng=datapercountrycorona$longitude, lat =datapercountrycorona$latitude,
                                                                                                                              chartdata = datapercountrycorona$sumcases,
                                                                                                                              showLabels = TRUE, 
                                                                                                                              width = 100) 
    })   
    
    
    
    output$nocasesPerPop <- renderValueBox({
      totalNoCases<- data %>% select(countriesAndTerritories,cases) %>% dplyr::filter(countriesAndTerritories==input$country) %>% dplyr::summarise_at("cases",sum)
      CountryPop <- data %>% select(countriesAndTerritories,popData2018) %>% dplyr::filter(countriesAndTerritories==input$country)
      filteredCasesPop <- round((totalNoCases/CountryPop$popData2018)*100000,0)
      
      valueBox(if(filteredCasesPop < 1){"< 1"} else {filteredCasesPop}, "Number of confirmed cases per 100.000 inhabitants (census 2018)", icon = icon("list", lib = "glyphicon"),
               color = "blue")
      
    })
    
    
    output$nocases <- renderValueBox({
      totalNoCases<- data %>% select(countriesAndTerritories,cases) %>% dplyr::filter(countriesAndTerritories==input$country) %>% dplyr::summarise_at("cases",sum) %>% prettyNum(big.mark = " ")
      valueBox(totalNoCases, "Number of confirmed cases", icon = icon("list", lib = "glyphicon"),
               color = "blue")
      
    })
    
    
    
    output$nodeathsPercases <- renderValueBox({
      totalNoDeaths <- data %>% select(countriesAndTerritories,deaths) %>% dplyr::filter(countriesAndTerritories==input$country) %>% dplyr::summarise_at("deaths",sum)
      totalNoCases <- data %>% select(countriesAndTerritories,cases) %>% dplyr::filter(countriesAndTerritories==input$country) %>% dplyr::summarise_at("cases",sum)
      
      filteredDeathsCases <- round(((totalNoDeaths/totalNoCases)*100),2)
      
      valueBox(if(filteredDeathsCases < 1) {"< 1 %"} else {paste0(filteredDeathsCases," %")}, "Percentage of deaths per total number of confirmed cases", icon = icon("list", lib = "glyphicon"),
               color = "black")
      
    })
    
    
    output$nocasescon <- renderValueBox({
      totalNoCases<- data %>% select(continentExp,cases) %>% dplyr::filter(continentExp==input$continent) %>% dplyr::summarise_at("cases",sum) %>% prettyNum(big.mark = " ")
     
      valueBox(totalNoCases, "Number of confirmed cases", icon = icon("list", lib = "glyphicon"),
               color = "navy")
      
    })
    
    
    populationofcontinent2018 <- reactive({
      data %>% select(continentExp,popData2018) %>% dplyr::filter(continentExp == input$continent) %>% dplyr::summarise_at("popData2018",sum, na.rm = TRUE) %>% select(popData2018) %>% prettyNum(big.mark = " ")
    })
    
    
    
    output$populationcontinent2018 <- renderValueBox({
      valueBox(populationofcontinent2018(),"Continent population (census 2018)", icon = icon("users"), color="navy")
      
    })
    
    
    output$nocasesPerPopcon <- renderValueBox({
      totalNoCases<- data %>% select(continentExp,cases) %>% dplyr::filter(continentExp==input$continent) %>% dplyr::summarise_at("cases",sum)
      ContinentPop <- data %>% select(continentExp, countriesAndTerritories, popData2018) %>% dplyr::filter(continentExp=="Europe") %>% distinct() %>% group_by(continentExp) %>% summarise(popData2018 = sum(popData2018))
      
      filteredCasesPop <- round((totalNoCases/ContinentPop$popData2018)*100000,0) 
      
      valueBox(if(filteredCasesPop < 1){"< 1"} else {filteredCasesPop}, "Number of confirmed cases per 100.000 inhabitants (census 2018)", icon = icon("list", lib = "glyphicon"),
               color = "navy")
      
    })
    
    
    output$nodeathsPercasescon <- renderValueBox({
      totalNoDeaths <- data %>% select(continentExp,deaths) %>% dplyr::filter(continentExp==input$continent) %>% dplyr::summarise_at("deaths",sum)
      totalNoCases <- data %>% select(continentExp,cases) %>% dplyr::filter(continentExp==input$continent) %>% dplyr::summarise_at("cases",sum)
      
      filteredDeathsCases <- round(((totalNoDeaths/totalNoCases)*100),2)
      
      valueBox(if(filteredDeathsCases < 1) {"< 1 %"} else {paste0(filteredDeathsCases," %")}, "Percentage of deaths per total number of confirmed cases", icon = icon("list", lib = "glyphicon"),
               color = "black")
       })
    
    
    output$treemapcountry <- renderPlot({
      treemap(bycountry,
              index="CountryofLiving",
              vSize="n",
              vColor = "n",
              type="index",
              palette="Blues",
              n=5,
              title = "Number of participants"
      )
    })
    
    output$treemapcontinent <- renderPlot({
      treemap(bycontinent,
              index="continentExp",
              vSize="n",
              vColor = "n",
              type="index",
              palette="Blues", 
              n=3,
              title = "Number of participants"
      )
    })
    
    

})

