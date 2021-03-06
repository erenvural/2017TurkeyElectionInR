first.tab.ui <- fluidPage(titlePanel("Uygulanan Filtreler Ile Tablo Olarak Se�im Sonu�lari"),
                       div(style="display: inline-block;vertical-align:top; width: 250px;", selectInput("variable", "Bir B�lge Se�iniz...",list(`Regions` = d))),
                       div(style="display: inline-block;vertical-align:top; width: 250px;", selectInput("variable2", "Bir Sehir Se�iniz...",list(`Cities` = cities))),
                       div(style="display: inline-block;vertical-align:top; width: 250px;", selectInput("variable3", "Bir Sonu� Se�iniz...", c("Yes","No"))),
                       tableOutput("data"), tableOutput("data2"),tableOutput("data3")
)

second.tab.ui <- fluidPage(
  titlePanel("Grafikler"),
  mainPanel(selectizeInput("variable4", "Bir B�lge Se�iniz...", d),
            plotOutput("plot1"),
            HTML("<br>"),
            HTML("<br>"),
            HTML("<br>"),
            HTML("<br>"),
            HTML("<br>"),
            selectizeInput("variable5", "Bir Sehir Se�iniz...", cities),
            plotOutput("plot4"),
            plotOutput("plot2"),plotOutput("plot3")))


#take_turkey_map = get_map(location = c(lon =  35.24332, lat = 38.96375), 
 #                         zoom = 5, maptype = 'hybrid', source = "google")
third.tab.ui <- fluidPage(
  selectizeInput("variable6", "Bir B�lge Se�iniz...", d),
  plotOutput(
    outputId = "mapOut3",
    width = 800, height = 500
  ),
  plotOutput(
    outputId = "mapOut4",
    width = 800, height = 500
  ),
  plotOutput(
    outputId = "mapOut1",
    width = 800, height = 500
  ) ,plotOutput(
    outputId = "mapOut2",
    width = 800, height = 500
  )
)

forth.tab.ui <- fluidPage(
  plotOutput(outputId="mapOut",
             width=800, height=500,  
             click = "plot_click"
  ),
  verbatimTextOutput(outputId="info"),
  verbatimTextOutput(outputId="coords")
)

ui = fluidPage(titlePanel("Se�im Sonu�lari"),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Tablolar", first.tab.ui),
                   tabPanel("Grafikler", second.tab.ui),
                   tabPanel("Haritalar", third.tab.ui),
                   tabPanel("B�t�n Sonu�lar", forth.tab.ui)
                 )
               )
)
server = function(input, output) {
  output$data <- renderTable({
    colnames(election.table) <- c(" ","Sehir", "Kayitli Se�men Sayisi", "Oy Kullanan Se�men Sayisi", "Ge�erli Oy Sayisi",
                                  "Ge�ersiz Oy Sayisi", "Evet Oylarinin Sayisi", "Evet Oy Orani", "Hayir Oylarinin Sayisi",
                                  "Hayir Oy Orani"," ", " ", " ", " ", "Bolge", " ", " ")
    election.table[election.table$Bolge %in% input$variable, c(2:10), drop = FALSE]
  }, rownames = FALSE)
  output$data2 <- renderTable({
    colnames(province_district_frame) <- c(" ", "Sehir", "Il�e", "Evet Oy Orani", "Hayir Oy Orani", "Sonu�")
    province_district_frame[province_district_frame$Sehir %in% input$variable2, c(2:5), drop = FALSE]
  }, rownames = FALSE)
  output$data3 <- renderTable({
    colnames(election.table) <- c(" ","Sehir", "Kayitli Se�men Sayisi", "Oy Kullanan Se�men Sayisi", "Ge�erli Oy Sayisi",
                                  "Ge�ersiz Oy Sayisi", "Evet Oylarinin Sayisi", "Evet Oy Orani", "Hayir Oylarinin Sayisi",
                                  "Hayir Oy Orani"," ", " ", " ", "Sonuc", "Bolge", " ", " ")
    election.table[election.table$Sonuc %in% input$variable3, c(2:10), drop = FALSE]
  }, rownames = FALSE
  )
  
  showCityResultByRegionInGGPlot <- reactive({
    Region <- input$variable4
    region_frame <- election.table[election.table$Bolge == Region, ]
    region_plot <- ggplot(data=region_frame, aes(x=Evet.Orani, y=Hayir.Orani, colour = Sonuc))
    region_plot <- region_plot + geom_point() + geom_label(aes(label=Sehir))+
      labs(x="Illere G�re Hayir Oranlari",y="Illere G�re Evet Oranlari", color= "Sonuclar: ")+
      ggtitle("Bir B�lgedeki Sehirler I�in Sonu�lar")
    print(region_plot)
  })
  
  output$plot1 <- renderPlot({
    showCityResultByRegionInGGPlot()
  }, height = 500, width = 750)
  
  showDistrictResultInGGPlot <- reactive({
    City <- input$variable5
    city_frame <- province_district_frame[province_district_frame$Sehir == City, ]
    district_plot <- ggplot(data=city_frame, aes(x=Evet.Orani, y=Hayir.Orani, colour = Sonuc))
    district_plot + geom_point() + geom_label(aes(label=Ilce))+
      labs(x="Il�elere G�re Hayir Oranlari",y="Il�elere G�re Evet Oranlari", color= "Sonuclar: ")+
      ggtitle("Bir Ildeki T�m Il�eler I�in Sonu�lar")
  })
  
  output$plot4 <- renderPlot({
    showDistrictResultInGGPlot()  
  }, height = 500, width = 750)
  
  output$plot2<-renderPlot({
    ggplot(data=election.table, aes(x=Evet.Orani, y=Hayir.Orani, colour = Sonuc))+
      geom_point()+
      geom_label(aes(label=Sehir))+
      labs(x="Illere G�re Hayir Oranlari",y="Illere G�re Evet Oranlari", color= "Sonuclar: ")+
      ggtitle("T�m Sehirler I�in Sonu�lar")
    }, height = 500, width = 750)
  
  output$plot3<-renderPlot({
    big_cities_frame <- election.table[election.table$Oy.Veren.Insan.Sayisi >= 1000000, ]
    big_cities_plot <- ggplot(data=big_cities_frame, aes(x=Evet.Orani, y=Hayir.Orani, colour = Sonuc))
    big_cities_plot + geom_point() + geom_label(aes(label=Sehir))+
      labs(x="Illere G�re Hayir Oranlari",y="Illere G�re Evet Oranlari", color= "Sonuclar: ")+
      ggtitle("B�y�k Sehirler I�in Sonu�lar")
  }, height = 500, width = 750)
  
  take_turkey_map = get_map(location = c(lon =  35.24332, lat = 38.96375), 
                            zoom = 5, maptype = 'hybrid', source = "google")
  turkey_map = ggmap(take_turkey_map)
  
  output$mapOut1 <- renderPlot({
    turkey_map + geom_point(aes(x = lon, y = lat, colour = Sonuc), data = election.table,
                            alpha = .5, size = 5)+
      labs(x="Koordinatlar",y="Koordinatlar", color= "Sonuclar: ")+
      ggtitle("B�t�n Sehirler I�in Sonu�lar")
    
  })
  
  output$mapOut2 <- renderPlot({
    turkey_map + geom_point(aes(x = lon, y = lat, colour = Sonuc), data = region_result_frame,
                            alpha = .5, size = 10) + geom_text(aes(x = lon, y = lat, label = Bolge),
                                                               data = region_result_frame, colour = I("white"))+
      labs(x="Koordinatlar",y="Koordinatlar", color= "Sonuclar: ")+
      ggtitle("B�lgeler I�in Sonu�lar")
      
  })
  
  output$mapOut4 <- renderPlot({
    # Hayir Kullanan Kitle I�in Renk Kodlari
    ccolors = c("#ff0000","#ffcccc","#ff6666","#b30000")
    
    turkey_map + geom_point(aes(x = lon, y = lat, colour = Hayir.Kullanan.Kitle), data = election.table,
                            alpha = .5, size = 5) + scale_colour_manual(values = ccolors)+
      labs(x="Koordinatlar",y="Koordinatlar", color= "Sonuclar: ")+
      ggtitle("B�t�n Sehirler I�in Sonu�lar")
  })
  
  showCitiesInRegionInGGMAP <- reactive({
    region <- input$variable6
    temp_region_frame <- election.table[election.table$Bolge == region, ]
    data_lenght <- nrow(temp_region_frame)
    temp_coordinates_dataframe <- data.frame()
    for (val in  c(1:data_lenght)){
      address <- temp_region_frame[val,1]
      address <- as.character(address)
      try(
        geocode(address)
      )
      temp_coordinates_dataframe <- rbind(temp_coordinates_dataframe, geocode(address))
    }
    temp_region_frame <- cbind(temp_region_frame,temp_coordinates_dataframe)
    
    mean_lon <- mean(temp_region_frame[["lon"]])
    mean_lat <- mean(temp_region_frame[["lat"]])
    take_region_map = get_map(location = c(lon =  mean_lon, lat = mean_lat), 
                              zoom = 6, maptype = 'hybrid', source = "google")
    region_map = ggmap(take_region_map)
    region_map + geom_point(aes(x = lon, y = lat, colour = Sonuc), data = temp_region_frame,
                            alpha = .5, size = 7) + geom_text(aes(x = lon, y = lat, label = Sehir),
                                                              data = temp_region_frame, colour = I("white"))+
      labs(x="Koordinatlar",y="Koordinatlar", color= "Sonuclar: ")+
      ggtitle("B�lgedeki B�t�n Sehirler I�in Sonu�lar")
    
  })
  output$mapOut3 <- renderPlot({
    showCitiesInRegionInGGMAP()
  })
  
  output$mapOut <- renderPlot({
    mapPoints <-  ggmap(take_turkey_map, extent = "normal") + 
      geom_point(aes(x = lon, y = lat, colour = Sonuc), data = election.table,
                 alpha = .5, size = 5)
    mapPoints
  })
  
  output$info <- renderPrint({
    ptClicked <- nearPoints(election.table, coordinfo = input$plot_click, 
                            threshold = 10, maxpoints = 1, 
                            xvar="lon", yvar="lat");
    data.frame(election.table[election.table$lon == ptClicked$lon, ]);
    
  })
  
  output$coords <- renderText({
    paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
  })

}
shinyApp(ui=ui, server = server)

