library(htmltab)
library(ggplot2)
library(ggmap)
library(xml2)
library(rvest)
library(dplyr)
library(readxl)
library(plyr)
library(grid)
library(stringi)
library(xlsx)
library(shiny)


DIR <- 'C:\\Users\\lenovo\\Desktop\\displaying_turkey_election_2017_result_in_r-master\\'
setwd(DIR)

#Reading Table
election.table <- read_excel("C:\\Users\\lenovo\\Desktop\\displaying_turkey_election_2017_result_in_r-master\\election.xlsx")
province_district_frame <- read_excel("C:\\Users\\lenovo\\Desktop\\displaying_turkey_election_2017_result_in_r-master\\district.xlsx")
region_result_frame <- read_excel("C:\\Users\\lenovo\\Desktop\\displaying_turkey_election_2017_result_in_r-master\\region.xlsx")

city_plot <- ggplot(data=election.table, aes(x=Evet.Orani, y=Hayir.Orani, colour = Sonuc))
city_plot + geom_point() + geom_label(aes(label=Sehir)) + 
  ggtitle("                                                 Referandum Sonuçlari") +
  labs(x="Illere Göre Hayir Oranlari",y="Illere Göre Evet Oranlari") 



big_cities_frame <- election.table[election.table$Oy.Veren.Insan.Sayisi >= 1000000, ]
big_cities_plot <- ggplot(data=big_cities_frame, aes(x=Evet.Orani, y=Hayir.Orani, colour = Sonuc))
big_cities_plot + geom_point() + geom_label(aes(label=Sehir)) + 
  ggtitle("                                                 Referandum Sonuçlari") +
  labs(x="Illere Göre Hayir Oranlari",y="Illere Göre Evet Oranlari")



region_average_plot <- ggplot(data=region_result_frame, aes(x=Evet.Ortalamasi, y=Hayir.Ortalamasi, colour = Sonuc))
region_average_plot + geom_point() + geom_label(aes(label=Bolge))


showCityResultByRegionInGGPlot <- function(Region){
  
  region_frame <- election.table[election.table$Bolge == Region, ]
  region_plot <- ggplot(data=region_frame, aes(x=Evet.Orani, y=Hayir.Orani, colour = Sonuc))
  region_plot + geom_point() + geom_label(aes(label=Sehir))
  
}
showCityResultByRegionInGGPlot("Marmara Bölgesi")



showDistrictResultInGGPlot <- function(City){
  
  city_frame <- province_district_frame[province_district_frame$Sehir == City, ]
  district_plot <- ggplot(data=city_frame, aes(x=Evet.Orani, y=Hayir.Orani, colour = Sonuc))
  district_plot + geom_point() + geom_label(aes(label=Ilce))
  
}
showDistrictResultInGGPlot("Ankara") #Sample





take_turkey_map = get_map(location = c(lon =  35.24332, lat = 38.96375), 
                          zoom = 5, maptype = 'hybrid', source = "google")
turkey_map = ggmap(take_turkey_map)


turkey_map + geom_point(aes(x = lon, y = lat, colour = Sonuc), data = election.table,
                        alpha = .5, size = 5)


take_turkey_map = get_map(location = c(lon =  35.24332, lat = 38.96375), 
                          zoom = 5, maptype = 'hybrid', source = "google")
turkey_map = ggmap(take_turkey_map)
turkey_map + geom_point(aes(x = lon, y = lat, colour = Sonuc), data = region_result_frame,
                        alpha = .5, size = 20) + geom_text(aes(x = lon, y = lat, label = Bolge),
                                                           data = region_result_frame, colour = I("white"))+
  labs(colour= "Sonuçlar:")


showCitiesInRegionInGGMAP <- function(region){
  
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
                                                            data = temp_region_frame, colour = I("white"))
  
}
showCitiesInRegionInGGMAP("Marmara Bölgesi") #Sample

cities <- levels(factor(election.table$Sehir))  
cities <- as.list(cities)

unique_region_name <- subset(election.table, !duplicated(Bolge))
region_list<-c()
i=1
while(i <= nrow(unique_region_name)) {
  region <- unique_region_name[i,15]
  region_list<-c(region_list,region)
  i=i+1
}
region_list
d <- c()
for(i in region_list){
  d <- c(d,i)
}
d

unique_city_name <- subset(province_district_frame, !duplicated(Sehir))
city_list<-c()
i=1
while(i <= nrow(unique_city_name)) {
  city <- unique_city_name[i,1]
  city_list<-c(city_list,city)
  i=i+1
}
city_list

View(province_district_frame)


if (interactive()) {
  
  shinyApp(
    ui = fluidPage(titlePanel("Election Results in Tables according to Filtres"),
                   
                   div(style="display: inline-block;vertical-align:top; width: 250px;", selectInput("variable", "Select a region...",list(`Regions` = d))),
                   div(style="display: inline-block;vertical-align:top; width: 250px;", selectInput("variable2", "Select a city...",list(`Cities` = cities))),
                   div(style="display: inline-block;vertical-align:top; width: 250px;", selectInput("variable3", "Select a result...", c("Yes","No"))),
                   tableOutput("data"), tableOutput("data2"),tableOutput("data3")
                   
    ),
    server = function(input, output) {
      output$data <- renderTable({
        election.table[election.table$Bolge %in% input$variable, c(1:10), drop = FALSE]
      }, rownames = FALSE)
      output$data2 <- renderTable({
        province_district_frame[province_district_frame$Sehir %in% input$variable2, c(1:4), drop = FALSE]
      }, rownames = FALSE)
      output$data3 <- renderTable({
        election.table[election.table$Sonuc %in% input$variable3, c(1:10), drop = FALSE]
      }, rownames = FALSE)
    }
  )
}

## GGPlots on GUI ########
# A basic shiny app with a plotOutput

ui <- fluidPage(
  titlePanel("GGPlot Operations"),
  sidebarPanel(
    actionButton("newplot", "Cities Result Plot")),
  mainPanel(plotOutput("plot2")))

server <- function(input,output){
  output$plot2<-renderPlot({
    ggplot(data=election.table, aes(x=Evet.Orani, y=Hayir.Orani, colour = Sonuc))+geom_point()+geom_label(aes(label=Sehir))},height = 400,width = 600)}

ggplotApp1 <- shinyApp(ui, server)


## GGMAPS on GUI #########   


ui <- fluidPage (
  plotOutput(outputId="mapOut",
             width=800, height=500,  
             click = "plot_click"
  ),
  verbatimTextOutput(outputId="info"),
  verbatimTextOutput(outputId="coords")
)
server <- function(input,output) {
  
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
shinyApp(server = server, ui = ui)
