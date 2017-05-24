#Web Mining Final Project
#Eren Vural - Mahmut Ko√ßaker 

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
#Functions################################################
#1. Function for transform numbers to factor as levelling
valueIndicator <- function (x) {
  if (x < 25) {
    return("Low")
  } else if (x >= 25 & x < 50) {
    return("Normal")
  } else if (x >= 50 & x < 75) {
    return("High")
  } else if (x >= 75 & x < 100) {
    return("Very High")
  } 
}

#2. Function for find winner
winnerFinder <- function (value) {
  if (value > 50) {
    return("Yes")
  } else {
    return("No")
  } 
}

#########################################################

#Taking main table
url <- "https://tr.wikipedia.org/wiki/2017_T%C3%BCrkiye_anayasa_de%C4%9Fi%C5%9Fikli%C4%9Fi_referandumu#.C4.B0llere_g.C3.B6re_sonu.C3.A7lar"
election.table <- htmltab(doc = url, which = "//*[@id='mw-content-text']/table[12]", encoding="UTF-8")

#Changing Column Names
colnames(election.table) <- c("Sehir","Kayitli.Secmen","Oy.Veren.Insan.Sayisi", "Gecerli.Oy",
"Gecersiz.Oy", "Evet.Sayisi", "Evet.Orani", "Hayir.Sayisi","Hayir.Orani","Katilim.Orani")

#Fixing Encoding Problem
Encoding(election.table$Sehir) <- "UTF-8"

#Converting the variables into the correct formats and creating new columns by using exist columns
election.table[,1] <- as.factor(election.table[,1])
election.table[,2] <- gsub("\\.","",election.table[,2])
election.table[,2] <- as.numeric(election.table[,2])
election.table[,3] <- gsub("\\.","",election.table[,3])
election.table[,3] <- as.numeric(election.table[,3])
election.table[,4] <- gsub("\\.","",election.table[,4])
election.table[,4] <- as.numeric(election.table[,4])
election.table[,5] <- gsub("\\.","",election.table[,5])
election.table[,5] <- as.numeric(election.table[,5])

election.table[,6] <- gsub("\\.","",election.table[,6])
election.table[,6] <- as.numeric(election.table[,6])

election.table[,8] <- gsub("\\.","",election.table[,8])
election.table[,8] <- as.numeric(election.table[,8])

election.table[,7] <- gsub(",",".",election.table[,7])
election.table[,7] <- as.double(election.table[,7])
election.table[,9] <- gsub(",",".",election.table[,9])
election.table[,9] <- as.double(election.table[,9])
election.table[,10] <- gsub(",",".",election.table[,10])
election.table[,10] <- as.double(election.table[,10])

election.table[,13] <- as.factor(election.table[,13])

#YALOVA Evet.Sayisi is NA (FIXING CODES)
election.table[79,6]
election.table[79,6] <- election.table[79,4] - election.table[79,8]
election.table[79,6]
#######################################

election.table["Evet.Kullanan.Kitle"] <- NA
election.table$Evet.Kullanan.Kitle <- sapply(election.table$Evet.Orani, FUN = valueIndicator) #valueIndicator:Our function for levelling
election.table[,11] <- as.factor(election.table[,11])

election.table["Hayir.Kullanan.Kitle"] <- NA
election.table$Hayir.Kullanan.Kitle <- sapply(election.table$Hayir.Orani, FUN = valueIndicator) #valueIndicator:Our function for levelling
election.table[,12] <- as.factor(election.table[,12])

election.table["Sonuc"] <- NA
election.table$Sonuc <- sapply(election.table$Evet.Orani, FUN = winnerFinder) #winnerFinder:Our function for find Winner
election.table[,13] <- as.factor(election.table[,13])
election.table <- election.table[order(election.table$Sehir),]                              

#Taking helper table that formed regions for using filling regions from city on main table
region.url <- "https://aliaydin29.wordpress.com/2012/07/07/turkiye-81-il-plaka-ve-bilgi-listesi/"
region.table <- htmltab(doc = region.url, which = "//*[@id='post-4045']/div/table", encoding="UTF-8")
Encoding(region.table$Ad) <- "UTF-8"
colnames(region.table)[8] <- "Bolge"
Encoding(region.table$Bolge) <- "UTF-8"
region.table <- region.table[order(region.table$Ad),]   

#Reading Election Table
election.table <- read_excel("C:\\Users\\lenovo\\Desktop\\displaying_turkey_election_2017_result_in_r-master\\election.xlsx")
View(election.table)

#Concating Tables
election.table$Bolge <- region.table$Bolge

#Taking province and districts table
province_district_frame <- data.frame()

#Creating city district url list from csv
base_url <- 'http://www.milliyet.com.tr//'
source_path <- paste(DIR, "cityhrefs.xlsx" ,sep="") 
href_excel_frame <- read_excel(source_path)
href_list<-c()
i=1
while(i <= nrow(href_excel_frame)) {
  b <- href_excel_frame[i,1]
  target_url <- paste(base_url,b,sep="")
  href_list<-c(href_list,target_url)
  i=i+1
}
href_list
  for(city in href_list){
  province_url <- city
  province_webpage <- read_html(province_url)
  
  city_name_html <- html_nodes(province_webpage,'.ilTop')  
  city_name <- html_text(city_name_html)
  city_name <- list(stri_split_fixed(str = city_name, pattern = " ", n = 2))
  city_name <- unlist(city_name)
  city_name <- city_name[[1]]
  
  district_name_data_html <- html_nodes(province_webpage,'.ilcerw .rcol1')
  district_name_data <- html_text(district_name_data_html)
  
  district_yes_data_html <- html_nodes(province_webpage,'.ilcerw .rcol2')
  district_yes_data <- html_text(district_yes_data_html)
  
  district_no_data_html <- html_nodes(province_webpage,'.ilcerw .rcol3')
  district_no_data <- html_text(district_no_data_html)
  
  temp_district_Data <- data.frame(city_name,district_name_data,district_yes_data,district_no_data)
  province_district_frame <<- rbind(province_district_frame,temp_district_Data)
  Sys.sleep(2)
}

#Reading District Table
province_district_frame <- read_excel("C:\\Users\\lenovo\\Desktop\\displaying_turkey_election_2017_result_in_r-master\\district.xlsx")
View(province_district_frame)

  
colnames(province_district_frame) <- c("Sehir","Ilce","Evet.Orani", "Hayir.Orani")

province_district_frame[,3] <- gsub("%","",province_district_frame[,3])
province_district_frame[,3] <- as.double(province_district_frame[,3])
province_district_frame[,4] <- gsub("%","",province_district_frame[,4])
province_district_frame[,4] <- as.double(province_district_frame[,4])
province_district_frame["Sonuc"] <- NA
province_district_frame$Sonuc <- sapply(province_district_frame$Evet.Orani, FUN = winnerFinder)
province_district_frame[,5] <- as.factor(province_district_frame[,5])
View(province_district_frame)

#Creating Region Result Table
unique_region_name <- subset(election.table, !duplicated(Bolge))
region_list<-c()
i=1
while(i <= nrow(unique_region_name)) {
  region <- unique_region_name[i,14]
  region_list<-c(region_list,region)
  i=i+1
}
region_list

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



region_result_frame <- data.frame()
for(val in  region_list){
  temp_region_frame <- election.table[election.table$Bolge == val, ]
  yes_mean <- mean(temp_region_frame[["Evet.Sayisi"]])
  no_mean <- mean(temp_region_frame[["Hayir.Sayisi"]])
  if(yes_mean > no_mean){
    result <- "Yes"
  }else{
    result <- "No"
  }
  temp_region_data_frame = data.frame(val,yes_mean, no_mean, result)
  region_result_frame <<- rbind(temp_region_data_frame, region_result_frame)
}
colnames(region_result_frame) <- c("Bolge","Evet.Ortalamasi","Hayir.Ortalamasi","Sonuc")

  # Take Lon and Lat from cities in a dataframe called coordinates_dataframe
  data_lenght <- nrow(election.table)
  coordinates_dataframe <- data.frame()
  for (val in  c(1:data_lenght)){
    address <- election.table[val,1]
    address <- as.character(address)
    try(
      geocode(address)
    )
    coordinates_dataframe <- rbind(coordinates_dataframe, geocode(address))
  }


#Filling city coordinates to main cities table
election.table <- cbind(election.table,coordinates_dataframe)

# Take Lon and Lat from regions in a dataframe called region_coordinates_dataframe
#regin_list_api occurs in the middle cities of regions
regin_list_api <- c("Yalova","Amasya","Kirsehir","Bingol","Izmir","Sanliurfa","Karaman")
region_coordinates_dataframe <- data.frame()

i = 1
for(val in regin_list_api){
  region_coordinates_dataframe <- rbind(region_coordinates_dataframe, geocode(val))
  i=i+1
}
#Filling region coordinates to main region table
region_result_frame <- cbind(region_result_frame,region_coordinates_dataframe)

###IMPORT TABLES TO EXCEL
write.xlsx(election.table, paste(DIR , 'election.xlsx', sep="/"))
write.xlsx(province_district_frame, paste(DIR , 'district.xlsx', sep="/"))
write.xlsx(region_result_frame, paste(DIR , 'region.xlsx', sep="/"))
#############

#GGplot Operations
#1.All cities Result
city_plot <- ggplot(data=election.table, aes(x=Evet.Orani, y=Hayir.Orani, colour = Sonuc))
city_plot + geom_point() + geom_label(aes(label=Sehir))

#2.Big Cities Result
big_cities_frame <- election.table[election.table$Oy.Veren.Insan.Sayisi >= 1000000, ]
big_cities_plot <- ggplot(data=big_cities_frame, aes(x=Evet.Orani, y=Hayir.Orani, colour = Sonuc))
big_cities_plot + geom_point() + geom_label(aes(label=Sehir))

#3. Region Result
region_average_plot <- ggplot(data=region_result_frame, aes(x=Evet.Ortalamasi, y=Hayir.Ortalamasi, colour = Sonuc))
region_average_plot + geom_point() + geom_label(aes(label=Bolge))

  
#4.All Cities Result according a region
#Function 4
showCityResultByRegionInGGPlot <- function(Region){
  
  region_frame <- election.table[election.table$Bolge == Region, ]
  region_plot <- ggplot(data=region_frame, aes(x=Evet.Orani, y=Hayir.Orani, colour = Sonuc))
  region_plot + geom_point() + geom_label(aes(label=Sehir))
  
}
showCityResultByRegionInGGPlot("Marmara Bˆlgesi") #Sample


#5.All Districts Result according a city
#Function 5
showDistrictResultInGGPlot <- function(City){
  
  city_frame <- province_district_frame[province_district_frame$Sehir == City, ]
  district_plot <- ggplot(data=city_frame, aes(x=Evet.Orani, y=Hayir.Orani, colour = Sonuc))
  district_plot + geom_point() + geom_label(aes(label=Ilce))
  
}
showDistrictResultInGGPlot("Ankara") #Sample

#GGMap Operations
#1. General
#geocode("Turkey") 35.24332 38.96375
take_turkey_map = get_map(location = c(lon =  35.24332, lat = 38.96375), 
                         zoom = 5, maptype = 'hybrid', source = "google")
turkey_map = ggmap(take_turkey_map)

#Show election result according to cities
turkey_map + geom_point(aes(x = lon, y = lat, colour = Sonuc), data = election.table,
                            alpha = .5, size = 5)

# + geom_text(aes(x = lon, y = lat, label = Sehir),data = election.table, colour = I("white"))
#2.Region Result on GGMap
take_turkey_map = get_map(location = c(lon =  35.24332, lat = 38.96375), 
                          zoom = 5, maptype = 'hybrid', source = "google")
turkey_map = ggmap(take_turkey_map)
turkey_map + geom_point(aes(x = lon, y = lat, colour = Sonuc), data = region_result_frame,
                        alpha = .5, size = 20) + geom_text(aes(x = lon, y = lat, label = Bolge),
                                                          data = region_result_frame, colour = I("white"))

#3. Cities Result according to region onn GGMap
#Function 6
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
showCitiesInRegionInGGMAP("Marmara Bˆlgesi") #Sample

#4. Most Voted Yes People
take_turkey_map = get_map(location = c(lon =  35.24332, lat = 38.96375), 
                          zoom = 5, maptype = 'hybrid', source = "google")
turkey_map = ggmap(take_turkey_map)
turkey_map + geom_point(aes(x = lon, y = lat, colour = Evet.Kullanan.Kitle), data = election.table,
                        alpha = .5, size = 5)
# + geom_text(aes(x = lon, y = lat, label = Sehir), data = election.table, colour = I("white"))
# Hayir Kullanan Kitle IÁin Renk Kodlari
ccolors = c("#ff0000","#ffcccc","#ff6666","#b30000")

#4. Most Voted No People
take_turkey_map = get_map(location = c(lon =  35.24332, lat = 38.96375), 
                          zoom = 5, maptype = 'hybrid', source = "google")
turkey_map = ggmap(take_turkey_map)
turkey_map + geom_point(aes(x = lon, y = lat, colour = Hayir.Kullanan.Kitle), data = election.table,
                        alpha = .5, size = 5) + scale_colour_manual(values = ccolors)



cities <- levels(factor(election.table$Sehir))  
cities <- as.list(cities)

######### SHINY GUI #############################

if (interactive()) {
  
  shinyApp(
    ui = fluidPage(titlePanel("Election Results in Tables according to Filtres"),
                   
  div(style="display: inline-block;vertical-align:top; width: 250px;", selectInput("variable", "Select a region...",list(`Regions` = region_list))),
  div(style="display: inline-block;vertical-align:top; width: 250px;", selectInput("variable2", "Select a city...",list(`Cities` = cities))),
  div(style="display: inline-block;vertical-align:top; width: 250px;", selectInput("variable3", "Select a region...", c("Yes","No"))),
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
  
  take_turkey_map = get_map(location = c(lon =  35.24332, lat = 38.96375), 
                            zoom = 5, maptype = 'hybrid', source = "google")
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
  
  
##ƒ∞PTAL KODLAR
# #geocode("Erzincan") 39.49112 39.7468
# #2. Specific City                                       
# #Show election result according to city districts
# showDistrictsResult <- function (City) {
#   geocode(City) 
#   take_city_map = get_map(location = c(lon =  39.49112, lat = 39.7468),  #citylon, citylat
#                           zoom = 5, maptype = 'hybrid', source = "google")
#   city_map = ggmap(take_city_map)
#   city_frame <- province_district_frame[province_district_frame$Sehir == City, ]
#   
#   district_list<-c()
#   i=1
#   while(i <= nrow(city_frame)) {
#     district <- city_frame[2,1]
#     district <- gsub("√ß","c",district)
#     district <- gsub("≈ü","s",district)
#     district <- gsub("ƒü","g",district)
#     district <- gsub("√∂","o",district)
#     district <- gsub("√º","u",district)
#     href_list<-c(href_list,district)
#     i=i+1
#   }
#   province_district_frame[,3] <- 
#   
#   city_map + geom_point(aes(x = lon, y = lat, colour = Sonuc), data = city_frame,
#                           alpha = .5, size = 5) + geom_text(aes(x = lon, y = lat, label = Ilce),
#                                                             data = city_frame, colour = I("white"))
# }
# 
# showDistrictsResult("Erzincan")

#3. Function for finding cities and regions
# base_url <- 'http://www.illeryollar.com'
# region_url <- paste(base_url,'bolgeler',sep="/")
# region_webpage <- read_html(region_url)
# region_data_html <- html_nodes(region_webpage,'.col-offset-1 a')
# region_data <- html_text(region_data_html)
# region_links <- html_attr(region_data_html,name = 'href')
# 
# 
# findCities <- function () {
#   result <- data.frame()
#   for(link in region_links){
#     dest_url <- paste(base_url, link,sep="")
#     print(dest_url)  
#     dest_webpage <- read_html(dest_url)
#     dest_data_html <- html_nodes(dest_webpage,'.col-md-9 div ul li a')
#     dest_data <- html_text(dest_data_html)
#     region_name_html <- html_nodes(dest_webpage,'.col-md-9 h2')
#     region_name <- html_text(region_name_html)
#     temp.data.frame = data.frame(dest_data,region_name)
#     result <- rbind(temp.data.frame,result)
#     Sys.sleep(1)
#   }
#   return(result)
# }
# 
# city.and.region.frame <- findCities()
# colnames(city.and.region.frame) <- c("Sehir","Bolge")
# city.and.region.frame[,1] <- as.factor(city.and.region.frame[,1])
# city.and.region.frame <- city.and.region.frame[order(city.and.region.frame$Sehir),]                              


#__
# turkey.map <- c(left = 25, bottom = 35, right = 46, top = 43)
# map <- get_stamenmap(turkey.map, zoom = 5, maptype = "toner-lite")
# ggmap(map)

#Tabloda koruma var hrefler gelmƒ±yor
# city_url <- 'http://www.milliyet.com.tr/referandum-2017/turkiye-anayasa-degisikligi-referandumu-sonuclari/'
# city_webpage <- read_html(city_url)
# city_data_html <- html_nodes(city_webpage, '.ilList a') #.links √ßalƒ±≈üƒ±yor 
# city_links <- html_attr(city_data_html, name = 'href')
# city_links

#2500 ˝lce var cek˝lm˝yor. yanl˝s gelenlerde oluyor
#province_district_frame <- read_excel("C:\\Users\\eren\\Desktop\\WebminingProject\\district.xlsx")
#View(province_district_frame)
# province_district_frame["Sehir.Ilce"] <- paste(province_district_frame$Sehir , province_district_frame$Ilce, " ")
# province_district_frame <- subset(province_district_frame, !duplicated(Sehir.Ilce))
# Take Lon and Lat from districst according to city in a dataframe called distirct_coordinates_dataframe
# data_lenght <- nrow(province_district_frame)
# distirct_coordinates_dataframe <- data.frame()
# for (val in  c(1:data_lenght)){
#   address <- province_district_frame[val,2]
#   address <- as.character(address)
#   try(
#     geocode(address)
#   )
#   distirct_coordinates_dataframe <- rbind(distirct_coordinates_dataframe, geocode(address))
# }
# 
# #Filling city coordinates to main cities table
# province_district_frame <- cbind(province_district_frame,distirct_coordinates_dataframe)