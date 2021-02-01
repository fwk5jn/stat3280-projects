# stat3280-projects
code from the semesters projects


      '''
      ### problem 1 ###

                  setwd("C:/Users/firem/Desktop/University of Virginia/spring20/STAT 3280/hw1")

                  prob1<- scan("HandWritten.txt") #scans in the data

                  ?matrix
                  p1<- matrix(data= prob1, nrow= 16, ncol= 16, byrow = T) #creates a matrix from the data

                  ?image

                  mysterynumber<-image(p1, axes = FALSE) #visualizes the data in the matrix into an image

                  p1<- apply(p1, 2 , rev) #the following two lines of code are used to flip the image so it is right side up
                  image(1:16,1:16, t(p1))

                  title('mystery number') #adds a title to the image

                  ### problem 2 ###


                  mixture <- read.csv("mixture.csv") #reads in the data

                  x<- mixture$x1 #creates a variable for the x corrdinates
                  y<- mixture$x2 #creates a variable for the y corrdiantes
                  length(x) #finds the length of the x column
                  length(y) #finds the length of the y column
                  slope<- (0.134/1.398) #establishes the slope (m) portion of our y=mx+b equation
                  intercept<- (0.978/1.398) #establishes the intercept (b)

                  plot(x,y, col=mixture$y+1, pch=18) #generates a plot of our data points
                  abline(intercept, slope, lty=2, lwd=2, col='blue') #introduces a regression line into the plot generated above
                  title('Classifier') #adds a title to the plot

                  #?abline
                  # (-0.978-0.134*10)/(-1.398) #uses the classification boundry to help us establish where our polygon will shade
                  # (-0.978-0.134*(-10))/(-1.398)
                  x1= c(-10,10,10,-10) #establishes the parameters of our black polygon
                  y1= c(-10,-10,(1.658083),(-0.2589413))


                  polygon(x1,y1, lty=1, col= rgb(0, 0, 0, 0.3)) #draws the black portion of the polygon that shades our graph

                  x2= c(-10, 10, 10, -10) #establishes the parameters of our red polygon
                  y2= c(10, 10, (1.658083),(-0.2589413))

                  polygon(x2,y2, lty=1, col= rgb(1, 0, 0, 0.5)) #draws the red portion of the polygon that shades our graph
                  #?polygon
                  legend("bottomleft", legend= c("x","y"), pch=18, col=c("black", "red"))


                  ### problem 3 ###
                  #refer to line 740 from notes

                  library(YaleToolkit)
                  x<- read.csv("TeacherHires.csv")
                  names(x)
                  head(x)
                  ###remove the N/A columns###
                  x= x[, -which(whatis(x)$missing==nrow(x))]
                  head(x)
                  ###now that N/A have been removed lets clean up these names a bit###

                  names(x)= c("interview", "hired", "date", "age", "gender", "residence", "undergrad", "graduate",
                              "masters", "sub", "teach", "exper", "kids", "volunteer")

                  summary(x)
                  ###the question asks about age discrimination so lets begin by exploring that variable###

                  whatis(x$age)

                  ###age is a mixed factor; lets see if we can fix this###

                  x$age=as.numeric(as.character(x$age))
                  summary(x$age)
                  whatis(x$age)
                  ###now that we've addressed age lets take a look at the undergraduate and graduate GPA's###

                  whatis(x$undergrad)
                  ###also a mixed factor; lets fix this###

                  x$undergrad=as.numeric(as.character(x$undergrad))
                  summary(x$undergrad)
                  whatis(x$undergrad)

                  ###and same thing for graduate###
                  x$graduate=as.numeric(as.character(x$graduate))
                  summary(x$graduate)
                  whatis(x$graduate)

                  ###there is a GPA of 9.97 in graduate GPA's which seems like an unreasonable outlier; so i'll remove it###

                  x= subset(x, graduate!=9.97, select=c("interview", "hired", "date", "age", "gender", "residence", "undergrad", "graduate",
                                                        "masters", "sub", "teach", "exper", "kids", "volunteer"))

                  summary(x$graduate)

                  ###another important variable we would like to consider during our investigation is gender###

                  whatis(x$gender)

                  x$gender=as.character(x$gender)
                  unique(x$gender)

                  x$gender[x$gender==""]=NA
                  x$gender[x$gender=="M"]="Male"
                  x$gender[x$gender=="F"]="Female"
                  x$gender= factor(x$gender)
                  summary(x$gender)



                  wi=whatis(x)
                  class(wi)
                  names(wi)
                  wi$variable.name
                  wi$missing


                  ?na.rm
                  ###consider na.rm function when vizualizing data


                  # HW 1
                  # Problem 4

                  data1 <- read.csv("Data1.csv", header = TRUE)

                  data1$WinningScore <- substr(data1$Score, 0, 2)
                  data1$LosingScore <- substr(data1$Score, 6, 8)

                  data1$LosingScore <- as.numeric(data1$LosingScore)
                  data1$WinningScore <- as.numeric(data1$WinningScore)

                  data2 <- data1[,c(2,3,4,6,7)]
                  head(data2)

                  data2$Winner <- as.character(data2$Winner)
                  data2[c(42,44,51,52,54,63,77,82,85,86,95,98,99,101,103,104,109,120),3] <- substring(data2[c(42,44,51,52,54,63,77,82,85,86,95,98,99,101,103,104,109,120),3],5,nchar(data2[c(42,44,51,52,54,63,77,82,85,86,95,98,99,101,103,104,109,120),3]))
                  data2[c(53,102),3] <- substring(data2[c(53,102),3],4,nchar(data2[c(53,102),3]))

                  # Bar Graph For number of Virginia wins, NC wins, and ties for each location
                  BarData <- data2[,c(2,3)]
                  counts <- table(BarData$Winner, BarData$Location)
                  counts
                  test <- rbind(counts[1,], counts[3,], counts[2,])

                  barplot(test, main="Game Outcomes by Location",
                          xlab="Location",ylab = "Number of Occurrences", col=c("lightblue","orange","grey"),
                          legend = c("North Carolina", "Virginia", "Tie"), beside=TRUE)


                  # Histograms of the score differentials when each team won
                  data2$ScoreDifferential <- data2$WinningScore - data2$LosingScore
                  HistData <- data2[,c(3,6)]
                  CarWins <- subset(HistData, Winner == "North Carolina")
                  VirgWins <- subset(HistData, Winner == "Virginia")

                  hist(CarWins$ScoreDifferential, breaks = 10, main = "Histogram of the Score Differentials for games North Carolina Won",
                       xlab = "Score Differential", col = "lightblue")

                  hist(VirgWins$ScoreDifferential, breaks = 10, main = "Histogram of the Score Differentials for games Virginia Won",
                       xlab = "Score Differential", col = "orange")




                  # Every 31 years Count of outcomes 

                  barplot2data <- data2[,c(1,3,4)]
                  First31 <- table(barplot2data$Winner[1:31])
                  Second31 <- table(barplot2data$Winner[32:62])
                  Third31 <- table(barplot2data$Winner[63:93])
                  Fourth31 <- table(barplot2data$Winner[94:124])

                  First31
                  Second31
                  Third31
                  Fourth31

                  layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
                  barplot(First31, main="Outcomes from the First 31 Games",
                          xlab="Outcome",ylab = "Number of Occurrences", col=c("lightblue","grey","orange"),
                          beside=TRUE)
                  barplot(Second31, main="Outcomes from the Second 31 Games",
                          xlab="Outcome",ylab = "Number of Occurrences", col=c("lightblue","orange"),
                          beside=TRUE)
                  barplot(Third31, main="Outcomes from the Third 31 Games",
                          xlab="Outcome",ylab = "Number of Occurrences", col=c("lightblue","grey","orange"),
                          beside=TRUE)
                  barplot(Fourth31, main="Outcomes from the Fourth 31 Games",
                          xlab="Outcome",ylab = "Number of Occurrences", col=c("lightblue","orange"),
                          beside=TRUE)




                  ################################################################################################
                  ################################################################################################

                  # Problem 1
                  # Part 1
                  library(gridExtra)
                  library(ggmap)
                  library(ggplot2)
                  library(grid)


                  Cloud1 <- read.table("cloudhigh1.txt", skip = 7, header = FALSE)
                  Cloud1 <- Cloud1[,-c(1,2,3)]
                  long1 <- read.table("cloudhigh1.txt", skip = 5, header = FALSE, nrows = 1, as.is = TRUE)
                  long1 <- unlist(long1)
                  names(Cloud1) <- long1
                  lat1 <- read.table("cloudhigh1.txt", skip = 7, header = FALSE)
                  lat1 <- lat1[,1]
                  rownames(Cloud1) <- lat1
                  Cloud1M <- data.matrix(Cloud1)

                  GridTimeSeries <- list()

                  for(t in 1:72){
                    cloudhigh <- read.table(paste("cloudhigh",t,".txt",sep=""), skip = 7, header = FALSE)
                    cloudhigh <- as.numeric(as.matrix(cloudhigh[,-(1:3)]))

                    GridTimeSeries[[t]] <- cloudhigh
                  }

                  GridTimeSeries2 <- list()

                  for(t in 1:72){
                    mat <- matrix(GridTimeSeries[[t]], nrow = 24, ncol = 24)
                    colnames(mat) <- long1
                    rownames(mat) <- lat1
                    GridTimeSeries2[[t]] <- mat
                  }

                  cloudhigh.months <- list()

                  for(i in 1:12){
                    data <- 0
                    month <- i
                    for(j in seq(month,72,12)){
                      data <- data + (matrix(GridTimeSeries2[[j]], nrow = 24, ncol = 24))
                    }
                    MonthAverage <- data / 6
                    colnames(MonthAverage) <- long1
                    rownames(MonthAverage) <- lat1
                    cloudhigh.months[[i]] <-  MonthAverage
                  }
                  names(cloudhigh.months) = c('January', 'Febuary', 'March', 'April', 'May','June','July',
                                              'August', 'September', 'October', 'November', 'December')

                  names(cloudhigh.months) <- month.abb

                  bigboimonth <- do.call(rbind, lapply(seq_along(cloudhigh.months), function(i) {
                    mat <- cloudhigh.months[[i]]
                    data.frame(avg = c(mat), longitude = rep(colnames(mat), each = nrow(mat)),
                               latitude = rep(rownames(mat), ncol(mat)), month = names(cloudhigh.months)[[i]])
                  }))



                  # Visualization

                  plot234 <- ggplot(bigboimonth) +
                    geom_point(aes(month, avg, colour = avg),size = .25) +
                    facet_grid(longitude ~ latitude) +
                    scale_color_gradient(low="orange", high="blue") +
                    theme(axis.text = element_blank(),
                          strip.text = element_blank(),
                          axis.ticks = element_blank(),
                          axis.ticks.length = unit(0, "pt"),
                          axis.title = element_blank(),
                          legend.position = c(1.05, 0.5),
                          legend.justification = c(0, 0.5),
                          plot.title = element_text(hjust = 0.5, size = 18, face = 'bold'),
                          panel.background = element_rect(fill = "transparent",colour = NA),
                          panel.border = element_rect(size=.5, fill=NA, colour = 'black'),
                          panel.grid.minor = element_blank(),
                          panel.grid.major = element_blank(),
                          plot.background = element_rect(fill = "transparent",colour = NA))

                  plot234

                  ###map with grey border###
                  map <- get_stamenmap( bbox = c(left = -113.8, bottom = -21.2, right = -56.2, top = 36.2), zoom = 4, maptype = "terrain-background")
                  ggmap(map) +
                    theme_void() +
                    theme(
                      plot.title = element_text(colour = "orange"),
                      panel.border = element_rect(colour = "grey", fill=NA, size=2)
                    ) + 
                    inset(ggplotGrob(plot234), xmin = Inf, xmax = -Inf, ymin = Inf, ymax = -Inf)


                  ###map with green border###
                  ggmap(map) +
                    theme_void() +
                    theme(
                      plot.title = element_text(colour = "orange"),
                      panel.border = element_rect(colour = "green", fill=NA, size=2)
                    ) + 
                    inset(ggplotGrob(plot234), xmin = Inf, xmax = -Inf, ymin = Inf, ymax = -Inf)


                  ##################FINAL VERSION (p1 part1)######################
                  p1<-ggmap(map) +
                    theme(
                      plot.title = element_text(colour = "orange"),
                      panel.border = element_rect(colour = "green", fill=NA, size=2),
                      axis.text.x = element_blank(),
                      axis.text.y = element_blank(),
                      axis.ticks = element_blank()
                    ) +
                    inset(ggplotGrob(plot234), xmin = Inf, xmax = -Inf, ymin = Inf, ymax = -Inf)

                  p1+ ggtitle("Average Monthly High Cloud Coverage for Central America (1995-2000)")+ xlab("Months(January-December)")+ylab("Average High Cloud Coverage Level")+
                    theme(plot.title = element_text(hjust = 0.5, size = 15, face = 'bold'))

                  ########### Part 2 ###############

                  cloudhigh.years <- list()

                  for(i in 0:5){
                    data <- 0
                    year <- i
                    for(j in seq((year*12)+1,(12*year)+12)){
                      data <- data + (matrix(GridTimeSeries2[[j]], nrow = 24, ncol = 24))
                    }
                    YearAverage <- data / 12
                    colnames(YearAverage) <- long1
                    rownames(YearAverage) <- lat1
                    cloudhigh.years[[i+1]] <-  YearAverage
                  }

                  names(cloudhigh.years) <- c("year1", "year2", "year3", "year4", "year5", "year6")

                  bigboiyear <- do.call(rbind, lapply(seq_along(cloudhigh.years), function(i) {
                    mat <- cloudhigh.years[[i]]
                    data.frame(avg = c(mat), longitude = rep(colnames(mat), each = nrow(mat)),
                               latitude = rep(rownames(mat), ncol(mat)), year = names(cloudhigh.years)[[i]])
                  }))



                  plotyear <- ggplot(bigboiyear) +
                    geom_point(aes(year, avg, colour = avg)) +
                    facet_grid(longitude ~ latitude) +
                    scale_color_gradient(low="orange", high="blue") +
                    theme(axis.text = element_blank(),
                          strip.text = element_blank(),
                          axis.ticks = element_blank(),
                          axis.ticks.length = unit(0, "pt"),
                          axis.title = element_blank(),
                          legend.position = c(1.05, 0.5),
                          legend.justification = c(0, 0.5),
                          plot.title = element_text(hjust = 0.5, size = 18, face = 'bold'),
                          panel.background = element_rect(fill = "transparent",colour = NA),
                          plot.background = element_rect(fill = "transparent",colour = NA))


                  plotyear

                  plotyear <- ggplot(bigboiyear) +
                    geom_point(aes(year, avg, colour = avg),size =.75) +
                    facet_grid(longitude ~ latitude) +
                    scale_color_gradient(low="orange", high="blue") +
                    theme(axis.text = element_blank(),
                          strip.text = element_blank(),
                          axis.ticks = element_blank(),
                          axis.ticks.length = unit(0, "pt"),
                          axis.title = element_blank(),
                          legend.position = c(1.05, 0.5),
                          legend.justification = c(0, 0.5),
                          plot.title = element_text(hjust = 0.5, size = 18, face = 'bold'),
                          panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank(),
                          panel.border = element_rect(size=.5, fill=NA, colour = 'black'),
                          panel.background = element_rect(fill = "transparent",colour = NA),
                          plot.background = element_rect(fill = "transparent",colour = NA))


                  plotyear

                  #####################TEST####################################

                  map <- get_stamenmap( bbox = c(left = -113.8, bottom = -21.2, right = -56.2, top = 36.2), zoom = 4, maptype = "terrain-background")
                  ggmap(map) +
                    theme_void() +
                    theme(
                      plot.title = element_text(colour = "orange"),
                      panel.border = element_rect(colour = "grey", fill=NA, size=2)
                    ) +
                    inset(ggplotGrob(plotyear), xmin = Inf, xmax = -Inf, ymin = Inf, ymax = -Inf)

                  ############FINAL VERSION (p1 part2)#######################

                  map <- get_stamenmap( bbox = c(left = -113.8, bottom = -21.2, right = -56.2, top = 36.2), zoom = 4, maptype = "terrain-background")
                  p2<-ggmap(map) +
                    theme(
                      plot.title = element_text(colour = "orange"),
                      panel.border = element_rect(colour = "green", fill=NA, size=2),
                      axis.text.x = element_blank(),
                      axis.text.y = element_blank(),
                      axis.ticks = element_blank()
                    ) +
                    inset(ggplotGrob(plotyear), xmin = Inf, xmax = -Inf, ymin = Inf, ymax = -Inf)
                  p2 + ggtitle("Average Yearly High Cloud Coverage for Central America")+ xlab("Years(1995-2000)")+ylab("Average High Cloud Coverage Level")+
                    theme(plot.title = element_text(hjust = 0.5, size = 15, face = 'bold'))

                  ###################################################################################################
                  ###################################################################################################

                  ##################################################################################
                  # hw3 problem 1
                  ##################################################################################
                  library(maps)
                  library(geosphere)
                  library(igraph)
                  library(tidyverse)
                  library(ggraph)

                  airport <- read.table("airports.txt", header = F, sep = ",")
                  head(airport)
                  colnames(airport)= c("airport_ID", "airport_name", "city", "county", "IATA", "ICAO", "lat", "long", "alt",
                                       "timezone", "daylight_savings_time", "timezone_type")

                  airlines <- read.table("airlines.dat", header = F, sep = ",")
                  head(airlines)
                  colnames(airlines)= c("airline_id", "name", "alias", "IATA", "ICAO", "callsign", "country", "active")

                  routes <- read.table("routes.dat", header = F, sep=",")
                  head(routes)
                  colnames(routes)= c("airline", "airline_id", "source_airport", "source_airport_id", "destination_airport", "destination_airport_id",
                                      "codeshare", "stops", "equipment")

                  connections_by_route <- routes %>% 
                    mutate_at(c("source_airport", "destination_airport"), as.character) %>%
                    mutate(
                      source_airport_ = if_else(
                        source_airport < destination_airport,
                        source_airport, destination_airport
                      ),
                      destination_airport_ = if_else(
                        source_airport < destination_airport, 
                        destination_airport, source_airport
                      )
                    ) %>% 
                    group_by(source_airport_, destination_airport_) %>%
                    summarise(n = n(), avg_stops = mean(stops)) %>%
                    ungroup() %>%
                    arrange(-n, -avg_stops) %>%
                    transmute(
                      source_airport = source_airport_,
                      destination_airport = destination_airport_,
                      n = n) %>%
                    head(1000)

                  airport_locations <- airport %>% 
                    select(IATA, lat, long) %>%
                    mutate(IATA = as.character(IATA))

                  connections_with_locations <- connections_by_route %>% 
                    left_join(airport_locations, by = c("source_airport" = "IATA")) %>%
                    left_join(
                      airport_locations, by = c("destination_airport" = "IATA"),
                      suffix = c("_src", "_dst"))

                  airport_ <- airport_locations %>% 
                    filter(IATA %in% unique(c(connections_by_route$source_airport, connections_by_route$destination_airport)))


                  arc <- gcIntermediate(
                    as.matrix(select(connections_with_locations, long_src, lat_src)),
                    as.matrix(select(connections_with_locations, long_dst, lat_dst)),
                    n = 1000, sp = TRUE, breakAtDateLine = TRUE 
                  )

                  maps::map("world", col="grey20", fill=TRUE, bg="light blue", lwd=.1)

                  points(x=airport_$long, y=airport_$lat, pch=19, cex = 0.25, col="orange")

                  # https://colorbrewer2.org
                  lines(arc, col=rgb(0.9411765, 0.2313725, 0.1254902, alpha=0.3) , cex=0.05)


                  # covid vizualization
                  # data preperation

                  library(RColorBrewer)
                  library(ggmap)
                  library(gganimate)
                  library(ggrepel)
                  library(ggthemes)
                  library(transformr)
                  library(tidyverse)
                  library(wbstats)
                  library(patchwork)
                  library(leaflet)

                  prepare_cases <- function(url, label) {
                    read_csv(url, col_types = cols(
                      .default = col_double(),
                      `Province/State` = col_character(),
                      `Country/Region` = col_character()
                    )) %>%
                      pivot_longer(
                        -one_of("Province/State", "Country/Region", "Lat", "Long"),
                        names_to = "Date",
                        values_to = "Count") %>%
                      mutate(Count = as.integer(Count)) %>%
                      mutate(Date = lubridate::mdy(Date), type = label) %>%
                      janitor::clean_names()

                  }

                  confirmed_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
                  death_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
                  recovered_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"

                  confirmed_world <- prepare_cases(confirmed_url, "confirmed")
                  death_world <- prepare_cases(death_url, "dead")
                  recovered_world <- prepare_cases(recovered_url, "recovered")

                  combined_world <- union_all(confirmed_world, death_world) %>%
                    union_all(recovered_world)

                  wide_world <- combined_world %>%
                    pivot_wider(names_from = "type", values_from = "count", values_fill = list(count = 0L))

                  uk_oversea_and_crown_dependencies <- wide_world %>%
                    select(province_state, country_region) %>%
                    filter(country_region == "United Kingdom" & !is.na(province_state)) %>%
                    distinct() %>%
                    magrittr::extract2("province_state")

                  uk_oversea_and_crown_dependencies

                  countries_of_interest <- c("China", "Italy", "Spain", "US", "Germany", "United Kingdom")

                  covid19 <- wide_world %>%
                    filter(country_region %in% countries_of_interest) %>%
                    filter(!(country_region == "United Kingdom" & province_state %in% uk_oversea_and_crown_dependencies))



                  #' The number of confirmed cases over time for a selection of countries

                  lastday <- max(covid19$Date)
                  lastday <- "2020-03-22"

                  totals_by_country_and_date <- covid19 %>%
                    group_by(country_region, date) %>%
                    summarise_at(c("confirmed", "dead", "recovered"), sum)

                  max(totals_by_country_and_date$confirmed)

                  # CHN-1392730000;DEU-82905782;ITA-60421760;ESP-46796540;GBR-66460344;USA-326687501

                  population = wb(indicator = "SP.POP.TOTL", startdate = 2018, enddate = 2018)

                  totals_by_country_and_date$population <- rep(NA, nrow(totals_by_country_and_date))

                  totals_by_country_and_date[totals_by_country_and_date$country_region == "China",][,"population"]<- "1392730000"
                  totals_by_country_and_date[totals_by_country_and_date$country_region == "Germany",][,"population"]<- "82905782"
                  totals_by_country_and_date[totals_by_country_and_date$country_region == "Italy",][,"population"]<- "60421760"
                  totals_by_country_and_date[totals_by_country_and_date$country_region == "Spain",][,"population"]<- "46796540"
                  totals_by_country_and_date[totals_by_country_and_date$country_region == "United Kingdom",][,"population"]<- "66460344"
                  totals_by_country_and_date[totals_by_country_and_date$country_region == "US",][,"population"]<- "326687501"

                  totals_by_country_and_date$population <- as.numeric(as.character(totals_by_country_and_date$population))

                  # graph1
                  p_confirmed1 <- ggplot(totals_by_country_and_date, aes(x = date, y = (confirmed/population)*1000000, color = country_region)) +
                    geom_line() +
                    geom_point(shape=9) +
                    labs(x = 'Date', y = 'Infected individuals per million in population', color = 'Country',
                         title = 'Persons infected by COVID-19') + 
                    theme_bw()


                  # graph2
                  p_dead_to_recovered <- ggplot(
                    mutate(totals_by_country_and_date, ratio = dead / recovered),
                    aes(x = date, y = ratio, color = country_region)) +
                    geom_line() +
                    labs(x = 'Date', y = 'Dead / Recovered', color = 'Country',
                         title = 'Ratio of Dead to Recovered Over Time') +
                    theme_bw()

                  #graph3
                  fraction_by_status <- totals_by_country_and_date %>%
                    replace_na(list(dead = 0, recovered = 0)) %>%
                    mutate(combined = dead + recovered) %>%
                    mutate(dead = dead / combined, recovered = recovered / combined) %>%
                    pivot_longer(c("dead", "recovered"), names_to = "status") %>%
                    mutate(status = factor(status, levels = c("dead", "recovered")))

                  p_share_of_status <- ggplot(fraction_by_status, aes(date, value)) +
                    geom_area(aes(fill = status)) +
                    facet_grid(country_region ~ .) +
                    scale_fill_manual(values = c("#d53e4f", "#3288bd")) +
                    theme_bw() +
                    labs(x = 'Date', y = "Fraction", color = 'Status',
                         title = 'Ratio of deaths and recoveries') +
                    theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

                  # gifs
                  p_dead_to_recovered +
                    transition_reveal(date)

                  # anim_save(
                  #   "p_dead_to_recovered.gif",
                  #   p_dead_to_recovered + transition_reveal(date)
                  # )

                  p_confirmed1 +
                    transition_reveal(date)

                  anim_save(
                    "p_confirmed1.gif",
                    p_confirmed1 + transition_reveal(date)
                  )

                  p_share_of_status + transition_reveal(date)

                  anim_save(
                    "p_share_of_status.gif",

                    p_share_of_status + transition_reveal(date)
                  )

                  # geographical distribution

                  cases_by_location_and_date <- wide_world %>%
                    group_by(country_region, lat, long, date) %>%
                    summarise_at(c("confirmed", "dead", "recovered"), sum) %>%
                    ungroup() %>%
                    filter(date == max(date))


                  qmplot(
                    long, lat, data = cases_by_location_and_date, size = confirmed,
                    color = confirmed,
                    alpha = .3,
                    maptype = "toner-lite"
                  ) + labs(title = 'Geographical Distribution of Confirmed Cases')+
                    theme(plot.title = element_text(hjust = 0.5))

                  # another vizualization

                  # http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip)
                  borders_df <- sf::read_sf("TM_WORLD_BORDERS_SIMPL-0.3.shp")

                  convert_to_sf <- function(data, crs = sf::st_crs(4326)) {
                    sf::st_sfc(purrr::map2(data$long, data$lat, ~sf::st_point(c(.x, .y)))) %>%
                      sf::st_sf(data, location = ., crs=crs)
                  }

                  wide_world_sf <- borders_df %>%
                    sf::st_join(
                      convert_to_sf(wide_world) %>% filter(date == max(date)),
                      sf::st_contains
                    ) %>%
                    group_by(NAME) %>%
                    summarise_at(c("confirmed", "dead", "recovered"), ~sum(., na.rm = TRUE)) %>%
                    mutate(label = paste("Confirmed:", confirmed))


                  leaflet::leaflet(data = wide_world_sf) %>%
                    leaflet::addProviderTiles("Stamen.Watercolor") %>%
                    leaflet::addPolygons(
                      # leaflet::colorQuantile
                      fillColor = ~confirmed %>% leaflet::colorBin("YlOrRd", .)(.),
                      fillOpacity = 0.7,
                      highlight = highlightOptions(bringToFront = TRUE),
                      label = ~label
                    )

                  # covid vizualization
                  # data preperation

                  library(RColorBrewer)
                  library(ggmap)
                  library(gganimate)
                  library(ggrepel)
                  library(ggthemes)
                  library(transformr)
                  library(tidyverse)
                  library(wbstats)
                  library(patchwork)
                  library(leaflet)

                  prepare_cases <- function(url, label) {
                    read_csv(url, col_types = cols(
                      .default = col_double(),
                      `Province/State` = col_character(),
                      `Country/Region` = col_character()
                    )) %>%
                      pivot_longer(
                        -one_of("Province/State", "Country/Region", "Lat", "Long"),
                        names_to = "Date",
                        values_to = "Count") %>%
                      mutate(Count = as.integer(Count)) %>%
                      mutate(Date = lubridate::mdy(Date), type = label) %>%
                      janitor::clean_names()

                  }

                  confirmed_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
                  death_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
                  recovered_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"

                  confirmed_world <- prepare_cases(confirmed_url, "confirmed")
                  death_world <- prepare_cases(death_url, "dead")
                  recovered_world <- prepare_cases(recovered_url, "recovered")

                  combined_world <- union_all(confirmed_world, death_world) %>%
                    union_all(recovered_world)

                  wide_world <- combined_world %>%
                    pivot_wider(names_from = "type", values_from = "count", values_fill = list(count = 0L))

                  uk_oversea_and_crown_dependencies <- wide_world %>%
                    select(province_state, country_region) %>%
                    filter(country_region == "United Kingdom" & !is.na(province_state)) %>%
                    distinct() %>%
                    magrittr::extract2("province_state")

                  uk_oversea_and_crown_dependencies

                  countries_of_interest <- c("China", "Italy", "Spain", "US", "Germany", "United Kingdom")

                  covid19 <- wide_world %>%
                    filter(country_region %in% countries_of_interest) %>%
                    filter(!(country_region == "United Kingdom" & province_state %in% uk_oversea_and_crown_dependencies))



                  #' The number of confirmed cases over time for a selection of countries

                  lastday <- max(covid19$Date)
                  lastday <- "2020-03-22"

                  totals_by_country_and_date <- covid19 %>%
                    group_by(country_region, date) %>%
                    summarise_at(c("confirmed", "dead", "recovered"), sum)

                  max(totals_by_country_and_date$confirmed)

                  # CHN-1392730000;DEU-82905782;ITA-60421760;ESP-46796540;GBR-66460344;USA-326687501

                  population = wb(indicator = "SP.POP.TOTL", startdate = 2018, enddate = 2018)

                  totals_by_country_and_date$population <- rep(NA, nrow(totals_by_country_and_date))

                  totals_by_country_and_date[totals_by_country_and_date$country_region == "China",][,"population"]<- "1392730000"
                  totals_by_country_and_date[totals_by_country_and_date$country_region == "Germany",][,"population"]<- "82905782"
                  totals_by_country_and_date[totals_by_country_and_date$country_region == "Italy",][,"population"]<- "60421760"
                  totals_by_country_and_date[totals_by_country_and_date$country_region == "Spain",][,"population"]<- "46796540"
                  totals_by_country_and_date[totals_by_country_and_date$country_region == "United Kingdom",][,"population"]<- "66460344"
                  totals_by_country_and_date[totals_by_country_and_date$country_region == "US",][,"population"]<- "326687501"

                  totals_by_country_and_date$population <- as.numeric(as.character(totals_by_country_and_date$population))

                  # graph1
                  p_confirmed1 <- ggplot(totals_by_country_and_date, aes(x = date, y = (confirmed/population)*1000000, color = country_region)) +
                    geom_line() +
                    geom_point(shape=9) +
                    labs(x = 'Date', y = 'Infected individuals per million in population', color = 'Country',
                         title = 'Persons infected by COVID-19') + 
                    theme_bw()


                  # graph2
                  p_dead_to_recovered <- ggplot(
                    mutate(totals_by_country_and_date, ratio = dead / recovered),
                    aes(x = date, y = ratio, color = country_region)) +
                    geom_line() +
                    labs(x = 'Date', y = 'Dead / Recovered', color = 'Country',
                         title = 'Ratio of Dead to Recovered Over Time') +
                    theme_bw()

                  #graph3
                  fraction_by_status <- totals_by_country_and_date %>%
                    replace_na(list(dead = 0, recovered = 0)) %>%
                    mutate(combined = dead + recovered) %>%
                    mutate(dead = dead / combined, recovered = recovered / combined) %>%
                    pivot_longer(c("dead", "recovered"), names_to = "status") %>%
                    mutate(status = factor(status, levels = c("dead", "recovered")))

                  p_share_of_status <- ggplot(fraction_by_status, aes(date, value)) +
                    geom_area(aes(fill = status)) +
                    facet_grid(country_region ~ .) +
                    scale_fill_manual(values = c("#d53e4f", "#3288bd")) +
                    theme_bw() +
                    labs(x = 'Date', y = "Fraction", color = 'Status',
                         title = 'Ratio of deaths and recoveries') +
                    theme_bw()+ theme(plot.title = element_text(hjust = 0.5))

                  # gifs
                  p_dead_to_recovered +
                    transition_reveal(date)

                  # anim_save(
                  #   "p_dead_to_recovered.gif",
                  #   p_dead_to_recovered + transition_reveal(date)
                  # )

                  p_confirmed1 +
                    transition_reveal(date)

                  anim_save(
                    "p_confirmed1.gif",
                    p_confirmed1 + transition_reveal(date)
                  )

                  p_share_of_status + transition_reveal(date)

                  anim_save(
                    "p_share_of_status.gif",

                    p_share_of_status + transition_reveal(date)
                  )

                  # geographical distribution

                  cases_by_location_and_date <- wide_world %>%
                    group_by(country_region, lat, long, date) %>%
                    summarise_at(c("confirmed", "dead", "recovered"), sum) %>%
                    ungroup() %>%
                    filter(date == max(date))


                  qmplot(
                    long, lat, data = cases_by_location_and_date, size = confirmed,
                    color = confirmed,
                    alpha = .3,
                    maptype = "toner-lite"
                  ) + labs(title = 'Geographical Distribution of Confirmed Cases')+
                    theme(plot.title = element_text(hjust = 0.5))

                  # another vizualization

                  # http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip)
                  borders_df <- sf::read_sf("TM_WORLD_BORDERS_SIMPL-0.3.shp")

                  convert_to_sf <- function(data, crs = sf::st_crs(4326)) {
                    sf::st_sfc(purrr::map2(data$long, data$lat, ~sf::st_point(c(.x, .y)))) %>%
                      sf::st_sf(data, location = ., crs=crs)
                  }

                  wide_world_sf <- borders_df %>%
                    sf::st_join(
                      convert_to_sf(wide_world) %>% filter(date == max(date)),
                      sf::st_contains
                    ) %>%
                    group_by(NAME) %>%
                    summarise_at(c("confirmed", "dead", "recovered"), ~sum(., na.rm = TRUE)) %>%
                    mutate(label = paste("Confirmed:", confirmed))


                  leaflet::leaflet(data = wide_world_sf) %>%
                    leaflet::addProviderTiles("Stamen.Watercolor") %>%
                    leaflet::addPolygons(
                      # leaflet::colorQuantile
                      fillColor = ~confirmed %>% leaflet::colorBin("YlOrRd", .)(.),
                      fillOpacity = 0.7,
                      highlight = highlightOptions(bringToFront = TRUE),
                      label = ~label
                    )

                  #######################################################################################################
                  #######################################################################################################
                  '''
