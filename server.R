library(shiny)
library(dplyr)
library(ggplot2)

shinyServer(function(input, output, session) {
  
  #get data for only order specified
  getData <- reactive({
    src <- input$source
    
    if (src == 'All') {
      newData <- read.csv('Data/glazy-data-all-20191107.csv', na = "NULL")
    } else if (src == 'Glazes') {
      newData <- read.csv('Data/glazy-data-glazes-20191107.csv', na = "NULL")
    } else if (src == 'Composites') {
      newData <- read.csv('Data/glazy-data-composites-20191107.csv', na = "NULL")
    } 
    
    coneConvert<- function(x) switch(as.character(x), '022'=1087, '021'=1112, '020'=1159, '019'=1213, '018'=1267, '017'=1301, '016'=1368, '015'=1382,  '014'=1395, '013'=1485, '012'=1549, '011'=1575,  '010'=1636, '09'=1665, '08'=1692,  '07'=1764,  '06'=1798, '05 &#189;'=1839, '05'=1870, '04'=1915, '03'=1960,  '02'=1972, '01'=1999, '1'=2028, '2'=2034, '3'=2039, '4'=2086, '5'=2118, '5 &#189;'=2133, '6'=2165, '7'=2194, '8'=2212, '9'=2235, '10'=2284, '11'=2322, '12'=2345, '13'=2389, '14'=2464, 'NA'=NA )
    newData$from_orton_cone<- sapply(newData$from_orton_cone, coneConvert)
    newData$to_orton_cone<- sapply(newData$to_orton_cone, coneConvert)
    
    return (newData)
  })
  
  output$table <- renderTable({
    #get data
    df <- getData()
    head(df, 100)
  })
  
  output$viz<- renderPlot({
    #get data
    df <- getData()
    
    #base plotting object
    g <- ggplot(df, aes(x = from_orton_cone, y = to_orton_cone)) + geom_point()
    g
  })
  
})
