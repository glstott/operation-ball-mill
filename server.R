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
  
  getVar<- reactive({
    if(input$isChem == 1){
      chemType<- switch(input$chemValType, "Percent"="_percent", "Percent Mol"="_percent_mol", "UMF"="_umf")
      var<- paste(input$chemSelect, chemType, sep = '')
    } else {
      var<- input$var
    }
    return(var)
  })
  
  output$table <- renderTable({
    sumstats = function(x) { 
      null.k <- function(x) sum(is.na(x))
      unique.k <- function(x) {if (sum(is.na(x)) > 0) length(unique(x)) - 1
        else length(unique(x))}
      range.k <- function(x) max(x, na.rm=TRUE) - min(x, na.rm=TRUE)
      mean.k=function(x) {if (is.numeric(x)) round(mean(x, na.rm=TRUE), digits=2)
        else "N*N"} 
      sd.k <- function(x) {if (is.numeric(x)) round(sd(x, na.rm=TRUE), digits=2)
        else "N*N"} 
      min.k <- function(x) {if (is.numeric(x)) round(min(x, na.rm=TRUE), digits=2)
        else "N*N"} 
      q05 <- function(x) quantile(x, probs=.05, na.rm=TRUE)
      q10 <- function(x) quantile(x, probs=.1, na.rm=TRUE)
      q25 <- function(x) quantile(x, probs=.25, na.rm=TRUE)
      q50 <- function(x) quantile(x, probs=.5, na.rm=TRUE)
      q75 <- function(x) quantile(x, probs=.75, na.rm=TRUE)
      q90 <- function(x) quantile(x, probs=.9, na.rm=TRUE)
      q95 <- function(x) quantile(x, probs=.95, na.rm=TRUE)
      max.k <- function(x) {if (is.numeric(x)) round(max(x, na.rm=TRUE), digits=2)
        else "N*N"} 
      
      sumtable <- cbind(as.matrix(colSums(!is.na(x))), sapply(x, null.k), sapply(x, mean.k), sapply(x, sd.k),
                        sapply(x, min.k), sapply(x, q05), sapply(x, q10), sapply(x, q25), sapply(x, q50),
                        sapply(x, q75), sapply(x, q90), sapply(x, q95), sapply(x, max.k)) 
      
      sumtable <- as.data.frame(sumtable); names(sumtable) <- c('count', 'null', 
                                                                'mean', 'std', 'min', '5%', '10%', '25%', '50%', '75%', '90%',
                                                                '95%', 'max') 
      return(sumtable)
    } 
    #get data
    df <- getData()
    sumstats(df[getVar()])
  })
  
  pltgen<- reactive({
    #get data
    df <- getData()
    var<- getVar()
    
    if (input$type == 'Histogram'){
      g <- ggplot(df, aes_string(x = var)) + geom_histogram()
    } else if (input$type == 'Boxplot'){
      g <- ggplot(df, aes_string(y = var, x=input$catVar)) + geom_boxplot()
    } 
    
    g
  })
  
  output$viz<- renderPlot({
    pltgen()
  })
  
  output$Download <- downloadHandler(
    filename = function(){
      paste(input$source, '-', input$type, '-', getVar(), '.png', sep = '')
    },
    content = function(file){
      ggsave(file, plot = pltgen(), device = 'png')
    }
  )
  
  output$DownloadData <- downloadHandler(
    filename = function() {
      paste(input$source, ".csv", sep = "")
    },
    content = function(file) {
      
      df<- getData()
      var<- getVar()
      print(input$plot_brush)
      if (!is.null(input$plot_brush) & input$type=='Histogram'){
        df<- df %>% filter(get(var) >= input$plot_brush$xmin & get(var) <= input$plot_brush$xmax )
        print(head(df))
      } else {
        df
      }
      
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  
  
  output$info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0( getVar(), " min=", round(e$xmin, 1)," | ", getVar(), " max=", round(e$xmax, 1))
    }
    
    paste0(
      "Selected Region for data download: \n", xy_range_str(input$plot_brush)
    )
  })
  
})
