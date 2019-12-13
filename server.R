library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(haven)
library(cluster)
library(rgl)
library(class)
library(randomForest)
library(gsubfn)
library(mltest)

shinyServer(function(input, output, session) {
  
  #---------------------------------------------------------------------------------------------------------------
  # --------------------------- General Functions -----------------------------------------------------------------------
  #---------------------------------------------------------------------------------------------------------------
  # Get the data according to user input of source file
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
  
  #---------------------------------------------------------------------------------------------------------------
  # --------------------------- Data Explorer -----------------------------------------------------------------------
  #---------------------------------------------------------------------------------------------------------------
  
  # For exploratory data, get the variable selection for chemical variables (adds a more interactive ui)
  getVar<- reactive({
    if(input$isChem == 1){
      chemType<- switch(input$chemValType, "Percent"="_percent", "Percent Mol"="_percent_mol", "UMF"="_umf")
      var<- paste(input$chemSelect, chemType, sep = '')
    } else {
      var<- input$var
    }
    return(var)
  })
  
  # For exploratory data, get summary of univariate characteristics
  output$table <- renderTable({
    # Summary Stat function thanks to a helpful question on stack overflow: https://stackoverflow.com/a/38892880
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
    # end of stack overflow function
    
    # get data and summarize
    df <- getData()
    sumstats(df[getVar()])
  })
  
  # generate plot for histogram or boxplot on data exploration page
  pltgen<- reactive({
    #get data
    df <- getData()
    var<- getVar()
    
    # Check user input and use to determine plot characteristics
    if (input$type == 'Histogram'){
      g <- ggplot(df, aes_string(x = var)) + geom_histogram()
    } else if (input$type == 'Boxplot'){
      g <- ggplot(df, aes_string(y = var, x=input$catVar)) + geom_boxplot()
    } 
    
    g
  })
  
  # Render visualization for eda page
  output$viz<- renderPlot({
    pltgen()
  })
  
  # Generate image to be downloaded of the plot
  output$Download <- downloadHandler(
    filename = function(){
      paste(input$source, '-', input$type, '-', getVar(), '.png', sep = '')
    },
    content = function(file){
      ggsave(file, plot = pltgen(), device = 'png')
    }
  )
  
  #generate the csv file basekd upon user selection for the data explorer page
  output$DownloadData <- downloadHandler(
    filename = function() {
      paste(input$source, ".csv", sep = "")
    },
    content = function(file) {
      
      df<- getData()
      var<- getVar()
      
      # Check that a selection is made. Default to the entire dataset if not. otherwise filter and output to df.
      if (!is.null(input$plot_brush) & input$type=='Histogram'){
        df<- df %>% filter(get(var) >= input$plot_brush$xmin & get(var) <= input$plot_brush$xmax )
        print(head(df))
      } else {
        df
      }
      
      # write results to the csv file
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  # Print the selected region of the desired variable for user so they know before download
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
  
  #---------------------------------------------------------------------------------------------------------------
  # --------------------------- Clustering -----------------------------------------------------------------------
  #---------------------------------------------------------------------------------------------------------------
  
  # For the clustering section, UI to select the x variable, filtering out the bad options.
  output$xcol<- renderUI({
    df<- getData()
    x<- colnames(select_if(df, is.numeric))
    x<- x[!startsWith(x, 'is_') & !startsWith(x, 'rgb') & !endsWith(x, 'id')]
    selectInput('xcol', 'Select the X Variable', x)
  })
  
  # For the clustering section, UI to select the y variable, filtering out the bad options and the selected X value.
  output$ycol<- renderUI({
    df<- getData()
    x<- colnames(select_if(df, is.numeric))
    x<- x[!startsWith(x, 'is_') & !startsWith(x, 'rgb') & !endsWith(x, 'id') & x!= input$xcol]
    selectInput('ycol', 'Select the Y Variable', x)
  })
  
  # Render the dendogram for the user
  output$biplt<- renderPlot({
    df<- getData()
    df<- na.omit(df)
    df<- df %>% mutate_if(is.numeric, scale)
    rownames(df)<- paste(df$id, substr(df$name, 1, 24), sep = '-')
    df$name<- NULL
    hierClust <-agnes(df[, c(input$xcol, input$ycol)], method=input$distForm)
    plot(hierClust, xlab = "", main=paste('Dendogram of', input$xcol, 'and', input$ycol))
  }, height = 1000, width = 2400)
  
  
  #---------------------------------------------------------------------------------------------------------------
  # --------------------------- Modeling   -----------------------------------------------------------------------
  #---------------------------------------------------------------------------------------------------------------
  
  # Getmodel function to make model available elsewhere
  getModel <- reactive({
    #Set the seed, get and filter the data to the needed training and test sets
    set.seed(666)
    df<- getData()
    target<- input$tarVar
    df<- df %>% select(target, from_orton_cone, SiO2_percent, Al2O3_percent, B2O3_percent, Li2O_percent, K2O_percent, Na2O_percent, KNaO_percent, BeO_percent, MgO_percent, CaO_percent, SrO_percent, BaO_percent, ZnO_percent, PbO_percent) %>% replace(., is.na(.), 0) %>% filter(!is.na(get(target)))
    trainVec<- sample(1:nrow(df), size = nrow(df)*input$ratio)
    testVec <- setdiff(1:nrow(df), trainVec)
    train <- df[trainVec, ]
    test <- df[testVec, ]
    mod<- input$supModel
    ts<- select(test, -target)
    
    #Train desired model
    if (mod == 'Random Forest Classifier') {
      if (target == 'surface_type'){
        rfit<- randomForest(surface_type ~ ., data=train,  mtry = ncol(train)/3, ntree=input$treeCount, importance=TRUE)
      } else {
        rfit<- randomForest(transparency_type ~ ., data=train,  mtry = ncol(train)/3, ntree=input$treeCount, importance=TRUE)
      }
      
      testpred<-predict(rfit, newdata=ts, type="response")
    } else if (mod == 'KNN Classifier') {
      rfit<- NULL
      trs<- select(train, -target)
      cl<- train[, target]
      testpred<- knn(train=trs, 
                     test=ts, cl=cl, k=input$kVal)
    }
    
    return (list(mod, train, test, rfit, testpred))
  })
  
  #Output frequency table. Log prints the percent misclass overall.
  output$results <- renderTable({
    list[mod, train, test, rfit, testpred]<- getModel()
    print(nrow(test))
    print(length(testpred))
    target<- input$tarVar
    t1<- table(Prediction=testpred, Actual=test[, target])
    print(1 - sum(diag(t1))/sum(t1))
    t1
  })
  
  #Output the stats table. Uses a function for ml predictions
  output$stats <- renderTable({
    list[mod, train, test, rfit, testpred]<- getModel()
    print(nrow(test))
    print(length(testpred))
    target<- input$tarVar
    t<-ml_test(testpred, test[, target], output.as.table=TRUE)
  }, rownames = TRUE)
  
  #New prediction generator. Read in the sliders, then grab an instance of the model before finding the prediction
  output$prediction <- renderText({
    x<- c(input$from_orton_cone, input$SiO2_percent, input$Al2O3_percent, input$B2O3_percent, input$Li2O_percent, input$K2O_percent, input$Na2O_percent, input$KNaO_percent, input$BeO_percent, input$MgO_percent, input$CaO_percent, input$SrO_percent, input$BaO_percent, input$ZnO_percent, input$PbO_percent)
    list[mod, train, test, rfit, testpred]<- getModel()
    
    if (mod == 'KNN Classifier') {
      y<-knn(train=train[, -1], 
          test=x, cl=train[, 1], k=input$kVal, prob=TRUE)
    } else {
      # A convoluted method to get the column names without manually entering them for the response
      x1<- head(train, 1)
      x1[, -1]<- x
      x1[,1]<- NULL
      y<- predict(rfit, newdata=x1, type="response")
    }
    paste("Your predicted class is: ", y )
    
  })
  
  #---------------------------------------------------------------------------------------------------------------
  # --------------------------- Just The Data -----------------------------------------------------------------------
  #---------------------------------------------------------------------------------------------------------------
  
  # Additional function to select all/none of the variables
  observe({
    df<- getData()
    if(input$selectall == 0) return(NULL) 
    else if (input$selectall%%2 == 0)
    {
      updateCheckboxGroupInput(session, "show_vars", "Columns to show:", names(df), selected = names(df))
    }
    else
    {
      updateCheckboxGroupInput(session, "show_vars", "Columns to show:",
                               names(df))
    }
  })
  
  # Render group input for the list of variables in the table which may be included.
  output$varControl<- renderUI({
    df<- getData()
    checkboxGroupInput("show_vars", "Columns to show:",
                       names(df), selected = names(df))
  })
  
  # render filtered table for the just the data section
  output$modTable <- renderDataTable({
    df<- getData()
    datatable(df[, input$show_vars, drop = FALSE], options = list(orderClasses = TRUE))
  })
  
  # In the just the data section, filtered data output
  output$longDownload<- downloadHandler('Glazy-filtered.csv', content = function(file) {
    s = input$modTable_rows_all
    df<- getData()
    write.csv(df[s, input$show_vars, drop = FALSE], file, row.names = FALSE)
  })
  
})
