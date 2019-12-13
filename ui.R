library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(haven)
library(cluster)
library(rgl)
library(tree)

shinyUI(navbarPage("Glazy Data Explorer",
                   tabPanel("About", 
                            withMathJax(), 
                            h1("About"),
                            
                            h2("Glazy Dataset"),
                            p("Through this Shiny app, we will explore the characteristics of Glazy data through a few different extracts of the database. ", a("Glazy", href='https://glazy.org/'), " is an open-source database of glaze recipes for ceramics. There are many chemical constituents in a given glaze which influence the color, opacity, and surface characteristics of a glaze. In addition, this chemistry affects the firing temperature range and the unique outcomes of firing within that range. "),
                            p("The recipes in the Glazy database come primarily from American potters' collections starting in the 1970s. Many of these glazes originate much earlier and hail from other regions such as China, Korea, or Japan. As a result, this data has some duplicate or nearly duplicate recipes. Furthermore, given that the recipes are primarily submitted by American potters, they will reflect a similar style and use Western-style firing temperatures more often than others. Another thing to note is that firing temperatures are typically measured by Orton pyrometric cones. Pyrometric cones measure a ", em("temperature equivalent"), ", not merely temperature. Cones bend according to the temperature in the kiln and the rate of temperature increase. Typically, firing temperature is kept between two Orton cones. In an effort to make these a bit more interpretable (cone numbers decrease from 022 to 01, then increase from 1 to 14 as temperature equivalent increases), we will convert these cone values to \\(^{\\circ}F\\) for the slow rate, i.e. \\(27^{\\circ}F/hour\\). " ),
                            
                            h3("Data Sources"),
                            tags$ul(
                              tags$li("All: All glazes, composites, and primitive materials found in the Glazy open source database, no additional cleaning performed. Extract found ", a('here.', href='https://github.com/derekphilipau/glazy-data/')),
                              tags$li("Glazes: contains only glazes, no additional cleaning performed. Extract found ", a('here.', href='https://github.com/derekphilipau/glazy-data/')),
                              tags$li("Composites: contains only composites, any recipe made up of multiple materials. No additional cleaning performed. Extract found ", a('here.', href='https://github.com/derekphilipau/glazy-data/'))
                            ),
                            
                            h3("Field Definitions"),
                            tags$ul(
                              tags$li('id: Material/Recipe ID in Glazy'),
                              tags$li('name: Name of material'),
                              tags$li('created_by_user_id: The user ID in Glazy'),
                              tags$li('material_type_id: Categorizes the material, e.g. "Celadon". '), 
                              tags$li('material_type: Name of the material category.  May have duplicates, as material_type is a tree.  e.g. "Celadon -> Blue" and "Blue"'),
                              tags$li('material_type_concatenated: Full name of the material category.  Includes the full Material Type path separated by " - ", e.g. "Iron - Celadon - Blue"'),
                              tags$li("material_state_id: ID of the material's state: Testing, Production, or Discontinued"),
                              tags$li('material_state: Testing, Production, or Discontinued'),
                              tags$li('rgb_r, rgb_g, rgb_b: RGB values that represent the color of the glaze.  Usually taken from a photo.  Not reliable.'),
                              tags$li('surface_type: indicates the surface finish/texture.'),
                              tags$li('transparency_type: indicates the color transparency. '),
                              tags$li('from_orton_cone, to_orton_cone: The temperature range for the material'),
                              tags$li('is_analysis: If true, this is simply a chemical analysis, for example an analysis of a Song Dynasty celadon glaze.'),
                              tags$li('is_primitive: Primitive materials are actual materials such as EPK, Whiting, Silica, etc.'),
                              tags$li('is_theoretical: Theoretical materials are primitive materials like Kaolin and Potash Feldspar that are based on formulas.'),
                              tags$li('is_private: If true, only visible to the user who created this material.'),
                              tags$li('SiO2_percent, Al2O3_percent, B2O3_percent, etc.: Percentage analysis'),
                              tags$li('Al2O3_umf, B2O3_umf, etc.:  UMF analysis'),
                              tags$li('Al2O3_percent_mol, B2O3_percent_mol, etc: Percent Mol analysis'),
                              tags$li('SiO2_Al2O3_ratio_umf: Silica Alumina ratio'),
                              tags$li('R2O_umf, RO_umf: Totals for R2O and RO values in UMF'),
                              tags$li('loi: Loss on Ignition')),
                            
                            h2("Glazy Data Explorer App"),
                            p("Using the above data, this app will let you explore the data through some basic statistics and graphs, export the data as a CSV file or export graphics, perform PCA on a few noteworthy variables, and model either transparency or surface texture using a few of the chemical and temperature data points. In the future, I will clean some of the underlying data with known issues. They are numerous, and there are many duplicate entries with only minor edits to recipes.  ")
                   ),
                   
                   tabPanel("Data Explorer", 
                            
                            # Sidebar with options for the data set
                            sidebarLayout(
                              sidebarPanel(
                                h3("Select the visualization options:"),
                                selectizeInput("source", "Data Source", selected = "All", 
                                               choices = c("All", "Glazes", "Composites")),
                                helpText("Note: files are quite large. It may take up to 2-3 minutes for results to appear."),
                                br(),
                                selectizeInput("type", "Type of Visualization", selected="Histogram", 
                                               choices=c('Histogram', 'Boxplot')),
                                checkboxInput("isChem", "Investigate chemical variable?"),
                                conditionalPanel('input.isChem == 1', 
                                                 selectizeInput("chemSelect", "Select Chemical", choices=c("SiO2","Al2O3","B2O3","Li2O","K2O","Na2O","KNaO","BeO","MgO","CaO","SrO","BaO","ZnO","PbO","P2O5","F","V2O5","Cr2O3","MnO","MnO2","FeO","Fe2O3","CoO","NiO","CuO","Cu2O","CdO","TiO2","ZrO","ZrO2","SnO2","HfO2","Nb2O5","Ta2O5","MoO3","WO3","OsO2","IrO2","PtO2","Ag2O","Au2O3","GeO2","As2O3","Sb2O3","Bi2O3","SeO2","La2O3","CeO2","PrO2","Pr2O3","Nd2O3","U3O8","Sm2O3","Eu2O3","Tb2O3","Dy2O3","Ho2O3","Er2O3","Tm2O3","Yb2O3", "Lu2O3")),
                                                 selectizeInput("chemValType", "Select Chemical Measurement Type", choices=c("Percent", "Percent Mol", "UMF")
                                                                )
                                ),
                                conditionalPanel('input.isChem == 0', 
                                                 selectizeInput("var", "Select Variable to Analyze", choices=c("from_orton_cone", "to_orton_cone", "loi", "SiO2_Al2O3_ratio_umf")
                                                                )
                                                 ), 
                                conditionalPanel('input.type == "Boxplot"', 
                                                 selectizeInput('catVar', 'Select Categorical "Slicing" Variable', choices=c('material_state', 'surface_type', 'transparency_type'))
                                                 ), downloadButton('Download', "Download Image"), downloadButton('DownloadData', "Download Data")
                              ),
                              
                              # Show outputs
                              mainPanel(
                                h2("Visualization"),
                                plotOutput("viz",
                                           click = "plot_click",
                                           dblclick = "plot_dblclick",
                                           hover = "plot_hover",
                                           brush = "plot_brush"),
                                conditionalPanel('input.type == "Histogram"', 
                                                 p("When using a histogram visualization, you can select a subset of the data for csv download. To do so, select the desired region. Variable values of the selected region will be used to subset the data before download. This will allow you to investigate outlier regions which seem erroneous."),
                                                 textOutput("info")
                                                 ),
                                h2("Summary Statistics"),
                                tableOutput("table")
                              )
                            )),
                   
                   tabPanel("Clustering", sidebarLayout(
                     sidebarPanel(
                       selectizeInput("source", "Data Source", 
                                      choices = c("All", "Glazes", "Composites")),
                       selectizeInput('distForm', 'Select Desired Clustering Method', choices= c("average", "single", "complete", "ward")),
                       uiOutput('xcol'),
                       uiOutput('ycol'),
                       helpText("Note: Plot is oversized to allow for easier reading of content. Identifiers are a concatenation of id and the first 24 characters of the name field. ")
                       
                     ), 
                     mainPanel(
                       plotOutput('biplt')
                     )
                   )),
                   

                   tabPanel("Model", sidebarLayout(
                     sidebarPanel(
                       selectizeInput("source", "Data Source",
                                      choices = c("All", "Glazes", "Composites")),
                       selectizeInput('supModel', 'Select Supervised Learning Model', choices= c("KNN Classifier", "Random Forest Classifier")),
                       selectizeInput("tarVar", "Select target variable", choices=c('surface_type', 'transparency_type')),
                       selectizeInput('outputType', "Select Desired Output", choices=c("Model Stats", "Frequency Table", "New Prediction")),
                       conditionalPanel('input.supModel == "Random Forest Classifier"',
                                        sliderInput("treeCount", "Select number of trees for model training",
                                                    min = 100, max = 1000, value = 100, step = 10)),
                       conditionalPanel('input.supModel == "KNN Classifier"',
                                        sliderInput("kVal", "Select value for K (how many neighbors should we use to estimate the point)",
                                                    min = 1, max = 24, value = 3, step = 1)),
                       sliderInput("ratio", "What percentage of the data should we use for training?",
                                   min=0.5, max=0.9, value=0.7, step=0.01),

                     ),
                     mainPanel(
                       conditionalPanel('input.outputType == "Model Stats"', 
                                        h1("Model Stats"), 
                                        tableOutput("stats")),
                       
                       conditionalPanel('input.outputType == "Frequency Table"', 
                                        h1("Frequency Table of Actual and Predicted"), tableOutput("results")),
                       
                       conditionalPanel('input.outputType == "New Prediction"', 
                                        h1("New Prediction"),
                                       sliderInput('from_orton_cone', 'from_orton_cone', min=1080, max = 2389, step=1, value=1080), 
                                       sliderInput('SiO2_percent', 'SiO2_percent', min=0, max = 100, step=1, value = 2), 
                                       sliderInput('Al2O3_percent', 'Al2O3_percent', min=0, max = 100, step=1, value = 2), 
                                       sliderInput('B2O3_percent', 'B2O3_percent',  min=0, max = 100, step=1, value = 2), 
                                       sliderInput('Li2O_percent', 'Li2O_percent', min=0, max = 100, step=1, value = 2), 
                                       sliderInput('K2O_percent', 'K2O_percent', min=0, max = 100, step=1, value = 2), 
                                       sliderInput('Na2O_percent', 'Na2O_percent', min=0, max = 100, step=1, value = 2), 
                                       sliderInput('KNaO_percent', 'KNaO_percent',min=0, max = 100, step=1, value = 2), 
                                       sliderInput('BeO_percent','BeO_percent',  min=0, max = 100, step=1, value = 2), 
                                       sliderInput('MgO_percent', 'MgO_percent',min=0, max = 100, step=1, value = 2), 
                                       sliderInput('CaO_percent', 'CaO_percent', min=0, max = 100, step=1, value = 2), 
                                       sliderInput('SrO_percent', 'SrO_percent', min=0, max = 100, step=1, value = 2), 
                                       sliderInput('BaO_percent', 'BaO_percent',min=0, max = 100, step=1, value = 2), 
                                       sliderInput('ZnO_percent', 'ZnO_percent', min=0, max = 100, step=1, value = 2), 
                                       sliderInput('PbO_percent', 'PbO_percent', min=0, max = 100, step=1, value = 2),
                                       h2(textOutput("prediction")))
                     )
                   )),
                   
                   tabPanel("Just the data", 
                            sidebarLayout(
                              sidebarPanel(
                                selectizeInput("source", "Data Source", 
                                               choices = c("All", "Glazes", "Composites")),
                                actionLink("selectall","Select All"),
                                uiOutput('varControl')
                              ),
                              mainPanel(
                                dataTableOutput("modTable"), 
                                downloadButton('longDownload', "Download Filtered Dataset")
                              )
                            )
                            )
))