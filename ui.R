library(ggplot2)
library(dplyr)

shinyUI(navbarPage("Glazy Data Explorer",
                   tabPanel("About", 
                            withMathJax(), 
                            h1("About"),
                            
                            h2("Glazy Dataset"),
                            p("Through this Shiny app, we will explore the characteristics of Glazy data through a few different extracts of the database. ", a("Glazy", href='https://glazy.org/'), " is an open-source database of glaze recipes for ceramics. There are many chemical constituents in a given glaze which influence the color, opacity, and surface characteristics of a glaze. In addition, this chemistry affects the firing temperature range and the unique outcomes of firing within that range. "),
                            p("The recipes in the Glazy database come primarily from American potters' collections starting in the 1970s. Many of these glazes originate much earlier and hail from other regions such as China, Korea, or Japan. As a result, this data has some duplicate or nearly duplicate recipes. Furthermore, given that the recipes are primarily submitted by American potters, they will reflect a similar style and use Western-style firing temperatures more often than others. " ),
                            
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
                            p("Using the above data, this app will let you explore the data through some basic statistics and graphs, export the data as a CSV file or export graphics, perform PCA on a few noteworthy variables, and model... ")
                   ),
                   tabPanel("Data Explorer", 
                            
                            # Sidebar with options for the data set
                            sidebarLayout(
                              sidebarPanel(
                                h3("Select the visualization options:"),
                                selectizeInput("source", "Data Source", selected = "All", 
                                               choices = c("All", "Glazes", "Composites")),
                                br(),
                                selectizeInput("type", "Type of Visualization/Summary", selected="Histogram", 
                                               choices=c('Histogram', 'Bivariate Scatter Plot', 'Summary Statistics')),
                                selectizeInput("chemSelect", "Select Chemical", choices=c("SiO2","Al2O3","B2O3","Li2O","K2O","Na2O","KNaO","BeO","MgO","CaO","SrO","BaO","ZnO","PbO","P2O5","F","V2O5","Cr2O3","MnO","MnO2","FeO","Fe2O3","CoO","NiO","CuO","Cu2O","CdO","TiO2","ZrO","ZrO2","SnO2","HfO2","Nb2O5","Ta2O5","MoO3","WO3","OsO2","IrO2","PtO2","Ag2O","Au2O3","GeO2","As2O3","Sb2O3","Bi2O3","SeO2","La2O3","CeO2","PrO2","Pr2O3","Nd2O3","U3O8","Sm2O3","Eu2O3","Tb2O3","Dy2O3","Ho2O3","Er2O3","Tm2O3","Yb2O3", "Lu2O3"))
                              ),
                              
                              # Show outputs
                              mainPanel(
                                plotOutput("viz"),
                                tableOutput("table")
                              )
                            )),
                   tabPanel("Clustering"),
                   tabPanel("Model"),
                   tabPanel("Just the data")
))