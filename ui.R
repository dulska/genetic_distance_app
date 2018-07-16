list.of.packages <- c("DT","data.table", "stringr", "stringi", "dplyr", "ggplot2","shiny", "openxlsx", "d3heatmap", "shinyjs", "reshape2", "plyr", "scales", "V8", "geosphere", "network", "igraph", "tidyr", "plotly")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

Vectorize(require)(package = c("DT","data.table", "stringr", "stringi", "dplyr", "ggplot2","shiny", "openxlsx", "d3heatmap", "shinyjs", "reshape2", "plyr", "scales", "V8", "geosphere", "network", "igraph", "tidyr", "plotly"), character.only = TRUE)


##########UI##########

jsResetCode <- "shinyjs.reset = function() {history.go(0)}"
style = "background-color:  #DBFCBA"
shinyUI(fluidPage(style = "background-color:  #ffffff; irs-line: #000000",
                  
                  
                  headerPanel(h1("Genetic Distance App ", 
                                 style = "font-family: 'Lobster', cursive;
                                 font-weight: 1200; color: #000000; text-align: center;")),
                  br(),
                  br(),
                  br(),
                  tags$hr(),
                  sidebarLayout(
                    sidebarPanel( style = "background-color:  #a5a4a4;",
                                  fileInput("file1", label = h4("Choose your file (.csv):"), multiple = FALSE, accept = c(".csv")),
                                  
                                  tags$hr(),
                                  h4("Please, push the button to start the analysis."),
                                  actionButton("analysis", "Run"),
                                  
                                  
                                  tags$hr(),
                                  selectInput ("columns",h4("Choose two coordinates:"),
                                               choices="",    selected=TRUE, multiple = TRUE),
                                  
                                  
                                  useShinyjs(),                                        
                                  extendShinyjs(text = jsResetCode), 

                                  tags$hr(),
                                  downloadButton('selectedData.csv', 'Download Selected Data (.csv)'),
                                  actionButton("refresh_button",  "Refresh")
                                  
                    ),
                    
                    
                    mainPanel(
                      mainPanel(style = "background-color:  #ffffff;", tabsetPanel(type = "tabs",
                              
                            tabPanel("Description", uiOutput("descript"),
                             br(),
                             
                             h4("This is a Shiny app to appoint the genetic distance and shortest pathway between different individuals using Dijkstra alghoritm.", 
                             style ="font-weight: 500; line-height: 1.1; color: #000000;"),
                             br(),
                                   h4("Data Format  :", 
                                      style ="font-weight: 500; line-height: 1.1; color: #000000;"),
                                   p("- ",tags$b("'.csv'"), "are requied. ", 
                                     style ="font-weight: 500; line-height: 1.1; color: #000000;"), 
                                                                                            
                                   p("- First column must be the names of samples ",tags$b("'IID'"),".", 
                                     style ="font-weight: 500; line-height: 1.1; color: #000000;"), 
                                                                                            
                                   p("- Another columns have to contain number of a specific individual" ,tags$b("'NR'"),", number identifying the species" ,tags$b("'BREED'"), "and genetic coordinates values" ,tags$b("'C1', 'C2', etc."), "(in this order).", 
                                     style ="font-weight: 500; line-height: 1.1; color: #000000;"),
                             br(),
                             br(),
                             br(),
                             p(em("Author: Justyna Dulska", 
                               style ="font-weight: 500; line-height: 1.1; color: #000000;")) 
                                                                                            
                             ),
                            
                            tabPanel("MDS",uiOutput("tableMessage7"),
                                     br(),
                                     h5("Mds-plot presents the position od the samples in the space od coordinates C1 - C2.", 
                                        style ="font-weight: 500; line-height: 1.1; color: #000000;"),
                                     br(),
                                    # h5("The breeds are presented as numbers, that describe:", 
                                    #    style ="font-weight: 500; line-height: 1.1; color: #000000;"),
                                    # h5("~ 1 - 'Zlotnicka biala' pig.", 
                                    #    style ="font-weight: 500; line-height: 1.1; color: #000000;"),
                                    # h5("~ 2 - 'Polska biala zwisloucha' pig.", 
                                    #    style ="font-weight: 500; line-height: 1.1; color: #000000;"),
                                    # h5("~ 3 - 	'Zlotnicka pstra' pig.", 
                                    #    style ="font-weight: 500; line-height: 1.1; color: #000000;"),
                                    # h5("~ 4 - 'Pulawska' pig.", 
                                    #    style ="font-weight: 500; line-height: 1.1; color: #000000;"),
                                    # br(),
                                     plotlyOutput("mds"),
                                     br()
                            ),
                            
                             tabPanel("Interactive Table", uiOutput("tableMessage1"),
                                      br(),
                                      h5("Input data table. Please, select interesting individuals from the table.", 
                                         style ="font-weight: 500; line-height: 1.1; color: #000000;"),
                                      br(),
                                      DT::dataTableOutput("sampletable")
                             ),
                            

                        
                                                                                   
                             tabPanel("Selected data", uiOutput("tableMessage2"),
                                      br(),
                                      h5("Table with selected individuals :", 
                                         style ="font-weight: 500; line-height: 1.1; color: #000000;"),
                                      br(),
                                      DT::dataTableOutput("selectedrow")
                             ),
                            
                            tabPanel("Heatmap",uiOutput("tableMessage3"),
                                     br(),
                                     h5("The heatmap shows the standardized  data value for each row and column. Any patterns in the heat map may indicate an association between the rows and the columns. It is simply a histogram of all the values, and how they correspond to the specified heatmap colour range.", 
                                        style ="font-weight: 500; line-height: 1.1; color: #000000;"),
                                     br(),
                                     h5("The heatmap for chosen data is visible below:", 
                                        style ="font-weight: 500; line-height: 1.1; color: #000000;"),
                                     br(),
                                     d3heatmapOutput("heatmap"),
                                     br()
                            ),
                                                                                   
                             tabPanel("Genetic distances table", uiOutput("tableMessage4"),
                                      br(),
                                      h5("Table showing the genetic distances between choosen individuals ( presented by numbers):", 
                                         style ="font-weight: 500; line-height: 1.1; color: #000000;"),
                                      br(),
                                      DT::dataTableOutput("distancetable")
                             ),
                                                                                   
                             tabPanel("Graph",uiOutput("tableMessage5"),
                             br(),
                             h5("A graph showing the nodes and the cost of their connection (genetic distances between indyvisuals):", 
                                style ="font-weight: 500; line-height: 1.1; color: #000000;"),
                             br(),
                             plotOutput("graph"),
                             br()
                             ),
                            
                            tabPanel("Dijkstra pathway", uiOutput("tableMessage6"),
                                     br(),
                                     h5("Pathway between first and last nodes designated by Dijkstra alghoritm (omission first - last connection):", 
                                        style ="font-weight: 500; line-height: 1.1; color: #000000;"),
                                     br(),
                                     textOutput("dijkstra")
                            
                             
                                                                                          
                             )
                       
                            ))
                           )
                           )
          )
    )