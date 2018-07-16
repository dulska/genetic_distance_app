#############################################################
## author: Justyna Dulska                                  ##
## data: 2017.12.11                                        ## 
## Shiny app to appoint the genetic                        ##
## distance and shortest pathway between                   ##     
## different individuals using Dijkstra alghoritm.         ##
#############################################################

#selectedrowindex = 0

########### Server.R #############

shinyServer(function(input, output,session) {
  options(shiny.maxRequestSize=1000*1024^2)
  
  #input data 
  
  fun<- function(myData){ 
    
    dane <- myData$datapath
    #dane <- data.frame(fread(dane, sep=" ", header=T))
    dane <- data.frame(read.csv(dane, sep=",", header=T))
    
    if 
    (colnames(dane)[1]=="IID") 
      rownames(dane) <- dane[,1]
    else
      dane <- dane
    
    if 
    (colnames(dane)[1]=="IID") 
      dane <- dane %>% select(-IID)
    else 
      dane <- dane
    
    
    return(dane)
  }
  
  # the reactive input data where we can make choices which samples are interesting for as
  myData <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    inFile
  }) 
  
  
  New_Input <- reactive({
    
    inFile <- input$file1
    data.frame(read.csv(input$file1$datapath))

  })
  
  
  observeEvent(input$analysis, {
    myData <- myData()
    
    if (is.null(myData$datapath)){
      return(NULL)
    }
    updateSelectInput(session,
                      inputId = "columns",
                      choices = names(New_Input()))
    return(myData)
  })
  
  
  # refresh button
  
  observeEvent(input$refresh_button, {
    js$reset()
  })     
  
  
  # table of data
  
  sampletable <- eventReactive(input$analysis, {
    myData <- myData()
    
    
    if (!is.null(myData)){
      Tab <- fun(myData)
      
      return(Tab)
    }  
  })
  
  # table with selected data
  
  variable <- reactive( {
    
    colindex <- input$columns
    
    selectedrowindex <<- input$sampletable_rows_selected
    
    selectedrowindex <<- as.numeric(unlist(selectedrowindex))
    
    selectedrow <- (round(sampletable(),6)[selectedrowindex,])
    
    selectedrow <- selectedrow[, as.numeric( c(which(colnames(selectedrow) %in% (colindex) )))]
    
    return(selectedrow)
    
  } )
  
  # genetic distance 
  
  distance <- reactive( {
    
    a <- as.data.table(distm(variable())/100)
    a2 <- round(a,0)
    
    dist <- function(a, i){
      
      distance_pig=NULL
      distance_pig$dist <- subset(a2, select = i)
      distance_pig$pig1=i
      distance_pig <- as.data.table(distance_pig)
      distance_pig$pig2 <- 1:dim(distance_pig)[1]
      distance_pig <- as.data.table(distance_pig)
      names(distance_pig) <- c("dist","pig1", "pig2")
      distance_pig <- distance_pig[, .(pig1, pig2, dist)]
      distance_pig <- distance_pig[- grep("0", distance_pig$dist),]
      distance_pig <- as.matrix(distance_pig)
      colnames(distance_pig) <- NULL
      distance_pig[,1] <- as.numeric(distance_pig[,1])
      distance_pig[,2] <- as.numeric(distance_pig[,2])
      distance_pig[,3] <- as.numeric(distance_pig[,3])
      d <- distance_pig
      
      d <- as.data.table(d)
     # names(d) <- c("pig1","pig2", "distance")
      return(d)
    }
    
    
    d <- NULL
    for (w in 1:(dim(a2)[1])){
      
      d[[w]] = (dist(a2, w))
      distance_pig <- rbind(d[[w]])
    }
    
    distance_pig <- NULL
    distance_pig <- rbind(distance_pig , do.call(rbind, d))
    distance_pig <- as.data.table(distance_pig)
    distance_pig2 <- distance_pig[, .(V1, V2)]
    distance_pig2=t(apply(distance_pig2, 1, sort))
    distance_pig2 <- unique(distance_pig2)
    
    distance_pig <- as.data.table(distance_pig)
    distance_pig2 <- as.data.table(distance_pig2)
    
    
    distance_pig <- left_join(distance_pig2, distance_pig)
    
    
    #distance_pig  = distance_pig[!duplicated(distance_pig[,3]),]
    distance_pig <- as.matrix(distance_pig)

    return(distance_pig)
    
  } )
    
    # Dijkstra's pathway 
    dijkstra <- reactive( {
    
      
    dist <- as.data.frame(distance())  
    c <- spread(dist, V2, V3)
    c$V1 <- "999"
    c[(dim(c)[2]),] <- "999"
    
    colnames(c)[colnames(c) == 'V1'] <- '1'
    
    
    for (i in 1:dim(c)[2]){
      c[,i] <- ifelse((is.na(c[,i])), "999", c[,i])
    }
    
    c[1,(dim(c)[2])] <- "999"
    
    
    c <- as.matrix(c)
    class(c) <- "numeric"
    
    # input parameters for function
    numb_od_nodes=(dim(c)[2]) 
    class(numb_od_nodes) <- "numeric"
    s=1 #source node
    dest=numb_od_nodes #destination node
    cost=c #distance matrix
    
    # Dijkstra's algorithm
    dijkstra=function(numb_od_nodes,s,cost,dest){
      
      #create empty variables to store data
      dest = numeric(numb_od_nodes)
      flag = numeric(numb_od_nodes)
      prev = numeric(numb_od_nodes)
      
      # for every node in the network
      for(i in 1:numb_od_nodes){
        prev[i] = -1
        dest[i] = cost[s,i] #= distance:  start node s - every other node in the network
      }
      
      #initialise counter which keeps track of number of steps through network
      count=2
      
      # until we have reached our destination node 
      while(count <= numb_od_nodes){
        min=999
        
        # loop over each node
        for(w in 1:numb_od_nodes){
          #new path is less long than the existing smallest one and flag[w] is equal to zero 
          #(we've not already included that node in route)
          if(dest[w] < min && !flag[w]){
            # updating the new shortest path and counter
            min=dest[w]
            u=w
          }
        }
        flag[u] = 1 #indicate that we go to this site
        count = count+1
        
        # loop over each node again keeping in mind where we have already been
        for(w in 1:numb_od_nodes){
          #if the new route is shorter than the previous route
          if((dest[u]+cost[u,w] < dest[w]) && !flag[w]){
            dest[w]=dest[u]+cost[u,w] #update the distance to destination
            prev[w]=u #keep track of the node visited
          }
        }
      }
      return(prev)
    }
    
    # function which returns path
    savepath = function(f,x){
      path=x
      while(f[x] != -1){
        path=c(path,f[x])
        x=f[x]
        savepath(f,x)
      }
      path=c(path,1)
      return(path)
    }
    
    # Run Dijkstra's algorithm with our distance matrix
    prev = dijkstra(numb_od_nodes,s,cost,dest)
    path = savepath(prev,dest)
    dijkstra_path <- path
    
    return(dijkstra_path)
    
    } )
    

  
  
  plotInput <- reactive ({
    
    a <- as.data.table(distm(variable())/100)
    a2 <- round(a,0)
    g = graph.empty(dim(a2)[1])
    g = add.edges(g,t(distance()[,1:2]),weight=distance()[,3])
   # plot(g)
    
    plot(g,edge.label=distance()[,3])
    
  })
  
  
  plotInput2 <- reactive ({
    
    b <- sampletable()

    plot_by_breed <- ggplot(b, aes(label = as.factor(NR))) +
      geom_point(aes(x=C1, y=C2, color = as.factor(BREED), shape = as.factor(BREED)), size=5) +
      ggtitle("MDS plot - breed") +
      theme_bw()
   # plot_by_breed
   # ggsave(paste0(path,"MDS.plot.breed.jpg"), width = 16, height = 9)
    
   p <- ggplotly(plot_by_breed)
   p
    
  })
  
  
  ##########table_messages##########
  
  
  output$tableMessage1 <- renderUI({
    myData <- myData()
    if (is.null(myData)){
      br()
      h3("Please, upload your file to show the table.")
    }
  })
  
  output$tableMessage2 <- renderUI({
    myData <- myData()
    if (is.null(myData)){
      br()
      h3("Please, select your data in the first table.")
    }
  })
  
  output$tableMessage3 <- renderUI({
    myData <- myData()
    if (is.null(myData)){
      br()
      h3("Please, upload your file to show the heatmap.")
    }
  })
  
  output$tableMessage4 <- renderUI({
    myData <- myData()
    if (is.null(myData)){
      br()
      h3("Please, upload your file to show the table.")
    }
  })
  
  output$tableMessage5 <- renderUI({
    myData <- myData()
    if (is.null(myData)){
      br()
      h3("Please, upload your file to show the graph.")
    }
  })
  
  output$tableMessage6 <- renderUI({
    myData <- myData()
    if (is.null(myData)){
      br()
      h3("Please, upload your file to create the pathway.")
    }
  })
  
  output$tableMessage7 <- renderUI({
    myData <- myData()
    if (is.null(myData)){
      br()
      h3("Please, upload your file to show the mds-plot.")
    }
  })
  
  ##########tables##########
  
  output$selectedData.csv <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(round(variable(),2), file)
      
    }
  )
  
  
  output$sampletable <- DT::renderDataTable({
    
    round(sampletable(),6)
    
  }, server = TRUE, selection = 'multiple')
  
  
  
  
  
  output$selectedrow <- DT::renderDataTable({
    
    round(variable(), 6)

  })
  
  output$distancetable <- DT::renderDataTable({
    
    h <- round(distance(), 6)
    h <- as.data.table(h)
    names(h) <- c("number pig 1", "number pig 2", "genetic distance")
    h
    
  })
  
  output$graph <- renderPlot({
    
    
    print(plotInput())
    
    
  })
  
  output$mds <- renderPlotly({
    
    
    print(plotInput2())
    
    
  })
  
  output$dijkstra <- renderPrint({
    
    print(dijkstra())
  })
  
  output$heatmap <- renderD3heatmap({
    
    d3heatmap(variable(), scale = "column")
    
  })
  

})





