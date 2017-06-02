# SET WORKING DIRECTORY!

# Load packages, data and function that only needs to be loaded once

#############
# Packages --------------------------------------------------------------------
#############

library("shiny")
library("psych")
library("qgraph")
library("bootnet")
library("igraph")
library("scales")
library("foreign")
library("readxl")

data(bfi)

#################################################
# NEW FUNCTIONALITY, i.e. community visualization -----------------------------
#################################################

ComDetQ <- function(g,  # qgraph object
                    type = c("spinglass",  # choice between walktrap
                             "walktrap"),  # and spinglass community detection
                    iterate = 15,  # number of steps/spins taken by the algorithm
                    negEdges = FALSE,  # if negative edges are considered by spinglass
                    size = 1.1,  # size of communityclouds
                    res = 100,  # smoothness of community clouds - not used in shiny
                    comCol = c("rainbow",  # color palettes
                               "heat.colors", 
                               "cm.colors", 
                               "topo.colors", 
                               "terrain.colors", 
                               "rainbow_hcl", 
                               "diverge_hcl",
                               "terrain_hcl", 
                               "sequential_hcl"),
                    nodeColored = TRUE # should nodes be additionally set off by color?
                    ){
  
  # Load necessary packages (loaded again because function should run independently)
  library("qgraph")  # important for network plotting
  library("igraph")  # important for community detection
  library("colorspace")  # important for color palettes
  library("scales")   # important for transparent colors
  
  ##################
  # ERROR MESSAGES #
  ##################
  
  if (!type %in% c("walktrap","spinglass")){
    stop("'type' argument must be one of 'walktrap', 'spinglass'")
  }
  # would stop the function if any other comDet was given
  
  if (!comCol %in% c("rainbow",  
                     "heat.colors", 
                     "cm.colors", 
                     "topo.colors", 
                     "terrain.colors", 
                     "rainbow_hcl", 
                     "diverge_hcl",
                     "terrain_hcl", 
                     "sequential_hcl")){
    stop("'comCol' argument must be one of 'rainbow', 'heat.colors',
         'cm.colors', 'topo.colors', 'terrain.colors', 'rainbow_hcl',
         'diverge_hcl', 'terrain_hcl', 'sequential_hcl'")
  }
  # would stop if comCol is not a prespecified color
  
  if (type == "spinglass" & iterate > 100){
    warning("If iterate > 100, spinglass might take a while to estimate.")
  } # warning message if spinglass is used in combination with
  # large number of iterations
  
  if (type == "walktrap" & negEdges){
    warning("Walktrap does not take negative edges into account.")
  } # warning message if walktrap and negEdges is used in combination
  
  if (size > 8){
    stop("'size' argument should not exceed 8.
         Otherwise, your clouds exceed the plotting area.")
  } # stops if size will result in enormous clouds
  
  if (res > 150){
    stop("Decrease the smoothness of your clouds (res).
         Otherwise, plotting takes very long. res should
         not exceed 150.")
  } # self-explanatory
  

  #########################
  ## COMMUNITY DETECTION ##
  #########################
  
  ig <-  as.igraph(g)  # transform qgraph into igraph object to run community detection
  
  ###########################################
  #      Spinglass community detection      #
  ###########################################
  
  if(type == "spinglass"){
    
    if(negEdges == FALSE){  # negative edges not accounted for
      sg <- spinglass.community(ig, spins = iterate)  # determine communities
      comMembership <- sg$membership  # retrieve membership per node
      noMembership <- as.vector(table(sg$membership))  # determine number of membership
    }
    
    if(negEdges == TRUE){  # negative edges accounted for
      sg <- spinglass.community(ig, spins = iterate, implementation = "neg")  # determine communities
      comMembership <- sg$membership  # retrieve membership per node
      noMembership <- as.vector(table(sg$membership))  # determine number of membership
    }
  }
  
  ##########################################
  #      Walktrap community detection      #
  ##########################################
  
  if(type == "walktrap"){
    wk <- walktrap.community(ig, steps = iterate)  # determine communities
    comMembership<- wk$membership  # retrieve membership per node
    noMembership <- as.vector(table(wk$membership))  # determine number of membership
  }
  
  #########################################################
  ##   Function to CALCULATE COEFFICIENTS surrounding    ##
  ##                      the nodes                      ##
  ##            which takes nodesize into account        ##
  ##            with much help by Sacha Epskamp          ##
  #########################################################
  
  Outline <- function(g,  # graph object (as in ComDetQ function)
                      size = 1.1,  # size of the clouds (as in ComDetQ function)
                      res = 100){  # resolution of the clouds 
                                   # (i.e. how smooth should they be)
    
    # Layout - x-y coefficients of node origins
    Layout <- g$layout
    
    # Vsize - width and height of the nodes
    vSize1 <- g$graphAttributes$Nodes$width
    vSize2 <- g$graphAttributes$Nodes$height
    
    # Shape - shape of the nodes
    shape <- g$graphAttributes$Nodes$shape
    
    # Number of nodes - number of nodes in the graph
    nNodes <- g$graphAttributes$Graph$nNodes
    
    # For every node, compute its outline coordinates:
    coeffEdge <- do.call(rbind, # Binds the results of 
                   # (x-y-coordinates of all nodes (number equal to res times number of nodes)):
                   lapply(seq_len(nNodes),  # 1. Creates a vector with length(i) = no of nodes
                          function(i){  # i is then equal to the no of nodes
                            do.call(rbind,  # Binds the results of 
                                    # (all x-y-coordinates (number equal to res) on the edge of one node):
                                    lapply(seq(0, 2*pi, length = res + 1)[-1],  # 2. Creates a vector 
                                           # (i.e. rotation values) with length of 
                                           # res + 1 [minus the first value, which is a collateral value of 0] 
                                           # whose values will be used in the internal qgraph function 
                                           # called Cent2Edge (see beneath)...
                                           # (Sacha Epkamp pointed me to it when I could not figure out 
                                           # a way to adjust the coordinates to different nodeshapes; 
                                           # it took him several weeks to write that function)
                                           function(r){  # r = vector created at 2.
                                             qgraph:::Cent2Edge(
                                               Layout[i, 1],
                                               Layout[i, 2],  # Cent2Edge takes the origin of each node 
                                               # (i.e. coefficients of the center),
                                               r,  # takes the rotation vector 
                                               # r <- seq(0, 2*pi, length = res +1)[-1] (see above)
                                               # and determines the coefficients of a number of points 
                                               # (number equal to the length of res), on the edge of the 
                                               # node with the same distance from the origin using trigonometric 
                                               # functions. See the documentation for a more detailed explanation 
                                               # of the math behind this function and an explanation for why I used it.
                                               cex = size*vSize1[i],
                                               cex2 = size*vSize2[i],   # multiplying the result by nodesize of each node
                                               shape = shape[i])  # taking shape into account.
                                                                  # and increasing the radius/size of the enclosing
                                                                  # object by size = size specified in the outline function
                                           }
                                    )
                            )
                          }
                   )
    )
    
    return(coeffEdge) # Returns the bound list of calculated coefficients
  }
  # Function Outline() computes coefficients of points that are equidistant from
  # the center of each node in the graph. These points are calculated
  # such that each subsequent point is rotated across the full circumference of 
  # the node. Thus, these new points create an outline around the node that can
  # vary in radius. 
  
  ########################################
  # Calculate coordinates of given graph #
  ########################################
  
  coords <- Outline(g, size = size, res = res)
  # Uses the Outline() function to calculate the coefficients surrounding the
  # graph nodes given in the ComDetQ function and the size and resolution 
  # provided there.
  
  ##########################
  # Create a layout matrix #
  ##########################
  
  layoutMatrix <- matrix(c(coords, rep(comMembership, each = res)), ncol = 3)
  # Creates a matrix with 3 columns in which the first two columns represent 
  # the coordinates calculated by Outline() and puts the membershipvector in 
  # the third column after repeating each node membership as many times as 
  # there were coordinates calculated (i.e. res, default = 100)
  
  #################################################
  ### Group coordinates according to membership ###
  #################################################
  
  groupedLayoutMatrix <- list()
  
  for (i in 1:length(unique(comMembership))) {
    groupedLayoutMatrix[[i]] <- subset(layoutMatrix, layoutMatrix[,3] == i)
  }
  # This for loop subsets the layout matrix and creates a list in which each 
  # listelement represents a matrix with only those coefficients in there that
  # belong to one group. If I had more time I would vectorize this as well with
  # the help of lapply.
  
  #######################
  # Define COLORpalette #
  #######################
  
  if (comCol == "rainbow"){
    colVector <- rainbow(length(unique(comMembership)))
  }
  if (comCol == "heat.colors"){
    colVector <- heat.colors(length(unique(comMembership)))
  }
  if (comCol == "cm.colors"){
    colVector <- cm.colors(length(unique(comMembership)))
  }
  if (comCol == "topo.colors"){
    colVector <- topo.colors(length(unique(comMembership)))
  }
  if (comCol == "terrain.colors"){
    colVector <- terrain.colors(length(unique(comMembership)))
  }
  if (comCol == "rainbow_hcl"){
    colVector <- rainbow_hcl(length(unique(comMembership)))
  }
  if (comCol == "diverge_hcl"){
    colVector <- diverge_hcl(length(unique(comMembership)))
  }
  if (comCol == "terrain_hcl"){
    colVector <- terrain_hcl(length(unique(comMembership)))
  }
  if (comCol == "sequential_hcl"){
    colVector <- sequential_hcl(length(unique(comMembership)))
  }
  
  transparency <- c(rep(0.7/length(noMembership), each = length(noMembership)))
  # Adapt transparency of clouds to the amount of communities there are
  
  ###############################
  # PLOT GRAPH WITH COMMUNITIES #
  ###############################
  
  if (nodeColored == TRUE) {
    qgraph(g, groups = as.factor(comMembership), legend = FALSE, palette = "gray")
  }
  
  if (nodeColored == FALSE){
    qgraph(g, legend = FALSE)
  }
  # Use gray node color to distinguish communities even more. Especially important
  # in large graphs with many overlapping clusters - option chosen by user
  
  # Plot clouds 
  hull <- list()
  for (i in 1:length(unique(comMembership))) {
    hull[[i]] <- chull(groupedLayoutMatrix[[i]])  # row numbers of exterior points
    hull[[i]] <- c(hull[[i]], hull[[i]][1])  # add last coefficient to make recursive
    polygon(groupedLayoutMatrix[[i]][hull[[i]], ],  
            col = alpha(colVector[i:(length(unique(comMembership)))], transparency[i]), 
            border = alpha(colVector[i:(length(unique(comMembership)))], 1)) 
  } # end of cloudplotting
  
} # end of NEW FUNCTIONALITY function



#############
# SHINY APP #
#############

#------------------------------------------------------------------------------
shinyServer(  # open shiny
  
  function(input, output){
    
    ######################
    # Define data -------------------------------------------------------------
    ######################

    typedata <- reactive({ # which format is the data stored in?
      switch(input$typedata,
             ".csv",
             ".sav",
             ".txt"
             #".xls",
             #".xlsx"
             )
    })
    
    data <- reactive({ # read datafiles
      if(input$demo == TRUE){
        file <- bfi[,1:25]
      } else {
        file1 <- input$file
        if(is.null(file1)){
          return()
        } else if(input$typedata == ".csv") { # unfortunately only csv works
          read.table(file = file1$datapath,
                     sep = input$sep,
                     header = input$header,
                     stringsAsFactors = input$stringfactors)
        } else if(input$typedata == ".sav"){
          read.spss(file = file1$datapath, 
                    to.data.frame = TRUE)
        } else if(input$typedata == ".txt"){
          read.table(file = file1$datapath,
                     sep = input$sep,
                     stringsAsFactors = input$stringfactors, 
                     header = input$header)
        }
      }
    }) # end dataupload
    
    columnrangefrom <- reactive({  # which columns should be included?
        input$columnrangefrom})
    
    columnrangeto <- reactive({
      input$columnrangeto})  
    
    
    #############################
    # Estimation method network #
    #############################
    
    method <- reactive({  # which estimation method of estimateNetwork?
      switch(input$method,
             "Partial Correlation" = "pcor",
             "GLASSO"= "EBICglasso",
             "IsingSampler" = "IsingSampler",
             "IsingFit" = "IsingFit",
             "MixedGraphicalModel" = "mgm",
             "Huge" = "huge")
      })  ## end method
    
    
    # All following code defines the options for each estimation method seperately
    # I could have modularized these input converters, however, estimation methods
    # often use different (combinations of) options. Eventually, I found it easier
    # and more readable to have seperate input for each method.
    # I hope the variables are self-explanatory by their names.
    
    ########
    # PCOR #
    ########
    
    layoutPcor <- reactive({
      switch(input$layoutPcor,
             "Circle" = "circle",
             "Spring" = "spring")
    })
    
    esPcor <- reactive({
      input$edgesizePcor  
    })

    nsPcor <- reactive({
      input$nodesizePcor
    })

    corMethodPcor <- reactive({
      switch(input$corMethodPcor,
             "cor_auto" = "cor_auto",
             "cov" = "cov",
             "cor" = "cor",
             "npn" = "npn")
    })
    
    
    ##########
    # GLASSO #
    ##########
    
    layoutEG <- reactive({
      switch(input$layoutEG,
             "Circle" = "circle",
             "Spring" = "spring")
    })
    
    esEG <- reactive({
      input$edgesizeEG  
    })
    
    nsEG <- reactive({
      input$nodesizeEG
    })
    
    corMethodEG <- reactive({
      switch(input$corMethodEG,
             "cor_auto" = "cor_auto",
             "cov" = "cov",
             "cor" = "cor",
             "npn" = "npn")
    })
    
    tuningEG <- reactive({
      as.numeric(input$tuningEG)
    })
    

    #################
    # ISING SAMPLER #
    #################
    
    splitMethodIS <- reactive({
      switch(input$splitMethodIS,
             "mean" = "mean",
             "median" = "median")
    })

    estimationMethodIS <- reactive({
      switch(input$estimationMethodIS,
             "default" = "default",
             "loglinear model" ="ll",
             "pseudolikelihood estimation" = "pl", 
             "sequential univariate regressions" = "uni",
             "bivariate regressions" = "bi")
    })
    
    layoutIS <- reactive({
      switch(input$layoutIS,
             "Circle" = "circle",
             "Spring" = "spring")
    })
    
    esIS <- reactive({
      input$edgesizeIS  
    })
    
    nsIS <- reactive({
      input$nodesizeIS
    })
    
    #############
    # ISING FIT #
    #############
    
    splitMethodIF <- reactive({
      switch(input$splitMethodIF,
             "mean" = "mean",
             "median" = "median")
    })
    
    ruleIF <- reactive({
      switch(input$ruleIF,
             "AND" = "AND",
             "OR" = "OR")
    })
    
    tuningIF <- reactive({
      as.numeric(input$tuningIF)
    })
    
    layoutIF <- reactive({
      switch(input$layoutIF,
             "Circle" = "circle",
             "Spring" = "spring")
    })
    
    esIF <- reactive({
      input$edgesizeIF  
    })
    
    nsIF <- reactive({
      input$nodesizeIF
    })
    
    # I decided to disable MGM as it was very buggy (see report)
    ################ 
    # MGM Type Var #
    ################
  #  
  #  typeVar <- reactive({
  #    as.factor(input$type)
  #  })
  #  
  #  levVar <- reactive({
  #    as.numeric(as.character(input$level))
  #  })
  #  
  #  critMgm <- reactive({
  #    switch(input$criterionMgm,
  #           "EBIC" = "EBIC",
  #           "CV" = "CV")
  #  })
  #  
  #  tuningMgm <- reactive({
  #    as.numeric(input$tuningMgm)
  #  })
  #  
  #  layoutMgm <- reactive({
  #    switch(input$layoutMgm,
  #           "Circle" = "circle",
  #           "Spring" = "spring")
  #  })
  #  
  #  esMgm <- reactive({
  #    input$edgesizeMgm  
  #  })
  #  
  #  nsMgm <- reactive({
  #    input$nodesizeMgm
  #  })
  #  
  #  ruleMgm <- reactive({
  #    switch(input$ruleMgm,
  #           "AND" = "AND",
  #           "OR" = "OR")
  #  })
    
    
    ########
    # Huge #
    ########
    
    critH <- reactive({
      switch(input$criterionH,
             "EBIC" = "ebic",
             "RIC" = "ric",
             "STARS" = "stars")
    })
    
    tuningH <- reactive({
      as.numeric(input$tuningH)
    })
    
    layoutH <- reactive({
      switch(input$layoutH,
             "Circle" = "circle",
             "Spring" = "spring")
    })
    
    esH <- reactive({
      input$edgesizeH  
    })
    
    nsH <- reactive({
      input$nodesizeH
    })
    
    
    ##################################
    # ESTIMATE AND VISUALIZE NETWORK #
    ##################################
    
    # The function below will use all the above defined options in 
    # estimateNetwork() and estimate the network based on the demo 
    # or uploaded data.
    # the estimated graph is then transformed into a regular qgraph
    # object because the estimateNetwork object does not include
    # the same layout information as used in ComDetQ.
    # The qgraph object is stored in the global environment, because
    # we will need it to be accessible by another shiny tab 
    # (i.e. communitiy detection).
    # The data needs to be changed into a format that estimateNetwork
    # can read, this is why it is embedded
    # in such a long function.
    
    observeEvent(input$goNw,{
    
    output$networkplot <- renderPlot({

      if(input$method == "Partial Correlation") {
        
        nw <- estimateNetwork(as.data.frame(apply(data()[, as.numeric(input$columnrangefrom):as.numeric(input$columnrangeto)], 
                                                  2, 
                                                  as.numeric)),
                              default = method(),
                              corMethod = corMethodPcor())
        g <<- qgraph(nw$graph, 
                     layout = layoutPcor(), 
                     vsize = nsPcor(), 
                     esize = esPcor(), 
                     labels = nw$labels)
        plot(g)
        
      }
      
      if(input$method == "GLASSO"){

        nw <- estimateNetwork(as.data.frame(apply(data()[, as.numeric(input$columnrangefrom):as.numeric(input$columnrangeto)], 
                                                  2, 
                                                  as.numeric)),
                              default = method(),
                              corMethod = corMethodEG(),
                              tuning = tuningEG(),
                              refit = input$refitEG)
        g <<- qgraph(nw$graph, 
                     layout = layoutEG(), 
                     vsize = nsEG(), 
                     esize = esEG(), 
                     labels = nw$labels)
        plot(g)

      }
      
    if(input$method == "IsingSampler"){
      
      nw <- estimateNetwork(as.data.frame(apply(data()[, as.numeric(input$columnrangefrom):as.numeric(input$columnrangeto)], 
                                                2, 
                                                as.numeric)),
                            default = isolate(method()),
                            split = isolate(splitMethodIS()),
                            method = isolate(estimationMethodIS()))
      g <<- qgraph(nw$graph, 
                   layout = isolate(layoutIS()), 
                   vsize = isolate(nsIS()), 
                   esize = isolate(esIS()), 
                   labels = nw$labels)
      plot(g)
      
    }
      
      if(input$method == "IsingFit"){
        
        nw <- estimateNetwork(as.data.frame(apply(data()[, as.numeric(input$columnrangefrom):as.numeric(input$columnrangeto)], 
                                                  2, 
                                                  as.numeric)),
                              default = isolate(method()),
                              tuning = isolate(tuningIF()),
                              split = isolate(splitMethodIF()),
                              rule = isolate(ruleIF()))
        g <<- qgraph(nw$graph, 
                     layout = isolate(layoutIF()), 
                     vsize = isolate(nsIF()), 
                     esize = isolate(esIF()), 
                     labels = nw$labels)
        plot(g)
        
      }      
      
     # if(input$method == "MixedGraphicalModel"){
     #   
     #    nw <- estimateNetwork(as.data.frame(apply(data()[, as.numeric(input$columnrangefrom):as.numeric(input$columnrangeto)], 
     #                                            2, 
     #                                             as.numeric)),
     #                          default = isolate(method()),
     #                          type = isolate(typeVar()),
     #                          lev = isolate(levVar()),
     #                          tuning = isolate(tuningMgm()),
     #                          criterion = isolate(critMgm()),
     #                          rule = isolate(ruleMgm()))
     #    g <<- qgraph(nw$graph, 
     #                 layout = isolate(layoutMgm()), 
     #                 vsize = isolate(nsMgm()), 
     #                 esize = isolate(esMgm()), 
     #                 labels = nw$labels)
     #    plot(g)
     #    
     # }
      
      if(input$method == "Huge"){
        
        nw <- estimateNetwork(as.data.frame(apply(data()[, as.numeric(input$columnrangefrom):as.numeric(input$columnrangeto)], 
                                                  2, 
                                                  as.numeric)),
                              default = isolate(method()),
                              tuning = isolate(tuningH()),
                              npn = TRUE,
                              criterion = isolate(critH()))
        g <<- qgraph(nw$graph, 
                     layout = isolate(layoutH()), 
                     vsize = isolate(nsH()), 
                     esize = isolate(esH()), 
                     labels = nw$labels)
        plot(g)
        
      }
    })
    })  # end of method logic
    
    ####################
    # DOWNLOAD NETWORK #
    ####################
    
    output$downloadnw <- downloadHandler(
      filename = function(){
      paste("network", class = ".pdf", sep = "")
        },
      content = function(file){
        pdf(file)
        qgraph(g)
        dev.off()
      }
    )
    
    
    ###########################
    # COMMUNITY DETECTION TAB #
    ###########################
    
    typeCA <- reactive({
      switch(input$typeCA,
             "Walktrap" = "walktrap",
             "Spinglass" = "spinglass")
    })
    
    colCom <- reactive({
      switch(input$colCom,
             "rainbow" = "rainbow",
             "heat.colors" = "heat.colors",
             "cm.colors" = "cm.colors",
             "topo.colors" = "topo.colors",
             "terrain.colors" = "terrain.colors", 
             "rainbow_hcl" = "rainbow_hcl",
             "diverge_hcl" = "diverge_hcl",
             "terrain_hcl" = "terrain_hcl",
             "sequential_hcl" = "sequential_hcl")
    })
    
    numberOfStepsSpins <- reactive({
      as.numeric(input$numberOfStepsSpins)
    })
    
    size <- reactive({
      as.numeric(input$size)
    })

    observeEvent(input$goCom,{
    output$comNetwork <- renderPlot({
      
      ComDetQ(g,
              type = isolate(typeCA()),
              iterate = isolate(numberOfStepsSpins()),
              negEdges = isolate(input$negEdges),
              size = isolate(size()),
              nodeColored = isolate(input$nodeColored),
              comCol = isolate(colCom())
              )
      
    })
    })  # Applies ComDetQ to the graph object estimated in the other tab
        # and saved in the global environment. Function includes the options
        # specified by the user.
    
    ######################
    # DOWNLOAD COMMUNITY #
    ######################
    
    # Unfortunately, I could not get this to work.
    
    output$downloadcomnetwork <- downloadHandler(
      filename = function(){
        paste("community", class = ".pdf", sep = "")
      },
      content = function(file){
        pdf(file)
        output$comNetwork <- renderPlot({
          
          ComDetQ(g,
                  type = isolate(typeCA()),
                  iterate = isolate(numberOfStepsSpins()),
                  negEdges = isolate(input$negEdges),
                  size = isolate(size()),
                  nodeColored = isolate(input$nodeColored),
                  comCol = isolate(colCom())
          )
          
        })
        dev.off()
      }
    )
    
    }
    )  # end of shiny server