
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

###########
# TESTING #
###########

# Create graph

library(qgraph)
set.seed(77)
g1 <- qgraph(cor(matrix(rnorm(24),12,12)), layout = "spring")
g2 <- qgraph(cor(matrix(rnorm(12^2),12,12)), layout = "spring")
g3 <- qgraph(cor(matrix(rnorm(50^2),50,50)), layout = "spring")


# Test against igraph plotting

library(igraph)
ig1 <- as.igraph(g1)
sig1 <- spinglass.community(ig1)
plot(sig1, ig1, layout = g1$layout)

ComDetQ(g1, type = "spinglass", nodeColored = TRUE, comCol = "heat.colors")

ig2 <- as.igraph(g2)
wkig2 <- walktrap.community(ig2, steps = 2)
plot(wkig2, ig2, layout = g2$layout)

ComDetQ(g2, type = "walktrap", negEdges = TRUE, iterate = 2, res =150, size = 1.2, 
        nodeColored = TRUE, comCol = "heat.colors" )

ig3 <- as.igraph(g3)
sig3 <- spinglass.community(ig3)
plot(sig3, ig3, layout = g3$layout)

ComDetQ(g3, type = "spinglass", nodeColored = TRUE, comCol = "rainbow")

# All visualizations show the same result.
# Warning messages work.
