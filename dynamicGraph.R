library("networkD3")
library("igraph")
library("plyr")
library("dplyr")

# Calculate degree for all nodes. Degree of a node tells us the number of edges incident to the node

# Calculate betweenness for all nodes. Betweenness centrality tells us how central
# the node is, in terms of the number of shortest paths from all nodes to all others
# that pass through that node

# Calculate Dice similarities between all pairs of nodes
# Dice similarity represents the similarity between two nodes’ first neighborhoods
# D(node1, node2) = 2 * number of mutual neighbours of  node1 and node2 /
# (number of neighbours of node1 + number of neighbours of node2)

#####################---------------------------------------------###################

                                        # Data format: dataframe with 3 variables; variables 1 & 2 correspond to interactions; variable 3 is weight of interaction
for(year in as.character(2009:2016)) {
edgeList <- read.csv(file.path("data", paste0(year, "_edgelist.csv")),
                    stringsAsFactors = F)
colnames(edgeList) <- c("SourceName", "TargetName", "Weight")

## Create the graph
gD <- igraph::graph.data.frame(edgeList, directed=FALSE)
vs <- unique(neighbors(gD, v = which(V(gD) == 821)))
vs2 <- unique(neighbors(gD, v = vs))
vsAll <- union(vs, vs2)
## show(vs)

## Filter the edges
flag <- which( (edgeList$SourceName %in% vsAll & edgeList$TargetName %in% vsAll))
edgeList <- edgeList[flag,]


# Create a graph. Simplify ensures that there are no duplicated edges or self loops
gD <- igraph::simplify(igraph::graph.data.frame(edgeList, directed=FALSE))

vcount(gD) #no of nodes
ecount(gD) # no of edges

# Create a node list object (actually a data frame object) that will contain information about nodes
nodeList <- data.frame(ID = c(0:(igraph::vcount(gD) - 1)), # because networkD3 library requires IDs to start at 0
                       nName = igraph::V(gD)$name) # V() - vertices of the graph

## Join in officer information to nodeList
officerFile <- file.path("data", "toy.officer_data_cleaned.csv")
officers <- read.csv(officerFile)
officers$ID <- officers$officer_id - 1
officers$secondary <- ifelse(officers$secondary == 1, officers$secondary + 1,
                             officers$secondary)
officers$tertiary <- ifelse(officers$tertiary == 1, officers$tertiary + 2,
                            officers$tertiary)
officers$class <- rowSums(officers[,c("primary", "secondary", "tertiary")])
nodeList <- join(nodeList, officers, by = "ID")


# Map node names from the edge list to node IDs
getNodeID <- function(x){
  which(x == igraph::V(gD)$name) - 1 # to ensure that IDs start at 0
}
# And add them to the edge list
edgeList <- plyr::ddply(edgeList, .variables = c("SourceName", "TargetName", "Weight"),
                        function (x) data.frame(SourceID = getNodeID(x$SourceName),
                                                TargetID = getNodeID(x$TargetName)))

############################################################################################
# Calculate some node properties and node similarities that will be used to illustrate
# different plotting abilities and add them to the edge and node lists

# Calculate degree for all nodes
nodeList <- cbind(nodeList, nodeDegree=igraph::degree(gD, v = igraph::V(gD), mode = "all"))

# Calculate betweenness for all nodes
betAll <- igraph::betweenness(gD, v = igraph::V(gD), directed = FALSE) / (((igraph::vcount(gD) - 1) * (igraph::vcount(gD)-2)) / 2)
betAll.norm <- (betAll - min(betAll))/(max(betAll) - min(betAll))
nodeList <- cbind(nodeList, nodeBetweenness=100*betAll.norm) # We are scaling the value by multiplying it by 100 for visualization purposes only (to create larger nodes)
rm(betAll, betAll.norm)

#Calculate Dice similarities between all pairs of nodes
dsAll <- igraph::similarity.dice(gD, vids = igraph::V(gD), mode = "all")

F1 <- function(x) {data.frame(diceSim = dsAll[x$SourceID +1, x$TargetID + 1])}
edgeList <- plyr::ddply(edgeList, .variables=c("SourceName", "TargetName", "Weight", "SourceID", "TargetID"),
                        function(x) data.frame(F1(x)))

rm(dsAll, F1, getNodeID, gD)

############################################################################################
# We will also create a set of colors for each edge, based on their dice similarity values
# We'll interpolate edge colors based on the using the "colorRampPalette" function, that
# returns a function corresponding to a collor palete of "bias" number of elements (in our case, that
# will be a total number of edges, i.e., number of rows in the edgeList data frame)
F2 <- colorRampPalette(c("#FFFF00", "#FF0000"), bias = nrow(edgeList), space = "rgb", interpolate = "linear")
colCodes <- F2(length(unique(edgeList$diceSim)))
edges_col <- sapply(edgeList$diceSim, function(x) colCodes[which(sort(unique(edgeList$diceSim)) == x)])

rm(colCodes, F2)

# update edgeList after this
############################################################################################

######################
## Load in officers ##
######################
## load(file.path("data", "ID.Rdata")

D3_network_LM <- networkD3::forceNetwork(Links = edgeList, # data frame that contains info about edges
                                         Nodes = nodeList, # data frame that contains info about nodes
                                         Source = "SourceID", # ID of source node
                                         Target = "TargetID", # ID of target node
                                         Value = "Weight", # value from the edge list (data frame) that will be used to value/weight relationship amongst nodes
                                         NodeID = "nName", # value from the node list (data frame) that contains node description we want to use (e.g., node name)
                                         Nodesize = "nodeBetweenness",  # value from the node list (data frame) that contains value we want to use for a node size
                                         Group = "nodeDegree",  # value from the node list (data frame) that contains value we want to use for node color
                                         colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"), # color scale to use
    ##                                      colourScale = JS("var o = d3.scale.ordinal()
    ## .domain([\"WHITE\", \"HISPANIC\", \"BLACK\", \"A_OTHER\"])
    ## .range(colorbrewer.RdBu[9]);"),
                                         height = 1000, # Size of the plot (vertical)
                                         width = 1000,  # Size of the plot (horizontal)
                                         fontSize = 20, # Font size
                                         linkDistance = networkD3::JS("function(d) { return 10*d.value; }"), # Function to determine distance between any two nodes, uses variables already defined in forceNetwork function (not variables from a data frame)
                                         linkWidth = networkD3::JS("function(d) { return d.value/5; }"),# Function to determine link/edge thickness, uses variables already defined in forceNetwork function (not variables from a data frame)
                                         opacity = 0.85, # opacity
                                         zoom = TRUE, # ability to zoom when click on the node
                                         opacityNoHover = 0.1, # opacity of labels when static
                                         linkColour = edges_col) # edge colors

# Plot network
D3_network_LM

# Save network as html file
networkD3::saveNetwork(D3_network_LM, paste0("D3net_", year, ".html"), selfcontained = TRUE)
}
