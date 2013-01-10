
################################################################################
## Hands on Section 1 - Load libraries

## If this is the first time you are running this code, uncomment the following
## line and run it.  It will install the packages used in this hands on lab.
## For BIWA Summit, these packages are already installed on the VM.
## source('R/setup.R')

## sna is an R package for social network analysis.  We'll be using it for its
## network plotting function, gplot().
## http://cran.r-project.org/web/packages/sna/index.html
require('sna')

## igraph is also an R package for social network analysis.  We will use it for
## our primary set of functions for examining the networks in this lab.
## http://cran.r-project.org/web/packages/igraph/index.html
require('igraph')

## RColorBrewer is a package that makes it easy to take advantage of the color
## pallets from the website colorbrewer2.org.
## http://cran.r-project.org/web/packages/RColorBrewer/index.html
require('RColorBrewer')

## The caret package (Classification And REgression Training) is "a set of
## functions that attempt to streamline the process of predictive analytics."
## http://caret.r-forge.r-project.org/
require('caret')

## ggplot2 is a package for plotting by Hadley Wickham.  It is based
## on the book The Grammer of Graphics by Leeland Wilkinson.
## http://cran.r-project.org/web/packages/ggplot2/index.html
require('ggplot2')

## Here we source a set of functions assembled for this lab.  Separating them
## from the lab's code allows us to focus on the concepts rather than the
## details of implementation.  You are encouraged to look through these
## functions after you are comfortable with the lab's content.
source('R/functions.R')

################################################################################
## Hands on Section 2 - Sentiment Analysis
## In this section we'll employ a basic and reasonably effective form of
## sentiment analysis.  We will count the number of positive and negative words
## within a tweet and score the tweets on the difference between those counts.
## For more information on this technique see Jeffrey Breen's tutorial here:
## http://www.inside-r.org/howto/mining-twitter-airline-consumer-sentiment

## The new twitter API (1.1) doesn't allow any interaction from twitteR without
## an API connection.  The file get_tweets shows how to use the package to
## acquire tweets on a given topic.  Here we load tweets about Oracle captured
## Jan 8th.
load("data/oracle_tweets.RData")

## Load vectors of positive and negative words to compare with the tweets
hu.liu.pos <- scan(file.path('data/opinion-lexicon-English',
  'positive-words.txt'), what='character', comment.char=';')
hu.liu.neg <- scan(file.path('data/opinion-lexicon-English',
  'negative-words.txt'), what='character', comment.char=';')

## Show 10 random words from each word vector
hu.liu.pos[runif(10, 1, length(hu.liu.pos))]
hu.liu.neg[runif(10, 1, length(hu.liu.neg))]

## Pass the tweet text vector to the score.sentiment function found in the
## function.R file.  This function will return a dataframe object which we
## assign to oracle.scores for further analysis
oracle.scores <- score.sentiment(oracle.tweets, hu.liu.pos, hu.liu.neg)

## look at the 5 most positive tweets
head(oracle.scores$text[order(-oracle.scores$score)], 5)

## look at the 5 most negative tweets
head(oracle.scores$text[order(oracle.scores$score)], 5)

## Apparently, cloud is a negative word...
any(hu.liu.neg == "cloud")

## Let's remove cloud and re-run
cloud <- which(hu.liu.neg == "cloud")

hu.liu.neg <- c(hu.liu.neg[1:cloud-1],
                hu.liu.neg[cloud+1:length(hu.liu.neg)])

oracle.scores <- score.sentiment(oracle.tweets, hu.liu.pos, hu.liu.neg)

## look again at the 5 most negative tweets
head(oracle.scores$text[order(oracle.scores$score)], 5)

## For more on twitter sentiment analysis check out:
## https://sites.google.com/site/miningtwitter/questions/sentiment which
## includes links to the word counting technique we used here as well as two
## more techniques which require manual package installation and/or API set-up
## making them out of scope for our lab

################################################################################
## Hands' on Section 3 - Network Basics

## Because a twitter API account is needed to download user network data, we'll
## use a pre-curated network of the biwa speakers.  The code used to create
## this network is included in the extras folder as 'get_speakers.R'.
load('data/biwa_speaker_network.Rdata')

## Let's take a first look at the network using the plot.igraph function from
## the igraph package.
plot.igraph(g,
            vertex.label=V(g)$name,
            layout=layout.kamada.kawai,
            vertex.label.dist = 0.2,
            vertex.label.color = 'Black',
            vertex.size = 4)

## Several speakers have no relationship with the main speaker network.  We'll
## remove them with the delete.isolates function (found in 'functions.R')
g <- delete.isolates(g)

## We'll plot the network again with no isolates, using the gplot function from
## the sna package instead of plot.igraph. gplot gives us more aesthetic
## control.
gplot(dat = as.matrix(get.adjacency(g)),
      label = V(g)$name,
      mode = "kamadakawai",
      pad = 0,
      label.pad = 1,
      label.pos = 1,
      vertex.sides = 100,
      edge.col = "#00000011",
      label.border = "#ffffff00")

## Networks consist of nodes (also called vertices) and edges (also called links)
## let's see how many of each are in the speaker network.
## To get the number of nodes we'll use the igraph function vcount (vertices count)
nodes <- vcount(g)
nodes

## To get the number of edges we'll use the igraph function ecount
edges <- ecount(g)
edges

## Networks can be directed or undirected.  Twitter and Facebook are great
## examples of each.  Facebook friends are always mutual (whether you like it or
## not) where twitter allows for others to follow you without you following
## them.  Let's look at our speaker network, coloring the mutual edges blue
## and the unreciprocated edges red.
mutual.edges <- is.mutual(g)

plot.igraph(g,
            vertex.label=V(g)$name,
            layout=layout.kamada.kawai,
            vertex.label.dist = 0.2,
            vertex.label.color = 'Black',
            vertex.size = 4,
            edge.color = brewer.pal(9, "Set1")[mutual.edges+1])

## Whether or not a network is directed or undirected will affect most of the
## measures we'll look at in the remainder of the lab.  It is important to know
## the directedness of your network at the outset of analysis and create the
## network object accordingly.

## The shortest paths matrix shows us the minimum number of edges between each
## member (degrees of separation)
shortest.paths(g, mode="all")

## The diameter of a network tells us the number of edges between the most
## distant members
diameter(g)

## igraph calculates diameter on the *directed* network by default. If we treat
## the graph as undirected the diameter drops to three.  This is because as a
## directed graph, we have unreachable members (see below).
diameter(g, directed=FALSE)

## Column 3, 9 and 20 of the directed reachability matrix show members who
## are unreachable because they follows no one else in the speaker network.  Row 18
## shows Jeff S, who is unheard as he is not followed by anyone else in the
## network.
reachability(g, mode = 'in')

## Create a vector of colors where all node positions are grey then set the
## nodes of interest red
unreachable.nodes <- rep("grey", vcount(g))
unreachable.nodes[c(3,9,20,18)] <- "red"

gplot(dat = as.matrix(get.adjacency(g)),
      label = V(g)$name,
      mode = "kamadakawai",
      pad = 0,
      label.pad = 1,
      label.pos = 1,
      vertex.sides = 100,
      vertex.col = unreachable.nodes, # use our vector for color
      edge.col = "#00000011",
      label.border = "#ffffff00")

## Reciprocity is the ratio of the number of reciprocated edges to the number of
## total edges.  For our network this is very high, which isn't surprising given
## the shared interests of the sample.
reciprocity(g)

## Density is the ratio of the number of possible edges in a graph to the number
## of actual edges. Zero means no one follows anyone and one means everyone
## follows everyone.  Density is also known as the clustering coefficient.
graph.density(g)

## Before we move on to measures of centrality (related to importance)
## let's create a simple network called krackhardt's kite, which is
## commonly used to illustrate centrality principles.

edges <- matrix(c(1,2,1,3,1,4,1,6,2,1,2,4,2,5,2,7,3,1,3,4,3,6,
                  4,1,4,2,4,3,4,5,4,6,4,7,5,2,5,4,5,7,
                  6,1,6,3,6,4,6,7,6,8,7,2,7,4,7,5,7,6,7,8,
                  8,6,8,7,8,9,9,8,9,10,10,9),
                nc=2, byrow=TRUE)

krackhardt <- as.undirected(graph.edgelist(edges))

plot(krackhardt)

################################################################################
## Hands On Section 4 - Centrality Measures

###
## Degree Centrality
## -----------------
## The number connections for a node.  For a directed network we can count in
## degree and out degree separately

## Let's start with our simple krackhardt kite network.  The degree function
## calculates the degree for each node and we'll store that in the k.deg vector
k.deg <- degree(krackhardt)
k.deg

## When we pass the k.deg vector to gplot as both the node label and the node
## size, we get an easy to interpret plot showing the number of connections for
## each node

gplot(dat = as.matrix(get.adjacency(krackhardt)),
      label = k.deg,
      vertex.cex = k.deg,
      mode = "kamadakawai",
      pad = 0,
      label.pad = 1,
      label.pos = 1,
      vertex.sides = 100,
      vertex.border = "#00000066",
      edge.col = "#00000011")

## Degree Centrality is simply the number of followers and followees of the
## speaker network. We'll calculate the in and out degrees separately use the
## degree() function.
out.deg <- degree(g, mode=c("out"))
in.deg <- degree(g, mode=c("in"))

## see which nodes have the max out and indegree
V(g)$name[which.max(out.deg)]
V(g)$name[which.max(in.deg)]

## see all nodes sorted by out and in degree
out.deg[order(-out.deg)]
in.deg[order(-in.deg)]

## plot the biwa speaker network with nodes sized by out degree
gplot(dat = as.matrix(get.adjacency(g)),
      label = V(g)$name,
      vertex.cex = scale(out.deg, center=FALSE),
      mode = "kamadakawai",
      pad = 0,
      label.pos = 1,
      vertex.sides = 100,
      vertex.border = "#00000066",
      edge.col = "#00000011")

## plot our network with nodes sized by in degree
gplot(dat = as.matrix(get.adjacency(g)),
      label = V(g)$name,
      vertex.cex = scale(in.deg, center=FALSE),
      mode = "kamadakawai",
      pad = 0,
      label.pos = 1,
      vertex.sides = 100,
      vertex.border = "#00000066",
      edge.col = "#00000011")

## All of the measures we've looked at so far have come from our speaker
## network, not the broader twitter network Let's size the nodes by the total
## follower count (total in degree), not just our speaker followers.
gplot(dat = as.matrix(get.adjacency(g)),
      label = V(g)$name,
      vertex.cex = scale(V(g)$num.followers, center=FALSE),
      mode = "kamadakawai",
      pad = 0,
      label.pos = 1,
      vertex.sides = 100,
      vertex.border = "#00000066",
      edge.col = "#00000011")

###
## Closeness Centrality
## --------------------
## Normalized closeness centrality shows the inverse of the number of edges to all
## other nodes in the graph (e.g. a value of .5 means an average of two edges)

## We use the closeness function of the igraph package to calculate the
## closeness with the normalized option set to TRUE so the numbers reflect the
## definition above
node.closeness <- closeness(krackhardt, normalized=TRUE)
round(node.closeness, digits = 2)

gplot(dat = as.matrix(get.adjacency(krackhardt)),
      label = round(node.closeness, digits = 2),
      vertex.cex = node.closeness * 10,
      mode = "kamadakawai",
      pad = 0,
      label.pos = 1,  # Below vertex
      vertex.sides = 100,  # Basically circular
      vertex.border = "#00000066",
      edge.col = "#00000011")

## We'll first examine in degree closeness in the biwa network, measuring who
## can be heard.
node.closeness <- closeness(g, mode = "in", normalized=TRUE)
node.closeness[order(-node.closeness)]

## plot our network with nodes sized by closeness
gplot(dat = as.matrix(get.adjacency(g)),
      label = V(g)$name,
      vertex.cex = scale(node.closeness, center=FALSE) * 2,
      mode = "kamadakawai",
      pad = 0,
      label.pos = 1,  # Below vertex
      vertex.sides = 100,  # Basically circular
      vertex.border = "#00000066",
      edge.col = "#00000011")

## Now we'll examine in and out degree at once.  Notice Shyam almost has only 1
## degree of separation to everyone in the network, either by following or being
## followed.  He's only missing Nick Beaird.
node.closeness <- closeness(g, mode = "all", normalized=TRUE)
node.closeness[order(-node.closeness)]

## plot the biwa network with nodes sized by closeness
gplot(dat = as.matrix(get.adjacency(g)),
      label = V(g)$name,
      vertex.cex = scale(node.closeness, center=FALSE) * 2,
      mode = "kamadakawai",
      pad = 0,
      label.pos = 1,  # Below vertex
      vertex.sides = 100,  # Basically circular
      vertex.border = "#00000066",
      edge.col = "#00000011")

###
## Betweenness Centrality
## ----------------------
## Betweenness Centrality shows the number of shortest paths a node is on
## between pairs of other nodes.  If multiple shortest paths between two nodes
## exist the betweenness is shared among all the intermediate nodes, resulting
## in the fractions seen in this analysis.

## Let's begin by again using our krackhardt kite network as an example
node.betweenness <- betweenness(krackhardt)
round(node.betweenness, digits=2)

gplot(dat = as.matrix(get.adjacency(krackhardt)),
      label = round(node.betweenness, digits=2),
      vertex.cex = scale(node.betweenness, center=FALSE)*2,
      mode = "kamadakawai",
      pad = 0,
      label.pos = 1,
      vertex.sides = 100,
      vertex.border = "#00000066",
      edge.col = "#00000011")

node.betweenness = betweenness(g,directed=TRUE)
node.betweenness[order(-node.betweenness)]

## plot our network with nodes sized by betweenness
gplot(dat = as.matrix(get.adjacency(g)),
      label = V(g)$name,
      vertex.cex = scale(node.betweenness, center=FALSE), # scale by rms
      mode = "kamadakawai",
      pad = 0,
      label.pos = 1,
      vertex.sides = 100,
      vertex.border = "#00000066",
      edge.col = "#00000011")

###
## Eigenvector centrality
## ----------------------
## Pagerank (Eigenvector centrality) Pagerank is a recursive measure of
## centrality, measuring each node by not only the number of connections it has,
## but weighting that metric by how many connections it's connections have,
## which are in turn weighted by their connections etc.  This can be computed
## using the dominant eigenvector of a modified adjacency matrix (don't worry if
## you don't know the matrix algebra, you can still use it effectively).

## Let's calculate pagerank for our krackhardt network
k.pagerank <- page.rank(krackhardt)$vector
gplot(dat = as.matrix(get.adjacency(krackhardt)),
      label = V(krackhardt),
      vertex.cex = scale(k.pagerank, center=FALSE)*2,
      mode = "kamadakawai",
      pad = 0,
      label.pos = 1,  # Below vertex
      vertex.sides = 100,  # Basically circular
      vertex.border = "#00000066",
      edge.col = "#00000011")

## In most cases page rank is very closely related to the degree.  In order to
## give a little more intuition about it, let's look at a (hopefully) familiar
## network and find a node whose eigenvector metric is much higher than expected
## based on their degree.

## Read in the pulp fiction social interaction graph from moviegalaxies.com
pf <- read.graph('./data/pulp_fiction.net', format=c("pajek"))

## Let's add a little more aesthetic control for this one...
## We'll adjust the color as well as the size for degree
pf.node.size <- degree(pf)/10
degree.color <- hsv(.6, .6, (pf.node.size - min(pf.node.size)) /
                     (1.25 * max(pf.node.size) - min(pf.node.size)))

## Run this a few times until you get a layout you like, it will be saved
## as a positions vector that we can use to look at the graph again
positions <- gplot(dat = as.matrix(get.adjacency(pf)),
                   label = V(pf)$id,
                   mode = "kamadakawai",
                   pad = -0.5,
                   boxed.labels = TRUE,
                   label.bg = "#ffffff55",
                   label.border = "#ffffff00",
                   label.pad = 0,
                   label.pos = 1,
                   label.col = degree.color,
                   vertex.sides = 100,
                   vertex.cex = pf.node.size,
                   vertex.border = "#00000066",
                   vertex.col = degree.color,
                   edge.col = "#00000011")

## Create a new dataframe to keep track of our centrality measures
cent<-data.frame(deg=degree(as.undirected(pf)),
                 eig=page.rank(as.undirected(pf))$vector)

## Run a simple regression attempting to predict eigenvector from degree and
## save the residuals
res<-lm(eig~deg,data=cent)$residuals

## Add the residuals to our dataframe
cent<-cbind(cent, res)

## We now have a measure (res) that we can use to size the nodes in our network.
## We are hoping to identify characters who are connected to only a few people,
## all of whom are "important" to the plot in the sense that they have high
## eigenvector centrality.
node.size <- abs(res) * 100
node.color <- hsv(.6, .6, (node.size - min(node.size)) /
                     (1.25 * max(node.size) - min(node.size)))

gplot(dat = as.matrix(get.adjacency(pf)),
      coord = positions,
      label = V(pf)$id,
      mode = "kamadakawai",
      pad = -0.5,
      boxed.labels = TRUE,
      label.bg = "#ffffff55",
      label.border = "#ffffff00",
      label.pad = 0,
      label.pos = 1,
      label.col = node.color,
      vertex.sides = 100,
      vertex.cex = node.size,
      vertex.border = "#00000066",
      vertex.col = node.color,
      edge.col = "#00000011")

## Let's look at the regression line to get a sense of how the res measure was
## created
p<-ggplot(cent,
          aes(x=deg,
              y=eig,
              label=V(pf)$id,
              colour=res,
              size=abs(res))) +
  geom_smooth(method="lm") +
  xlab("Degree Centrality") +
  ylab("Page Rank Centrality")

p + geom_text() + ggtitle("Degree vs Page Rank in Pulp Fiction Network")

## Now that we've explored eigenvector centrality with example networks,
## let's look at it for the biwa speaker network.
pr = page.rank(g)
pr$vector[order(-pr$vector)]

## plot our network with nodes sized by page rank
gplot(dat = as.matrix(get.adjacency(g)),
      label = V(g)$name,
      vertex.cex = scale(pr$vector, center=FALSE),
      mode = "kamadakawai",
      pad = 0,
      label.pos = 1,
      vertex.sides = 100,
      vertex.border = "#00000066",
      edge.col = "#00000011")

## Let's again look for people who have a limited number of high eigenvector
## connections
cent<-data.frame(deg=degree(g, mode="in"),eig=page.rank(g)$vector)
res<-lm(eig~deg,data=cent)$residuals
cent<-transform(cent,res=res)

## We'll look at the linear model first this time
p<-ggplot(cent,
          aes(x=deg,
              y=eig,
              label=V(g)$name,
              colour=res,
              size=abs(res))) +
  geom_smooth(method="lm") +
  xlab("Degree Centrality") +
  ylab("Page Rank Centrality")

p + geom_text() + ggtitle("Degree vs Page Rank in BIWA Speaker Network")

## Let's plot the network again, with the nodes sized by the residual
## Note that the residual could be positive or negative.  We'll color
## positive values blue, and negative red in our plot.
node.size <- abs(res) * 100
node.color <- ifelse(res > 0, 2, 1)

gplot(dat = as.matrix(get.adjacency(g)),
      label = V(g)$name,
      vertex.cex = node.size,
      vertex.col = brewer.pal(9, "Set1")[node.color],
      mode = "kamadakawai",
      pad = 0,
      label.pos = 1,  # Below vertex
      vertex.sides = 100,  # Basically circular
      vertex.border = "#00000066",
      edge.col = "#00000011")

################################################################################
## Hands On Section 5 - Cliques, K-cores and Communities

###
## Cliques
## -------
## Cliques are sets of nodes which are all connected to one another.  Let's use
## the kite network and find the largest clique.
V(krackhardt)[largest.cliques(krackhardt)[[1]]]

## We'll plot the graph with this clique highlighted
largest.clique.nodes <- rep("light gray", vcount(krackhardt))
largest.clique.nodes[largest.cliques(krackhardt)[[1]]] <- "red"

gplot(dat = as.matrix(get.adjacency(krackhardt)),
      label = V(krackhardt),
      vertex.col = largest.clique.nodes,
      mode = "kamadakawai",
      pad = 0,
      label.pos = 1,
      vertex.sides = 100,
      vertex.cex = 2,
      vertex.border = "#00000066",
      edge.col = "#00000011")

## Now we'll find the largest cliques in the speaker network, making sure the
## graph is treated as undirected.
g.mutual <- delete.isolates(delete.edges(g,which(!is.mutual(g))))

largest.cliques(as.undirected(g.mutual))

## Note that there are 6 cliques with 4 members each
V(g.mutual)$name[largest.cliques(as.undirected(g.mutual))[[1]]]
V(g.mutual)$name[largest.cliques(as.undirected(g.mutual))[[6]]]

## Lets plot a graph with the first clique highlighted
largest.clique.nodes <- rep("gray", vcount(g.mutual))
largest.clique.nodes[largest.cliques(as.undirected(g.mutual))[[1]]] <- "red"

gplot(dat = as.matrix(get.adjacency(g.mutual)),
      label = V(g.mutual)$name,
      vertex.col = largest.clique.nodes,
      mode = "kamadakawai",
      pad = 0,
      label.pos = 1,
      vertex.sides = 100,
      vertex.border = "#00000066",
      edge.col = "#00000011")

clique.only <- induced.subgraph(as.undirected(g.mutual),
                                vids=largest.cliques(as.undirected(g.mutual))[[1]])

gplot(dat = as.matrix(get.adjacency(clique.only)),
      label = V(clique.only)$name,
      mode = "circle",
      pad = 0,
      label.pos = 1,
      vertex.sides = 100,
      vertex.border = "#00000066",
      edge.col = "#00000011")

## We'll now find a clique in a larger network

## Load the oracle aces twitter network
load("data/aces_network.RData")
aces <- delete.isolates(aces)

## For the clique analysis we'll only look at mutual followers
mutual.aces <- delete.isolates(delete.edges(aces, which(!is.mutual(aces))))
vcount(mutual.aces)

## Now we'll find the largest cliques in the aces network, making sure the
## graph is treated as undirected.
largest.cliques(as.undirected(mutual.aces))

## What are their names?
V(mutual.aces)$name[largest.cliques(as.undirected(mutual.aces))[[1]]]

## Lets plot a graph with the clique highlighted
largest.clique.nodes <- rep("gray", vcount(mutual.aces))
largest.clique.nodes[largest.cliques(as.undirected(mutual.aces))[[1]]] <- "red"

gplot(dat = as.matrix(get.adjacency(mutual.aces)),
      vertex.col = largest.clique.nodes,
      mode = "fruchtermanreingold",
      pad = 0,
      label.pos = 1,
      vertex.sides = 100,
      vertex.border = "#00000066",
      edge.col = "#00000011")

## We'll create a new subgraph with only the members of the clique, and plot
## them using the "circle" layout mode to emphasize the pattern found in cliques
clique.only <- induced.subgraph(as.undirected(mutual.aces),
                                vids=largest.cliques(as.undirected(mutual.aces))[[1]])

gplot(dat = as.matrix(get.adjacency(clique.only)),
      label = V(clique.only)$name,
      mode = "circle",
      pad = 0,
      label.pos = 1,
      vertex.sides = 100,
      vertex.border = "#00000066",
      edge.col = "#00000011")

###
## K-Cores
## -------

## To be included in a clique, a node must be tied to all other nodes in the
## clique.  A k-core is a group where nodes have k edges within the group.
## An example with our kite network will help make this more clear.

## The graph.coreness() function in igraph returns a vector containing
## the degree of the highest-degree k-core to which each vertex
## belongs.
coreness = graph.coreness(as.undirected(krackhardt))
coreness

## Note that the output of graph.coreness only gives us the *degree* of the k-core,
## not the k-core itself.  Two nodes with coreness of 3 may not be connected at
## all and may be in separate k-cores.

## We'll get more intuition for k-cores by plotting the kite network and
## color-coding by k-core:
gplot(dat = as.matrix(get.adjacency(krackhardt)),
      label = V(krackhardt),
      vertex.col = brewer.pal(8, "Set1")[coreness],
      mode = "kamadakawai",
      pad = 0,
      label.pos = 1,
      vertex.sides = 100,
      vertex.cex = 2,
      vertex.border = "#00000066",
      edge.col = "#00000011")

## Now we can plot the k-cores for the speaker network
coreness = graph.coreness(as.undirected(g))
coreness

gplot(dat = as.matrix(get.adjacency(g)),
      label = V(g)$name,
      vertex.col = brewer.pal(8, "Set1")[coreness],
      mode = "kamadakawai",
      pad = 0,
      label.pos = 1,
      vertex.sides = 100,
      vertex.border = "#00000066",
      edge.col = "#00000011")

## Now we can plot the k-cores for the speaker network
coreness = graph.coreness(as.undirected(mutual.aces))

## What is the max coreness
max(coreness)

## How many people have that coreness?
length(coreness[coreness == max(coreness)])

## We'll plot the core of the aces network
core.only <- induced.subgraph(as.undirected(mutual.aces),
                              vids=which(coreness == max(coreness)))

gplot(dat = as.matrix(get.adjacency(core.only)),
      label = V(core.only)$name,
      mode = "kamadakawai",
      pad = 0,
      label.pos = 1,
      vertex.sides = 100,
      vertex.border = "#00000066",
      edge.col = "#00000011")

## Note that our core still includes the largest clique
length(largest.cliques(core.only)[[1]])
length(largest.cliques(as.undirected(mutual.aces))[[1]])

###
## Community Detection
## -------------------
## The animation in the figures folder demonstrates an intuitive approach to
## community detection.  However, our speaker network is very dense and the
## edge-betweenness approach fails to find a good split for our communities.
eb <- edge.betweenness.community(g)
eb

## Edge betweenness is pretty slow with even moderate networks
eb.aces <- edge.betweenness.community(aces)
sizes(eb.aces)

## Let's instead look at two popular algorithms for finding structure in large graphs

## Walktrap
## The walktrap algorithm detects communities through a series of short random
## walks, with the idea that the nodes encountered on any given random walk are
## more likely to be within a community than not. While edge betweenness started
## with the whole network and removed edges to create clusters, this algorithm
## initially treats all nodes as communities of their own, then merges them into
## communities, and these communities into larger communities, and so on.
wt <- walktrap.community(g)
wt

## Show the community sizes
sizes(wt)

## Show the names of members in 2nd community
V(g)$name[membership(wt)==2]

## plot the network with communities colored separately
gplot(dat = as.matrix(get.adjacency(g)),
      label = V(g)$name,
      vertex.col = brewer.pal(9, "Set1")[membership(wt)],
      mode = "kamadakawai",
      pad = 0,
      label.pos = 1,
      vertex.sides = 100,
      vertex.border = "#00000066",
      edge.col = "#00000011")

## We can visualize the clusters generated by walktrap as a dendrogram. Here,
## the y-axis reflects the distance metric (smaller "distances" mean more
## similarity) used by the walktrap algorithm. The lower the horizontal bar
## connecting nodes and communities, the more similar they are.
wt_dend <- as.dendrogram(wt, use.modularity=TRUE)
op <- par(mar = par("mar") + c(5,0,0,0))
plot(wt_dend )
par(op)

## Let's try walktrap on our aces network.  Notice how much faster it is than
## the EB algorithm.
wt.aces <- walktrap.community(aces)
sizes(wt.aces)

## Which community is Mark Rittman in?
(marks.comm <- wt.aces$membership[wt.aces$names == "Mark Rittman"])

## Who else is in that community?
V(aces)$name[membership(wt.aces)==marks.comm]

## What is listed as their expertise on the aces website?
V(aces)$expertise[membership(wt.aces)==marks.comm]

## Let's get a sense of the homophily of 2 other groups
V(aces)$expertise[membership(wt.aces) == 1]
V(aces)$expertise[membership(wt.aces) == 4]

## Fast Greedy
## The walktrap algorithm above works with networks much larger than our speaker network,
## but the time it takes to run grows with the square of the number of nodes
## times the number of edges O(|V||E|^2).  For very large networks we can turn to
## the fast greedy algorithm, which takes less time to run as the network size
## increases O(|V||E|log|V|).
fg = fastgreedy.community(as.undirected(g))
fg

## Plot the communitites found by the algorithm
gplot(dat = as.matrix(get.adjacency(g)),
      label = V(g)$name,
      vertex.col = brewer.pal(3, "Set1")[membership(fg)],
      mode = "kamadakawai",
      pad = 0,
      label.pos = 1,
      vertex.sides = 100,
      vertex.border = "#00000066",
      edge.col = "#00000011")

## Plot the dendrogram
op <- par(mar = par("mar") + c(5,0,0,0))
plot(as.dendrogram(fg))
par(op)

## Let's try fastgreedy on our aces network.
fg.aces <- fastgreedy.community(as.undirected(aces))
sizes(fg.aces)

## Which community is Mark Rittman in?
(marks.comm <- fg.aces$membership[fg.aces$names == "Mark Rittman"])

## Who else is in that community
V(aces)$name[membership(fg.aces)==marks.comm]
V(aces)$expertise[membership(fg.aces)==1]
V(aces)$expertise[membership(fg.aces)==2]
V(aces)$expertise[membership(fg.aces)==3]

## Modularity is one way to measure of the success of a community finding
## algorithm.  The higher the modularity, the more dense edges within groups are
## and the more sparse edges reaching outside of the groups are.

## We can use igraph's modularity function to find the modularity of the divisions
## created by our two algorithms.
modularity(fg)
modularity(wt)

modularity(fg.aces)
modularity(wt.aces)

## Modularity is not the only indicator of a good community finding approach. In
## practice, it makes sense to try multiple algorithms and select one for
## production based on factors such as speed, interpretability, and modularity.

## Finally, let's save a plot of our aces network with the nodes colored by
## expertise.  We can save the plot as a large PNG in order zoom in
## and out.
load("data/aces_coords.RData")

node.size <- log(V(aces)$num.followers)/8
node.color <- brewer.pal(11, "Paired")[as.factor(V(aces)$expertise)]

png(
  "figures/aces.png",
  width     = 6400,
  height    = 6400,
)
gplot(dat = as.matrix(get.adjacency(aces)),
      coord = aces.coords,
      label = V(aces)$name,
      mode = "fruchtermanreingold",
      layout.par = list(niter = 10000),
      pad = -0.5,
      boxed.labels = TRUE,
      label.bg = "#ffffff55",
      label.border = "#ffffff00",
      label.pad = 0,
      label.pos = 1,
      vertex.sides = 100,
      vertex.cex = node.size,
      vertex.border = "#00000066",
      vertex.col = node.color,
      edge.col = "#00000011",
      arrowhead.cex = .25)

legend(-40, 20, levels(as.factor(V(aces)$expertise)),
       fill=brewer.pal(11, "Paired"), cex=4)

dev.off()

################################################################################
## Hands On Section 6 - Predictive analytics with network attributes

## What attributes do we have from twitteR?
list.vertex.attributes(aces)

## Degree Centrality
in.deg <- degree(aces, mode=c("in"))
out.deg <- degree(aces, mode=c("out"))

## Closeness Centrality
in.closeness <- closeness(aces, mode = "in", normalized=TRUE)
out.closeness <- closeness(aces, mode = "out", normalized=TRUE)

## Betweenness Centrality
betweenness = betweenness(aces,directed=TRUE)

## Eigenvector Centrality
pr <- page.rank(aces)$vector
res<-lm(pr~in.deg)$residuals

## Create a dataframe with the attributes we'll use to predict whether or not an
## ace is an ace director
aces.df <- data.frame(num.followers = V(aces)$num.followers,
                      num.followees = V(aces)$num.followees,
                      num.tweets = V(aces)$num.tweets,
                      in.deg = in.deg,
                      out.deg = out.deg,
                      in.closeness = in.closeness,
                      out.closeness = out.closeness,
                      betweenness = betweenness,
                      pr = pr,
                      res = res)

## Create a target variable to predict.  It has to be a factor for caret to
## recognize we want a classification model and not a regression one.
target <- ifelse(V(aces)$type == "ACE Director", "ACE.Director", "ACE")
target <- factor(target, levels=(c("ACE.Director", "ACE")))

## Create Training/Testing Partition where we build a model with 3/4 of the
## data and withhold 1/4 to test how well our model will work on data it's
## never seen
set.seed(1234)
inTrain <- createDataPartition(target, p = 3/4,
                               list = FALSE)
## training data
trainClass <- target[inTrain]
trainDescr <- aces.df[inTrain,]

## test data
testClass <- target[-inTrain]
testDescr <- aces.df[-inTrain,]

## Here we set up some parameters for the training procedure of our model
## We'll use cross validation to pick the best model from the many we
## generate by using the caret train function.
cvControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3,
                          classProbs = TRUE, summaryFunction = twoClassSummary)

## Train and tune a random forest model.
rfFit <- train(trainDescr, trainClass, method = "rf",
                trControl = cvControl, verbose=FALSE,
                metric="ROC", importance=TRUE, tuneLength=9)

## View a confusion matrix to evaluate the performance of our model with
## the held out sample.
predictions <- extractPrediction(list(rfFit),
                                 testX = testDescr, testY = testClass)
predictions <- predictions[predictions$dataType == "Test",]
confusionMatrix(predictions$pred, predictions$obs)

## Importance of our features from 0 to 100, with 100 being maximum importance
varImp(rfFit)
