
## This section is separated from the hands on lab because it requires manual
## instalation of imagemagick software: http://www.imagemagick.org/script/index.php
## The output of the function can be found in the figures folder.

## We'll use the animation package to create an animated gif of the edge betweenness
## algorithm
require("animation")

## This function is modified from the excellent social network analysis labs from
## Stanford university found here: http://sna.stanford.edu/rlabs.php
jitter.ani <-function(x, g){
  l <- layout.fruchterman.reingold(g, niter=5000)
  ebc <- edge.betweenness.community(g)

  colbar <- rainbow(6)
  colbar2 <- c(rainbow(5), rep("black",15))
  dg <- log(degree(g)) * 3

  for (i in 1:x) {
    print(i)
    g2 <- delete.edges(g, ebc$removed.edges[seq(length=i-1)]+1)
    eb <- edge.betweenness(g2)
    cl <- clusters(g2)$membership
    q <- modularity(g, cl)
    E(g2)$color <- "grey"
    E(g2)[ order(eb, decreasing=TRUE)[1:5] ]$color <- colbar2[1:5]

    E(g2)$width <- 1
    E(g2)[ color != "grey" ]$width <- 2

    plot(g2, layout=l, vertex.size=dg,
       edge.label.color="red", vertex.color=brewer.pal(8, "Set2")[cl+1],
       edge.label.font=2, vertex.label=V(g2)$id, vertex.label.dist = -0.25,
         vertex.label.color="black")
    title(main=paste("Modularity =", round(q,3)), font=2)
    ty <- seq(1,by=-strheight("1")*1.5, length=20)
    text(-1.3, ty, adj=c(0,0.5), round(sort(eb, dec=TRUE)[1:20],2),
       col=colbar2, font=2)
  }
}

## Read in The Godfather social interaction graph from moviegalaxies.com
tg <- read.graph('data/the_godfather.net', format=c("pajek"))
saveMovie(movie.name="figures/godfather.gif", jitter.ani(38, tg),
          interval = 2, outdir = getwd(), ani.height = 1000, ani.width = 1000)
