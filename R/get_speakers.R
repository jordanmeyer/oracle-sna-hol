
require('twitteR')
require('ROAuth')
require('igraph')

## load twitter API credentials - see twitter_api.R for details
load("secure/credentials.RData")
registerTwitterOAuth(twitCred)

################################################################################
## Create the biwa speaker network

## Manually create a vector of speaker twitter accounts
speakers <- c('@OracleAskTom',
              '@rstackow',
              '@mpd65',
              '@Cathye_Pendley',
              '@dvlamis',
              '@dan_mcclary',
              '@markrittman',
              '@brendantierney',
              '@kevin_mcginley',
              '@SQLMaria',
              '@florianschouten',
              '@EvansBI',
              '@chennaimuskito',
              '@MarkHornick',
              '@CharlieDataMine',
              '@arikaplan1',
              '@stewartbryson',
              '@borkur',
              '@bidashboardstev',
              '@jordancmeyer',
              '@sunil_ranka',
              '@shyamvaran',
              '@jeffs3030',
              '@Beaird_Nick',
              '@erichelmer')

## Create a list of twitteR user objects for the speakers
speakerinfo <- lapply(speakers, getUser)

## Create a vector of twitter IDs for the speakers
speakerids <- sapply(speakerinfo, id)

## Get a count of all speaker followers
speakerallfollowers <- sapply(speakerinfo, function(x) x$followersCount)

## Get a count of all speaker followers
speakertweets <- sapply(speakerinfo, function(x) x$statusesCount)

## Twitter 1.1 API only allows 15 follower requests per 15 minutes
## This function pauses before requesting followers
slowFollowers <- function(user){
  print(user$name)

  if (user$followersCount == 0){
    print('no followers')
    return()
  }

  if (user$protected){
    print('protected')
    return()
  }

  Sys.sleep(60)
  return(user$getFollowerIDs())
}

## Create a list of follower ID vectors for each speaker
userfollowers <- lapply(speakerinfo, slowFollowers)

## Keep only other speakers in the follower list
speakerfollowers <- lapply(userfollowers, FUN = function(x) intersect(x, speakerids))

## Create a new graph object and add speakers as nodes
g <- graph.empty()
g <- add.vertices(g, length(speakerinfo), name=sapply(speakerinfo, name), id=speakerids,
                  num.followers=speakerallfollowers, speaker.tweets=speakertweets)

## Remove newlines from names
V(g)$name <- gsub('\n', '', V(g)$name)

## Transform our follower list into an edge list matrix
edgelist <- matrix(,0,2)
loners <- c()
for(i in 1:length(speakerids)){
  if(length(speakerfollowers[[i]]) > 0){
    for(j in 1:length(speakerfollowers[[i]])){
      edgelist <- rbind(edgelist, c(speakerfollowers[[i]][j], speakerids[i]))
    }}
  else {
    loners <- c(loners, speakerids[i])
  }
}

## igraph references nodes by position, create a position vector
positions <- 1:length(speakerids)
names(positions) <- speakerids

## Create a matrix with edges listed by node number (not id) in each row
edges <- matrix(c(positions[edgelist[,1]], positions[edgelist[,2]]), nc=2)

## Add the transposed matrix as the graph edges
g <- add.edges(g, t(edges))

## Save the graph for future use
save(g, file = 'data/biwa_speaker_network.Rdata')
