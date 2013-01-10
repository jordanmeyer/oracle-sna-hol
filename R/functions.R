
###
## score.sentiment()
## The following function can be found in the following tutorial:
## http://www.inside-r.org/howto/mining-twitter-airline-consumer-sentiment
## I added the pos.matches and neg.matches to the returned dataframe so
## we'd have a bit more detail about the scoring.
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)

  ## we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
  ## we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
  scores = ldply(sentences, function(sentence, pos.words, neg.words) {

    ## clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    ## and convert to lower case:
    sentence = tolower(sentence)

    ## split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    ## sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)

    ## compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)

    ## match() returns the position of the matched term or NA
    ## we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)

    ## and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = data.frame(score = sum(pos.matches) - sum(neg.matches),
      pos = sum(pos.matches), neg=sum(neg.matches))

    return(score)
  }, pos.words, neg.words, .progress=.progress )

  scores.df = data.frame(score=scores$score, text=sentences,
    pos.matches=scores$pos, neg.matches=scores$neg)
  return(scores.df)
}

###
## CleanTweet()
## The following functions clean up tweets for further analysis
RemoveUnicode <- function(tweet){
  iconv(tweet, "UTF-8", "ASCII")
}

RemoveDots <- function(tweet) {
  gsub("[\\.\\,\\;]+", " ", tweet)
}

RemoveLinks <- function(tweet) {
  gsub("http:[^ $]+", "", tweet)
}

RemoveAtPeople <- function(tweet) {
  gsub("@\\w+", "", tweet)
}

CleanTweet <- function(tweet) {
  s1 <- RemoveUnicode(tweet)
  s2 <- RemoveLinks(s1)
  s3 <- RemoveAtPeople(s2)
  s4 <- RemoveDots(s3)
  s4
}

###
## delete.isolates()
## The following function deletes isolated nodes
delete.isolates <- function(graph, mode = 'all') {
  isolates <- which(degree(graph, mode = mode) == 0)
  delete.vertices(graph, isolates)
}

###
## reachability()
## Create a reachability matrix for a graph
reachability <- function(g, mode) {
  reach_mat = matrix(nrow = vcount(g),
    ncol = vcount(g))
  for (i in 1:vcount(g)) {
    reach_mat[i,] = 0
    this_node_reach <- subcomponent(g, (i), mode = mode)

    for (j in 1:(length(this_node_reach))) {
      alter = this_node_reach[j]
      reach_mat[i, alter] = 1
    }
  }
  return(reach_mat)
}
