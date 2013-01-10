
## twitteR allows R to communicate with the twitter API
## http://cran.r-project.org/web/packages/twitteR/index.html
require('twitteR')

## Here we source a set of functions assembled for this lab.  Separating them
## from the lab's code allows us to focus on the concepts rather than the
## details of implementation.  You are encouraged to look through these
## functions after you are comfortable with the lab's content.
source('R/functions.R')

## load twitter API credentials - see twitter_api.R for details
load("secure/credentials.RData")
registerTwitterOAuth(twitCred)

## The twitteR package can do a basic search without an API connection.  Let's
## download the available tweets containing #biwasummit.  We'll use these
## later in the sentiment analysis section.  Note that this is changing in the
## next release of twitteR.  The new twitter API (1.1) requires authentication
## for searching.
oracle.tweets <- searchTwitter('#oracle', n=1500)

## View the 5 most recent #oracle tweets
head(oracle.tweets, 5)

## Look at the structure of a tweet object.  The twitteR package gives us much
## more than the text alone.
str(oracle.tweets[[1]])

## Create a new vector from the tweets with only the tweet's text
oracle.tweets <- laply(oracle.tweets, function(t) t$getText())

## Run some simple cleaning functions found in functions.R
oracle.tweets <- laply(oracle.text, CleanTweet)

save(oracle.tweets, file="data/oracle_tweets.RData")
