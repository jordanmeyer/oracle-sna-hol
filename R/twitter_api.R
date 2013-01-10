
## twitteR allows R to communicate with the twitter API
## http://cran.r-project.org/web/packages/twitteR/index.html
require('twitteR')

################################################################################
## Set up a twitter API credential --- **RUN ONCE AND SAVE CREDENTIAL**
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL = "http://api.twitter.com/oauth/access_token"
authURL = "http://api.twitter.com/oauth/authorize"
consumerKey = "YOURKEYHERE"
consumerSecret = "YOURSECRETHERE"
twitCred <- OAuthFactory$new(consumerKey=consumerKey,
                             consumerSecret=consumerSecret,
                             requestURL=requestURL,
                             accessURL=accessURL,
                             authURL=authURL)

## This step requires interaction each time it's run
twitCred$handshake()

## Save the credential for future use without the handshake
save(twitCred, file="../secure/credentials.RData")

## See get_tweets.R and get_speakers.R for examples of using the saved cred
