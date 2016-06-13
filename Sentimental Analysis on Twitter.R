# Get all required libraries
library("ROAuth")
library("twitteR")
library("wordcloud")
library("tm")
require(sentiment)
require(plyr)
require(stringr)
require(plotrix)
require(ggplot2)

#Esablish connection with Twitter api
#Provide your Twitter api key,secret,access_token,access_token_secret 
api_key <- ""   
api_secret <- ""
access_token <- ""
access_token_secret <- ""
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
1


#Search twitter for tweets with a keyword and initialize them to tweets variable
tweets = searchTwitter("#ebola", n=1500, lang="en")

#Clean the tweets by removing unnessary chars like symbols,digits,http,re-tweets,@,punct
clean_tweets<-sapply(tweets, function(x) x$getText())
clean_tweets<-gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweets)
clean_tweets<-gsub("@\\w+", "", clean_tweets)
clean_tweets<-gsub("[[:punct:]]", "", clean_tweets)
clean_tweets<-gsub("[[:digit:]]", "", clean_tweets)
clean_tweets<-gsub("http\\w+", "", clean_tweets)
clean_tweets<-gsub("[ \t]{2,}", "", clean_tweets)
clean_tweets<-gsub("^\\s+|\\s+$", "", clean_tweets)

#function to initialize values that are missing
try.error = function(x)
{
   z = NA
   try_error = tryCatch(tolower(x), catch=function(e) e)
   #incase of no error
   if (!inherits(try_error, "error"))
   z = tolower(x)
   return(z)
}
clean_tweets<-sapply(clean_tweets, try.error)
clean_tweets<-clean_tweets[!is.na(clean_tweets)]
names(clean_tweets) = NULL

#Catagorize emotions
class_emo<-classify_emotion(clean_tweets, algorithm="bayes", prior=1.0)
emotions<-class_emo[,7]
emotions[is.na(emotions)]="unknown"
class_pol<-classify_polarity(clean_tweets, algorithm="bayes")
polarity<-class_pol[,4]
sent_df<-data.frame(text=clean_tweets, emotions=emotions,
polarity<-polarity, stringsAsFactors=FALSE)
sent_df<-within(sent_df,
emotions<-factor(emotions, levels=names(sort(table(emotions), decreasing=TRUE))))

#Get the set of Positive and Negtive words from Word bank text files
pos<-scan('Wordbank\positive-words.txt',what='character',comment.char=';')
neg<-scan('Wordbank\negative-words.txt',what='character',comment.char=';')

#Create function to scan and anaylyze the tweets with postive and negative words
score.sentiment<-function(lines, pos.words, neg.words, .progress='none') 
{
scores<-laply(lines, function(tweet_line, pos.words, neg.words) 
{
tweet_line<-gsub('[[:punct:]]', '', tweet_line)
tweet_line<-gsub('[[:cntrl:]]', '', tweet_line)
tweet_line<-gsub('\\d+', '', tweet_line)
tweet_line<-tolower(tweet_line)
word.list<-str_split(tweet_line, '\\s+')
words<-unlist(word.list)
pos.matches<-match(words, pos.words)
neg.matches<-match(words, neg.words) 
pos.matches<-!is.na(pos.matches) 
neg.matches<-!is.na(neg.matches)
score = sum(pos.matches) - sum(neg.matches) 
return(score) 
}, pos.words, neg.words, .progress=.progress ) 
scores.df = data.frame(score=scores, text=lines)
return(scores.df)
}

#call the function created above 
analysis<-score.sentiment(clean_tweets, pos, neg)

#pass the table values of analysis to variable b
b=table(analysis$score)

#Initalize some new variable for Negative,Positive and Neutral
n<-0
p<-0
c<-0

#For loop to calculate n,p and c aggreagated values
for (i in 1:length(b)){ if(names(b[i])<0) {n=n+as.vector(b[i])} else if(names(b[i])>0) {p=p+as.vector(b[i])} else if (names(b[i])==0) {c=as.vector(b[i])}}

#Plot the qplot
q = qplot(analysis$score)+xlab("Sentimental Score") +ylab("Number of Tweets")+ggtitle("QPlot-Sentimental Analysis")
q


#Plot the Pie chart with the above n,p,c values
slices <- c(n,p,c)
lbls <- c("Negative","Positive","Neutral")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) 
lbls <- paste(lbls,"%",sep="") 
pie3D(slices,labels=lbls,explode=0.1,main="Pie Chart-Sentimental Analysis")


#Plot WordCloud with the emotions determined from the tweets
emos<-levels(factor(sent_df$emotions))
nemo<-length(emos)
emotion.docs<-rep("", nemo)
for (i in 1:nemo)
{
   tmp = clean_tweets[emotions == emos[i]]
   emotion.docs[i] = paste(tmp, collapse=" ")
}

emotion.docs<-removeWords(emotion.docs, stopwords("english"))
corpus<-Corpus(VectorSource(emotion.docs))
tdm<-TermDocumentMatrix(corpus)
tdm<-as.matrix(tdm)
colnames(tdm)<-emos
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "Word Cloud-Sentimental Analysis")
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),scale = c(3,.5), random.order = FALSE, title.size = 1.5,main="Title")

