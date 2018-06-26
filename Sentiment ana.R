#***************************************************#
#Link twitter API
library(ROAuth)
library(twitteR)

consumer_key <-"R0FSYjaxukpYQ4i8M4Yku27Zu"
consumer_secret <- "6oIWQTrLO0yHS3nu5PBfM8mhL518udKaJa5Fs00NSbpY7kDXNZ"
access_token<-"344611954-TuHETldB82okM4fdctH1JUOA4EyADodkXM66f9yx"
access_secret <- "RgKSJJ9fo9ZZXTwBNUFvHLRIU96tNKE2t0Ds5xMlR3zH2"

setup_twitter_oauth(consumer_key ,consumer_secret, access_token,  access_secret )

#***********************************************#

#Extract tweets
delta_tweets<-searchTwitteR("@Delta",n=1000)          #Delta Airlines
SW_tweets<-searchTwitteR("@southwestair",n=1000)      #Southwest Airlines
Spirit_tweets<-searchTwitteR("@spiritairlines",n=1000)#Spirit Airlines
AA_tweets<-searchTwitteR("@americanair",n=1000)       #American Airlines
#**************************************************#
#Opinion Lexicon
pos.words=scan('positive-words.txt',what='charachter',comment.char = ';')
neg.words=scan('negative-words.txt',what='charachter',comment.char = ';')

neg.words=c(neg.words,'delay','sucks','wtf','stranded','wait','clogged')
#***************************************************#

#Extracting tweet texts
sample=NULL
for (tweet in delta_tweets)
sample=c(sample,tweet$getText())

samplesw=NULL
for (tweet in SW_tweets)
  samplesw=c(samplesw,tweet$getText())

samplespi=NULL
for (tweet in Spirit_tweets)
  samplespi=c(samplespi,tweet$getText())

sampleaa=NULL
for (tweet in AA_tweets)
  sampleaa=c(sampleaa,tweet$getText())

#****************************************************#
#Sentiment Scoring

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  list=lapply(sentences, function(sentence, pos.words, neg.words)
  {
    sentence = gsub('[[:punct:]]',' ',sentence)
    sentence = gsub('[[:cntrl:]]','',sentence)
    sentence = gsub('\\d+','',sentence)
    sentence = gsub('\n','',sentence)
    
    sentence = tolower(sentence)
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    pp=sum(pos.matches)
    nn = sum(neg.matches)
    score = sum(pos.matches) - sum(neg.matches)
    list1=c(score, pp, nn)
    return (list1)
  }, pos.words, neg.words)
  score_new=lapply(list, `[[`, 1)
  pp1=score=lapply(list, `[[`, 2)
  nn1=score=lapply(list, `[[`, 3)
  
  scores.df = data.frame(score=score_new, text=sentences)
  positive.df = data.frame(Positive=pp1, text=sentences)
  negative.df = data.frame(Negative=nn1, text=sentences)
  
  list_df=list(scores.df, positive.df, negative.df)
  return(list_df)
}

#**********************************************************#
#Scoring the results

delta_score=score.sentiment(sample,pos.words,neg.words)
SW_score=score.sentiment(samplesw,pos.words,neg.words)
Spirit_score=score.sentiment(samplespi,pos.words,neg.words)
AA_score=score.sentiment(sampleaa,pos.words,neg.words)

library(reshape)
#Creating a copy of delta_score data frame
test1=delta_score[[1]]
test2=delta_score[[2]]
test3=delta_score[[3]]

#Creating a copy of SW_score data frame
test4=SW_score[[1]]
test5=SW_score[[2]]
test6=SW_score[[3]]

#Creating a copy of Spirit_score data frame
test7=Spirit_score[[1]]
test8=Spirit_score[[2]]
test9=Spirit_score[[3]]

#Creating a copy of AA_score data frame
test10=AA_score[[1]]
test11=AA_score[[2]]
test12=AA_score[[3]]


#Creating three different data frames for Score, Positive and Negative
#Removing text column from data frame
test1$text=NULL
test2$text=NULL
test3$text=NULL
test4$text=NULL
test5$text=NULL
test6$text=NULL
test7$text=NULL
test8$text=NULL
test9$text=NULL
test10$text=NULL
test11$text=NULL
test12$text=NULL
#Storing the first row(Containing the sentiment scores) in variable q
q1=test1[1,]
q2=test2[1,]
q3=test3[1,]
q4=test4[1,]
q5=test5[1,]
q6=test6[1,]
q7=test7[1,]
q8=test8[1,]
q9=test9[1,]
q10=test10[1,]
q11=test11[1,]
q12=test12[1,]

qq1=melt(q1, ,var='Score')
qq2=melt(q2, ,var='Positive')
qq3=melt(q3, ,var='Negative')

qq4=melt(q4, ,var='Score')
qq5=melt(q5, ,var='Positive')
qq6=melt(q6, ,var='Negative')

qq7=melt(q7, ,var='Score')
qq8=melt(q8, ,var='Positive')
qq9=melt(q9, ,var='Negative')

qq10=melt(q10, ,var='Score')
qq11=melt(q11, ,var='Positive')
qq12=melt(q12, ,var='Negative')

qq1['Score'] = NULL
qq2['Positive'] = NULL
qq3['Negative'] = NULL

qq4['Score'] = NULL
qq5['Positive'] = NULL
qq6['Negative'] = NULL

qq7['Score'] = NULL
qq8['Positive'] = NULL
qq9['Negative'] = NULL

qq10['Score'] = NULL
qq11['Positive'] = NULL
qq12['Negative'] = NULL

#Creating data frame
table1 = data.frame(Text=delta_score[[1]]$text, Score=qq1)
table2 = data.frame(Text=delta_score[[2]]$text, Score=qq2)
table3 = data.frame(Text=delta_score[[3]]$text, Score=qq3)

table4 = data.frame(Text=SW_score[[1]]$text, Score=qq4)
table5 = data.frame(Text=SW_score[[2]]$text, Score=qq5)
table6 = data.frame(Text=SW_score[[3]]$text, Score=qq6)

table7 = data.frame(Text=Spirit_score[[1]]$text, Score=qq7)
table8 = data.frame(Text=Spirit_score[[2]]$text, Score=qq8)
table9 = data.frame(Text=Spirit_score[[3]]$text, Score=qq9)

table10 = data.frame(Text=AA_score[[1]]$text, Score=qq10)
table11 = data.frame(Text=AA_score[[2]]$text, Score=qq11)
table12 = data.frame(Text=AA_score[[3]]$text, Score=qq12)

#Merging three data frames into one
table_delta=data.frame(Text=table1$Text, Score=table1$value,
                       Positive=table2$value, Negative=table3$value)
table_SW=data.frame(Text=table4$Text, Score=table4$value,
                       Positive=table5$value, Negative=table6$value)
table_Spirit=data.frame(Text=table7$Text, Score=table7$value,
                       Positive=table8$value, Negative=table9$value)
table_AA=data.frame(Text=table10$Text, Score=table10$value,
                       Positive=table11$value, Negative=table12$value)


#************************************************************************#


#Histogram Delta
library(ggplot2)
ggplot(table_delta,aes(table_delta$Positive))+geom_histogram(binwidth = 1,fill= rainbow(5))
ggplot(table_delta,aes(table_delta$Negative))+geom_histogram(binwidth = 1,fill=rainbow(6))
ggplot(table_delta,aes(table_delta$Score,xlab='Sentiment'))+
  geom_histogram(binwidth = 1,fill=rainbow(9))+labs(title="Delta Airlines",x="Sentiment Score",y="Tweet Count")

hist(table_delta$Negative, col=rainbow(10))
hist(table_delta$Score, col=rainbow(10))

#Histogram SW
hist(table_SW$Positive, col=rainbow(10))
hist(table_SW$Negative, col=rainbow(10))
hist(table_SW$Score, col=rainbow(10))
ggplot(table_SW,aes(table_SW$Score))+geom_histogram(binwidth = 1,fill=rainbow(9))+labs(title="Southwest Airlines",x="Sentiment Score",y="Tweet Count")

hist(table_Spirit$Positive, col=rainbow(10))
hist(table_Spirit$Negative, col=rainbow(10))
hist(table_Spirit$Score, col=rainbow(10))
ggplot(table_Spirit,aes(table_Spirit$Score))+geom_histogram(binwidth = 1,fill=rainbow(9))+labs(title="Spirit Airlines",x="Sentiment Score",y="Tweet Count")

hist(table_AA$Positive, col=rainbow(10))
hist(table_AA$Negative, col=rainbow(10))
hist(table_AA$Score, col=rainbow(10))
ggplot(table_AA,aes(table_AA$Score))+geom_histogram(binwidth = 1,fill=rainbow(10))+labs(title="American Airlines",x="Sentiment Score",y="Tweet Count")


#Pie
#Delta
slices <- c(sum(table_delta$Positive), sum(table_delta$Negative))
labels <- c("Positive", "Negative")
library(plotrix)
#pie(slices, labels = labels, col=rainbow(length(labels)), main="Sentiment Analysis")
pie(slices, labels = labels, col=rainbow(length(labels)), main="Sentiment Analysis for Delta")

View(slices)
#Southwest
slices <- c(sum(table_SW$Positive), sum(table_SW$Negative))
labels <- c("Positive", "Negative")
#pie(slices, labels = labels, col=rainbow(length(labels)), main="Sentiment Analysis")
pie3D(slices, labels = labels, col=rainbow(length(labels)),explode=0.00, main="Sentiment Analysis")

#American
slices <- c(sum(table_AA$Positive), sum(table_AA$Negative))
labels <- c("Positive", "Negative")
#pie(slices, labels = labels, col=rainbow(length(labels)), main="Sentiment Analysis")
pie(slices, labels = labels, col=rainbow(length(labels)),explode=0.00, main="Sentiment Analysis")

#Spirit
slices <- c(sum(table_Spirit$Positive), sum(table_Spirit$Negative))
labels <- c("Positive", "Negative")
#pie(slices, labels = labels, col=rainbow(length(labels)), main="Sentiment Analysis")
pie(slices, labels = labels, col=rainbow(length(labels)),explode=0.00, main="Sentiment Analysis")
#**********************************************************************************#


#Good

Sc = table_Spirit$Score

#Output of following is FALSE or TRUE
good <- sapply(Sc, function(Sc) Sc <= 3 && Sc > 0)
#Converts to actual value
Sc[good]
list_good = Sc[good]
value_good = length(list_good)

#Very good

vgood <- sapply(Sc, function(Sc) Sc > 3 && Sc <6)
#Converts to actual value
Sc[vgood]
list_vgood = Sc[vgood]
value_vgood = length(list_vgood)

#Outstanding

vvgood <- sapply(Sc, function(Sc) Sc >= 6)
#Converts to actual value
Sc[vvgood]
list_vvgood = Sc[vvgood]
value_vvgood = length(list_vvgood)

#Bad : Unsatisfactory

#Output of following is FALSE or TRUE
bad <- sapply(Sc, function(Sc) Sc >= -3 && Sc < 0)
#Converts to actual value
Sc[bad]
list_bad = Sc[bad]
value_bad = length(list_bad)

#Very bad : Poor

#Output of following is FALSE or TRUE
vbad <- sapply(Sc, function(Sc) Sc < -3 && Sc > -6)
#Converts to actual value
Sc[vbad]
list_vbad = Sc[vbad]
value_vbad = length(list_vbad)

#Awful

vvbad <- sapply(Sc, function(Sc) Sc <= -6)
#Converts to actual value
Sc[vvbad]
list_vvbad = Sc[vvbad]
value_vvbad = length(list_vvbad)

#Neutral
neutral <- sapply(Sc, function(Sc) Sc == 0) 
list_neutral = Sc[neutral]
value_neutral = length(list_neutral)

library(plotrix)
slices1 <- c(value_good, value_vvbad, value_bad, value_vgood, value_vbad, value_neutral, value_vvgood )
lbls1 <- c("Good", "Awful", "Unsatisfactory", "Great", "Poor", "Neutral", "Outstanding")
pct <- round(slices1/sum(slices1)*100) #Percentage
lbls1 <- paste(lbls1, pct) # add percents to labels 
lbls1 <- paste(lbls1,"%",sep="") # ad % to labels 
pie(slices1,labels = lbls1, col=rainbow(length(lbls1)),
    main="Number of tweets with particular sentiment")
#****************************************************************************#

#Wordcloud
library(wordcloud)
SW.tweets<- searchTwitter("@AmericanAir", lang="en", n=1500, resultType="recent")
SW_text = sapply(SW.tweets, function(x) x$getText())
df <- do.call("rbind", lapply(SW.tweets, as.data.frame))
SW_text <- sapply(df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
str(SW_text)

#corpus is a collection of text documents
library(tm)
SW_corpus <- Corpus(VectorSource(SW_text))
SW_corpus
inspect(SW_corpus[1])
#clean text
SW_clean <- tm_map(SW_corpus, removePunctuation)
SW_clean <- tm_map(SW_clean,content_transformer(tolower))
SW_clean <- tm_map(SW_clean, removeWords, stopwords("english"))
SW_clean <- tm_map(SW_clean, removeNumbers)
SW_clean <- tm_map(SW_clean, stripWhitespace)
SW_clean <- tm_map(SW_clean, removeWords,c("AmericanAir","inkglobal","jefflipsky","americanair","american","flight","trip","get","land","can","hey","week","time","just","know","now"))


#wordcloud
#wordcloud(quake_clean)
wordcloud(SW_clean, random.order=F)
#wordcloud(quake_clean, random.order=F, scale=c(4,0.5))#max font size,min font size
#wordcloud(quake_clean, random.order=F,col=rainbow(50), scale=c(4,0.5))
wordcloud(SW_clean, random.order=F,max.words=200,use.r.layout=FALSE,col=brewer.pal(8,"Dark2"), scale=c(5,0.5))
