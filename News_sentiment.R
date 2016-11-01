#+++++++++++++++++++++++++++++++++++++++++++++++++++++
# Sentiment analysis on National Stock Exhange (NSE), India data from googlenews
# 
# Text analytics

#Author: Ramanathan Perumal
#Mail: ramamet4@gmail.com
#Modified: 12/10/2016

#++++++++++++++++++++++++++++++++++++++++++++++++++++
 
 
          #-----------------------------------------------------
	  # Step-0: Problem statement
	  #-----------------------------------------------------
	  
         # Stocks price movement normally depends on investors' sentiment on the concern. 
         # Every day, we collect various news about the company through different sources like GoogleNews, YahooFinance, Social Networks sites
         # and Printed medias. 
         # So concentrating and monitoring a particular stock is a complex task. Data collection and cleaning of the text data from the web sources 
         # are the key steps on Natural Language Processing (NLP) algorithms. We need to sort out positive and negative news with proper 
         # scoring pattern. In this work, I use R package "tm" for sentiment analysis with support of other packages for cleaning and plotting theme
         # web mined data.
 
 
 	  #-----------------------------------------------------
	  # Step-1: Collecting data
	  #-----------------------------------------------------
 
 
 library(plyr)
 library(dplyr)
 library(stringr)
 library(ggplot2)
 library(sentiment)
 library(wordcloud)
 library(RColorBrewer)
 library(qdap)
 library(tm)
 library(tm.plugin.webmining)
 
 
##################score sentiment#######################################
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {   
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('#', "", sentence)
    sentence = gsub('@', "", sentence)
    sentence = gsub('\\d+', '', sentence)
    
    # and convert to lower case:
    sentence = tolower(sentence)
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

    
    	#-----------------------------------------------------
	# Step-2: Data cleaning 
	#-----------------------------------------------------


#clean the text 
clean.text = function(x)
{
   # tolower
   x = tolower(x)
   # remove rt
   x = gsub("rt", "", x)
   # remove at
   x = gsub("@\\w+", "", x)
   # remove punctuation
   x = gsub("[[:punct:]]", "", x)
   # remove numbers
   x = gsub("[[:digit:]]", "", x)
   # remove links http
   x = gsub("http\\w+", "", x)
   # remove graphical characters
   #x= gsub("[:graph:]","", x)
   # remove tabs
   #x = gsub("[ |\t]{2,}", "", x)
   # remove blank spaces at the beginning
   #x = gsub("^ ", "", x)
   # remove blank spaces at the end
   x = gsub(" $", "", x)
   # remove UTF-8 symbol in tweets
   #x= gsub("\u0081|`", "",x)
   #remove \n 
   #x = gsub("\n", "", x)
   #unneccesary \U marks
   #x = gsub("\U", "", x)
   #unneccesary \t marks
   #x=gsub("[ \t]{2,}", "",x)
   return(x)
}

tryTolower = function(x)
{
  # create missing value
  # this is where the returned value will be
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y)
}


        #-----------------------------------------------------
	# Step-3: Training a model on the data
	#-----------------------------------------------------

##########################################################
#positive and negative wordlist from internet 
pos <-scan('https://raw.githubusercontent.com/jeffreybreen/twitter-sentiment-analysis-tutorial-201107/master/data/opinion-lexicon-English/positive-words.txt', what='character', comment.char=';')
neg <- scan('https://raw.githubusercontent.com/jeffreybreen/twitter-sentiment-analysis-tutorial-201107/master/data/opinion-lexicon-English/negative-words.txt', what='character', comment.char=';')

############################################################## 

news="NIFTY" 
news.sep= gsub("\\s", "", news) 

googlenews <- WebCorpus(GoogleNewsSource(news)) #, since="1-1-2015", until="31-1-2015"))
headers.google<-meta(googlenews,tag="heading")
clean.head.google=clean.text(headers.google)
fact.clean.head.google=as.factor(clean.head.google)
df.clean.head.google=as.factor(clean.head.google)


         #------------------------------------------------
	# Step-4: Improving model performance 
	#-----------------------------------------------

#sentimental scores for the news
scores <- score.sentiment(fact.clean.head.google, pos, neg, .progress='text')
 
 
 scores.tab=table(scores$score)

 ram=data.frame(scores.tab)
 ram$score=as.numeric(as.character(ram$Var1))

 ram$Color <- ifelse(ram$score>0, "positive",
             ifelse(ram$score<0, "negative",
              "neutral"  ))
              
 ram$Subject=as.factor(as.character(ram$Color))

 xLab=xlab(expression(paste("score")))
 yLab=ylab(expression(paste("counts")))  
 
 length.news=length(googlenews)
 
 
 
        #-----------------------------------------------------
	# Step-5: Output presentation
	#-----------------------------------------------------
 
 p1= ggplot(ram,aes(x=Var1,y=Freq,fill=Subject))+
    geom_bar(stat="identity")+
    geom_text(aes(label=ram$Freq), vjust=1.6, color="white", size=3.5)+
    scale_fill_brewer(palette="Dark2")
  #  scale_fill_manual(values=c("darkred", "royalblue", "darkgreen"))
  
p2=p1+xLab+yLab+
   # scale_x_continuous(expand = c(0, 0))+
    scale_y_continuous(expand = c(0, 0),limits=c(-10,(max(ram$Freq)+20))) +
   # guides(color = guide_legend(nrow=4))+
    theme_bw()+ ggtitle(paste0("Sentiment analysis of ",length.news," news headlines on '",news,"'",sep=""))+
    theme(plot.title = element_text(lineheight=5, face="bold"))+
    theme(panel.border = element_rect(linetype = "solid", colour = "black", size=2))+
    theme(legend.position=c(0.20,0.75))+
    theme(axis.title.y = element_text(size = rel(1.5), angle = 90))+
    theme(axis.title.x = element_text(size = rel(1.5), angle = 0))+
    theme(axis.text = element_text(size=15)) +
    theme(panel.border = element_rect(linetype = "solid", colour = "black", size=2))+
    theme(legend.text = element_text(size = 15, colour = "grey1"))+
    theme(legend.background = element_rect(colour = "grey60"))+
    theme(legend.title = element_text(size = 15, colour = "gray3"))+
    geom_vline(xintercept = which(ram$Var1 == '0'), color="yellow2", size=2, linetype=4,alpha=0.7)
    #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    #guides(colour=FALSE)
  
 pdf(paste0("Sentiment_",news.sep,".pdf",sep=""),width=10,height=7)
 print("sentimentAnalysis printed-----")
 print(p2)	
 dev.off()
 
 #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
