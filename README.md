# Sentiment_Analysis

###### Web scraping and Text Analytics

Stocks price movement normally depends on investors' sentiment on the concern. 
Every day, we collect various news about the company through different sources like GoogleNews, YahooFinance, Social    Networks sites and Printed medias. 
        So concentrating and monitoring a particular stock is a complex task. Data collection and cleaning of the text data from the web sources are the key steps on Natural Language Processing (NLP) algorithms. We need to sort out positive and negative news with proper scoring pattern. In this work, I use R package "tm" for sentiment analysis with support of other packages for cleaning and plotting theme web mined data.

## Sentiment Analysis for "Crude Oil" @ 01/11/2016

        >> head(clean.head.google)
        
        [1] "crude oil is getting slammed after another opec meeting flop  business insider"                                 
        [2] "iran eyes new crude oil buyers  press tv"                                                                       
        [3] "heres what a bunch of analysis is saying about crude oil right now  business insider"                           
        [4] "crude oil trend boosts heating oil prices creating steepest price increase in  years  pr newswire press release"
        [5] "global commodities firm marex spectron opens calgary crude oil office  the globe and mail"                      
        [6] "serbias nis mo cons net profit falls on low crude oil prices  seenews"    

![sentiment_crudeoil](https://cloud.githubusercontent.com/assets/16385390/19896524/eb1cc8c0-a054-11e6-8ac3-6ba243a24a24.png)

## Sentiment Analysis for "NIFTY" @ 01/11/2016
![sentiment_nifty](https://cloud.githubusercontent.com/assets/16385390/19884640/f7c595aa-a01a-11e6-9e0a-7e4ed8b06224.png)
