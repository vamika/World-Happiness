r1<-read.csv("C://Users//Vamika Razdan//Downloads//worldhappinessreport.csv")
fb_authr<-function(){
  fb_auth=fbOAuth(app_id = "XXXX",app_secret = "XXXX",extended_permissions = TRUE)
}
dataanalysis<-function(){
  
  print("Welcome to the training of DataScience using R")
  print("Enter the analysis topic you want to start off with")
  print("1. Filter using dplyr ")
  print("2. Twitter Data Mining")
  print("3. Facebook Data Mining")
  print("4. KNN Classification")
  print("5. Regression")
  print("6. Decision Tree")
  print("7. Clustering")
  print("8.Exit")
  t= scan()
  #Filter
  filter12<-function(){
    library(dplyr)
    print("Do you want to see the data year wise?If yes enter 'y'else 'n ")
    o=scan(what='character')
    if(o=='y'|| o=='Y' ){
      print("Enter the choice number from the options:")
      print("1. 2015")
      print("2. 2016")
      print("3. 2017")
      q=scan()
      if(q==1){
          w<-r1%>%filter(Year==2015)
        View(w)
        s=w[,1:3]
        View(s)
        s[,2]<-NULL
        # just focus on one year
        w$Country
        s$Country
        #w$Country <- gsub('\\s', '', w$Country) #strip spaces to join with h (happiness scores)
        #s$Country <- gsub('\\s', '', s$Country) #strip spaces to join with data (happiness scores)
        # Just focus on col 4:9 - GDP, Social Support, Life Expectancy, Freedom, Generosity, Corruption
        # where there is a complete data-set (no "NAs")
        s12 <- w[complete.cases(w[,c(1,4:9)]), ]#it removes the null values from a vector nd combines the rows without null values or removing null values.
        #join the hapiness ranking on as 1st column
        #s13 <- merge(s,s12, by.x = "Country", by.y="Country")
        # Create a K-Means cluster with 3 groups based on cols 5:10
        # (GDP, Social Support, Life Expectancy, Freedom, Generosity, Corruption)
        km <- kmeans(s12[, 5:10],3, iter.max=100)
        g1 <- subset[km$cluster == 1,]$Happiness.Score.y
        g2 <- subset[km$cluster == 2,]$Happiness.Score.y
        g3 <- subset[km$cluster == 3,]$Happiness.Score.y
        hist(g1, xlim=c(0,10), col=rgb(1,0,0,0.5), breaks=seq(0.25,10,0.25) 
             
             , main = "Histogram of Happiness Score for 3 cluster-groups"
             
             , xlab = "Country Happiness Score")
        g1
        
        hist(g2, xlim=c(0,10), col=rgb(0,1,0,0.5), breaks=seq(0.25,10,0.25), add=T)
        
        hist(g3, xlim=c(0,10), col=rgb(0,0,1,0.5), breaks=seq(0.25,10,0.25), add=T)
        legend("topright", c("Group1", "Group2", "Group3")
               
               , fill=c(rgb(1,0,0,0.5),rgb(0,1,0,0.5),rgb(0,0,1,0.5)) )
        top <- which.max(c(mean(g1),mean(g2), mean(g3))) # which is the top group
        
      }else if (q==2){
        r<-r1%>%filter(Year==2016)
        View(r)
        ab=r[,1:3]
        View(ab)
        ab[,2]<-NULL
        # just focus on one year
        r$Country
        ab$Country
        #w$Country <- gsub('\\s', '', w$Country) #strip spaces to join with h (happiness scores)
        #s$Country <- gsub('\\s', '', s$Country) #strip spaces to join with data (happiness scores)
        # Just focus on col 4:9 - GDP, Social Support, Life Expectancy, Freedom, Generosity, Corruption
        # where there is a complete data-set (no "NAs")
        ab12 <- r[complete.cases(r[,c(1,4:9)]), ]#it removes the null values from a vector nd combines the rows without null values or removing null values.
        #join the hapiness ranking on as 1st column
        #s13 <- merge(s,s12, by.x = "Country", by.y="Country")
        # Create a K-Means cluster with 3 groups based on cols 5:10
        # (GDP, Social Support, Life Expectancy, Freedom, Generosity, Corruption)
        
        km1 <- kmeans(ab12[, 5:10],3, iter.max=100)
        
        g11 <- subset[km1$cluster == 1,]$Happiness.Score.y
        
        g22 <- subset[km1$cluster == 2,]$Happiness.Score.y
        
        g33 <- subset[km1$cluster == 3,]$Happiness.Score.y
        
        hist(g11, xlim=c(0,10), col=rgb(1,0,0,0.5), breaks=seq(0.25,10,0.25) 
             
             , main = "Histogram of Happiness Score for 3 cluster-groups"
             
             , xlab = "Country Happiness Score")
        hist(g22, xlim=c(0,10), col=rgb(0,1,0,0.5), breaks=seq(0.25,10,0.25), add=T)
        hist(g33, xlim=c(0,10), col=rgb(0,0,1,0.5), breaks=seq(0.25,10,0.25), add=T)
        legend("topright", c("Group1", "Group2", "Group3")
               
               , fill=c(rgb(1,0,0,0.5),rgb(0,1,0,0.5),rgb(0,0,1,0.5)) )
        top <- which.max(c(mean(g11),mean(g22), mean(g33))) # which is the top group
        }else if (q==3){
        t<-r1%>%filter(r1$Year==2017)
        View(t)
        abc=t[,1:3]
        View(abc)
          abc[,2]<-NULL
      # just focus on one year
        t$Country
        abc$Country
          #w$Country <- gsub('\\s', '', w$Country) #strip spaces to join with h (happiness scores)
      #s$Country <- gsub('\\s', '', s$Country) #strip spaces to join with data (happiness scores)
          # Just focus on col 4:9 - GDP, Social Support, Life Expectancy, Freedom, Generosity, Corruption
       # where there is a complete data-set (no "NAs")
      abc12 <- t[complete.cases(t[,c(1,4:9)]), ]#it removes the null values from a vector nd combines the rows without null values or removing null values.
        #join the hapiness ranking on as 1st column
        #s13 <- merge(s,s12, by.x = "Country", by.y="Country")
        # Create a K-Means cluster with 3 groups based on cols 5:10
        # (GDP, Social Support, Life Expectancy, Freedom, Generosity, Corruption)
        km2 <- kmeans(abc12[, 5:10],3, iter.max=100)
        g111 <- subset[km2$cluster == 1,]$Happiness.Score.y
        g222 <- subset[km2$cluster == 2,]$Happiness.Score.y
        g333 <- subset[km2$cluster == 3,]$Happiness.Score.y
        hist(g111, xlim=c(0,10), col=rgb(1,0,0,0.5), breaks=seq(0.25,10,0.25) 
             
             , main = "Histogram of Happiness Score for 3 cluster-groups"
             
             , xlab = "Country Happiness Score")
        hist(g222, xlim=c(0,10), col=rgb(0,1,0,0.5), breaks=seq(0.25,10,0.25), add=T)
        hist(g333, xlim=c(0,10), col=rgb(0,0,1,0.5), breaks=seq(0.25,10,0.25), add=T)
        legend("topright", c("Group1", "Group2", "Group3")
               
               , fill=c(rgb(1,0,0,0.5),rgb(0,1,0,0.5),rgb(0,0,1,0.5)) )
        
        top <- which.max(c(mean(g111),mean(g222), mean(g333))) # which is the top group
        }
      }
    print("Do you want to see a scatter plot? If yes then press 'y' else 'n'")
    jk=scan(what = 'character')
    if(jk=='y'||jk=='Y'){
        plot(.~Year,data=r1)
      dotchart(r1[,,1],gdata=colMeans(r1[,,1]),gpch=16,gcolor='Red')
      scatterplot(Year~Country,data=r1,xlab="Year",ylab="Happiness.Rank")
      abline(lm(Year~Happiness.Score,data=r1),lwd=6,col='gray50')
    }
    dataanalysis()
  }
  
  #twitter 
  twitterdatamining<-function(){
    library(syuzhet)
    library(readr)
    library(twitteR)
    consumer_key <- 'XXXX'
    consumer_secret <- 'XXXXX'
    access_token<-'XXXXX'
    access_secret <- 'XXXXX'
    library(twitteR)
    setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
    
    #Fetching Tweets
    library(twitteR)
    
    tweet1 = searchTwitter("worldhappiness",n=10,lang="en", resultType = "recent")
    tweet1
    tweetdf=twListToDF(tweet1)
    write(tweetdf$text,file = "E:/R/tweet.txt",append = T)
    #Cleaning Twitter Data
    tweet_data.text = sapply(tweet1,function(x) x$getText())
    tweet_data.text
    #Remove Puntuation
    x=gsub("\\W"," ",tweet_data.text)
    x
    #Remove Digit
    x=gsub("\\d"," ",x)
    x
    #Convert to Lower
    x = tolower(x)
    x
    #Remove Retweet
    x = gsub("RT"," ",tweet_data.text)
    x
    #Remove @
    x = gsub("@\\w+"," ",x)
    x
    
    #Remove Links
    x = gsub("http\\w+"," ",x)
    x
    #Remove Tabs
    x = gsub("[|\t]{2,}"," ",x)
    x
    #Remove Blank Space at the beginning
    x = gsub("^"," ",x)
    x
    #Remove Blank Space at the End
    x = gsub("$"," ",x)
    x
    #Removing Certain Words(StopWords)
    library(tm)
    y = removeWords(x,c('happiness','world',stopwords()))
    y
    y = stripWhitespace(y)
    print(y)
    #Convert to Corpus
    tweet_corpus = Corpus(VectorSource(y))
    tweet_corpus
    #Convert Corpus to Plain Text Document
    tweet_corpus = tm_map(tweet_corpus,PlainTextDocument)
    #Stemming Document
    tweet_corpus = tm_map(tweet_corpus,stemDocument)
    library(wordcloud)
    par(mfrow=c(1,2))
    wordcloud(y, random.order=FALSE,min.freq = 0.45)
    #Perform Sentiment Analysis
    library(RSentiment)
    calculate_score(y)
    calculate_sentiment(y)
    calculate_total_presence_sentiment(y)
    #Sentiment Analysis
    sentiment = get_nrc_sentiment(y)
    sentiment
    z = cbind(y,sentiment)
    z
    #Count Sentiment Words by Category
    TotalSentiment = data.frame(colSums(z[,c(2:11)]))
    names(TotalSentiment) = "Count"
    TotalSentiment
    TotalSentiment = cbind("Sentiment"=rownames(TotalSentiment), TotalSentiment)
    TotalSentiment
    rownames(TotalSentiment) = NULL
    TotalSentiment
    #Plot the Sentiment Words
    barplot(TotalSentiment$Count, names.arg = TotalSentiment$Sentiment,xlab = "Sentiment", ylab="Total Count", main = "Total Sentiment Score", col = rainbow(11))
    dataanalysis()
  }
  
  #fb
  fb<-function(){
    library(Rfacebook)
    library(RCurl)
    library(stringr)
    library(wordcloud)
    m=getUsers("me",fb_auth,private_info = TRUE)#Extracting the info
    page=searchPages(string = "world happiness",token = fb_auth)#extracting pages with worldhappiness
    name123=(page$name)
    username=(page$username)
    df1=data.frame(name123,username)#dataframe
    row.has.na <- apply(df1, 1, function(x){any(is.na(x))})
    sum(row.has.na)
    final.filtered <- df1[!row.has.na,]#remove rows having NA values
    print("Search pages on happiness:")
    print(final.filtered)
    print("Wordcloud one page:")
    revi<-function(){
      print("Which page would you like to see?")
      xy=scan()
      page=getPage(page = df1[xy,2]  ,token = fb_auth,n=5)#info about the 
      msg=(page$message)#extract page message
      msg
      tr2=paste(msg,collapse = " ")
      tr2
      tr2=gsub(pattern = "\\W",replace=" ",tr2)
      tr2
      tr2=gsub(pattern = "\\d",replace=" ",tr2)
      tr2
      tr2=tolower(tr2)
      tr2
      library(tm)
      tr2=removeWords(tr2,stopwords())
      tr2
      tr2=gsub(pattern = "\\b[A-z]\\b{1}",replace=" ",tr2)
      tr2
      tr2=stripWhitespace(tr2)
      tr2
      tb1=str_split(tr2,pattern = "\\s+")
      tb1
      tb1=unlist(tb1)
      wordcloud(tb1,random.order = FALSE,min.freq = .35,col=rainbow(4))
      pw1=scan("E:/R/PROJECTPPT/positive-words.txt",what = 'character',comment.char = ";")
      pw1
      nw1=scan("E:/R/PROJECTPPT/negative-words.txt",what = 'character',comment.char = ";")
      nw1
      match(tb1,pw1)
      p=sum(!is.na(match(tb1,pw1)))
      p
      n=sum(!is.na(match(tb1,nw1)))
      n
      fd=cbind(p,n)
      score=p-n
      print(score)
      barplot(score,xlab = "Score",main="Sentiment Analysis",col=rainbow(2),ylim = c(0,60),xlim = c(0,10))
      abline(h=0)
      dataanalysis()
    }
    revi()
    dataanalysis()
  }
  
  
  #KNN
  knn12<-function(){
    
    summary(r1)
    View(r1)
    row_index=sample(1:nrow(r1),0.8*nrow(r1))
    #install.packages("class")
    r1$Country<-NULL
    library(class)
    View(r1)
    d=function(x){((x-min(x))/(max(x)-min(x)))}
    norm_data=lapply(r1[,-10],d)
    class(norm_data)
    norm_data=as.data.frame(norm_data)
    train_data=norm_data[row_index,]
    test_data=norm_data[-row_index,]
    pred_model<-knn(train_data,test_data,r1[row_index,10],k=5)
    pred_model
    predicted_table=table(pred_model,r1[-row_index,10])
    print("Predicted Model")
    print(predicted_table)
    a=0.2*nrow(r1)
    diag_element=diag(predicted_table)
    diag_element
    accuracy_r1=(sum(diag_element)/a)*100
    print(paste("The accuracy is:",accuracy_r1))
    print("Do you want to see a plot")
    jk=scan(what = 'character')
    if(jk=='y'|| jk=='Y' ){
      plot(Happiness.Score~.,data=r1,pch=2,cex=.5,col="blue") 
    }
    
    dataanalysis()
    
  }
  #Decision Tree
  decisiontree<-function(){
    
    View(r1)
    
    summary(r1)
    library(rpart)
    library(rpart.plot)
    cod<-rpart(Year~Happiness.Score,data=r1,method="class")
    print(cod)
    
    
    plot(cod,margin=0.1,main=" happiness")
    text(cod, use.n=TRUE, all=TRUE, cex=.7)
    library(rattle)
    fancyRpartPlot(cod)
    printcp(cod)
    plotcp(cod, minline = TRUE)
    cod1<-prune(cod,cp= 0.018)#which node to take
    fancyRpartPlot(cod1)
    print(cod1)
    #Predicting accuracy 
    library(dplyr)
    
    r1%>%mutate(Happy_Score=ifelse((Happiness.Score>5),1,0))->r1
    r1%>%select(-Happiness.Score)->r1
    #Confusion Matrix
    actual<-r1$Happy_Score
    predicted<-predict(cod1,type = "class")
    
    head(predicted)
    head(as.numeric(predicted))
    predicted<-as.numeric(predicted)
    predicted<-ifelse(predicted==2,1,0)
    library(caret)
    print(confusionMatrix(predicted,actual,positive="1"))
    library(irr)
    #kappa metric
    print(kappa2(data.frame(actual,predicted)))
    
    #ROC curve analysis
    library(ROCR)
    pred<-prediction(actual,predicted)
    perf<-performance(pred,"tpr","fpr")
    plot(perf,col="red")
    abline(0,1, lty = 8, col = "grey")
    
    auc<-performance(pred,"auc")
    unlist(auc@y.values)
    print(paste("The accuracy of the decision tree is",auc@y.values))
    dataanalysis()
  }
  #Regression
  regression<-function(){
    #    View(r1)
    scatter.smooth(x=r1$Happiness.Rank,y=r1$Happiness.Score,main="Happiness")#scatterplot
    abline(lm(Happiness.Score~Happiness.Rank,data=r1),lwd=6,col='grey50')
    par(mfrow=c(1,2))#divide graph area n 2 columns
    boxplot(r1$Freedom,main="Freedom",sub=paste("Outlier rows: "))
    boxplot(r1$Happiness.Score,main="Happiness Score",sub=paste("Outlier rows: "))
    set.seed(100) #setting seed to reproduce results of random sampling
    trainingRowIndex<- sample(1:nrow(r1),0.8*nrow(r1)) #row indices for training data
    trainingRowIndex
    trainingData<-r1[trainingRowIndex, ] #model training data
    testData<-r1[-trainingRowIndex, ] #test Data
    #Build the model on training data-
    lmMod<- lm(Happiness.Rank~Happiness.Score+Economy..GDP.per.Capita.+Family+Health..Life.Expectancy.+Freedom+Trust..Government.Corruption.+Generosity,data=trainingData) #Build the model
    distPred<-predict(lmMod,testData) #predict happiness rank
    print(summary(lmMod)) #model summary
    #resid(lmMod)
    actuals_preds<-data.frame(cbind(actuals=testData$Happiness.Rank,predicteds=distPred)) #make actuals
    correlation_accuracy<-cor(actuals_preds)#82.7%
    print(correlation_accuracy)
    head(actuals_preds)
    print (head(actuals_preds))
     dataanalysis()
  }
  
  #Clustering
  clustering<-function(){
    View(r1)
   
    d=r1[,3:9]
    d
    cd_norm1<-function(x){((x-min(x))/(max(x)-min(x)))}
    m_norm1<-as.data.frame(lapply(d,cd_norm1))
    
    library(class)
    k2<-kmeans(d,3)
    k2
    
    table(k2$cluster,r1$Year)
    library(ggplot2)
    ggplot(r1, aes(Happiness.Score,Economy..GDP.per.Capita.+Family+Health..Life.Expectancy.+Freedom+Trust..Government.Corruption.+Generosity, color = factor(Year))) + geom_point()
    dataanalysis()
  }
  
  exit<-function(){
    print("Do you want to exit?Y or N")
    fgh=scan(what = 'character')
    
    if(fgh=='n' || fgh=='N'){
      dataanalysis() 
    }
    if(fgh=='y' || fgh=='Y')
      
      return(0)
  }
  
  switch(t,"1"=filter12(),"2"=twitterdatamining(),"3"=fb(),"4"=knn12(),"5"=regression(),"6"=decisiontree(),"7"=clustering(),"8"=exit())
  #t=scan()
}
