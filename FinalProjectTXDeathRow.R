#Descriptive Statistics 
summary(TexasExecuted)

#Creates a new column for the year
dateSplit <- TexasExecuted$Date
dateSplit <- as.Date(dateSplit)
ds <- data.frame(year = as.numeric(format (dateSplit, format = "%Y")), 
                 month = as.numeric(format(dateSplit, format = "%m")),
                 day = as.numeric(format(dateSplit, format = "%d")))
TexasExecuted$Year <- ds

#Creates A New Table
TexasExecuted$TimeHeld <- TexasExecuted$Age - as.numeric(TexasExecuted$`Age (when received)`)

#The word "love" occurred 228 times.
love <- grep("[Ll]ove", TexasExecuted$`Last Statement`)
#The word "family" occurred 149 times. 
family <- grep("[Ff]amily", TexasExecuted$`Last Statement`)
#The word "thank" occurred 125 times. 
thank <- grep("[Tt]hank", TexasExecuted$`Last Statement`)
#The word "sorry" occurred 98 times.
sorry <- grep("[Ss]orry", TexasExecuted$`Last Statement`)
#The word "god" occurred 89 times. 
god <- grep("[Gg]od", TexasExecuted$`Last Statement`)
#The word "innocent" occurred 21 times. 
innocent <- grep("[Ii]nnocent", TexasExecuted$`Last Statement`)
#95 people declined to give a statement
noStatement <- grep("[Nn]one", TexasExecuted$`Last Statement`)

#Used a library called plotly instead of ggplot
install.packages("plotly")
packageVersion("plotly")
library("plotly")
year <- c(1982:2017)
number <- each
data <- data.frame(year, number)
p <- plot_ly(data,  x = ~year, y = ~number , type = 'scatter', mode = 'lines')%>%
  layout(title = "number of the each year executed")
p


#This creates a line graph.
aj = length(which(TexasExecuted$Year == 2017))
ai = length(which(TexasExecuted$Year == 2016))
ah = length(which(TexasExecuted$Year == 2015))
ag = length(which(TexasExecuted$Year == 2014))
af = length(which(TexasExecuted$Year == 2013))
ae = length(which(TexasExecuted$Year == 2012))
ad = length(which(TexasExecuted$Year == 2011))
ac = length(which(TexasExecuted$Year == 2010))
ab = length(which(TexasExecuted$Year == 2009))
aa = length(which(TexasExecuted$Year == 2008))
z = length(which(TexasExecuted$Year == 2007))
y = length(which(TexasExecuted$Year == 2006))
x = length(which(TexasExecuted$Year == 2005))
w = length(which(TexasExecuted$Year == 2004))
v = length(which(TexasExecuted$Year == 2003))
u = length(which(TexasExecuted$Year == 2002))
t = length(which(TexasExecuted$Year == 2001))
s = length(which(TexasExecuted$Year == 2000))
r = length(which(TexasExecuted$Year == 1999))
q = length(which(TexasExecuted$Year == 1998))
p = length(which(TexasExecuted$Year == 1997))
o = length(which(TexasExecuted$Year == 1996))
n = length(which(TexasExecuted$Year == 1995))
m = length(which(TexasExecuted$Year == 1994))
l = length(which(TexasExecuted$Year == 1993))
k = length(which(TexasExecuted$Year == 1992))
j = length(which(TexasExecuted$Year == 1991))
i = length(which(TexasExecuted$Year == 1990))
h = length(which(TexasExecuted$Year == 1989))
g = length(which(TexasExecuted$Year == 1988))
f = length(which(TexasExecuted$Year == 1987))
e = length(which(TexasExecuted$Year == 1986))
d = length(which(TexasExecuted$Year == 1985))
c = length(which(TexasExecuted$Year == 1984))
b = length(which(TexasExecuted$Year == 1983))
a = length(which(TexasExecuted$Year == 1982))
each = c(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,aa,ab,ac,ad,ae,af,ag,ah,ai,aj)


#Generate TreeMap
install.packages("treemap")
library(treemap)

#Table of the number of times a County Appears
table(TexasExecuted$County) 
treemap(TexasExecuted, index = "County", vSize = "TimeHeld", title = "Counties Inmates are From", 
        fontfamily.title = "serif", fontfamily.labels = "serif")

#Word Cloud Code Starts Here
install.packages("tm", dependencies = TRUE)
install.packages("wordcloud", dependencies = TRUE)
install.packages("RColorBrewer", dependencies = TRUE)
library(tm)
library(wordcloud)
library(RColorBrewer)

#Word Cloud for 1982 - 1989
TEbyYear<- subset.data.frame(TexasExecuted,Year == 1982:1989)

docs <- Corpus(VectorSource(TEbyYear$`Last Statement`))

docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
dtm  <- TermDocumentMatrix(docs, control = list(removePunctuation = TRUE, 
                                                stopwords = stopwords("english"), 
                                                removeNumbers = TRUE, tolower = TRUE))
#dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
set.seed(1234)
wordcloud(words = d$word, 
          freq = d$freq, min.freq = 1, max.words=200, 
          random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#Word Cloud fro 1990 - 1999
TEbyYear<- subset.data.frame(TexasExecuted,Year == 1990:1999)

docs <- Corpus(VectorSource(TEbyYear$`Last Statement`))

docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
dtm  <- TermDocumentMatrix(docs, control = list(removePunctuation = TRUE, 
                                                stopwords = stopwords("english"), 
                                                removeNumbers = TRUE, tolower = TRUE))
#dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
set.seed(1234)
wordcloud(words = d$word, 
          freq = d$freq, min.freq = 1, max.words=200, 
          random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


#Word Cloud for 2000 - 2009
TEbyYear<- subset.data.frame(TexasExecuted,Year == 2000:2009)

docs <- Corpus(VectorSource(TEbyYear$`Last Statement`))

docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
dtm  <- TermDocumentMatrix(docs, control = list(removePunctuation = TRUE, 
                                                stopwords = stopwords("english"), 
                                                removeNumbers = TRUE, tolower = TRUE))
#dtm <- TermDocumentMatrix(docs)

#Word Cloud for 2010 - 2017
TEbyYear<- subset.data.frame(TexasExecuted,Year == 2010:2017)

docs <- Corpus(VectorSource(TEbyYear$`Last Statement`))

docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
dtm  <- TermDocumentMatrix(docs, control = list(removePunctuation = TRUE, 
                                                stopwords = stopwords("english"), 
                                                removeNumbers = TRUE, tolower = TRUE))
#dtm <- TermDocumentMatrix(docs)

m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
set.seed(1234)
wordcloud(words = d$word, 
          freq = d$freq, min.freq = 1, max.words=200, 
          random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#By Race(Black)
TEbyrace<- subset.data.frame(TexasExecuted,Race == "Black")

docs <- Corpus(VectorSource(TEbyrace$`Last Statement`))

docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
dtm  <- TermDocumentMatrix(docs, control = list(removePunctuation = TRUE, 
                                                stopwords = stopwords("english"), 
                                                removeNumbers = TRUE, tolower = TRUE))
#dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
set.seed(1234)
wordcloud(words = d$word, 
          freq = d$freq, min.freq = 1, max.words=200, 
          random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#By Race(White)
TEbyrace2<- subset.data.frame(TexasExecuted,Race == "White")

docs <- Corpus(VectorSource(TEbyrace2$`Last Statement`))

docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
dtm  <- TermDocumentMatrix(docs, control = list(removePunctuation = TRUE, 
                                                stopwords = stopwords("english"), 
                                                removeNumbers = TRUE, tolower = TRUE))
#dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
set.seed(1234)
wordcloud(words = d$word, 
          freq = d$freq, min.freq = 1, max.words=200, 
          random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
#by rise(Hispanic)
TEbyrace3<- subset.data.frame(TexasExecuted,Race == "Hispanic")

docs <- Corpus(VectorSource(TEbyrace3$`Last Statement`))

docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
dtm  <- TermDocumentMatrix(docs, control = list(removePunctuation = TRUE, 
                                                stopwords = stopwords("english"), 
                                                removeNumbers = TRUE, tolower = TRUE))
#dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
set.seed(1234)
wordcloud(words = d$word, 
          freq = d$freq, min.freq = 1, max.words=200, 
          random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#By Race (Other)
TEbyrace4<- subset.data.frame(TexasExecuted,Race == "Other")

docs <- Corpus(VectorSource(TEbyrace4$`Last Statement`))

docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
dtm  <- TermDocumentMatrix(docs, control = list(removePunctuation = TRUE, 
                                                stopwords = stopwords("english"), 
                                                removeNumbers = TRUE, tolower = TRUE))
#dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
set.seed(1234)
wordcloud(words = d$word, 
          freq = d$freq, min.freq = 1, max.words=200, 
          random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#Code for Word Cloud by Age Starts Here
#by age(19-29)
TEbyage<- subset.data.frame(TexasExecuted_1_,Age == 19:29)

docs <- Corpus(VectorSource(TEbyage$`Last Statement`))

docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
dtm  <- TermDocumentMatrix(docs, control = list(removePunctuation = TRUE, 
                                                stopwords = stopwords("english"), 
                                                removeNumbers = TRUE, tolower = TRUE))
#dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
set.seed(1234)
wordcloud(words = d$word, 
          freq = d$freq, min.freq = 1, max.words=200, 
          random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
#by age(30-39)
TEbyage2<- subset.data.frame(TexasExecuted_1_,Age == 30:39)

docs <- Corpus(VectorSource(TEbyage2$`Last Statement`))

docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
dtm  <- TermDocumentMatrix(docs, control = list(removePunctuation = TRUE, 
                                                stopwords = stopwords("english"), 
                                                removeNumbers = TRUE, tolower = TRUE))
#dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
set.seed(1234)
wordcloud(words = d$word, 
          freq = d$freq, min.freq = 1, max.words=200, 
          random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
#by age(40-49)
TEbyage3<- subset.data.frame(TexasExecuted_1_,Age == 40:49)

docs <- Corpus(VectorSource(TEbyage3$`Last Statement`))

docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
dtm  <- TermDocumentMatrix(docs, control = list(removePunctuation = TRUE, 
                                                stopwords = stopwords("english"), 
                                                removeNumbers = TRUE, tolower = TRUE))
#dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
set.seed(1234)
wordcloud(words = d$word, 
          freq = d$freq, min.freq = 1, max.words=200, 
          random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
#by age(50-59)
TEbyage4<- subset.data.frame(TexasExecuted_1_,Age == 50:59)

docs <- Corpus(VectorSource(TEbyage4$`Last Statement`))

docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
dtm  <- TermDocumentMatrix(docs, control = list(removePunctuation = TRUE, 
                                                stopwords = stopwords("english"), 
                                                removeNumbers = TRUE, tolower = TRUE))
#dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
set.seed(1234)
wordcloud(words = d$word, 
          freq = d$freq, min.freq = 1, max.words=200, 
          random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
#by age(60-68)
TEbyage4<- subset.data.frame(TexasExecuted_1_,Age == 60:68)

docs <- Corpus(VectorSource(TEbyage4$`Last Statement`))

docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
dtm  <- TermDocumentMatrix(docs, control = list(removePunctuation = TRUE, 
                                                stopwords = stopwords("english"), 
                                                removeNumbers = TRUE, tolower = TRUE))
#dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
set.seed(1234)
wordcloud(words = d$word, 
          freq = d$freq, min.freq = 1, max.words=200, 
          random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#Generates a Stacked Bar Chart
RaceByStatement <- c("Black", "White","Hispanic","Other")
total <- length(TexasExecuted$`Last Statement`)
total1 = total - nostatement
black = length(which(TexasExecuted$Race == "Black"&TexasExecuted1$`Last Statement` == "None"))
black1 = length(which(TexasExecuted$Race == "Black")) - black
white = length(which(TexasExecuted$Race == "White"&TexasExecuted1$`Last Statement` == "None"))
white1 = length(which(TexasExecuted$Race == "White")) - white
hispanic = length(which(TexasExecuted$Race == "Hispanic"&TexasExecuted1$`Last Statement` == "None"))
hispanic1 = length(which(TexasExecuted$Race == "Hispanic")) - hispanic
other = length(which(TexasExecuted$Race == "Other"&TexasExecuted1$`Last Statement` == "None"))
other1 = length(which(TexasExecuted$Race == "Other")) - other
SF_total = c(black1, white1, hispanic1, other1)
LA_nostatement = c(black, white, hispanic, other)
q <- plot_ly(data, x = ~RaceByStatement, y = ~SF_total, type = 'bar', name = 'Gave a Statement') %>%
  add_trace(y = ~LA_nostatement, name = 'Declined to Give a Statement') %>%
  layout(yaxis = list(title = 'number'), barmode = 'stack')
q


#Generates Box Plots
boxplot(Texas$Age, main="boxplot of age of executed offenders", xlab="Age",
        ylab = "Age of Executed Offenders", horizontal = TRUE) 
Texas$`Age (when received)` = as.numeric(Texas1$`Age (when received)`, na.rm =TRUE)
boxplot(Texas$`Age (when received)`, main="boxplot of age when convicted", xlab="Age",
        ylab = "age when convicted", horizontal = TRUE, na.rm=TRUE) 
Texas$TimeHeld = (Texas1$Age - Texas1$`Age (when received)`)
boxplot(Texas$TimeHeld, main="boxplot of time held", xlab="time held", horizontal = TRUE)

