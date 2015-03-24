mydata<-read.csv("C:/Data/tmwildfire1.csv")
tmwildfire<-as.data.frame(mydata)
library(tm)
myCorpus <- Corpus(VectorSource(tmwildfire$TEXT))
# convert to lower case
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
# remove URLs
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
myCorpus <- tm_map(myCorpus, removeURL)
# add two extra stop words: "available" and "via"
myStopwords <- c(stopwords('english'), "via", "available","like", "near")
# remove stopwords from corpus
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)

#combine san diego
myCorpus<- tm_map(myCorpus, gsub, pattern="san diego", replacement="sandiego")
myCorpus<- tm_map(myCorpus, gsub, pattern="san", replacement="1")
myCorpus<- tm_map(myCorpus, gsub, pattern="diego", replacement="sandiego")


# strip whitespace
myCorpus <- tm_map(myCorpus,stripWhitespace)
# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)

# plain text doc
myCorpus <- tm_map(myCorpus, PlainTextDocument)
# keep a copy of corpus to use later as a dictionary for stem completion
myCorpusCopy <- myCorpus
# stem words
myCorpus <- tm_map(myCorpus, stemDocument)
# inspect documents (tweets) numbered 11 to 15
# inspect(myCorpus[11:15])
# The code below is used for to make text fit for paper width
for(i in 11:15){
cat(paste("[[",i,"]]",sep=""))
writeLines(strwrap(myCorpus[[i]],width=73))
}
##############################stemming words#############################
# Define modified stemCompletion function, the original stemCom
stemCompletion_mod <- function(x,dict=myCorpusCopy) {
  PlainTextDocument(stripWhitespace(paste(stemCompletion(unlist(strsplit(as.character(x)," ")),dictionary=dict, type="shortest"),sep="", collapse=" ")))
}
# apply the function to myCorpus
myCorpus <- lapply(myCorpus, stemCompletion_mod, myCorpusCopy)
myCorpus.dataframe <- data.frame(text=unlist(sapply(myCorpus, `[`, "content")), 
    stringsAsFactors=F)
# replace counting with county
myCorpus <- gsub(myCorpus.dataframe$text,pattern="counting", replacement="county")
myCorpus <- gsub(myCorpus,pattern="ranch", replacement="4sranch")

#convert vector to corpus
myCorpus <- Corpus(VectorSource(myCorpus))


inspect(myCorpus[11:15])
###############################word cloud##############################
# building a term-document matrix
myTdm <- TermDocumentMatrix(myCorpus)
myTdm
# inspect frequent words
findFreqTerms(myTdm, lowfreq=60)
termFrequency <- rowSums(as.matrix(myTdm))
termFrequency <- subset(termFrequency, termFrequency>=40)
library(ggplot2)
qplot(names(termFrequency), termFrequency, geom="bar", stat="identity", xlab="Terms") + coord_flip()     
library(wordcloud)
m <- as.matrix(myTdm)
# calculate the frequency of words and sort it descendingly by frequency
v = sort(rowSums(m), decreasing=TRUE)
d = data.frame(word=names(v), freq=v)
# word cloud
wordcloud(d$word, d$freq, min.freq=30,random.color=TRUE, colors=rainbow(7))










