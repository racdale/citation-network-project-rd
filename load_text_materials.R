# load text mining library
library(tm)

# load title data
load('extract2.RData') # needs processing in hbhat4000's citation-network-project-master

# create corpus
#corpus = Corpus(VectorSource(c(title$name,title$abstract)))
corpus = Corpus(VectorSource(title$abstract))
# convert to lowercase
corpus = tm_map(corpus, tolower)
# remove stopwords
corpus = tm_map(corpus, removeWords, stopwords("english"))
# eliminate extra whitespace
corpus = tm_map(corpus, stripWhitespace)
# eliminate punctuation
removepunct = function(x) { return(gsub("[[:punct:]]","",x)) }
corpus = tm_map(corpus, removepunct)
# eliminate numbers
removenum = function(x) { return(gsub("[0-9]","",x)) }
corpus = tm_map(corpus, removenum)

# make term-document matrix
tXd = DocumentTermMatrix(corpus) 
