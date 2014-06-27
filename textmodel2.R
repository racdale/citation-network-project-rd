setwd('~/Dropbox/cogsci_interdisciplinarity/citation-network-project-master')
# NB: needs processing in hbhat4000's citation-network-project-master

cos.sim = function(ix) {
    A = X[ix[1],]
    B = X[ix[2],]
    return(sum(A*B)/sqrt(sum(A^2)*sum(B^2)))
}   
# beautiful code to build dissimilarity matrix, using cos.sim above:
# http://stats.stackexchange.com/questions/31565/is-there-an-r-function-that-will-compute-the-cosine-dissimilarity-matrix

# clear all memory
rm(list=ls(all=TRUE))

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

# convert to sparse matrix
library('Matrix')
tXd.mat = sparseMatrix(tXd$i,tXd$j,x=tXd$v) # removes col's with 0's... need to fix
save(tXd.mat,tXd,file="tXd.Rdata")
#load('tXd.Rdata')

# tXd.mat too big for non-sparse SVD; use irlba
S = irlba(tXd.mat,nu=25,nv=25) # very slow beyond 30 or so

getjournalcosines = function(jid) {
    print(journal[jid,]$name)
    articles = which(title$JID==jid)
    artids = title$TID[articles]
    cosines = c()
    for (j in artids) { # loop through all articles for this journal
        print(paste('processing',which(artids==j),'of',length(artids),'articles'))
        auths = comb$AID[comb$TID==j] # get the authors of this title
        auths = auths[author[auths,]$name!=""]        
        if (length(auths)>1) { # only with plurality of authors
          authtitles = unique(comb$TID[comb$AID %in% auths]) # get all the authors' titles
          X <<- S$v[authtitles,]
          n = nrow(X) 
          cmb = expand.grid(i=1:n, j=1:n) 
          C = matrix(apply(cmb,1,cos.sim),n,n) # get all pairwise cosine similarity measures
          cos.range = c(mean(C[lower.tri(C)])-sd(C[lower.tri(C)]),mean(C[lower.tri(C)])+sd(C[lower.tri(C)]))
          cosines = c(cosines,max(cos.range)) # grabs +1 SD from mean...
        }
    }  
    return(cosines)
}

# explore CogSci, JID = 47
cogsci = getjournalcosines(47)
# explore JEPLMC, JID = 4
jeplmc = getjournalcosines(4)
jeplmc = jeplmc[!is.na(jeplmc)]
jdat = rbind(data.frame(j="jeplmc",cs=jeplmc),data.frame(j="cogsci",cs=cogsci))
boxplot(jdat$cs~jdat$j)


