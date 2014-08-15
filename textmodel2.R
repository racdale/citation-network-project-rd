setwd('~/Dropbox/cogsci_interdisciplinarity/citation-network-project-master/commitable')
# NB: needs processing in hbhat4000's citation-network-project-master

# clear all memory
rm(list=ls(all=TRUE))

source('load_text_materials.R')
source('cosine_similarity_matrix.R')
source('author_cosine_by_journal.R')
source('journal_cosine_by_content.R')

# convert to sparse matrix
library('Matrix')
tXd.mat = sparseMatrix(tXd$i,tXd$j,x=tXd$v,dims=c(tXd$nrow,tXd$ncol)) # removes col's with 0's... need to fix
#save(tXd.mat,tXd,file="tXd.Rdata")
load('tXd.Rdata')

# tXd.mat too big for non-sparse SVD; use irlba
library(irlba)
S <<- irlba(tXd.mat,nu=25,nv=25) # very slow beyond 30 or so

jids = as.numeric(names(sort(table(title$JID),decreasing=T)[30:70])) # get the most common journals

j_auth_cos = data.frame()

system.time(for (jid in jids) {
  print(paste('author by cosine processing: ',jid,journal[jid,]$name))
  res = author_cosine_by_journal(jid,1)
  j_auth_cos = rbind(j_auth_cos,data.frame(jid=jid,cos=res[,1],n=res[,2]))  
});
#boxplot(cos~jid,data=j_auth_cos[j_auth_cos$n>10,]) # boxplot by journal...
#summary(lm(cos~as.factor(jid)*n,data=j_auth_cos[j_auth_cos$n>10,])) # does n predict cos? weakly
#hist(j_auth_cos$cos,100)
acosagg = aggregate(j_auth_cos[j_auth_cos$n>10,]$cos,by=list(j_auth_cos[j_auth_cos$n>10,]$jid),mean) # only journals with n > 10 articles in the results
acosagg = sort(acosagg$x,index.return=T)
plot(acosagg$x,col='white',xlim=c(0,length(jids))) # plot names of journals ranked with y = cosine (higher = more consistent authorship content)
text(1:length(acosagg$ix)+1,acosagg$x,labels=journal[jids,]$name,cex=.65)

j_journ_cos = data.frame()

system.time(for (jid in jids) {
  print(paste('journal by content processing: ',jid,journal[jid,]$name))
  res = journal_cosine_by_content(jid,1)
  if (length(res)>0) {
    j_journ_cos = rbind(j_journ_cos,data.frame(jid=jid,cos=res[,1],n=res[,2]))  
  }
});

jcosagg = aggregate(j_journ_cos[j_journ_cos$n>4,]$cos,by=list(j_journ_cos[j_journ_cos$n>4,]$jid),mean) # only journals with n > 10 articles in the results
jcosagg = sort(jcosagg$x,index.return=T)
plot(jcosagg$x,col='white',xlim=c(0,length(jids))) # plot names of journals ranked with y = cosine (higher = more consistent authorship content)
text(1:length(jcosagg$ix)+1,jcosagg$x,labels=journal[jids,]$name,cex=.65)

plot(acosagg$x,jcosagg$x,col='black')
points(acosagg$x[6],jcosagg$x[6],col=rgb(0,.5,.2),lwd=10)
text(acosagg$x,jcosagg$x,labels=journal[jids,]$name,cex=.65)


j_vect_cos = data.frame()

for (jid in jids) {
  print(paste('author journal vector processing: ',jid,journal[jid,]$name))
  artids = which(title$JID==jid)
  for (j in artids) { # loop through all articles for this journal
    auths = comb$AID[comb$TID==j] # get the authors of this title
    auths = auths[author[auths,]$name!=""]
    authtitles = unique(comb$TID[comb$AID %in% auths & comb$TID!=j])
    journs = title[title$TID %in% authtitles,]$JID
    journs2 = c()
    for (auth in auths) {
      authtitles = unique(comb$TID[comb$AID ==auth & comb$TID!=j])
      journs2 = c(journs2,unique(title[title$TID %in% authtitles,]$JID))
    }        
  }      
  res = length(unique(journs2))/length(journs2)
  j_vect_cos = rbind(j_vect_cos,data.frame(jid=jid,cos=res))
}









