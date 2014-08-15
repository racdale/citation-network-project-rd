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

jids = as.numeric(names(sort(table(title$JID),decreasing=T)[30:40])) # get the most common journals

j_auth_cos = data.frame()

system.time(for (jid in jids) {
  print(paste('author by cosine processing: ',jid,journal[jid,]$name))
  res = author_cosine_by_journal(jid,1)
  j_auth_cos = rbind(j_auth_cos,data.frame(jid=jid,cos=res[,1],n=res[,2]))  
});
#boxplot(cos~jid,data=j_auth_cos[j_auth_cos$n>10,]) # boxplot by journal...
#summary(lm(cos~as.factor(jid)*n,data=j_auth_cos[j_auth_cos$n>10,])) # does n predict cos? weakly
#hist(j_auth_cos$cos,100)
jcosagg = aggregate(j_auth_cos[j_auth_cos$n>10,]$cos,by=list(j_auth_cos[j_auth_cos$n>10,]$jid),mean) # only journals with n > 10 articles in the results
jcosagg = sort(jcosagg$x,index.return=T)
plot(jcosagg$x,col='white',xlim=c(0,length(jids))) # plot names of journals ranked with y = cosine (higher = more consistent authorship content)
text(1:length(jcosagg$ix)+1,jcosagg$x,labels=journal[jids,]$name,cex=.65)

j_journ_cos = data.frame()

system.time(for (jid in jids) {
  print(paste('journal by content processing: ',jid,journal[jid,]$name))
  res = journal_cosine_by_content(jid,1)
  if (length(res)>0) {
    j_journ_cos = rbind(j_journ_cos,data.frame(jid=jid,cos=res[,1],n=res[,2]))  
  }
});

jcosagg = aggregate(j_journ_cos[j_journ_cos$n>10,]$cos,by=list(j_journ_cos[j_journ_cos$n>10,]$jid),mean) # only journals with n > 10 articles in the results
jcosagg = sort(jcosagg$x,index.return=T)
plot(jcosagg$x,col='white',xlim=c(0,length(jids))) # plot names of journals ranked with y = cosine (higher = more consistent authorship content)
text(1:length(jcosagg$ix)+1,jcosagg$x,labels=journal[jids,]$name,cex=.65)




