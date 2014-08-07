author_cosine_by_journal = function(jid,validate) {
  #print(journal[jid,]$name)
  artids = which(title$JID==jid)
  cosines = data.frame()
  for (j in artids) { # loop through all articles for this journal
    #print(paste('processing',which(artids==j),'of',length(artids),'articles'))
    auths = comb$AID[comb$TID==j] # get the authors of this title
    auths = auths[author[auths,]$name!=""]        
    # get all the authors' titles; exclusive this journal article though
    authtitles = unique(comb$TID[comb$AID %in% auths & comb$TID!=j])
    authtitles = authtitles[nchar(title[authtitles,]$abstract)>0]
    if (length(auths)>1 & length(authtitles)>2) { # only with plurality of authors; plurality of papers
      X <<- S$u[authtitles,] # u contains the documents (article abstracts)
      n = nrow(X) 
      if (!is.null(n)) { # must have at least two articles to compare after removing empty abstracts
        cmb = expand.grid(i=1:n, j=1:n) 
        C = matrix(apply(cmb,1,cos.sim),n,n) # get all pairwise cosine similarity measures
        #cos.range = c(mean(C[lower.tri(C)])-sd(C[lower.tri(C)]),mean(C[lower.tri(C)])+sd(C[lower.tri(C)]))
        if (validate==1) { # let's check to see if similar abstracts do look similar; write to file, inspect
          high_cos = which(C>.95 & C<1,arr.ind=T) # very high cosine, but not 1
          if (length(high_cos)>0) {
            write(title[authtitles[high_cos[1,1]],]$abstract,'validate.txt',append=T)
            write(title[authtitles[high_cos[1,2]],]$abstract,'validate.txt',append=T)
            write("-----------------------",'validate.txt',append=T)  
          }
        }
        cosines = rbind(cosines,data.frame(m=mean(C[lower.tri(C)]),n=dim(C)[1])) # grabs +1 SD from mean...
      }
    }
  }  
  return(cosines)
}
## examples
## explore CogSci, JID = 47
# cogsci = cosine_values_authors(47)
## explore JEPLMC, JID = 4
# jeplmc = cosine_values_authors(4)
# jdat = rbind(data.frame(j="jeplmc",cs=jeplmc),data.frame(j="cogsci",cs=cogsci))
# boxplot(jdat$cs~jdat$j)
# summary(lm(jdat$cs~jdat$j))
