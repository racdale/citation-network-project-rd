journal_cosine_by_content = function(jid,validate) {
  #print(journal[jid,]$name)
  artids = which(title$JID==jid)
  cosines = data.frame()
  for (j in artids) { # loop through all articles for this journal
    # get all the authors' titles; exclusive this journal article though
    titles = title[title$JID==j & title$TID!=j,]$TID
    titles = titles[nchar(title[titles,]$abstract)>0]
    X <<- S$u[titles,] # u contains the documents (article abstracts)
    n = nrow(X) 
    if (is.null(n)) { n = 0; }
    if (n>0) {    
      cmb = expand.grid(i=1:n, j=1:n) 
      C = matrix(apply(cmb,1,cosine_similarity_matrix),n,n) # get all pairwise cosine similarity measures
      #if (validate==1) { # let's check to see if similar abstracts do look similar; write to file, inspect
      #  high_cos = which(C>.95 & C<1,arr.ind=T) # very high cosine, but not 1
      #  if (length(high_cos)>0) {
      #    write(title[titles[high_cos[1,1]],]$abstract,'validate.txt',append=T)
      #    write(title[titles[high_cos[1,2]],]$abstract,'validate.txt',append=T)
      #    write("-----------------------",'validate.txt',append=T)  
      #  }
      #}
      cosines = rbind(cosines,data.frame(m=mean(C[lower.tri(C)]),n=dim(C)[1])) # grabs +1 SD from mean...
    }
  }
  return(cosines)
}
