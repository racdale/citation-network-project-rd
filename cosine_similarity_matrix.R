cosine_similarity_matrix = function(ix) {
  A = X[ix[1],]
  B = X[ix[2],]
  return(sum(A*B)/sqrt(sum(A^2)*sum(B^2)))
}   
# beautiful code to build dissimilarity matrix, using cos.sim above:
# http://stats.stackexchange.com/questions/31565/is-there-an-r-function-that-will-compute-the-cosine-dissimilarity-matrix