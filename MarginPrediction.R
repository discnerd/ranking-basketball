
Predict_Margin <- function(A_unnormed, shares, team1index, team2index, base_ranking){
  library(expm)
  library(dplyr)
#  A<- A_unnormed
   n <- length(diag(A_unnormed))
   b <- rep(1,n)
  # for(i in 1:n){
  #   if(sum(A[i,])!=0){ 
  #     A[i,]=A[i,]/sum(A[i,])
  #   }
  # }
  # rank_old<- t(b)
  # 
  # e<-eigen(t(A))
  # if(sum(near((e$values),1))<2){
  #   e<-Re(eigen(t(A))$vectors[,1])
  #   base_ranking <- e/sum(e)*length(e)
  # } else{
  #   base_ranking<-t(b)%*% (A)  
  # }
  # while( norm(rank_old-base_ranking)>0.00000001){
  #   rank_old <- base_ranking
  #   base_ranking <- base_ranking %*% A
  # } 
   
  distance = 10000;
   
  for(share in shares){
    A <-A_unnormed
    A[ team1index ,team2index  ]=A[ team1index ,team2index  ]+ (1-share);
    A[ team2index ,team1index  ]=A[ team2index ,team1index  ]+share;
    A[ team1index ,team1index  ]=A[ team1index ,team1index  ]+share;
    A[ team2index ,team2index  ]=A[ team2index ,team2index  ]+ (1-share);
    
    for(i in 1:length(teams$Team)){
      if(sum(A[i,])!=0){ 
        A[i,]=A[i,]/sum(A[i,])
      }
    }
    old_rank <- t(b)
    e<-eigen(t(A))
    iter=0;
    #if(FALSE){#
      if(sum(near((e$values),1))<2){
      e<-Re(eigen(t(A))$vectors[,1])
      Rating <- e/sum(e)*length(e)
      cat("e")
    } else{
      Rating<-t(b)%*% (A)  
    }
    while(norm(old_rank-Rating)>0.0000001) {
      old_rank <- Rating
      Rating <- Rating %*% A
      iter=iter+1
    }
    cat(iter)
    if(distance > norm(Rating - base_ranking)){
      pt_diff <- which(shares == share, arr.ind = TRUE)-(length(shares)+1)/2
      distance = norm(Rating - base_ranking)
      cat( pt_diff , " ", distance, "\n")
    } else{
     # cat(which(shares == share, arr.ind = TRUE)-(length(shares)+1)/2, 
     cat(pt_diff+1," ", norm(Rating - base_ranking), "\n")
      break
    }    
    
  }
 
  return( pt_diff )

}