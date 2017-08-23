
Predict_Margin <- function(A_unnormed, shares, team1index, team2index, base_ranking){
  library(expm)
  library(dplyr)
  #  A<- A_unnormed
  n <- length(diag(A_unnormed))
  share=shares[(length(shares)+1)/2]
  Rating = getRanking(A_unnormed, share, team1index, team2index)
  
  distance0=norm(Rating - base_ranking,"2")
  cat(share, "\t", distance0, "\n")
  share=shares[(length(shares)+1)/2-1]
  Rating = getRanking(A_unnormed, share, team1index, team2index)
  
  distance1=norm(Rating - base_ranking,"2")
  cat(share, "\t", distance1, "\n")
  
  #cat("-1:", distance1, "\t 0:", distance0, "\n")
  if(distance1>distance0){
    distance=distance0
    pt_diff <- 0
    for(share in shares[((length(shares)+1)/2+1):length(shares)]){
      Rating = getRanking(A_unnormed, share, team1index, team2index)
      if(distance > norm(Rating - base_ranking,"2")){
        pt_diff <- which(shares == share, arr.ind = TRUE)-(length(shares)+1)/2
        distance = norm(Rating - base_ranking,"2")
        cat(share, " ", pt_diff , " ", distance, "\n")
      } else{
        cat(share , " ", which(shares == share, arr.ind = TRUE)-(length(shares)+1)/2, 
            " ",  norm(Rating - base_ranking,"2"), "\n")
        break
      }
    }
  } else{
    distance=distance1
    pt_diff <- -1
    for(share in shares[((length(shares)+1)/2-2):1]){
      Rating = getRanking(A_unnormed, share, team1index, team2index)
      if(distance > norm(Rating - base_ranking,"2")){
        pt_diff <- which(shares == share, arr.ind = TRUE)-(length(shares)+1)/2
        distance = norm(Rating - base_ranking,"2")
        cat( pt_diff , " ", distance, "\n")
      } else{
        cat(share , " ", which(shares == share, arr.ind = TRUE)-(length(shares)+1)/2, 
            " ",  norm(Rating - base_ranking,"2"), "\n")
        break
      }    
      
    }
  }
  
  
  
  return( pt_diff )
  
}

getRanking <- function(A_unnormed, share, team1index, team2index){
  A <-A_unnormed
  A[ team1index ,team2index  ]=A[ team1index ,team2index  ]+ (1-share);
  A[ team2index ,team1index  ]=A[ team2index ,team1index  ]+share;
  A[ team1index ,team1index  ]=A[ team1index ,team1index  ]+share;
  A[ team2index ,team2index  ]=A[ team2index ,team2index  ]+ (1-share);
  
  n<-dim(A)[1]
  
  for(i in 1:n){
    #cat(share ,":", i, "\n")
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
    ##cat("e")
  } else{
    Rating<-t(rep(1,n))%*% (A)  
  }
  while(norm(old_rank-Rating)/n>0.00001) {
    old_rank <- Rating
    Rating <- Rating %*% A
    iter=iter+1
  }
  return(Rating)
}