
library(combinat)

rand_mes <- function(clusidx_lis){
  cidx1<-clusidx_lis[[1]];cidx2<-clusidx_lis[[2]]
  n_sample<-length(cidx1)
  form<-combn(1:n_sample,2)
  TN<-0;TP<-0
  for(i in 1:ncol(form)){
    pair<-form[,i]
    if(cidx1[pair[1]]==cidx1[pair[2]]){
      if(cidx2[pair[1]]==cidx2[pair[2]]){
        TP=TP+1
      }}
    if(cidx1[pair[1]]!=cidx1[pair[2]]){
      if(cidx2[pair[1]]!=cidx2[pair[2]]){
        TN=TN+1
      }}
  }
  r<-(TP+TN)/ncol(form)
  return(r)
}



      