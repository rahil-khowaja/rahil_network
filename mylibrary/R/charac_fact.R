# Converting character variables into factors
charac_factor<-function(Data){
  num_columns<-sapply(Data, is.character)
  Data[num_columns]<-lapply(Data[num_columns], as.factor)
  return(Data)
}