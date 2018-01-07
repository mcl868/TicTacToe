getInputPlayerName<-function(Nb){
  if(Nb %in% c(1,2)){
    if(Nb==1)Play<-"one"
    if(Nb==2)Play<-"two"
    c<-readline(prompt = paste0("Name of player ",Play,": "))
    c<-as.character(c)
    return(c)
  }
}
