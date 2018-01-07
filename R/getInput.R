getInput<-function(inpstat){
  if(missing(inpstat)){
    n<-readline(prompt = "Select a field: ")
    n<-as.numeric(n)
  } else {
    n<-readline(prompt = paste0(inpstat,", Select a field: "))
    n<-as.numeric(n)
  }
return(n)
}
