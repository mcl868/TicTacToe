BeginTTT<-function(){
  if(rbinom(1,1,0.5)==1){
    Pla<-"Player 1 turn"
  } else {
    Pla<-"Player 2 turn"
  }
  return(Pla)
}
