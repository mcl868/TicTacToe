StartGame<-function(Numb="Single"){
  gameState<-c(1:9)*0
  if(tolower(Numb)=="single"){
  comp<-BeginTTT();
  pa1<-"Player 1"
  pa2<-"Player 2"
  
  if(comp=="Player 1 turn"){
    pa1<-"Player 1"
    pa2<-"Player 2"
  } else {
    pa1<-"Player 2"
    pa2<-"Player 1"
  }

  if(comp=="Player 1 turn")cat("You are Player Two")
  if(comp=="Player 2 turn")cat("You are Player One")
  if(comp=="Player 1 turn")gameState[7]<-1
  Plotfct(gameState,player1=pa1,player2=pa2)
  for(jj in 1:100){
    if(evaluateGameState(gameState)==0 |!evaluateGameState(gameState)==42){
      if(jj==100){
        if(evaluateGameState(gameState)==1)vari<-paste("Player 1 win")
        if(evaluateGameState(gameState)==-1)vari<-paste("Player 2 win")
        if(evaluateGameState(gameState)==0)vari<-paste("Draw")
        cat("Game over\n")
        message(vari);if(exists("jj"))rm("jj")}
    } else {
      if(comp==PlayerConsole(gameState))a<-PlayerStrategy(gameState,strategyRandom(gameState)) else
        a<-getInput()
      if(is.na(a) | a >9){
        cat("No possible")
      } else {
        if(gameState[a]==0){
          if(PlayerConsole(gameState)=="Player 1 turn")gameState[a]<-1 else gameState[a]<--1
          Plotfct(gameState,player1=pa1,player2=pa2)
        } else cat("pick another field")}};if(exists("a"))rm("a")
  }}
  if(tolower(Numb)=="twoplayer"){
  	while(!evaluateGameState(gameState) %in% c(-1,0,1)){
cVec<-c()
for(iii in 1:2)cVec[iii]<-getInputPlayerName(iii)
if("" %in% cVec | length(unique(cVec))==1){
  if(length(unique(cVec))==1){cat("You have enter two different names")
  } else {
    cat("You have not ented your names")
    }
} else{
  pa1<-"Player 1"
  pa2<-"Player 2"
  comp<-BeginTTT();
  if(comp=="Player 1 turn"){
    pa1<-cVec[1]
    pa2<-cVec[2]
  } else {
    pa1<-cVec[2]
    pa2<-cVec[1]
  }

  Plotfct(gameState,player1=pa1,player2=pa2)
    for(jj in 1:100){
      if(evaluateGameState(gameState)==0 |!evaluateGameState(gameState)==42){
        if(jj==100){
      if(evaluateGameState(gameState)==1)vari<-paste0("    ",pa1," wins the game.")
      if(evaluateGameState(gameState)==-1)vari<-paste0("    ",pa2," wins the game.")
      if(evaluateGameState(gameState)==0)vari<-paste("Draw")
      cat("Game over\n")
      message(vari);if(exists("jj"))rm("jj")}
        } else {
      if(PlayerConsole(gameState)=="Player 1 turn")a<-getInput(pa1)
      if(PlayerConsole(gameState)=="Player 2 turn")a<-getInput(pa2)
        if(is.na(a) | a >9){
          cat("No possible")
        } else {
          if(gameState[a]==0){
            if(PlayerConsole(gameState)=="Player 1 turn")gameState[a]<-1 else gameState[a]<--1
            Plotfct(gameState,player1=pa1,player2=pa2)
          } else cat("pick another field")}};if(exists("a"))rm("a")
  }}
}}
}
