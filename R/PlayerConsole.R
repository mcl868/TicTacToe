PlayerConsole<-function(gameState){
  if(evaluateGameState(gameState)==42){
    if(sum(gameState==1)==0){"Player 1 turn"} else {
      if(sum(gameState==1)>sum(gameState==-1)){
        "Player 2 turn"
      } else {
        "Player 1 turn"
      }}
  } else {cat("Game finish and it is:",evaluateGameState(gameState))}
}
