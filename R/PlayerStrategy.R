PlayerStrategy<-function(gameState,strategyRandom){
  if(PlayerConsole(gameState)=="Player 1 turn")player=1
  if(PlayerConsole(gameState)=="Player 2 turn")player=-1

  simvector<-runif(sum(strategyRandom==max(strategyRandom)))

  ResElemt<-(c(1:9)[strategyRandom==max(strategyRandom)])[simvector==max(simvector)]
  return(ResElemt)
}
