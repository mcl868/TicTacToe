strategyRandom<-function(gameState){
if(evaluateGameState(gameState)==42){
  GameMatrix<-t(matrix(gameState,3,3))

  if(length(strategy(gameState))==0){
    resRanSta<-1*(gameState==0)/sum(gameState==0)
    } else {
      resRanSta<-1*(c(1:9) %in% as.numeric(unlist(strategy(gameState))))/
        length(unique(unlist(strategy(gameState))))
    }
  return(resRanSta)}
}
