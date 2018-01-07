evaluateGameState<-function(gameState){
  GameMatrix<-t(matrix(gameState,3,3))

  Diag<-GameMatrix[1,1]+GameMatrix[2,2]+GameMatrix[3,3]
  OffDiag<-GameMatrix[1,3]+GameMatrix[2,2]+GameMatrix[3,1]
  ColMa<-colSums(GameMatrix)
  RowMa<-rowSums(GameMatrix)

  if((Diag==3) | (Diag==-3) | (OffDiag==3) | (OffDiag==-3) | (any(ColMa==3)) |
     (any(ColMa==-3)) | (any(RowMa==3)) | (any(RowMa==-3)) | sum(GameMatrix==0)>0){
    if((Diag==3) | (Diag==-3) | (OffDiag==3) | (OffDiag==-3) | (any(ColMa==3)) |
       (any(ColMa==-3)) | (any(RowMa==3)) | (any(RowMa==-3))){
      if(Diag==3)gameRes<-1;
      if(Diag==-3)gameRes<--1
      if(OffDiag==3)gameRes<-1;
      if(OffDiag==-3)gameRes<--1
      if(any(ColMa==3))gameRes<-1;
      if(any(ColMa==-3))gameRes<--1
      if(any(RowMa==3))gameRes<-1;
      if(any(RowMa==-3))gameRes<--1
    } else {
      gameRes<-42
    }
  } else {
    gameRes<-0
  }

  return(gameRes)
}
