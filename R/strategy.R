strategy<-function(gameState){
  if(evaluateGameState(gameState)==42){
    GameMatrix<-t(matrix(gameState,3,3))
    if(PlayerConsole(gameState)=="Player 1 turn")player=1
    if(PlayerConsole(gameState)=="Player 2 turn")player=-1

    GameMatrixNum<-t(matrix(c(1:9),3,3))

    Oppent<-1*(GameMatrix==c(-1,1)[!(c(-1,1)==player)])
    Own<-1*(GameMatrix==player)

    resSta<-list()

    if((any(rowSums(Own)==2*player) & any(rowSums(GameMatrix)==2*player))|
       (any(colSums(Own)==2*player) & any(colSums(GameMatrix)==2*player))){
      if(any(rowSums(Own)==2*player) & any(rowSums(GameMatrix)==2*player))resSta$WinMove<-
          GameMatrixNum[rowSums(Own)==2,GameMatrix[rowSums(Own)==2,]==0]
      if(any(colSums(Own)==2*player) & any(colSums(GameMatrix)==2*player))resSta$WinMove<-
          GameMatrixNum[GameMatrix[,colSums(Own)==2]==0,colSums(Own)==2]
    } else {
      if(sum(gameState %in% 0)==1)resSta$PlugInelement<-c(1:9)[(gameState %in% 0)]
      if(GameMatrix[1,1]==player & GameMatrix[2,2]==0 & GameMatrix[3,3]==0)
        resSta$PlugInelement<-9
      if(GameMatrix[1,1]==0 & GameMatrix[2,2]==0 & GameMatrix[3,3]==player)
        resSta$PlugInelement<-1
      if(GameMatrix[3,1]==player & GameMatrix[2,2]==0 & GameMatrix[1,3]==0)
        resSta$PlugInelement<-3
      if(GameMatrix[3,1]==0 & GameMatrix[2,2]==0 & GameMatrix[1,3]==player)
        resSta$PlugInelement<-7


      if(Oppent[1,1]+Oppent[2,2]+Oppent[3,3]>1){
        resSta$Blockelement<-if(gameState[(1*(Oppent[1,1]==0)+5*(Oppent[2,2]==0)+9*(Oppent[3,3]==0))]==0)
          (1*(Oppent[1,1]==0)+5*(Oppent[2,2]==0)+9*(Oppent[3,3]==0))
      }
      if(Oppent[1,3]+Oppent[2,2]+Oppent[3,1]>1){
        resSta$Blockelement<-if(gameState[3*(Oppent[1,3]==0)+5*(Oppent[2,2]==0)+7*(Oppent[3,1]==0)]==0)
          3*(Oppent[1,3]==0)+5*(Oppent[2,2]==0)+7*(Oppent[3,1]==0)
      }
      if(any(colSums(Oppent)>1)){
        COLELEMNT<-GameMatrixNum[,c(1:3)[colSums(Oppent)>1]]
        resSta$BlockelementCol<-c()
        for(jj in 1:length(COLELEMNT))if(gameState[COLELEMNT[jj]]==0)resSta$BlockelementCol[jj]<-COLELEMNT[jj]
        if(!is.null(resSta$BlockelementCol))resSta$BlockelementCol<-resSta$BlockelementCol[!is.na(resSta$BlockelementCol)]
      }
      if(any(rowSums(Oppent)>1)){
        ROWELEMNT<-GameMatrixNum[c(1:3)[rowSums(Oppent)>1],]
        resSta$BlockElementRow<-c()
        for(jj in 1:length(ROWELEMNT))if(gameState[ROWELEMNT[jj]]==0)resSta$BlockElementRow[jj]<-ROWELEMNT[jj]
        if(!is.null(resSta$BlockElementRow))resSta$BlockElementRow<-resSta$BlockElementRow[!is.na(resSta$BlockElementRow)]
      }
      if(any(colSums(GameMatrix)>1)){
        GameMatrixNumTemp<-GameMatrixNum[,colSums(Own)>1]
        resSta$PutelementCol<-GameMatrixNumTemp[(GameMatrix==0)[,colSums(Own)>1]]}
      if(any(rowSums(GameMatrix)>1)){
        GameMatrixNumTemp<-GameMatrixNum[rowSums(Own)>1,]
        resSta$PutelementRow<-GameMatrixNumTemp[(GameMatrix==0)[rowSums(Own)>1,]]}
      if(Own[1,1]==0 & GameMatrix[1,1]==0){resSta$Putelement<-1}
      if(!Own[2,2]==0){resSta$Putelement<-min((c(1:9)[gameState==0]))}
    }

    return(resSta) } else {cat("Game finish and it is:",evaluateGameState(gameState))}

}
