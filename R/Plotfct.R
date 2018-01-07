Plotfct<-function(gameState,player1,player2){
  temp<-matrix(gameState,3,3)
  placeX<-c(1/6,1/2,5/6)
  placeY<-c(5/6,1/2,1/6)
  par(mai=c(0.3,0.3,0.3,0.3))
  plot(0,0,col=0,xlab="",ylab="",xlim=c(0,1),ylim=c(0,1),axes = FALSE,main="Tic Tac Toe")
  legend(0.1,0.1,ncol=2, inset = c(0,0.4) , cex = 1.5, bty = "n",
         legend = c(player1,player2), pch = c(4,1))

  segments(1/3,0,1/3,1);segments(2/3,0,2/3,1)
  segments(0,1/3,1,1/3);segments(0,2/3,1,2/3)
  for(jj in 1:3)for(ii in 1:3)if(!temp[jj,ii]==0){
    if(temp[jj,ii]==1)points(placeX[jj],placeY[ii],pch=4,lwd=4,cex=4)
    if(temp[jj,ii]==-1)points(placeX[jj],placeY[ii],pch=1,lwd=4,cex=4)
  }
}
