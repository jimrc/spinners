diceroll <- function(nRolls=10, max = 6, speed = 10, plotIt=TRUE, bkg="red", pips = "white", edge=1.2){
  rollSeq <- sample(max, nRolls, replace=TRUE)
  if(plotIt){
    opar <- par(mar=rep(1.1,4))
      pipLocs <- list(one = list(theta=0, r=0),
                    two = list(theta=c(1,5) * pi/4, r = .7),
                    tre = list(theta=c(1, 0, 5)*pi/4, r= c(.8,0,.8)),
                    four = list(theta=c(1,3,5,7)*pi/4, r = .8),
                    five = list(theta=c(1,3,3,5,7)*pi/4, r = c(1,1,0,1,1)*.9),
                    six = list(theta=c(1,3,0,4,5,7)*pi/4, r = c(.83, .83,.6,.6,.83,.83)),
                    others=  list(theta=0, r=0)  )
    rotation <- runif(nRolls, 0, pi/2)
    edge <- edge * sqrt(2) 
    for(ndx in 1:nRolls){
      plot(-1:1 *2.6,-1:1 *2.6, type="n",xlab="",ylab="",xaxt="n",yaxt="n", fin=c(2,2), bty="n")
      polygon(x = edge*cos(rotation[ndx] + c(1,-1,-3,3)*pi/4),  col=bkg, 
              y = edge*sin(rotation[ndx] +c(1,-1,-3,3)*pi/4 ) * dev.size()[1]/dev.size()[2])
      if(rollSeq[ndx] > 6){
        text( 0, 0, as.character(rollSeq[ndx]), crt = rotation[ndx], col="white",cex=6 )
      } else {
        points(x= cos(pipLocs[[rollSeq[ndx]]]$theta + rotation[ndx]) * pipLocs[[rollSeq[ndx]]]$r ,
             y= sin(pipLocs[[rollSeq[ndx]]]$theta + rotation[ndx]) * pipLocs[[rollSeq[ndx]]]$r *
               dev.size()[1]/dev.size()[2], col= pips, cex=5, pch=20)
      }
      text(y=-1.9, x=-2.6 + 5/nRolls * (1:ndx), rollSeq[1:ndx], cex=3)
      Sys.sleep(2/speed)
    }
  }
  par(opar)
  rollSeq
}

## diceroll(nRo=10,max=9, speed=5)

