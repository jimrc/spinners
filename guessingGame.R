
guessingGame <- function(){
  ## game to see if we can do better with blind guessing.
  #require(cairoDevice)
  #Cairo()
  palette(c("brown","blue"))
  options(locatorBell=FALSE)
  plot(0:49, rep(0:1, 25), xaxt="n",yaxt="n",, type="n", bty="n",xlab="",ylab="", main = "Guessing Game")

  ## get speed of computer
  tt <- system.time(
    for (i in 1:100) rect(12,.5,13,.55, col = "white", bord="white")
  )
  ## set timing to give about .5 sec per answer
  napTime <- .7 - tt[3] / 100

  mtext(side=3, line=.2, at=25,"Watch this random sequence")
  ## using L and R to mimic rats pressing left or right levers.
  ## Prob of L is either .33 or .66, chosen at random, R gets the remainder
  myProb <- sample(1:2)/3
   ## create demo data
  samp1 <- sample(1:2, 20, prob = myProb, replace=TRUE)

  ## loop over 20 in the demo
  for(i in 1:20){
    text(c("L","R")[samp1[i]], x=(i-1)*2.5, y=.9, cex=3, col=samp1[i])
    Sys.sleep(napTime)
    rect(-1, .7, 2.5*i+5.5, 1.1,  col = "white", bord="white")
  }
  ## draw sample of size 50 for them to guess
  samp2 <- sample(1:2, 50, prob = myProb, replace=TRUE)
  ## give instructions
  mtext(side=1, line=.2, at=25,"Click on L or R for each guess.")
  text(x=c(23, 27),y=.1, c("L","R"),col=1:2, cex=3)

  answer <- samp2*0
  ## Loop to plot guess and evaluate it.
  for(i in 1:25){  ## row 1
    answer[i] <- identify(n=1, x=c(23,27), y=c(.1,.1), labels="", pos=TRUE)$ind
    text(c("L","R")[answer[i]], x=2*i-2, y=.7, cex=3, col=answer[i])
    if(answer[i] != samp2[i]) text("X" , x=2*i-2, y=.7, cex=3, col=samp2[i])
  }
  for(i in 25+1:25){  ## row 2
    answer[i] <- identify(n=1, x=c(23,27), y=c(.1,.1), labels="", pos=TRUE)$ind
    text(c("L","R")[answer[i]], x=2*(i-26), y=.5, cex=3, col=answer[i])
    if(answer[i] != samp2[i]) text("X" , x=2*(i-26), y=.5, cex=3, col=samp2[i])
  }
  ## show results
  rect(10,-1,40,.3,   col = "white", bord="white")
  mtext(side=1, line=.2, at=25,"Click on L or R for each guess.",col="white")
  right <- sum(answer==samp2)
  percent= right*2
  text(x=25, y=.2, paste(right," of 50 right for ", percent," percent",
               sep=""),  col = "black")
}

guessingGame()
