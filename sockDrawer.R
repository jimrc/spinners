
sockDrawer  <- function(Factor1, nDraws, plotIt=TRUE, speed = 10, replace=TRUE){
  ## function to randomly sample "socks" from a "drawer" 
  nObjects <- length(Factor1)
  if(! is.factor(Factor1)){
    Factor1 <- as.factor(Factor1)
  }
  nCat <- length(levels(Factor1))
  randomNdx <- sample(nObjects, nDraws, replace=replace)
  if(plotIt){
    opar <- par(mar=c(4,2,1,1)+.1)
    ## shorten names of factor levels for easier printing
    #levels(Factor1) <- colnames( model.matrix( ~ Factor1 + 0))
    palette( colorspace::rainbow_hcl(nCat, c = 40, l = 60, start = 20, 
                                     end = 360*(nCat-1)/nCat))
    plot( 0:1, c(-.5,1)  , type="n",xlab="",ylab="",xaxt="n",yaxt="n", fin=c(2,2), bty="n")
    rect( xleft=0, xright=1, ytop=1,ybottom=0, col="cornsilk")
    #if(nObjects > 24){
      Ncols <- ceiling( nObjects ^ .5) +1
      Nrows <- ceiling( nObjects / Ncols)
    #}  else {
    #  Nrows <- 3; Ncols <- 8
    #}
    xmid <- (0:(nObjects-1) %% Ncols)/Ncols + 1/Ncols/2
    ymid <- (0:(nObjects-1) %/% Ncols)/(Ncols) + 1.5/Ncols/2  
    radius <- 1/Ncols/4
    showAllSocks <- function(){
      for(ndx in 1:nObjects){
        plotrix::draw.circle(r = radius, x = xmid[ndx], y=ymid[ndx], col = unclass(Factor1)[ndx],
                             border=NA)
      }
      text( x=xmid, y = ymid, Factor1)
    }
    ## draw out the samples
    xOut <- (1:nDraws )/nDraws -1/2/nDraws
    yOut <- -radius -.1
    showAllSocks()
    for(ndx in 1:nDraws){
      plotrix::draw.circle(r = radius, x = xmid[randomNdx[ndx]], y=ymid[randomNdx[ndx]], 
                         col = "cornsilk")
      plotrix::draw.circle(r = radius, x = xOut[ndx], y=yOut, col =  unclass(Factor1)[randomNdx[ndx]] )
      text(xOut[ndx], yOut, Factor1[randomNdx[ndx]])
      Sys.sleep(3/speed)
      if(replace)
        showAllSocks()
    }
  }
  par(opar)
  Factor1[randomNdx]
}

#sockDrawer(rep(LETTERS[1:3],4),3, replace=T)
#sockDrawer(rep(LETTERS[1:3],4),3, replace=F,speed=2)
