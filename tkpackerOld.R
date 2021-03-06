# source("diceroll.R")

require(aplpack)

plot.dice <- function(){
   refresh.code<-function(...){
     nRolls <- slider(no=1);
     max <- slider(no=2);
     speed <- slider(no=3)
     #type=  slider(obj.name="type")
     return(diceroll(nRolls = nRolls, max = max, speed = speed))
 }
 # slider(obj.name="type",obj.value="l")
  slider( refresh.code,
       sl.names=c("Number of Rolls","Max Number on Dice","Speed"),
       sl.mins=c(1, 2, 1),
       sl.maxs=c(40, 20, 40),
       sl.deltas=c(1, 1, .5),
       sl.defaults=c(10, 6, 10), title="Dice Rolls"
         )
}
plot.dice()
 
## source("spinner.R")
plot.spinner <- function(...){
   refresh.code<-function(...){
     nCat <- slider(no=1);
     nDraws <- slider(no=2);
     speed <- slider(no=3)
     #type=  slider(obj.name="type")
     return(spin(nCat = nCat, nDraws = nDraws, speed = speed, ...))
 }
 # slider(obj.name="type",obj.value="l")
  slider( refresh.code,
       sl.names=c("Number of Pie Pieces","Number of Spins","Speed"),
       sl.mins=c(2, 1, 3),
       sl.maxs=c(20, 40, 40),
       sl.deltas=c(1, 1, .5),
       sl.defaults=c(2,1,15), title="Spinner"
         )
}
plot.spinner()


# source("socks.R")
plot.drawer <- function(...){
   refresh.code<-function(...){
     sockTypes <- slider(no=1);
     sockCopies <- slider(no=2)
     nDraws <- slider(no=3);
     speed <- slider(no=4)
     replc <- slider(obj.name="replc")
     socks <- rep(LETTERS[1:sockTypes], sockCopies)
     return(sockDrawer(socks, nDraws = nDraws, replace= TRUE, speed = speed, ...))
 }
  slider(obj.name="replc",obj.value="l")
  slider( refresh.code,
       sl.names=c("Number of Colors of Socks", "Copies of each color","Number of Draws","Speed"),
       sl.mins=c(2, 1, 1, 3),
       sl.maxs=c(4, 20, 40, 40),
       sl.deltas=c(1, 1, 1, .5),
       sl.defaults=c(2, 2, 1, 15), title="Sock Drawer"
         )
}
plot.drawer()

#########################################################
## Example from
## http://www.r-bloggers.com/tcltk-gui-example-with-variable-input-by-user/
#########################################################
   
require(tcltk)
mydialog <- function(){ 
 
       xvar <- tclVar("")
       yvar <- tclVar("")
       zvar <- tclVar("")
 
       tt <- tktoplevel()
       tkwm.title(tt,"MYTEST")
       x.entry <- tkentry(tt, textvariable=xvar)
       y.entry <- tkentry(tt, textvariable=yvar)
       z.entry <- tkentry(tt, textvariable=zvar)
 
       reset <- function() {
         tclvalue(xvar)<-""
         tclvalue(yvar)<-""
         tclvalue(zvar)<-""
        }
 
       reset.but <- tkbutton(tt, text="Reset", command=reset)
 
       submit <- function() {
         x <- as.numeric(tclvalue(xvar))
         y <- as.numeric(tclvalue(yvar))
         z <- as.numeric(tclvalue(zvar))
         tkmessageBox(message=paste("x + y + z = ", x+y+z, ""))
       }
       submit.but <- tkbutton(tt, text="submit", command=submit)
 
       quit.but <- tkbutton(tt, text = "Close Session", 
           command = function() {
           q(save = "no")
           tkdestroy(tt)
           }
        )
 
       tkgrid(tklabel(tt,text="Put your variables.."),columnspan=3, pady = 10)
       tkgrid(tklabel(tt,text="x variable"), x.entry, pady= 10, padx= 10)
       tkgrid(tklabel(tt,text="y variable"), y.entry, pady= 10, padx= 10)
       tkgrid(tklabel(tt,text="z variable"), z.entry, pady= 10, padx= 10)
       tkgrid(submit.but, reset.but, quit.but, pady= 10, padx= 10)
 
    }
 
mydialog

#########################################################
##################   
#########################################################


  require(tcltk)
  source("spinner.R")
  spin.env <-  new.env(parent=.GlobalEnv)
  reSpin <- function(nCat, nDraws = 1, nReps =1, prob = rep(1/nCat, nCat),
                     names = 1:nCat, speed = 10, plotIt = TRUE){
    tmp <- matrix( as.numeric(spin(nCat, nDraws * nReps, prob, names,
                                   speed, plotIt)),
                  nReps, nDraws)
    assign("samples", tmp, envir = spin.env)
    tmp
  }

spinFrame <- function(...){
  if( !(as.character(tcl("info", "tclversion")) >= "8.5"))
    stop("Please update tcl to version 8.5 or higher")
   box <- tktoplevel()
    tkwm.title(box, "Spinner")
    tkwm.geometry(box, "+0+15")
    tkpack(f.slider <- ttkframe(box))   
   ## slider1 -- Pie Pieces
    nCat <- tclVar("")
    tkpack(fr <- ttkframe(f.slider), side ="top")
     lab <- tklabel(fr, text = "Number of Pieces", width = "20")
     sc <- tkscale(fr, from = 2, to = 10, showvalue = TRUE, resolution = 1, orient = "horiz")
     tkpack(lab, sc, side =  "left")
     tkconfigure(sc, variable= nCat)

   ## slider2 -- number of spins
    nDraws <- tclVar("")
    tkpack(fr <- ttkframe(f.slider), side ="top")
     lab <- ttklabel(fr, text = "Number of Spins", width = "20")
     sc <- tkscale(fr, from = 1, to = 20, showvalue = TRUE, resolution = 1, orient = "horiz")
     tkpack(lab, sc, side =  "left")
     tkconfigure(sc, variable= nDraws)

   ## slider3 -- speed
    speed <- tclVar("")
    tkpack(fr <- ttkframe(f.slider), side ="top")
     lab <- ttklabel(fr, text = "Speed", width = "20")
     sc <- tkscale(fr, from = 10, to = 50, showvalue = TRUE, resolution = 5, orient = "horiz") 
     tkpack(lab, sc, side =  "left")
     tkconfigure(sc, variable= speed)
   
   ## repeat B times  slider 4
   nReps <-  tclVar("")
   tkpack(fr <- ttkframe(f.slider), side ="top")
   lab <- ttklabel(fr, text = "Number of Repeats", width = "20")
   sc <- tkscale(fr, from = 5, to = 1000, showvalue = TRUE, resolution = 5, orient = "horiz") 
   tkpack(lab, sc, side =  "left")
   tkconfigure(sc, variable= nReps)
   
   ## Choose statistic
   stats <- c("mean","min","max","runLength")
   ## from twiddler
   cFrame <- ttkframe(box, relief = "flat", borderwidth = 2)
   statFunction <- tclVar()
   tclvalue(statFunction) <- stats[1]
   cLabel <- ttklabel(cFrame, text = "Summarizing Function")
   tkpack(cFrame, controlLabel, side = "left")
   tkpack(ttkcombobox(cFrame, values = stats, textvariable = statFunction),
            side='left')
 
   ### Run Button
    tkpack(r.but <- ttkframe(box), fill = "x")
    tkpack(ttkbutton(r.but, text = "Run", 
           command = function(){
              reSpin(nCat =  as.numeric(tclvalue(nCat)),
                     nDraws = as.numeric(tclvalue(nDraws)),
                     nReps =  as.numeric(tclvalue(nReps)),
                     speed =  as.numeric(tclvalue(speed)), ...)
             if(length(dev.list()) < 2){
               dev.new()
             } else {
               dev.set(3)
             }
             samplDistPlot(apply( spin.env$samples, 1,statFunction ))
          
             dev.set(2)
           }
            ), 
           side="right")

### Exit Button
    tkpack(f.but <- ttkframe(box), fill = "x")
    tkpack(ttkbutton(f.but, text = "Exit", command = function()
                    tkdestroy(box)))

 }

  ## forced assignment does work -- can build a new environment for storage.
  ## how to store each on update? Assign to a list?

# Need to store multiple runs -- as a list?
# If nDraw or nCat or probs change, start over?

## Change run to "Run Once"
## New sliders for reps
  ## what if they don't need to see the spins?  a No-Plot button?
  ## spin-chooser for function: min, median, mean, max. proportion

## eventually make sliders which input probs and adjust to add to one.
##  Plot for sample statistics:  based on hist

samplDistPlot <- function(samp, speed=10){
  xlim = range(samp, na.rm = TRUE)
  samp <-  samp[!is.na(samp)]
  sampSize <-  length(samp)
  dvc <- dev.size()         ## what if resized?
  chrSize <-  par()$cin
  nBins <- dvc[1] / chrSize[1]
  nUnique <- length(unq <- sort(unique(samp)))
  if( nUnique < nBins){
    mids = unq
    nBreaks = nUnique
    which.bin <- factor(sapply(samp, function(x) which.max( (x == unq) * 1 )))
    x <-  samp
  } else {
    nBreaks <- ceiling(nBins + 1)
    breaks <- seq(xlim[1] -.5 /nBreaks, xlim[2] + .5 / nBreaks, length = nBreaks)
    mids <-  0.5 * (breaks[-1] + breaks[-nBreaks])
    which.bin <- cut(samp, breaks)
    x <- mids[as.numeric(which.bin)]
  }
  bins <- model.matrix(~ 0 + which.bin)
  stackUp <- apply(bins, 2, cumsum)# * chrSize[2]
  y <- stackUp[cbind(1:sampSize, as.numeric(which.bin))] 
  plot(x,y, type="n",xlab="", ylab="Frequency")
  for(i in 1:sampSize){
    points(x[i],y[i], pch=16, col=4)
    Sys.sleep(100 / speed / sampSize)
  }
}

