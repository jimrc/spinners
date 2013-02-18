
 require(tcltk)
 require(aplpack)
 
 source("spinner.R")

 plot.spinner <- function(){
   refresh.code<-function(...){
     nCat <- slider(no=1);
     nDraws <- slider(no=2);
     speed <- slider(no=3)
     #type=  slider(obj.name="type")
     return(spin(nCat = nCat, nDraws = nDraws, speed = speed))
 }
 # slider(obj.name="type",obj.value="l")
  slider( refresh.code,
       sl.names=c("Number of Pie Pieces","Number of Spins","Speed"),
       sl.mins=c(2, 1, 3),
       sl.maxs=c(20, 40, 40),
       sl.deltas=c(1, 1, .5),
       sl.defaults=c(2,1,10)
         )
}
plot.spinner()

source("diceroll.R")
 plot.diceroll <- function(...){
   refresh.code<-function(...){
     max <- slider(no=1);
     nRolls <- slider(no=2);
     speed <- slider(no=3)
     #type=  slider(obj.name="type")
     return(diceroll(nRolls = nRolls, max = max,  speed = speed, ...))
 }
 # slider(obj.name="type",obj.value="l")
  slider( refresh.code,
       sl.names=c("Max Spots","Number of Rolls","Speed"),
       sl.mins=c(2, 1, 3),
       sl.maxs=c(20, 40, 40),
       sl.deltas=c(1, 1, .5),
       sl.defaults=c(6, 1, 10)
         )
}
plot.diceroll()

