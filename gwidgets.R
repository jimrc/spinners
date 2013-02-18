require(gwidgetsRGtk2)

win <- gwindow("Hello World, ad nauseum", visible=TRUE)
 group <- ggroup(horizontal = FALSE, container=win)
 obj <- gbutton("Hello...",container=group, handler = function(h,...) gmessage("world"))
 obj <- glabel("Hello...", container =group,  handler = function(h,...) gmessage("world"))
obj <- gcombobox(c("Hello","world"), container=group)
obj <- gedit("Hello world", container=group)
obj <- gtext("Hello world", container=group, font.attr=list(style="bold"))


w <- gwindow("Two widgets")
g <- ggroup(container = w)
widget1 <- gbutton("Click me to update the counter", container=g, handler = function(h,...) {
  oldVal <- svalue(widget2)
  svalue(widget2) <- as.numeric(oldVal) + 1
})
widget2 <- glabel(0, container=g)


### Density plot demo

availDists <- c(Normal="rnorm", Exponential="rexp")
 availKernels <- c("gaussian", "epanechnikov", "rectangular",
                   "triangular", "biweight", "cosine", "optcosine")
updatePlot <- function(h,...) {
  x <- do.call(availDists[svalue(distribution)],list(svalue(sampleSize)))
  plot(density(x, adjust = svalue(bandwidthAdjust),
  kernel = svalue(kernel)),main="Density plot") 
  rug(x)
}
 distribution <- gradio(names(availDists), horizontal=FALSE, handler=updatePlot)
 kernel <- gcombobox(availKernels, handler=updatePlot)
 bandwidthAdjust <- gslider(from=0,to=2,by=.01, value=1, handler=updatePlot)
 sampleSize <- gradio(c(50,100,200, 300), handler = updatePlot)
 window <- gwindow("gWidgetsDensity")
 BigGroup <- ggroup(cont=window)
 group <- ggroup(horizontal=FALSE, container=BigGroup)
 tmp <- gframe("Distribution", container=group)
 add(tmp, distribution)
 tmp <- gframe("Sample size", container=group)
 add(tmp,sampleSize)
 tmp <- gframe("Kernel", container=group)
 add(tmp,kernel)
 tmp <- gframe("Bandwidth adjust", container=group)
 add(tmp,bandwidthAdjust, expand=TRUE)
 add(BigGroup, ggraphics())
#####



  win <- gwindow("gwtkdensity")
  gp <- ggroup(horizontal=FALSE, cont=win)
  tmp <- gframe("Distribution", container=gp, expand=TRUE)
  distribution <- gradio(names(availDists), horizontal=FALSE,
   cont=tmp, handler=updatePlot)
  }
  availDists <- c(Normal = "rnorm", Exponential="rexp")
  availKernels <- c("gaussian", "epanechnikov", "rectangular",
    "triangular", "biweight", "cosine", "optcosine")
  tmp <- gframe("Sample size", container=gp, expand=TRUE)
  sampleSize <- gradio(c(50,100,200, 300), cont=tmp,
     handler =updatePlot)
  tmp <- gframe("Kernel", container=gp, expand=TRUE)
     kernel <- gcombobox(availKernels, cont=tmp,handler=updatePlot)
  tmp <- gframe("Bandwidth adjust", container=gp, expand=TRUE)
  bandwidthAdjust <- gslider(from=0,to=2,by=.01, value=1,
    cont=tmp, expand=TRUE,handler=updatePlot)

##  t test form

varList <- list(type="fieldset",columns = 2,
                label = "Variable(s)",
                label.pos = "top",
                label.font = c(weight="bold"),
                children = list( list(name = "x",label = "x",type = "gedit", text = ""),
                           list(name = "y",label = "y",type = "gedit",
                                text = "",depends.on = "x",
                                depends.FUN = function(value) nchar(value) > 0,depends.signal = "addHandlerBlur"
                                )
                  )
                )
## list for alternative
 
  altList <- list(type = "fieldset",label = "Hypotheses",columns = 2,
                  children = list(
                                list(name = "mu",type = "gedit",label = "Ho: mu=",text = "0",coerce.with = as.numeric),
                    list(name = "alternative",type="gcombobox",label = "HA: ",items = c("two.sided","less","greater")
                         )
                    )
    )
## now make t-test list
      tTest <- list(type = "ggroup",horizontal = FALSE,
                   children = list(varList, altList,
                                list(type = "fieldset",label = "two sample test",columns = 2,depends.on = "y",depends.FUN = function(value) nchar(value) > 0,depends.signal = "addHandlerBlur",
                                     children = list(list(name = "paired",label = "paired samples",type = "gcombobox",items = c(FALSE, TRUE)
                                       ),
            list(name = "var.equal",label = "assume equal var",
                 type = "gcombobox",items = c(FALSE, TRUE)
                 )
                                       )
                                     ),
                     list(type = "fieldset", columns = 1,
                          children = list(
                            list(name = "conf.level",label = "confidence level",type = "gedit",text = "0.95",coerce.with = as.numeric)
                            )
                          )
                     )
)
# Code to call the layout
w <- gwindow("t.test")
g <- ggroup(horizontal = FALSE, cont = w)
fl <- gformlayout(tTest, cont = g, expand=TRUE)
bg <- ggroup(cont = g)
addSpring(bg)
b <- gbutton("run t.test", cont = bg)
addHandlerChanged(b, function(h,...) {
out <- svalue(fl)
out$x <- svalue(out$x) # turn text into numbers
if(out$y == "") {
out$y <- out$paired <- NULL
} else {
out$y <- svalue(out$y)
}
## easy, not pretty
print(do.call("t.test",out))
})

  ## one widget changes another example
w <- gwindow("Two widgets")
g <- ggroup(container = w)
 updateFn <-  function(h,...) {
   oldVal <- svalue(widget2)
   svalue(widget2) <- as.numeric(oldVal) + 1
 }
widget1 <- gbutton("Click me to update the counter", container=g,
                   handler = updateFn)
widget2 <- glabel(0, container=g)


  #### Density Example  #####

updatePlot <- function(h,...) {
  x <- do.call(availDists[svalue(distribution)],list(svalue(sampleSize)))
  plot(density(x, adjust = svalue(bandwidthAdjust),
  kernel = svalue(kernel)),main="Density plot") 
  rug(x)
}
 distribution <- gradio(names(availDists), horizontal=FALSE, handler=updatePlot)
 kernel <- gcombobox(availKernels, handler=updatePlot)
 bandwidthAdjust <- gslider(from=0,to=2,by=.01, value=1, handler=updatePlot)
 sampleSize <- gradio(c(50,100,200, 300), handler = updatePlot)
 window <- gwindow("gWidgetsDensity")
 BigGroup <- ggroup(cont=window)
 group <- ggroup(horizontal=FALSE, container=BigGroup)
 tmp <- gframe("Distribution", container=group)
 add(tmp, distribution)
 tmp <- gframe("Sample size", container=group)
 add(tmp,sampleSize)
 tmp <- gframe("Kernel", container=group)
 add(tmp,kernel)
 tmp <- gframe("Bandwidth adjust", container=group)
 add(tmp,bandwidthAdjust, expand=TRUE)
 add(BigGroup, ggraphics())
#####

## convert to spinner #######
source("shinyApp/spinner.R")
options(guiToolkit="RGtk2")
require(gWidgets)

current.dev <- dev.cur()
updatePlot1 <- function(h,...) {
  dev.set(current.dev +1)
  nCat <- as.numeric(svalue(nCateg))
  dd <- spin(nCat = nCat, nDraws=svalue(nSpins),
                     prob = rep(1,nCat)/nCat,  ## needs to be specified
                     speed= svalue(speed), plotIt=TRUE)
  svalue(draws) <- as.character(dd)
}
 nCateg <- gslider(from=2, to=8, by=1, value=3, handler=updatePlot1)
 nSpins <- gslider(from=1, to = 20, by=1, value = 2, handler=updatePlot1)
 speed <- gslider(from=1, to = 30, by=1, value = 8, handler=updatePlot1)
 window <- gwindow("Spinner")
 BigGroup <- ggroup(cont=window)
 inputgroup <- ggroup(horizontal=FALSE, container=BigGroup)
 tmp <- gframe("Number of Pie Slices", container=inputgroup)
 add(tmp, nCateg, expand=TRUE)
 tmp <- gframe("Number of Spins", container=inputgroup)
 add(tmp, nSpins, expand=TRUE)
 tmp <- gframe("Speed", container=inputgroup)
 add(tmp, speed, expand=TRUE)
 plotgroup <- ggroup(horizontal=FALSE, container=BigGroup)
  add(plotgroup, ggraphics(width=300, height=300))
#  outputgroup <- gframe(container=plotgroup)
  draws <- gedit("output", handler =updatePlot1)
 tmp <- gframe("Table of Draws", container=inputgroup)
 add(tmp, draws)

 add(plotgroup, ggraphics(width=500, height = 200))




require(tcltk)
library(tkrplot)

source("spinner.R")
current.dev <- dev.cur()

Myhscale <- 1.5    # Horizontal scaling
Myvscale <- 1.5    # Vertical scaling

plotFunction <- function()
{
    params <- par(bg="white")
   sp <- spin(2,2)
    par(params)
    print(sp)
}
tt <- tktoplevel()
tkwm.title(tt,"Spinner")
img <- tkrplot(tt,fun=plotFunction,hscale=Myhscale,vscale=Myvscale)
tkgrid(img)
## spinner didn't go round
## mtext failed.  Font too big?

## try aplpack
require(aplpack)

  ## 2 examples from slider
 print.of.p.and.q<-function(){
 refresh.code<-function(...){
   p.old <- slider(obj.name="p.old")
   p <- slider(no=1);
   if(abs(p-p.old)>0.001) {slider(set.no.value=c(2,1-p))}
   q <- slider(no=2);
   if(abs(q-(1-p))>0.001) {slider(set.no.value=c(1,1-q))}
   slider(obj.name="p.old",obj.value=p)
   cat("p=",p,"q=",1-p,"\n")
 }
 slider(refresh.code,sl.names=c("value of p","value of q"),
       sl.mins=c(0,0),sl.maxs=c(1,1),sl.deltas=c(.01,.01),sl.defaults=c(.2,.8))
 slider(obj.name="p.old",obj.value=slider(no=1))
}
print.of.p.and.q()


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
