library(shiny)

    spin <- function(nCat, nDraws=1, prob = rep(1/nCat, nCat), names = 1:nCat,
                 speed = 10, plotIt=TRUE){
  ## function to draw randomly from a spinner
  opar <- par(mar=c(4,2,1,1)+.1)
  palette( colorspace::rainbow_hcl(nCat, c = 60, l = 80, start = 20, 
                                    end = 360*(nCat-1)/nCat))
  ## set background image
  pie(prob, labels=names, col = 1:nCat, radius = 1, clockwise = TRUE)
  ## generate all random values
  draws <- runif(nDraws) 
  ## print(draws)
  ## convert to categories
  outcomes <- cut(draws, cumsum(c(0, prob)), label=names)
  if(plotIt){
   i <-  1
    ## spinner is plotted at 36 locations
    steps <- pi/2 - 2 * pi * seq(0, 1 + draws[i] , length=36)
    for(stp in steps){
      pie(prob, labels=names, col = 1:nCat, radius = 1, clockwise = TRUE)
      points(0, 0, cex=4, pch=20)
      arrows(-cos(stp)/2, -sin(stp)/2, cos(stp)*.9, sin(stp)*.9,
             length=.3, angle = 10, lwd=4)
      Sys.sleep(.04)
    }
    mtext("O", side=1, at= -1,
          col = unclass(outcomes[1]), cex=4, line=1)
  if(nDraws > 1) {
    if(speed < 20 ){
      ## keep plotting more draws
      Sys.sleep(10/speed) ## longer pause 
      for(i in 2:nDraws){
        ## spinner is plotted at 36 locations
        steps <- pi/2 - 2 * pi * seq(0, 1 + draws[i] , length=36)
        for(stp in steps){
          pie(prob, labels=names, col = 1:nCat, radius = 1, clockwise = TRUE)
          mtext(rep("O",i-1), side=1, at= -1 + (0:(i-2)) /(nDraws -1) *2,
                col = unclass(outcomes[1:(i-1)]), cex=4, line=1)
          points(0, 0, cex=4, pch=20)
          arrows(-cos(stp)/4, -sin(stp)/4, cos(stp)*.9, sin(stp)*.9,
                 length=.3, angle = 10, lwd=4)
          Sys.sleep(.04)  ## short pause 
        }
        mtext(rep("O",i), side=1, at= -1 + (0:(i-1))/(nDraws-1) *2  ,
              col = unclass(outcomes[1:i]), cex=4, line=1)
        Sys.sleep(8/speed)
      }
    }
    ## show all draws
    ends <-  pi/2 - 2 * pi * draws
    arrows(-cos(ends)/2, -sin(ends)/2, cos(ends)*.9, sin(ends)*.9,
              length=.3, angle = 10, lwd=4)
  
    mtext(rep("O",nDraws), side=1, at= -1 + (0:(nDraws-1))/(nDraws-1) *2  ,
          col = unclass(outcomes), cex=4, line=1)
  }
 }
  par(opar)
  outcomes
}

    

# Define server logic required to plot spinner
shinyServer(function(input, output) {

   output$spinner <- reactivePlot(function() {
     plot(c(input$nCat, input$nDraws, input$speed) )
     
     spin(input$nCat, input$nDraws, prob=(1:input$nCat),
          names=LETTERS[1:input$nCat],
          speed=input$speed)
   })
   
})
