
server <- shinyServer(function(input, output,session) {
  ( DF <- data.frame(Time=rep(0,leng),IOP_Exp=rep(0,leng), IOP_bis=rep(0,leng), stringsAsFactors = FALSE))
#( DF <- data.frame(Time=dane$V1,IOP_Exp=x1, IOP_bis=finalx[,4],Magic=rep(1,leng), stringsAsFactors = FALSE))

  values <- reactiveValues()
  values$data <-DF	
  observeEvent(input$recalc,{
    ############################################################################
    ##################RANDOM VECTOR WITH A GIVEN CORRELATION####################
    ############################################################################
    
    #https://stat.ethz.ch/pipermail/r-help/2007-April/128925.html
    
    #dane <-read.csv("C:/Users/lenovo/Desktop/praca/cm_uj/shiny/dane.csv", header=FALSE,sep=";")
    x1<-values$data[,2]
    leng<-length(x1)
    
    # x2, x3, and x4 in a matrix, these will be modified to meet the criteria
    x234 <- scale(matrix( rnorm(3*length(x1)), ncol=3 ))
    
    # put all into 1 matrix for simplicity
    x1234 <- cbind(scale(x1),x234)
    
    # find the current correlation matrix
    c1 <- var(x1234)
    
    # cholesky decomposition to get independence
    chol1 <- solve(chol(c1))
    
    newx <-  x1234 %*% chol1 
    
    # check that we have independence and x1 unchanged
    zapsmall(cor(newx))
    all.equal( x1234[,1], newx[,1] )
    
    # create new correlation structure (zeros can be replaced with other r vals)
    newc <- matrix( 
      c(1  , 0.4, 0.5, 0.2, 
        0.4, 1  , 0  , 0  ,
        0.5, 0  , 1  , 0  ,
        0.2, 0  , 0  , 1  ), ncol=4 )
    rand<-runif(1,-1,1)
    newc[1,4]<-rand
    newc[4,1]<-newc[1,4]
    while(min(eigen(newc)$values)<0){
      rand<-runif(1,-1,1)
      newc[1,4]<-rand
      newc[4,1]<-newc[1,4]}
    
    # check that it is positive definite
    #eigen(newc)
    chol2 <- chol(newc)
    finalx <- newx %*% chol2 * sd(x1) + mean(x1)
    
    ############################################################################
    ############################################################################
    ############################################################################
    values$data[,3] <- finalx[,4]     
    #values$data[,4] <- rand   
    
    ##CorrCoeff
    output$result <- renderText({round(rand,3)})
    
    
    output$hot <- renderRHandsontable({
      ## DF <- values$data
      if (!is.null(DF))
        rhandsontable( values$data, useTypes = as.logical(input$useType), stretchH = "all")
    })
    
    #values$data[,3] <- values$data[,2] +100
  })
  
  ## Handsontable
  #observe({
  #  if (!is.null(input$hot)) {
  #    values$data = hot_to_r(input$hot)
  #  } else {
  #    if (is.null(values$data))
  #      DF <- DF
  #    else
  #      DF <- values$data
  #  }
  #  values$data <- DF
 # })  
  output$hot <- renderRHandsontable({
    #values$data = hot_to_r(input$hot)
    ## DF <- values$data
    if (!is.null(DF))
      rhandsontable( values$data, useTypes = as.logical(input$useType), stretchH = "all")
  })
  
  ## Save 
  observeEvent(input$save, {
    finalDF <- isolate(values$data)
    saveRDS(finalDF, file=file.path(outdir, sprintf("%s.rds", outfilename)))
  })
  ##CorrCoeff
  output$result <- renderText({round(rand,3)})
})

## run app 
#runApp(list(ui=ui, server=server))
#function(input, output)
#  return(invisible())
shinyApp(ui=ui,server=server)

#editTable(DF)
