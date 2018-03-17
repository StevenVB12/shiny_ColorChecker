
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(raster)
library(imager)
library(sp)

shinyServer(function(input, output) {
  
  # load files
  myimage <- reactive({
    
    if (is.null(input$file) & input$action2 != 0){
      
      im <- imager::load.image('butterfly_example.jpg')

      return(im)
    }
    
    if (!is.null(input$file)){
      
      inFile <- input$file
    
      im <- imager::load.image(inFile$datapath)
      
      return(im)
    }
    

  })

  # fill data.frame with clicked corner points
  xyT <- reactiveValues(df = NULL)

  observeEvent(input$plot_click,{
    
    if(is.null(xyT$df)){
      x <- input$plot_click$x
      y <- input$plot_click$y
      
      xyT$df <- rbind(xyT$df, c(x,y))
      colnames(xyT$df) <- c('x','y')
    }
    else if(nrow(xyT$df) < 4){
      x <- input$plot_click$x
      y <- input$plot_click$y
      
      xyT$df <- rbind(xyT$df, c(x,y))
      colnames(xyT$df) <- c('x','y')
    }
    })
  
  output$plot_clickedpoints <- renderTable({
    # print(xyT$df)
    return(xyT$df)
  })
  
  
  # plot points and polygons
  output$OriginalImage1 <- renderPlot({
    
    if (!is.null(input$file) | input$action2 != 0){
      plot(myimage())
      
      if(!is.null(xyT$df)){
        if(nrow(xyT$df) == 4){
          
          polygon(xyT$df[,1], xyT$df[,2], border = "green")
          

          points(mydataProc()[[1]], pch=20, col = 'green')
          points(mydataProc()[[2]], pch=20, col = 'red')
          points(mydataProc()[[3]], pch=20, col = 'red')
          
          points(mydataProc()[[4]], pch=20, col = 'green')
          points(mydataProc()[[5]], pch=20, col = 'red')
          points(mydataProc()[[6]], pch=20, col = 'red')
          
          points(mydataProc()[[7]], pch=20, col = 'green')
          points(mydataProc()[[8]], pch=20, col = 'red')
          points(mydataProc()[[9]], pch=20, col = 'red')
          
          points(mydataProc()[[10]], pch=20, col = 'green')
          points(mydataProc()[[11]], pch=20, col = 'red')
          points(mydataProc()[[12]], pch=20, col = 'red')
          
          
          text(mydataProc2()[[1]][,1:2], label=mydataProc2()[[1]][,3], col = 'green')
          
          for(e in 1:nrow(mydataProc2()[[2]]))
          polygon(c(mydataProc2()[[2]]$x1[e],mydataProc2()[[2]]$x2[e],mydataProc2()[[2]]$x4[e],mydataProc2()[[2]]$x3[e]),
                  c(mydataProc2()[[2]]$y1[e],mydataProc2()[[2]]$y2[e],mydataProc2()[[2]]$y4[e],mydataProc2()[[2]]$y3[e]), border = 'red')
          
        }
      }
      
    }
    else{return()}
  })
  
  
  # plot calibrated image
  output$OriginalImage2 <- renderPlot({
    
    if (!is.null(mydataProc3())){
      plot(mydataProc3())
    }
    else{return()}
  })
          
  
  ###
  
  # Calculate outer points
  mydataProc <- reactive({
    
    patchSize <- input$decimal
    prop <- 1 - patchSize
    
    if (!is.null(input$file) | input$action2 != 0){
      if(!is.null(xyT$df)){
        if(nrow(xyT$df) == 4){
            
        xyDF <- as.data.frame(xyT$df)
        
        line1 <- xyDF[1:2,]
        line2 <- xyDF[2:3,]
        line3 <- xyDF[3:4,]
        line4 <- xyDF[c(4,1),]
        
        xdiff <- (line1$x[2]-line1$x[1])/6
        ydiff <- (line1$y[2]-line1$y[1])/6
        
        xmin <- line1$x[1]
        ymin <- line1$y[1]
        
        xySubA <- list(x = c((xmin+xmin+xdiff)/2,(xmin+xdiff+xmin+xdiff*2)/2,(xmin+xdiff*2+xmin+xdiff*3)/2,(xmin+xdiff*3+xmin+xdiff*4)/2,(xmin+xdiff*4+xmin+xdiff*5)/2,(xmin+xdiff*5+xmin+xdiff*6)/2),
                       y = c((ymin+ymin+ydiff)/2,(ymin+ydiff+ymin+ydiff*2)/2,(ymin+ydiff*2+ymin+ydiff*3)/2,(ymin+ydiff*3+ymin+ydiff*4)/2,(ymin+ydiff*4+ymin+ydiff*5)/2,(ymin+ydiff*5+ymin+ydiff*6)/2))
        
        xySubDF_1A <- as.data.frame(xySubA)
        
        xySubBa <- list(x = c(xmin+xdiff-(xdiff/2)*prop,xmin+xdiff*2-(xdiff/2)*prop,xmin+xdiff*3-(xdiff/2)*prop,xmin+xdiff*4-(xdiff/2)*prop,xmin+xdiff*5-(xdiff/2)*prop,xmin+xdiff*6-(xdiff/2)*prop),
                        y = c(ymin+ydiff-(ydiff/2)*prop,ymin+ydiff*2-(ydiff/2)*prop,ymin+ydiff*3-(ydiff/2)*prop,ymin+ydiff*4-(ydiff/2)*prop,ymin+ydiff*5-(ydiff/2)*prop,ymin+ydiff*6-(ydiff/2)*prop))
        xySubBb <- list(x = c(xmin+(xdiff/2)*prop,xmin+xdiff+(xdiff/2)*prop,xmin+xdiff*2+(xdiff/2)*prop,xmin+xdiff*3+(xdiff/2)*prop,xmin+xdiff*4+(xdiff/2)*prop,xmin+xdiff*5+(xdiff/2)*prop),
                        y = c(ymin+(ydiff/2)*prop,ymin+ydiff+(ydiff/2)*prop,ymin+ydiff*2+(ydiff/2)*prop,ymin+ydiff*3+(ydiff/2)*prop,ymin+ydiff*4+(ydiff/2)*prop,ymin+ydiff*5+(ydiff/2)*prop))
        
        xySubDF_1Ba <- as.data.frame(xySubBa)
        xySubDF_1Bb <- as.data.frame(xySubBb)
        
        
        xdiff <- (line3$x[2]-line3$x[1])/6
        ydiff <- (line3$y[2]-line3$y[1])/6
        
        xmin <- line3$x[1]
        ymin <- line3$y[1]
        
        xySubA <- list(x = c((xmin+xmin+xdiff)/2,(xmin+xdiff+xmin+xdiff*2)/2,(xmin+xdiff*2+xmin+xdiff*3)/2,(xmin+xdiff*3+xmin+xdiff*4)/2,(xmin+xdiff*4+xmin+xdiff*5)/2,(xmin+xdiff*5+xmin+xdiff*6)/2),y = c((ymin+ymin+ydiff)/2,(ymin+ydiff+ymin+ydiff*2)/2,(ymin+ydiff*2+ymin+ydiff*3)/2,(ymin+ydiff*3+ymin+ydiff*4)/2,(ymin+ydiff*4+ymin+ydiff*5)/2,(ymin+ydiff*5+ymin+ydiff*6)/2))
        
        xySubDF_3A <- as.data.frame(xySubA)
        
        xySubBa <- list(x = c(xmin+xdiff-(xdiff/2)*prop,xmin+xdiff*2-(xdiff/2)*prop,xmin+xdiff*3-(xdiff/2)*prop,xmin+xdiff*4-(xdiff/2)*prop,xmin+xdiff*5-(xdiff/2)*prop,xmin+xdiff*6-(xdiff/2)*prop),
                        y = c(ymin+ydiff-(ydiff/2)*prop,ymin+ydiff*2-(ydiff/2)*prop,ymin+ydiff*3-(ydiff/2)*prop,ymin+ydiff*4-(ydiff/2)*prop,ymin+ydiff*5-(ydiff/2)*prop,ymin+ydiff*6-(ydiff/2)*prop))
        xySubBb <- list(x = c(xmin+(xdiff/2)*prop,xmin+xdiff+(xdiff/2)*prop,xmin+xdiff*2+(xdiff/2)*prop,xmin+xdiff*3+(xdiff/2)*prop,xmin+xdiff*4+(xdiff/2)*prop,xmin+xdiff*5+(xdiff/2)*prop),
                        y = c(ymin+(ydiff/2)*prop,ymin+ydiff+(ydiff/2)*prop,ymin+ydiff*2+(ydiff/2)*prop,ymin+ydiff*3+(ydiff/2)*prop,ymin+ydiff*4+(ydiff/2)*prop,ymin+ydiff*5+(ydiff/2)*prop))
        
        xySubDF_3Ba <- as.data.frame(xySubBa)
        xySubDF_3Bb <- as.data.frame(xySubBb)
        
        xdiff <- (line2$x[2]-line2$x[1])/4
        ydiff <- (line2$y[2]-line2$y[1])/4
        
        xmin <- line2$x[1]
        ymin <- line2$y[1]
        
        xySubA <- list(x = c((xmin+xmin+xdiff)/2,(xmin+xdiff+xmin+xdiff*2)/2,(xmin+xdiff*2+xmin+xdiff*3)/2,(xmin+xdiff*3+xmin+xdiff*4)/2),y = c((ymin+ymin+ydiff)/2,(ymin+ydiff+ymin+ydiff*2)/2,(ymin+ydiff*2+ymin+ydiff*3)/2,(ymin+ydiff*3+ymin+ydiff*4)/2))
        
        xySubDF_2A <- as.data.frame(xySubA)
        
        xySubBa <- list(x = c(xmin+xdiff-(xdiff/2)*prop,xmin+xdiff*2-(xdiff/2)*prop,xmin+xdiff*3-(xdiff/2)*prop,xmin+xdiff*4-(xdiff/2)*prop),
                        y = c(ymin+ydiff-(ydiff/2)*prop,ymin+ydiff*2-(ydiff/2)*prop,ymin+ydiff*3-(ydiff/2)*prop,ymin+ydiff*4-(ydiff/2)*prop))
        xySubBb <- list(x = c(xmin+(xdiff/2)*prop,xmin+xdiff+(xdiff/2)*prop,xmin+xdiff*2+(xdiff/2)*prop,xmin+xdiff*3+(xdiff/2)*prop),
                        y = c(ymin+(ydiff/2)*prop,ymin+ydiff+(ydiff/2)*prop,ymin+ydiff*2+(ydiff/2)*prop,ymin+ydiff*3+(ydiff/2)*prop))
        
        xySubDF_2Ba <- as.data.frame(xySubBa)
        xySubDF_2Bb <- as.data.frame(xySubBb)
        
        xdiff <- (line4$x[2]-line4$x[1])/4
        ydiff <- (line4$y[2]-line4$y[1])/4
        
        xmin <- line4$x[1]
        ymin <- line4$y[1]
        
        xySubA <- list(x = c((xmin+xmin+xdiff)/2,(xmin+xdiff+xmin+xdiff*2)/2,(xmin+xdiff*2+xmin+xdiff*3)/2,(xmin+xdiff*3+xmin+xdiff*4)/2),
                       y = c((ymin+ymin+ydiff)/2,(ymin+ydiff+ymin+ydiff*2)/2,(ymin+ydiff*2+ymin+ydiff*3)/2,(ymin+ydiff*3+ymin+ydiff*4)/2))
        
        xySubDF_4A <- as.data.frame(xySubA)
        
        xySubBa <- list(x = c(xmin+xdiff-(xdiff/2)*prop,xmin+xdiff*2-(xdiff/2)*prop,xmin+xdiff*3-(xdiff/2)*prop,xmin+xdiff*4-(xdiff/2)*prop),
                        y = c(ymin+ydiff-(ydiff/2)*prop,ymin+ydiff*2-(ydiff/2)*prop,ymin+ydiff*3-(ydiff/2)*prop,ymin+ydiff*4-(ydiff/2)*prop))
        xySubBb <- list(x = c(xmin+(xdiff/2)*prop,xmin+xdiff+(xdiff/2)*prop,xmin+xdiff*2+(xdiff/2)*prop,xmin+xdiff*3+(xdiff/2)*prop),
                        y = c(ymin+(ydiff/2)*prop,ymin+ydiff+(ydiff/2)*prop,ymin+ydiff*2+(ydiff/2)*prop,ymin+ydiff*3+(ydiff/2)*prop))
        
        xySubDF_4Ba <- as.data.frame(xySubBa)
        xySubDF_4Bb <- as.data.frame(xySubBb)
        
        return(list(xySubDF_1A,xySubDF_1Ba,xySubDF_1Bb,
                    xySubDF_2A,xySubDF_2Ba,xySubDF_2Bb,
                    xySubDF_3A,xySubDF_3Ba,xySubDF_3Bb,
                    xySubDF_4A,xySubDF_4Ba,xySubDF_4Bb))
        }
      }
    }
  })
  
  # Calculate label positions
  mydataProc2 <- reactive({
    
    patchSize <- patchSize <- input$decimal
    prop <- 1 - patchSize
    
    xySubDF_1A <- mydataProc()[[1]]
    xySubDF_1Ba <- mydataProc()[[2]]
    xySubDF_1Bb <- mydataProc()[[3]]
    xySubDF_2A <- mydataProc()[[4]]
    xySubDF_2Ba <- mydataProc()[[5]]
    xySubDF_2Bb <- mydataProc()[[6]]
    xySubDF_3A <- mydataProc()[[7]]
    xySubDF_3Ba <- mydataProc()[[8]]
    xySubDF_3Bb <- mydataProc()[[9]]
    xySubDF_4A <- mydataProc()[[10]]
    xySubDF_4Ba <- mydataProc()[[11]]
    xySubDF_4Bb <- mydataProc()[[12]]
    
    if (!is.null(input$file) | input$action2 != 0){
      if(!is.null(xyT$df)){
        if(nrow(xyT$df) == 4){
  
          labels <- list(c(1,7,13,19),
                         c(2,8,14,20),
                         c(3,9,15,21),
                         c(4,10,16,22),
                         c(5,11,17,23),
                         c(6,12,18,24))
          
          xyTot <- c()
          xyMid <- c()
 
          for(e in 1:nrow(xySubDF_1A)){
            
            xyLine1 <- xySubDF_1A[e,]
            xyLine2 <- xySubDF_3A[6:1,][e,]
            
            xdiff <- (xyLine2$x-xyLine1$x)/4
            ydiff <- (xyLine2$y-xyLine1$y)/4
            
            xmin <- xyLine1$x
            ymin <- xyLine1$y
            
            xySub <- list(x = c((xmin+xmin+xdiff)/2,(xmin+xdiff+xmin+xdiff*2)/2,(xmin+xdiff*2+xmin+xdiff*3)/2,(xmin+xdiff*3+xmin+xdiff*4)/2),
                          y = c((ymin+ymin+ydiff)/2,(ymin+ydiff+ymin+ydiff*2)/2,(ymin+ydiff*2+ymin+ydiff*3)/2,(ymin+ydiff*3+ymin+ydiff*4)/2))
            
            xySubDF <- as.data.frame(xySub)
            
            # text(xySubDF, label=labels[[e]], col = 'green')
            
            xySubDFLabel <- cbind(xySubDF, label=labels[[e]])
            
            xyMid <- rbind(xyMid, xySubDFLabel)
            
            
            xyLine1a <- xySubDF_1Ba[e,]
            xyLine1b <- xySubDF_1Bb[e,]
            
            xyLine3a <- xySubDF_3Bb[6:1,][e,]
            xyLine3b <- xySubDF_3Ba[6:1,][e,]
            
            xdiffa <- (xyLine3a$x-xyLine1a$x)/4
            ydiffa <- (xyLine3a$y-xyLine1a$y)/4
            
            xdiffb <- (xyLine3b$x-xyLine1b$x)/4
            ydiffb <- (xyLine3b$y-xyLine1b$y)/4
            
            xmina <- xyLine1a$x
            ymina <- xyLine1a$y
            
            xminb <- xyLine1b$x
            yminb <- xyLine1b$y
            
            
            xySubAa <- list(x = c(xmina+xdiffa-(xdiffa/2)*prop,xmina+xdiffa*2-(xdiffa/2)*prop,xmina+xdiffa*3-(xdiffa/2)*prop,xmina+xdiffa*4-(xdiffa/2)*prop),
                            y = c(ymina+ydiffa-(ydiffa/2)*prop,ymina+ydiffa*2-(ydiffa/2)*prop,ymina+ydiffa*3-(ydiffa/2)*prop,ymina+ydiffa*4-(ydiffa/2)*prop))
            xySubAb <- list(x = c(xmina+(xdiffa/2)*prop,xmina+xdiffa+(xdiffa/2)*prop,xmina+xdiffa*2+(xdiffa/2)*prop,xmina+xdiffa*3+(xdiffa/2)*prop),
                            y = c(ymina+(ydiffa/2)*prop,ymina+ydiffa+(ydiffa/2)*prop,ymina+ydiffa*2+(ydiffa/2)*prop,ymina+ydiffa*3+(ydiffa/2)*prop))
            
            xySubBa <- list(x = c(xminb+xdiffb-(xdiffb/2)*prop,xminb+xdiffb*2-(xdiffb/2)*prop,xminb+xdiffb*3-(xdiffb/2)*prop,xminb+xdiffb*4-(xdiffb/2)*prop),
                            y = c(yminb+ydiffb-(ydiffb/2)*prop,yminb+ydiffb*2-(ydiffb/2)*prop,yminb+ydiffb*3-(ydiffb/2)*prop,yminb+ydiffb*4-(ydiffb/2)*prop))
            xySubBb <- list(x = c(xminb+(xdiffb/2)*prop,xminb+xdiffb+(xdiffb/2)*prop,xminb+xdiffb*2+(xdiffb/2)*prop,xminb+xdiffb*3+(xdiffb/2)*prop),
                            y = c(yminb+(ydiffb/2)*prop,yminb+ydiffb+(ydiffb/2)*prop,yminb+ydiffb*2+(ydiffb/2)*prop,yminb+ydiffb*3+(ydiffb/2)*prop))
            
            xySubAaDF <- as.data.frame(xySubAa)
            xySubAbDF <- as.data.frame(xySubAb)
            xySubBaDF <- as.data.frame(xySubBa)
            xySubBbDF <- as.data.frame(xySubBb)
            
            xySubRow <- cbind(xySubAaDF,xySubAbDF,xySubBaDF,xySubBbDF, label=labels[[e]])
            colnames(xySubRow) <- c('x1','y1','x2','y2','x3','y3','x4','y4','label')
            
            xyTot <- rbind(xyTot, xySubRow)
            
          }
          # print(xyMid)
          return(list(xyMid,xyTot))
        }
      }
    }
  })
  
  # Calculate RGB and model
  mydataProc3 <- reactive({
    
    if ((!is.null(input$file) | input$action2 != 0) & input$action1 != 0){
      if(!is.null(xyT$df)){
        if(nrow(xyT$df) == 4){
          
          withProgress(message = 'Calibrating image...................', value = 0,  {
          
          im <- myimage()
          xyTot <- mydataProc2()[[2]]
          
          mR <- raster::as.matrix(R(im))*255
          mG <- raster::as.matrix(G(im))*255
          mB <- raster::as.matrix(B(im))*255
          
          rR <- raster::raster(mR)
          rG <- raster::raster(mG)
          rB <- raster::raster(mB)
          
          extent(rR) <- c(0, dim(im)[2], 0, dim(im)[1])
          extent(rG) <- c(0, dim(im)[2], 0, dim(im)[1])
          extent(rB) <- c(0, dim(im)[2], 0, dim(im)[1])
          
          rR <- flip(t(rR),'y')
          rG <- flip(t(rG),'y')
          rB <- flip(t(rB),'y')
          
          xyTot$imR <- NA
          xyTot$imG <- NA
          xyTot$imB <- NA
          
          xyTot <- xyTot[order(xyTot$label),]
          
          n <- 27
          i <- 0
          
          for(e in 1:nrow(xyTot)){
            
            print(paste('Calculating observed RGB values for patch', e, sep = ' '))
            polygon(c(xyTot$x1[e],xyTot$x2[e],xyTot$x4[e],xyTot$x3[e]),
                    c(xyTot$y1[e],xyTot$y2[e],xyTot$y4[e],xyTot$y3[e]), border = 'red')
            
            outline <- rbind(c(xyTot$x1[e],xyTot$y1[e]),
                             c(xyTot$x2[e],xyTot$y2[e]),
                             c(xyTot$x4[e],xyTot$y4[e]),
                             c(xyTot$x3[e],xyTot$y3[e]))
            
            poly <- sp::Polygons(list(sp::Polygon(outline)),paste("r"))
            polyList  <- c(poly)
            polyNames <- c(paste("r"))
            sr <- sp::SpatialPolygons(polyList)
            srdf <- sp::SpatialPolygonsDataFrame(sr, data.frame(1:length(polyNames), row.names=polyNames))
            
            extrR <- raster::extract(rR, srdf)
            extrG <- raster::extract(rG, srdf)
            extrB <- raster::extract(rB, srdf)
            
            xyTot$imR[e] <- mean(extrR[[1]])
            xyTot$imG[e] <- mean(extrG[[1]])
            xyTot$imB[e] <- mean(extrB[[1]])
            
            i <- i + 1
            incProgress(1/n, detail = paste("Calculating observed RGB values for patch ", i))
          }
          
          # Colorimetric values for ColorCheker targets
          l1 <- c(1, 115, 82, 68)
          l2 <- c(2, 194, 150, 130)
          l3 <- c(3, 98, 122, 157)
          l4 <- c(4, 87, 108, 67)
          l5 <- c(5, 133, 128, 177)
          l6 <- c(6, 103, 189, 170)
          l7 <- c(7, 214, 126, 44)
          l8 <- c(8, 80, 91, 166)
          l9 <- c(9, 193, 90, 99)
          l10 <- c(10, 94, 60, 108)
          l11 <- c(11, 157, 188, 64)
          l12 <- c(12, 224, 163, 46)
          l13 <- c(13, 56, 61, 150)
          l14 <- c(14, 70, 148, 73)
          l15 <- c(15, 175, 54, 60)
          l16 <- c(16, 231, 199, 31)
          l17 <- c(17, 187, 86, 149)
          l18 <- c(18, 8, 133, 161)
          l19 <- c(19, 243, 243, 242)
          l20 <- c(20, 200, 200, 200)
          l21 <- c(21, 160, 160, 160)
          l22 <- c(22, 122, 122, 121)
          l23 <- c(23, 85, 85, 85)
          l24 <- c(24, 52, 52, 52)
          
          ColorCheckerRGB <- as.data.frame(rbind(l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l14,l15,l16,l17,l18,l19,l20,l21,l22,l23,l24))
          colnames(ColorCheckerRGB) <- c('label', 'sR', 'sG', 'sB')
          
          dat <- merge(xyTot, ColorCheckerRGB, by = 'label')
          
          
          print('Calculating polynomial regression...')
          
          sR <- dat$sR
          sG <- dat$sG
          sB <- dat$sB
          
          imR <- dat$imR
          imG <- dat$imG
          imB <- dat$imB
          
          modelR <- lm(sR ~ imR + imG + imB + imR^2 + imG^2 + imB^2)
          
          i <- i + 1
          incProgress(1/n, detail = paste("Calculating polynomial regression R"))
          
          modelG <- lm(sG ~ imR + imG + imB + imR^2 + imG^2 + imB^2)
          
          i <- i + 1
          incProgress(1/n, detail = paste("Calculating polynomial regression B"))
          
          modelB <- lm(sB ~ imR + imG + imB + imR^2 + imG^2 + imB^2)
          
          i <- i + 1
          incProgress(1/n, detail = paste("Calculating polynomial regression G"))
          
          
          dfIm = data.frame(
            imR = matrix(mR, ncol=1),
            imG = matrix(mG, ncol=1),
            imB = matrix(mB, ncol=1)
          )
          
          print('Calibrating colors...')
          
          i <- i + 1
          incProgress(1/n, detail = paste("Calibrating colors"))
          
          prR <- predict(modelR, dfIm)
          prG <- predict(modelG, dfIm)
          prB <- predict(modelB, dfIm)
          
          dfCal <- as.data.frame(cbind(prR,prG,prB))
          
          print('Rebuilding image...')
          
          i <- i + 1
          incProgress(1/n, detail = paste("Calibrating colors"))
          
          R = matrix(dfCal$prR, nrow=dim(im)[1])
          G = matrix(dfCal$prG, nrow=dim(im)[1])
          B = matrix(dfCal$prB, nrow=dim(im)[1])
          
          imCal = array(dim=dim(im))
          imCal[,,,1] = R
          imCal[,,,2] = G
          imCal[,,,3] = B
          
          imCal <- imager::as.cimg(imCal)
          })
          
          return(imCal)
          
        }
      }
    }
  })
        
  ###  
  output$fileUploaded <- reactive({
    return(!is.null(input$file))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  output$exampleloaded <- reactive({
    return(input$action2 != 0)
  })
  outputOptions(output, 'exampleloaded', suspendWhenHidden=FALSE)
  
  output$coordinates_clicked <- reactive({
    return(!is.null(xyT$df))
  })
  outputOptions(output, 'coordinates_clicked', suspendWhenHidden=FALSE)
  
  output$calibrated <- reactive({
    return(input$action1 != 0)
  })
  outputOptions(output, 'calibrated', suspendWhenHidden=FALSE)
  
  ###
  fName <- renderText({
    inFile <- input$file
    nam <- gsub(".jpg","", inFile$name)
    nam <- gsub(".jpeg","", nam)
    return(nam)
  })
  
  output$downloadData <- downloadHandler(

    filename = function() {
      paste(fName(),'_calibrated.jpg',sep='')
    },
    content = function(file) {
      save.image(mydataProc3(), file, quality = 1)
    }
  )
  

})
