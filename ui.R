
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(raster)
library(imager)
library(sp)

shinyUI(fluidPage(
  
  title = h3("Shiny ColorChecker"),

  fluidRow(column(3,
                  h3("ColorChecker calibrator"),
                  
                  h3(),
                  fileInput("file", accept = c('image/jpeg'), h4("1. File input")),
                  
                  actionButton("action2", "Load example"),
                  
                  h3(),
                  h4("2. Select corners"),
                  h5("Select the four corners of the ColorChecker in the image like this:"),
                  div(img(src = "ColorChecker_corners.png", height = 180, width = 120, align = "center"), style="text-align: center;"),
                  
                  h3(),
                  sliderInput("decimal", h4("3. Adjust patch area:"), min = 0, max = 1, value = 0.6, step = 0.1),
                  h5("In case the red polygons do not match the ColorChecker patches, you can asjust the selected area"),
                  
                  h3(),
                  h4("4. Run calibration"),
                  actionButton("action1", "Go!"),
                  
                  h3(),
                  downloadButton("downloadData", "Download calibrated image"),
                  
                  h3(),
                  h5("- Steven M. Van Belleghem", align = "right"),
                  
                  h3(),
                  conditionalPanel(condition = "output.coordinates_clicked" , h4("xy corners ColorChecker")),
                  tableOutput("plot_clickedpoints")),
  
            column(9,
                  conditionalPanel(condition = "output.fileUploaded" , h3("Original image")),
                  conditionalPanel(condition = "output.exampleloaded" , h3("Example image")),
                  plotOutput("OriginalImage1", click = "plot_click", height = "800px"),
                  conditionalPanel(condition = "output.calibrated" , h3("Calibrated image")),
                  plotOutput("OriginalImage2", height = "800px")
                  )
           
  )
  
))
