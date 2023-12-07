#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tuneR)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Noise demo"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("noise",
                        label="Noise volume:",
                        min = 0,
                        max = 100,
                        value = 0)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           uiOutput("audio")
        )
    )
)

get_audio_tag<-function(input){
  if (input$noise == 0) {
    return(tags$audio(src = "3k.wav", type ="audio/wav", controls = NA))
  } else {
    filename <- paste0(input$noise, ".wav")
    if (!file.exists(filename)) {
      w <- readWave("www/3k.wav")
      n <- readWave("www/wnoise.wav")
      n_scale <- input$noise/100
      wn <- w + n_scale*n
      writeWave(normalize(wn), paste0("www/",filename))
      return(tags$audio(src = filename, type ="audio/wav", controls = NA))
    }
  }
}

# Define server logic required to draw a histogram
server <- function(input, output) {
  if (!file.exists("www/3k.wav")) {
    w <- sine(3000, samp.rate=41000, duration=3*41000)
    w <- 0.5 * w
    writeWave(w, "www/3k.wav")
  }
  if (!file.exists("www/wnoise.wav")) {
    w <- noise(kind="white", samp.rate=41000, duration=3*41000)
    writeWave(w, "www/wnoise.wav")
  }
  output$audio <- renderUI({
    get_audio_tag(input)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
