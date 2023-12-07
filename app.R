library(shiny)
library(tuneR)

if (!file.exists("www/3k.wav")) {
  w <- 0.45*sine(3000, samp.rate=41000, duration=3*41000)
  writeWave(w, "www/3k.wav")
}
if (!file.exists("www/6k.wav")) {
  w <- 0.45*sine(6000, samp.rate=41000, duration=3*41000)
  writeWave(w, "www/6k.wav")
}
if (!file.exists("www/wnoise.wav")) {
  w <- 0.45*noise(kind="white", samp.rate=41000, duration=3*41000)
  writeWave(w, "www/wnoise.wav")
}
if (!file.exists("www/pnoise.wav")) {
  w <- 0.45*noise(kind="pink", samp.rate=41000, duration=3*41000)
  writeWave(w, "www/pnoise.wav")
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Noise demo"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput("animal", 
                      "Animal:",
                      c("3kHz sine" = "3kHz",
                        "6kHz sine" = "6kHz")),
          sliderInput("animal_v",
                      label="Animal volume:",
                      min = 0,
                      max = 100,
                      value = 50),
          selectInput("noise", 
                      "Noise:",
                      c("white noise" = "white",
                        "pink noise" = "pink")),
          sliderInput("noise_v",
                      label="Noise volume:",
                      min = 0,
                      max = 100,
                      value = 50)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          uiOutput("audio")
        )
    )
)

get_audio_tag<-function(input){
  filename <- paste0(paste(input$animal, input$animal_v, input$noise, input$noise_v, sep="_"), ".wav")
  if (!file.exists(paste0("www/",filename))) {
    if (input$noise == "white") {
      wn <- readWave("www/wnoise.wav")
    }
    if (input$noise == "pink") {
      wn <- readWave("www/pnoise.wav")
    }
    if (input$animal == "3kHz") {
      wa <- readWave("www/3k.wav")
    }
    if (input$animal == "6kHz") {
      wa <- readWave("www/6k.wav")
    }
    
    a_scale <- input$animal_v/100
    n_scale <- input$noise_v/100
    w <- a_scale*wa + n_scale*wn 
    writeWave(w, paste0("www/",filename))
  }
  return(tags$audio(src = filename, type ="audio/wav", controls = NA))
}

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$audio <- renderUI({
    get_audio_tag(input)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
