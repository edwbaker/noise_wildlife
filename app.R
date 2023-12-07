library(shiny)
library(tuneR)
library(seewave)

ui <- fluidPage(
    # Application title
    titlePanel("Noise demo"),

    sidebarLayout(
        sidebarPanel(
          selectInput("animal", 
                      "Animal:",
                      c("Blackbird" = "blackbird",
                        "Bush criket" = "bush_cricket")),
          sliderInput("animal_v",
                      label="Animal volume:",
                      min = 0,
                      max = 100,
                      value = 50),
          selectInput("noise", 
                      "Noise:",
                      c("White noise" = "wnoise",
                        "Road" = "road"),
                      selected="road"),
          sliderInput("noise_v",
                      label="Noise volume:",
                      min = 0,
                      max = 100,
                      value = 0)
        ),

        mainPanel(
          plotOutput("spectro"),
          uiOutput("audio")
        )
    )
)

v_wave <- reactiveVal({
  readWave("www/blackbird.wav")
})


get_audio_tag<-function(input){
  filename <- paste0("_",paste(input$animal, input$animal_v, input$noise, input$noise_v, sep="_"), ".wav")
  wa <- readWave(paste0("www/",input$animal,".wav"))
  wn <- readWave(paste0("www/",input$noise,".wav"))

  
  a_scale <- input$animal_v/200
  n_scale <- input$noise_v/200
  w <- a_scale*wa + n_scale*wn 
  print(max(w@left))
  if (!file.exists(paste0("www/",filename))) {
    writeWave(w, paste0("www/",filename))
  }
  v_wave(w)
  return(tags$audio(src = filename, type ="audio/wav", controls = NA))
}


server <- function(input, output) {
  output$spectro <- renderPlot({
    spectro(v_wave(),
            norm=F, scale=F, wl=256
            )
  })
  output$audio <- renderUI({
    get_audio_tag(input)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
