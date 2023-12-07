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
                        "Bush cricket" = "bush_cricket")),
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
          plotOutput("oscillo"),
          uiOutput("audio")
        )
    )
)

v_wave <- reactiveVal({
  readWave("www/blackbird.wav")
})

oscillo2 <- function (wave, f, channel = 1, from = NULL, to = NULL, fastdisp = FALSE, 
                      scroll = NULL, zoom = FALSE, k = 1, j = 1, cex = NULL, labels = TRUE, 
                      tlab = "Time (s)", alab = "Amplitude", byrow = TRUE, identify = FALSE, 
                      nidentify = NULL, plot = TRUE, colwave = "black", coltitle = "black", 
                      cextitle = 1.2, fonttitle = 2, collab = "black", cexlab = 1, 
                      fontlab = 1, colline = "black", colaxis = "black", cexaxis = 1, 
                      fontaxis = 1, coly0 = "lightgrey", tcl = 0.5, title = FALSE, 
                      xaxt = "s", yaxt = "n", type = "l", bty = "l", alim=NULL) 
{
  ptm.start <- proc.time()
  input <- inputw(wave = wave, f = f, channel = channel)
  wave <- input$w
  f <- input$f
  rm(input)
  p <- k * j
  if (!is.null(from) && is.na(from)) {
    from <- NULL
  }
  if (!is.null(to) && is.na(to)) {
    to <- NULL
  }
  if (is.null(from) && is.null(to)) {
    a <- 0
    b <- length(wave)
    from <- 0
    to <- length(wave)/f
  }
  if (is.null(from) && !is.null(to)) {
    a <- 1
    b <- round(to * f)
    from <- 0
  }
  if (!is.null(from) && is.null(to)) {
    a <- round(from * f) + 1
    b <- length(wave)
    to <- length(wave)/f
  }
  if (!is.null(from) && !is.null(to)) {
    if (from > to) 
      stop("'from' cannot be superior to 'to'")
    if (from == 0) {
      a <- 1
    }
    else {
      a <- round(from * f) + 1
    }
    b <- round(to * f)
  }
  wave <- as.matrix(wave[a:b, ])
  n <- nrow(wave)
  if (isTRUE(fastdisp) & n > 20000) {
    res <- round(n/20000)
    res <- round(log2(res))
    res <- 2^res
    wave <- as.matrix(wave[seq(0, n, by = res), ])
    n <- nrow(wave)
  }
  if (plot) {
    if (is.null(alim)) {
      alim <- max(abs(wave), na.rm=TRUE)
    }
    if (k == 1 & j == 1) {
      if (!is.null(scroll)) {
        if (!is.numeric(scroll)) 
          stop("scroll has to a numeric")
        if (length(scroll) > 1) 
          stop("length of scroll cannot be superior to 1")
        if (zoom) 
          stop("zoom and scroll cannot be used together")
        if (identify) 
          stop("identify and scroll cannot be used together")
        step <- round(seq(0, n, length.out = scroll + 
                            1))
        lstep <- length(step)
        pos <- 1:(lstep - 1)
        plot.dynosc <- function(panel) {
          with(panel, {
            soscillo(wave = wave, f = f, from = step[pos]/f, 
                     to = step[pos + 1]/f, colwave = colwave, 
                     collab = collab, tlab = tlab, alab = alab, 
                     cexlab = cexlab, fontlab = fontlab, colline = colline, 
                     colaxis = colaxis, cexaxis = cexaxis, fontaxis = fontaxis, 
                     coly0 = coly0, bty = bty, tickup = alim, ylim = c(-alim, alim))
            title(main = pos, col.main = coltitle, cex.main = cextitle, 
                  font.main = fonttitle)
          })
          panel
        }
        osc.panel <- rpanel::rp.control("Window")
        rpanel::rp.slider(osc.panel, pos, from = 1, to = lstep - 
                            1, resolution = 1, title = "Window", action = plot.dynosc)
      }
      else {
        if (zoom) {
          par(tcl = 0.5, col.axis = colaxis, cex.axis = cexaxis, 
              font.axis = fontaxis, col = colline, las = 0)
          plot(x = seq(from, to, length.out = n), y = wave, 
               col = colwave, type = type, cex = cex, xaxs = "i", 
               yaxs = "i", xlab = "", ylab = "", ylim = c(-alim, 
                                                          alim), xaxt = xaxt, yaxt = yaxt, cex.lab = 0.8, 
               font.lab = 2, bty = bty)
          if (bty == "l" | bty == "o") {
            axis(side = 1, col = colline, labels = FALSE)
            axis(side = 2, at = alim, 
                 col = colline, labels = FALSE)
          }
          mtext(tlab, col = collab, font = fontlab, cex = cexlab, 
                side = 1, line = 3)
          mtext(alab, col = collab, font = fontlab, cex = cexlab, 
                side = 2, line = 2.5)
          abline(h = 0, col = coly0, lty = 2)
          cat("choose start and end positions on the wave\n")
          if (.Platform$OS.type == "windows") 
            flush.console()
          coord <- locator(n = 2)
          from <- coord$x[1]
          c <- from * f - a
          to <- coord$x[2]
          d <- to * f - a
          if (d < c) {
            c <- d
            d <- c
          }
          wave <- as.matrix(wave[c:d, 1])
          n <- nrow(wave)
        }
        op <- par(tcl = tcl, col.axis = colaxis, cex.axis = cexaxis, 
                  font.axis = fontaxis, col = colline, las = 0)
        plot(x = seq(from, to, length.out = n), y = wave, 
             col = colwave, type = type, cex = cex, xaxs = "i", 
             yaxs = "i", xlab = "", ylab = "", ylim = c(-alim, 
                                                        alim), xaxt = xaxt, yaxt = yaxt, cex.lab = 0.8, 
             font.lab = 2, bty = bty)
        if (bty == "l" | bty == "o") {
          axis(side = 1, col = colline, labels = FALSE)
          axis(side = 2, at = alim, 
               col = colline, labels = FALSE)
        }
        if (labels) {
          mtext(tlab, col = collab, font = fontlab, side = 1, 
                line = 3, cex = cexlab)
          mtext(alab, col = collab, font = fontlab, cex = cexlab, 
                side = 2, line = 3)
        }
        abline(h = 0, col = coly0, lty = 2)
        if (is.expression(title)) {
          title <- title
        }
        else {
          if (title == FALSE) {
            title <- ""
          }
          else {
            if (is.character(title)) {
              title <- title
            }
            else {
              title <- paste("Total time =", as.character(round(n/f, 
                                                                3)), "s - f =", as.character(f), "Hz")
            }
          }
        }
        title(main = title, col.main = coltitle, cex.main = cextitle, 
              font.main = fonttitle)
        if (identify) {
          cat("choose points on the wave\n")
          if (.Platform$OS.type == "windows") 
            flush.console()
          x <- seq(from = from, to = to, length.out = n)
          y <- wave
          if (is.null(nidentify)) {
            nidentify <- length(x)
          }
          id <- identify(x = x, y = y, labels = round(x, 
                                                      3), col = "red", n = nidentify, plot = TRUE)
          time <- x[id]
          abline(v = time, col = "red")
          amp <- y[id, 1]
          res <- cbind(time, amp)
          return(res)
        }
        par(op)
      }
    }
    else {
      if (!is.null(scroll)) 
        stop("scroll cannot be used with a multi-frame window")
      if (zoom) 
        stop("'zoom' does work with a single-frame window only ('k'=1 and 'j'=1)")
      if (identify) 
        stop("'identify' does work with a single-frame window only ('k'=1 and 'j'=1)")
      x <- n%/%p
      def.par <- par(no.readonly = TRUE)
      on.exit(par(def.par))
      m <- matrix(1:p, k, j, byrow = byrow)
      layout(m)
      par(tcl = tcl, oma = c(3, 2, 2, 0.5), mar = rep(0, 
                                                      4) + 0.8, mgp = c(0, 0.15, 0), col.axis = colaxis, 
          cex.axis = cexaxis, font.axis = fontaxis, col = colline, 
          las = 0)
      wave1 <- as.matrix(wave[0:x, ])
      n1 <- nrow(wave1)
      plot(x = seq(from, from + (x/f), length.out = n1), 
           y = wave1, col = colwave, type = type, cex = cex, 
           xaxs = "i", yaxs = "i", xlab = "", ylab = "", 
           ylim = c(-alim, alim), xaxt = xaxt, yaxt = yaxt, 
           bty = bty)
      axis(side = 1, col = colline, labels = FALSE)
      if (bty == "l" | bty == "o") {
        axis(side = 2, at = alim, col = colline, 
             labels = FALSE)
        axis(side = 1, col = colline, labels = FALSE)
      }
      abline(h = 0, col = coly0, lty = 2)
      if (is.character(title)) 
        title <- paste(title)
      if (title == FALSE) {
        title <- paste("")
      }
      else {
        title <- paste("Window time =", as.character(round(n/(p * 
                                                                f), 3)), "s - Total time =", as.character(round(n/f, 
                                                                                                                3)), "s - f =", as.character(f), "Hz")
      }
      mtext(paste(title), side = 3, line = 0.4, col = coltitle, 
            cex = cextitle, font = fonttitle, outer = TRUE)
      if (labels) {
        mtext(tlab, col = collab, side = 1, line = 1.5, 
              font = fontlab, cex = cexlab, outer = TRUE)
        mtext(alab, col = collab, side = 2, font = fontlab, 
              cex = cexlab, line = 0.4, outer = TRUE)
      }
      for (i in 1:(p - 1)) {
        xx <- ((i * n)%/%p) + 1
        yy <- ((i + 1) * n)%/%p
        wave2 <- as.matrix(wave[xx:yy, ])
        n2 <- nrow(wave2)
        plot(x = seq(from + (xx/f), from + (yy/f), length.out = n2), 
             y = wave2, col = colwave, type = type, cex = cex, 
             xaxs = "i", yaxs = "i", xlab = "", ylab = "", 
             ylim = c(-alim, alim), xaxt = xaxt, yaxt = yaxt, 
             bty = bty)
        if (bty == "l" | bty == "o") {
          axis(side = 2, at = alim, col = colline, 
               labels = FALSE)
          axis(side = 1, col = colline, labels = FALSE)
        }
        abline(h = 0, col = coly0, lty = 2)
      }
    }
    ptm <- proc.time() - ptm.start
    if (isTRUE(plot) && ptm[3] > 10) 
      cat("This took quite a lot of time to display this graphic, you may set 'fastdisp=TRUE' for a faster, but less accurate, display\n")
    invisible(wave)
  }
  else return(wave)
}    

get_audio_tag<-function(input){
  filename <- paste0("_",paste(input$animal, input$animal_v, input$noise, input$noise_v, sep="_"), ".wav")
  wa <- readWave(paste0("www/",input$animal,".wav"))
  wn <- readWave(paste0("www/",input$noise,".wav"))

  
  a_scale <- input$animal_v/200
  n_scale <- input$noise_v/200
  w <- a_scale*wa + n_scale*wn 
  if (!file.exists(paste0("www/",filename))) {
    writeWave(w, paste0("www/",filename))
  }
  v_wave(w)
  return(tags$audio(src = filename, type ="audio/wav", controls = NA))
}


server <- function(input, output) {
  output$spectro <- renderPlot({
    if (!(input$animal_v == 0 && input$noise_v ==0)) {
      spectro(v_wave(),
              norm=F, scale=F, wl=256
              )
    }
  })
  output$oscillo <- renderPlot({
    if (!(input$animal_v == 0 && input$noise_v ==0)) {
      oscillo2(v_wave(),alim=0.5)
    }
  })
  output$audio <- renderUI({
    get_audio_tag(input)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


