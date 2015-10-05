library(data.table)
library(plotrix)
library(ggmap)

shinyServer(function(input, output, session) {
 
  

  Sys.setlocale('LC_ALL','C')
  options(digits=2)
  # Create a random name for the log file
  logfilename <- paste0('logfile',
                        floor(runif(1, 1e+05, 1e+06 - 1)),
                        '.txt')
  
  
  
  formulaText <- reactive({
    paste(input$variable)
  })
  
  output$caption <- renderText({

    
    
    name <- c("Mr_R@unifi","Mr_X@unifi","Mr_Z@unifi")
    name <- as.data.frame(name)
    address <- c("DPulze Cyberjaya Block D, Cyberjaya CBD, Persiaran Multimedia, Cyber 12, 63000 Cyberjaya, Selangor Darul Ehsan",
                 "Cyberjaya Campus: Jalan Multimedia, 63100 Cyberjaya, Selangor, Malaysia",
                 "Persiaran Rimba Permai, Cyber 8 (Persiaran Ceria), 63000 Cyberjaya")
    address <- as.data.frame(address)
    long <- c("0.34","0.89","0.23")
    long <- as.data.frame(long)
    lat <- c("0.11","0.22","0.44")
    lat <- as.data.frame(lat)
    hsi <- c("4.6","4.82","4.33")
    hsi <- as.data.frame(hsi)
    vob <- c("4.12","4.34","4.78")
    vob <- as.data.frame(vob)
    hyptv <- c("4.24","4.88","4.32")
    hyptv <- as.data.frame(vob)
    dt <- cbind(name,address,long,lat,hsi,vob,hyptv)

    formulaText()
  })
  
  
  
  # This observer adds an entry to the log file every time
  # input$n changes.
  obs <- observe({    
    cat(input$voltage, '\n', file = logfilename, append = TRUE)
  })

  
  render_battery_table <- function(){
    
    A <- read.csv("http://s3.amazonaws.com/csvpastebin/uploads/d019eb968e1393af1716160e317682bf/batteryperformance.csv", sep=",",
                  nrows = 100)
    A
  }
  
  # This funtion will render the Vaping PowerCharts
  
  render_googlemaps <- function(){
    df <- read.csv("http://s3.amazonaws.com/csvpastebin/uploads/80313b2723a392ba761c5587342fb35c/MSC_FDC_DATA.csv")
    df <- as.data.frame(df)
    df <- df[df$EXC_ABB == 'CBJ2',]
    map  <- get_map(location = c(lon = mean(df$LONG), lat = mean(df$LAT)), zoom = 13 , maptype = "roadmap", scale = 2)
    #p <- ggmap(map) +  geom_point(data = df, aes(x = lon, y = lat, fill = "red", alpha = 0.1), size = 5, shape = 21) + geom_text(data = df, aes(x = lon, y = lat, label = paste(OLT_CODE,FDC_CODE), size = 3, vjust = 0, hjust = -0.5))
    p <- ggmap(map) 
    p
  }
  
  render_powercharts <- function(){
    B <- seq(3.2, 7.1, 0.1)
    A <- seq(1.2, 5.1, 0.1)
    C <- outer(B*B,A, '/')
    dt = as.data.table(C)
    dt2 = as.list(data.table(t(dt)))
    tbl <- as.data.frame(dt2)
    colnames(tbl) <- c(seq(3.2, 7.1, 0.1))
    rownames(tbl) <- seq(1.2, 5.1, 0.1)
    par(mar = c(4, 4, 4, 4))
    color2D.matplot(tbl, 
                    main = "Vaping Power Chart 1.0",
                    show.values = TRUE,
                    axes = FALSE,
                    xlab = "Voltage",
                    ylab = "Ohm",
                    vcex = 0.5,
                    vcol = "black",
                    extremes = c("blue","lightgreen","green","lightyellow","yellow","magenta","pink","red"),
                    show.legend = FALSE)
    axis(3, at = seq_len(ncol(tbl)) - 0.5,
         labels = names(tbl), tick = FALSE, cex.axis = 0.6)
    axis(2, at = seq_len(nrow(tbl)) -0.5,
         labels = rev(rownames(tbl)), tick = FALSE, las = 1, cex.axis = 0.6)
  }
  
  w <- function(voltage,ohms) {
    result <- (voltage * voltage) / ohms
    return(result)
  }
  
  r <- function(watt) {
    if ( watt >= 2.0 & watt <3.8 ) {
      return <- "<font color=\"Blue\">[ LOW ] Power level maybe too low to produce a strong vapor</font>"
    }
    else if ( watt >= 3.8 & watt < 8.4 ) {
      return <- "<font color=\"Green\" >[ GOOD ] Best vapor and Cartomizer / Atomizer Performance , coil life is optimal </font>"
    }
    else if ( watt >= 8.4 & watt < 10 ) {
      return <- "<font color=\"Orange\">[ WARM ] Delicate juice start to burn , plenty of throat hit </font>"
    }
    else if ( watt >= 10 & watt < 11 ) {
      return <- "<font color=\"#FA5858\">[ HOT ] Some juice will burn , coil life is shorter </font>"
    }
    else if ( watt >= 11  ) {
      return <- "<font color=\"Red\">[ TOO HOT ] Danger ! Power may melt wire and coil will die </font>"
    }
  }
  
  
  
  # When the client ends the session, suspend the observer.
  # Otherwise, the observer could keep running after the client
  # ends the session.
  session$onSessionEnded(function() {
    obs$suspend()
    
    # Also clean up the log file for this example
    unlink(logfilename)
  })
  
  output$customer <- renderUI({ 
    
    name <- c("Mr_R@unifi","Mr_X@unifi","Mr_Z@unifi")
    name <- as.data.frame(name)
    address <- c("DPulze Cyberjaya Block D, Cyberjaya CBD, Persiaran Multimedia, Cyber 12, 63000 Cyberjaya, Selangor Darul Ehsan",
                 "Cyberjaya Campus: Jalan Multimedia, 63100 Cyberjaya, Selangor, Malaysia",
                 "Persiaran Rimba Permai, Cyber 8 (Persiaran Ceria), 63000 Cyberjaya")
    address <- as.data.frame(address)
    long <- c("0.34","0.89","0.23")
    long <- as.data.frame(long)
    lat <- c("0.11","0.22","0.44")
    lat <- as.data.frame(lat)
    hsi <- c("4.6","4.82","4.33")
    hsi <- as.data.frame(hsi)
    vob <- c("4.12","4.34","4.78")
    vob <- as.data.frame(vob)
    hyptv <- c("4.24","4.88","4.32")
    hyptv <- as.data.frame(vob)
    customer <- cbind(name,address,long,lat,hsi,vob,hyptv)
    
    choices = setNames(customer$name)
    selectInput("name", "Select your choice",  choices)
    
    #selectInput(customer$long, "Customer Selection:",  customer$address )
  })
  
  output$maps <- renderPlot(render_googlemaps())

  output$plot <- renderPlot(render_powercharts())

  output$battery <- renderTable(render_battery_table())

  output$voltage <- renderText({
    paste0("Selected Voltage is: ", input$voltage , " V ")
  })
  
  output$ohms <- renderText({
    paste0("Selected Resistance is: ", input$ohms , " Ohm ")
  })
  
  output$watt <- renderText({
    paste0("Computed Watt is: ", print(signif(w(input$voltage,input$ohms) ), digits=2) , " W "   )
  })
  
  output$recommendation <- renderText({
    paste0(r(w(input$voltage,input$ohms)) )
  })
  
  output$color <- renderText({
    paste0(HTML(r(w(input$voltage,input$ohms))) )
  })
  
})