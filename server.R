library(ggmap)
library(dplyr)
library(stringr)

shinyServer(function(input, output, session) {
  
  
  #https://s3.amazonaws.com/csvpastebin/uploads/b28cd41beec5f4c85b36189f2aea6e30/account.csv
  newrow = as.data.frame(t(c(1:13)))
  df1 <- read.csv("http://s3.amazonaws.com/csvpastebin/uploads/12bad2d273e2468c12515492eac4dfca/CEI_perUser_Score.csv")
  colnames(newrow) <- colnames(df1)
  newrow$login_id = "All Customer"
  df1 <- rbind(df1,newrow)
  #s_options[[sprintf("option label %d 1", x)]] <- sprintf("option-%d-1", x)
  #s_options <- list(df1$login_id)
  updateSelectInput(session, "user", choices = as.character(as.factor(df1$login_id)), selected=as.character(as.factor(df1$login_id)))
  
  

  Sys.setlocale(locale="C")
  options(digits=5)
  # Create a random name for the log file
  logfilename <- paste0('logfile',
                        floor(runif(1, 1e+05, 1e+06 - 1)),
                        '.txt')
  #df1 <- read.csv("http://s3.amazonaws.com/csvpastebin/uploads/9a319ae37fef74ac2acee15e6872d6e3/CEI_perUser_Score.csv")
  #output$test = renderUI(selectInput("test", "Select your choice", choices = as.character(as.factor(df1$login_id)) , label = "ss" ))

  
  observe({
    #x <- input$user

  })
  
  formulaText <- reactive({
    
    
    if (paste(input$user) == "All Customer") {
      
      
      # 101.67 2.9017
    
      
       # output$maps = renderPlot(ggmap(get_map(location = "PutraJaya, Malaysia", zoom = 15 , maptype = "roadmap" ),legend = "right",extent = "panel", height = 600, width = 1800 )+ ggtitle("FDC Location in Google Maps"))
       
        output$cei_table = renderDataTable({
          
        #output$hsicolor1 = renderText("<th bgcolor=white scope=row>0.0</th>")
          
        output$vobbscore = renderText("<th bgcolor=white scope=row>0.0</th>")
        output$hsiscore = renderText("<th bgcolor=white scope=row>0.0</th>")
        output$iptvscore = renderText("<th bgcolor=white scope=row>0.0</th>")
        output$overallscore = renderText("<th bgcolor=white scope=row>0.0</th>")

        newrow = as.data.frame(t(c(1:13)))
        df1 <- read.csv("http://s3.amazonaws.com/csvpastebin/uploads/12bad2d273e2468c12515492eac4dfca/CEI_perUser_Score.csv")
        colnames(newrow) <- colnames(df1)
        newrow$login_id = "All Customer"
        df1 <- rbind(df1,newrow)
        df2 <- subset(df1, !is.na(df1$LONG))								 
        df3 <- subset(df2, !is.na(df2$package_name))
        lon <-data.frame(df3$LONG)
        lat <-data.frame(df3$LAT)
        f <-data.frame(df3$REF_FDC)
        
        mydf <- as.data.frame(cbind(lon,lat,f))
        names(mydf) = c("lon","lat","f")
        cbj <- get_map(location = c(lon = mean(mydf$lon), lat = mean(mydf$lat)), zoom = 12)
        output$maps = renderPlot(ggmap(cbj) + geom_point(data = mydf, aes(x = lon, y = lat),  alpha = .5 , col = "red" , size = 5 ) 
                                 +  geom_text(data = mydf , aes(x = lon, y = lat , label = f, size = 3, vjust = 0, hjust = -0.5)) 
                                 +  ggtitle("FDC Location in MSC Zone")) 

        
        df1 <- read.csv("http://s3.amazonaws.com/csvpastebin/uploads/12bad2d273e2468c12515492eac4dfca/CEI_perUser_Score.csv")
        #df1[c(1,3,4,5,6)]
      }, options = list( pageLength = 10 , bAutoWidth = TRUE,scrollX = TRUE,scrolly = TRUE))
    } else
    {
      output$cei_table = renderDataTable({
      
        df2 <- read.csv("http://s3.amazonaws.com/csvpastebin/uploads/04ee173cef6e6e09605af889bf0ff3ff/account.csv")
        df1 <- read.csv("http://s3.amazonaws.com/csvpastebin/uploads/9a319ae37fef74ac2acee15e6872d6e3/CEI_perUser_Score.csv")
        df3 <- merge(x = df1, y = df2, by = "login_id", all.x = TRUE)
        df3 <- filter(df3, login_id == paste(input$user))
   
        
        #output$vobbscore = renderText(as.numeric(str_replace(df3[,7] , "\\[1] ", "")))
        
        #renderText("<th bgcolor=white scope=row>0.0</th>")
        
        
        if (as.numeric(str_replace(df3[,7] , "\\[1] ", ""))  > 0.0 & as.numeric(str_replace(df3[,7] , "\\[1] ", ""))  < 0.67 )  {
          output$vobbscore = renderText(paste("<th bgcolor=red scope=row>",paste(round(as.numeric(str_replace(df3[,7] , "\\[1] ", "")),2)),paste("</th>")))
          
        } else if (as.numeric(str_replace(df3[,7] , "\\[1] ", ""))  >= 0.67 & as.numeric(str_replace(df3[,7] , "\\[1] ", ""))  < 0.88 )  {
          output$vobbscore = renderText(paste("<th bgcolor=yellow scope=row>",paste(round(as.numeric(str_replace(df3[,7] , "\\[1] ", "")),2)),paste("</th>")))
          
        } else if (as.numeric(str_replace(df3[,7] , "\\[1] ", ""))  >= 0.88 )  {
          output$vobbscore = renderText(paste("<th bgcolor=green scope=row>",paste(round(as.numeric(str_replace(df3[,7] , "\\[1] ", "")),2)),paste("</th>")))
        }
        
          
        if (as.numeric(str_replace(df3[,8] , "\\[1] ", ""))  > 0.0 & as.numeric(str_replace(df3[,8] , "\\[1] ", ""))  < 0.67 )  {
            output$hsiscore = renderText(paste("<th bgcolor=red scope=row>",paste(round(as.numeric(str_replace(df3[,8] , "\\[1] ", "")),2)),paste("</th>")))
            
        } else if (as.numeric(str_replace(df3[,8] , "\\[1] ", ""))  >= 0.67 & as.numeric(str_replace(df3[,8] , "\\[1] ", ""))  < 0.88 )  {
            output$hsiscore = renderText(paste("<th bgcolor=yellow scope=row>",paste(round(as.numeric(str_replace(df3[,8] , "\\[1] ", "")),2)),paste("</th>")))
            
        } else if (as.numeric(str_replace(df3[,8] , "\\[1] ", ""))  >= 0.88 )  {
            output$hsiscore = renderText(paste("<th bgcolor=green scope=row>",paste(round(as.numeric(str_replace(df3[,8] , "\\[1] ", "")),2)),paste("</th>")))
            
        }
        
        # output$iptvscore = renderText(as.numeric(str_replace(df3[,9] , "\\[1] ", ""))/5)
        
        if (as.numeric(str_replace(df3[,9] , "\\[1] ", ""))/5  >= 0.0 & as.numeric(str_replace(df3[,9] , "\\[1] ", ""))/5  < 0.67 )  {
          output$iptvscore = renderText(paste("<th bgcolor=red scope=row>",paste(round(as.numeric(str_replace(df3[,9] , "\\[1] ", ""))/5,2)),paste("</th>")))
        }
        else if (as.numeric(str_replace(df3[,9] , "\\[1] ", ""))/5  >= 0.67 & as.numeric(str_replace(df3[,9] , "\\[1] ", ""))/5  < 0.88 )  {
          output$iptvscore = renderText(paste("<th bgcolor=yellow scope=row>",paste(round(as.numeric(str_replace(df3[,9] , "\\[1] ", ""))/5,2)),paste("</th>")))
        }
        else if (as.numeric(str_replace(df3[,9] , "\\[1] ", ""))/5  >= 0.88  )  {
          output$iptvscore = renderText(paste("<th bgcolor=green scope=row>",paste(round(as.numeric(str_replace(df3[,9] , "\\[1] ", ""))/5,2)),paste("</th>")))
        }
        
        #if (as.numeric(str_replace(df3[,9] , "\\[1] ", ""))/5  > 0.0 & as.numeric(str_replace(df3[,9] , "\\[1] ", ""))/5  < 0.67 )  {
        #  output$iptvscore = renderText(paste("<th bgcolor=red scope=row>",paste(round(as.numeric(str_replace(df3[,9] , "\\[1] ", ""))/5),2),paste("</th>")))
          
        #} else if (as.numeric(str_replace(df3[,9] , "\\[1] ", ""))/5  >= 0.67 & as.numeric(str_replace(df3[,9] , "\\[1] ", ""))/5  < 0.88 )  {
        #  output$iptvscore = renderText(paste("<th bgcolor=yellow scope=row>",paste(round(as.numeric(str_replace(df3[,9] , "\\[1] ", ""))/5),2),paste("</th>")))
          
        #} else if (as.numeric(str_replace(df3[,9] , "\\[1] ", ""))/5  >= 0.88 )  {
        #  output$iptvscore = renderText(paste("<th bgcolor=green scope=row>",paste(round(as.numeric(str_replace(df3[,9] , "\\[1] ", ""))/5),2),paste("</th>")))
          
       # }
        
        
        if (as.numeric(str_replace(df3[,8] , "\\[1] ", ""))  > 0.0 & as.numeric(str_replace(df3[,8] , "\\[1] ", ""))  < 0.67 )  {
          output$overallscore = renderText(paste("<th bgcolor=red scope=row>",paste(sum( as.numeric(str_replace(df3[,7] , "\\[1] ", "")) + as.numeric(str_replace(df3[,8] , "\\[1] ", "")) 
                                                                                     + as.numeric(str_replace(df3[,9] , "\\[1] ", ""))/5  )/3),paste("</th>")))
          
        } else if (as.numeric(str_replace(df3[,8] , "\\[1] ", ""))  >= 0.67 & as.numeric(str_replace(df3[,8] , "\\[1] ", ""))  < 0.88 )  {
          output$overallscore = renderText(paste("<th bgcolor=yellow scope=row>",paste(sum( as.numeric(str_replace(df3[,7] , "\\[1] ", "")) + as.numeric(str_replace(df3[,8] , "\\[1] ", "")) 
                                                                                        + as.numeric(str_replace(df3[,9] , "\\[1] ", ""))/5  )/3),paste("</th>")))
          
        } else if (as.numeric(str_replace(df3[,8] , "\\[1] ", ""))  >= 0.88 )  {
          output$overallscore = renderText(paste("<th bgcolor=green scope=row>",paste(sum( as.numeric(str_replace(df3[,7] , "\\[1] ", "")) + as.numeric(str_replace(df3[,8] , "\\[1] ", "")) 
                                                                                       + as.numeric(str_replace(df3[,9] , "\\[1] ", ""))/5  )/3),paste("</th>")))
          
        }
        


        output$fdc = renderText(str_replace(df3[,10] , "\\[1] ", ""))
        output$dn = renderText(str_replace(df3[,5] , "\\[1] ", ""))
        output$reffdc = renderText(str_replace(df3[,11] , "\\[1] ", ""))
        output$model = renderText(str_replace(df3[,16] , "\\[1] ", ""))
        
        output$name = renderText(str_replace(df3[,14] , "\\[1] ", ""))
        output$package = renderText(str_replace(df3[,3] , "\\[1] ", ""))
        output$address = renderText(paste("JALAN ",str_replace(df3[,17] , "\\[1] ", "")," , ",str_replace(df3[,18] , "\\[1] ", "")," , ",
                                          str_replace(df3[,20] , "\\[1] ", "")," , ",str_replace(df3[,19] , "\\[1] ", "")," , ",str_replace(df3[,21] , "\\[1] ", "")))
        

     #   output$maps = renderPlot(ggmap(get_map(location = c(lon = mean(df3$LONG) 
                             #                               , lat = mean(df3$LAT)) , zoom = 15 , maptype = "roadmap" ),legend = "right",extent = "panel", height = 600, width = 1800  ) +   geom_point(aes(x =  df3$LONG , y = df3$LAT), data = df3, alpha = .5 , col = "red")
                              #   + geom_text(data = df3 , aes(x = df3$LONG , y = df3$LAT , label = paste(df3$REF_FDC), size = 3, vjust = 0, hjust = -0.5))
                             #    +ggtitle("FDC Location in Google Maps"))
        
   
       
        output$maps <- tryCatch(
          {
            
            lon <-data.frame(df3$LONG)
            lat <-data.frame(df3$LAT)
            f <- data.frame(df3$REF_FDC)
            
            mydf <- as.data.frame(cbind(lon,lat,f))
            names(mydf) = c("lon","lat","f")
            cbj <- get_map(location = c(lon = mean(mydf$lon), lat = mean(mydf$lat)), zoom = 15)
            renderPlot(ggmap(cbj) + geom_point(data = mydf, aes(x = lon, y = lat),  alpha = .5 , col = "red" , size = 5) 
                       +  geom_text(data = mydf , aes(x = lon, y = lat , label = f, size = 3, vjust = 0, hjust = -0.5)) 
                       +  ggtitle("FDC Location in MSC Zone")) 
          })
        
        
      
        df3

      }, options = list( pageLength = 10, bAutoWidth = TRUE ,scrollX = TRUE,scrolly = TRUE)) 
      
      
     
      
    }

   
    
    paste(input$user)
    
  })
  
  output$caption <- renderText({

    formulaText()
  })
  
  
  
  # This observer adds an entry to the log file every time
  # input$n changes.
  obs <- observe({    
    cat(input$voltage, '\n', file = logfilename, append = TRUE)
    cat(input$user, '\n', file = logfilename, append = TRUE)
  })

 
  render_hsi_single_customer <- function(){
    #data <- read.csv("http://s3.amazonaws.com/csvpastebin/uploads/86a7d7c118b9be49adc14f1d64565a4e/lostcarrier.csv")
    data <- sample(1:10, 1000,replace = TRUE)
    hist(data , col = "#8199A7")
  }
  
  render_vobb_single_customer <- function(){
    data1 <- sample(1:10, 1000,replace = TRUE)
    hist(data1,col = "#A4CACC",main = "")
  }
  
  render_iptv_single_customer <- function(){
    data2 <- sample(1:10, 1000,replace = TRUE)
    hist(data2,col = "#42616E",main = "")
  }
  
  
  render_hsi_all_customer <- function(){
    data3 <- sample(1:1000, 1000,replace = TRUE)
    hist(data3,col = "#8199A7",main = "",xlim=c(0,1000),las=1,breaks=200)
  }
  
  render_vobb_all_customer <- function(){
    data4 <- sample(1:1000, 1000,replace = TRUE)
    hist(data4,col = "#A4CACC",main = "",xlim=c(0,1000),las=1,breaks=200)
  }
  
  render_iptv_all_customer <- function(){
    data5 <- sample(1:1000, 1000,replace = TRUE)
    hist(data5,col = "#42616E",main = "",xlim=c(0,1000),las=1,breaks=200)
  }
  
  
  # This funtion will render the Vaping PowerCharts
  
  render_googlemaps <- function(){
    
    df1 <- read.csv("http://s3.amazonaws.com/csvpastebin/uploads/80313b2723a392ba761c5587342fb35c/MSC_FDC_DATA.csv")
    df2 <- as.data.frame(df1)
    df2 <- df2[df2$EXC_ABB == 'CBJ2',] 
    map1  <- get_map(location = c(lon = mean(df1$LONG), lat = mean(df1$LAT)), zoom = 13 , maptype = "roadmap", scale = 2)
    #p <- ggmap(map) +  geom_point(data = df, aes(x = lon, y = lat, fill = "red", alpha = 0.1), size = 5, shape = 21) + geom_text(data = df, aes(x = lon, y = lat, label = paste(OLT_CODE,FDC_CODE), size = 3, vjust = 0, hjust = -0.5))
    p <- ggmap(map1,legend = "right",extent = "panel") 
    p
  }
  

  
  output$cei_table = renderDataTable({
    df1 <- read.csv("http://s3.amazonaws.com/csvpastebin/uploads/12bad2d273e2468c12515492eac4dfca/CEI_perUser_Score.csv")
    #df1[c(1,3,4,5,6)]
  }, options = list( pageLength = 10, bAutoWidth = TRUE))
  
 
  
  
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
  
  output$hsi <- renderPlot(render_hsi_single_customer())
  
  output$vobb <- renderPlot(render_vobb_single_customer())
  
  output$iptv <- renderPlot(render_iptv_single_customer())
  
  output$hsi_all <- renderPlot(render_hsi_all_customer())
  
  output$vobb_all <- renderPlot(render_vobb_all_customer())
  
  output$iptv_all <- renderPlot(render_iptv_all_customer())
  
  #output$maps <- renderPlot(render_googlemaps())

  output$plot <- renderPlot(render_powercharts())


  
})