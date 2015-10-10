library(ggmap)
library(dplyr)
library(stringr)

shinyServer(function(input, output, session) {
  #https://s3.amazonaws.com/csvpastebin/uploads/b28cd41beec5f4c85b36189f2aea6e30/account.csv
  
  newrow = as.data.frame(t(c(1:13)))
  df1 <-
    read.csv("CEI_perUser_Score.csv")
  colnames(newrow) <- colnames(df1)
  newrow$login_id = "ALL CUSTOMER"
  df1 <- rbind(df1,newrow)
  df1 <- df1[with(df1, order(login_id)), ]
  
  newrowexchange = as.data.frame(t(c(1:13)))
  colnames(newrowexchange) <- colnames(df1)
  newrowexchange$exchange = "ALL EXCHANGE"
  df2 <- rbind(df1,newrowexchange)
  agg <- aggregate(df2$voice_score, list(df2$exchange),mean)
  agg <- agg[with(df2, order(exchange)), ]
  names(agg) <- c("exchange","% of vobb performance")
  agg <- na.omit(agg)
  
  updateSelectInput(
    session, "user", choices = as.character(as.factor(df1$login_id)), selected =
      as.character(as.factor(df1$login_id))
  )
  updateSelectInput(
    session, "exchange", choices = as.character(as.factor(agg$exchange)), selected =
      as.character(as.factor(agg$exchange))
  )
  
  output$hsi <-
    tryCatch({
      renderPlot(
        barplot(
          1, type = "n" ,  ylim = c(0,50),col = "#A4CACC", ylab = "Total termination", xlab =
            "date"
        )
      )
    })
  
  Sys.setlocale(locale = "C")
  options(digits = 5)
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
    if (paste(input$user) == "ALL CUSTOMER" &
        paste(input$exchange) == "ALL EXCHANGE") {
      # 101.67 2.9017
      
      
      # output$maps = renderPlot(ggmap(get_map(location = "PutraJaya, Malaysia", zoom = 15 , maptype = "roadmap" ),legend = "right",extent = "panel", height = 600, width = 1800 )+ ggtitle("FDC Location in Google Maps"))
      
      output$cei_table = renderDataTable({
        #output$hsicolor1 = renderText("<th bgcolor=white scope=row>0.0</th>")
        
        output$vobbscore = renderText("<th bgcolor=white scope=row>0.0</th>")
        output$hsiscore = renderText("<th bgcolor=white scope=row>0.0</th>")
        output$iptvscore = renderText("<th bgcolor=white scope=row>0.0</th>")
        output$overallscore = renderText("<th bgcolor=white scope=row>0.0</th>")
        
        newrow = as.data.frame(t(c(1:13)))
        df1 <-
          read.csv("CEI_perUser_Score.csv")
        colnames(newrow) <- colnames(df1)
        newrow$login_id = "ALL CUSTOMER"
        df1 <- rbind(df1,newrow)
        df2 <- subset(df1,!is.na(df1$LONG))
        df3 <- subset(df2,!is.na(df2$package_name))
        lon <- data.frame(df3$LONG)
        lat <- data.frame(df3$LAT)
        f <- data.frame(df3$REF_FDC)
        
        df2 <-
          read.csv("account.csv")
        df1 <-
          read.csv("CEI_perUser_Score.csv")
        df3 <-
          merge(
            x = df1, y = df2, by = "login_id", all.x = FALSE , all.y = FALSE
          )
        df3 <-
          aggregate(
            cbind(
              df3$voice_score,df3$internet_score,df3$iptv_score / 5,df3$LONG,df3$LAT
            ) ~ df3$exchange , data = df3, mean , na.rm = TRUE
          )
        names(df3) <-
          c("exchange","vobb_score","hsi_score","iptv_score","long","lat")
        print(df3)
        
        df4 <- colMeans(df3[2:4])
        df4 <- as.data.frame(t(melt(df4)))
        print(df4)
        
        if (df4$vobb_score  > 0.0 & df4$vobb_score  < 0.67)  {
          output$vobbscore = renderText(paste(
            "<th bgcolor=red scope=row>",paste(round(as.numeric(
              df4$vobb_score
            ),2)),paste("</th>")
          ))
          
        } else if (df4$vobb_score >= 0.67 &
                   df4$vobb_score  < 0.88)  {
          output$vobbscore = renderText(paste(
            "<th bgcolor=yellow scope=row>",paste(round(as.numeric(
              df4$vobb_score
            ),2)),paste("</th>")
          ))
          
        } else if (df4$vobb_score  >= 0.88)  {
          output$vobbscore = renderText(paste(
            "<th bgcolor=green scope=row>",paste(round(as.numeric(
              df4$vobb_score
            ),2)),paste("</th>")
          ))
          
        }
        
        
        if (df4$hsi_score  > 0.0 & df4$hsi_score  < 0.67)  {
          output$hsiscore = renderText(paste(
            "<th bgcolor=red scope=row>",paste(round(as.numeric(
              df4$hsi_score
            ),2)),paste("</th>")
          ))
          
        } else if (df4$hsi_score >= 0.67 &
                   df4$hsi_score  < 0.88)  {
          output$hsiscore = renderText(paste(
            "<th bgcolor=yellow scope=row>",paste(round(as.numeric(
              df4$hsi_score
            ),2)),paste("</th>")
          ))
          
        } else if (df4$hsi_score  >= 0.88)  {
          output$hsiscore = renderText(paste(
            "<th bgcolor=green scope=row>",paste(round(as.numeric(
              df4$hsi_score
            ),2)),paste("</th>")
          ))
          
        }
        
        if (df4$iptv_score  > 0.0 & df4$iptv_score  < 0.67)  {
          output$iptvscore = renderText(paste(
            "<th bgcolor=red scope=row>",paste(round(as.numeric(
              df4$iptv_score
            ),2)),paste("</th>")
          ))
          
        } else if (df4$iptv_score >= 0.67 &
                   df4$iptv_score  < 0.88)  {
          output$iptvscore = renderText(paste(
            "<th bgcolor=yellow scope=row>",paste(round(as.numeric(
              df4$iptv_score
            ),2)),paste("</th>")
          ))
          
        } else if (df4$iptv_score  >= 0.88)  {
          output$iptvscore = renderText(paste(
            "<th bgcolor=green scope=row>",paste(round(as.numeric(
              df4$iptv_score
            ),2)),paste("</th>")
          ))
          
        }
        
        overallscore = sum(round(as.numeric(
          str_replace(df4$vobb_score , "\\[1] ", "")
        ),2),round(as.numeric(
          str_replace(df4$hsi_score , "\\[1] ", "")
        ),2),round(as.numeric(
          str_replace(df4$iptv_score , "\\[1] ", "")
        ) / 5,2))
        
        overallscore <- overallscore / 3
        
        print(overallscore)
        
        if (overallscore  > 0.0 & overallscore  < 0.67)  {
          output$overallscore = renderText(paste(
            "<th bgcolor=red scope=row>",paste(round(as.numeric(
              overallscore
            ),2)),paste("</th>")
          ))
          
        } else if (overallscore >= 0.67 & overallscore  < 0.88)  {
          output$overallscore = renderText(paste(
            "<th bgcolor=yellow scope=row>",paste(round(as.numeric(
              overallscore
            ),2)),paste("</th>")
          ))
          
        } else if (overallscore  >= 0.88)  {
          output$overallscore = renderText(paste(
            "<th bgcolor=green scope=row>",paste(round(as.numeric(
              overallscore
            ),2)),paste("</th>")
          ))
          
        }
        
        
        output$maps <- tryCatch({
          #print(lon)
          
          
          lon <- data.frame(df3$long)
          lat <- data.frame(df3$lat)
          f <- data.frame(df3$exchange)
          mydf <- as.data.frame(cbind(lon,lat,f))
          names(mydf) = c("lon","lat","f")
          cbj <-
            get_map(location = c(
              lon = mean(mydf$lon), lat = mean(mydf$lat)
            ), zoom = 12)
          renderPlot(
            ggmap(cbj) + geom_point(
              data = mydf, aes(x = lon, y = lat),  alpha = .5 , col = "red" , size = 5
            )
            +  geom_text(
              data = mydf , aes(
                x = lon, y = lat , label = f, size = 3, vjust = 0, hjust = -0.5
              )
            )
            +  ggtitle("Exchange Location in MSC Zone")
          )
          
        })
        
        df3
        
        #df1[c(1,3,4,5,6)]
      }, options = list(
        lengthMenu = c(5, 10, 15,20), pageLength = 5, bAutoWidth = TRUE ,scrollX = TRUE,scrolly = TRUE
      ))
    }
    else
    {
      if (paste(input$user) != "ALL CUSTOMER" &
          paste(input$exchange) == "ALL EXCHANGE") {
        print("masukkkk !!")
        
        output$selcustomer <- renderText({
          paste(input$user)
        },quoted =)
        TRUE
        
        
        j <- read.csv("hsi_customer.csv")
        j <- filter(j, j$login_id  == paste(input$user))
        print(j)
        output$hsi <-
          tryCatch({
            renderPlot(
              barplot(
                j$total, names.arg = j$dt, ylim = c(0,50),col = "#A4CACC", ylab = "Total termination", xlab =
                  "date"
              )
            )
          })
        
        
        output$cei_table = renderDataTable({
          df2 <-
            read.csv("account.csv")
          df1 <-
            read.csv("CEI_perUser_Score.csv")
          df3 <-
            merge(x = df1, y = df2, by = "login_id")
          
          
          df3 <- filter(df3, login_id == paste(input$user))
          names(df3) = c(names(df1),names(df2[2:9]))
          
          #df3 <- as.data.frame(df3)
          #df3 <- cbind(names(df3))
          
          print(df3$login_id)
          
          output$fdc = renderText(paste(df3$FDC))
          output$dn = renderText(paste(df3$devicename))
          output$reffdc = renderText(paste(df3$REF_FDC))
          output$model = renderText(paste(df3$vendor))
          
          output$name = renderText(paste(df3$account_name))
          output$package = renderText(paste(df3$package_name))
          output$address = renderText(
            paste(
              str_replace(df3$street_name , "NULL", ""),str_replace(df3$section_name , "NULL", ""),
              str_replace(df3$postal_code , "NULL", ""),str_replace(df3$city_name , "NULL", ""), str_replace(df3$state_code , "NULL", "")
            )
          )
          
          output$maps <- tryCatch({
            
            df2 <-
              read.csv("account.csv")
            df1 <-
              read.csv("CEI_perUser_Score.csv")
            df3 <-
              merge(x = df1, y = df2, by = "login_id", all.x = TRUE)
            
            
            df3 <- filter(df3, login_id == paste(input$user))
            names(df3) = c(names(df1),names(df2[2:9]))
            
            if ( is.na(as.numeric(df3$LAT)) & is.na(as.numeric(df3$LONG)) )
            {
              
              if (as.character(df3$section_name) == "NULL")
              {
                print(as.character(df3$section_name))
                
                gc <- geocode(paste(str_replace(df3$zone , "ZONE", "")), source = "google")
                #lon <- data.frame(gc$lon)
                #lat <- data.frame(gc$lat)
                
                lon <- data.frame(gc$lon)
                lat <- data.frame(gc$lat)
                f <- data.frame(paste(df3$account_name))
                mydf <- as.data.frame(cbind(lon,lat,f))
                mydf <- as.data.frame(cbind(lon,lat,f))
                names(mydf) = c("lon","lat","f")
                
                print(mydf)
                cbj <-
                  get_map(location = paste(str_replace(df3$zone , "ZONE", "")) , zoom = 12)
                renderPlot(ggmap(cbj) + geom_point(
                  data = mydf, aes(x = lon, y = lat),  alpha = .5 , col = "red" , size = 5
                )+  geom_text(
                  data = mydf , aes(
                    x = lon, y = lat , label = f, size = 3, vjust = 0, hjust = -0.3
                  )))
              } else
              {
                print("ada alamat")
                print(as.character(df3$section_name))
                
                gc <-
                  geocode(paste(df3$street_name, df3$section_name, df3$city_name), source = "google")
                
                lon <- data.frame(gc$lon)
                lat <- data.frame(gc$lat)
                f <- data.frame(paste(df3$account_name))
                mydf <- as.data.frame(cbind(lon,lat,f))
                mydf <- as.data.frame(cbind(lon,lat,f))
                names(mydf) = c("lon","lat","f")
                
                print(mydf)
                cbj <-
                  get_map(location = paste(df3$section_name) , zoom = 12)
                renderPlot(ggmap(cbj) + geom_point(
                  data = mydf, aes(x = lon, y = lat),  alpha = .5 , col = "red" , size = 5
                )+  ggtitle("Unifi Customer Location") +  geom_text(
                                   data = mydf , aes(
                                     x = lon, y = lat , label = f, size = 3, vjust = 0, hjust = -0.3
                                   )))
                  
                
              }
             
             # if (df3$street_name == "")
             # {
                
#                 cbj <-
#                   get_map(location = paste(
#                     str_replace(df3$zone , "ZONE", "")
#                   ) , zoom = 12)
#                 renderPlot(
#                   ggmap(cbj)) 
             # }
            }
            
         
              
#               + geom_point(
#                 data = mydf, aes(x = lon, y = lat),  alpha = .5 , col = "red" , size = 5
#               )
#               +  geom_text(
#                 data = mydf , aes(
#                   x = lon, y = lat , label = f, size = 3, vjust = 0, hjust = -0.5
#                 )
#               )
#               +  ggtitle("Exchange Location in MSC Zone")
         #   )
            
          })
          
          #ggmap(get_map(location = "P9C/1 PRESINT 9 PUTRAJAYA ", zoom = 15 , maptype = "roadmap" ),legend = "right",extent = "panel", height = 600, width = 1800 )+ ggtitle("FDC Location in Google Maps")
          
          #P9C/1 PRESINT 9 62250 PUTRAJAYA WIL
          
          #              df3 <-
          #               aggregate(
          #                 cbind(
          #                   df3$voice_score,df3$internet_score,df3$iptv_score / 5,df3$LONG,df3$LAT
          #                 ) ~ df3$exchange , data = df3, mean , na.rm = TRUE
          #               )
          #
          #             names(df3) <-
          #               c("exchange","vobb_score","hsi_score","iptv_score","long","lat")
          
          #print(df3)
          
          #df3 <- colMeans(df3[2:6])
          #df3 <- as.data.frame(t(melt(df3)))
          
          print(df3)
          
          if (as.numeric(str_replace(df3$voice_score , "\\[1] ", ""))  > 0.0 &
              as.numeric(str_replace(df3$voice_score , "\\[1] ", ""))  < 0.67)  {
            output$vobbscore = renderText(paste(
              "<th bgcolor=red scope=row>",paste(round(as.numeric(
                str_replace(df3$voice_score , "\\[1] ", "")
              ),2)),paste("</th>")
            ))
            
          } else if (as.numeric(str_replace(df3$voice_score , "\\[1] ", ""))  >= 0.67 &
                     as.numeric(str_replace(df3$voice_score , "\\[1] ", ""))  < 0.88)  {
            output$vobbscore = renderText(paste(
              "<th bgcolor=yellow scope=row>",paste(round(as.numeric(
                str_replace(df3$voice_score , "\\[1] ", "")
              ),2)),paste("</th>")
            ))
            
          } else if (as.numeric(str_replace(df3$voice_score , "\\[1] ", ""))  >= 0.88)  {
            output$vobbscore = renderText(paste(
              "<th bgcolor=green scope=row>",paste(round(as.numeric(
                str_replace(df3$voice_score , "\\[1] ", "")
              ),2)),paste("</th>")
            ))
          }
          
          if (as.numeric(str_replace(df3$internet_score , "\\[1] ", ""))  > 0.0 &
              as.numeric(str_replace(df3$internet_score , "\\[1] ", ""))  < 0.67)  {
            output$hsiscore = renderText(paste(
              "<th bgcolor=red scope=row>",paste(round(as.numeric(
                str_replace(df3$internet_score , "\\[1] ", "")
              ),2)),paste("</th>")
            ))
            
          } else if (as.numeric(str_replace(df3$internet_score , "\\[1] ", ""))  >= 0.67 &
                     as.numeric(str_replace(df3$internet_score , "\\[1] ", ""))  < 0.88)  {
            output$hsiscore = renderText(paste(
              "<th bgcolor=yellow scope=row>",paste(round(as.numeric(
                str_replace(df3$internet_score , "\\[1] ", "")
              ),2)),paste("</th>")
            ))
            
          } else if (as.numeric(str_replace(df3$internet_score , "\\[1] ", ""))  >= 0.88)  {
            output$hsiscore = renderText(paste(
              "<th bgcolor=green scope=row>",paste(round(as.numeric(
                str_replace(df3$internet_score , "\\[1] ", "")
              ),2)),paste("</th>")
            ))
          }
          
          if (as.numeric(str_replace(df3$iptv_score , "\\[1] ", ""))  > 0.0 &
              as.numeric(str_replace(df3$iptv_score , "\\[1] ", ""))  < 0.67)  {
            output$iptvscore = renderText(paste(
              "<th bgcolor=red scope=row>",paste(round(
                as.numeric(str_replace(df3$iptv_score , "\\[1] ", "")) / 5,2
              )),paste("</th>")
            ))
            
          } else if (as.numeric(str_replace(df3$iptv_score , "\\[1] ", ""))  >= 0.67 &
                     as.numeric(str_replace(df3$iptv_score , "\\[1] ", ""))  < 0.88)  {
            output$iptvscore = renderText(paste(
              "<th bgcolor=yellow scope=row>",paste(round(
                as.numeric(str_replace(df3$iptv_score , "\\[1] ", "")) / 5,2
              )),paste("</th>")
            ))
            
          } else if (as.numeric(str_replace(df3$iptv_score , "\\[1] ", ""))  >= 0.88)  {
            output$iptvscore = renderText(paste(
              "<th bgcolor=green scope=row>",paste(round(
                as.numeric(str_replace(df3$iptv_score , "\\[1] ", "")) / 5,2
              )),paste("</th>")
            ))
          }
          
          overallscore = sum(round(as.numeric(
            str_replace(df3$voice_score , "\\[1] ", "")
          ),2),round(as.numeric(
            str_replace(df3$internet_score , "\\[1] ", "")
          ),2),round(as.numeric(
            str_replace(df3$iptv_score , "\\[1] ", "")
          ) / 5,2))
          
          overallscore <- overallscore / 3
          
          print(overallscore)
          
          
          if (overallscore  > 0.0 & overallscore  < 0.67)  {
            output$overallscore = renderText(paste(
              "<th bgcolor=red scope=row>",paste(round(as.numeric(
                overallscore
              ),2)),paste("</th>")
            ))
            
          } else if (overallscore >= 0.67 &
                     overallscore  < 0.88)  {
            output$overallscore = renderText(paste(
              "<th bgcolor=yellow scope=row>",paste(round(as.numeric(
                overallscore
              ),2)),paste("</th>")
            ))
            
          } else if (overallscore  >= 0.88)  {
            output$overallscore = renderText(paste(
              "<th bgcolor=green scope=row>",paste(round(as.numeric(
                overallscore
              ),2)),paste("</th>")
            ))
            
          }
          
          
          
          #print("593")
          df3
          
        }, options = list(
          lengthMenu = c(5, 10, 15,20), pageLength = 5, bAutoWidth = TRUE ,scrollX = TRUE,scrolly = TRUE
        ))
        
        
      } else
      {
        if (paste(input$user) == "ALL CUSTOMER" &
            paste(input$exchange) != "ALL EXCHANGE") {
          output$cei_table = renderDataTable({
            df2 <-    read.csv("account.csv")
            df1 <-
              read.csv("CEI_perUser_Score.csv")
            df3 <-
              merge(
                x = df1, y = df2, by = "login_id", all.x = FALSE , all.y = FALSE
              )
            df3 <-
              aggregate(
                cbind(
                  df3$voice_score,df3$internet_score,df3$iptv_score,df3$LONG,df3$LAT
                ) ~ df3$exchange , data = df3, mean , na.rm = TRUE
              )
            names(df3) <-
              c("exchange","vobb_score","hsi_score","iptv_score","long","lat")
            
            
            df3 <- filter(df3, exchange == paste(input$exchange))
            print(df3)
            
            
            if (as.numeric(str_replace(df3$vobb_score , "\\[1] ", ""))  > 0.0 &
                as.numeric(str_replace(df3$vobb_score , "\\[1] ", ""))  < 0.67)  {
              output$vobbscore = renderText(paste(
                "<th bgcolor=red scope=row>",paste(round(
                  as.numeric(str_replace(
                    df3$vobb_score , "\\[1] ", ""
                  )),2
                )),paste("</th>")
              ))
              
            } else if (as.numeric(str_replace(df3$vobb_score , "\\[1] ", ""))  >= 0.67 &
                       as.numeric(str_replace(df3$vobb_score , "\\[1] ", ""))  < 0.88)  {
              output$vobbscore = renderText(paste(
                "<th bgcolor=yellow scope=row>",paste(round(
                  as.numeric(str_replace(
                    df3$vobb_score , "\\[1] ", ""
                  )),2
                )),paste("</th>")
              ))
              
            } else if (as.numeric(str_replace(df3$vobb_score , "\\[1] ", ""))  >= 0.88)  {
              output$vobbscore = renderText(paste(
                "<th bgcolor=green scope=row>",paste(round(
                  as.numeric(str_replace(
                    df3$vobb_score , "\\[1] ", ""
                  )),2
                )),paste("</th>")
              ))
            }
            
            if (as.numeric(str_replace(df3$hsi_score , "\\[1] ", ""))  > 0.0 &
                as.numeric(str_replace(df3$hsi_score , "\\[1] ", ""))  < 0.67)  {
              output$hsiscore = renderText(paste(
                "<th bgcolor=red scope=row>",paste(round(
                  as.numeric(str_replace(df3$hsi_score , "\\[1] ", "")),2
                )),paste("</th>")
              ))
              
            } else if (as.numeric(str_replace(df3$hsi_score , "\\[1] ", ""))  >= 0.67 &
                       as.numeric(str_replace(df3$hsi_score , "\\[1] ", ""))  < 0.88)  {
              output$hsiscore = renderText(paste(
                "<th bgcolor=yellow scope=row>",paste(round(
                  as.numeric(str_replace(df3$hsi_score , "\\[1] ", "")),2
                )),paste("</th>")
              ))
              
            } else if (as.numeric(str_replace(df3$hsi_score , "\\[1] ", ""))  >= 0.88)  {
              output$hsiscore = renderText(paste(
                "<th bgcolor=green scope=row>",paste(round(
                  as.numeric(str_replace(df3$hsi_score , "\\[1] ", "")),2
                )),paste("</th>")
              ))
            }
            
            if (as.numeric(str_replace(df3$iptv_score , "\\[1] ", ""))  > 0.0 &
                as.numeric(str_replace(df3$iptv_score , "\\[1] ", ""))  < 0.67)  {
              output$iptvscore = renderText(paste(
                "<th bgcolor=red scope=row>",paste(round(
                  as.numeric(str_replace(
                    df3$iptv_score , "\\[1] ", ""
                  )) / 5,2
                )),paste("</th>")
              ))
              
            } else if (as.numeric(str_replace(df3$iptv_score , "\\[1] ", ""))  >= 0.67 &
                       as.numeric(str_replace(df3$iptv_score , "\\[1] ", ""))  < 0.88)  {
              output$iptvscore = renderText(paste(
                "<th bgcolor=yellow scope=row>",paste(round(
                  as.numeric(str_replace(
                    df3$iptv_score , "\\[1] ", ""
                  )) / 5,2
                )),paste("</th>")
              ))
              
            } else if (as.numeric(str_replace(df3$iptv_score , "\\[1] ", ""))  >= 0.88)  {
              output$iptvscore = renderText(paste(
                "<th bgcolor=green scope=row>",paste(round(
                  as.numeric(str_replace(
                    df3$iptv_score , "\\[1] ", ""
                  )) / 5,2
                )),paste("</th>")
              ))
            }
            
            overallscore = sum(round(as.numeric(
              str_replace(df3$vobb_score , "\\[1] ", "")
            ),2),round(as.numeric(
              str_replace(df3$hsi_score , "\\[1] ", "")
            ),2),round(as.numeric(
              str_replace(df3$iptv_score , "\\[1] ", "")
            ) / 5,2))
            
            overallscore <- overallscore / 3
            
            print(overallscore)
            
            
            if (overallscore  > 0.0 & overallscore  < 0.67)  {
              output$overallscore = renderText(paste(
                "<th bgcolor=red scope=row>",paste(round(
                  as.numeric(overallscore),2
                )),paste("</th>")
              ))
              
            } else if (overallscore >= 0.67 &
                       overallscore  < 0.88)  {
              output$overallscore = renderText(paste(
                "<th bgcolor=yellow scope=row>",paste(round(
                  as.numeric(overallscore),2
                )),paste("</th>")
              ))
              
            } else if (overallscore  >= 0.88)  {
              output$overallscore = renderText(paste(
                "<th bgcolor=green scope=row>",paste(round(
                  as.numeric(overallscore),2
                )),paste("</th>")
              ))
              
            }
            
            
            j <- read.csv("hsi.csv")
            j <- filter(j, j$msan  == paste(input$exchange))
            print(j)
            output$hsi_all <-
              tryCatch({
                renderPlot(
                  barplot(
                    j$total, names.arg = j$dt, ylim = c(0,10000),col = c("#8199A7","#42616E","#A4CACC"),
                    ylab = "Termination", xlab =
                      "date"
                  )
                )
              })
            
            print("----------------")
            
            
            output$maps <- tryCatch({
              #print(lon)
              
              
              lon <- data.frame(df3$long)
              lat <- data.frame(df3$lat)
              f <- data.frame(df3$exchange)
              mydf <- as.data.frame(cbind(lon,lat,f))
              names(mydf) = c("lon","lat","f")
              cbj <-
                get_map(location = c(
                  lon = mean(mydf$lon), lat = mean(mydf$lat)
                ), zoom = 12)
              renderPlot(
                ggmap(cbj) + geom_point(
                  data = mydf, aes(x = lon, y = lat),  alpha = .5 , col = "red" , size = 5
                )
                +  geom_text(
                  data = mydf , aes(
                    x = lon, y = lat , label = f, size = 3, vjust = 0, hjust = -0.5
                  )
                )
                +  ggtitle("Exchange Location in MSC Zone")
              )
              
            })
            
            
            df3
            
          }, options = list(
            lengthMenu = c(5, 10, 15,20), pageLength = 5, bAutoWidth = TRUE ,scrollX = TRUE,scrolly = TRUE
          ))
          
        }
      }
      
      
      
      
      
      
      
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
  
  
  render_hsi_single_customer <- function() {
    #data <- read.csv("http://s3.amazonaws.com/csvpastebin/uploads/86a7d7c118b9be49adc14f1d64565a4e/lostcarrier.csv")
    #data <- sample(1:10, 1000,replace = TRUE)
    #hist(data , col = "#8199A7")
  }
  
  render_vobb_single_customer <- function() {
    data1 <- sample(1:10, 1000,replace = TRUE)
    hist(data1,col = "#A4CACC",main = "")
  }
  
  render_iptv_single_customer <- function() {
    data2 <- sample(1:10, 1000,replace = TRUE)
    hist(data2,col = "#42616E",main = "")
  }
  
  
  #  render_hsi_all_customer <- function() {
  # data3 <- sample(1:1000, 1000,replace = TRUE)
  # hist(
  #  data3,col = "#8199A7",main = "",xlim = c(0,1000),las = 1,breaks = 200
  # )
  
  #     j <- read.csv("hsi.csv")
  #     j <- filter(j, j$msan == "CBJ")
  #     ur <- barplot(j$total, names.arg=j$dt, ylim=c(0,10000),col=c("#8199A7","#42616E","#A4CACC"),
  #                   ylab="Termination", xlab="date")
  #     output$hsi_all <- renderPlot(ur)
  
  #  }
  
  render_vobb_all_customer <- function() {
    data4 <- sample(1:1000, 1000,replace = TRUE)
    hist(
      data4,col = "#A4CACC",main = "",xlim = c(0,1000),las = 1,breaks = 200
    )
  }
  
  render_iptv_all_customer <- function() {
    data5 <- sample(1:1000, 1000,replace = TRUE)
    hist(
      data5,col = "#42616E",main = "",xlim = c(0,1000),las = 1,breaks = 200
    )
  }
  
  
  # This funtion will render the Vaping PowerCharts
  
  render_googlemaps <- function() {
    df1 <-
      read.csv("MSC_FDC_DATA.csv")
    df2 <- as.data.frame(df1)
    df2 <- df2[df2$EXC_ABB == 'CBJ2',]
    map1  <-
      get_map(
        location = c(lon = mean(df1$LONG), lat = mean(df1$LAT)), zoom = 13 , maptype = "roadmap", scale = 2
      )
    #p <- ggmap(map) +  geom_point(data = df, aes(x = lon, y = lat, fill = "red", alpha = 0.1), size = 5, shape = 21) + geom_text(data = df, aes(x = lon, y = lat, label = paste(OLT_CODE,FDC_CODE), size = 3, vjust = 0, hjust = -0.5))
    p <- ggmap(map1,legend = "right",extent = "panel")
    p
  }
  
  
  
  output$cei_table = renderDataTable({
    df1 <-
      read.csv("CEI_perUser_Score.csv")
    #df1[c(1,3,4,5,6)]
  }, options = list(
    lengthMenu = c(5, 10, 15,20), pageLength = 5, bAutoWidth = TRUE ,scrollX = TRUE,scrolly = TRUE
  ))
  
  
  
  
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
    address <-
      c(
        "DPulze Cyberjaya Block D, Cyberjaya CBD, Persiaran Multimedia, Cyber 12, 63000 Cyberjaya, Selangor Darul Ehsan",
        "Cyberjaya Campus: Jalan Multimedia, 63100 Cyberjaya, Selangor, Malaysia",
        "Persiaran Rimba Permai, Cyber 8 (Persiaran Ceria), 63000 Cyberjaya"
      )
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
  
  #output$hsi <- renderPlot(render_hsi_single_customer())
  
  output$vobb <- renderPlot(render_vobb_single_customer())
  
  output$iptv <- renderPlot(render_iptv_single_customer())
  
  #output$hsi_all <- renderPlot(render_hsi_all_customer())
  
  output$vobb_all <- renderPlot(render_vobb_all_customer())
  
  output$iptv_all <- renderPlot(render_iptv_all_customer())
  
  #output$maps <- renderPlot(render_googlemaps())
  
  output$plot <- renderPlot(render_powercharts())
  
  
  
})