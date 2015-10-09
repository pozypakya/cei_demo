library(shiny)
library(data.table)
library(plotrix)
library(shinythemes)
library(reshape2)


shinyUI(fluidPage(#theme = shinytheme("cerulean"),
  titlePanel(HTML('<b><font size="6"color="#42616E">Customer Experience Index ( CEI ) 1.0</font></b>')),
  fluidRow(
    column(4, wellPanel(HTML('<p><img width=30% height=30% src="tm.gif"/></p>'),
                                                
      htmlOutput("caption"),
      selectInput('zone', 'Zone :', choices=c("ZONE CYBERJAYA")),
      selectInput('exchange', 'Exchange :', choices=c("ALL EXCHANGE")),
      selectInput("user", "User :",choices=c("ALL CUSTOMER")),
      
      htmlOutput("test"),
      
      HTML('<b>Customer</b><br>'),br(),
      HTML('Name : '),htmlOutput("name"),br(),
      HTML('Address : '),htmlOutput("address"),br(),
      HTML('Package : '),htmlOutput("package"),br()
    ),
    wellPanel(
      HTML('<b>Device</b><br>'),br(),
      HTML('Device Name : '),htmlOutput("dn"),br(),
      HTML('FDC Name : '),htmlOutput("fdc"),br(),
      HTML('REF FDC : '),htmlOutput("reffdc"),br(),
      HTML('Model : '),htmlOutput("model"),br()
    ),
    wellPanel(HTML('<b>HSI History for xyz@unifi</b>'),plotOutput("hsi", width = "100%", height = "400px")),
    wellPanel(HTML('<b>VOBB History for xyz@unifi</b>'),plotOutput("vobb", width = "100%", height = "400px")),
    wellPanel(HTML('<b>IPTV History for xyz@unifi</b>'),plotOutput("iptv", width = "100%", height = "400px"))
    )
    ,
    column(8,
           #wellPanel(HTML('<b>Customer\'s Experience Index</b>'),br(),br(),HTML('The Triple Play Experience Index assess key system components on a scale of 1.0 to 5.0')),
           #wellPanel(HTML('<b>Status</b><br><br>'),HTML('<img width=5% height=5% src="cau.png"/>&nbsp&nbsp New data available . Your experience index needs to be refresh.&nbsp&nbsp&nbsp&nbsp'), actionButton("refresh", "Refresh")),
           wellPanel(HTML('<b>Score</b><br><br>'),
           HTML('<table width="100%" border="1">  <tr><th>Component</th><th>What  is rated</th><th>Subscore</th><th>Best  Score</th>  </tr>  <tr><th >
                High  Speed Internet</th><td>Throughput,  latency per second</td><td  ><p>'),htmlOutput("hsiscore"),HTML('</p></td><td rowspan="3" 
                ><b><font size="5" >'),htmlOutput("overallscore"),HTML('</font></b></td>  </tr>  <tr><th >Voice  over Broadband
                </th><td>Mean  opinion Score (MOS)</td><td  ><p>'),htmlOutput("vobbscore"),HTML('</p></td>  </tr>  <tr><th >HyppTV</th>
                <td>Video  Mean Opinion Score, vMOS</td><td  ><p>'),htmlOutput("iptvscore"),HTML('</p></td>  </tr></table>')),
           wellPanel(HTML('<b>DataSet</b><br><br>'),div(dataTableOutput("cei_table"), style = "font-size:80%")),
           wellPanel(
           HTML('<b>Maps</b><br><br>'),
           HTML('<tr><th scope="row">'),plotOutput("maps", width = "100%", height = "600px"),HTML('</th></tr>')),
           wellPanel(HTML('<b>Performance</b>'),
                     wellPanel(HTML('<b>HSI</b>'),plotOutput("hsi_all", width = "100%", height = "400px")),br(),
                     wellPanel(HTML('<b>VOBB</b>'),plotOutput("vobb_all", width = "100%", height = "400px")),br(),
                     wellPanel(HTML('<b>IPTV</b>'),plotOutput("iptv_all", width = "100%", height = "400px")),br()
                     ),
           br(),
           br()
           
    )
  )
))