library(shiny)
library(data.table)
library(plotrix)
library(shinythemes)
library(reshape2)


shinyUI(fluidPage(#theme = shinytheme("flatly"),
  titlePanel(
    HTML(
      '<b><font face="Tahoma, Geneva, sans-serif" size="6"color="#42616E">TM Customer Experience Index (CEI)</font></b>'
    )
  ),
  fluidRow(
    column(
      4, wellPanel(
        HTML('<p><img width=30% height=30% src="tm.gif"/></p>'),
        
        htmlOutput("caption"),
        selectInput('zone', 'Zone :', choices = c("ZONE CYBERJAYA")),
        selectInput('exchange', 'Exchange :', choices =
                      c("ALL EXCHANGE")),
        selectInput("user", "User :",choices = c("ALL CUSTOMER")),
        
        htmlOutput("test")
      ),
      wellPanel(
        HTML('<b>Customer</b>'),br(),
        HTML('Name : '),textOutput("name",inline = TRUE),br(),
        HTML('Address : '),textOutput("address",inline = TRUE),br(),
        HTML('Package : '),textOutput("package",inline = TRUE),br()
      ),
      wellPanel(
        HTML('<b>Device</b>'),br(),
        HTML('Device Name : '),textOutput("dn",inline = TRUE),br(),
        HTML('FDC Name : '),textOutput("fdc",inline = TRUE),br(),
        HTML('REF FDC : '),textOutput("reffdc",inline = TRUE),br(),
        HTML('Model : '),textOutput("model",inline = TRUE),br()
      ),
      wellPanel(
        HTML('<b>HSI Trending for </b>'),textOutput("selcustomer",inline = TRUE),plotOutput("hsi", width = "100%", height = "400px")
      ),
      wellPanel(
        HTML('<b>VOBB Trending for </b>'),plotOutput("vobb", width = "100%", height = "400px")
      ),
      wellPanel(
        HTML('<b>IPTV Trending for </b>'),plotOutput("iptv", width = "100%", height = "400px")
      )
    )
    ,
    column(
      8,
      #wellPanel(HTML('<b>Customer\'s Experience Index</b>'),br(),br(),HTML('The Triple Play Experience Index assess key system components on a scale of 1.0 to 5.0')),
      #wellPanel(HTML('<b>Status</b><br><br>'),HTML('<img width=5% height=5% src="cau.png"/>&nbsp&nbsp New data available . Your experience index needs to be refresh.&nbsp&nbsp&nbsp&nbsp'), actionButton("refresh", "Refresh")),
      wellPanel(HTML(" Summary : { Some notes here  ....} "),br(),
        HTML('<b>Score</b><br><br>'),
        HTML(
          '<table width="100%" border="4" bordercolor=#42616E >  <tr><th>Component</th><th>What  is rated</th><th>Subscore</th><th>Best  Score</th>  </tr>  <tr><th >
          High  Speed Internet</th><td>Throughput,  latency per second</td><td  ><p>'
        ), htmlOutput("hsiscore"),HTML('</p></td><td rowspan="3"
                ><b><font size="5" >'),htmlOutput("overallscore"),HTML(
                  '</font></b></td>  </tr>  <tr><th >Voice  over Broadband
                  </th><td>Mean  opinion Score (MOS)</td><td  ><p>'
                ),htmlOutput("vobbscore"),HTML(
                  '</p></td>  </tr>  <tr><th >HyppTV</th>
                  <td>Video  Mean Opinion Score, vMOS</td><td  ><p>'
                ),htmlOutput("iptvscore"),HTML('</p></td>  </tr></table>'),br(),HTML(" Summary : { Some notes here regarding the summary ....} ")
                ),
      wellPanel(HTML(" Summary : { Some notes here  ....} "),br(),
        HTML('<b>DataSet</b><br><br>'),div(dataTableOutput("cei_table"), style = "font-size:80%")
      ),
      wellPanel(HTML(" Summary : { Some notes here  ....} "),br(),
        HTML('<b>Maps</b><br><br>'),
        HTML('<tr><th scope="row">'),plotOutput("maps", width = "100%", height = "600px"),HTML('</th></tr>')
      ),
      wellPanel(HTML(" Summary : { Some notes here  ....} "),br(),
        HTML('<b>Performance</b>'),
        wellPanel(
          HTML('<b>HSI</b>'),plotOutput("hsi_all", width = "100%", height = "400px")
        ),br(),
        wellPanel(HTML(" Summary : { Some notes here  ....} "),br(),
          HTML('<b>VOBB</b>'),plotOutput("vobb_all", width = "100%", height = "400px")
        ),br(),
        wellPanel(HTML(" Summary : { Some notes here  ....} "),br(),
          HTML('<b>IPTV</b>'),plotOutput("iptv_all", width = "100%", height = "400px")
        ),br()
      ),
      br(),
      br()
      
      )
  )))