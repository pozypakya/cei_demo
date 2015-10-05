library(shiny)
library(data.table)
library(plotrix)


shinyUI(fluidPage(
  HTML('<p><img width=10% height=10% src="tm.png"/></p>'),
  titlePanel("TM Customer Experience Index Demo"),
  fluidRow(
    column(4, wellPanel(
      br(),
      htmlOutput("caption"),
      selectInput("variable", "Customer Selection:",
                  list("Mr_R@unifi" = "Mr_R@unifi", 
                       "Mr_X@unifi" = "Mr_X@unifi", 
                       "Mr_Z@unifi" = "Mr_Z@unifi")),
      
      HTML('<b>Customer Info</b><br>'),
      HTML('Name : Mr_R@unifi'),br(),
      HTML('Address : Tune Hotel - DPulze Cyberjaya Block D, Cyberjaya CBD, Persiaran Multimedia, Cyber 12, 63000 Cyberjaya, Selangor Darul Ehsan'),
      br(),br(),br()
    ),
    wellPanel(
      HTML('More info'),br()
    ))
    ,
    column(8,
           HTML('<b>Customer\'s Experience Index</b>'),
           br(),
           HTML('The Triple Play Experience Index assess key system components on a scale of 1.0 to 5.0
           '),
           br(),br(),
           HTML('<p><img width=5% height=5% src="cau.png"/></p>'),
           HTML('<b>New data available. </b>'),
           HTML('<b>Your Experience Index needs to be refresh</b>'),
           br(),br(),
           HTML('<table width="100%" border="1">  <tr><th>Component</th><th>What  is rated</th><th>Subscore</th><th>Best  Score</th>  </tr>  <tr><th >High  Speed Internet</th><td>Throughput,  latency per second</td><td><p>4.5</p></td><td rowspan="3" bgcolor="#0080C0"><p><font size="10"color="#FFFFFF">4.33</font></p></td>  </tr>  <tr><th >Voice  over Broadband</th><td>Mean  opinion Score (MOS)</td><td><p>4.3</p></td>  </tr>  <tr><th >HyppTV</th><td>Video  Mean Opinion Score, vMOS</td><td><p>4.2</p></td>  </tr></table>'),
           br(),
           HTML('<b>Customer\'s OLT Location</b>'),
           HTML('<table width="100%" border="1"><tr><th scope="row">'),plotOutput("maps", width = "100%", height = "400px"),HTML('</th></tr></table>'),
           
           
           br(),

                      
           br()
           
    )
  )
))