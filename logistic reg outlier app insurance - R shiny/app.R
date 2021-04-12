  library(shiny)
  library(shinydashboard)
  library(shinythemes)
  library(shinyanimate)
  library(shinyBS)
  library(shinyjs)
  library(plotly)
  library(readxl)
  library(shinyWidgets)
  library(DT)
  library(magrittr)
  library(anomalize) #tidy anomaly detectiom
  library(tidyverse) #tidyverse packages like dplyr, ggplot, tidyr
  library(coindeskr) #bitcoin price extraction from coindesk
  library(ggthemes) 
  library(ggpubr)
  library(ggplot2)
  library(forecast)
  library(tseries)
  library(shinyBS)
  library(prophet)
  library(lubridate)
  library(dplyr)
  library(plotly)
  # library(mailR)
  library(timeDate)
  library(tidyr)
  library(gbm, quietly=TRUE)
  library(pROC, quietly=TRUE)
  library(microbenchmark, quietly=TRUE)
  library(shinycssloaders)
  library(xgboost, quietly=TRUE)
  library(randomForest)
  library(ggplot2)
  library(gridExtra)
  library(highcharter)
  library(ggplot2)
  library(plyr)
  library(ggrepel)
  library(pdp)
  library(reshape2)
  options(shiny.port = 7924)
  
  gm= tags$h3(strong("Good Morning",style="color:#116bac"))
  ga= tags$h3(strong("Good Afternoon",style="color:#116bac"))
  ge= tags$h3(strong("Good Evening",style="color:#116bac"))
  
  #===========
  ## FUNCTIONS
  #===========
  ## SIMPLE GREETING
  good_time <- function(){
    
    ## LOAD PACKAGE
    require(lubridate, quietly = T)
    
    ## ISOLATE currHour
    currhour = hour(now())
    
    ## RUN LOGIC
    if(currhour < 12){
      return(gm)
    } 
    else if(currhour >= 12 & currhour < 17){
      return(ga)
    }
    else if( currhour >= 17){
      return(ge)  
    }
  }
  
  
  
  ## STARTING LOGGED VALUE; LET'S CHANGE THAT!
  Logged = FALSE;
  
  
  #====
  # UI
  #====
  ## make login screen
  ui1 <- function(){
    
    tagList(tags$style(HTML('body {font-family:"Verdana",Georgia,Serif; background-color:#116bac}')),
            div(id="container",align="center",
                div(id = "login",
                    # make login panel
                    wellPanel(id="well",style = "overflow-y: ;width:100%;height:100%",
                              br(),
                              HTML(paste0('
                                          <h2>
                                          Hello, ', good_time(),
                                          '</h2>',
                                          '<h3>
                                          <br>You are in Admin page.</br>
                                          </h3>')
                              ),
                              br(),
                              br(),
                              tags$div(textInput("userName", "Username",width = "100%"),align="left"),
                              br(),
                              tags$div(passwordInput("passwd", "Password",width = "100%"),align="left"),
                              br(),
                              # button
                              tags$div(actionButton("Login", "Log in"),align="center"),
                              # login error message
                              tags$div(uiOutput("message"),align="center")
                    )
                    
                )
            ),
            # css for container
            tags$style(type = "text/css", 
                       "#container{
                       display: flex;
                       justify-content: center;
                       margin-top: 150px;
  }"),
            # css for login well panel
            tags$style(type="text/css", "
                   #login,{
                   font-size:14px; 
                   width: 360px;}"),
            # well panel
            tags$style(type="text/css",
                       "#well{
                   padding: 50px;
                   background: white;
                   border: 1px;
                   box-shadow: ;}"),
            # welcome text css
            tags$style(type = 'text/css',
                       "h2, h3{
                   color: #525252;}"),
            # input fields
            tags$style(type="text/css",
                       "#userName, #passwd{
                   box-shadow: none;
                   outline:none;
                   border: none;
                   padding-left: 0;
                   border-bottom: 2px solid #116bac;
                   border-radius: 0;
                   }
                   #userName:focus, #passwd:focus{
                   box-shadow: 0px 10px 10px -5px lightgray;
                   }"),
            # button css
            tags$style(type='text/css',
                       "#Login{
                   outline: none;
                   margin-left: 0px;
                   width: 100px;
                   font-size: 12pt;
                   background: transparent;
                   border: 2px solid #116bac;
                   color: #116bac;
                   border-radius: 10px;
                   transition: 0.8s ease-in-out;
                   }
                   #Login:hover{
                   background: #116bac;
                   color: white;}"),
            # error box - fadeOut animation
            tags$style(type="text/css",
                       "@-webkit-keyframes fadeOut {
                   from {
                   opacity: 1;
                   }
                   to {
                   opacity: 0;
                   }
                   }
                   @keyframes fadeOut {
                   from {
                   opacity: 1;
                   }
                   to {
                   opacity: 0;
                   }
                   }"),
            tags$style(type="text/css",
                       "#error-box{
                   margin-top: 20px;
                   margin-left: 0px;
                   padding: 5px 10px 5px 10px;
                   text-align: center;
                   width: 325px;
                   color: white;
                   background: #ef3b2c;
                   border: 1px solid #ef3b2c;
                   border-radius: 5px;
                   -webkit-animation: fadeOut;
                   animation: fadeOut;
                   opacity: 0;
                   animation-duration: 15s;}")
    )
  }
  
  #=========
  # PRINT UI
  #=========
  ui = (uiOutput("page"))
  
  #========
  # SERVER
  #========
  
  server = shinyServer(function(input, output,session){
    options(shiny.maxRequestSize=150*1024^2)
    users <- data.frame(User=c("a","b","c"),Password=c("a","b","c"))
    ## BEGIN BUILD LOG IN SCREEN
    USER <- reactiveValues(Logged = Logged)
    
    ## ERROR CHECKING
    observeEvent(input$Login,{
      
      ## output error message
      output$message <- renderUI({
        if(!is.null(input$Login)){
          my_username <- length(users$User[grep(pattern = input$userName, x = users$User)])
          my_password <- length(users$User[grep(pattern = input$passwd, x = users$Password)])
          if(input$Login > 0){
            if(my_username < 1 ||  my_password < 1){
              HTML("<div id='error-box'>
                   Sorry, that's not the right username or password. Please, 
                   try again. If you continue to have problems,
                   <a href='https://github.com/sailogeshh'>
                   <u>Contact ..</u></a>
                   </div>")
            }
          }
        }
      })
      
      ## CHECK INPUT
      if (USER$Logged == FALSE) {
        if (!is.null(input$Login)) {
          if (input$Login > 0) {
            Username <- isolate(input$userName)
            Password <- isolate(input$passwd)
            Id.username <- which(users$User == Username)
            Id.password <- which(users$Password == Password)
            if (length(Id.username) > 0 & length(Id.password) > 0) {
              if (Id.username %in% Id.password) {
                USER$Logged <- TRUE
              }
            }
          }
        }
      }
    })
    
    ## Make UI
    observe({
      # What to do when logged = F
      if (USER$Logged == FALSE) {
        output$page <- renderUI({
          div(class="outer",do.call(bootstrapPage,c("",ui1())))
        })
      }
      
      # Render UI when logged = T
      if (USER$Logged == TRUE) 
      {
        ## get the current user's authorization level 
        user_log <- toupper(input$userName)
        
        # if admin ("input.SELECT == 1 || input.FED == 2" )
        if(user_log == "A" ){
          output$page <- renderUI({
            ###################################################### ADMIN UI PAGE ###################################################################################################################
            fluidPage(
              
              
              theme = shinytheme("simplex"),
              tagList(
                useShinyjs(),
                tags$style(HTML("
                                
                                
                                .navbar  {
                                background-color:white; }
                                
                                .navbar .navbar-nav {float: left; 
                                margin-top: 32px;
                                color: #; 
                                font-size: 20px; 
                                background-color: #; }
                                
                                .navbar.navbar-default.navbar-static-top{ 
                                color: #; 
                                font-size: 23px; 
                                background-color: # ;}
                                
                                .navbar .navbar-header {
                                float: left;
                                
                                background-color: # ;}
                                
                                .navbar-default .navbar-brand { color: #054b94; 
                                font-size: 28px; 
                                margin-bottom:32px;
                                background-color: # ;} 
                                
                                ")),
                
                
                tags$head(HTML("<title>app</title> <link rel='icon' type='image/gif/png' href='log.png'>")),
                
                navbarPage(id="tabs",
                           
                           title = tags$div(img(src="","Outlier Detection", style="color:white;font-weight:200%;margin-top: -5px;margin-left: 30px;", height = 60)),position = "fixed-top",
                           selected = tags$div(bsButton("dummy0",strong("Upload"),style = "danger",size="small"),style="color:white;margin-top: -22px;font-weight:100%;",align="center"),inverse = F,
                           
                           tabPanel(title = tags$div(bsButton("dummy0",strong("Upload"),style = "danger",size="small"),style="color:white;margin-top: -22px;font-weight:100%;",align="center"),
                                    
                                    fluidPage(
                                      
                                      tags$style(" #modal1 .modal-header {background-color:#; border-top-left-radius: 0px; border-top-right-radius: 0px}
                                                 #modal1 .modal-dialog { width: 1800px;}
                                                 #modal1 .modal-content  {background-color:#;}"), 
                                      
                                      tags$style(type="text/css",
                                                 ".shiny-output-error { visibility: hidden; }",
                                                 ".shiny-output-error:before { visibility: hidden; }"
                                      ),
                                      tags$head(tags$style("#pppp{color:black; font-size:35px; font-style:italic; text-align=center;
                                                           overflow-y:scroll; max-height: 300px; background: ghostwhite;}")),
                                      tags$head(tags$style("#roi{color:black; font-size:35px; font-style:italic; text-align=center;
                                                           overflow-y:scroll; max-height: 300px; background: ghostwhite;}")),
                                      
                                      
                                      br(),
                                      br(),
                                      br(),
                                      
                                      column(6,
                                             
                                             br(),br(),br(),br(),br(),br(),br(),
                                             tags$div(id = 'logo1',img(src="t.png",height='100%',width='100%'),align="center")
                                      ),
                                      
                                      br(),
                                      br(),
                                      
                                      column(6,
                                             
                                             
                                             bootstrapPage(
                                               
                                               br(),
                                               tags$div(id = 'logo2',img(src="logo.png",height='40%',width='40%'),align="center"),br(),
                                               
                                               tags$div(h4(strong(em("Outlier Analysis")),style="color:#2e5cb8;font-size:200%"),align="center"),
                                               
                                               
                                               withAnim(),
                                               br(),
                                               uiOutput('fileupload'),
                                               #uiOutput("bss"),br(),
                                               uiOutput('checkbox'),
                                               uiOutput("button"),
                                               uiOutput("helptext"),
                                               br(),
                                               br(),
                                               bsPopover(id = "dummy000",title = "Note:",content = "XXX",placement = "right"),
                                               bsPopover(id="check",title = "",content = "Note: I accept Terms & Conditions.. Show the Analyse button",placement = "right"),
                                               tags$div(bsButton("reset", label = "Reset ?", icon =   icon("repeat",lib = "glyphicon"),block = F, style="danger",size = "small"),align="center"),
                                               
                                               
                                               #tags$h1(actionButton("myuser","Logout",icon=icon("user")),style="text-align:center"),
                                               br(),
                                               
                                               tags$div(class = "header", checked = NA,style="text-align:center;color:#929292;font-size:100%",
                                                        tags$tbody("Need Help ?"),
                                                        tags$a(href = "https://github.com/sailogeshh", "Contact ", target="_blank")
                                               ),tags$div(actionLink("reset2",""),align="center"),
                                               br()
                                             )
                                      )
                                      
                                      
                                      
                                    )),
                           
                           
                           
                           tabPanel(value = "mytab2",
                                    
                                    title = tags$div(bsButton("dummy",strong("Result..!"),style = "primary",size="small"),style="color:white;margin-top: -22px;font-weight:100%;",align="center"),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    tabsetPanel(type = "tabs", selected = tags$div(h4(strong("Outlier Plot"),style="color:#2e5cb8;font-size:120%"),align="center"),
                                                # tabPanel(tags$div(h4(strong("Predicted"),style="color:#2e5cb8;font-size:120%"),align="center"), uiOutput("down"),
                                                #          dataTableOutput('predict',height = "1200px")),
                                                #tabPanel(tags$div(h4(strong("Estimated coefficients"),style="color:#2e5cb8;font-size:120%"),align="center"),withSpinner( plotOutput('one',height = "800px"))),
                                                #tabPanel(tags$div(h4(strong("Odds ratios"),style="color:#2e5cb8;font-size:120%"),align="center"),withSpinner(  plotOutput('two',height = "800px"))),
                                                tabPanel(tags$div(h4(strong("Accuracy plot"),style="color:#2e5cb8;font-size:120%"),align="center"),withSpinner(  plotOutput('three',height = "800px"))),
                                                #tabPanel(tags$div(h4(strong("Standardized residuals"),style="color:#2e5cb8;font-size:120%"),align="center"),withSpinner(  plotOutput('four',height = "800px"))),
                                                tabPanel(tags$div(h4(strong("Outlier Plot"),style="color:#2e5cb8;font-size:120%"),align="center"),withSpinner(  plotOutput('five',height = "800px"))),
                                                #tabPanel(tags$div(h4(strong("Cook's distance"),style="color:#2e5cb8;font-size:120%"),align="center"),withSpinner(  plotOutput('six',height = "800px"))),
                                                tabPanel(tags$div(h4(strong("Standardized residuals against the leverage"),style="color:#2e5cb8;font-size:120%"),align="center"),withSpinner(  plotOutput('seven',height = "800px")))
                                                #tabPanel(tags$div(h4(strong("8"),style="color:#2e5cb8;font-size:120%"),align="center"),withSpinner(  plotOutput('eight',height = "800px"))),
                                                #tabPanel(tags$div(h4(strong("the observation plot against the standardized residuals, the leverage, and the Cook's distance"),style="color:#2e5cb8;font-size:120%"),align="center"),withSpinner(  plotOutput('nine',height = "800px")))
                                    )
                           ),
                           
                           
                           
                           
                           tabPanel(
                             title = tags$a(href="javascript:history.go(0)",tags$div(bsButton("ss",strong("Logout"),style = "success",size="small"),
                                                                                     style="color:white;margin-top: -12px;font-weight:100%;",align="center"),style="color:white;margin-top: -32px;")
                           )
                           
                           # tabPanel(
                           #   title = tags$div(bsButton("levellll",strong("User Manual"),style = "primary",size="small"),style="color:white;margin-top: -22px;font-weight:100%;",align="center"),
                           #   br(),
                           #   br(),
                           #   br(),
                           #   br(),
                           #   br(),
                           #   tags$div(id = 'rr',img(src="User Manual - fraud analytics-01.jpg",height='100%',width='100%'),align="center"),
                           #   tags$div(id = 'rr',img(src="User Manual - fraud analytics-02.jpg",height='100%',width='100%'),align="center"),
                           #   tags$div(id = 'rr',img(src="User Manual - fraud analytics-03.jpg",height='100%',width='100%'),align="center"),
                           #   tags$div(id = 'rr',img(src="User Manual - fraud analytics-04.jpg",height='100%',width='100%'),align="center"),
                           #   tags$div(id = 'rr',img(src="User Manual - fraud analytics-05.jpg",height='100%',width='100%'),align="center"),
                           #   tags$div(id = 'rr',img(src="User Manual - fraud analytics-06.jpg",height='100%',width='100%'),align="center"),
                           #   tags$div(id = 'rr',img(src="User Manual - fraud analytics-07.jpg",height='100%',width='100%'),align="center"),
                           #   tags$div(id = 'rr',img(src="User Manual - fraud analytics-08.jpg",height='100%',width='100%'),align="center"),
                           #   tags$div(id = 'rr',img(src="User Manual - fraud analytics-09.jpg",height='100%',width='100%'),align="center"),
                           #   tags$div(id = 'rr',img(src="User Manual - fraud analytics-10.jpg",height='100%',width='100%'),align="center"),
                           #   tags$div(id = 'rr',img(src="User Manual - fraud analytics-11.jpg",height='100%',width='100%'),align="center"),
                           #   tags$div(id = 'rr',img(src="User Manual - fraud analytics-12.jpg",height='100%',width='100%'),align="center"),
                           #   tags$div(id = 'rr',img(src="User Manual - fraud analytics-13.jpg",height='100%',width='100%'),align="center"),
                           #   tags$div(id = 'rr',img(src="User Manual - fraud analytics-14.jpg",height='100%',width='100%'),align="center"),
                           #   tags$div(id = 'rr',img(src="User Manual - fraud analytics-15.jpg",height='100%',width='100%'),align="center"),
                           #   tags$div(id = 'rr',img(src="User Manual - fraud analytics-16.jpg",height='100%',width='100%'),align="center"),
                           #   tags$div(id = 'rr',img(src="User Manual - fraud analytics-17.jpg",height='100%',width='100%'),align="center"),
                           #   tags$div(id = 'rr',img(src="User Manual - fraud analytics-18.jpg",height='100%',width='100%'),align="center"),
                           #   tags$div(id = 'rr',img(src="User Manual - fraud analytics-19.jpg",height='100%',width='100%'),align="center"),
                           #   tags$div(id = 'rr',img(src="User Manual - fraud analytics-20.jpg",height='100%',width='100%'),align="center"),
                           #   tags$div(id = 'rr',img(src="User Manual - fraud analytics-21.jpg",height='100%',width='100%'),align="center"),
                           #   tags$div(id = 'rr',img(src="User Manual - fraud analytics-22.jpg",height='100%',width='100%'),align="center"),
                           #   tags$div(id = 'rr',img(src="User Manual - fraud analytics-23.jpg",height='100%',width='100%'),align="center")
                           # 
                           # )
                           
                           
                           # navbarMenu(#tags$div(bsButton("dummy4",strong("More"),style = "primary",size="small"),style="color:white;margin-top: -12px;font-weight:100%;",align="center"),
                           #            #tabPanel(
                           #             tags$div(tags$a(href="javascript:history.go(0)",bsButton("logoutadmin", label = "Logout", icon =   icon("repeat",lib = "glyphicon"),block = F, style="success"),style="text-align:center"),align="center")
                           #              #br()
                           #            #)
                           # )
                )
              )
            )
            
            #########################################################################################################################################################################
            
            
            
          })
        }
        
        else if(user_log == "B" ){
          output$page <- renderUI({
            
            fluidPage(
              
              # Valid themes are: cerulean, cosmo, cyborg, darkly, flatly, journal, lumen, paper, readable, sandstone, simplex, slate, spacelab, superhero, united, yeti.
              theme = shinytheme("simplex"),
              tagList(
                useShinyjs(),
                tags$style(HTML("
                                
                                
                                .navbar  {
                                background-color:white; }
                                
                                .navbar .navbar-nav {float: left; 
                                margin-top: 32px;
                                color: #; 
                                font-size: 20px; 
                                background-color: #; }
                                
                                .navbar.navbar-default.navbar-static-top{ 
                                color: #; 
                                font-size: 23px; 
                                background-color: # ;}
                                
                                .navbar .navbar-header {
                                float: left;
                                
                                background-color: # ;}
                                
                                .navbar-default .navbar-brand { color: #054b94; 
                                font-size: 28px; 
                                margin-bottom:32px;
                                background-color: # ;} 
                                
                                ")),
                
                
                tags$head(HTML("<title>app</title> <link rel='icon' type='image/gif/png' href='log.png'>")),
                
                navbarPage(id="tabs",
                           
                           title = tags$div(img(src="","Outlier detection", style="color:white;font-weight:200%;margin-top: -5px;margin-left: 30px;", height = 60)),position = "fixed-top",
                           selected = tags$div(bsButton("dummy0",strong("Upload"),style = "danger",size="small"),style="color:white;margin-top: -22px;font-weight:100%;",align="center"),inverse = F,
                           
                           tabPanel(title = tags$div(bsButton("dummy0",strong("Upload"),style = "danger",size="small"),style="color:white;margin-top: -22px;font-weight:100%;",align="center"),
                                    
                                    fluidPage(
                                      
                                      tags$style(" #modal1 .modal-header {background-color:#; border-top-left-radius: 0px; border-top-right-radius: 0px}
                                                 #modal1 .modal-dialog { width: 1800px;}
                                                 #modal1 .modal-content  {background-color:#;}"), 
                                      
                                      tags$style(type="text/css",
                                                 ".shiny-output-error { visibility: hidden; }",
                                                 ".shiny-output-error:before { visibility: hidden; }"
                                      ),
                                      tags$head(tags$style("#pppp{color:black; font-size:35px; font-style:italic; text-align=center;
                                                           overflow-y:scroll; max-height: 300px; background: ghostwhite;}")),
                                      tags$head(tags$style("#roi{color:black; font-size:35px; font-style:italic; text-align=center;
                                                           overflow-y:scroll; max-height: 300px; background: ghostwhite;}")),
                                      
                                      
                                      br(),
                                      br(),
                                      br(),
                                      
                                      column(7,
                                             
                                             br(),br(),br(),br(),br(),br(),br(),
                                             tags$div(id = 'logo1',img(src="logis.jpg",height='100%',width='100%'),align="center")
                                      ),
                                      
                                      br(),
                                      br(),
                                      
                                      column(5,
                                             
                                             
                                             bootstrapPage(
                                               
                                               br(),
                                               tags$div(id = 'logo2',img(src="loki.jpg",height='40%',width='40%'),align="center"),br(),
                                               
                                               tags$div(h4(strong(em("Outlier Analysis")),style="color:#2e5cb8;font-size:200%"),align="center"),
                                               
                                               
                                               withAnim(),
                                               br(),
                                               uiOutput('fileupload2'),
                                               #uiOutput("bss"),br(),
                                               uiOutput('checkbox2'),
                                               uiOutput("button2"),
                                               uiOutput("helptext2"),
                                               br(),
                                               br(),
                                               bsPopover(id = "dummy000",title = "Note:",content = "XXX",placement = "right"),
                                               bsPopover(id="check2",title = "",content = "Note: I accept the Terms & Conditions.. Show the Analyse button",placement = "right"),
                                               tags$div(bsButton("reset2", label = "Reset ?", icon =   icon("repeat",lib = "glyphicon"),block = F, style="danger",size = "small"),align="center"),
                                               
                                               
                                               #tags$h1(actionButton("myuser","Logout",icon=icon("user")),style="text-align:center"),
                                               br(),
                                               
                                               tags$div(class = "header", checked = NA,style="text-align:center;color:#929292;font-size:100%",
                                                        tags$tbody("Need Help ?"),
                                                        tags$a(href = "https://github.com/sailogeshh", "Contact", target="_blank")
                                               ),tags$div(actionLink("reset2",""),align="center"),
                                               br()
                                             )
                                      )
                                      
                                      
                                      
                                    )),
                           
                           
                           
                           tabPanel(value = "mytab2",
                                    
                                    title = tags$div(bsButton("dummy2",strong("Result..!"),style = "primary",size="small"),style="color:white;margin-top: -22px;font-weight:100%;",align="center"),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    tabsetPanel(type = "tabs", selected = tags$div(h4(strong("Outlier Plot"),style="color:#2e5cb8;font-size:120%"),align="center"),
                                                # tabPanel(tags$div(h4(strong("Predicted"),style="color:#2e5cb8;font-size:120%"),align="center"), uiOutput("down"),
                                                #          dataTableOutput('predict',height = "1200px")),
                                                #tabPanel(tags$div(h4(strong("Estimated coefficients"),style="color:#2e5cb8;font-size:120%"),align="center"),withSpinner( plotOutput('one2',height = "800px"))),
                                                #tabPanel(tags$div(h4(strong("Odds ratios"),style="color:#2e5cb8;font-size:120%"),align="center"),withSpinner(  plotOutput('two2',height = "800px"))),
                                                tabPanel(tags$div(h4(strong("Accuracy plot"),style="color:#2e5cb8;font-size:120%"),align="center"),withSpinner(  plotOutput('three2',height = "800px"))),
                                                #tabPanel(tags$div(h4(strong("Standardized residuals"),style="color:#2e5cb8;font-size:120%"),align="center"),withSpinner(  plotOutput('four2',height = "800px"))),
                                                tabPanel(tags$div(h4(strong("Outlier Plot"),style="color:#2e5cb8;font-size:120%"),align="center"),withSpinner(  plotOutput('five2',height = "800px"))),
                                                #tabPanel(tags$div(h4(strong("Cook's distance"),style="color:#2e5cb8;font-size:120%"),align="center"),withSpinner(  plotOutput('six2',height = "800px"))),
                                                tabPanel(tags$div(h4(strong("Standardized residuals against the leverage"),style="color:#2e5cb8;font-size:120%"),align="center"),withSpinner(  plotOutput('seven2',height = "800px")))
                                                #tabPanel(tags$div(h4(strong("8"),style="color:#2e5cb8;font-size:120%"),align="center"),withSpinner(  plotOutput('eight',height = "800px"))),
                                                #tabPanel(tags$div(h4(strong("the observation plot against the standardized residuals, the leverage, and the Cook's distance"),style="color:#2e5cb8;font-size:120%"),align="center"),withSpinner(  plotOutput('nine2',height = "800px")))
                                    )
                           ),
                           
                           
                           
                           
                           tabPanel(
                             title = tags$a(href="javascript:history.go(0)",tags$div(bsButton("ss2",strong("Logout"),style = "success",size="small"),
                                                                                     style="color:white;margin-top: -12px;font-weight:100%;",align="center"),style="color:white;margin-top: -32px;")
                           )
                           
                           # tabPanel(
                           #   title = tags$div(bsButton("levellll",strong("User Manual"),style = "primary",size="small"),style="color:white;margin-top: -22px;font-weight:100%;",align="center"),
                           #   br(),
                           #   br(),
                           #   br(),
                           #   br(),
                           #   br(),
                           #   tags$div(id = 'rr',img(src="User Manual - fraud analytics-01.jpg",height='100%',width='100%'),align="center"),
                           #   tags$div(id = 'rr',img(src="User Manual - fraud analytics-02.jpg",height='100%',width='100%'),align="center"),
                           #   tags$div(id = 'rr',img(src="User Manual - fraud analytics-03.jpg",height='100%',width='100%'),align="center"),
                           #   tags$div(id = 'rr',img(src="User Manual - fraud analytics-04.jpg",height='100%',width='100%'),align="center"),
                           #   tags$div(id = 'rr',img(src="User Manual - fraud analytics-05.jpg",height='100%',width='100%'),align="center"),
                           #   tags$div(id = 'rr',img(src="User Manual - fraud analytics-06.jpg",height='100%',width='100%'),align="center"),
                           #   tags$div(id = 'rr',img(src="User Manual - fraud analytics-07.jpg",height='100%',width='100%'),align="center"),
                           #   tags$div(id = 'rr',img(src="User Manual - fraud analytics-08.jpg",height='100%',width='100%'),align="center"),
                           #   tags$div(id = 'rr',img(src="User Manual - fraud analytics-09.jpg",height='100%',width='100%'),align="center"),
                           #   tags$div(id = 'rr',img(src="User Manual - fraud analytics-10.jpg",height='100%',width='100%'),align="center"),
                           #   tags$div(id = 'rr',img(src="User Manual - fraud analytics-11.jpg",height='100%',width='100%'),align="center"),
                           #   tags$div(id = 'rr',img(src="User Manual - fraud analytics-12.jpg",height='100%',width='100%'),align="center"),
                           #   tags$div(id = 'rr',img(src="User Manual - fraud analytics-13.jpg",height='100%',width='100%'),align="center"),
                           #   tags$div(id = 'rr',img(src="User Manual - fraud analytics-14.jpg",height='100%',width='100%'),align="center"),
                           #   tags$div(id = 'rr',img(src="User Manual - fraud analytics-15.jpg",height='100%',width='100%'),align="center"),
                           #   tags$div(id = 'rr',img(src="User Manual - fraud analytics-16.jpg",height='100%',width='100%'),align="center"),
                           #   tags$div(id = 'rr',img(src="User Manual - fraud analytics-17.jpg",height='100%',width='100%'),align="center"),
                           #   tags$div(id = 'rr',img(src="User Manual - fraud analytics-18.jpg",height='100%',width='100%'),align="center"),
                           #   tags$div(id = 'rr',img(src="User Manual - fraud analytics-19.jpg",height='100%',width='100%'),align="center"),
                           #   tags$div(id = 'rr',img(src="User Manual - fraud analytics-20.jpg",height='100%',width='100%'),align="center"),
                           #   tags$div(id = 'rr',img(src="User Manual - fraud analytics-21.jpg",height='100%',width='100%'),align="center"),
                           #   tags$div(id = 'rr',img(src="User Manual - fraud analytics-22.jpg",height='100%',width='100%'),align="center"),
                           #   tags$div(id = 'rr',img(src="User Manual - fraud analytics-23.jpg",height='100%',width='100%'),align="center")
                           # 
                           # )
                           
                           
                           # navbarMenu(#tags$div(bsButton("dummy4",strong("More"),style = "primary",size="small"),style="color:white;margin-top: -12px;font-weight:100%;",align="center"),
                           #            #tabPanel(
                           #             tags$div(tags$a(href="javascript:history.go(0)",bsButton("logoutadmin", label = "Logout", icon =   icon("repeat",lib = "glyphicon"),block = F, style="success"),style="text-align:center"),align="center")
                           #              #br()
                           #            #)
                           # )
                )
              )
            )
            
            #########################################################################################################################################################################
            
            
          })
          
        }
        
        else if(user_log == "C" ){
          output$page <- renderUI({
            fluidPage(
              
              # Valid themes are: cerulean, cosmo, cyborg, darkly, flatly, journal, lumen, paper, readable, sandstone, simplex, slate, spacelab, superhero, united, yeti.
              theme = shinytheme("simplex"),
              tagList(
                useShinyjs(),
                tags$style(HTML("
                                
                                
                                .navbar  {
                                background-color:white; }
                                
                                .navbar .navbar-nav {float: left; 
                                margin-top: 32px;
                                color: #; 
                                font-size: 20px; 
                                background-color: #; }
                                
                                .navbar.navbar-default.navbar-static-top{ 
                                color: #; 
                                font-size: 23px; 
                                background-color: # ;}
                                
                                .navbar .navbar-header {
                                float: left;
                                
                                background-color: # ;}
                                
                                .navbar-default .navbar-brand { color: #054b94; 
                                font-size: 28px; 
                                margin-bottom:32px;
                                background-color: # ;} 
                                
                                ")),
                
                
                tags$head(HTML("<title>app</title> <link rel='icon' type='image/gif/png' href='log.png'>")),
                
                navbarPage(id="tabs",
                           
                           title = tags$div(img(src="","", style="color:white;font-weight:200%;margin-top: -5px;margin-left: 30px;", height = 60)),position = "fixed-top",
                           selected = tags$div(bsButton("dummy0",strong("Upload"),style = "danger",size="small"),style="color:white;margin-top: -22px;font-weight:100%;",align="center"),inverse = F,
                           
                           tabPanel(title = tags$div(bsButton("dummy0",strong("Upload"),style = "danger",size="small"),style="color:white;margin-top: -22px;font-weight:100%;",align="center"),
                                    
                                    fluidPage(
                                      
                                      tags$style(" #modal1 .modal-header {background-color:#; border-top-left-radius: 0px; border-top-right-radius: 0px}
                                                 #modal1 .modal-dialog { width: 1800px;}
                                                 #modal1 .modal-content  {background-color:#;}"), 
                                      
                                      tags$style(type="text/css",
                                                 ".shiny-output-error { visibility: hidden; }",
                                                 ".shiny-output-error:before { visibility: hidden; }"
                                      ),
                                      tags$head(tags$style("#pppp{color:black; font-size:35px; font-style:italic; text-align=center;
                                                           overflow-y:scroll; max-height: 300px; background: ghostwhite;}")),
                                      tags$head(tags$style("#roi{color:black; font-size:35px; font-style:italic; text-align=center;
                                                           overflow-y:scroll; max-height: 300px; background: ghostwhite;}")),
                                      
                                      
                                      br(),
                                      br(),
                                      br(),
                                      
                                      column(7,
                                             
                                             br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                                             tags$div(id = 'logo1',img(src="cap.JPG",height='130%',width='100%'),align="center")
                                      ),
                                      
                                      br(),
                                      br(),
                                      
                                      column(5,
                                             
                                             
                                             bootstrapPage(
                                               
                                               br(),
                                               tags$div(id = 'logo2',img(src="rr.png",height='60%',width='60%'),align="center"),br(),
                                               
                                               tags$div(h4(strong(em("Outlier Analysis")),style="color:#2e5cb8;font-size:200%"),align="center"),
                                               
                                               
                                               withAnim(),
                                               br(),
                                               uiOutput('fileupload3'),
                                               #uiOutput("bss"),br(),
                                               uiOutput('checkbox3'),
                                               uiOutput("button3"),
                                               uiOutput("helptext3"),
                                               br(),
                                               br(),
                                               bsPopover(id = "dummy000",title = "Note:",content = "XXX",placement = "right"),
                                               bsPopover(id="check3",title = "",content = "Note: I accept the  Terms & Conditions.. Show the Analyse button",placement = "right"),
                                               tags$div(bsButton("reset3", label = "Reset ?", icon =   icon("repeat",lib = "glyphicon"),block = F, style="danger",size = "small"),align="center"),
                                               
                                               
                                               #tags$h1(actionButton("myuser","Logout",icon=icon("user")),style="text-align:center"),
                                               br(),
                                               
                                               tags$div(class = "header", checked = NA,style="text-align:center;color:#929292;font-size:100%",
                                                        tags$tbody("Need Help ?"),
                                                        tags$a(href = "https://github.com/sailogeshh", "Contact Us...", target="_blank")
                                               ),tags$div(actionLink("reset2",""),align="center"),
                                               br()
                                             )
                                      )
                                      
                                      
                                      
                                    )),
                           
                           
                           
                           tabPanel(value = "mytab2",
                                    
                                    title = tags$div(bsButton("dummy2",strong("Result..!"),style = "primary",size="small"),style="color:white;margin-top: -22px;font-weight:100%;",align="center"),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    tabsetPanel(type = "tabs", 
                                                tabPanel(tags$div(h4(strong("Outlier plot"),style="color:#2e5cb8;font-size:120%"),align="center"),br(),
                                                         wellPanel(
                                                           downloadButton('downloadData', 'Download data...'),
                                                           plotlyOutput('hifi',height = "800px")
                                                         )
                                                         
                                                )
                                                # tabPanel(tags$div(h4(strong("Outlier plot"),style="color:#2e5cb8;font-size:120%"),align="center"),br(),withSpinner(  dataTableOutput("finaldata")))
                                                
                                    )),
                                    
                                    
                                    
                                    tabPanel(
                                      title = tags$a(href="javascript:history.go(0)",tags$div(bsButton("s5s",strong("Logout"),style = "success",size="small"),
                                                                                              style="color:white;margin-top: -12px;font-weight:100%;",align="center"),style="color:white;margin-top: -32px;")
                                    )
                                    
                           
                )
              )
            )
            
            #########################################################################################################################################################################
            
          })
        }
        
        # if standard user
        else{
          output$page <- renderUI({
            h2("suneel")
            
          })
        }
      }
    })
    
    
    
    #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ############################## server1 ##############################################$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    
    observeEvent(input$analyse, {
      confirmSweetAlert(
        session = session,
        inputId = "confirmation",
        type = "warning",
        title = "Are you sure the data was uploaded ?",
        tags$div(strong(h3("If upload Done then go to the Result..! tab for outpus..",style="color:green;")),align="center"),
        btn_labels = c("Nope", "Yep"),
        danger_mode = TRUE
      )
    })
    
    
    output[["fileupload"]] <- renderUI({
      input$reset
      tags$div(fileInput("file",label = tags$h4(strong(em("Upload data..")),style="color:#004264;font-size:160%"),accept= c(
        "text/csv",
        "text/comma-separated-values,text/plain",
        ".csv")),align="center")
      
    })
    
    # output[["bss"]] <- renderUI({  
    #   tags$div(h5("Note: Once the data was uploaded go to next tab for outputs..!",style="color:#2e5cb8;font-size:100%"),align="center")
    # })
    
    
    
    output[["checkbox"]] <- renderUI({
      input$reset
      tags$div(checkboxInput("check",tags$a(href = "https://github.com/sailogeshh/", "Terms & Conditions",style="color:green;"),value = TRUE),align="center")
      
    })
    
    output[["button"]] <- renderUI({
      if (is.null(input$file)) return()
      tags$div(bsButton("analyse",strong("Lets Go..!"),icon = icon("refresh"),style = "primary",size="medium"),
               style="color:white;font-weight:100%;",align="center")
      
    })
    
    
    
    ############################################# Data ###############################################################################  
    
    data <-reactive({
      file1 <- input$file
      if(is.null(file1)) {return(NULL)}
      data <- read.csv(file1$datapath)
      data=data.frame(data)
      #data=data.frame(readxl::read_excel("ega.xlsx"))
      data
      
    })
    
    #data=read.csv("insurance data.csv")
    fit <- reactive({
      data=data()[,-c(2,5,6,8,15,16,20,22,25,26,27,38,39,40)]
      options(scipen=999)
      data$fraud_reported<-as.factor(data$fraud_reported)
      data$bodily_injuries<-as.factor(data$bodily_injuries)
      data$witnesses<-as.factor(data$witnesses)
      DV_name <-names(data[1])
      IV_names <- names(data)[names(data) != DV_name]
      IV_group <- paste(IV_names, collapse="+")
      frm <- as.formula(paste(DV_name, "~", IV_group, sep = ""))
      fit <- glm(frm, data=data, family=binomial(logit))
      fit
    })
    
    
    p1 <- function(){
      
      coeff <-  cbind(coef = coef(fit()), confint(fit()))
      df.coeff <- data.frame(Predictors=dimnames(coeff)[[1]], Coefficient_Estimate=coeff[,1], cf_lcl=coeff[,2], cf_ucl=coeff[,3], p=c(coef(summary(fit()))[,4],"NA"))
      df.coeff$p <- as.numeric(sub(",", ".", levels(df.coeff$p))[df.coeff$p])
      df.coeff$p.value <- ifelse(df.coeff$p < 0.01,"<0.01",ifelse(df.coeff$p < 0.05,"0.01<p<0.05",ifelse(df.coeff$p < 0.1,"0.05<p<0.1", ">0.1")))
      
      ggplot(df.coeff, aes(x=Coefficient_Estimate, y=Predictors, color=p.value, label=round(Coefficient_Estimate,3))) + geom_point() + 
        geom_errorbarh(aes(xmax = cf_ucl, xmin = cf_lcl, height = .2)) + geom_text(vjust = 0, nudge_y = 0.08) + geom_vline(xintercept = 0,
                                                                                                                           colour="grey", linetype = "longdash") + theme_bw() + labs(x = "Coefficients", y="Predictors")
      
    }
    
    p2 <- function(){
      odds <- cbind(OR = exp(coef(fit())), exp(confint(fit())))
      df.odds <- data.frame(Predictors=dimnames(odds)[[1]], Odds_Ratio=odds[,1], or_lcl=odds[,2], or_ucl=odds[,3], p=c(coef(summary(fit()))[,4],"NA"))
      df.odds.to.use <- df.odds[-1,]
      df.odds.to.use$p <- as.numeric(sub(",", ".", levels(df.odds.to.use$p))[df.odds.to.use$p])
      
      df.odds.to.use$p.value <- ifelse(df.odds.to.use$p < 0.01,"<0.01",ifelse(df.odds.to.use$p < 0.05,"0.01<p<0.05", ifelse(df.odds.to.use$p < 0.1, "0.05<p<0.1",">0.1")))
      ggplot(df.odds.to.use, aes(x=Odds_Ratio, y=Predictors, color=p.value, label=round(Odds_Ratio,3))) + geom_point() +
        geom_errorbarh(aes(xmax = or_ucl, xmin = or_lcl, height = .2)) + geom_text(vjust = 0, nudge_y = 0.08) + 
        geom_vline(xintercept = 1, colour="grey", linetype = "longdash") + theme_bw() + labs(x = "Odds ratios", y="Predictors")
    }
    
    p3 <- function(){
      data=data()[,-c(2,5,6,8,15,16,20,22,25,26,27,38,39,40)]
      options(scipen=999)
      data$fraud_reported<-as.factor(data$fraud_reported)
      data$bodily_injuries<-as.factor(data$bodily_injuries)
      data$witnesses<-as.factor(data$witnesses)
      df.diagn <-data.frame(point_lbl=as.numeric(rownames(data)), y=fit()$y, pred.prob=fit()$fitted.values, res=rstandard(fit()), CookDist=cooks.distance(fit()),
                            DepVar=as.factor(fit()$y), leverage=hatvalues(fit()))
      
      n.of.predictors <- sum(hatvalues(fit()))-1 #get the number of parameters (ecluding the intercept); it takes into account the levels of dummy-coded predictors if present
      lev.thresh <- round(3*((n.of.predictors+1)/nrow(data)),3)
      df.diagn$lever.check <- ifelse(df.diagn$leverage>lev.thresh,"lever. not ok","lever. ok")
      obs_per_factor <- count(df.diagn$DepVar) # requires 'plyr'
      U <- wilcox.test(df.diagn$pred.prob ~ df.diagn$DepVar)$statistic
      auc <- round(1-U/(obs_per_factor$freq[1]*obs_per_factor$freq[2]), 3)
      p3 <- ggplot(df.diagn, aes(x=pred.prob, y=y, color=DepVar)) +  geom_point(position=position_jitter(h=0.1),aes(size = leverage),shape=19, alpha=0.90)+ xlim(0,1) + theme_bw() + labs(x = paste("Predicted probability\n( AUC:", auc, " )"), y="Dependent variable")
      p3
    }
    
    p4 <- function(){
      data=data()[,-c(2,5,6,8,15,16,20,22,25,26,27,38,39,40)]
      options(scipen=999)
      data$fraud_reported<-as.factor(data$fraud_reported)
      data$bodily_injuries<-as.factor(data$bodily_injuries)
      data$witnesses<-as.factor(data$witnesses)
      df.diagn <-data.frame(point_lbl=as.numeric(rownames(data)), y=fit()$y, pred.prob=fit()$fitted.values, res=rstandard(fit()), CookDist=cooks.distance(fit()),
                            DepVar=as.factor(fit()$y), leverage=hatvalues(fit()))
      p4 <- ggplot(df.diagn, aes(x=pred.prob, y=res, color=DepVar, label=point_lbl)) + geom_point(aes(size = CookDist), shape=1, alpha=.90) + geom_hline(yintercept = 0,
                                                                                                                                                         colour="grey", linetype = "longdash") + theme_bw() + geom_text_repel(data = subset(df.diagn, abs(res) > 3 & CookDist > 1), aes(label = point_lbl), size = 2.7,
                                                                                                                                                                                                                              colour="black", box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines")) + labs(x = "Predicted probability\n(labelled points=residual >|3| AND Cook's dist.>1)",
                                                                                                                                                                                                                                                                                                                            y="Standardized residuals")#requires 'ggrepel'
      p4
    }
    
    p5 <- function(){
      data=data()[,-c(2,5,6,8,15,16,20,22,25,26,27,38,39,40)]
      options(scipen=999)
      data$fraud_reported<-as.factor(data$fraud_reported)
      data$bodily_injuries<-as.factor(data$bodily_injuries)
      data$witnesses<-as.factor(data$witnesses)
      df.diagn <-data.frame(point_lbl=as.numeric(rownames(data)), y=fit()$y, pred.prob=fit()$fitted.values, res=rstandard(fit()), CookDist=cooks.distance(fit()),
                            DepVar=as.factor(fit()$y), leverage=hatvalues(fit()))
      n.of.predictors <- sum(hatvalues(fit()))-1 #get the number of parameters (ecluding the intercept); it takes into account the levels of dummy-coded predictors if present
      lev.thresh <- round(3*((n.of.predictors+1)/nrow(data)),3)
      df.diagn$lever.check <- ifelse(df.diagn$leverage>lev.thresh,"lever. not ok","lever. ok")
      p5 <- ggplot(df.diagn, aes(x=pred.prob, y=leverage, color=lever.check)) + geom_point(aes(size = leverage), shape=19, alpha=.90) + 
        geom_hline(yintercept = lev.thresh, colour="red", linetype = "longdash") + theme_bw() + geom_text_repel(data = subset(df.diagn, leverage > lev.thresh), 
                                                                                                                aes(label = point_lbl), size = 2.7, colour="black", box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines")) + 
        labs(x = paste("Predicted probability\n( leverage threshold: 3*(k+1)/N=", lev.thresh, " )"), y="Leverage") #requires 'ggrepel'
      p5
    }
    
    
    
    p6 <- function(){
      data=data()[,-c(2,5,6,8,15,16,20,22,25,26,27,38,39,40)]
      options(scipen=999)
      data$fraud_reported<-as.factor(data$fraud_reported)
      data$bodily_injuries<-as.factor(data$bodily_injuries)
      data$witnesses<-as.factor(data$witnesses)
      df.diagn <-data.frame(point_lbl=as.numeric(rownames(data)), y=fit()$y, pred.prob=fit()$fitted.values, res=rstandard(fit()), CookDist=cooks.distance(fit()),
                            DepVar=as.factor(fit()$y), leverage=hatvalues(fit()))  
      
      p6 <- ggplot(df.diagn, aes(x=pred.prob, y=CookDist, color=DepVar)) + geom_point(aes(size = CookDist), shape=1, alpha=.90) + theme_bw() + 
        geom_text_repel(data = subset(df.diagn, CookDist > 1), aes(label = point_lbl), size = 2.7, colour="black", box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines")) +
        labs(x = "Predicted probability\n(labelled points=Cook's dist.>1)", y="Cook's distance") #requires 'ggrepel'
      p6
    }
    
    
    p7 <- function(){
      data=data()[,-c(2,5,6,8,15,16,20,22,25,26,27,38,39,40)]
      options(scipen=999)
      data$fraud_reported<-as.factor(data$fraud_reported)
      data$bodily_injuries<-as.factor(data$bodily_injuries)
      data$witnesses<-as.factor(data$witnesses)
      df.diagn <-data.frame(point_lbl=as.numeric(rownames(data)), y=fit()$y, pred.prob=fit()$fitted.values, res=rstandard(fit()), CookDist=cooks.distance(fit()),
                            DepVar=as.factor(fit()$y), leverage=hatvalues(fit()))
      n.of.predictors <- sum(hatvalues(fit()))-1 #get the number of parameters (ecluding the intercept); it takes into account the levels of dummy-coded predictors if present
      lev.thresh <- round(3*((n.of.predictors+1)/nrow(data)),3)
      df.diagn$lever.check <- ifelse(df.diagn$leverage>lev.thresh,"lever. not ok","lever. ok")  
      
      p7 <- ggplot(df.diagn, aes(x=res, y=leverage, color=DepVar)) + geom_point(aes(size = CookDist), shape=19, alpha=.90) +
        geom_hline(yintercept = lev.thresh, colour="red", linetype = "longdash") + theme_bw() +
        geom_text_repel(data = subset(df.diagn, abs(res) > 3 | leverage > lev.thresh | CookDist > 1), aes(label = point_lbl),
                        size = 2.7, colour="black", box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines")) +
        labs(x = paste("Standardized residuals\n(labelled points=residual>|3| OR lever.>", lev.thresh,"OR Cook's dist.>1)"), y="Leverage") #requires 'ggrepel'
      p7
    }
    
    
    p8 <- function(){
      data=data()[,-c(2,5,6,8,15,16,20,22,25,26,27,38,39,40)]
      options(scipen=999)
      data$fraud_reported<-as.factor(data$fraud_reported)
      data$bodily_injuries<-as.factor(data$bodily_injuries)
      data$witnesses<-as.factor(data$witnesses)
      df.diagn <-data.frame(point_lbl=as.numeric(rownames(data)), y=fit()$y, pred.prob=fit()$fitted.values, res=rstandard(fit()), CookDist=cooks.distance(fit()),
                            DepVar=as.factor(fit()$y), leverage=hatvalues(fit()))
      n.of.predictors <- sum(hatvalues(fit()))-1 #get the number of parameters (ecluding the intercept); it takes into account the levels of dummy-coded predictors if present
      lev.thresh <- round(3*((n.of.predictors+1)/nrow(data)),3)
      df.diagn$lever.check <- ifelse(df.diagn$leverage>lev.thresh,"lever. not ok","lever. ok")
      
      p8 <- ggplot(df.diagn, aes(x=res, y=leverage, color=DepVar)) + geom_point(aes(size = CookDist), shape=1, alpha=.80) +
        geom_hline(yintercept = lev.thresh, colour="grey", linetype = "longdash") + theme_bw() +
        geom_text_repel(data = subset(df.diagn, abs(res) > 3 | leverage > lev.thresh & CookDist > 1), aes(label = point_lbl),
                        size = 2.7, colour="black", box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines")) +
        labs(x = paste("Standardized residuals\n(labelled points=residual>|3| OR lever.>", lev.thresh,"AND Cook's dist.>1)"), y="Leverage") #requires 'ggrepel'
      p8
    }
    
    
    p9top11 <- function(){
      data=data()[,-c(2,5,6,8,15,16,20,22,25,26,27,38,39,40)]
      options(scipen=999)
      data$fraud_reported<-as.factor(data$fraud_reported)
      data$bodily_injuries<-as.factor(data$bodily_injuries)
      data$witnesses<-as.factor(data$witnesses)
      df.diagn <-data.frame(point_lbl=as.numeric(rownames(data)), y=fit()$y, pred.prob=fit()$fitted.values, res=rstandard(fit()), CookDist=cooks.distance(fit()),
                            DepVar=as.factor(fit()$y), leverage=hatvalues(fit()))
      n.of.predictors <- sum(hatvalues(fit()))-1 #get the number of parameters (ecluding the intercept); it takes into account the levels of dummy-coded predictors if present
      lev.thresh <- round(3*((n.of.predictors+1)/nrow(data)),3)
      df.diagn$lever.check <- ifelse(df.diagn$leverage>lev.thresh,"lever. not ok","lever. ok") 
      
      p9 <- ggplot(df.diagn, aes(x=point_lbl, y=res, label=point_lbl)) + geom_point() + theme_bw() + geom_text_repel(data = subset(df.diagn, abs(res) > 3), aes(label = point_lbl), size = 2.7, colour="black", box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines")) + labs(x = "Observation number\n(labelled points=resid.>|3|)", y="Standardized residuals") #requires 'ggrepel'
      p10 <- ggplot(df.diagn, aes(x=point_lbl, y=leverage, label=point_lbl)) + geom_point() + theme_bw() + geom_text_repel(data = subset(df.diagn, leverage > lev.thresh), aes(label = point_lbl), size = 2.7, colour="black", box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines")) + labs(x = paste("Observation number\n(labelled points=leverage>",lev.thresh,")"), y="Leverage") #requires 'ggrepel'
      p11 <- ggplot(df.diagn, aes(x=point_lbl, y=CookDist, label=point_lbl)) + geom_point() + theme_bw() + geom_text_repel(data = subset(df.diagn, CookDist > 1), aes(label = point_lbl), size = 2.7, colour="black", box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines")) + labs(x = "Observation number\n(labelled points=Cook's dist.>1)", y="Cook's distance") #requires 'ggrepel'
      grid.arrange(p9, p10, p11,  ncol=1)
    }
    
    observeEvent(input$confirmation, {
      if(input$confirmation==TRUE){     
        output$one <- renderPlot({
          p1()
        })  }
    })
    
    
    observeEvent(input$confirmation, {
      if(input$confirmation==TRUE){
        output$two <- renderPlot({
          p2()
        })  }
    })
    
    observeEvent(input$confirmation, {
      if(input$confirmation==TRUE){
        output$three <- renderPlot({
          p3()
        })  }
    })
    
    observeEvent(input$confirmation, {
      if(input$confirmation==TRUE){
        output$four <- renderPlot({
          p4()
        })  }
    })
    
    observeEvent(input$confirmation, {
      if(input$confirmation==TRUE){
        output$five <- renderPlot({
          p5()
        })  }
    })
    
    observeEvent(input$confirmation, {
      if(input$confirmation==TRUE){
        output$six <- renderPlot({
          p6()
        })  }
    })
    
    observeEvent(input$confirmation, {
      if(input$confirmation==TRUE){
        output$seven <- renderPlot({
          p7()
        })  }
    })
    
    observeEvent(input$confirmation, {
      if(input$confirmation==TRUE){
        output$eight <- renderPlot({
          p8()
        })  }
    })
    
    observeEvent(input$confirmation, {
      if(input$confirmation==TRUE){
        output$nine <- renderPlot({
          p9top11()
        })
      }
    })
    
    #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$############################################################################$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    
    
    
    
    
    #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$############################## server2 ##############################################$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    
    
    
    observeEvent(input$analyse2, {
      confirmSweetAlert(
        session = session,
        inputId = "confirmation2",
        type = "warning",
        title = "Are you sure the data was uploaded ?",
        tags$div(strong(h3("If upload Done then go to the Result..! tab for outpus..",style="color:green;")),align="center"),
        btn_labels = c("Nope", "Yep"),
        danger_mode = TRUE
      )
    })
    
    
    output[["fileupload2"]] <- renderUI({
      input$reset2
      tags$div(fileInput("file2",label = tags$h4(strong(em("Upload data..")),style="color:#004264;font-size:160%"),accept= c(
        "text/csv",
        "text/comma-separated-values,text/plain",
        ".csv")),align="center")
      
    })
    
    # output[["bss"]] <- renderUI({  
    #   tags$div(h5("Note: Once the data was uploaded go to next tab for outputs..!",style="color:#2e5cb8;font-size:100%"),align="center")
    # })
    
    
    
    output[["checkbox2"]] <- renderUI({
      input$reset2
      tags$div(checkboxInput("check2",tags$a(href = "https://github.com/sailogeshh", "Terms & Conditions",style="color:green;"),value = TRUE),align="center")
      
    })
    
    output[["button2"]] <- renderUI({
      if (is.null(input$file2)) return()
      tags$div(bsButton("analyse2",strong("Lets Go..!"),icon = icon("refresh"),style = "primary",size="medium"),
               style="color:white;font-weight:100%;",align="center")
      
    })
    
    
    
    ############################################# Data ###############################################################################  
    
    data2 <-reactive({
      file1 <- input$file2
      if(is.null(file1)) {return(NULL)}
      data <- read.csv(file1$datapath)
      data=data.frame(data)
      #data=data.frame(readxl::read_excel("ega.xlsx"))
      data
      
    })
    
    #data2<- function(){read.csv("mydata.csv")}
    
    fit2 <- function(){
      data=data2()
      options(scipen=999)
      DV_name <-names(data[1])
      IV_names <- names(data)[names(data) != DV_name]
      IV_group <- paste(IV_names, collapse="+")
      frm <<- as.formula(paste(DV_name, "~", IV_group, sep = ""))
      fit <<- glm(frm, data=data, family=binomial(logit))
      fit
    }
    
    
    p12 <- function(){
      
      coeff <-  cbind(coef = coef(fit2()), confint(fit2()))
      df.coeff <- data.frame(Predictors=dimnames(coeff)[[1]], Coefficient_Estimate=coeff[,1], cf_lcl=coeff[,2], cf_ucl=coeff[,3], p=coef(summary(fit2()))[,4])
      df.coeff$p.value <- ifelse(df.coeff$p < 0.01,"<0.01",ifelse(df.coeff$p < 0.05,"0.01<p<0.05",ifelse(df.coeff$p < 0.1,"0.05<p<0.1", ">0.1")))
      
      ggplot(df.coeff, aes(x=Coefficient_Estimate, y=Predictors, color=p.value, label=round(Coefficient_Estimate,3))) + geom_point() + 
        geom_errorbarh(aes(xmax = cf_ucl, xmin = cf_lcl, height = .2)) + geom_text(vjust = 0, nudge_y = 0.08) + geom_vline(xintercept = 0,
                                                                                                                           colour="grey", linetype = "longdash") + theme_bw() + labs(x = "Coefficients", y="Predictors")
      
    }
    
    p22 <- function(){
      odds <- cbind(OR = exp(coef(fit2())), exp(confint(fit2())))
      df.odds <- data.frame(Predictors=dimnames(odds)[[1]], Odds_Ratio=odds[,1], or_lcl=odds[,2], or_ucl=odds[,3], p=coef(summary(fit2()))[,4])
      df.odds.to.use <- df.odds[-1,]
      df.odds.to.use$p.value <- ifelse(df.odds.to.use$p < 0.01,"<0.01",ifelse(df.odds.to.use$p < 0.05,"0.01<p<0.05", ifelse(df.odds.to.use$p < 0.1, "0.05<p<0.1",">0.1")))
      ggplot(df.odds.to.use, aes(x=Odds_Ratio, y=Predictors, color=p.value, label=round(Odds_Ratio,3))) + geom_point() +
        geom_errorbarh(aes(xmax = or_ucl, xmin = or_lcl, height = .2)) + geom_text(vjust = 0, nudge_y = 0.08) + 
        geom_vline(xintercept = 1, colour="grey", linetype = "longdash") + theme_bw() + labs(x = "Odds ratios", y="Predictors")
    }
    
    p32 <- function(){
      data=data2()
      options(scipen=999)
      DV_name <-names(data[1])
      IV_names <- names(data)[names(data) != DV_name]
      IV_group <- paste(IV_names, collapse="+")
      df.diagn <-data.frame(point_lbl=as.numeric(rownames(data)), y=fit2()$y, pred.prob=fit2()$fitted.values, res=rstandard(fit2()), CookDist=cooks.distance(fit2()),
                            DepVar=as.factor(fit2()$y), leverage=hatvalues(fit2()))
      
      n.of.predictors <- sum(hatvalues(fit2()))-1 #get the number of parameters (ecluding the intercept); it takes into account the levels of dummy-coded predictors if present
      lev.thresh <- round(3*((n.of.predictors+1)/nrow(data)),3)
      df.diagn$lever.check <- ifelse(df.diagn$leverage>lev.thresh,"lever. not ok","lever. ok")
      obs_per_factor <- count(df.diagn$DepVar) # requires 'plyr'
      U <- wilcox.test(df.diagn$pred.prob ~ df.diagn$DepVar)$statistic
      auc <- round(1-U/(obs_per_factor$freq[1]*obs_per_factor$freq[2]), 3)
      p3 <- ggplot(df.diagn, aes(x=pred.prob, y=y, color=DepVar)) + geom_point(position=position_jitter(h=0.1),aes(size = leverage),shape=19, alpha=0.90) + xlim(0,1) + 
        theme_bw() + labs(x = paste("Predicted probability\n( AUC:", auc, " )"), y="Dependent variable")
      p3
    }
    
    p42 <- function(){
      data=data2()
      options(scipen=999)
      DV_name <-names(data[1])
      IV_names <- names(data)[names(data) != DV_name]
      IV_group <- paste(IV_names, collapse="+")
      df.diagn <-data.frame(point_lbl=as.numeric(rownames(data)), y=fit2()$y, pred.prob=fit2()$fitted.values, res=rstandard(fit2()), CookDist=cooks.distance(fit2()),
                            DepVar=as.factor(fit2()$y), leverage=hatvalues(fit2()))
      p4 <- ggplot(df.diagn, aes(x=pred.prob, y=res, color=DepVar, label=point_lbl)) + geom_point(aes(size = CookDist), shape=1, alpha=.90) + geom_hline(yintercept = 0,
                                                                                                                                                         colour="grey", linetype = "longdash") + theme_bw() + geom_text_repel(data = subset(df.diagn, abs(res) > 3 & CookDist > 1), aes(label = point_lbl), size = 2.7,
                                                                                                                                                                                                                              colour="black", box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines")) + labs(x = "Predicted probability\n(labelled points=residual >|3| AND Cook's dist.>1)",
                                                                                                                                                                                                                                                                                                                            y="Standardized residuals")#requires 'ggrepel'
      p4
    }
    
    p52 <- function(){
      data=data2()
      options(scipen=999)
      DV_name <-names(data[1])
      IV_names <- names(data)[names(data) != DV_name]
      IV_group <- paste(IV_names, collapse="+")
      df.diagn <-data.frame(point_lbl=as.numeric(rownames(data)), y=fit2()$y, pred.prob=fit2()$fitted.values, res=rstandard(fit2()), CookDist=cooks.distance(fit2()),
                            DepVar=as.factor(fit2()$y), leverage=hatvalues(fit2()))
      n.of.predictors <- sum(hatvalues(fit2()))-1 #get the number of parameters (ecluding the intercept); it takes into account the levels of dummy-coded predictors if present
      lev.thresh <- round(3*((n.of.predictors+1)/nrow(data)),3)
      df.diagn$lever.check <- ifelse(df.diagn$leverage>lev.thresh,"lever. not ok","lever. ok")
      p5 <- ggplot(df.diagn, aes(x=pred.prob, y=leverage, color=lever.check)) + geom_point(aes(size = leverage), shape=19, alpha=.90) + 
        geom_hline(yintercept = lev.thresh, colour="red", linetype = "longdash") + theme_bw() + geom_text_repel(data = subset(df.diagn, leverage > lev.thresh), 
                                                                                                                aes(label = point_lbl), size = 2.7, colour="black", box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines")) + 
        labs(x = paste("Predicted probability\n( leverage threshold: 3*(k+1)/N=", lev.thresh, " )"), y="Leverage") #requires 'ggrepel'
      p5
    }
    
    
    
    p62 <- function(){
      data=data2()
      options(scipen=999)
      DV_name <-names(data[1])
      IV_names <- names(data)[names(data) != DV_name]
      IV_group <- paste(IV_names, collapse="+")
      df.diagn <-data.frame(point_lbl=as.numeric(rownames(data)), y=fit2()$y, pred.prob=fit2()$fitted.values, res=rstandard(fit2()), CookDist=cooks.distance(fit2()),
                            DepVar=as.factor(fit2()$y), leverage=hatvalues(fit2()))  
      
      p6 <- ggplot(df.diagn, aes(x=pred.prob, y=CookDist, color=DepVar)) + geom_point(aes(size = CookDist), shape=1, alpha=.90) + theme_bw() + 
        geom_text_repel(data = subset(df.diagn, CookDist > 1), aes(label = point_lbl), size = 2.7, colour="black", box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines")) +
        labs(x = "Predicted probability\n(labelled points=Cook's dist.>1)", y="Cook's distance") #requires 'ggrepel'
      p6
    }
    
    
    p72 <- function(){
      data=data2()
      options(scipen=999)
      DV_name <-names(data[1])
      IV_names <- names(data)[names(data) != DV_name]
      IV_group <- paste(IV_names, collapse="+")
      df.diagn <-data.frame(point_lbl=as.numeric(rownames(data)), y=fit2()$y, pred.prob=fit2()$fitted.values, res=rstandard(fit2()), CookDist=cooks.distance(fit2()),
                            DepVar=as.factor(fit2()$y), leverage=hatvalues(fit2()))
      n.of.predictors <- sum(hatvalues(fit2()))-1 #get the number of parameters (ecluding the intercept); it takes into account the levels of dummy-coded predictors if present
      lev.thresh <- round(3*((n.of.predictors+1)/nrow(data)),3)
      df.diagn$lever.check <- ifelse(df.diagn$leverage>lev.thresh,"lever. not ok","lever. ok")  
      
      p7 <- ggplot(df.diagn, aes(x=res, y=leverage, color=DepVar)) + geom_point(aes(size = CookDist), shape=19, alpha=.90) +
        geom_hline(yintercept = lev.thresh, colour="red", linetype = "longdash") + theme_bw() +
        geom_text_repel(data = subset(df.diagn, abs(res) > 3 | leverage > lev.thresh | CookDist > 1), aes(label = point_lbl),
                        size = 2.7, colour="black", box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines")) +
        labs(x = paste("Standardized residuals\n(labelled points=residual>|3| OR lever.>", lev.thresh,"OR Cook's dist.>1)"), y="Leverage") #requires 'ggrepel'
      p7
    }
    
    
    p82 <- function(){
      data=data2()
      options(scipen=999)
      DV_name <-names(data[1])
      IV_names <- names(data)[names(data) != DV_name]
      IV_group <- paste(IV_names, collapse="+")
      df.diagn <-data.frame(point_lbl=as.numeric(rownames(data)), y=fit2()$y, pred.prob=fit2()$fitted.values, res=rstandard(fit2()), CookDist=cooks.distance(fit2()),
                            DepVar=as.factor(fit2()$y), leverage=hatvalues(fit2()))
      n.of.predictors <- sum(hatvalues(fit2()))-1 #get the number of parameters (ecluding the intercept); it takes into account the levels of dummy-coded predictors if present
      lev.thresh <- round(3*((n.of.predictors+1)/nrow(data)),3)
      df.diagn$lever.check <- ifelse(df.diagn$leverage>lev.thresh,"lever. not ok","lever. ok")
      
      p8 <- ggplot(df.diagn, aes(x=res, y=leverage, color=DepVar)) + geom_point(aes(size = CookDist), shape=1, alpha=.80) +
        geom_hline(yintercept = lev.thresh, colour="grey", linetype = "longdash") + theme_bw() +
        geom_text_repel(data = subset(df.diagn, abs(res) > 3 | leverage > lev.thresh & CookDist > 1), aes(label = point_lbl),
                        size = 2.7, colour="black", box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines")) +
        labs(x = paste("Standardized residuals\n(labelled points=residual>|3| OR lever.>", lev.thresh,"AND Cook's dist.>1)"), y="Leverage") #requires 'ggrepel'
      p8
    }
    
    
    p9top112 <- function(){
      data=data2()
      options(scipen=999)
      DV_name <-names(data[1])
      IV_names <- names(data)[names(data) != DV_name]
      IV_group <- paste(IV_names, collapse="+")
      df.diagn <-data.frame(point_lbl=as.numeric(rownames(data)), y=fit2()$y, pred.prob=fit2()$fitted.values, res=rstandard(fit2()), CookDist=cooks.distance(fit2()),
                            DepVar=as.factor(fit2()$y), leverage=hatvalues(fit2()))
      n.of.predictors <- sum(hatvalues(fit2()))-1 #get the number of parameters (ecluding the intercept); it takes into account the levels of dummy-coded predictors if present
      lev.thresh <- round(3*((n.of.predictors+1)/nrow(data)),3)
      df.diagn$lever.check <- ifelse(df.diagn$leverage>lev.thresh,"lever. not ok","lever. ok") 
      
      p9 <- ggplot(df.diagn, aes(x=point_lbl, y=res, label=point_lbl)) + geom_point() + theme_bw() + geom_text_repel(data = subset(df.diagn, abs(res) > 3), aes(label = point_lbl), size = 2.7, colour="black", box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines")) + labs(x = "Observation number\n(labelled points=resid.>|3|)", y="Standardized residuals") #requires 'ggrepel'
      p10 <- ggplot(df.diagn, aes(x=point_lbl, y=leverage, label=point_lbl)) + geom_point() + theme_bw() + geom_text_repel(data = subset(df.diagn, leverage > lev.thresh), aes(label = point_lbl), size = 2.7, colour="black", box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines")) + labs(x = paste("Observation number\n(labelled points=leverage>",lev.thresh,")"), y="Leverage") #requires 'ggrepel'
      p11 <- ggplot(df.diagn, aes(x=point_lbl, y=CookDist, label=point_lbl)) + geom_point() + theme_bw() + geom_text_repel(data = subset(df.diagn, CookDist > 1), aes(label = point_lbl), size = 2.7, colour="black", box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines")) + labs(x = "Observation number\n(labelled points=Cook's dist.>1)", y="Cook's distance") #requires 'ggrepel'
      grid.arrange(p9, p10, p11,  ncol=1)
    }
    
    observeEvent(input$confirmation2, {
      if(input$confirmation2==TRUE){     
        output$one2 <- renderPlot({
          p12()
        })  }
    })
    
    
    observeEvent(input$confirmation2, {
      if(input$confirmation2==TRUE){
        output$two2 <- renderPlot({
          p22()
        })  }
    })
    
    observeEvent(input$confirmation2, {
      if(input$confirmation2==TRUE){
        output$three2 <- renderPlot({
          p32()
        })  }
    })
    
    observeEvent(input$confirmation2, {
      if(input$confirmation2==TRUE){
        output$four2 <- renderPlot({
          p42()
        })  }
    })
    
    observeEvent(input$confirmation2, {
      if(input$confirmation2==TRUE){
        output$five2 <- renderPlot({
          p52()
        })  }
    })
    
    observeEvent(input$confirmation2, {
      if(input$confirmation2==TRUE){
        output$six2 <- renderPlot({
          p62()
        })  }
    })
    
    observeEvent(input$confirmation2, {
      if(input$confirmation2==TRUE){
        output$seven2 <- renderPlot({
          p72()
        })  }
    })
    
    observeEvent(input$confirmation2, {
      if(input$confirmation2==TRUE){
        output$eight2 <- renderPlot({
          p82()
        })  }
    })
    
    observeEvent(input$confirmation2, {
      if(input$confirmation2==TRUE){
        output$nine2 <- renderPlot({
          p9top112()
        })
      }
    })
    
    
    
    #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$############################## server3 ##############################################$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    
    
    
    observeEvent(input$analyse3, {
      confirmSweetAlert(
        session = session,
        inputId = "confirmation3",
        type = "warning",
        title = "Are you sure the data was uploaded ?",
        tags$div(strong(h3("If upload Done then go to the Result..! tab for outpus..",style="color:green;")),align="center"),
        btn_labels = c("Nope", "Yep"),
        danger_mode = TRUE
      )
    })
    
    
    output[["fileupload3"]] <- renderUI({
      input$reset3
      tags$div(fileInput("file3",label = tags$h4(strong(em("Upload data..")),style="color:#004264;font-size:160%"),accept= c(
        "text/csv",
        "text/comma-separated-values,text/plain",
        ".csv")),align="center")
      
    })
    
    
    
    output[["checkbox3"]] <- renderUI({
      input$reset3
      tags$div(checkboxInput("check3",tags$a(href = "https://github.com/sailogeshh", "Terms & Conditions",style="color:green;"),value = TRUE),align="center")
      
    })
    
    output[["button3"]] <- renderUI({
      if (is.null(input$file3)) return()
      tags$div(bsButton("analyse3",strong("Lets Go..!"),icon = icon("refresh"),style = "primary",size="medium"),
               style="color:white;font-weight:100%;",align="center")
      
    })
    
    
    
    ############################################# Data ###############################################################################  
    
    data3 <-reactive({
      file1 <- input$file3
      if(is.null(file1)) {return(NULL)}
      data <- read.csv(file1$datapath)
      # data=data.frame(abs(data))
      #data=data.frame(readxl::read_excel("ega.xlsx"))
      data
      
    })
    
    
    
    #data2<- function(){read.csv("mydata.csv")}
    observeEvent(input$confirmation3, {
      if(input$confirmation3==TRUE){
        output$hifi <- renderPlotly({
          file1 <- input$file3
          data <- read.csv(file1$datapath)
          # data=data.frame(abs(data))
          #data=data.frame(readxl::read_excel("ega.xlsx"))
          data = data[,-1]
          
          dkk = dim(data)[2]
          
          p = plot_ly(type = 'box')
          
          for (i in 1:dkk) {
            p = add_boxplot(p, y = data[,i],name = names(data)[i])
          }
          p
          
          
        })
      }
    })
    
    outlier <- function (x,method="mean",addthres=FALSE){
      if (method=="mean") {
        avrg <- mean(x)
        stdev <-sd(x)
        dtf <<- data.frame(ID=seq.int(length(x)), obs=x, outlier=abs(x-avrg)>2*stdev)
        midp <<- avrg
        lower <<- avrg-2*stdev
        upper <<- avrg+2*stdev
        outliern <<- length(which(dtf=="TRUE"))
      } else {}
      if (method=="median") {
        med <- median(x)
        MAD <-median(abs(med-x))
        dtf <<- data.frame(ID=seq.int(length(x)), obs=x, outlier=abs(x-med)>2*(MAD/0.6745))
        midp <<- med
        lower <<- med-2*(MAD/0.6745)
        upper <<- med+2*(MAD/0.6745)
        outliern <<- length(which(dtf=="TRUE"))
      } else {}
      if (method=="boxplot") {
        Q1 <- quantile(x, 0.25)
        Q3 <- quantile(x, 0.75)
        IntQ <-Q3-Q1
        dtf <<- data.frame(ID=seq.int(length(x)), obs=x, outlier=x<Q1-1.5*IntQ | x>Q3+1.5*IntQ)
        midp <<- median(x)
        lower <<- Q1-1.5*IntQ
        upper <<- Q3+1.5*IntQ
        outliern <<- length(which(dtf=="TRUE"))
      } else {}
      if (addthres==TRUE) {
        p <- ggplot(dtf, aes(x=ID, y=obs, label=ID)) + geom_point(aes(colour=outlier)) + geom_text_repel(data = subset(dtf, outlier=="TRUE"), aes(label = ID), size = 2.7, colour="black", box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines")) + labs(x=paste("observation ID number\n number of outliers detected=", outliern, "\n( outlier detection method=", method, ")"), y="observation value") + geom_hline(yintercept = midp, colour="black", linetype = "longdash") + geom_hline(yintercept = lower, colour="black", linetype = "longdash") + geom_hline(yintercept = upper, colour="black", linetype = "longdash")
      } else {
        p <- ggplot(dtf, aes(x=ID, y=obs, label=ID)) + geom_point(aes(colour=outlier)) + geom_text_repel(data = subset(dtf, outlier=="TRUE"), aes(label = ID), size = 2.7, colour="black", box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines")) + labs(x=paste("observation ID number\n( outlier detection method=", method, ")"), y="observation value") #requires 'ggrepel'
      }
      return(p)
    }
    
    
    download11 = reactive({
      
      outl = list()
      outl1 = list()
      
      data1 = data3()
      data2 = data1[,-1]
      
      
      for(i in colnames(data2)){
        gfgf = outlier(data2[,i], method = "median", addthres = TRUE)
        dd = data.frame(gfgf$data[,3])
        names(dd) = c(i)
        outl[[i]] = dd
        outl1[[i]] = gfgf
      }
      
      final = do.call('cbind',outl)
      
      library(sjmisc)
      
      hsdjkh = row_count(final, count = TRUE, append = FALSE)
      
      final = data.frame(final, hsdjkh)
      
      Percentage = round((final$rowcount/(dim(final)[2]-1))*100, 2)
      
      final = data.frame(final, Percentage)
      
      final = data.frame(data1$customer_id,final)
      
      names(final)[1] = c('Customer_Id')
      names(final)
      
      final1 = final[order(final$Percentage,decreasing = TRUE), ]
      
      final1
      
      final2 = final1 %>% filter(rowcount >=1)
      
      dim2 = dim(final1)[1]/10
      
      if(dim(final2)[1]<=dim2){
        dddd = final2
      }else{
        dddd = final2[1:dim2,]
      }
      
      dddd
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("dataset-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(download11(), file)
      })
    
    output[['finaldata']] = renderDataTable({
      download11()
    })
    
    
    #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$############################################################################$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    
    
    
    
  }) # END SHINYAPP
  
  #======
  # RUN
  #======
  shinyApp(ui = ui, server = server)
  
  


