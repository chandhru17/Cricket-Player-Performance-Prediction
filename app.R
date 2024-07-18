library(shiny)

squad <- read.csv('squad.csv')
source <- read.csv('deliveries.csv')

is_present <- FALSE
row <- 1
ar <- 0

ui <- fluidPage(
  includeCSS('style.css'),
  div(class = 'container',
      h1('Cric Analyst.',class = 'head'),
      p('Unravel the dynamics of cricket with our expert analysis.',class = 'slogan'),
      br(),
      hr(),
      textInput('playerIn', label = 'Enter player name:'),
      radioButtons('option','Choose a method:',choices = c('Overall Performance ','Against a particular team')),
      conditionalPanel(
        condition = "input.option == 'Against a particular team'",
        selectInput('dropdown','Select an opposing team',choices = c('Chennai Super Kings','Delhi Daredevils','Kolkata Knight Riders','Mumbai Indians','Kings XI Punjab','Rajasthan Royals','Royal Challengers Bangalore','Sunrisers Hyderabad')),
      ),
      conditionalPanel(
        condition = 'input.condition',
        radioButtons('all_rounder_choice','Choose a style for the all rounder:',choices = c('Batting','Bowling'))
      ),
      uiOutput('ar_ui'),
      uiOutput('ar_btn'),
      actionButton('analyze', 'Analyze',class = 'btn'),
      hr(),
      textOutput('completed'),
      textOutput('error1'),
      textOutput('error2'),
      br(),
      p('Since the application is built on data from 2008-2019, any new team added or new players joined after the period may not be reflected',class = 'warning'),
      br(),
      p('With ♥, from India',class = 'message')
  )
)

server <- function(session,input, output) {
  
  
  observeEvent(input$analyze, {
    all_rounder <- reactiveVal(FALSE)
    ar_ui <- reactiveVal(FALSE)
    ar_btn <- reactiveVal(FALSE)
    
    player <- strsplit(input$playerIn, "\\s+")[[1]]
    player <- as.vector(player)
    if(length(player) == 1){
      second <- player[1]
      first <- '--'
    }else{
      first <- player[1]
      second <- player[2]
    }
    
    for(i in squad$Player.s.List){
      if(is_present){
        break
      }
      if(grepl(tolower(input$playerIn),tolower(i))){
        player_type<-squad[row,4]
        is_present<-TRUE
        team<-squad[row,8]
      }
      row<-row+1
    }
    
    if(is_present == FALSE){
      output$completed <- renderText('An error occured due to one of the following')
      output$error1 <- renderText('1. Player is not part of 2023 edition')
      output$error2 <- renderText('2. Player\'s name might be mispelt')
    }else{
      opposing_team <- input$dropdown
      if(opposing_team %in% c('Kolkata Knight Riders','Royal Challengers Bangalore','Chennai Super Kings','Kings XI Punjab')){
        len <- 3
      }else{
        len <- 2
      }
      
      opt <-trimws(input$option)
      
      
      if ('Overall Performance' %in% opt) {
        status <- 0
        
      } else {
        status <- 1
        
      }
      
      
      
      if(player_type == 'ALL-ROUNDER'){
        updateActionButton(session,'analyze',label = '')
        ar <- 1
        
        output$ar_ui <- renderUI({
          selectInput('ar_drop','Select a style for the all rounder:',choices = c('Batting','Bowling'))
        })
        output$ar_btn <- renderUI(actionButton('ar_btn','Analyze'))
        
        opt <- trimws(input$option)
        
        if('Overall Performance' %in% opt){
          status <- 0
        }else{
          status <- 1
        }
        
        
        observeEvent(input$ar_btn,{
          
          cmd <- paste('Rscript', 'analyze.R',len,ar,first,second,input$dropdown,status,input$ar_drop)
          system(cmd)
          output$completed <- renderText({'Successfully generated the graph pdf!!'})
          
          pdf_file <- paste(getwd(),'/Rplots.pdf',sep = '')
          
          browseURL(pdf_file)
          if (Sys.info()["sysname"] == "Windows") {
            system(paste("start", shQuote(pdf_file)))
          }
        })
        
      }else{
        cmd <- paste('Rscript', 'analyze.R',len,ar,first,second,input$dropdown,status)
        system(cmd)
        output$completed <- renderText({'Successfully generated the graph pdf!!'})
        
        pdf_file <- paste(getwd(),'/Rplots.pdf',sep = '')
        
        browseURL(pdf_file)
        
        if (Sys.info()["sysname"] == "Windows") {
          system(paste("start", shQuote(pdf_file)))
        }
      }
      
    }
  })
}

shinyApp(ui, server)
