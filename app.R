## app.R ##
library(shinydashboard)
library(shiny)
library(shinyWidgets)

ui <- dashboardPage(skin = "green",
  dashboardHeader(title = "Direct Second Year Diploma to degree college predictor",titleWidth = 600),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home Page", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Seat,SLGMN Prediction", tabName = "widgets", icon = icon("th")),
      menuItem("College Prediction", tabName = "sample", icon = icon("th"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              tags$h2("Welcome to Direct Second Year College Predictor..."),
              tags$h3("Are you diploma holder???...and worring about admission in Government college for  B.E./B.Tech"),
              tags$h3("Then you are on right place...."),
              tags$h3("You can.."),
              tags$h3("1.Predict number of seats of each seat type for year next year"),
              tags$h3("2.Predict cut off  SLGMN wise and Percentage wise of each seat type  "),
              tags$h3("3.Probability of getting admission to college on the basis of marks   and SLGMN"),
              tags$h3("Government and Autonoums Engineering Colleges in Maharashtra.."),
              tags$h4("1.Government College Of Engineering,Amravati"),
              tags$h4("2.Government College of Engineering, Aurangabad"),
              tags$h4("3.Government College of Engineering, Jalgaon"),
              tags$h4("4.College of Engineering, Pune"),
              tags$h4("5.Sardar Patel College of Engineering, Andheri"),
              tags$h4("6.Government College of Engineering, Avasari"),
              tags$h4("7.Veermata Jijabai Technological Institute(VJTI), Matunga, Mumbai"),
              tags$h4("8.Shri Guru Gobind Singhji Institute of Engineering and Technology, Nanded"),
              tags$h4("9.Government College of Engineering, Karad"),
              tags$h4("10.Walchand College of Engineering, Sangli")
              
              
              
              
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              mainPanel(
                
                plotOutput("phonePlot"),
                plotOutput("phonePlot1"),
                plotOutput("phonePlot2")
              ),
              fluidRow(
               
                  sidebarPanel(
                    uiOutput("data1") ## uiOutput - gets the UI from the server
                  ),
                 
                    sidebarPanel(
                      uiOutput("data2") ## uiOutput - gets the UI from the server
                    ),
                  sidebarPanel(
                    selectInput("gender", "Gender:",
                                c("Male" = "male",
                                  "Female" = "female"
                                ))
                  ),
                 
                    sidebarPanel(
                      selectInput('IDID', "select Branch ", "")
                    ),
                  sidebarPanel(
                    selectInput('seatT', "Seat Type","")
                    
                  ),
                  box(
                    title = "Prediction by calculating Average for Next Year", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    
                      
                      textInput("inText", "Predicted Seats:"),
                      
                      textInput("inText1", "Predicted SLGMN wise Cut off:"),
                      
                      textInput("inText26", "Predicted cut off wise Cut off:")
                    
                    
                  )
          
       
     ,
      box(
        title = "Prediction by using Regression", status = "primary", solidHeader = TRUE,
        collapsible = TRUE,
        
          
          selectInput(
            inputId =  "date_from", 
            label = "Select Year:", 
            choices = 2019:2100
          )
          ,
          textInput("inTextseat", "Predicted Seats:"),
          
          textInput("inTextslgmn", "Predicted SLGMN wise Cut off:"),
          
          textInput("inTextcut", "Predicted cut off wise Cut off:"),
          actionButton("do23", "Click Me")
          
        
      )
      
      )
      ),
      tabItem( tags$head(tags$script(HTML('
      Shiny.addCustomMessageHandler("jsCode",
                                          function(message) {
                                          eval(message.code);
                                          }
      );
                                          '))),tabName = "sample",
              mainPanel(
                plotOutput("pie1"),
                plotOutput("pie11")
              ),
              fluidRow(
                sidebarPanel(
                  uiOutput("data12") ## uiOutput - gets the UI from the server
                ),
                
                sidebarPanel(
                  uiOutput("data13") ## uiOutput - gets the UI from the server
                ),
                sidebarPanel(
                  selectInput("gender1", "Gender:",
                              c("Male" = "male",
                                "Female" = "female"
                              ))
                ),
                
                sidebarPanel(
                  selectInput('IDID1', "select Branch ", "")
                ),
                sidebarPanel(
                  selectInput('seatT1', "Seat Type","")
                  
                ),
                sidebarPanel(
                  textInput("inText22", "Enter Diploma Marks:"),
                  textInput("ggg", "Percentage of Your's:"),
                  textInput("inText222", "Enter Diploma Marks:"),
                  textInput("ggg1", "Percentage of Your's:")
                )
                
                
              )
      )
      
    )
  )
)


server <- function(input, output,session) {
  library(readr)
  library(dplyr)
  library(jsonlite)
  library(readxl)
 #Book3 <- read_xlsx("C:/Users/WIN 10/Desktop/template/cuttoff.xlsx")
  #Book3<- read.csv("finalcutoff.csv")
  Book3<- read.csv("cutofcsv.csv")
  cs<-read.csv("category_seats.csv")
  seat1 <- read.csv("projectseat.csv")
  sl<-read.csv("SLGMNfinalincsv.csv")
  output$data1 <- renderUI({
    selectInput("data1", "Select College", choices = (Book3$college))
  })
  
  output$data12 <- renderUI({
    selectInput("data12", "Select College", choices = (Book3$college))
  })
  
  output$data2 <- renderUI({
    selectInput("data2", "Select Cetegory", choices = (cs$category))
  })
  
  output$data13 <- renderUI({
    selectInput("data13", "Select Cetegory", choices = (cs$category))
  })
  
  sel= reactive({
    ret16<-subset(Book3,college==input$data1)
    return (ret16$branch)
    
  } )
  
  
  
  sel1= reactive({
    ret17<-subset(cs,category==input$data2 &gender==input$gender)
    
    return (ret17$seatType)
    
  } )
  
  sel2= reactive({
    ret18<-subset(Book3,college==input$data12)
    return (ret18$branch)
    
  } )
  
  
  
  sel3= reactive({
    ret19<-subset(cs,category==input$data13 &gender==input$gender1)
    
    return (ret19$seatType)
    
  } )
  
  
  observeEvent(input$do22, {
    retval122<-subset(Book3,college==input$data12 &branch==input$IDID1)
    req(input$seatT1)
    fff122=input$seatT1
    dip<-input$inText22
    rettval<-select(retval122,fff122)
    finall<-subset(rettval,rettval[[fff122]]!=0)
    countt=colSums(finall>dip)/nrow(finall)*100
   updateTextInput(session,inputId = "ggg",value=first(countt))
  })
  
  
  observeEvent(input$do23, {
    library(readr)
    datee<-as.numeric(input$date_from)
    retval1<-subset(seat1,college==input$data1 &branch==input$IDID)
    fff=input$seatT
    req(input$seatT)
    
    relation <- lm(retval1[[fff]]~YEAR,data=retval1)
    
    a <- data.frame(YEAR=datee)
    
    result <-  predict(relation,a)
    
    retval124<-subset(Book3,college==input$data1 &branch==input$IDID)

    relation1 <- lm(retval124[[fff]]~YEAR,data=retval124)
    
    result1 <-  predict(relation1,a)
    
    retval12<-subset(sl,college==input$data1 &branch==input$IDID)
    
    relation2 <- lm(retval12[[fff]]~YEAR,data=retval12)
    
    result2 <-  predict(relation2,a)

    updateTextInput(session,inputId = "inTextseat",value=first(floor(result)))
    updateTextInput(session,inputId = "inTextcut",value=first(floor(result1)))
    updateTextInput(session,inputId = "inTextslgmn",value=first(floor(result2)))
    
    
  })
  
  randomVals <- eventReactive(input$do, {
    runif(input$seatT)
  })
 
  output$pie1 <- renderPlot({
    library(plotrix)
    retval122<-subset(sl,college==input$data12 &branch==input$IDID1)
    req(input$seatT1)
    fff122=input$seatT1
    dip<-input$inText22
    rettval<-select(retval122,fff122)
    finall<-subset(rettval,rettval[[fff122]]!=0)
    countt=colSums(finall>dip)/nrow(finall)*100
    not=100-countt
    req(input$seatT1)
    pie11<-c(floor(first(not)),floor(first(countt)))
    piepercent<- round(100*pie11/sum(pie11), 1)
    label1<-c("You can not get seat","you can get seat")
    lbls <- paste(label1, "\n", piepercent, sep="")
    pie3D(pie11,labels=lbls,explode=0.1,main="Percentage of Having College",col = rainbow(length(pie11)))
    legend("topright", c("You can not get seat","you can get seat"), cex = 0.8,
           fill = rainbow(length(pie11)))
    updateTextInput(session,inputId = "ggg",value=first(countt))
    
  })
  output$pie11 <- renderPlot({
    library(plotrix)
    retval122<-subset(Book3,college==input$data12 &branch==input$IDID1)
    req(input$seatT1)
    fff122=input$seatT1
    dip<-input$inText222
    req(dip)
    rettval<-select(retval122,fff122)
    print(rettval)
    finall<-subset(rettval,rettval[[fff122]]!=0)
    print(finall)
    countt=colSums((finall>dip))/nrow(finall)*100
    print(countt)
    not=100-countt
    req(input$seatT1)
    pie11<-c(floor(first(not)),floor(first(countt)))
    piepercent<- round(100*pie11/sum(pie11), 1)
    label1<-c("You can not get seat","you can get seat")
    lbls <- paste(label1, "\n", piepercent, sep="")
    pie3D(pie11,labels=lbls,explode=0.1,main="Percentage of Having College",col = rainbow(length(pie11)))
    legend("topright", c("You can not get seat","you can get seat"), cex = 0.8,
           fill = rainbow(length(pie11)))
    updateTextInput(session,inputId = "ggg1",value=first(countt))
    
  })
  output$phonePlot <- renderPlot({
    retval1<-subset(seat1,college==input$data1 &branch==input$IDID)
    x<-retval1$YEAR
    fff1=input$seatT
    req(input$seatT)
    y=retval1[[fff1]]
    xx1<- barplot(y,names.arg=x,col=c("darkblue","red","green","darkviolet"),legend.text=x, args.legend=list(bty="n",horiz=TRUE), main="No. Of Seats Per Year",
                  xlab="Year",ylab="No. Of seats")
    text(x = xx1, y = y, label = y, pos = 3, cex = 0.8, col = "red")
    
    retval1<-subset(seat1,college==input$data1 &branch==input$IDID)
    fff=input$seatT
    req(input$seatT)
    meane=mean(retval1[[fff]])
    
    if(meane!=0)
    {
      if(meane<1)
      {
        if(mean>0.5)
        {
          meane=1;
        }
      }
    }
    
    updateTextInput(session,inputId = "inText",value=floor(meane))
    session$sendCustomMessage(type="jsCode",
                              list(code= "$('#inText').prop('disabled',true)"))
   
  })
  output$phonePlot1 <- renderPlot({
    retval11<-subset(sl,college==input$data1 &branch==input$IDID)
    x1<-retval11$YEAR
    print(x1)
    req(input$seatT)
    fff11=input$seatT
    y1=retval11[[fff11]]
    print(y1)
   xx<- barplot(y1,names.arg=x1,col=c("darkblue","red","green","darkviolet"), main="SLGMN wise cutoff Per Year",
            xlab="Year",ylab="No. Of seats", width = 0.85,legend.text=x1, args.legend=list(bty="n",horiz=TRUE))
   text(x = xx, y = y1, label = y1, pos = 3, cex = 0.8, col = "red")
   
   retval12<-subset(sl,college==input$data1 &branch==input$IDID)
   fff12=input$seatT
   req(input$seatT)
   meane1=mean(retval12[[fff12]])
   print(meane1)
   updateTextInput(session,inputId = "inText1",value=floor(meane1))
   session$sendCustomMessage(type="jsCode",
                             list(code= "$('#inText1').prop('disabled',true)"))
   
   
   
  })
  output$phonePlot2 <- renderPlot({
    retval101<-subset(Book3,college==input$data1 &branch==input$IDID)
    x01<-retval101$YEAR
    print(x01)
    req(input$seatT)
    fff101=input$seatT
    y01=retval101[[fff101]]
    print(y01)
    xx<- barplot(y01,names.arg=x01,col=c("darkblue","red","green","darkviolet"), main="SLGMN wise cutoff Per Year",
                 xlab="Year",ylab="No. Of seats", width = 0.85,legend.text=x01, args.legend=list(bty="n",horiz=TRUE))
    text(x = xx, y = y01, label = y01, pos = 3, cex = 0.8, col = "red")
    
    retval124<-subset(Book3,college==input$data1 &branch==input$IDID)
    fff124=input$seatT
    req(input$seatT)
    meane14=mean(retval124[[fff124]])
    print(meane14)
    updateTextInput(session,inputId = "inText26",value=meane14)
    session$sendCustomMessage(type="jsCode",
                              list(code= "$('#inText26').prop('disabled',true)"))
  })
  
    
  observe({
    updateSelectInput(session, inputId = "IDID",
                      choices = sel()
    )
    updateSelectInput(session, inputId = "seatT",
                      choices = sel1()
    )
    updateSelectInput(session, inputId = "IDID1",
                      choices = sel2()
    )
    updateSelectInput(session, inputId = "seatT1",
                      choices = sel3()
    )
    
  
    
    })
  
 
   
    
    
}

shinyApp(ui, server)