#Kieran Brady 
#12343851
#Semi-Automatic Quoting System for the Fabrication Engineering Sector
#Dashboard


#Load Packages
library(shinydashboard)
library(shiny)
library(dplyr)

#configure dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Ard Precision"),  #dashboard heading
  dashboardSidebar( #dashboard side menu options
    sidebarMenu(
      menuItem("Quotation", tabName = "Quotation", icon= icon("th")),
      menuItem("Tracking", tabName = "Tracking", icon= icon("th"))
      
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Quotation", #Quoting tab layout
              titlePanel("Ard Precision Quotation System"), #tab title
              
              fluidRow(column(10,offset=1,textInput("QuoteNo","Quote Ref. No:","Q000"), #quote reference number input
                              textInput("Customer","Customer:"))),
              
              fluidRow(column(10,offset=1,textAreaInput("desc","Job Description",cols = 200, rows = 6))), # #job description input
              
              fluidRow(column(5, offset=1,
                              numericInput('Weight','Weight (Ton)', 0,min=1, max = 300,step = 1), #weight input
                              numericInput('Weld','Welds (m)', 0,min=0, max = 500,step = 1), #weld input
                              numericInput('DrilledHoles','Drilled Holes ', 0,min=0, max = 100,step = 1)), #drilled holes input
                              column(4, offset=1, numericInput('Bends','No. of Bends', 0,min=0, max = 50,step = 1), #bends input
                              numericInput('PlasmaCutting','Plasma Cutting (m)', 0,min=0, max = 500,step = 1))), #plasmacutting input
              
              fluidRow(column(3,offset = 5, actionButton("submit","Submit"))), #submit input for prediction
              
              
              fluidRow(column(5, offset = 4,
                              tableOutput("breakdown"))), # quote breakdown table
              
              fluidRow(column(5, offset = 5,
                              tableOutput("total"))), #totals table 
              
              
              fluidRow(column(4, offset= 2,downloadButton("quote", "Save PDF")),column(4, offset= 2,downloadButton("quoteW", "Save Word")))), #export to pdf or word document
      
      tabItem(tabName = "Tracking", #job tracking and evaluation tab
              
              titlePanel("Job Tracking & Evaluation"), #header
              titlePanel("Load Quotation"), #header
              
              fluidRow(column(10,offset=1,textInput("QuoteNo1","Quote Ref. No:","Q0000"),
                              textInput("Customer1","Customer:"))), #load quote
              
              
              fluidRow(column(5,offset = 4,
                              tableOutput("loadQ"))), 
              headerPanel("Actual Costs"), #print quote details
      
              fluidRow(column(5, offset=1,
                      numericInput('Weight1','Weight (Ton)', 0,min=1, max = 300,step = 1), #input actual weight
                      numericInput('Weld1','Welds (m)', 0,min=0, max = 500,step = 1), # input actual weld
                      numericInput('DrilledHoles1','Drilled Holes ', 0,min=0, max = 100,step = 1)), #input actual drilled holes
                      column(4, offset=1, numericInput('Bends1','No. of Bends', 0,min=0, max = 50,step = 1), #input actual bends
                      numericInput('PlasmaCutting1','Plasma Cutting (m)', 0,min=0, max = 500,step = 1))), #input actual plasmacutting
      
             fluidRow(column(3,offset = 5, actionButton("submit1","Submit"))), #submit for comparison
             
             titlePanel("Quoted vs. Actual Breakdown in Euro"), #header
             fluidRow(column(3, offset = 4,
                             tableOutput("eval"))), #print comparison table
             
             titlePanel("Total Difference Euro"),
             fluidRow(column(3, offset = 4,
                             tableOutput("total1"))), #print difference from predicted to actual in euro
             
             titlePanel("To update Training Data to include this order select below:"),
             fluidRow(column(3,offset = 5, actionButton("submit2","Update Training Data")))) #update training data
    )
  )
)
 
#configure server
server <- function(input, output) { 
  
  n1 <- reactive({ #reactive function for cost prediction
    Item <- c("Weight (Ton)","Welding (m)","Drilled Holes", "Bends ","Plasma Cutting (m)" )
    set.seed(123)
    read.csv("Training Data1.csv", fileEncoding="UTF-8-BOM")->quotes;
 
    mod1<-  lm(formula=PriceMS~ Weight, quotes)
    Test1 <- data.frame(Weight=input$Weight)
    Pred1 <- predict(mod1,Test1)
    
    modW<- lm(PriceW ~ Weld,quotes)
    TestW <- data.frame(Weld=input$Weld)
    PredW <- predict(modW,TestW)
    
    modDH<- lm(PriceDH ~ DrilledHoles,quotes)
    TestDH <- data.frame(DrilledHoles=input$DrilledHoles)
    PredDH <- predict(modDH,TestDH)
    
    modB<- lm(PriceB ~ Bends,quotes)
    TestB <- data.frame(Bends=input$Bends)
    PredB <- predict(modW,TestW)
    
    modPC<- lm(PricePC ~ PlasmaCutting,quotes)
    TestPC <- data.frame(PlasmaCutting=input$PlasmaCutting)
    PredPC <- predict(modPC,TestPC)
    
    
    Euro <- c(Pred1,PredW,PredDH,PredB,PredPC)
    
  })
  
  n2 <- reactive({ #function for converting breakdown to table
    Euro <- n1()
    Item <- c("Weight (Ton)","Welding (m)","Drilled Holes", "Bends","Plasma Cutting (m)" )
    Qty <- c(input$Weight, input$Weld, input$DrilledHoles, input$Bends, input$PlasmaCutting)
    df1 <- data.frame(Item, Qty, Euro)
    df1 <- df1 %>% filter(Euro>0.005) %>% mutate_if(is.numeric, round, digits = 2)
  })
  
  n3 <- reactive({ #getting vat value of total and creating table
    TotalLV <- sum(n1())
    Vat <- 0.23*TotalLV
    Total <- Vat + TotalLV
    Euro<- c(TotalLV, Vat, Total)
    Totals <- c("Total Ex. Vat", "VAT", "Total")
    df2 <- data.frame(Totals,Euro)
    df2
  })
  

 
  
   saveQ <- function(Q){ #function for saving quote
    filename <- sprintf("%s_%s.csv", input$QuoteNo, input$Customer)
    write.csv(Q, file = file.path(filename), row.names = F, quote = T)
  }
  
  saveTD <- function(Q){ #function for saving updated training data
    filename <- sprintf("Training Data1.csv")
    write.csv(Q, file = file.path(filename), row.names = F, quote = T)
  }
  
  n4 <- reactive({ #get costs for actual values
    Item <- c("Weight (Ton)","Welding (m)","Drilled Holes", "Bends ","Plasma Cutting (m)" )
    set.seed(123)
    read.csv("Training Data1.csv", fileEncoding="UTF-8-BOM")->quotes;

    mod1<-  lm(formula=PriceMS~ Weight, quotes)
    Test1 <- data.frame(Weight=input$Weight1)
    Pred1 <- predict(mod1,Test1)
    
    modW<- lm(PriceW ~ Weld,quotes)
    TestW <- data.frame(Weld=input$Weld1)
    PredW <- predict(modW,TestW)
    
    modDH<- lm(PriceDH ~ DrilledHoles,quotes)
    TestDH <- data.frame(DrilledHoles=input$DrilledHoles1)
    PredDH <- predict(modDH,TestDH)
    
    modB<- lm(PriceB ~ Bends,quotes)
    TestB <- data.frame(Bends=input$Bends1)
    PredB <- predict(modW,TestW)
    
    modPC<- lm(PricePC ~ PlasmaCutting,quotes)
    TestPC <- data.frame(PlasmaCutting=input$PlasmaCutting1)
    PredPC <- predict(modPC,TestPC)
    
    
    ACTEuro <- c(Pred1,PredW,PredDH,PredB,PredPC)
    
  })
  
  n5 <- reactive({ #create table of actual value
    filename <- sprintf("%s_%s.csv",  input$QuoteNo1, input$Customer1)
    data <- read.csv(filename, fileEncoding="UTF-8-BOM")
    data %>% filter(Euro>0.005) %>% mutate_if(is.numeric, round, digits = 2)
  })
  
  n6 <- reactive({ #create comparison table
    Q<- n5()
    A <- n4()
    evalBD <-  data.frame(Q,A)
    evalBD <- evalBD %>% rename(Quoted = Euro) %>% rename(Actual = A)  %>% mutate(Diff = Quoted - Actual) %>% select(Item,Quoted,Actual,Diff)
   
     })
  
  n7 <- reactive({ #Get difference between predicted and actual costs
    QQ<- n6()
    Total <- sum(QQ$Diff)
    df <- data.frame(Total)
  })
  
  n8 <- reactive({ #add quote values to training data
   BB<- c(input$Weight1, input$Weld1, input$DrilledHoles1, input$Bends1, input$PlasmaCutting1)
   CC <- n4()
   TT <- c(BB,CC)
   read.csv("Training Data1.csv", fileEncoding="UTF-8-BOM")->quotes;
    quotes <- rbind(quotes,TT)
   })
  
 #outputs
  
  output$breakdown <- renderTable({n2()})#quote breakdown table
  output$total <- renderTable({n3()})#quote totals table
  output$quote <- downloadHandler( #pdf output
    filename = "Quote.html",
    content = function(file) {
      tempReport <- file.path(tempdir(), "Q0000.Rmd")
      file.copy("Q0000.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(BD = n2(), QN = input$QuoteNo, C = input$Customer, TL = n3(), QD = input$desc)
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))})
  
  observeEvent(input$submit, {saveQ(n2())}) # submit quote values
  
  output$quoteW <- downloadHandler( #word document output
    filename = "Quote.docx",
    content = function(file) {
      tempReport <- file.path(tempdir(), "Q0000.Rmd")
      file.copy("Q0000.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(BD = n2(), QN = input$QuoteNo, C = input$Customer, TL = n3(), QD = input$desc, M = input$material)
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))})
  
  output$loadQ <- renderTable({n5()})# load quote table
  output$eval <- renderTable({n6()}) # comaprison table
  output$total1 <- renderTable({n7()}) # total difference
  
  observeEvent(input$submit2, {saveTD(n8())}) #add quote data to training data
  
  }

# Run the application 
shinyApp(ui = ui, server = server)