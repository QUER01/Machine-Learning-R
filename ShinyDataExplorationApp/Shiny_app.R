#install.packages("dplyr")
#install.packages("GGally")
#install.packages("FactoMineR")
#install.packages("Boruta")
#install.packages("shiny")
#install.packages("ggplot2")
#install.packages("car")
#install.packages("FactorMineR")

library(Boruta)
library(shiny)
library(ggplot2)
library(dplyr)
library(car)
library(GGally)
#library(FactoMineR)
#myfile <- file.choose()
#data <- read.csv(myfile, header=TRUE, sep=",")
data <- datasets::iris
ui <- fluidPage(
  titlePanel("Data Exploration"),
  sidebarLayout(
    sidebarPanel(
      
      fileInput("file","Upload the file"), # fileinput() function is used to get the file upload contorl option
      helpText("Default max. file size is 5MB"),
      tags$hr(),
      h5(helpText("Select the read.table parameters below")),
      checkboxInput(inputId = 'header', label = 'Header',value = TRUE),
      checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
      br(),
      radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ',')
      ,
      
      
      selectInput("Primary_Col_Input", "Primary Column", choices = colnames(data), selected =colnames(data[1])),
      selectInput("Secondary_Col_Input", "Secondary Column", choices = colnames(data),selected = colnames(data[1])),
      checkboxGroupInput("Col_Input", "Selected Columns", choices = colnames(data) , selected =  colnames(data))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("SummaryNew", 
                 fluidRow(
                   column(12,dataTableOutput("filedf"))
                   
                 )),
        tabPanel("Summary", 
                 fluidRow(
                   column(12,"Summary"),
                   column(12, dataTableOutput("plot1")),
                   column(12,"Bar Chart"),
                   column(12,plotOutput("plot1.1"))
                 )),
        tabPanel("Histogram",
                 fluidRow(
                   column(12,"Histogram"),
                   column(12, plotOutput("plot2")),
                   column(12,"QQ plot"),
                   column(12, plotOutput("plot2.1")),
                   column(12,"log QQ plot"),
                   column(12, plotOutput("plot2.2")),
                   column(12,"exp QQ plot"),
                   column(12, plotOutput("plot2.3"))
                 )),
        tabPanel("Correlations",
                 fluidRow(
                   column(12,"Scatterplot"),
                   column(12, plotOutput("plot3")),
                   column(12,"Correlation Matrix"),
                   column(12, plotOutput("plot3.1"))
                 )),
        tabPanel("Correlation Matrix",
                 fluidRow(
                   column(12,"Correlation Matrix"),
                   column(12, plotOutput("plot4.1")),
                   column(12, plotOutput("plot4.2")),
                   column(12, plotOutput("plot4.3")),
                   column(12, plotOutput("plot4.4"))
                 )),
        tabPanel("Dimension Reduction",
                 fluidRow(
                   column(12,"Boruta output: please wait until R has computed your results."),
                   actionButton("Start_Boruta", "Start calculating decission tree with pruning"),
                   column(12, dataTableOutput("plot5.1"))
                   #,column(12, plotOutput("plot5.2"))
                 ))
        
        
      )
    )
  )
)
server <- function(input, output, session) {
  
  # This reactive function will take the inputs from UI.R and use them for read.table() to read the data from the file. It returns the dataset in the form of a dataframe.
  # file$datapath -> gives the path of the file
  data <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()} 
    read.table(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
    
  })
  
  # this reactive output contains the summary of the dataset and display the summary in table format
  output$filedf <- renderDataTable({
    if(is.null(data())){return ()}
    Summary(data())
  })
  
  
  
  
  
  
  
  # Filter the data
  main_filtered <- reactive({ 
    if(is.null(data())){return ()}
    data [c(input$Col_Input,"Response")] %>% filter(      )
  })
  
  filtered <- reactive({ 
    main_filtered()[c(input$Primary_Col_Input,input$Secondary_Col_Input)] %>% filter(      )
  })
  
  
  
  
  # Plot the data
  # First plot on the right
  output$plot1 <-     renderDataTable({
    summary(filtered()) 
  })
  
  output$plot1.1 <-     renderPlot({ 
    ggplot(main_filtered(),aes(main_filtered()[c(input$Primary_Col_Input)]) ) + geom_bar()
  })
  
  # Second plot on the right
  output$plot2 <- renderPlot({  
    ggplot(filtered(), aes(filtered()[c(input$Primary_Col_Input)])) +  geom_histogram()
  })
  
  output$plot2.1 <- renderPlot({  
    ggplot(filtered(),  aes(sample=filtered()[c(input$Primary_Col_Input)])) +   stat_qq()
  })
  
  output$plot2.2 <- renderPlot({  
    ggplot(filtered(),  aes(sample=log(filtered()[c(input$Primary_Col_Input)]))) +   stat_qq()
  })
  
  output$plot2.3 <- renderPlot({  
    ggplot(filtered(),  aes(sample=exp(filtered()[c(input$Primary_Col_Input)]))) +   stat_qq()
  })
  
  # Third plot on the right
  output$plot3 <- renderPlot({  
    ggplot(filtered(),  aes(x=filtered()[c(input$Primary_Col_Input)],y =filtered()[c(input$Secondary_Col_Input)] )) + geom_point()
  })
  
  output$plot3.1 <-     renderPlot({ 
    ggcorr(filtered(), label = TRUE, label_size = 3, label_round = 2, label_alpha = TRUE)
  })
  
  
  output$plot4.1 <-     renderPlot({
    ggcorr(main_filtered()[1:15], label = TRUE, label_size = 3, label_round = 2, label_alpha = TRUE)
  })
  
  
  output$plot4.2 <-     renderPlot({
    ggcorr(main_filtered()[16:31], label = TRUE, label_size = 3, label_round = 2, label_alpha = TRUE)
  })
  
  
  output$plot4.3 <-     renderPlot({ 
    ggcorr(main_filtered()[32:47], label = TRUE, label_size = 3, label_round = 2, label_alpha = TRUE)
  })
  
  
  output$plot4.4 <-     renderPlot({ 
    ggcorr(main_filtered()[48:63], label = TRUE, label_size = 3, label_round = 2, label_alpha = TRUE) 
  })
  
  
  observeEvent(input$Start_Boruta, 
               {
                 #boruta.train <- Boruta(Response~., data = main_filtered()[], doTrace = 2, maxRuns = 11)
                 boruta.train <- Boruta(Response~., data = main_filtered()[], doTrace = 2)
                 final.boruta <- TentativeRoughFix(boruta.train)
                 boruta.df <- attStats(final.boruta)
                 boruta.df[["names"]] <- rownames(boruta.df)
                 output$plot5.1 <-     renderDataTable({ 
                   boruta.df[c(7,1:6)]
                 })
                 
               })
  
  
  
  
  
}

shinyApp(ui = ui, server = server)