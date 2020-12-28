library(fastDummies)
library(data.table)
data=data.frame(A=c(5,2,4),B=c('male','male','female'),C=c(1,3,5))
shinyServer(
  function(input, output){
    #Select Explanatory Nominal Variables 
    output$ColumnSelector_dummy <- renderUI({
      selectInput("SelectedDummy","Select Nominal Variables (values: male,female)", 
                  choices = as.list(names(data)),multiple=TRUE, selected = NULL)
    })
    #
    df_subset_dummy <- reactive({
      a <- subset(data, select = input$SelectedDummy)
      return(a) 
    })
    #Convert Nominal variables to Dummy variables 
    df_subset_dummy_tranformed <- reactive({
      df1 <- lapply( df_subset_dummy(), factor)
      df2 <- fastDummies::dummy_cols(df1)
      drops <- names(df1)
      df3 <- df2[, !(names(df2) %in% drops)]
      return(df3)
    })
    #Select Explanatory Ordinal Variables
    output$ColumnSelector_ordinal<- renderUI({
      selectInput("SelectedOrdinal","Select Ordinal Variables (values: 1,2,3,4,5,6)", 
                  choices = as.list(names(data)), multiple=TRUE,selected = NULL )
    }) 
    df_subset_ordinal <- reactive({
      a <- subset(data, select = input$SelectedOrdinal)
      return(a) 
    })  
    #Join Ordinal and Nominal dataframes   
    df_nominal_ordinal_bind <- reactive({  
      df <- cbind(df_subset_dummy_tranformed(),df_subset_ordinal()) 
      return(df)
    })
    output$table_ordinal_nominal <- renderTable(head(df_nominal_ordinal_bind()))  
  })

shinyUI(
  fluidPage(
    tabsetPanel(
      tabPanel("Data", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(
                   uiOutput("ColumnSelector_dummy"),  
                   uiOutput("ColumnSelector_ordinal")
                 ),
                 mainPanel(
                   tabsetPanel(
                     tabPanel('Subsets',
                              tableOutput('table_ordinal_nominal')
                     ) )) ) ))))