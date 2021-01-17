

server <- function(input, output,session) {
  
  output$text <- renderUI({
                    if(input$na_str=="other")
                    {
                    textInput("na_text", "Input", "please enter NA string")
                    }
                        })
  
  
  data <- reactive({
            if (is.null(input$file)) { return(NULL) }
            else{
              df <- read.csv(input$file$datapath,
                             sep = input$sep,
                             na.strings = if(input$na_str=="other"){input$na_text}else{input$na_str},
                             header = input$header,stringsAsFactors = TRUE
                             )
              return(df)
            }
                  })
  
  
  output$head <- renderDataTable({head(data(),n = 10)})
  
  # tab-2 Non-Metric Detection & Conversion
  data_fr_str <- reactive({data_frame_str(data())})
  
  
  output$df_str <- renderDataTable({
    data_fr_str()
  })
  
  
  #if unique value of any column is less than 10 and it is numeric then select it automatically for conversion
  output$vars2conv_fact <- renderUI({
    if (is.null(input$file)) { return(NULL) }
    else{
      cond_df <- data_fr_str() %>% filter((class=="numeric"| class=="integer") & unique_value_count<7)
      cols <- cond_df$variable
      selectInput("selVar2conv",
                  label = "Select columns for factor conversion ",
                  choices=names(data()),
                  multiple = TRUE,
                  selectize = TRUE,
                  selected=cols)
    }
  })
  
  
  values <- reactiveValues(df_data = NULL) 
  
  observeEvent(input$conv, {
    
    data_copy <- data()
    Factors <- input$selVar2conv
    print(data_copy[Factors])
    data_copy[Factors]<-lapply(data_copy[Factors],factor)
    print(str(data_copy))
    values$df_data <- data_copy
    
  })
  
  
  
  data_fac <- reactive({values$df_data}) # convereted factorial dataset
  
  output$df_conv_str <- renderDataTable({data_frame_str(data_fac())})

  # tab-3 Missing Value
  
  
  output$missing_plot <- renderPlot({
    display_missing_percentage(data_fac())
  })
  
  
  
  
  
  
  
  
  
  # tab-4 Dummy Variables
  
  output$vars2conv <- renderUI({
                      if (is.null(input$file)) { return(NULL) }
                      else{
                         cols <- names(data_fac()%>%select_if(is.factor))
                         selectInput("selVar",
                         label = "Select columns for conversion ",
                         choices=names(data()),
                         multiple = TRUE,
                         selectize = TRUE,
                         selected=cols)
                           }
                             })
  
  
  
  values_dummy <- reactiveValues(dummy_df = NULL) 
  
  observeEvent(input$conv2, {
    
    if(length(input$selVar)==0){return(data())}
    else{
      if (input$selVar == ""){
        values_dummy$dummy_df <- data_fac()
      }else{
        
        dummy_df <- dummy_columns(data_fac(),
                                  select_columns = input$selVar,
                                  remove_selected_columns = input$rem_org,
                                  remove_first_dummy = input$rem_first_dum,
                                  ignore_na = input$ign_NA)
        values_dummy$dummy_df <- dummy_df
      }
     
    }
    
    
  })
  
  
  
  data_fac_dummy <- reactive({values_dummy$dummy_df}) # convereted factorial dataset
  
   # dummy_data <- reactive({
   #                      if(length(input$selVar)==0){return(data())}
   #                      else{
   #                          dummy_df <- dummy_columns(data(),
   #                                                    select_columns = input$selVar,
   #                                                    remove_selected_columns = input$rem_org,
   #                                                    remove_first_dummy = input$rem_first_dum,
   #                                                    ignore_na = input$ign_NA)
   #                      return(dummy_df)
   #                          }
   #                        })
  
   
   output$dummy_table <- renderDataTable({
                                  head(data_fac_dummy(),
                                       n=10)})
   
   
   output$dummy_download <- downloadHandler(
                                      filename = function() {
                                      paste("dummy", ".csv", sep = "")
     },
     content = function(file) {
       write.csv(data_fac_dummy(), file, row.names = FALSE)
     }
   )
   
   
   
   

}

