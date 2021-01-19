

server <- function(input, output,session) {
  
  output$text <- renderUI({
                    if(input$na_str=="other")
                    {
                    textInput("na_text", "Input", "please enter NA string")
                    }
                        })
  # Tab-1----
  
  data <- reactive({
            if (is.null(input$file)) { 
              return(VIM::sleep)
              
              
              
              }
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
  
  # tab-2 Non-Metric Detection & Conversion ------
  data_fr_str <- reactive({data_frame_str(data())})
  
  
  output$df_str <- renderDataTable({
    data_fr_str()
  })
  
  
  #if unique value of any column is less than 10 and it is numeric then select it automatically for conversion
  output$vars2conv_fact <- renderUI({
    #if (is.null(input$file)) { return(NULL) }
    #else{
      cond_df <- data_fr_str() %>% filter((class=="numeric"| class=="integer") & unique_value_count<7)
      cols <- cond_df$variable
      selectInput("selVar2conv",
                  label = "Select columns for factor conversion ",
                  choices=names(data()),
                  multiple = TRUE,
                  selectize = TRUE,
                  selected=cols)
   # }
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
  
  output$df_conv_str <- renderDataTable({head(data_frame_str(data_fac()),n=10)})

 
  
  # tab-3 Missing Value----
  
  # sub-tab-1
  output$missing_plot <- renderPlot({
    display_missing_percentage(data_fac())
  })
  
  
  # sub-tab-2
  miss_cols_list <- reactive({miss_cols(data_fac())})
  
  
  # list of integer column for imputation
  output$intvars2imp <- renderUI({
   # if (is.null(input$file)) { return(NULL) }
   # else{
      int_cols <- miss_cols_list()
      cols <- int_cols[[1]]
      selectInput("IntVar2Imp",
                  label = "Select numerical columns for imputation ",
                  choices=names(data_fac()),
                  multiple = TRUE,
                  selectize = TRUE,
                  selected=cols)
   # }
  })

  # list of categorical columns for imputation  
  output$catvars2imp <- renderUI({
    #if (is.null(input$file)) { return(NULL) }
   # else{
      cat_cols <- miss_cols_list()
      cols <- cat_cols[[2]]
      selectInput("CatVar2Imp",
                  label = "Select categorical columns for imputation",
                  choices=names(data_fac()),
                  multiple = TRUE,
                  selectize = TRUE,
                  selected=cols)
   # }
  })
  
  
  # imputed_df <- reactive({
  #   imputer(df = data_fac(),
  #           num_method = input$selIntImpMethod,
  #           cat_method = input$selCatImpMethod,
  #           int_cols = input$IntVar2Imp,
  #           cat_cols = input$CatVar2Imp,
  #           original_flag = TRUE
  #           )
  # })
  
  values1 <- reactiveValues(df_imputed = NULL) 
  values2 <- reactiveValues(replaced_by = NULL) 
  
  observeEvent(input$imp, {
    
    if(is.null(input$IntVar2Imp)){
      values1$df_imputed <- data_fac()
      sendSweetAlert(
        session = session,
        title = "Warning !!! Select Variable",
        text = NULL,
        type = "warning"
      )
      
    }else{
      df <- data_fac()
      imputed_df <-  imputer(df = df,
                             num_method = input$selIntImpMethod,
                             cat_method = input$selCatImpMethod,
                             int_cols = input$IntVar2Imp,
                             cat_cols = input$CatVar2Imp,
                             original_flag = input$orig_col
      )
      values1$df_imputed <- imputed_df[[1]]
      values2$replaced_by <- imputed_df[[2]]
      sendSweetAlert(
        session = session,
        title = "Imputed Successfully !!",
        text = "All in order",
        type = "success"
      )
    }
    
  
    
  })
  
  
  
  output$int_imp <- renderDataTable({head(values1$df_imputed,10)})
  
  output$int_replacement <- renderDataTable({values2$replaced_by})
  
  
  output$imp_download <- downloadHandler(
    filename = function() {
      paste("imputed", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values1$df_imputed, file, row.names = FALSE)
    }
  )
  
  
  
  # tab-4 Dummy Variables------
  
  output$vars2conv <- renderUI({
                      #if (is.null(input$file)) { return(NULL) }
                     # else{
                         df <- values1$df_imputed
                         cols <- names(df%>%select_if(is.factor))
                         selectInput("selVar",
                         label = "Select columns for conversion ",
                         choices=names(df),
                         multiple = TRUE,
                         selectize = TRUE,
                         selected=cols)
                       #    }
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

