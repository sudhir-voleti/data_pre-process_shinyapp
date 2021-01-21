

server <- function(input, output,session) {
  
  output$text <- renderUI({
                    if(input$na_str=="other")
                    {
                    textInput("na_text", "Input", "please enter NA string")
                    }
                        })
#------------------------------------------------------Tab-1 Data Upload-----------------------------------------------#
  
  data <- reactive({
            if (is.null(input$file)) { 
               
              df <- sample_data(input$ex_data)
              
              return(df)
              
              
              
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
  
  
#----------------------------------------------------Tab-2 Non-Metric Detection & Conversion ----------------------------#
  data_fr_str <- reactive({data_frame_str(data())}) # get structure of uploaded dataset
  
  
  output$df_str <- renderDataTable({
    data_fr_str()
  })
  
  
  #if unique value of any column is less than 7 and it is numeric or integer then auto-select it  for factor conversion
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
   # print(Factors)
    if(is.null(Factors)){
      values$df_data <- data_copy
    }else{
      data_copy[Factors]<-lapply(data_copy[Factors],factor)
    #  print(str(data_copy))
      values$df_data <- data_copy
    }
   # print(data_copy[Factors])
    
    
  })
  
  
  
  data_fac <- reactive({if(is.null(values$df_data))
    {
    return(data())
  }else{
      return(values$df_data)
    }
    
    }) # convereted factorial dataset
  
  output$df_conv_str <- renderDataTable({head(data_frame_str(data_fac()),n=10)})

 
  
#------------------------------------------------- Tab-3 Missing Value--------------------------------------------#
  
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
      print(int_cols)
      cols <- int_cols[[1]]
      selectInput("IntVar2Imp",
                  label = "Select numerical columns for imputation ",
                  choices=cols,
                  multiple = TRUE,
                  selectize = TRUE,
                  selected=cols)
   # }
  })
  
  output$knn_parms <- renderUI({
      if(input$selIntImpMethod!='knn'){
        return(NULL)}
      else{
        checkboxInput("ind","create replacement indicator",value = TRUE)
      }
    })
  
  
  # list of categorical columns for imputation  
  output$catvars2imp <- renderUI({
    #if (is.null(input$file)) { return(NULL) }
   # else{
      cat_cols <- miss_cols_list()
      cols <- cat_cols[[2]]
      selectInput("CatVar2Imp",
                  label = "Select categorical columns for imputation",
                  choices=cols,
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
  
  values1 <- reactiveValues(
                          # if(is.null(input$IntVar2Imp)){return(data_fac())}
                          #    else{return(NULL)}
                             df_imputed = NULL
                          ) #imputed df after factor conversion
  
  
  values2 <- reactiveValues(replaced_by = NULL) 
  
  observeEvent(input$imp, {
    
    if(is.null(input$IntVar2Imp)){
      values1$df_imputed <- data_fac()
      sendSweetAlert(
        session = session,
        title = "Information",
        text = "Since no variable is Imputed, data from previous tab will be used in Transformation tab",
        type = "info"
      )
      
    }else{
      df <- data_fac()
      imputed_df <-  imputer(df = df,
                             num_method = input$selIntImpMethod,
                             cat_method = input$selCatImpMethod,
                             int_cols = input$IntVar2Imp,
                             cat_cols = input$CatVar2Imp,
                             original_flag = input$orig_col,
                             replacement_ind = ifelse(!is.null(input$ind),input$ind,FALSE)
      )
      values1$df_imputed <- imputed_df[[1]]
      values2$replaced_by <- imputed_df[[2]]
      sendSweetAlert(
        session = session,
        title = "Imputed Successfully !!",
        text = NULL,
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
  
  #--------------------------------------------------- Tab-4 Data Transformation-------------------------------------------#
  
  values3 <- reactiveValues(trans_df = NULL) 
  
  
  df_imputed <- reactive({
    
    if(is.null(values1$df_imputed)){
    return(data())
    }else{
      return(values1$df_imputed)
    }
    })
  
  df_imputed_cols <- reactive({df_imp_str <- data_frame_str(df_imputed())
                              int_cols <- df_imp_str %>% filter(class=="numeric"| class=="integer")
                              return(int_cols$variable)
                              
                              })
 # cols_to_tr <- reactive({miss_cols(df_imputed())})

  output$std_vars <- renderUI({
    
    
    cols <- df_imputed_cols()
    # if (is.null(input$file)) { return(NULL) }
    # else{
    #int_cols <- c("a","b")
    print(cols)
    #int_cols <- cols_to_tr[[1]]
    #print(int_cols)
    #cols <- int_cols[[1]]
    selectInput("trans_cols",
                label = "Select numerical columns for imputation ",
                choices=cols,
                multiple = TRUE,
                selectize = TRUE,
                selected=cols)
    # }
  })
  
  
  observeEvent(input$transform, {
    
    if(is.null(input$trans_cols)){
      values3$trans_df <- df_imputed()
      sendSweetAlert(
        session = session,
        title = "Information",
        text = "Since no variable is selected, data from previous tab will be used in dummy tab",
        type = "info"
      )
      
    }else{
      df <- df_imputed()
      
      trans_df <- data_transform(df = df,
                                 method = input$method,
                                 cols = input$trans_cols,
                                 original_flag = input$orig_col1)
      
      
      values3$trans_df <- trans_df
      
      sendSweetAlert(
        session = session,
        title = "Transformed Successfully !!",
        text = NULL,
        type = "success"
      )
    }
    
    
    
  })
  
  
  output$trans_download <- downloadHandler(
    filename = function() {
      paste("transformed", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values3$trans_df, file, row.names = FALSE)
    }
  )
  
  
  
  # 
  # 
  # 
  # trans_df <- reactive({data_transform(df = df_imputed(),
  #                                      method = input$method,
  #                                      cols = input$trans,
  #                                      original_flag = input$orig_col1)})
  
  output$trans_df <- renderDataTable({head(values3$trans_df,10)})
  
#--------------------------------------------------- Tab-5 Dummy Variables-------------------------------------------#
  
  df_transformed <- reactive({
    
    if(is.null(values3$trans_df)){
      return(data())
    }else{
      return(values3$trans_df)
    }
  })
  
  
  
  
  
  
  
  output$vars2conv <- renderUI({
                      #if (is.null(input$file)) { return(NULL) }
                     # else{
                         df <- df_transformed()
                         cols <- names(df%>%select_if(is.factor))
                         selectInput("selVar",
                         label = "Select columns for conversion ",
                         choices=cols,
                         multiple = TRUE,
                         selectize = TRUE,
                         selected=cols)
                       #    }
                             })
  
  
  
  values_dummy <- reactiveValues(dummy_df = NULL) 
  
  observeEvent(input$conv2, {
    
    if(is.null(input$selVar)){return(data())}
    else{
      if (input$selVar == ""){
        values_dummy$dummy_df <- values3$trans_df
      }else{
        df <- values3$trans_df
        dummy_df <- dummy_columns(df,
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

