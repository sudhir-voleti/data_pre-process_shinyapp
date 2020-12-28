

server <- function(input, output) {
  
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
                             header = input$header
                             )
              return(df)
            }
                  })
  
  
  output$head <- renderDataTable({head(data(),n = 10)})
  
  # tab-2 coding
  output$org_text <- renderPrint({str(data())})
  
  

  
  
  output$vars2conv <- renderUI({
                      if (is.null(input$file)) { return(NULL) }
                      else{
                         cols <- names(data()%>%select_if(is.character))
                         selectInput("selVar",
                         label = "Select columns for conversion ",
                         choices=names(data()),
                         multiple = TRUE,
                         selectize = TRUE,
                         selected=cols)
                           }
                             })
  
  
   dummy_data <- reactive({
                        if(length(input$selVar)==0){return(data())}
                        else{
                            dummy_df <- dummy_columns(data(),
                                                      select_columns = input$selVar,
                                                      remove_selected_columns = input$rem_org,
                                                      remove_first_dummy = input$rem_first_dum,
                                                      ignore_na = input$ign_NA)
                        return(dummy_df)
                            }
                          })
  
   
   output$dummy_table <- renderDataTable({
                                  head(dummy_data(),
                                       n=10)})
   
   
   output$dummy_download <- downloadHandler(
                                      filename = function() {
                                      paste("dummy", ".csv", sep = "")
     },
     content = function(file) {
       write.csv(dummy_data(), file, row.names = FALSE)
     }
   )
   
   
   
   

}

