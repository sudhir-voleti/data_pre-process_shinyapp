# 06-navlist.R

library(shiny)
library(shinythemes)
library(dplyr)
library(fastDummies)

ui <- fluidPage(theme = shinytheme("cerulean"),
                tagList(
                  tags$head(tags$script(type="text/javascript", src = "code.js")),
                  navbarPage(title="Basic Data Analysis",
                             tabPanel(title = "Upload Data",
                                      #titlePanel("Upload data for imputation"),
                                      
                                      
                                      #---code for example datasets will come
                                      
                                      # fluidRow(
                                      #   hr(),
                                      #   h3("Upload Example Datasets"),
                                      #   
                                      #   
                                      #   
                                      # ),
                                      
                                      
                                      
                                      fluidRow
                                      (
                                        
                                        hr(),
                                        h3("Upload Data"),
                                        column(5,
                                               fileInput('file',
                                                         label = 'Choose File',
                                                         placeholder = 'No File Selected')
                                        ),
                                        column(3,
                                               radioButtons("sep", 
                                                            label = "Seperator",
                                                            choices = list("Comma" = ',', "Semicolon" = ';', "Tab" = '\t',"Space"=' '), 
                                                            selected = ",")
                                        ),
                                        # column(2,radioButtons("quote", label = "Quote",
                                        #                       choices = list("None" = 1, "Double Quote" = 2, "Single Quote" = 3), 
                                        #                       selected = 1)
                                        #        ),
                                        column(3,radioButtons("na_str", label = "Select NA Values in Dataset",
                                                              choices = list("." = ".", "NA" = "NA","Other"="other"),
                                                              selected = "NA"),
                                               uiOutput("text")
                                        ),
                                        
                                        column(1,checkboxInput(inputId = "header",
                                                               label = "Header",value = TRUE
                                        )
                                        )
                                      ),#end of fluid row-1
                                      hr(),
                                      fluidRow(
                                        h3("Review Data"),
                                        dataTableOutput('head')
                                      )
                                      
                             ),# end of tabpanel-1
                             
                             
                             tabPanel(title="Non-Metric Detection & Conversion",
                                      sidebarLayout(
                                        sidebarPanel(
                                          h4("Select columns to convert"),
                                          uiOutput('vars2conv_fact'),
                                          hr(),
                                          actionButton("conv", "Convert"),
                                        ),
                                        mainPanel(
                                          h4("Uploaded Data Structure"),
                                          dataTableOutput('df_str'),
                                          h4("Data structure after conversion"),
                                          dataTableOutput("df_conv_str")
                                        )
                                          
                                        )
                                      ),
                             
                             
                             tabPanel(title = "Missing Value",
                                      sidebarLayout(
                                        sidebarPanel(
                                          sidebarMenu("ih")
                                      
                                        
                                      ),
                                      mainPanel(
                                        h4("Missing Value Distribution"),
                                        plotOutput("missing_plot")
                                        
                                        
                                      )
                                      
                                      )
                             ),
                             
                             tabPanel(title = "Dummy Variables",
                                      sidebarLayout(
                                        sidebarPanel(
                                            h4("Select columns to encode"),
                                            uiOutput('vars2conv'),
                                          hr(),
                                          checkboxInput("rem_org",
                                                        label = "Keep original columns",
                                                        value = TRUE
                                          ),
                                          hr(),
                                          helpText("Removes the first dummy and keep only n-1 dummies."),
                                          checkboxInput("rem_first_dum","Remove first dummy",
                                                        value = FALSE),
                                          hr(),
                                          helpText("Create new column for missing value or ignore"),
                                          checkboxInput("ign_NA","Ignore NA",
                                                        value = TRUE),
                                          hr(),
                                          actionButton("conv2", "Convert",icon = ),
                                          downloadButton("dummy_download")
                                        ),
                                        
                                        mainPanel(
                                          h3("Uploaded Data Structure"),
                                          
                                          hr(),
                                          h3("Review converted dummy data"),
                                          
                                          dataTableOutput('dummy_table')
                                        )
                                      )
                             )
                             
                             
                             
                  )# end of navbarpage
                  
                  
                )# end of taglist
  
  )# end of fluidpage

  
  
  
  
  
