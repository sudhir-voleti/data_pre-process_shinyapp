# 06-navlist.R

library(shiny)
library(shinythemes)
library(dplyr)
library(fastDummies)

ui <- fluidPage(theme = shinytheme("cerulean"),
                tagList(
                  tags$head(tags$script(type="text/javascript", src = "code.js")),
                  navbarPage(title="Dummy Encoder",
                             tabPanel(title = "Upload Data",
                                      #titlePanel("Upload data for imputation"),
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
                                        column(3,radioButtons("na_str", label = "NA Strings",
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
                             tabPanel(title = "Create Dummy Variables",
                                      sidebarLayout(
                                        sidebarPanel(
                                            h4("Select columns and options for dummy encoding"),
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
                                          downloadButton("dummy_download")
                                        ),
                                        
                                        mainPanel(
                                          h3("Uploaded Data Structure"),
                                          verbatimTextOutput('org_text'),
                                          hr(),
                                          h3("Review converted dummy data"),
                                          
                                          dataTableOutput('dummy_table')
                                        )
                                      )
                             )
                             
                             
                             
                  )# end of navbarpage
                  
                  
                )# end of taglist
  
  )# end of fluidpage

  
  
  
  
  
