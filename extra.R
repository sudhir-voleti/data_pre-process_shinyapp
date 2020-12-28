# basic code for a one-hot encoding and descrtiptive stats wala app 
df <- read.csv("D://churn_data.csv")
if (!require("purrr")){install.packages("purrr"); library("purrr")}
if (!require("fastDummies")){install.packages("fastDummies"); library("fastDummies")}

#Function 1: Upload data function------
upload_data <- function(file){
  df <- read.csv(file)
  return(df)
}


dataf <- upload_data('https://vincentarelbundock.github.io/Rdatasets/csv/carData/Salaries.csv')


#Function 2: select numeric cols-----
select_numeric_cols <- function(df){
  num_df <- df %>% select_if(is.numeric)
  return(num_df)
}

data_n <- select_numeric_cols(dataf)

#Function 3: select cat cols-----
select_cat_cols <- function(df){
  cat_df <- df %>% select_if(is.character)
  return(cat_df)
}

data_c <- select_cat_cols(dataf)



convert_cat_to_num <- function(df){
  
}
#Function 4: convert to dummies----
# 04-well.R

sapply(df, function(x){
                    x<-unique(x)
                    y <- length(x)
                    return(y)
                    })