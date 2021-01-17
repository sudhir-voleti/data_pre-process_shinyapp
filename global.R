
data_frame_str <- function(data){
  df_str <- data.frame(variable = names(data),
             class = sapply(data, class),
             first_values = sapply(data, function(x) paste0(head(x),  collapse = ", ")),
             unique_value_count = sapply(data,function(x) length(unique(x))),
             row.names = NULL) 
  return(df_str)
}

display_missing_percentage <- function(data){
  # count total, missing value & its percentage 
  missing.values <- data %>%
    gather(key = "key", value = "val") %>%
    mutate(isna = is.na(val)) %>%
    group_by(key) %>%
    mutate(total = n()) %>%
    group_by(key, total, isna) %>%
    summarise(num.isna = n()) %>%
    mutate(pct = num.isna / total * 100)
  
  if(sum(missing.values$isna)==0){
    
    percentage.plot <- missing.values %>%
      ggplot() +
      geom_bar(aes(x = reorder(key, desc(pct)), 
                   y = pct, fill=isna), 
               stat = 'identity', alpha=0.8) +
      #scale_x_discrete(limits = levels) +
      scale_fill_manual(name = "", 
                        values = c('steelblue', 'tomato3'), labels = c("Present", "Missing")) +
      coord_flip() +
      labs(title = "Percentage of missing values", x =
             'Variable', y = "% of missing values")
    return(percentage.plot)
    
  }else{
    
    levels <- (missing.values  %>% filter(isna == T) %>% arrange(desc(pct)))$key
    
    percentage.plot <- missing.values %>%
      ggplot() +
      geom_bar(aes(x = reorder(key, desc(pct)), 
                   y = pct, fill=isna), 
               stat = 'identity', alpha=0.8) +
      scale_x_discrete(limits = levels) +
      scale_fill_manual(name = "", 
                        values = c('steelblue', 'tomato3'), labels = c("Present", "Missing")) +
      coord_flip() +
      labs(title = "Percentage of missing values", x =
             'Variable', y = "% of missing values") 
     
    return(percentage.plot)
  }
  
}
