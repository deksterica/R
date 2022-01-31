
# input: tidy_check_table, has column PayDate with dates

# setting env 
weekdays_cycle <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Satruday", "Sunday")

# 
# payforms = list( c("weekly", ...delay),
#                  c("bi-weekly", ...delay), 
#                  c("semi-monthly", c(start1, start2), c(end1, end2)),  
#                  c("monthly", start, end))

# this function takes table and payform, 
#  and returns two tables:  same table with added columns paystart and payend
#                           table with moved check dates

get_paystart_payend <- function( tidy_check_table, payform ) {
  
  if ( payform[1] == "weekly")  {
    
  #   days_to_period_end = payform[2] 0 # DEFAULT
    if ( is.na(payform[2])) payform[2] <- 0
      
  #   days_to_period_period_start = 7 # DEFAULT
  #   
  #   # grab weekday that appears most in check dates
  #   # seek out those that are not, and move them to closest weekday & report on moves
  #   # set paystart and pay end
    adding_dates <- tidy_check_table %>%
    
                    mutate( weekday = weekdays(PayDate),
                                      most_common_weekday = sort(weekday)[1] ) %>% 
    
                    rowwise() %>% 
    
                    mutate( how_many_days_off =  which( weekdays_cycle == most_common_weekday ) - which( weekdays_cycle == weekday ) ) %>%
    
                    mutate( payend = PayDate + days( how_many_days_off) - days( payform[2]),
                            paystart = payend - days(6) ) 
  }
  
  if  ( payform[1] == "bi-weekly") {
    
  #   days_to_period_end = 0 # DEFAULT
    if ( is.na(payform[2])) payform[2] <- 0
  #   days_to_period_period_start = 14 # DEFAULT
  #   
  #   # grab weekday that appears in most paychecks
  #   # seek out those that are not, and move them to closest weekday & report on moves
  #   # set paystart and pay end
  
    adding_dates <- tidy_check_table %>%
                    
                    mutate( weekday = weekdays(PayDate),
                            most_common_weekday = sort(weekday)[1] ) %>% 
      
                    rowwise() %>% 
      
                    mutate( how_many_days_off =  which( weekdays_cycle == most_common_weekday ) - which( weekdays_cycle == weekday ) ) %>%
      
                    mutate( payend = PayDate + days( how_many_days_off) - days( payform[2]),
                            paystart = payend - days(13) ) 
  }
  
  # 
  # if monthly {
  #   
  #   paystart_day <- 1 # DEFAULT
  #   payend_day <- last_date_in_month # DEFAULT
  #   
  #   # find to what period check refers to
  #   # set paystart and pay end
  #   
  # }
  # 
  # if semi-monthly {
  #   
  #   paystart_days <- c(1, 15) # DEFAULT
  #   payend_days <- c(16, last_date_in_month) # DEFAULT
  #   
  #   # find to what period check refers to
  #   # set paystart and pay end
  #   
  # }

  
  c( adding_dates, 
     adding_dates[which(adding_dates$how_many_days_off !=0 ),] )
  
  
 }
 
# 
# checktable1 <- check_table
# names(checktable1) <- c( "Name",  "PayDate" ,  "Total Amount")
# 
# get_paystart_payend( checktable1, c("bi-weekly", 8))
# 

