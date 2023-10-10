library(ggplot2)
library("lubridate")
library(dplyr)

data <- read.csv("D:/Master/Statistical/Assignment/customer_shopping_data.csv")


head(data)
#  
summary(data)

# Missing Values
missing_values <- is.na(data)
missvalues <- sum(missing_values)

print(sprintf("Number of missing value in the dataset is %d", missvalues))

data$invoice_date <- as.Date(data$invoice_date, format = '%d/%m/%Y')

invoiceDateObject <- as.Date(data$invoice_date, format = '%d/%m/%Y')
invoice_year <- format(invoiceDateObject,"%Y")

#invoice_year

 agg_data <- data %>%
   group_by(invoice_date) %>%
   summarise(
     age_number = median(age),            
     category_count = n_distinct(category),
     price = sum(price),
     total_quantity = sum(quantity),
     shopping_mall_count = n_distinct(shopping_mall)
    )
 agg_data
 max_price <- max(agg_data$price)
max_price
# Print the aggregated data
#//print(agg_data)
#plot(data)  
 
 invoicedate <- agg_data$invoice_date
 
# 1.1 Time series Plot  
 # for price 
# ggplot() +
#   geom_line(agg_data, mapping= aes(x= invoice_date, y=price),color="blue")+
#   geom_point(agg_data, mapping= aes(x= invoice_date, y=price),color="blue")+
#   labs(x = "Invoice Date", y = "Price", title = "Time Series Plot of Invoice date to price") +

 #   for total quantity 
  ggplot() +
   geom_line(agg_data, mapping=aes(x=invoice_date, y = total_quantity),color="red")+
   geom_point(agg_data, mapping=aes(x=invoice_date, y = total_quantity),color="red")+  
labs(x = "Invoice Date", y = "quantity", title = "Time Series Plot of Invoice date to quantity") +
   
     #   for age 
    ggplot() +
    geom_line(agg_data, mapping = aes(x=invoice_date , y=age_number),color="green") +  
    geom_point(agg_data, mapping = aes(x=invoice_date , y= age_number),color="green") + 
    labs(x = "Invoice Date", y = "age", title = "Time Series Plot of Invoice date to age") +
    #   for shopping_mall_count 
    ggplot()+
    geom_line(agg_data, mapping= aes(x=invoice_date, y = shopping_mall_count),color="purple")+
   geom_point(agg_data, mapping= aes(x=invoice_date, y = shopping_mall_count),color="purple")+
   labs(x = "Invoice Date", y = "shopping mall", title = "Time Series Plot of invoice date to shoppingmall")  +
    
    
    
    
    ggplot(data = data,aes(x=invoice_date,y=quantity))+
    geom_point()+facet_grid(~shopping_mall)
    
    
    
    
    
    ggplot()+
    geom_line(agg_data, mapping= aes(x= invoice_date, y=price,color="price_color"),size =1)+
    #geom_point(agg_data, mapping= aes(x= invoice_date, y=price),color="price_color")+
    geom_line(agg_data, mapping = aes(x=invoice_date , y=age_number,color="age_color"),size =1) +  
    #geom_point(agg_data, mapping = aes(x=invoice_date , y= age_number),color="age_color") + 
    geom_line(agg_data, mapping=aes(x=invoice_date, y = total_quantity,color="quantity_color"),size =1)+
   # geom_point(agg_data, mapping=aes(x=invoice_date, y = total_quantity),color="quantity_color")+ 
    geom_line(agg_data, mapping= aes(x=invoice_date, y = shopping_mall_count,color="mall_color") ,size =1)+
    #geom_point(agg_data, mapping= aes(x=invoice_date, y = shopping_mall_count),color="mall_color")+
    scale_y_continuous(breaks = seq(0, 1500000, by = 10000), limits = c(0,150000))+
      scale_color_manual(
        values = c("price_color" ="blue","age_color"="green","quantity_color"= "red", "mall_color" = "blue"),
        name = "Legend Title",
        labels = c("Price Line", "Age ","Quntity ","Shopping Mall")
      ) +
      
      theme(
        legend.position = c(1, 1),  # Adjust the position (top-right corner)
        legend.justification = c(1, 1)  # Align the legend to the top-right
      ) +
    scale_x_date(date_breaks = "6 months") +
    labs(
      title = "Plot Invoice date with price, age,quanity and shopping mall",
      x = "Invoice date",
      y = "price,age,quantity and shopping mall"
    )
    

   # 1.1 Distribution for each sales data
    ### Showing mean, median and mode
    
    #In R we do not have direct function to calculate mode so we do it with our own calculation.
    
  # hist(agg_data$invoice_date, main="Sales Distribution", xlab="InvoiceDate",breaks=3, col="blue")
    
    mode_price <-  as.numeric(names(sort(table(agg_data$price),decreasing=TRUE)[1]))
    mode_age <-  as.numeric(names(sort(table(agg_data$age_number),decreasing=TRUE)[1]))
    mode_category_count <-  as.numeric(names(sort(table(agg_data$category_count),decreasing=TRUE)[1]))
    mode_shoppingmall <-  as.numeric(names(sort(table(agg_data$shopping_mall_count),decreasing=TRUE)[1]))
    mode_totalsales_quantity <-  as.numeric(names(sort(table(agg_data$total_quantity),decreasing=TRUE)[1]))
   
     ### Frequency distribution of the output EEG signal with mean, median and mode representation
   
    #histogram with stats total sales quantity
    ggplot(aes(x = total_quantity), data = agg_data) + 
      geom_histogram(color = 'black', fill = "grey" ,binwidth = 0.5) + 
      geom_vline(aes(xintercept=median(total_quantity),
                     color="median"), linetype="dashed",
                 size=1) +
      geom_vline(aes(xintercept=mean(total_quantity),
                     color="mean"), linetype="dashed",
                 size=1) +
      geom_vline(aes(xintercept=mode_totalsales_quantity,
                     color="mode"), linetype="dashed",
                 size=1) +
      scale_color_manual(name = "Legend", values = c(median = "blue", mean = "red", mode = "grey"))+
      geom_rug()+ #Show the individual observations with black lines
      labs(title = "Frequency distribution of the output total sales quantity",
           caption = "Also called histogram") + 
      xlab("Output total sales quantity") + 
      ylab("Frequency ") 
    
    #histogram with stats Price
    ggplot(aes(x = price), data = agg_data) + 
      geom_histogram(color = 'black', fill = "grey" ,binwidth = 0.5) + 
      geom_vline(aes(xintercept=median(price),
                     color="median"), linetype="dashed",
                 size=1) +
      geom_vline(aes(xintercept=mean(price),
                     color="mean"), linetype="dashed",
                 size=1) +
      geom_vline(aes(xintercept=mode_shoppingmall,
                     color="mode"), linetype="dashed",
                 size=1) +
      scale_color_manual(name = "Legend", values = c(median = "blue", mean = "red", mode = "grey"))+
      geom_rug()+ #Show the individual observations with black lines
      labs(title = "Frequency distribution of the input price",
           caption = "Also called histogram") + 
      xlab("input Price") + 
      ylab("Frequency ") 
    
    #histogram with stats shopping mall
     ggplot(aes(x = shopping_mall_count), data = agg_data) + 
      geom_histogram(color = 'black', fill = "grey" ,binwidth = 0.5) + 
      geom_vline(aes(xintercept=median(shopping_mall_count),
                     color="median"), linetype="dashed",
                 size=1) +
      geom_vline(aes(xintercept=mean(shopping_mall_count),
                     color="mean"), linetype="dashed",
                 size=1) +
      geom_vline(aes(xintercept=mode_shoppingmall,
                     color="mode"), linetype="dashed",
                 size=1) +
      scale_color_manual(name = "Legend", values = c(median = "blue", mean = "red", mode = "grey"))+
      geom_rug()+ #Show the individual observations with black lines
      labs(title = "Frequency distribution of the input shopping mall count",
           caption = "Also called histogram") + 
      xlab("input shopping mall count") + 
      ylab("Frequency ") 
    
    

   
   #histogram with stats age
   ggplot(aes(x = age_number), data = agg_data) + 
     geom_histogram(color = 'black', fill = "grey" ,binwidth = 0.5) + 
     geom_vline(aes(xintercept=median(age_number),
                    color="median"), linetype="dashed",
                size=1) +
     geom_vline(aes(xintercept=mean(age_number),
                    color="mean"), linetype="dashed",
                size=1) +
     geom_vline(aes(xintercept=mode_age,
                    color="mode"), linetype="dashed",
                size=1) +
     scale_color_manual(name = "Legend", values = c(median = "blue", mean = "red", mode = "grey"))+
     geom_rug()+ #Show the individual observations with black lines
     labs(title = "Frequency distribution of the input age",
          caption = "Also called histogram") + 
     xlab("input age") + 
     ylab("Frequency ") 
   
   #histogram with stats category
   ggplot(aes(x = category_count), data = agg_data) + 
     geom_histogram(color = 'black', fill = "grey" ,binwidth = 0.5) + 
     geom_vline(aes(xintercept=median(category_count),
                    color="median"), linetype="dashed",
                size=1) +
     geom_vline(aes(xintercept=mean(category_count),
                    color="mean"), linetype="dashed",
                size=1) +
     geom_vline(aes(xintercept=mode_category_count,
                    color="mode"), linetype="dashed",
                size=1) +
     scale_color_manual(name = "Legend", values = c(median = "blue", mean = "red", mode = "grey"))+
     geom_rug()+ #Show the individual observations with black lines
     labs(title = "Frequency distribution of the category",
          caption = "Also called histogram") + 
     xlab("input category count") + 
     ylab("Frequency ") 
   
## Task 1.3: Correlation and scatter plots (between different input customer data and the output predicted 
#     sales quantity) to examine their dependencies

   
   #Scatter plot for price with sales quantity
   ggplot(agg_data, aes(x = price, y = total_quantity,
                  colour = total_quantity)) +
     geom_point(show.legend = TRUE) +
     geom_smooth(method=lm, level=0.95)+ #add linear trend line
     scale_color_gradient(low = "#EEDBBD", high = "#9F3632") +
     labs(title = "Relationship between input price & Output quantity") + 
     xlab("Input price") + 
     ylab("Output  sales quantity") + 
     stat_cor(p.accuracy = 0.05, r.accuracy = 0.01, method = "pearson" )
   
   #Scatter plot for age with sales quantity
   ggplot(agg_data, aes(x = age_number, y =total_quantity ,
                  colour = total_quantity)) +
     geom_point(show.legend = TRUE) +
     geom_smooth(method=lm, level=0.95)+
     # stat_cor(method="pearson")+
     scale_color_gradient(low = "#D5D5D5", high = "#49525E") +
     labs(title = "Relationship between input age & Output sales quantity") + 
     xlab("Input age") + 
     ylab("Output sales quantity") + 
     stat_cor(p.accuracy = 0.05, r.accuracy = 0.01, method = "pearson" ) 
  
    #Scatter plot for shopping mall with sales quantity
   ggplot(agg_data, aes(x = shopping_mall_count, y =total_quantity ,
                        colour = total_quantity)) +
     geom_point(show.legend = TRUE) +
     geom_smooth(method=lm, level=0.95)+
     # stat_cor(method="pearson")+
     scale_color_gradient(low = "#EEDBBD", high = "#9F3632") +
     labs(title = "Relationship between input shoppingmall count & Output sales quantity") + 
     xlab("Input shopping mall") + 
     ylab("Output sales quantity") + 
     stat_cor(p.accuracy = 0.05, r.accuracy = 0.01, method = "pearson" ) 
   
   #Scatter plot for category with sales quantity
   ggplot(agg_data, aes(x = category_count, y =total_quantity ,
                        colour = total_quantity)) +
     geom_point(show.legend = TRUE) +
     geom_smooth(method=lm, level=0.95)+
     # stat_cor(method="pearson")+
     scale_color_gradient(low = "#D5D5D5", high = "#49525E") +
     labs(title = "Relationship between input category count & Output sales quantity") + 
     xlab("Input category") + 
     ylab("Output sales quantity") + 
     stat_cor(p.accuracy = 0.05, r.accuracy = 0.01, method = "pearson" ) 
   ### Visualization scatter plot with density plot and correlation coefficient in single view
   ggpairs(agg_data) 
   
  
  # Task 2: Regression – modeling the relationship between sales data 
  
   #task 2: Regression – modeling the relationship between sales data
   # Create a data frame with your x and y data
   #convert data into a time-series
   ts1_data <- ts(agg_data, start=c(2020, 1, 1), frequency = 365)
   
   latestdf <- data.frame(
     x1 = ts1_data[, "age_number"],
     x2 = ts1_data[, "category_count"],
     x3 = ts1_data[, "price"],
     x4 = ts1_data[, "shopping_mall_count"],
     y = ts1_data[, "total_quantity"]
   )
   plot(latestdf)
   
   cor(latestdf)
   
   options(scipen = 999)
   
   #create 5 modal formula
   model1 <- lm(y ~ poly(x4, 1, raw = TRUE) + poly(x1, 2, raw = TRUE) + poly(x1, 3, raw = TRUE) +
                  poly(x2, 4, raw = TRUE) + poly(x1, 4, raw = TRUE), data = latestdf)
   
   model2 <- lm(y ~ poly(x4, 1, raw = TRUE) + poly(x1, 3, raw = TRUE) + poly(x3, 4, raw = TRUE), data = latestdf)
   
   model3 <- lm(y ~ poly(x3, 3, raw = TRUE) + poly(x3, 4, raw = TRUE), data = latestdf)
   
   model4 <- lm(y ~ poly(x2, 1, raw = TRUE) + poly(x1, 3, raw = TRUE) + poly(x3, 4, raw = TRUE), data = latestdf)
   
   model5 <- lm(y ~ poly(x4, 1, raw = TRUE) + poly(x1, 2, raw = TRUE) + poly(x1, 3, raw = TRUE) +
                  poly(x3, 4, raw = TRUE), data = latestdf)
   ### Task 2.1.1: Using least square, estimating model parameter for model 1
  
    estimated_parameters_list <- list(
     Model1 = coef(model1),
     Model2 = coef(model2),
     Model3 = coef(model3),
     Model4 = coef(model4),
     Model5 = coef(model5)
   ) 
  
    
    # print for model1   
  print(estimated_parameters_list$Model1) 
    
  # print for model2   
  print(estimated_parameters_list$Model2) 
  # print for model3   
  print(estimated_parameters_list$Model3) 
  # print for model4   
  print(estimated_parameters_list$Model4) 
  # print for model5   
  print(estimated_parameters_list$Model5) 
  
  
   
   ### Task 2.2  ###
   # Calculate RSS for each model
   rss_values <- c(
     sum(model1$residuals^2),
     sum(model2$residuals^2),
     sum(model3$residuals^2),
     sum(model4$residuals^2),
     sum(model5$residuals^2)
   )
   
   # Create a data frame to store the RSS for each model
   rss_df <- data.frame(
     Model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
     RSS = rss_values
   )
   
   # Print the RSS for each model
   print(rss_df$)
  # Task 2.3  calculate_log_likelihood
   
   calculate_log_likelihood <- function(model) {
     n <- length(model$residuals)
     sigma_sq <- sum(model$residuals^2) / (n - length(model$coefficients))
     log_likelihood <- -n/2 * log(2 * pi * sigma_sq) - sum(model$residuals^2) / (2 * sigma_sq)
     return(log_likelihood)
   }
   
   log_likelihood_values <- c(
     calculate_log_likelihood(model1),
     calculate_log_likelihood(model2),
     calculate_log_likelihood(model3),
     calculate_log_likelihood(model4),
     calculate_log_likelihood(model5)
   )
   print(log_likelihood_values)
   
   # Task 2.4.1 calculate Akaike information 
   aic_values <- c(
     AIC(model1),
     AIC(model2),
     AIC(model3),
     AIC(model4),
     AIC(model5)
   )
   aic_values
   
   # Task 2.4.2 calculate BIC information 
   bic_values <- c(
     BIC(model1),
     BIC(model2),
     BIC(model3),
     BIC(model4),
     BIC(model5)
   )
   bic_values
   
   
   # Task 2.5 Checking distribution of model prediction errors
   
   predictions1 <- predict(model1)
   predictions2 <- predict(model2)
   predictions3 <- predict(model3)
   predictions4 <- predict(model4)
   predictions5 <- predict(model5)
   
   # errors in each models
   errors1 <- y - predictions1
   errors2 <- y - predictions2
   errors3 <- y - predictions3
   errors4 <- y - predictions4
   errors5 <- y - predictions5

      print(errors1)
   ## Task 2.5.2: Visualizing prediction error with Q-Q plot
   
   # Q-Q plots of prediction error for model 1
   
   qqnorm(t(errors1),col = "grey", main = "Q-Q Plot for Model 1" )
   qqline(errors1, col = "red", lwd = 1,lty = 2)
   
   # Q-Q plots of prediction error for model 2
   
   qqnorm(t(errors2),col = "grey", main = "Q-Q Plot for Model 2" )
   qqline(errors2, col = "red", lwd = 1,lty = 2)
   
   # Q-Q plots of prediction error for model 3
   
   qqnorm(t(errors3),col = "grey", main = "Q-Q Plot for Model 3" )
   qqline(errors3, col = "red", lwd = 1,lty = 2)
   
   # Q-Q plots of prediction error for model 4
   
   qqnorm(t(errors4),col = "grey", main = "Q-Q Plot for Model 4" )
   qqline(errors4, col = "red", lwd = 1,lty = 2)
   
   # Q-Q plots of prediction error for model 5
   
   qqnorm(t(errors5),col = "grey", main = "Q-Q Plot for Model 5" )
   qqline(errors5, col = "red", lwd = 1,lty = 2)   
   
   

   
      # Task 3.1: Compute 2 parameter posterior distributions
   
    # Here, we will compute 2 parameter posterior distributions, which will be the 2 parameters with largest absolute value in our least squares estimation (Task 2.1) of the selected model 2. Then we will keep all the other parameters of our model as constant.
   # Creator vector of theta_hat of selected model 2 and sorting them to find out two largest absolute value and printing it
   
   arr_1=0
   arr_2=0
   f_value=0
   s_value=0
  
    model4
   
    # Before that we convert our model and theta hat into matrix
   x_model_4 <- as.matrix(model4)
   x_model_4
   
   #values from model4
   thetebias <- 1866.95#selected parameter
   thetaone <-30.25499   # selected parameter
   thetatwo <- 165.645  # constant value
   thetathree <- 0.02080128 # constant value
  
   Epison <- rss_df$RSS[4] * 2 ## fixing value of eplision
   Epison
   num <- 100 #number of iteration
   ##Calculating Y-hat for performing rejection ABC
   counter <- 0
   for (i in 1:num) {
     range1 <- runif(1,-5000,1000) # calculating the range
     range1
     range2 <- runif(1,0,100)
     New_thetahat <- matrix(c(range1,range2,thetatwo,thetathree))
     New_thetahat
     New_Y_Hat <- model4 %*% New_thetahat ## calculating new Y-hat
     New_Y_Hat
     new_RSS <- sum((y-New_Y_Hat)^2)
     new_RSS
     if (new_RSS > Epison){
       arr_1[i] <- range1
       arr_2[i] <- range2
       counter = counter+1
       f_value <- matrix(arr_1)
       s_value <- matrix(arr_2)
     }
   }
   hist(f_value)
   hist(s_value)
   
   ###ploting Joint and Marginal Posterior Distribution of the graph
   plot(f_value,s_value, col = c("brown", "blue"), main = "Joint and Marginal Posterior Distribution")
   par(mfrow=c(1,1))
   
   
   