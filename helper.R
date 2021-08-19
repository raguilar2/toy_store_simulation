# This file contains the functions that will be used in the toy 
# store simulation.

# computes cost of ordering a toy
order_cost <- function(y, t){
  sum(20*y + t) 
}

# determines price based on the amount sold in the last 10 days for each toy 
# note: prices do not change for first 10 days
price <- function(sales, initial_prices, t){
  if(t > 10) {
    num_toy1_sold <- sum(sales[(t - 10):(t - 1), 1]) # total toy 1 sold
    num_toy2_sold <- sum(sales[(t - 10):(t - 1), 2]) # total toy 2 sold
    num_toy3_sold <- sum(sales[(t - 10):(t - 1), 3]) # total toy 3 sold
    
    # finds which toy sold the most/ least
    min_sold <- which.min(c(num_toy1_sold, num_toy2_sold, num_toy3_sold))
    max_sold <- which.max(c(num_toy1_sold, num_toy2_sold, num_toy3_sold))
    
    # decreases price of toy that sold the least by 5%
    initial_prices[min_sold] <- round(initial_prices[min_sold]*0.95, 2)
    
    # increases price of toy that sold the most by 5%
    initial_prices[max_sold] <- round(initial_prices[max_sold]*1.05, 2) 
  }
  initial_prices # returns prices
}

# adjusts the amount of inventory before an order is made and the 
# maximum amount of each toy in the inventory 
min_stock <- max_stock <- function(sales, initial_cutoff, t){
  if(t > 10) {
    num_toy1_sold <- sum(sales[(t - 10):(t - 1), 1]) # total toy 1 sold
    num_toy2_sold <- sum(sales[(t - 10):(t - 1), 2]) # total toy 2 sold
    num_toy3_sold <- sum(sales[(t - 10):(t - 1), 3]) # total toy 3 sold
    
    # finds which toy sold the most/ least
    min_sold <- which.min(c(num_toy1_sold, num_toy2_sold, num_toy3_sold))
    max_sold <- which.max(c(num_toy1_sold, num_toy2_sold, num_toy3_sold))
    
    # decreases cutoff of toy that sold the least by 5%
    initial_cutoff[min_sold] <- ceiling(initial_cutoff[min_sold]*0.95)
    
    # increases cutoff of toy that sold the most by 5%
    initial_cutoff[max_sold] <- ceiling(initial_cutoff[max_sold]*1.05) 
  }
  initial_cutoff # returns cutoff 
}

# determines probabilty that a customer wants to get a toy for each toy
# based on trends for last 10 days
prob_want <- function(pr, t){
  if(t > 10) {
    num_toy1_sold <- sum(sales[(t - 10):(t - 1), 1]) # total toy 1 sold
    num_toy2_sold <- sum(sales[(t - 10):(t - 1), 2]) # total toy 2 sold
    num_toy3_sold <- sum(sales[(t - 10):(t - 1), 3]) # total toy 3 sold
    
    # total sold in last 10 days
    total_sold <- sum(num_toy1_sold, num_toy2_sold, num_toy3_sold) 
    
    # estimates probabilities for each toy by finding proportion sold
    probs <- c(num_toy1_sold/total_sold, num_toy2_sold/total_sold,
               num_toy3_sold/total_sold)
  } else {
    probs <- c(1/3, 1/3, 1/3) # initial probabilities
  }
  probs # returns probabilities
}

# determines the probability that a customer will purchase a toy
# given the price and day
prob_purchase <- function(initial_r, r, t){
  
  # This function allows time to have a multiplicative effect on the probability
  # of purchasing a toy (from 0.45 to 1.5). We assume that customers will 
  # remember the original price. Thus, they are more likely to make a purchase 
  # when the price is below the original but less likely when the price is 
  # greater than the original. 
  prob1 <- ((initial_r[1]/r[1])^3)*(exp((t - 47)/55) + 0.5) # toy 1 probability
  prob2 <- ((initial_r[2]/r[2])^3)*(exp((t - 47)/55) + 0.5) # toy 2 probability
  prob3 <- ((initial_r[3]/r[3])^3)*(exp((t - 47)/55) + 0.5) # toy 3 probability
  
  # keeps the results within 0 and 1
  if(prob1 > 1){
    prob1 <- 1
  }
  if(prob2 > 1){
    prob2 <- 1
  }
  if(prob3 > 1){
    prob3 <- 1
  }
  
  c(prob1, prob2, prob3) # returns probabilities
}

# determines how often customers arrive 
c_lambda_t <- function(t, lambda1){
  if(t <= 22){ 
    # increases how often customers arrive until Black Friday
    lambda <- lambda1*((t/21) + (20/21))
  } else if (22 < t & t <= 26) { 
    # decreases how often customers arrive until the following weekend
    lambda <- lambda1*(-(t/8)+4.75)
  } else {
    # increases how often customers arrive until Christmas Eve
    lambda <- lambda1*((t/20) + 0.2)
  }
}

poisson_process <- function(T_ = 1, lambda, lambda_t){
  t <- 0
  i <- 0
  x <- 0
  u2 <- NA
  while(t <= T_){
    u1 <- runif(1) 
    t <- t - (1/lambda)*log(u1)
    u2 <- runif(1)
    if(u2 <= (lambda_t/lambda) & t <= T_){
      i <- i + 1
      if(i == 1){
        x <- t
      } else {
        x <- c(x, t)
      }
    }
  }
  list(events = i, event_times = x, u = u2)
}

# finds the total amount of demand in one day
demand <- function(c_lambda, c_lambda_t, p_lambda, p_lambda_t, prob_want, prob_purchase){
  # initialize demand
  d <- c(0, 0, 0)
  
  # generates amount of customers in one day
  c <- poisson_process(lambda = c_lambda, lambda_t = c_lambda_t)[[1]]
  
  for(i in 1:c){
    # generates the amount of toys a customer wants 
    w <- poisson_process(lambda = p_lambda, lambda_t = p_lambda_t)[[1]] + 1
    
    # figures out how many of EACH toy is wanted by a customer. Assuming 
    # someone walks in wanting a certain amount of toys (maybe they want 
    # one toy for each of their children). These probabilities will change 
    # as customers will also have a better idea of what their kids want based 
    # on market trends as well
    
    w_each_toy <- 
      as.numeric(rmultinom(1, w, c(prob_want[1], prob_want[2], prob_want[3])))
    
    # finds how many toys the customer will end up buying after
    # "seeing" the price of each toy, prob for each toy will be 
    # determined by a function of price on a given day
    
    p_toy1 <- ifelse(w_each_toy[1] == 0, 0,
                     rbinom(w_each_toy[1], 1, prob_purchase[1]))
    p_toy2 <- ifelse(w_each_toy[2] == 0, 0,
                     rbinom(w_each_toy[2], 1, prob_purchase[2]))
    p_toy3 <- ifelse(w_each_toy[3] == 0, 0,
                     rbinom(w_each_toy[3], 1, prob_purchase[3]))
    
    d <- d + c(p_toy1, p_toy2, p_toy3) # records total demand
  }
  d # returns demand
}