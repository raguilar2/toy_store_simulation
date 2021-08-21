# This file contains the toy store simulation. The simulation determines what 
# combination of inventory cutoffs and %MSRP will maximize profits.

source("helper.R")
set.seed(213)

r_prop <- c(90, 95, 100, 105, 110, 125) # %MSRP
min_cuts <- c(200, 400, 600, 800) # min amount before order
max_cuts <- c(400, 600, 900, 1200) # max amount in inventory

# initialize array that will contain the results of each combination
results <- array(0, dim = c(length(r_prop), length(min_cuts), length(max_cuts)))

# estimate daily profits for every combination of prices and inventory variables
for (l in 1:length(max_cuts)) {
  for (k in 1:length(min_cuts)) {
    for (j in 1:length(r_prop)) {
      profits <- numeric(100) # initialize vector that will record profits
      
      for (i in 1:100) {
        s <- c(min_cuts[k], min_cuts[k], min_cuts[k]) # choose min 
        S <- c(max_cuts[l], max_cuts[l], max_cuts[l]) # choose max
        r <- c(r_prop[j], r_prop[j], r_prop[j]) # choose price
        initial_r <- c(100, 100, 100) # chosen for easy interpretation
        t <- 0 # keeps track of time, in days
        L <- 10 # delivery time
        SS <- list(S, c(0, 0, 0)) # inventory on hand and on order
        h <- c(1, 1, 1) # cost is one dollar per toy per day
        C <- 0 # total order cost
        H <- 0 # total holding cost
        R <- 0 # total revenue
        t0 <- 1 # replaces generating next customer
        t1 <- c(Inf, Inf, Inf) # day when delivery arrives for each toy
        
        # initialize dataset to keep track of daily sales per toy
        sales <- data.frame(toy1 = numeric(47), toy2 = numeric(47), toy3 = numeric(47))
        
        # simulates sales from November 8th to December 24th
        while(t <= 46){
          if(all(t0 < t1)){
            t <- t0 # set t to current day
            r <- price(sales, r, t) # update price
            s <- min_stock(sales, s, t) # update cut offs
            S <- max_stock(sales, S, t) # update cut offs
            
            # determines total amount of toys customers WANT to purchase in this day
            D <- demand(c_lambda = 25, c_lambda_t = c_lambda_t(t, 10), 
                        p_lambda = 3, p_lambda_t = 3, # ~3 toys per cust
                        prob_want = prob_want(sales, t), 
                        prob_purchase = prob_purchase(initial_r, r, t)) 
            
            # checks inventory and determines amount of each toy customers CAN buy
            w <- c(min(c(D[1], SS[[1]][1])), 
                   min(c(D[2], SS[[1]][2])), 
                   min(c(D[3], SS[[1]][3]))) 
            
            sales[t,] <- w # records amount of each toy that was sold on day t
            
            SS[[1]] <- SS[[1]] - w # adjust inventory based on sales
            R <- R + as.numeric(w %*% r) # determine revenue
            
            # determines holding cost at end of day 
            H <- H + (t0 - t)*as.numeric(SS[[1]]%*%h)
            
            # orders more toys if inventory is below cutoff 
            # and there's no order on the way
            
            # orders for toy 1
            if(SS[[1]][1] < s[1] & SS[[2]][1] == 0 & S[1] - SS[[1]][1] > 0){ 
              SS[[2]][1] <- S[1] - SS[[1]][1] # set amount in the order
              t1[1] <- t + L # set delivery date
            }
            # orders for toy 2
            if(SS[[1]][2] < s[2] & SS[[2]][2] == 0 & S[2] - SS[[1]][2] > 0){ 
              SS[[2]][2] <- S[2] - SS[[1]][2] # set amount in the order
              t1[2] <- t + L # set delivery date
            }
            # orders for toy 3
            if(SS[[1]][3] < s[3] & SS[[2]][3] == 0 & S[3] - SS[[1]][3] > 0){ 
              SS[[2]][3] <- S[3] - SS[[1]][3] # set amount in the order
              t1[3] <- t + L # set delivery date
            }
            
            t0 <- t + 1 # increments day instead of generating next customer
            
          } else if(any(t1 <= t0)){ 
            if(t1[1] <= t0){ # order for toy 1 arrives
              C <- C + order_cost(SS[[2]][1], t) # determine order costs
              SS[[1]][1] <- SS[[1]][1] + SS[[2]][1] # adds order to inventory
              SS[[2]][1] <- 0 # reset amount ordered
              t1[1] <- Inf # reset delivery date
            }
            if(t1[2] <= t0){ # order for toy 2
              C <- C + order_cost(SS[[2]][2], t) # determine order costs
              SS[[1]][2] <- SS[[1]][2] + SS[[2]][2] # adds order to inventory
              SS[[2]][2] <- 0 # reset amount ordered
              t1[2] <- Inf # reset delivery date
            }
            if(t1[3] <= t0){ # order for toy 3
              C <- C + order_cost(SS[[2]][3], t) # determine order costs
              SS[[1]][3] <- SS[[1]][3] + SS[[2]][3] # adds order to inventory
              SS[[2]][3] <- 0 # reset amount ordered
              t1[3] <- Inf # reset delivery date
            }
          }
        }
        profits[i] <- round((R - C - H)/46, 2) # determine profit per day 
      }
      results[j, k, l] <- mean(profits) # records average profits into array
    }
  }
}

# save results and inputs tested
save(results, max_cuts, min_cuts, r_prop, file = "inputs_results.Rdata")
