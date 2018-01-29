library(tidyverse)
library(stringr)
library(modelr)

# Algorithm 4
# Hill-Climbing

Hill_climibing <- function(fn, lb, ub, ini, change, stop_fun, max_it = 200, full_output = F) {
  le <- length(lb)
  
  S_archive <- matrix(NA, nrow = max_it, ncol = le)
  obj <- rep(NA_real_, max_it)
  
  S <- ini(lb, ub) #  The Initialization Procedure
  best <- fn(S)
  
  gen <- 1 # generation counter
  S_archive[gen, ] <- S
  obj[gen] <- best
  
  stop_search <- F 
  while (!stop_search) {
    gen <- gen + 1
    R <- change(S, lb, ub)
    R_obj <- fn(R)
    if (R_obj > best) {
      S <- R
      best <- R_obj
    }
    
    obj[gen] <- best
    S_archive[gen, ] <- S
    stop_search <- stop_fun(S_archive, max_it) # If stop fun returns T, then stop search
  }
  
  S_archive <- S_archive[!is.na(S_archive[,1]),]
  obj <- obj[!is.na(obj)]
  
  if (full_output){
    return(list(S_archive = S_archive, obj = obj))
  } else {
    return(S)
  }
}

fn <- function(x){
  sum(sin(x))
}

ini <- function(lb, ub){
  le <- length(lb)
  x <- rep(NA_real_, le)
  
  for (i in 1:le){
    li <- lb[i]
    ui <- ub[i]
    x[i] <- runif(1, li, ui)
  }
  return(x)
}

change <- function(x, lb, ub, step = 0.1){
  # old version of change
  
  more_data <- T
  
  while(more_data){
    new_x <- ini(lb, ub) * step * sample(x = c(-1,1),size = 1, prob = c(0.5,0.5)) + x
    
    range_check <- sum(new_x < lb) + sum(new_x > ub) # range_check == 0, if new_x is within the range
    more_data <- ifelse(range_check == 0, F, T) # if new_x is within range, break while, else repeat
  }
  
  return(new_x)
}

change <- function(x, lb, ub, noise = 0.5 * (ub - lb), p = 1) {
  # x: vector to be changed 
  # lb: lower bond of x vector
  # ub: upper bond of x vector
  # noise: noises to be added to x vector 
  # p: probability of changing a element in vector
  
  le <- length(x)
  dx <- rep(0, le) # noices to be added
  
  for (i in 1:le) {
    if (p >= runif(1)) {
      add_noise <- T
      while (add_noise) {
        dx[i] <- runif(1, -1, 1) * noise[i]
        temp <- x[i] + dx[i]
        range_check <- sum(temp < lb[i]) + sum(temp > ub[i]) # range_check == 0, if temp is within the range
        add_noise <- ifelse(range_check == 0, F, T) # if temp is within range, break while, else repeat
      }
    }
  }
  
  return (x + dx)
}

stop_fun <- function(S_archive, max_it = 200){
  x <- S_archive[,1]
  if (sum(!is.na(x)) >= max_it){
    return(T)
  } else {
    return(F)
  }
}

lb <- c(0,0,0)
ub <- c(1,1,1)

result <- Hill_climibing(fn, lb, ub, ini, change, stop_fun, full_output = T)
plot(result$obj)

# Algorithm 5
# Steepest Ascent Hill-Climbing
Steepest_hill_climibing <- function(fn, lb, ub, ini, change, stop_fun, 
                                    n = 5, max_it = 2000, full_output = F) {
  le <- length(lb)
  
  S_archive <- matrix(NA, nrow = max_it, ncol = le)
  obj <- rep(NA_real_, max_it)
  
  S <- ini(lb, ub) #  The Initialization Procedure
  best <- fn(S)
  
  gen <- 1 # generation counter
  S_archive[gen,] <- S
  obj[gen] <- best
  
  stop_search <- F
  while (!stop_search) {
    gen <- gen + 1
    
    R <- change(S, lb, ub)
    R_obj <- fn(R) # search n neighbours, choose the best one
    for (i in 1:(n - 1)) {
      R_temp <- change(S, lb, ub)
      R_obj_temp <- fn(R_temp)
      
      if (R_obj_temp > R_obj){
        R_obj <- R_obj_temp
        R <- R_temp
      }
    }

    if (R_obj > best) { # replace start point with current best
      S <- R
      best <- R_obj
    }
    
    obj[gen] <- best
    S_archive[gen,] <- S
    stop_search <-
      stop_fun(S_archive) # If stop fun returns T, then stop search
  }
  
  S_archive <- S_archive[!is.na(S_archive[, 1]), ]
  obj <- obj[!is.na(obj)]
  
  if (full_output) {
    return(list(S_archive = S_archive, obj = obj))
  } else {
    return(S)
  }
}

fn <- function(x){
  sum(sin(x))
}

ini <- function(lb, ub){
  le <- length(lb)
  x <- rep(NA_real_, le)
  
  for (i in 1:le){
    li <- lb[i]
    ui <- ub[i]
    x[i] <- runif(1, li, ui)
  }
  return(x)
}

lb <- c(0,0,0)
ub <- c(1,1,1)

result <- Steepest_hill_climibing(fn, lb, ub, ini, change, stop_fun, full_output = T)
plot(result$obj)

# Algorithm 6
# Steepest Ascent Hill-Climbing with Replacement
Steepest_hill_climibing_w_replace <- function(fn, lb, ub, ini, change, stop_fun, 
                                    n = 5, max_it = 2000, full_output = F) {
  le <- length(lb)
  
  S_archive <- matrix(NA, nrow = max_it, ncol = le)
  obj <- rep(NA_real_, max_it)
  
  S <- ini(lb, ub) #  The Initialization Procedure
  best <- fn(S)
  
  gen <- 1 # generation counter
  S_archive[gen,] <- S
  obj[gen] <- best
  
  stop_search <- F
  while (!stop_search) {
    gen <- gen + 1
    
    R <- change(S, lb, ub)
    R_obj <- fn(R) # search n neighbours, choose the best one
    for (i in 1:(n - 1)) {
      R_temp <- change(S, lb, ub)
      R_obj_temp <- fn(R_temp)
      
      if (R_obj_temp > R_obj){
        R_obj <- R_obj_temp
        R <- R_temp
      }
    }
    
    S <- R # direct replacement without comparing to the starting point S
    # R compare with the best discovered so far, if replace the best
    
    if (R_obj > best) {
      best_para <- R
      best <- R_obj
    }
    
    obj[gen] <- best
    S_archive[gen,] <- best_para
    stop_search <-
      stop_fun(S_archive) # If stop fun returns T, then stop search
  }
  
  S_archive <- S_archive[!is.na(S_archive[, 1]), ]
  obj <- obj[!is.na(obj)]
  
  if (full_output) {
    return(list(S_archive = S_archive, obj = obj))
  } else {
    return(S)
  }
}
fn <- function(x){
  sum(sin(x))
}

ini <- function(lb, ub){
  le <- length(lb)
  x <- rep(NA_real_, le)
  
  for (i in 1:le){
    li <- lb[i]
    ui <- ub[i]
    x[i] <- runif(1, li, ui)
  }
  return(x)
}

change <- function(x, lb, ub, noise = 0.5 * (ub - lb), p = 1) {
  # x: vector to be changed 
  # lb: lower bond of x vector
  # ub: upper bond of x vector
  # noise: noises to be added to x vector 
  # p: probability of changing a element in vector
  
  le <- length(x)
  dx <- rep(0, le) # noices to be added
  
  for (i in 1:le) {
    if (p >= runif(1)) {
      add_noise <- T
      while (add_noise) {
        dx[i] <- runif(1, -1, 1) * noise[i]
        temp <- x[i] + dx[i]
        range_check <- sum(temp < lb[i]) + sum(temp > ub[i]) # range_check == 0, if temp is within the range
        add_noise <- ifelse(range_check == 0, F, T) # if temp is within range, break while, else repeat
      }
    }
  }
  
  return (x + dx)
}

stop_fun <- function(S_archive, max_it = 200){
  x <- S_archive[,1]
  if (sum(!is.na(x)) >= max_it){
    return(T)
  } else {
    return(F)
  }
}

lb <- c(0,0,0)
ub <- c(2,2,2)

result <- Steepest_hill_climibing_w_replace(fn, lb, ub, ini, change, stop_fun, full_output = T)
plot(result$obj)

# Algorithm 7 
# Generate a Random Real-Valued Vector
ini <- function(lb, ub){
  le <- length(lb)
  x <- rep(NA_real_, le)
  
  for (i in 1:le){
    li <- lb[i]
    ui <- ub[i]
    x[i] <- runif(1, li, ui)
  }
  return(x)
}

lb <- c(0,0,0)
ub <- c(1,1,1)

ini(lb, ub)

# Algorithm 8 
# Bounded Uniform Convolution
change <- function(x, lb, ub, noise = 0.5 * (ub - lb), p = 1) {
  # x: vector to be changed 
  # lb: lower bond of x vector
  # ub: upper bond of x vector
  # noise: noises to be added to x vector 
  # p: probability of changing a element in vector
  
  le <- length(x)
  dx <- rep(0, le) # noices to be added
  
  for (i in 1:le) {
    if (p >= runif(1)) {
      add_noise <- T
      while (add_noise) {
        dx[i] <- runif(1, -1, 1) * noise[i]
        temp <- x[i] + dx[i]
        range_check <- sum(temp < lb[i]) + sum(temp > ub[i]) # range_check == 0, if temp is within the range
        add_noise <- ifelse(range_check == 0, F, T) # if temp is within range, break while, else repeat
      }
    }
  }
  
  return (x + dx)
}

change(c(0.5,0.5), c(0,0), c(1,1), p = 0.5)
change(c(0.5,0.5), c(0,0), c(1,1), p = 1)
change(c(0.5,0.5), c(0,0), c(1,1), p = 0)

# Algorithm 9 
# Random Search
Random_search <- function(fn, lb, ub, random, stop_fun, max_it = 200, full_output = F){
  le <- length(lb)
  S_archive <- matrix(NA, nrow = max_it, ncol = le)
  obj <- rep(NA_real_, max_it)
  
  S <- random(lb, ub) #  The Initialization Procedure
  best <- fn(S)
  
  gen <- 1 # generation counter
  S_archive[gen, ] <- S
  obj[gen] <- best
  
  stop_search <- F 
  while (!stop_search) {
    gen <- gen + 1
    R <- random(lb, ub)
    R_obj <- fn(R)
    if (R_obj > best) {
      S <- R
      best <- R_obj
    }
    
    obj[gen] <- best
    S_archive[gen, ] <- S
    stop_search <- stop_fun(S_archive) # If stop fun returns T, then stop search
  }
  
  S_archive <- S_archive[!is.na(S_archive[, 1]), ] # remove NA values
  obj <- obj[!is.na(obj)] # remove NA values
  
  if (full_output){
    return(list(S_archive = S_archive, obj = obj))
  } else {
    return(S)
  }
}

result <- Random_search(fn, lb, ub, random, stop_fun, max_it = 200, full_output = T)
plot(result$obj)

# Algorithm 10 
# Hill-Climbing with Random Restarts
Search_w_restart <- function(search_algorithm, num_search , max_time, 
                             fn, lb, ub, change, stop_fun, max_it = 200, full_output = F, ...){
  
  time_per_search <- ceiling(runif(num_search)*max_time)
  for (i in 1:num_search){
    search_algorithm(fn, lb, ub, ini, change, stop_fun, n = 5, max_it = time_per_search, full_output = F)
    
  }
}
























































