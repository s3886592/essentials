library(tidyverse)
library(stringr)
library(modelr)

# Algorithm 0
# Bubble Sort

Bubble_sort <- function(x) {
  le <- length(x)
  swapped <- T
  
  if (le < 2) {
    return(x)
  } else {
    while (swapped) {
      swapped <- F
      for (i in 1:(le - 1)) {
        if (x[i] > x[i + 1]) {
          
          temp <- x[i]
          x[i] <- x[i + 1]
          x[i + 1] <- temp
          
          swapped <- T
        }
      }
    }
    return(x)
  }
}

# Test
Bubble_sort(1)

Bubble_sort(c(1,-2,3,45,5,6))
