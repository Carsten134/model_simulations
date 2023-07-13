## requirements ########################
library(tidyverse)
library(stats)

## model-parameters ####################
# size of simulation
n <- 1000

# model parameters - lowest and highest
# quality of car
q_low <- 0.1
q_high <- 1

# evaluations of buyer and seller
# IMPORTANT: buyer must be lower than seller
v_buyer <- 0.5
v_seller <- 0.4


## simulation #########################

# abstracting data processing from distribution
# this way it is way easier to test the model on different distributions
post_processing <- function(sim_data) {
  if (!is.vector(sim_data)) {
    stop("error in post_processing, sim_data is not a vector")
  }
  if (!is.numeric(sim_data)) {
    stop("error in post_processing, sim_data is not a numeric vector")
  }
  n <- length(sim_data)
  sim_quality <- sort(sim_data)
  
  avg_quality <- rep(0, n)
  avg_quality[1] <- sim_quality[1]
  
  for (i in 2:n) {
    avg_quality[i] <- (1/i)*((i-1)*avg_quality[i-1] + sim_quality[i])
  }
  
  result <- data.frame(y = c(sim_quality, avg_quality),
                         x = c(sim_quality, sim_quality),
                         line_category = c(rep("indifference curve", n), rep("avg_quality", n)))
  return(result)
}

# 
## plotting ###########################

sim_data %>% ggplot(aes(x = x,
                        y = y,
                        group = line_category,
                        color = line_category)) +
  geom_line()
