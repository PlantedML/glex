library(future)
library(future.apply)
library(doFuture)

registerDoFuture()
plan(multicore)
source("main_loop.r")

# Read command-line arguments
file <- "params.csv"
args <- commandArgs(trailingOnly = TRUE)

line_num <- as.numeric(args[1])
nsim <- as.numeric(args[2])


loaded_params <- read.csv("params.csv")

sim_params <- loaded_params[line_num, ]

print(paste("Simulating using parameters:", sim_params))
print(paste("For this many iterations:", nsim))


# sim_res <- simulate_for_B(sim_params$n, sim_params$c, sim_params$s, B = nsim, par = T)

N <- 10^(2:6)
sim_res <- simulate_for_N(N = N, s = F, c = 0.3, only_emp_p = T)
save(sim_res, file = paste0('sim_res', sim_params$n, sim_params$c, sim_params$s, '.RData'))

print("Finished! and saved")