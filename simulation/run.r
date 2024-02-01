source("main_loop.r")

# Read command-line arguments
file <- "params.csv"
args <- commandArgs(trailingOnly = TRUE)

line_num <- as.numeric(args[1])
nsim <- as.numeric(args[2])


loaded_params <- read.csv("params.csv")

sim_params <- loaded_params[line_num, ]

print("Simulating using parameters:")
print(sim_params)
print("For this many iterations:")
print(nsim)


sim_res <- simulate_for_B(sim_params$n, sim_params$c, sim_params$s, B = nsim)
save(sim_res, paste0('sim_res', sim_params$n, sim_params$c, sim_params$s, '.RData'))