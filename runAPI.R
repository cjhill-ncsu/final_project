
library(plumber)

r <- plumb("API.R")

#run on the port in Dockerfile
r$run(host = "0.0.0.0", port = 8000) 
