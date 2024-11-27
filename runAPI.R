
library(plumber)

r <- plumb("API.R")

#run on the port in Dockerfile
r$run(port=8000)
