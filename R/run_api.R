library(plumber)

r <- plumb("R/endpoint.R")

r$handle("GET", "/", function(req, res){
  2000
})



r$run(port = as.integer(Sys.getenv("PORT", unset = 8080)), host = "0.0.0.0")
