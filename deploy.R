library(rsconnect)

setAccountInfo(name = "terrawise",
               token = Sys.getenv("SHINY_TOKEN"),
               secret = Sys.getenv("SHINY_SECRET"))

deployApp(forceUpdate = TRUE,
          appFiles = c("Data/", "www/"))