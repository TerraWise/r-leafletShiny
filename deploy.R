library(rsconnect)
library(aws.s3)

setAccountInfo(name = "terrawise",
               token = Sys.getenv("SHINY_TOKEN"),
               secret = Sys.getenv("SHINY_SECRET"))

deployApp(forceUpdate = TRUE,
          appFiles = c("app.R", "Data/", "www/",
                       "_brand.yml", "renv.lock"))