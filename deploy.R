library(rsconnect)

setAccountInfo(name = "terrawise",
               token = Sys.getenv("SHINY_TOKEN"),
               secret = Sys.getenv("SHINY_SECRET"))

print(paste("AWS_ACCESS_KEY_ID:", Sys.getenv("AWS_ACCESS_KEY_ID")))
print(paste("AWS_SECRET_ACCESS_KEY:", Sys.getenv("AWS_SECRET_ACCESS_KEY")))

deployApp(forceUpdate = TRUE,
          appFiles = c("app.R", "Data/", "www/",
                       "_brand.yml", "renv.lock"))