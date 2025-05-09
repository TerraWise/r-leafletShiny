library(rsconnect)
library(aws.s3)

setAccountInfo(name = "terrawise",
               token = Sys.getenv("SHINY_TOKEN"),
               secret = Sys.getenv("SHINY_SECRET"))

Sys.setenv(
  "AWS_ACCESS_KEY_ID" = Sys.getenv("AWS_ACCESS_KEY_ID"),
  "AWS_SECRET_ACCESS_KEY" = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
  "AWS_DEFAULT_REGION" = "ap-southeast-2" # Replace with your region
)

# Test S3 access
print(bucketlist())  # List all accessible buckets

# deployApp(forceUpdate = TRUE,
#           appFiles = c("app.R", "Data/", "www/",
#                        "_brand.yml", "renv.lock"))