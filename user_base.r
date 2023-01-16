# passwords for air quality app

# create a user base then hash passwords with sodium 
# then save to an rds file in app directory 
library(sodium)
library(tibble)
library(purrr)

# Do not upload this to github!!!
pw <- read.csv("passwordsBook.csv")

user_base <- tibble::tibble(
  user = pw$user, 
  password = purrr::map_chr(pw$password, 
                            sodium::password_store), 
  permissions = pw$permissions, # aren't used for anything 
  name = pw$name
)

saveRDS(user_base, "www/user_base.rds")

