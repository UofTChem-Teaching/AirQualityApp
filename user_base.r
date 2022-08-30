# passwords for air quality app

# create a user base then hash passwords with sodium 
# then save to an rds file in app directory 
library(sodium)
library(tibble)
library(purrr)

user_base <- tibble::tibble(
  user = c("TA", "Jess"), 
  password = purrr::map_chr(c("CHM135_TA", "CHM135_Jess"), 
                            sodium::password_store), 
  permissions = c("standard", "admin"), # aren't used for anything 
  name = c("User One", "User Two")
)

saveRDS(user_base, "www/user_base.rds")