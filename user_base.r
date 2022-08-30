# passwords for air quality app

# create a user base then hash passwords with sodium 
# then save to an rds file in app directory 
library(sodium)
library(tibble)
library(purrr)

user_base <- tibble::tibble(
  user = c("user1", "user2"), 
  password = purrr::map_chr(c("pass1", "pass2"), sodium::password_store), 
  permissions = c("admin", "standard"), 
  name = c("User One", "User Two")
)

saveRDS(user_base, "user_base.rds")