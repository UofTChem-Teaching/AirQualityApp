# passwords for air quality app

# create a user base then hash passwords with sodium 
# then save to an rds file in app directory 
library(sodium)
library(tibble)
library(purrr)

pw <- read.csv("passwordsBook.csv")

user_base <- tibble::tibble(
  user = c("", ""), 
  password = purrr::map_chr(c("", ""), 
                            sodium::password_store), 
  permissions = c("", ""), # aren't used for anything 
  name = c("", "")
)

saveRDS(user_base, "www/user_base.rds")

