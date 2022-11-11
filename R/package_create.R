library(usethis)
#https://r-pkgs.org/whole-game.html
create_package(getwd())
use_git()



use_mit_license("John Thomas Menchaca")


library(tidyverse)
options(dplyr.summarise.inform = FALSE)
library(readxl)
library(writexl)
library(janitor)
library(broom)
library(glue)
library(scales)
library(fastDummies)
