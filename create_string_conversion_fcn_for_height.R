data <- read.csv('data.csv')
height <- data$Height

convert_to_height <- function(character) {
  library(stringr)
  feet_inches <- str_split(character, pattern = "'")
  feet_inches <- as.integer(unlist(feet_inches))
  inches <- 12*feet_inches[1] + feet_inches[2]
  return(inches)
}

first_try <- convert_to_height(height[1])