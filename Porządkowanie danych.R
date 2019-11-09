library(tidyverse)

######################
# Tibbles
######################
install.packages("tidyverse")
library(tidyverse)

as_tibble(iris)
tibble(
  x = 1:5, 
  y = 1, 
  z = x ^ 2 + y
)

tb <- tibble(
  `:)` = "smile", 
  ` ` = "space",
  `2000` = "number"
)
tb

df <- tibble(
  x = runif(5),
  y = rnorm(5)
)

# Extract by name
df$x
df[["x"]]

# Extract by position
df[[1]]

df %>% .$x
df %>% .[["x"]]

######################
#Tidy data
######################

# Data set one
table1
# Data set two
table2
# Data set three
table3
# Data set four
table4a  # cases
table4b  # population
# Data set five
table5


# Data set one
table1$cases / table1$population * 10000

# Data set two
case_rows <- c(1, 3, 5, 7, 9, 11, 13, 15, 17)
pop_rows <- c(2, 4, 6, 8, 10, 12, 14, 16, 18)
table2$value[case_rows] / table2$value[pop_rows] * 10000

# Data set three
# No basic solution

# Data set four & five
cases <- c(table4a$`1999`, table4a$`2000`)
population <- c(table4b$`1999`, table4b$`2000`)
cases / population * 10000

######################
#spread
######################
?spread
table2
spread(table2, type, count)

######################
#gather & join
######################
?gather
table4a  # cases
gather(table4a, "year", "cases", 2:3)

table4b  # population
gather(table4b, "year", "population", -1)

tidy4a <- gather(table4a, "year", "cases", 2:3)
tidy4b <- gather(table4b, "year", "population", 2:3)
left_join(tidy4a, tidy4b)

######################
#separate
######################
table3
separate(table3, rate, into = c("cases", "population"))
separate(table3, rate, into = c("cases", "population"), sep = "/")
separate(table3, year, into = c("century", "year"), sep = 2)

######################
#unite
######################
table5
unite(table5, "new", century, year, sep = "")

######################
#data pipeline
######################
library(ggplot2)
library(dplyr)
data(diamonds)
head(diamonds)

# By using intermediate values
cut_depth <- group_by(diamonds, cut, depth)
cut_depth
cut_depth <- summarise(cut_depth, n = n())
cut_depth
cut_depth <- filter(cut_depth, depth > 55, depth < 70)
cut_depth
cut_depth <- mutate(cut_depth, prop = n / sum(n))
cut_depth

cut_depth1 <- group_by(diamonds, cut, depth)
cut_depth1
cut_depth2 <- summarise(cut_depth1, n = n())
cut_depth2
cut_depth3 <- filter(cut_depth2, depth > 55, depth < 70)
cut_depth3
cut_depth4 <- mutate(cut_depth3, prop = n / sum(n))
cut_depth4

# By "composing" functions
mutate(
  filter(
    summarise(
      group_by(
        diamonds,
        cut,
        depth
      ),
      n = n()
    ),
    depth > 55,
    depth < 70
  ),
  prop = n / sum(n)
)

#With the pipe, we can write the above sequence of operations as:
cut_depth <- diamonds %>%
  group_by(cut, depth) %>%
  summarise(n = n()) %>%
  filter(depth > 55, depth < 70) %>%
  mutate(prop = n / sum(n))
cut_depth

# Data frames are shared in memory
install.packages("pryr")
library(pryr)
diamonds2 <- diamonds %>%
  mutate(price_per_carat = price / carat)
object_size(diamonds)
object_size(diamonds2)
object_size(diamonds,diamonds2)

######################
# Data import/export
######################
heights <- read_csv("heights.csv")

read_csv("a,b,c
  1,2,3
  4,5,6")

read_csv("The first line of metadata
  The second line of metadata
  x,y,z
  1,2,3", skip = 2)

read_csv("# A comment I want to skip
  x,y,z
  1,2,3", comment = "#")

read_csv("1,2,3\n4,5,6", col_names = c("x", "y", "z"))

#specify the value (or values) that are used to represent missing values
read_csv("a,b,c\n1,2,.", na = ".")

# Parsing
parse_integer(c("1","2","0.1","4","$4.5"))
parse_double(c("1","2","0,1","4","$4.5"))
parse_double(c("1","2","0,1","4","$4.5"), 
             locale = locale(decimal_mark = ","))
parse_number(c("1","2","0,1","4","$4.5"), 
             locale = locale(decimal_mark = ","))
parse_character("Chrz¹szcz brzmi w trzcinie")
parse_character("Chrz¹szcz brzmi w trzcinie",
                locale = locale(encoding = "Windows-1250"))
parse_datetime("2017-01-02T1215")

guess_parser(c("1","2","0.1","4","$4.5"))

read_csv("heights.csv", 
         guess_max = 1001)
read_csv("heights.csv", 
         col_types = cols(.default = col_character()))

#Writing to a file

write.csv(heights, "heights2.csv")
