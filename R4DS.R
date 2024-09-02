# 3.2.4
library(ggplot2)
ggplot(data = mpg)
#1. an empty graph
#2
dim(mpg)[1] #rows 
dim(mpg)[2] #cols
#3
?mpg
#4
ggplot(data = mpg) +
  geom_point(mapping = aes(x = hwy ,y= cyl))
#5 categorical variables
ggplot(data = mpg) +
  geom_point(mapping = aes(x = class ,y= drv))
# 3.3.1
#1
install.packages("nycflights13")
library(nycflights13)
library(tidyverse)
?flights
dim(flights)
arr_delay_2_hours <- filter(flights , arr_delay >= 120)
to_houston <- filter(flights, dest %in% c('IAH','HOU') )
dep_sum <- filter(flights , month %in% c(7,8,9))
arr_late_only <- filter(flights , arr_delay > 120 , dep_delay <=0)

read_delim("file_path.txt", delim = "|")


read_csv('"x,y\n1,\'a,b\'"', quote = "'")



read_csv("a,b\n1,2,3\n4,5,6")


read_csv("a,b,c\n1,2\n1,2,3,4")

read_csv("a,b\n\"1")

read_csv("a,b\n1,2\na,b")

read_delim("a;b\n1;3", delim = ";")

library("ggbeeswarm")
library("lvplot")
install.packages("ggstance")
library("ggstance")

summary(select(diamonds, x, y, z))
ggplot(diamonds) +
  geom_histogram(mapping = aes(x = x), binwidth = 0.01)
ggplot(filter(diamonds), aes(x = price)) +
  geom_histogram(binwidth = 100, center = 0)

my_locale <- locale(date_format = "%d/%m/%Y")
parse_date("31/12/2023", locale = my_locale)

my_locale <- locale(decimal_mark = ",", grouping_mark = ".")


library(readr)


d1 <- "January 1, 2010"
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("August 19 (2015)", "July 1 (2015)")
d5 <- "12/30/14" # Dec 30, 2014

t1 <- "1705"
t2 <- "11:15:10.12 PM"


parse_date(d1, format = "%B %d, %Y")
parse_date(d2, format = "%Y-%b-%d")
parse_date(d3, format = "%d-%b-%Y")
parse_date(d4, format = "%B %d (%Y)")
parse_date(d5, format = "%m/%d/%y")

parse_time(t1, format = "%H%M")
parse_datetime(t2, format = "%I:%M:%S.%f %p")
library(tidyverse)

table1 <- tibble(
  country = c("Afghanistan", "Afghanistan", "Brazil", "Brazil", "China", "China"),
  year = c(1999, 2000, 1999, 2000, 1999, 2000),
  cases = c(745, 2666, 37737, 80488, 212258, 213766),
  population = c(19987071, 20595360, 172006362, 174504898, 1272915272, 1280428583)
)

table2 <- tibble(
  country = c(rep(c("Afghanistan", "Brazil", "China"), each = 2)),
  year = rep(c(1999, 2000), 3),
  type = rep(c("cases", "population"), 3),
  count = c(745, 19987071, 2666, 20595360, 37737, 172006362, 80488, 174504898, 212258, 1272915272, 213766, 1280428583)
)

table4a <- tibble(
  country = c("Afghanistan", "Brazil", "China"),
  `1999` = c(745, 37737, 212258),
  `2000` = c(2666, 80488, 213766)
)

table4b <- tibble(
  country = c("Afghanistan", "Brazil", "China"),
  `1999` = c(19987071, 172006362, 1272915272),
  `2000` = c(20595360, 174504898, 1280428583)
)

rate_table2 <- table2 %>%
  group_by(country, year) %>%
  summarize(rate = sum(count[type == "cases"]) / sum(count[type == "population"]) * 10000)

combined_table <- left_join(table4a, table4b, by = "country")
rate_combined <- combined_table %>%
  pivot_longer(cols = c(cases_1999, cases_2000, population_1999, population_2000),
               names_to = c("year", "type"),
               values_to = "value") %>%
  group_by(country, year) %>%
  summarize(rate = sum(value[type == "cases"]) / sum(value[type == "population"]) * 10000)

ggplot(table1, aes(year, cases)) +
  geom_line(aes(group = country), color = "grey50") +
  geom_point(aes(color = country))

tidy_table2 <- table2 %>%
  pivot_wider(names_from = type, values_from = count)
ggplot(tidy_table2, aes(year, cases)) +
  geom_line(aes(group = country), color = "grey50") +
  geom_point(aes(color = country))

library("stringr")
install.packages("microbenchmark")
library("microbenchmark")
output <- vector("double", ncol(mtcars))
names(output) <- names(mtcars)
for (i in names(mtcars)) {
  output[i] <- mean(mtcars[[i]])
}
output

output <- vector("list", ncol(nycflights13::flights))
names(output) <- names(nycflights13::flights)
for (i in names(nycflights13::flights)) {
  output[[i]] <- class(nycflights13::flights[[i]])
}
output

library("modelr")
library("lubridate")
library("broom")
library("splines")

tibble(carat = seq(0.25, 5, by = 0.25)) %>%
  add_predictions(mod_log) %>%
  ggplot(aes(x = carat, y = 2^pred)) +
  geom_line() +
  labs(x = "carat", y = "price")


install.packages("gapminder")
library("gapminder")

lifeExp ~ poly(year, 2)

country_model <- function(df) {
  lm(lifeExp ~ poly(year - median(year), 2), data = df)
}

by_country <- gapminder %>%
  group_by(country, continent) %>%
  nest()

by_country <- by_country %>%
  mutate(model = map(data, country_model))
by_country <- by_country %>%
  mutate(
    resids = map2(data, model, add_residuals)
  )

install.packages("ggbeeswarm")
library("viridis")
library("datamodelr")
library("ggbeeswarm")
by_country %>%
  mutate(glance = map(model, broom::glance)) %>%
  unnest(glance, .drop = TRUE) %>%
  ggplot(aes(continent, r.squared)) +
  geom_beeswarm()



map(1:3, runif)

range(mtcars$mpg)
#> [1] 10.4 33.9
fivenum(mtcars$mpg)
#> [1] 10.4 15.3 19.2 22.8 33.9
boxplot.stats(mtcars$mpg)

mtcars %>%
  group_by(cyl) %>%
  summarise(q = list(quantile(mpg))) %>%
  unnest()

quantile(mtcars$mpg)

mtcars %>%
  group_by(cyl) %>%
  summarise_each(funs(list))

ggplot(
  data = mpg,
  mapping = aes(x = fct_reorder(class, hwy), y = hwy)
) +
  geom_boxplot() +
  coord_flip() +
  labs(
    title = "Compact Cars have > 10 Hwy MPG than Pickup Trucks",
    subtitle = "Comparing the median highway mpg in each class",
    caption = "Data from fueleconomy.gov",
    x = "Car Class",
    y = "Highway Miles per Gallon"
  )

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Fuel Efficiency Decreases with Engine Size",
    caption = "Data from fueleconomy.gov",
    y = "Highway Miles per Gallon",
    x = "Engine Displacement"
  )
df <- tibble(
  x = rnorm(10000),
  y = rnorm(10000)
)
ggplot(df, aes(x, y)) +
  geom_hex() +
  scale_colour_gradient(low = "white", high = "red") +
  coord_fixed()

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  geom_smooth(se = FALSE) +
  scale_x_continuous("Engine displacement (L)") +
  scale_y_continuous("Highway fuel economy (mpg)") +
  scale_colour_discrete("Car type")