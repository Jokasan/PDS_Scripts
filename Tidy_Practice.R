## Some Tidyverse Practice, PDSR:

## Load in data for examples:

pacman::p_load(rvest,tidyverse)

current_year = lubridate::year(Sys.Date())

# url = glue::glue("http://www.basketball-reference.com/leagues/NBA_{current_year-1}_totals.html")
# 
# bball = read_html(url) %>% 
#   html_nodes("#totals_stats") %>% 
#   html_table() %>% 
#   data.frame() 
# 
# save(bball, file='bball.RData')

### Selecting Columns:
bball |> colnames()

bball |> select(Player,Pos,Age) |> head()
# Do the inverse for not selecting specific columns:
bball |> select(-Player,-Pos,-Age) |> head()

## Helper Functions:
bball |> select(Player, contains("3P"), ends_with("RB"), starts_with("X")) |> head()

## Filtering Rows + Slice:

bball |> 
  filter(Age < 32, Pos == "PF" | Pos == "C") |> 
  distinct(Player, Pos,Age) |> 
  slice(1:10)

# Filtering even works for newly created variables:

bball |> 
  unite("PosTeam", Pos,Tm) |> 
  filter(PosTeam == "C_TOT") |> 
  select(Player,PosTeam,Age) |> 
  arrange(desc(Age))
  
# Generating new data:

# The following example selects every variable minus the specified ones
# and converts them to numeric:

bball2 <- bball |> 
  mutate(across(c(-Player,-Pos,-Tm), as.numeric))

# Now that the variables are the correct format we can summarise it as follows:
bball2 = bball %>% 
  mutate(
    trueShooting = PTS / (2 * (FGA + (.44 * FTA))),
    effectiveFG  = (FG + (.5 * X3P)) / FGA,
    shootingDif  = trueShooting - FG.
  )

summary(select(bball2, shootingDif)) 

## Grouping and summarizing data:

# Another common task is to look at group based statistics. Conceptually
# the task involves three main steps: split, apply, combine, to get figures for 
# each position:

bball |> 
  mutate(across(c(-Player,-Pos,-Tm), as.numeric)) |> 
  select(Pos, FG, FGA, FG., FTA, X3P, PTS) %>% 
  mutate(
    trueShooting = PTS / (2 * (FGA + (.44 * FTA))),
    effectiveFG = (FG + (.5 * X3P)) / FGA,
    shootingDif = trueShooting - FG.
  ) %>%  
  group_by(Pos) %>%                                                 
  summarize(
    `Mean FG%` = mean(FG., na.rm = TRUE),
    `Mean True Shooting` = mean(trueShooting, na.rm = TRUE)) |> 
  na.omit()

# A lot can be done with grouped data. The following example groups data by 
# position, the calculates the correlation between field-goal percentage and
# free-throw shooting percentage. some players have multiple positions, so we
# will reduce those to whatever their first position is, using case_when(). 

bball %>% 
  mutate(
    Pos = case_when(
      Pos == 'PG-SG' ~ 'PG',
      Pos == 'C-PF'  ~ 'C',
      Pos == 'SF-SG' ~ 'SF',
      Pos == 'PF-C'  | Pos == 'PF-SF' ~ 'PF',
      Pos == 'SG-PF' | Pos == 'SG-SF' ~ 'SG',
      TRUE ~ Pos
    )) %>% 
  nest_by(Pos) %>%     
  mutate(FgFt_Corr = list(cor(data$FG., data$FT.))) %>% 
  unnest(c(Pos, FgFt_Corr))

# Since data frames are lists, anything can be done to the columns, we can
# even run regression models:

library(nycflights13)

flights |> 
  group_by(carrier) -> carriers

group_size(carriers)

flights |> 
  nest_by(carrier) |> 
  mutate(model= list(lm(arr_delay~dep_time, data = data))) -> mods
mods

mods |> 
  summarise(
    carrier = carrier,
    AdjRsquar = summary(model)$adj.r.squared,
    coef_dep_time = coef(model)[2]) 

## Renaming Columns:
# Easy way to clean column names:
janitor::clean_names(bball)

# The column names in a data frame may often be more complex than simply changing
# The follwoing example uses regular expressions to clean up the names of the 
# variables:

bball = bball %>%
  rename_with(
    str_replace,      # function
    contains('.'),    # columns
    pattern = '\\.',  # function arguments
    replacement = '%'
  ) %>% 
  rename_with(str_remove, starts_with('X'), pattern = 'X')

colnames(bball)

## Merging Data:
# A left_join():

band_members
band_instruments
left_join(band_members,band_instruments)
right_join(band_members,band_instruments)
inner_join(band_members,band_instruments)
full_join(band_members,band_instruments)
anti_join(band_members, band_instruments)
anti_join(band_instruments, band_members)

## Pivoting Axes
# Some of the following tidyr functions, under the 
# tidyverse umbrella will be necessary for you to 
# be familiar with:

# Turning wide data into long format:
stocks <- data.frame(
  time= as.Date(Sys.Date())+ 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)

stocks %>% 
  head()

# Turn this data into long format using pivot_longer:

stocks %>% 
  pivot_longer(
    cols = -time,
    names_to = 'stock',
    values_to = 'price'
  ) %>% 
  head()
# A more complex examples containing multiple repeated
# entries:

stocks <- data.frame(
  time = as.Date('2009-01-01') + 0:9,
  X_1 = rnorm(10, 0, 1),
  X_2 = rnorm(10, 0, 1),
  Y_1 = rnorm(10, 0, 2),
  Y_2 = rnorm(10, 0, 2),
  Z_1 = rnorm(10, 0, 4),
  Z_2 = rnorm(10, 0, 4)
)
stocks %>% head()

# An additional column is added for labeling and posit the
# separator for the column names:

stocks %>% 
  pivot_longer(
    cols = -time,
    names_to = c('stock','entry'),
    names_sep = '_',
    values_to = 'price'
  ) %>% 
  head()

# The most previous example is not an example of tidy data
# The following is an example using the bball dataset. Separating
# player into first and last names based on space:

bball %>% 
  separate(Player, into = c('first_name', 'last_name'), sep=' ') %>% 
  select(1:5) %>% 
  head()

# Tidyverse Exercises:
# load in the movies dataset form ggplot2movies
pacman::p_load(ggplot2movies)
data('movies')

# Exercise 1a:
# Use mutate to create a centered version of the rating variable,
# A centered variable is one whose mean has been subtracted from
# it:
movies %>% 
  transmute(centered_rating = rating - mean(rating))

# Exercise 1b:
# Use filter to create a new data frame that has only movies 
# from the years 2000 and beyond. Use the greater than or equal operator.

movies %>% 
  filter(year >= 2000) -> df

# Exercise 1c:
# Select only title, year, budget,length, rating and votes:
# At least three ways to do this:
movies %>% 
  select(title:votes)

movies %>% 
  select(title, year,budget,length,rating,votes)

movies %>% 
  select(1:6)

# 1d, rename length into length_in_min
movies %>% 
  rename(length_in_min = length) %>% 
  select(length_in_min)

## Exercise 2, Use group_by to group the data by year, and summarize to create 
# a new variable that is the average budget. The summarize function works just 
# like mutate in this case. Use the mean function to get the average, but you’ll
# also need to use the argument na.rm = TRUE within it because the earliest years
# have no budget recorded.

movies %>% 
  group_by(year) %>% 
  summarise(avg_budg=mean(budget, na.rm=TRUE)) %>% 
  ungroup() %>% 
  arrange(desc(avg_budg)) 

## Exercise 3
# use pivot_longer to create tidy data from the following data:
data = tibble(id=1:10,
              x=rnorm(10),
              y =rnorm(10))

data %>% 
pivot_longer(!id, names_to = "value",
             values_to = "values")

## Exercise 4
# Put several actions together in piped ops:
# filter movies after 1990
# select select variables as before but also 
# mpaa, Action and Drama
# group by action or drama 
# The average rating:

movies %>% 
  select(title, year,budget,length,rating,votes,mpaa,Action,Drama) %>% 
  group_by(mpaa,Action) %>% 
  summarise(avg_rating=mean(rating,na.rm=TRUE))





















