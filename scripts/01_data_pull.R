library(rvest)
library(nbastatR)
library(tidyverse)


#### Free Agency Data ####

# Selecting years used for data set
years <- as.character(2017:2020)

# Looping over years to create urls to pull data from
base_url <- "https://www.basketball-reference.com/friv/free_agents.cgi?year="
urls <- map(years,
            ~ paste0(base_url, .))
# Looping over urls to pull free agency data
raw_data <- map2_df(urls, years,
                    ~ read_html(.x) %>%
                      html_node('#free_agents') %>%
                      html_table() %>%
                      mutate(year = .y))

# Cleaning up free agency data
# Extracting length and salary from Terms field
free_agency <- raw_data %>%
  select(Player, year, Terms) %>%
  mutate(length = str_extract(Terms, "([0-9])(['-])(yr+)")) %>%
  mutate(length = as.numeric(str_extract(length, "[0-9]"))) %>%
  mutate(total_salary = parse_number(str_extract(Terms,"\\$(.*?)M"))) %>%
  # Fixing edge cases
  mutate(total_salary = ifelse(Player == "Klay Thompson" &
                                 year == "2019", 189.9, total_salary))

# There are some players who signed a contract but don't have a salary
# Mostly made up of those who just have a minimum deal
# Filtering for these
no_salary <- free_agency %>%
  filter(is.na(total_salary) & !(is.na(length)))
# Pulling player bios (includes salary info)
player_bios <- bref_bios(players = no_salary$Player)
# Extracting salary info for each of these missing players
# NOTE: Going to remove anyone who had multiple salaries in one year
salaries_for_missing <- player_bios %>%
  filter(nameTable %in% c("Salaries", "Contracts")) %>%
  unnest(dataTable) %>%
  mutate(year = str_extract(slugSeason, "^[0-9]{4}")) %>%
  group_by(namePlayerBREF, year) %>%
  add_count() %>%
  filter(n == 1) %>%
  select(Player = namePlayerBREF, year, avg_salary = amountSalary)
# Joining this info to missing player tibble
# NOTE: This is taking the first year of the contract into account only
no_salary <- no_salary %>%
  select(-total_salary) %>%
  inner_join(salaries_for_missing)

# Calculating average salary for those who have total salary
free_agency <- free_agency %>%
  filter(!is.na(total_salary)) %>%
  mutate(avg_salary = (total_salary / length) * 1000000) %>%
  select(Player, year, length, avg_salary)
# Combining free agency tibble with missing players
free_agency <- no_salary %>%
  select(Player, year, length, avg_salary) %>%
  bind_rows(free_agency)

# Calculating salary cap at different offseasons
salary_cap <- tibble(
  year = as.character(2017:2020),
  cap = c(99093000, 101869000, 109140000, 109140000))
free_agency <- free_agency %>%
  left_join(salary_cap) %>%
  mutate(poc = avg_salary / cap)


#### Stats Data ####

# Going to pull traditional and advanced stats from bref

# Creating per game and advanced urls
per_game_urls <- map(
  years,
  ~ paste0("https://www.basketball-reference.com/leagues/NBA_", .x,
           "_per_game.html"))
advanced_urls <- map(
  years,
  ~ paste0("https://www.basketball-reference.com/leagues/NBA_", .x,
           "_advanced.html"))

# Pulling data and putting it into data frames
raw_per_game <- map2_df(per_game_urls, years,
                        ~ read_html(.x) %>%
                          html_node('#per_game_stats') %>%
                          html_table() %>%
                          setNames(make.names(names(.),
                                              unique = TRUE)) %>%
                          mutate(year = .y))
raw_advanced <- map2_df(advanced_urls, years,
                        ~ read_html(.x) %>%
                          html_node('#advanced_stats') %>%
                          html_table() %>%
                          setNames(make.names(names(.),
                                              unique = TRUE)) %>%
                          mutate(year = .y))

# Selecting a specific set of columns
fields_per_game <- c("year", "Player", "Pos", "Age", "Tm", "G",
                     "FG", "FGA", "FG.", "X3P", "X3PA", "X3P.",
                     "FT", "FTA", "FT.", "ORB", "DRB", "TRB",
                     "AST", "STL", "BLK", "TOV", "PTS")
fields_advanced <- c("year", "Player", "Tm", "TS.", "X3PAr",
                     "FTr", "USG.", "OWS", "DWS", "OBPM", "DBPM")
# Joining data
stats <- raw_per_game %>%
  select(all_of(fields_per_game)) %>%
  inner_join(raw_advanced %>%
               select(all_of(fields_advanced))) %>%
  group_by(Player, year) %>%
  add_count() %>%
  filter(n == 1 | Tm == "TOT") %>%
  select(-n)

combo <- free_agency %>%
  left_join(stats) %>%
  mutate_at(.vars = vars(Age, G:DBPM), .funs = as.numeric)

write_csv(combo, "data/raw_data.csv")
