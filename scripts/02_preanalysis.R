library(tidyverse)

combo <- read_csv("data/raw_data.csv")

#### Correlations ####

# Looking at how numeric predictors correlation with target
correlations <- combo %>%
  select_if(is.numeric) %>%
  select(poc:DBPM) %>%
  cor(use = "complete.obs")
# Most correlated variables are offense based
# Only a few slightly negative correlated variables
correlations %>%
  as_tibble(rownames = "field") %>%
  select(field, poc) %>%
  mutate(field = fct_reorder(field, poc)) %>%
  filter(poc < 1) %>%
  ggplot(aes(x = field, y = poc)) +
  geom_col() +
  coord_flip()


#### Categorical predictors ####

# Only real categorical predictor we might care about is position
combo %>%
  select_if(~ !is.numeric(.))

# A lot of levels with position
# Should simplify this
combo %>%
  ggplot(aes(x = Pos, y = poc)) +
  geom_boxplot()

# If a player changes teams, they have a slightly lower poc
combo %>%
  mutate(team_change = Tm == "TOT") %>%
  ggplot(aes(x = team_change, y = poc)) +
  geom_boxplot()
