cleaNDatA <- data %>%
  rename(ID = 1, Name = 2, Age = 3) %>%
  mutate(Age = ifelse(Age < 0, NA, Age)) %>%
  drop_na() %>%
  mutate(Age = as.integer(Age)) %>%
  select(-Name)
