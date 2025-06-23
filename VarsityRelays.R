library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Sheet1 - all swimmers
# Sheet 2 - No Gretchen Walsh
# Sheet 3 - No Gretchen Walsh or Alex Walsh
# Sheet 4 - No Gretchen Walsh, Alex Walsh, or Claire Curzan
df <- read.csv('Varsity Relays - Sheet1.csv')
colnames(df)[1] <- "Swimmer"
colnames(df)[2] <- "Back"
colnames(df)[3] <- "Breast"
colnames(df)[4] <- "Fly"
colnames(df)[5] <- "Free"

# Time of 2nd place 400 Medley Relay
goal_time_seconds = 204.98

# Create lists of swimmers with valid times for each stroke
backstrokers <- df %>% filter(!is.na(Back)) %>% select(Swimmer, Back)
breaststrokers <- df %>% filter(!is.na(Breast)) %>% select(Swimmer, Breast)
flyers <- df %>% filter(!is.na(Fly)) %>% select(Swimmer, Fly)
freestylers <- df %>% filter(!is.na(Free)) %>% select(Swimmer, Free)

# Generate all possible relay combinations
relays <- expand.grid(
  backstrokers$Swimmer, breaststrokers$Swimmer, flyers$Swimmer, 
  freestylers$Swimmer,
  stringsAsFactors = FALSE
)
colnames(relays) <- c("Back", "Breast", "Fly", "Free")

# Ensure unique swimmers in each relay
relays <- relays %>% 
  rowwise() %>% 
  filter(length(unique(c(Back, Breast, Fly, Free))) == 4) %>% 
  ungroup()

# Merge with times
times <- df %>% pivot_longer(-Swimmer, names_to = "Stroke", values_to = "Time")
relay_times <- relays %>%
  left_join(times %>% filter(Stroke == "Back"), by = c("Back" = "Swimmer")) %>%
  rename(Back_Time = Time) %>%
  left_join(times %>% filter(Stroke == "Breast"), 
            by = c("Breast" = "Swimmer")) %>%
  rename(Breast_Time = Time) %>%
  left_join(times %>% filter(Stroke == "Fly"), by = c("Fly" = "Swimmer")) %>%
  rename(Fly_Time = Time) %>%
  left_join(times %>% filter(Stroke == "Free"), by = c("Free" = "Swimmer")) %>%
  rename(Free_Time = Time)

# Compute total relay time and filter for valid relays
winning_relays <- relay_times %>%
  mutate(Total_Time = Back_Time + Breast_Time + Fly_Time + Free_Time) %>%
  filter(Total_Time <= goal_time_seconds) %>% 
  select(Back, Back_Time, Breast, Breast_Time, Fly, Fly_Time, Free, Free_Time,
         Total_Time) %>% 
  arrange(Total_Time)

# Generate all possible combinations of swimmers that can create at least 1 
# winning relay
unique_winning_relays <- winning_relays %>%
  rowwise() %>%
  mutate(Swimmer_Set = paste(sort(c(Back, Breast, Fly, Free)), 
                             collapse = ", ")) %>%
  ungroup() %>%
  count(Swimmer_Set, sort = TRUE)

# Generate which combination of swimmers can form the most event-winning relays
most_frequent_relay <- unique_winning_relays %>% slice_max(n, n = 1)

# Get all valid relays corresponding to the most frequent combination
most_frequent_relay_swimmers <- most_frequent_relay$Swimmer_Set[1]
most_frequent_winning_relays <- winning_relays %>%
  rowwise() %>%
  mutate(Swimmer_Set = paste(sort(c(Back, Breast, Fly, Free)), 
                             collapse = ", ")) %>%
  ungroup() %>%
  filter(Swimmer_Set == most_frequent_relay_swimmers)

write.csv(winning_relays, 'winning_relays.csv', row.names = TRUE)
