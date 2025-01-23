# Load necessary libraries
library(proxy)  # For distance calculations
library(dplyr)  # For data manipulation
library(tidyr)  # For data cleaning

# Load historical player data from CSV
historical_data <- read.csv("historical.csv")  

# Filter historical data to only include seasons after 1982
historical_data <- historical_data %>% filter(Year > 1982)  

# Load current player data from CSV
current_data <- read.csv("current.csv")  

# Load advanced metrics for current players
advanced <- read.csv("advanced2023.csv")  

# Find common columns between advanced stats and current data
column_join <- intersect(names(advanced), names(current_data))  

# Print the common columns for verification
print(column_join)  

# Merge current data with advanced metrics based on common player identifiers
current_data <- merge(current_data, advanced, by = c("Rk", "Player", "Age", "Pos", "G", "GS", "MP"), all = TRUE)  

# Ensure only one entry per player by taking the first non-missing value for each column
current_data <- current_data %>%
  group_by(Player) %>%
  summarize(across(everything(), ~ first(na.omit(.))))  

# Assign the current season year to the dataset
current_data <- current_data %>% mutate(Year = 2023)  

# Find common columns between historical and current datasets
common_columns <- intersect(names(historical_data), names(current_data))  

# Keep only common columns in historical data
historical_data <- historical_data[common_columns]  

# Keep only common columns in current data
current_data <- current_data[common_columns]  

# Print common columns for verification
print(common_columns)  

# Specify columns to exclude from numeric conversion
exclude_columns <- c("Player", "Pos", "Tm")  

# Convert all applicable columns to numeric format in the current dataset
current_data <- current_data %>%
  mutate(across(!all_of(exclude_columns), ~ as.numeric(as.character(.))))  

# Convert all applicable columns to numeric format in the historical dataset
historical_data <- historical_data %>%
  mutate(across(!all_of(exclude_columns), ~ as.numeric(as.character(.))))  

# Convert total stats in historical data to per-game stats by dividing by games played
historical_data <- historical_data %>%
  mutate(across(c(PTS, PF, TOV, BLK, STL, AST, TRB, DRB, ORB, FTA, FT, X2PA, X2P, X3PA, X3P, FG, FGA), ~ round(.x / G, 3)))  

# Add a "Source" column to differentiate between historical and current data
current_data$Source <- "Current"  
historical_data$Source <- "Historical"  

# Remove the first row from current data and other unneeded rows
current_data <- current_data[-1, ]  
current_data <- current_data %>% filter(Player!="Player")

# Combine historical and current datasets into one
all_data <- rbind(historical_data, current_data)  

# Normalize numerical columns, excluding categorical fields
all_data_scaled <- scale(select(all_data, -Player, -Pos, -Tm, -Source, -Year))  

# Replace any NA values in the dataset with 0
all_data_scaled[is.na(all_data_scaled)] <- 0  

# Extract scaled data for current players
current_scaled <- all_data_scaled[all_data$Source == "Current", , drop = FALSE]  

# Extract scaled data for historical players
historical_scaled <- all_data_scaled[all_data$Source == "Historical", , drop = FALSE]  

# Create an empty dataframe to store player matches
matches <- data.frame(Current_Player = character(),
                      Historical_Player = character(),
                      Distance = numeric(),
                      Historical_Year = integer(),
                      stringsAsFactors = FALSE)  
# Perform Principal Component Analysis (PCA) on the scaled current player data
pca_result <- prcomp(current_scaled, center = TRUE, scale. = TRUE)
#Extract absolute values of the first principal component's rotation coefficients# - gives more importance to features with higher PCA weights   
weights <- abs(pca_result$rotation[,1])
# Apply the calculated weights to the scaled current and historic player data
current_weighted <- current_scaled * weights
historical_weighted <- historical_scaled * weights

# Iterate through each current player to find their most similar historical player
for (i in 1:nrow(current_weighted)) {  
  # Extract the current player's stats
  current_row <- current_weighted[i, , drop = FALSE]  
  
  # Compute Euclidean distances between current player and all historical players
  distances <- proxy::dist(current_row, historical_weighted, method = "euclidean")  
  
  # Find the index of the closest historical match
  min_idx <- which.min(distances)
  # Extract the minimum distance (i.e., the closest historical player match)  
  min_distance <- distances[min_idx]
  # Find the maximum distance among all historical player comparisons  
  max_distance <- max(distances)  
  #Compute a similarity score based on the relative distance  
  similarity_score <- (1 - (min_distance / max_distance)) * 100
  
  # Store match details in the matches dataframe
  matches <- rbind(matches, data.frame(
    Current_Player = all_data$Player[all_data$Source == "Current"][i],
    Historical_Player = all_data$Player[all_data$Source == "Historical"][min_idx],
    Historical_Year = historical_data$Year[min_idx],
    Distance = distances[min_idx],
    Similarity_Score = round(similarity_score, 2)
  ))  
}  

# Sort matches by increasing distance (best matches first)
matches <- matches %>% arrange(Distance)  

# Print the sorted matches
print(matches)  

#Save as CSV
write.csv(matches, "historical_comps.csv", row.names = FALSE)
