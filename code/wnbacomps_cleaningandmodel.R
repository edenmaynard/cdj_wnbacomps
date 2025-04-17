# Load required libraries
library(fastDummies)   # For creating dummy (one-hot encoded) variables
library(purrr)         # For functional programming helpers
library(FNN)           # For k-nearest neighbors functions

# Read in the men's data from CSV and start cleaning it up.
mens <- read.csv("~/Documents/CDJ/CDJ Semester 2 Project/EverythingCombinedMens.csv") |> 
  select(-X) |>  
  mutate(Pos = case_when(   # Simplify position labels: PF and SF become "F", PG and SG become "G"
    Pos == "PF" ~ "F",
    Pos == "SF" ~ "F",
    Pos == "PG" ~ "G",
    Pos == "SG" ~ "G",
    TRUE ~ Pos
  )) |> 
  dummy_cols(select_columns = c("Pos"), remove_first_dummy = F) %>%  # Create dummy variables for position
  dplyr::select(-Pos) |>   # Remove original Pos column after dummy encoding
  unique() |> 
  separate(height, into = c("Feet", "Inches"), sep = "-", remove = FALSE) %>%  # Split height into feet and inches
  mutate(
    Feet = as.numeric(Feet),      # Convert feet to numeric
    Inches = as.numeric(Inches),  # Convert inches to numeric
    height = (Feet * 12) + Inches,  # Convert height to total inches
    weight = as.numeric(weight)     # Ensure weight is numeric
  ) |> 
  select(-Feet, -Inches)|>  # Remove temporary feet and inches columns
  summarize(   # Select and rename the variables to be used later
    Player,
    Team,
    G,
    GS,
    Age,
    height, weight, 
    College,
    MP,
    PTS,
    FG,
    FGA,
    `FG.`,
    X3P,
    X3PA,
    X2P,
    X2PA,
    `FT.`,
    TRB,
    STL,
    BLK,
    TOV,
    AST,
    Pos_C,
    Pos_F,
    Pos_G,
    Season
  )

# Create a normalization function to scale numeric data to the 0-1 range.
normalize <- function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

# Apply normalization to the men's numeric data
mens_normalized <- mens |> 
  mutate(
    across(where(is.numeric) & !all_of(c("Team", "College", "Season")), normalize)
  ) |> 
  filter(!is.na(height)) |>  # Remove rows missing height
  filter(!is.na(weight))      # Remove rows missing weight

# Read in the women's data from CSV and clean it up similarly
women <- read.csv("~/Documents/CDJ/CDJ Semester 2 Project/wnba_combined.csv") |> 
  mutate(Pos = case_when(   # Clean up position labels same as before
    Pos == "F-C" ~ "F",
    Pos == "C-F" ~ "C",
    Pos == "G-F" ~ "G",
    Pos == "F-G" ~ "F",
    TRUE ~ Pos
  ))  |> 
  filter(!is.na(Pos)) |>  # Filter out any rows with missing position data
  dummy_cols(select_columns = c("Pos"), remove_first_dummy = F) %>%
  dplyr::select(-Pos) |>  # Remove original Pos column after dummy encoding
  unique() |> 
  separate(height, into = c("Feet", "Inches"), sep = "-", remove = FALSE) %>%  # Separate height into feet and inches
  mutate(
    Feet = as.numeric(Feet),      # Convert feet to numeric
    Inches = as.numeric(Inches),  # Convert inches to numeric
    height = (Feet * 12) + Inches,  # Calculate height in inches
    weight = as.numeric(weight)     # Ensure weight is numeric
  ) |> 
  select(-Feet, -Inches)|> 
  summarize(   # Select and rename columns to match the men's data structure
    Player = player.name,
    Team = team,
    G = GP,
    GS,
    Age = age,
    height, weight, 
    College = school,
    MP = `MP...5`,
    PTS,
    FG,
    FGA,
    `FG.`,
    X3P,
    X3PA,
    X2P,
    X2PA,
    `FT.`,
    TRB,
    STL,
    BLK,
    TOV,
    AST,
    Pos_C,
    Pos_F,
    Pos_G,
    Season
  )

# Normalize the women's data in a similar fashion to the men's data.
womens_normalized <- women |> 
  mutate(
    across(where(is.numeric) & !all_of(c("Team", "College")), normalize)
  ) |> 
  filter(!is.na(height)) |>  # Filter out rows missing height
  filter(!is.na(weight))      # Filter out rows missing weight

# (Optional) Write the normalized datasets to CSV files
#write.csv(mens_normalized, "MenNormalized.csv")
#write.csv(womens_normalized, "WomenNormalized.csv")

str(mens_normalized)
str(womens_normalized)

# Set seed for reproducibility in our nearest neighbors analysis
set.seed(123)

# Remove NA values.
train_data <- na.omit(mens_normalized)
test_data <- na.omit(womens_normalized)

# Define weights for different variables 
weights <- c(
  height = 2, 
  weight = 2, 
  Pos_C = 1.5,
  Pos_F = 1.5,
  Pos_G = 1.5,
  FG. = 3,
  X3P = 3,
  STL = 2,
  BLK = 2,
  TRB = 2
)

# Set the number of neighbors 
k <- 5

# Run k-nearest neighbors search using the numeric columns (selecting columns by index here).
# We're comparing the test data (women's) against the train data (men's) based on selected features.
knn_result <- get.knnx(
  data = train_data[c(3:7, 9:(ncol(train_data)-1))], 
  query = test_data[c(3:7, 9:(ncol(test_data)-1))], 
  k = k
)

# Convert the kNN output into data frames.
top_matches <- as.data.frame(knn_result$nn.index)
top_distances <- as.data.frame(knn_result$nn.dist)

# Convert the distances into similarity scores (closer distances mean higher similarity)
similarity_scores <- 1 - (top_distances / max(top_distances))
similarity_scores <- round(similarity_scores * 100, 2)  # Express as percentage scores

# Create a table with the top match information for each test case.
top_matches_named <- tibble(
  Player_Source = test_data$Player,
  Match_1 = map_chr(top_matches$V1, ~ train_data$Player[.x]),
  Match_1_year = map_chr(top_matches$V1, ~ train_data$Season[.x]),
  Score_1 = map_dbl(top_distances$V1, ~ similarity_scores$V1[which(top_distances$V1 == .x)]),
  Match_2 = map_chr(top_matches$V2, ~ train_data$Player[.x]),
  Match_2_year = map_chr(top_matches$V2, ~ train_data$Season[.x]),
  Score_2 = map_dbl(top_distances$V2, ~ similarity_scores$V2[which(top_distances$V2 == .x)]),
  Match_3 = map_chr(top_matches$V3, ~ train_data$Player[.x]),
  Match_3_year = map_chr(top_matches$V3, ~ train_data$Season[.x]),
  Score_3 = map_dbl(top_distances$V3, ~ similarity_scores$V3[which(top_distances$V3 == .x)])
) |> 
  # Convert the score and year columns to character format (helps with pivoting)
  mutate(across(starts_with("Score_"), as.character),
         across(starts_with("Match_") & ends_with("_year"), as.character)) |> 
  # Reshape the data to have one row per match using pivot_longer and pivot_wider
  pivot_longer(
    cols = matches("Match_\\d+|Match_\\d+_year|Score_\\d+"),
    names_to = c("Type", "Match_Num"), 
    names_pattern = "(Match|Match_year|Score)_(\\d+)",
    values_drop_na = TRUE  
  ) |>
  pivot_wider(
    names_from = Type,  # Spread out the Match, Match_year, and Score columns
    values_from = value,
    values_fn = list
  ) %>%
  mutate(
    # Extract the first element from lists into new columns
    Match2 = map_chr(Match, ~ .x[1]),  
    Match_year = map_chr(Match, ~ .x[2]),  
    Score = as.numeric(Score)  # Convert Score back to numeric
  ) |> 
  select(-Match)

# Merge the top matches with the men's original dataset to add more details about the matches.
matched_data <- top_matches_named |> 
  left_join(mens, by = c("Match2" = "Player", "Match_year" = "Season")) |> 
  rename(Match_Player = Match2,
         Season = Match_year)

# Write the final matched data to a CSV file for further review or analysis.
write.csv(matched_data, "NBA_WNBA_Match_Test1.csv")