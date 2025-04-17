# Load required packages for data manipulation, plotting, and radar charts
library(dplyr)      # Data manipulation
library(ggplot2)    # Plotting (if needed for other visuals)
library(scales)     # Scaling functions
library(devtools)   # Development tools
library(ggradar)    # Radar charts using ggplot2 (if desired)
library(fmsb)       # Radar chart function 'radarchart'

#### Example to test out a radar chart with Kobe Bryant and LeBron James data

# Filter out the data for Kobe Bryant (03-04) and LeBron James (13-14) from the training dataset
# Then, select specific columns (stats) for the radar chart.
kobe_scaled <- train_data |> 
  filter((Player == "Kobe Bryant" & Season == "03-04") | (Player == "LeBron James" & Season == "13-14")) |> 
  select(height, weight, MP, PTS, FG., AST, X3P, TRB, STL, BLK)

# Prepare the data for the radar chart.
# The first two rows are set as the maximum and minimum values for scaling purposes.
# Then, the filtered data (kobe_scaled) is added as the actual data points.
radar_ready <- rbind(
  rep(1, ncol(kobe_scaled)),  # Maximum values for each stat (assumes normalized data)
  rep(0, ncol(kobe_scaled)),  # Minimum values for each stat
  kobe_scaled                 # Actual data points for Kobe and LeBron
)

# Plot the radar chart using the prepared data.
radarchart(radar_ready,
           cglty = 3,         # Set grid line type (dashed)
           cglcol = "gray",   # Set grid line color
           cglwd = 1,         # Set grid line width
           pcol = c(4, "salmon"),  # Line colors for the two players (4 corresponds to blue)
           plwd = 2,          # Line width for the plotted data lines
           plty = 1,          # Line type (solid)
           pfcol = c(
             adjustcolor("skyblue", alpha.f = 0.5),  # Fill color for one area with transparency
             adjustcolor("salmon", alpha.f = 0.5)      # Fill color for the other area with transparency
           ))       

# Add a legend to the radar chart with labels for Kobe and LeBron.
legend(x = 1.10, y = 1.10,                   # Position of the legend (x and y coordinates)
       legend = c("Kobe 03-04", "LeBron 13-14"),  # Labels for the players and seasons
       col = c("skyblue", "salmon"),             # Colors matching the plotted lines/areas
       lty = 1,                # Line type (solid)
       lwd = 2,                # Line width
       bty = "n")              # No box around the legend

################################################################################
# A'ja Wilson and Anthony Davis Comparison
################################################################################

# Read in the matched data between NBA and WNBA from a CSV file,
# then select only the relevant columns and join with the normalized men's dataset.
matched_nba_wnba_normalized <- read.csv("NBA_WNBA_Match_Test1.csv") |> 
  select(-X) |> 
  select(Player_Source, Match_Num, Score, Match_Player, Season) |> 
  left_join(mens_normalized, by = c("Match_Player" = "Player", "Season" = "Season"))

# For A'ja Wilson and Anthony Davis, filter to keep only the first match for A'ja Wilson.
# Then, select the key stats for the radar chart.
aja_ad <- matched_nba_wnba_normalized  |> 
  filter((Player_Source == "A'ja Wilson") & (Match_Num == "1")) |> 
  select(height, weight, MP, PTS, FG., AST, X3P, TRB, STL, BLK)

# Prepare the radar chart data for the A'ja Wilson and Anthony Davis comparison.
# The data is constructed by binding together:
#   1. Maximum values (row of 1's)
#   2. Minimum values (row of 0's)
#   3. Data for A'ja Wilson (sourced from the normalized WNBA dataset)
#   4. Data for Anthony Davis (obtained from the matched data)
aja_ad_ready <- rbind(
  rep(1, ncol(aja_ad)),
  rep(0, ncol(aja_ad)), 
  womens_normalized |> 
    filter((Player == "A'ja Wilson")) |> 
    select(height, weight, MP, PTS, FG., AST, X3P, TRB, STL, BLK),
  aja_ad
)

# Plot the radar chart for A'ja Wilson vs. Anthony Davis.
radarchart(aja_ad_ready,
           cglty = 3,          # Grid line type (dashed)
           cglcol = "gray",    # Grid line color
           cglwd = 1,          # Grid line width
           pcol = c(4, "salmon"),  # Colors for the two plotted lines
           plwd = 2,           # Line width
           plty = 1,           # Line type (solid)
           pfcol = c(
             adjustcolor("skyblue", alpha.f = 0.5),  # Fill color with transparency for one area
             adjustcolor("salmon", alpha.f = 0.5)      # Fill color with transparency for the other area
           ),
           vlabels = c("Height", "Weight", "MP", "PTS", "FG%", "AST", "3P", "TRB", "STL", "BLK"))  # Custom variable labels

# Add a legend for the A'ja Wilson vs. Anthony Davis radar chart.
legend(x = 1.10, y = 1.10,
       legend = c("A'ja Wilson 23-24", "Anthony Davis 16-17"),
       col = c("skyblue", "salmon"),
       lty = 1,
       lwd = 2,
       bty = "n")  

# Add a main title for the chart.
title(main = "Comparison of A'ja Wilson and Anthony Davis", cex.main = 1.2)

# Save the current plot as a high-resolution PNG file in the specified folder.
dev.copy(png, filename = "~/Documents/CDJ/CDJ Semester 2 Project/CDJ Basketball Comparison Spider Charts/Aja_Wilson_Comp.png", width = 3000, height = 1800, res = 300)
dev.off()

################################################################################
# Dearica Hamby and LeBron James Comparison
################################################################################

# Set the selected player variable to "Dearica Hamby"
selected_player <- "Dearica Hamby"

# Read and join the matched data for the selected player and the normalized men's dataset.
matched_nba_wnba_normalized <- read.csv("NBA_WNBA_Match_Test1.csv") |> 
  select(-X) |> 
  select(Player_Source, Match_Num, Score, Match_Player, Season) |> 
  left_join(mens_normalized, by = c("Match_Player" = "Player", "Season" = "Season"))

# Filter the matched data to only include the first match for the selected player,
# then select the key stats for the radar chart.
aja_ad <- matched_nba_wnba_normalized  |> 
  filter((Player_Source == selected_player) & (Match_Num == "1")) |> 
  select(height, weight, MP, PTS, FG., AST, X3P, TRB, STL, BLK)

# Prepare the radar chart data:
#   1. Row of max values (1's)
#   2. Row of min values (0's)
#   3. Data from the women's normalized dataset for the selected player
#   4. The matched data (from the NBA normalized dataset)
aja_ad_ready <- rbind(
  rep(1, ncol(aja_ad)),
  rep(0, ncol(aja_ad)), 
  womens_normalized |> 
    filter((Player == selected_player)) |> 
    select(height, weight, MP, PTS, FG., AST, X3P, TRB, STL, BLK),
  aja_ad
)

# Plot the radar chart for Dearica Hamby vs. LeBron James.
radarchart(aja_ad_ready,
           cglty = 3,
           cglcol = "gray",
           cglwd = 1,
           pcol = c(4, "salmon"),
           plwd = 2,
           plty = 1,
           pfcol = c(
             adjustcolor("skyblue", alpha.f = 0.5),
             adjustcolor("salmon", alpha.f = 0.5)
           ),
           vlabels = c("Height", "Weight", "MP", "PTS", "FG%", "AST", "3P", "TRB", "STL", "BLK"))

# Create legend labels dynamically using paste() for the selected player.
legend(x = 1.10, y = 1.10,
       legend = c(paste(selected_player, " 23-24"), "LeBron James 13-14"),
       col = c("skyblue", "salmon"),
       lty = 1,
       lwd = 2,
       bty = "n")  

# Add a title for the chart.
title(main = paste("Comparison of ", selected_player, "and LeBron James"), cex.main = 1.2)

# Save the radar chart as a PNG file.
dev.copy(png, filename = "~/Documents/CDJ/CDJ Semester 2 Project/CDJ Basketball Comparison Spider Charts/LeBron_Comp.png", width = 3000, height = 1800, res = 300)
dev.off()

################################################################################
# Caitlin Clark and James Harden Comparison
################################################################################

# Set the selected player and the matched player variables.
selected_player <- "Caitlin Clark"
matched_player <- "James Harden"

# Read and join the matched data for Caitlin Clark from the NBA/WNBA CSV file with the normalized men's data.
matched_nba_wnba_normalized <- read.csv("NBA_WNBA_Match_Test1.csv") |> 
  select(-X) |> 
  select(Player_Source, Match_Num, Score, Match_Player, Season) |> 
  left_join(mens_normalized, by = c("Match_Player" = "Player", "Season" = "Season"))

# Filter the data to include only the first match for the selected player and select relevant stats.
aja_ad <- matched_nba_wnba_normalized  |> 
  filter((Player_Source == selected_player) & (Match_Num == "1")) |> 
  select(height, weight, MP, PTS, FG., AST, X3P, TRB, STL, BLK)

# Prepare the radar chart data by combining:
#   - Maximum values
#   - Minimum values
#   - Data for the selected player from the women's normalized dataset
#   - The matched data for Caitlin Clark
aja_ad_ready <- rbind(
  rep(1, ncol(aja_ad)),
  rep(0, ncol(aja_ad)), 
  womens_normalized |> 
    filter((Player == selected_player)) |> 
    select(height, weight, MP, PTS, FG., AST, X3P, TRB, STL, BLK),
  aja_ad
)

# Plot the radar chart for Caitlin Clark vs. James Harden.
radarchart(aja_ad_ready,
           cglty = 3,
           cglcol = "gray",
           cglwd = 1,
           pcol = c(4, "salmon"),
           plwd = 2,
           plty = 1,
           pfcol = c(
             adjustcolor("skyblue", alpha.f = 0.5),
             adjustcolor("salmon", alpha.f = 0.5)
           ),
           vlabels = c("Height", "Weight", "MP", "PTS", "FG%", "AST", "3P", "TRB", "STL", "BLK"))

# Create a legend for the comparison, using paste() to form the label for Caitlin Clark.
legend(x = 1.10, y = 1.10,
       legend = c(paste(selected_player, " 23-24"), paste(matched_player, " 16-17")),
       col = c("skyblue", "salmon"),
       lty = 1,
       lwd = 2,
       bty = "n")  

# Add a title for the chart.
title(main = paste("Comparison of ", selected_player, "and", matched_player), cex.main = 1.2)

# Save the radar chart as a PNG file.
dev.copy(png, filename = "~/Documents/CDJ/CDJ Semester 2 Project/CDJ Basketball Comparison Spider Charts/CaitlinClark_Comp.png", width = 3000, height = 1800, res = 300)
dev.off()

################################################################################
# Angel Reese and Three Matched Comparisons
################################################################################

# Set the selected player and define multiple matched players.
selected_player <- "Angel Reese"
matched_player <- c("Carlos Boozer", "Greg Monroe", "Domantas Sabonis")

# Read and join the matched data using the CSV file.
matched_nba_wnba_normalized <- read.csv("NBA_WNBA_Match_Test1.csv") |> 
  select(-X) |> 
  select(Player_Source, Match_Num, Score, Match_Player, Season) |> 
  left_join(mens_normalized, by = c("Match_Player" = "Player", "Season" = "Season"))

# Filter the data for Angel Reese (all matches) and select the relevant stats.
aja_ad <- matched_nba_wnba_normalized  |> 
  filter((Player_Source == selected_player)) |> 
  select(height, weight, MP, PTS, FG., AST, X3P, TRB, STL, BLK)

# Prepare the data for the radar chart:
#   - Maximum values row
#   - Minimum values row
#   - Data for Angel Reese from the women's normalized dataset
#   - The matched data from the NBA normalized dataset
aja_ad_ready <- rbind(
  rep(1, ncol(aja_ad)),
  rep(0, ncol(aja_ad)), 
  womens_normalized |> 
    filter((Player == selected_player)) |> 
    select(height, weight, MP, PTS, FG., AST, X3P, TRB, STL, BLK),
  aja_ad
)

# Plot the radar chart for Angel Reese compared to three matched players.
radarchart(aja_ad_ready,
           cglty = 3,
           cglcol = "gray",
           cglwd = 1,
           pcol = c("skyblue", "salmon", "purple", "gold"),   # Four lines for Angel Reese and the three comparisons
           plwd = 2,
           plty = 1,
           pfcol = c(
             adjustcolor("skyblue", alpha.f = 0.5),
             adjustcolor("salmon", alpha.f = 0.5),
             adjustcolor("purple", alpha.f = 0.5),
             adjustcolor("gold", alpha.f = 0.5)
           ),
           vlabels = c("Height", "Weight", "MP", "PTS", "FG%", "AST", "3P", "TRB", "STL", "BLK"))

# Add a legend for the Angel Reese comparisons, including the match details in the labels.
legend(x = 1.10, y = 1.10,
       legend = c(paste(selected_player, "23-24"),
                  paste(matched_player[1], "03-04", "(45.97)"),
                  paste(matched_player[2], "14-15", "(40.65)"),
                  paste(matched_player[3], "19-20", "(40.63)")),
       col = c("skyblue", "salmon", "purple", "gold"),
       lty = 1,
       lwd = 2,
       bty = "n",
       cex = 0.8)  # Smaller legend text

# Add a title with a subtitle for context.
title(main = paste("Comparison of ", selected_player, ", ", matched_player[1], ", ", matched_player[2], " and ", matched_player[3], sep = ""),
      cex.main = 1.2,
      sub = "The Three Top Comparisons of Angel Reese")

# Save the radar chart as a PNG file.
dev.copy(png, filename = "~/Documents/CDJ/CDJ Semester 2 Project/CDJ Basketball Comparison Spider Charts/Angel_Comp.png", width = 3400, height = 1800, res = 300)
dev.off()

################################################################################
# Multiple Matches for 16-17 Stephen Curry Comparison
################################################################################

# Set the selected players and the matched player.
selected_player <- c("Arike Ogunbowale", "Kelsey Plum", "Sabrina Ionescu")
matched_player <- "Stephen Curry"

# Read and join the matched data.
matched_nba_wnba_normalized <- read.csv("NBA_WNBA_Match_Test1.csv") |> 
  select(-X) |> 
  select(Player_Source, Match_Num, Score, Match_Player, Season) |> 
  left_join(mens_normalized, by = c("Match_Player" = "Player", "Season" = "Season"))

# Filter the data for the specified conditions: selected players matched to Stephen Curry in 16-17 season.
aja_ad <- matched_nba_wnba_normalized  |> 
  filter((Player_Source %in% selected_player) & (Match_Player == matched_player & Season == "16-17")) |> 
  select(height, weight, MP, PTS, FG., AST, X3P, TRB, STL, BLK) |> 
  unique()

# Prepare the radar chart data:
#   - Maximum values row
#   - Minimum values row
#   - Data for the selected players from the women's normalized dataset
#   - The matched data for Stephen Curry from the NBA normalized dataset
aja_ad_ready <- rbind(
  rep(1, ncol(aja_ad)),
  rep(0, ncol(aja_ad)), 
  womens_normalized |> 
    filter((Player %in% selected_player)) |> 
    select(height, weight, MP, PTS, FG., AST, X3P, TRB, STL, BLK),
  aja_ad
)

# Plot the radar chart for the 2016-17 Stephen Curry match comparisons.
radarchart(aja_ad_ready,
           cglty = 3,
           cglcol = "gray",
           cglwd = 1,
           pcol = c("gold", "salmon", "purple", "skyblue"),   # Colors for the matched lines
           plwd = 2,
           plty = 1,
           pfcol = c(
             adjustcolor("gold", alpha.f = 0.5),
             adjustcolor("salmon", alpha.f = 0.5),
             adjustcolor("purple", alpha.f = 0.5),
             adjustcolor("skyblue", alpha.f = 0.5)
           ),
           vlabels = c("Height", "Weight", "MP", "PTS", "FG%", "AST", "3P", "TRB", "STL", "BLK"))

# Create a legend that shows the labels for Stephen Curry and the selected players,
# including match season and a score in parentheses.
legend(x = 1.10, y = 1.10,
       legend = c(
         paste(matched_player, "16-17"),
         paste(selected_player[3], "23-24", "(58.97)"),
         paste(selected_player[2], "23-24", "(52.16)"),
         paste(selected_player[1], "23-24", "(29.40)")
       ),
       col = c("skyblue", "gold", "purple", "salmon"),
       lty = 1,
       lwd = 2,
       bty = "n",
       cex = 0.8)  # Smaller legend text

# Add a title and subtitle for the chart.
title(main = paste("Comparison of ", matched_player, ", ", selected_player[1], ", ", selected_player[2], " and ", selected_player[3], sep = ""),
      cex.main = 1.2,
      sub = "The Many Matches of 2016-17 Steph")

# Save the radar chart as a PNG file.
dev.copy(png, filename = "~/Documents/CDJ/CDJ Semester 2 Project/CDJ Basketball Comparison Spider Charts/Steph_Comp.png", width = 3400, height = 1800, res = 300)
dev.off()