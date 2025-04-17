library(dplyr)
library(tidyverse)

# Read in the NBA stats CSV for each season from 2003-04 to 2023-24,
# and add a "Season" column to each data frame.
NBA_Stats_03_04 <- read_csv("NBA STATS/NBA Stats 03-04 - Sheet 1.csv") %>% mutate(Season = "03-04")
NBA_Stats_04_05 <- read_csv("NBA STATS/NBA Stats 04-05 - Sheet 1.csv") %>% mutate(Season = "04-05")
NBA_Stats_05_06 <- read_csv("NBA STATS/NBA Stats 05-06 - Sheet 1.csv") %>% mutate(Season = "05-06")
NBA_Stats_06_07 <- read_csv("NBA STATS/NBA Stats 06-07 - Sheet 1.csv") %>% mutate(Season = "06-07")
NBA_Stats_07_08 <- read_csv("NBA STATS/NBA Stats 07-08 - Sheet 1.csv") %>% mutate(Season = "07-08")
NBA_Stats_08_09 <- read_csv("NBA STATS/NBA Stats 08-09 - Sheet 1.csv") %>% mutate(Season = "08-09")
NBA_Stats_09_10 <- read_csv("NBA STATS/NBA Stats 09-10 - Sheet 1.csv") %>% mutate(Season = "09-10")
NBA_Stats_10_11 <- read_csv("NBA STATS/NBA Stats 10-11 - Sheet 1.csv") %>% mutate(Season = "10-11")
NBA_Stats_11_12 <- read_csv("NBA STATS/NBA Stats 11-12 - Sheet 1.csv") %>% mutate(Season = "11-12")
NBA_Stats_12_13 <- read_csv("NBA STATS/NBA Stats 12-13 - Sheet 1.csv") %>% mutate(Season = "12-13")
NBA_Stats_13_14 <- read_csv("NBA STATS/NBA Stats 13-14 - Sheet1.csv") %>% mutate(Season = "13-14")
NBA_Stats_14_15 <- read_csv("NBA STATS/NBA Stats 14-15 - Sheet1.csv") %>% mutate(Season = "14-15")
NBA_Stats_15_16 <- read_csv("NBA STATS/NBA Stats 15-16 - Sheet1.csv") %>% mutate(Season = "15-16")
NBA_Stats_16_17 <- read_csv("NBA STATS/NBA Stats 16-17 - Sheet1.csv") %>% mutate(Season = "16-17")
NBA_Stats_17_18 <- read_csv("NBA STATS/NBA Stats 17-18 - Sheet1.csv") %>% mutate(Season = "17-18")
NBA_Stats_18_19 <- read_csv("NBA STATS/NBA Stats 18-19 - Sheet1.csv") %>% mutate(Season = "18-19")
NBA_Stats_19_20 <- read_csv("NBA STATS/NBA Stats 19-20 - Sheet1.csv") %>% mutate(Season = "19-20")
NBA_Stats_20_21 <- read_csv("NBA STATS/NBA Stats 20-21 - Sheet1.csv") %>% mutate(Season = "20-21")
NBA_Stats_21_22 <- read_csv("NBA STATS/NBA Stats 21-22 - Sheet1.csv") %>% mutate(Season = "21-22")
NBA_Stats_22_23 <- read_csv("NBA STATS/NBA Stats 22-23 - Sheet1.csv") %>% mutate(Season = "22-23")
NBA_Stats_23_24 <- read_csv("NBA STATS/NBA Stats 23-24.xlsx - Sheet1.csv") %>% mutate(Season = "23-24")

# Convert all relevant columns in the 16-17 season data frame to numeric, except for the player information columns.
NBA_Stats_16_17 <- NBA_Stats_16_17 |> mutate(across(-c(Player, Team, Pos, Season, Awards, `Player-additional`), as.numeric)) 

# Combine all the season data frames into one large data frame.
df_NBA_Stats_All_Seasons <- bind_rows(
  NBA_Stats_03_04, NBA_Stats_04_05, NBA_Stats_05_06, NBA_Stats_06_07,
  NBA_Stats_07_08, NBA_Stats_08_09, NBA_Stats_09_10, NBA_Stats_10_11,
  NBA_Stats_13_14, NBA_Stats_14_15, NBA_Stats_15_16, NBA_Stats_16_17,
  NBA_Stats_17_18, NBA_Stats_18_19, NBA_Stats_19_20, NBA_Stats_20_21,
  NBA_Stats_21_22, NBA_Stats_22_23, NBA_Stats_23_24
)

# Read in additional NBA player bio data for each season, adding a "Season" column.
nba_0304 <- read_csv("NBA PLAYER BIOS/nba0304.csv") %>% mutate(Season = "03-04")
nba_0405 <- read_csv("NBA PLAYER BIOS/nba0405.csv") %>% mutate(Season = "04-05")
nba_0506 <- read_csv("NBA PLAYER BIOS/nba0506.csv") %>% mutate(Season = "05-06")
nba_0607 <- read_csv("NBA PLAYER BIOS/nba0607.csv") %>% mutate(Season = "06-07")
nba_0708 <- read_csv("NBA PLAYER BIOS/nba0708.csv") %>% mutate(Season = "07-08")
nba_0809 <- read_csv("NBA PLAYER BIOS/nba0809.csv") %>% mutate(Season = "08-09")
nba_0910 <- read_csv("NBA PLAYER BIOS/nba0910.csv") %>% mutate(Season = "09-10")
nba_1011 <- read_csv("NBA PLAYER BIOS/nba1011.csv") %>% mutate(Season = "10-11")
nba_1112 <- read_csv("NBA PLAYER BIOS/nba1112.csv") %>% mutate(Season = "11-12")
nba_1213 <- read_csv("NBA PLAYER BIOS/nba1213.csv") %>% mutate(Season = "12-13")
nba_1314 <- read_csv("NBA PLAYER BIOS/nba1314.csv") %>% mutate(Season = "13-14")
nba_1415 <- read_csv("NBA PLAYER BIOS/nba1415.csv") %>% mutate(Season = "14-15")
nba_1516 <- read_csv("NBA PLAYER BIOS/nba1516.csv") %>% mutate(Season = "15-16")
nba_1617 <- read_csv("NBA PLAYER BIOS/nba1617.csv") %>% mutate(Season = "16-17")
nba_1718 <- read_csv("NBA PLAYER BIOS/nba1718.csv") %>% mutate(Season = "17-18")
nba_1819 <- read_csv("NBA PLAYER BIOS/nba1819.csv") %>% mutate(Season = "18-19")
nba_1920 <- read_csv("NBA PLAYER BIOS/nba1920.csv") %>% mutate(Season = "19-20")
nba_2021 <- read_csv("NBA PLAYER BIOS/nba2021.csv") %>% mutate(Season = "20-21")
nba_2122 <- read_csv("NBA PLAYER BIOS/nba2122.csv") %>% mutate(Season = "21-22")
nba_2223 <- read_csv("NBA PLAYER BIOS/nba2223.csv") %>% mutate(Season = "22-23")
nba_2324 <- read_csv("NBA PLAYER BIOS/nba2324.csv") %>% mutate(Season = "23-24")

# Combine all the NBA bio data into one data frame.
# The summarize step below extracts and renames certain columns,
# then groups by Player and Season to keep maximum values for height, weight, and age.
df_NBA_Stats_All_Seasons2 <- bind_rows(
  nba_0304, nba_0405, nba_0506, nba_0607, nba_0708, nba_0809, nba_0910, nba_1011,
  nba_1112, nba_1213, nba_1314, nba_1415, nba_1516, nba_1617, nba_1718, nba_1819,
  nba_1920, nba_2021, nba_2122, nba_2223, nba_2324
) |> 
  summarize(Player = Anchor_anchor__cSc3P,
            Age = `Anchor_anchor__cSc3P 3`,
            College = `Anchor_anchor__cSc3P 4`,
            height = `tablescraper-selected-row`,
            weight = `tablescraper-selected-row 2`,
            Season) |> 
  group_by(Player, Season) |> 
  mutate(weight = max(weight),
         Age = max(Age),
         height = max(height)) |> 
  unique()

# Merge the stats data with the bio data by Player and Season.
# Then, remove rows where NA's exist (using age) and clean up the data.
all_combined <- df_NBA_Stats_All_Seasons |> left_join(df_NBA_Stats_All_Seasons2, by = c("Season", "Player")) |> 
  rename(Age = Age.x) |> 
  select(-Age.y) |> 
  filter(!(is.na(Age))) |> 
  unique() |> 
  group_by(Player, Season) |> 
  mutate(count = n()) |> 
  mutate(keep = case_when(
    count == 5 & Team == "4TM" ~ 1,
    count == 4 & Team =="3TM" ~ 1,
    count == 3 & Team == "2TM" ~ 1,
    count == 1 ~ 1,
    TRUE ~ 0
  )) |> 
  unique() |> 
  filter(keep == 1) |> 
  select(-keep, -count) |> 
  mutate(College = ifelse(is.na(College), "None", College)) |> 
  select(-Rk)

# Write the final combined NBA data to a CSV file.
write.csv(all_combined, "EverythingCombinedMens.csv")

# WNBA

# Read in the WNBA stats for the 2023-24 season.
WNBA_Stats_23_24 <- read_csv("~WNBA/wnba_stats.csv") %>%
  mutate(Season = "23-24")

# Read in the WNBA bios for the 2023-24 season.
wnba_2324 <- read_csv("~WNBA/wnba_bios.csv") %>% 
  mutate(Season = "23-24")

# Merge the WNBA bios and stats together by matching player name, team, and season.
wnba_combined <- wnba_2324 |> left_join(WNBA_Stats_23_24, by = c("player-name" = "Player", "team" = "Team", "Season")) |> 
  mutate(PTS = PTS.x, AST = AST.x) |> 
  select(!c(PTS.x, PTS.y, AST.x, AST.y))

# Write the final combined WNBA data to a CSV file.
write.csv(wnba_combined, "wnba_combined.csv")



df_NBA_Stats_All_Seasons

WNBA_Stats_08_09 <- read_csv("ALL WNBA/wnba2008.csv") %>% mutate(Season = "08-09")
WNBA_Stats_09_10 <- read_csv("ALL WNBA/wnba2009.csv") %>% mutate(Season = "09-10")
WNBA_Stats_10_11 <- read_csv("ALL WNBA/wnba2010.csv") %>% mutate(Season = "10-11")
WNBA_Stats_11_12 <- read_csv("ALL WNBA/wnba2011.csv") %>% mutate(Season = "11-12")
WNBA_Stats_12_13 <- read_csv("ALL WNBA/wnba2012.csv") %>% mutate(Season = "12-13")
WNBA_Stats_13_14 <- read_csv("ALL WNBA/wnba2013.csv") %>% mutate(Season = "13-14")
WNBA_Stats_14_15 <- read_csv("ALL WNBA/wnba2014.csv") %>% mutate(Season = "14-15")
WNBA_Stats_15_16 <- read_csv("ALL WNBA/wnba2015.csv") %>% mutate(Season = "15-16")
WNBA_Stats_16_17 <- read_csv("ALL WNBA/wnba2016.csv") %>% mutate(Season = "16-17")
WNBA_Stats_17_18 <- read_csv("ALL WNBA/wnba2017.csv") %>% mutate(Season = "17-18")
WNBA_Stats_18_19 <- read_csv("ALL WNBA/wnba2018.csv") %>% mutate(Season = "18-19")
WNBA_Stats_19_20 <- read_csv("ALL WNBA/wnba2019.csv") %>% mutate(Season = "19-20")
WNBA_Stats_20_21 <- read_csv("ALL WNBA/wnba2020.csv") %>% mutate(Season = "20-21")
WNBA_Stats_21_22 <- read_csv("ALL WNBA/wnba2021.csv") %>% mutate(Season = "21-22")
WNBA_Stats_22_23 <- read_csv("ALL WNBA/wnba2022.csv") %>% mutate(Season = "22-23")
WNBA_Stats_23_24 <- read_csv("ALL WNBA/wnba2023.csv") %>% mutate(Season = "23-24")
WNBA_Stats_24_25 <- read_csv("ALL WNBA/wnba2024.csv") %>% mutate(Season = "24-25")



df_WNBA_Stats_All_Seasons <- bind_rows(
 WNBA_Stats_08_09, WNBA_Stats_09_10, WNBA_Stats_10_11,
  WNBA_Stats_13_14, WNBA_Stats_14_15, WNBA_Stats_15_16, WNBA_Stats_16_17,
  WNBA_Stats_17_18, WNBA_Stats_18_19, WNBA_Stats_19_20, WNBA_Stats_20_21,
  WNBA_Stats_21_22, WNBA_Stats_22_23, WNBA_Stats_23_24
)


LeBron <- df_NBA_Stats_All_Seasons |> filter(Player == "LeBron James")

Candace <- read.csv("Candace.csv") |> mutate(Player = "Candace Parker") |> 
  rename(`FG%` = FG.,
         `FT%` = FT.,)

LeBron_Candace <- bind_rows(LeBron, Candace) |> 
  group_by(Player) |> 
  mutate(Season_Number = seq(n()),
         `MP%` = ifelse(Player == "Candace Parker", MP/40, MP/48))

# FG Percentage
LeBron_Candace |> 
  ggplot(aes(x = Season_Number, y = `FG%`, group = Player, color = Player)) + 
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  theme_fivethirtyeight() +
  labs(
    title = "LeBron vs Candace FG%",
    x = "Season Number",
    y = "Field Goal %"
  ) + 
  scale_color_manual(
    values = c(
      "LeBron James" = "#702F8A",
      "Candace Parker" = "#FDB927"
    )
  ) +
  scale_y_continuous(limits = c(0.38, 0.57)) + 
  theme(
    axis.title = element_text(),  
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 25, color = "black")
  )

options(chromote.headless = "new")
ggsave("LeBron_CandaceFG.png")


# FT Percentage
LeBron_Candace |> 
  ggplot(aes(x = Season_Number, y = `FT%`, group = Player, color = Player)) + 
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  theme_fivethirtyeight() +
  labs(
    title = "LeBron vs Candace FT%",
    x = "Season Number",
    y = "Free Throw%"
  ) + 
  scale_color_manual(
    values = c(
      "LeBron James" = "#702F8A",
      "Candace Parker" = "#FDB927"
    )
  ) +
  theme(
    axis.title = element_text(),  
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 25, color = "black")
  )
ggsave("LeBron_CandaceFT.png")

# MP

LeBron_Candace |> 
  ggplot(aes(x = Season_Number, y = `MP%`, group = Player, color = Player)) + 
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  theme_fivethirtyeight() +
  labs(
    title = "LeBron vs Candace MP%",
    x = "Season",
    y = "Percentage of Minutes Played Per Game"
  ) + 
  scale_color_manual(
    values = c(
      "LeBron James" = "#702F8A",
      "Candace Parker" = "#FDB927"
    )
  ) +
  theme(
    axis.title = element_text(),  
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 25, color = "black")
  )
ggsave("LeBron_CandaceMP.png")



# TRB

LeBron_Candace |> 
  ggplot(aes(x = Season_Number, y = `TRB`, group = Player, color = Player)) + 
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  theme_fivethirtyeight() +
  labs(
    title = "LeBron vs Candace Total Rebounds Per Game",
    x = "Season",
    y = "Total Rebounds Per Game"
  ) + 
  scale_color_manual(
    values = c(
      "LeBron James" = "#702F8A",
      "Candace Parker" = "#FDB927"
    )
  ) +
  theme(
    axis.title = element_text(),  
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 25, color = "black")
  )
ggsave("LeBron_CandaceTRB.png")

  
# AST

LeBron_Candace |> 
  ggplot(aes(x = Season_Number, y = `AST`, group = Player, color = Player)) + 
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  theme_fivethirtyeight() +
  labs(
    title = "LeBron vs Candace Assists Per Game",
    x = "Season",
    y = "Assists Per Game"
  ) + 
  scale_color_manual(
    values = c(
      "LeBron James" = "#702F8A",
      "Candace Parker" = "#FDB927"
    )
  ) +
  theme(
    axis.title = element_text(),  
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 25, color = "black")
  )
ggsave("LeBron_CandaceAST.png")



# PTS

LeBron_Candace |> 
  ggplot(aes(x = Season_Number, y = `PTS`, group = Player, color = Player)) + 
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  theme_fivethirtyeight() +
  labs(
    title = "LeBron vs Candace Points Per Game",
    x = "Season",
    y = "Points Per Game"
  ) + 
  scale_color_manual(
    values = c(
      "LeBron James" = "#702F8A",
      "Candace Parker" = "#FDB927"
    )
  ) +
  theme(
    axis.title = element_text(),  
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 25, color = "black")
  )
ggsave("LeBron_CandacePTS.png")
