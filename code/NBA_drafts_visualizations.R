# Loading in packages
library(readr)
library(dplyr)
library(tidyverse)
library(mgcv)
library(qgam)
library(caret)
library(ggplot2)
library(ggrepel)
library(forcats)
library(viridisLite)

# Load in CSV data

team_summary <- read_csv("team_summary_2004_2024.csv")
drafts_w <- read_csv("drafts_WdEPV_2004_2024.csv")


# ANALYSIS 
# Team bar charts                                                         

# 1.  Total weighted surplus (late-pick bonus applied)
ggplot(team_summary,
       aes(x = reorder(Tm, Total_WdEPV), y = Total_WdEPV, fill = Tm)) +
  geom_col(show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  scale_fill_manual(values = team_pal, guide = "none") +
  labs(title = "Total Weighted Draft Surplus (WdEPV) by Team, 2004-2024",
       x = "Team", y = "Σ WdEPV") +
  theme_minimal()

# 2.  Average weighted surplus per pick
ggplot(team_summary,
       aes(x = reorder(Tm, Avg_WdEPV), y = Avg_WdEPV, fill = Tm)) +
  geom_col(show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  scale_fill_manual(values = team_pal, guide = "none") +
  labs(title = "Average Weighted Surplus per Pick (WdEPV), 2004-2024",
       x = "Team", y = "Avg WdEPV per Pick") +
  theme_minimal()

# 3.  Raw surplus vs pick expectation (dEPV) – totals
ggplot(team_summary,
       aes(x = reorder(Tm, Total_dEPV), y = Total_dEPV, fill = Tm)) +
  geom_col(show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  scale_fill_manual(values = team_pal, guide = "none") +
  labs(title = "Total Draft Surplus (dEPV) by Team, 2004-2024",
       x = "Team", y = "Σ dEPV") +
  theme_minimal()

# 4.  Average surplus per pick
ggplot(team_summary,
       aes(x = reorder(Tm, Avg_dEPV), y = Avg_dEPV, fill = Tm)) +
  geom_col(show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  scale_fill_manual(values = team_pal, guide = "none") +
  labs(title = "Average  Surplus per Pick (dEPV), 2004-2024",
       x = "Team", y = "Avg dEPV per Pick") +
  theme_minimal()

# 5. Player-level scatter : dEPV  vs  WdEPV                                   
df_scatter <- drafts_w |> drop_na(dEPV, WdEPV)

## thresholds for auto-labeling extremes (top 1.75 %)
x_thr <- quantile(df_scatter$dEPV,  0.9825)
y_thr <- quantile(df_scatter$WdEPV, 0.9825)

ggplot(df_scatter,
       aes(x = dEPV, y = WdEPV, colour = Pk)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_point(size = 2, alpha = 0.7) +
  ## label only the most extreme over-achievers
  geom_text_repel(
    data = filter(df_scatter, dEPV >= x_thr | WdEPV >= y_thr),
    aes(label = paste0(Player, " (", Year, ")")),
    max.overlaps = Inf,
    segment.alpha = 0.5,
    size = 3
  ) +
  scale_colour_viridis_c(option = "D", guide = "none") +
  labs(title = "Draft Over-Achievers, 2004-2024",
       subtitle = "Raw surplus (dEPV) vs Late-Pick-Weighted surplus (WdEPV)",
       x = "dEPV  (surplus vs Expected Pick Value)",
       y = "WdEPV (dEPV × late-pick bonus)") +
  theme_minimal()


# 6. total VOADP by franchise
ggplot(team_summary,
       aes(x = reorder(Tm, Total_VOADP),
           y = Total_VOADP, fill = Tm)) +
  geom_col(show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  scale_fill_manual(values = team_pal, guide = "none") +
  labs(title = "Total VOADP by Franchise (2004-2024 Drafts)",
       x = "Team", y = "Σ VOADP") +
  theme_minimal()

# 7. average VOADP per pick by franchise
ggplot(team_summary,
       aes(x = reorder(Tm, Avg_VOADP),
           y = Avg_VOADP, fill = Tm)) +
  geom_col(show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  scale_fill_manual(values = team_pal, guide = "none") +
  labs(title = "Average VOADP per Pick by Franchise (2004-2024 Drafts)",
       x = "Team", y = "Avg VOADP per Pick") +
  theme_minimal()

# 8. Break Even Rate (percentage of picks above 0 VOADP) per team
ggplot(team_summary,
       aes(x = reorder(Tm, BreakEvenRate),
           y = BreakEvenRate, fill = Tm)) +
  geom_col(show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  scale_fill_manual(values = team_pal, guide = "none") +
  labs(title = "Break Even Rate by Franchise (2004-2024 Drafts)",
       x = "Team", y = "Break Even Rate") +
  theme_minimal()

# 9. Year-by-Year Cumulative WdEPV (shows drafting trajectory instead of one aggregate number)

## Calculate cumulative WdEPV by team and year
cum_surplus <- drafts_w %>% 
  group_by(Tm, Year) %>% 
  summarise(Year_WdEPV = sum(WdEPV, na.rm = TRUE), .groups = "drop") %>% 
  arrange(Tm, Year) %>% 
  group_by(Tm) %>% 
  mutate(Cum_WdEVP = cumsum(Year_WdEPV))

ggplot(cum_surplus,
       aes(x = Year, y = Cum_WdEVP, colour = Tm)) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = team_pal, guide = "none") +
  labs(title = "Cumulative Draft Surplus (WdEPV) by Franchise, 2004-2024",
       subtitle = "Late-pick bonus applied; upward slope = value added that year",
       x = "Draft Year", y = "Cumulative WdEPV") +
  theme_minimal()


# 10. Avg WdEPV vs Break-Even-Rate (bubble = pick volume)
ggplot(team_summary,
       aes(x = BreakEvenRate,
           y = Avg_WdEPV,
           size = Picks,
           label = Tm)) +
  geom_point(alpha = .8, colour = "#1f78b4") +
  geom_text(vjust = -1, size = 3) +
  scale_size(range = c(3,10), guide = "none") +
  geom_vline(xintercept = mean(team_summary$BreakEvenRate), linetype = "dotted") +
  geom_hline(yintercept = mean(team_summary$Avg_WdEPV),    linetype = "dotted") +
  labs(title = "Draft Performance Quadrants, 2004-2024",
       subtitle = "X = % picks above zero,  Y = Avg late-pick-weighted surplus",
       x = "Break-Even Rate",
       y = "Average WdEPV per Pick") +
  theme_minimal()


# 11. Heat-map by Team × Decade (mean z_dEPV_decade)                    
team_decade_z <- drafts_w %>% 
  group_by(Tm, Decade) %>% 
  summarise(Avg_z_dEPV_dec = mean(z_dEPV_decade, na.rm = TRUE), .groups = "drop")

ggplot(team_decade_z,
       aes(x = Decade,
           y = fct_reorder(Tm, -Avg_z_dEPV_dec),
           fill = Avg_z_dEPV_dec)) +
  geom_tile() +
  scale_fill_gradient2(low  = "#9e0142",
                       mid  = "white",
                       high = "#1a9850",
                       midpoint = 0,
                       name = "z-score") +
  labs(title = "Draft Surplus z-Score by Team and Decade",
       subtitle = "Positive = above-tier average for that decade",
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 7))

# 12. Average Lottery Surplus by Team 
lottery_avg <- drafts_w %>% 
  filter(PickTier == "Lottery") %>% 
  group_by(Tm) %>% 
  summarise(
    Picks        = n(),
    Avg_z_lotto  = mean(z_dEPV, na.rm = TRUE),
    .groups      = "drop"
  )

ggplot(lottery_avg,
       aes(x = reorder(Tm, Avg_z_lotto),
           y = Avg_z_lotto,
           fill = Tm)) +
  geom_col(show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = team_pal, guide = "none") +
  coord_flip() +
  labs(title = "Average Surplus in the Lottery (z-Score, 2004-2024)",
       subtitle = "z_dEPV > 0 = above-tier average value",
       x = "Team", y = "Mean z-Score (Lottery Picks Only)") +
  theme_minimal()

# 13.  Bar chart  ––  Average first-round z-score surplus per franchise                 
first_round_summary <- drafts_w %>% 
  filter(Pk <= 30) %>%                    
  group_by(Tm) %>% 
  summarise(
    Picks_FR = n(),
    Avg_FR_z = mean(z_dEPV, na.rm = TRUE),  
    .groups   = "drop"
  ) %>% 
  arrange(desc(Avg_FR_z))

ggplot(first_round_summary,
       aes(x = reorder(Tm, Avg_FR_z), y = Avg_FR_z, fill = Tm)) +
  geom_col(show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  scale_fill_manual(values = team_pal, guide = "none") +
  labs(title = "Average First-Round Surplus (z-score) by Franchise, 2004-2024",
       subtitle = "z_dEPV > 0  ⇒  above-tier",
       x = "Team", y = "Mean z-Score (First-Round Picks)") +
  theme_minimal()
