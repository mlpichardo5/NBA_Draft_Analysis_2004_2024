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


# Adding a team color pallet, from NBA.com 
team_pal <- c(
  ATL = "#E03A3E",  BOS = "#007A33",  BKN = "#000000",  CHO = "#00788C",
  CHI = "#CE1141",  CLE = "#6F263D",  DAL = "#00538C",  DEN = "#0E2240",
  DET = "#C8102E",  GSW = "#1D428A",  HOU = "#CE1141",  IND = "#002D62",
  LAC = "#C8102E",  LAL = "#552583",  MEM = "#5D76A9",  MIA = "#98002E",
  MIL = "#00471B",  MIN = "#0C2340",  NOP = "#0C2340",  NYK = "#006BB6",
  OKC = "#007AC1",  ORL = "#0077C0",  PHI = "#006BB6",  PHO = "#1D1160",
  POR = "#E03A3E",  SAC = "#5A2D81",  SAS = "#C4CED4",  TOR = "#CE1141",
  UTA = "#002B5C",  WAS = "#002B5C"
)


# Loading in dataset
drafts <- read_csv("2004-2024_drafts_with_year_and_updated_teams.csv")


# 1.  Compute Seasons played and VPS (VORP per Season)
drafts <- drafts |> 
  mutate(
    Seasons = pmax(1, 2024 - Year + 1),
    VPS     = VORP / Seasons       
  )


# 2.  VOADP on VPS (value vs average classmate)
drafts <- drafts |> 
  group_by(Year) |> 
  mutate(
    # Grouping by draft class, calculating the mean VPS for that class
    class_mean_vps = mean(VPS, na.rm = TRUE),
    # Value Over Average Draft Pick (VOADP) is the difference between player's VPS and class mean VPS
    VOADP = VPS -  class_mean_vps
  ) |> 
  ungroup()

## See which draft classes had the highest average VPS
drafts |>
  distinct(Year, class_mean_vps) |>      
  arrange(desc(class_mean_vps)) |>       
  slice_head(n = 20)                      



# 3.  EPV and dEPV on VPS  
## Fit the Expected Pick Value (EPV) curve (mean VPS per pick)
gam_epv <- gam(
  ## VPS ~ Pk specifies the model formula, where VPS is the response variable and Pk is the predictor variable (draft pick number).
  ## s(Pk, bs = "ps", k = 10, m = 2, xt = list(monotone = 1)) specifies that the predictor variable Pk should be modeled as a smooth function using a penalized spline basis (bs = "ps") with 10 basis functions (k = 10) and a second-order penalty (m = 2). 
  ### The xt argument specifies that the smooth function should be monotonic in the positive direction (monotone = 1).
  VPS ~ s(Pk, bs = "ps", k = 10, m = 2, xt = list(monotone = 1)),
  data = drafts
)
## The gam_epv result is an object that stores the smooth "mean VPS at eeach pick" curve, or the EPV curve


## Add EPV and the change in EPV (dEPV) to each player
drafts <- drafts %>% 
  mutate(
    # expected VPS at this pick
    EPV   = as.numeric(predict(gam_epv, newdata = .)),  
    # The difference between actual VPS and expected VPS at this pick
    dEPV  = VPS - EPV                                   
  )


## Plot the EPV curve (EPV should resemble the graph of 1/x in quadrant 1)
ggplot(drafts, aes(x = Pk, y = VPS)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_line(aes(y = EPV), color = "blue", size = 1) +
  labs(
    title    = "Expected Pick Value (EPV) Curve",
    subtitle = "GAM fit to mean VPS per pick",
    x        = "Draft Pick",
    y        = "VPS"
  ) +
  theme_minimal()


# Era-drift fix   –   one spline per decade  (factor-smooth basis)
drafts <- drafts %>% 
  mutate(Decade = factor(case_when(
    # Adding tags to decades
    Year < 2010  ~ "2000s",
    Year < 2020  ~ "2010s",
    TRUE         ~ "2020s"
  ), levels = c("2000s","2010s","2020s")))   # keep order tidy

gam_epv_decade <- gam(
  # Generalised additive model generating smooth curve of expected value for each decade
  VPS ~ s(Pk, Decade, bs = "fs", k = 10, m = 2),
  data   = drafts,
  method = "REML"
)

drafts <- drafts %>% 
  mutate(
    # feeds every row's Pk and Decade into the GAM model to get expected value within decade curve
    EVP_decade  = as.numeric(predict(gam_epv_decade, newdata = .)),
    # calculate the difference between actual VPS and expected VPS for that decade
    dEPV_decade = VPS - EVP_decade
  )
## Using a separate spline per decade could correct for long term shifts in draft strength and playing style




# 4.  Helper: add_WdEPV()    (pick-adjusted dEPV; rewards non-lottery picks)  
add_WdEPV <- function(df, floor_k = 14, power = 0.5) {
  stopifnot(all(c("Pk", "dEPV") %in% names(df)))
  # Ensure Pk is numeric and dEPV is numeric
  bonus <- ifelse(df$Pk <= floor_k, 
                  1,                          # lottery and earlier → no boost
                  (df$Pk / floor_k)^power)    # later picks -> bonus > 1
  # Apply the bonus to dEPV to get WdEPV
  df$WdEPV <- df$dEPV * bonus
  # Add attributes for the late-pick bonus parameters
  attr(df, "wd_epv_floor") <- floor_k
  # Add the power parameter to the attributes
  attr(df, "wd_epv_power") <- power
  # Return the modified dataframe
  df
}

## Apply the add_WdEPV function to the drafts dataset, creating new dataste drafts_w
drafts_w <- add_WdEPV(drafts) 


# Bias Fix 2: Pick-mix Bias  ->  z-score within pick tiers
drafts_w <- drafts_w %>% 
  mutate(
    PickTier = case_when(
      # Assigning pick tiers based on the draft pick number
      Pk <= 14 ~ "Lottery",
      Pk <= 22 ~ "Mid-1st",
      Pk <= 30 ~ "Late-1st",
      Pk <= 44 ~ "Early-2nd",
      TRUE     ~ "Late-2nd"
    )
  ) %>%
  # Group by PickTier to calculate z-scores
  group_by(PickTier) %>% 
  mutate(
    # Calculate z-scores for dEPV and dEPV_decade within each pick tier
    ## Subtract each tier's mean and divide by its standard deviation
    z_dEPV       = (dEPV - mean(dEPV, na.rm = TRUE)) / sd(dEPV, na.rm = TRUE),
    z_dEPV_decade   = (dEPV_decade - mean(dEPV_decade, na.rm = TRUE)) /
      sd(dEPV_decade, na.rm = TRUE)
    # z_dEPV and z_dEPV_decade are now standardized scores within each pick tier; tell how many standard deviations a player beats or misses typical surplus for that tier
  ) %>% 
  ungroup()
## Raw averages penalise lottery-heavy franchises because their picks face tougher expected baselines. 
### By converting each surplus to a within-tier z score, every pick's performance is measured relative to peers in the same tier


# Create team_summary to use for visualizations 
team_summary <- drafts_w |> 
  group_by(Tm) |> 
  summarise(
    # --- basic counts ----
    Picks          = n(),
    
    # --- VOADP metrics (value vs average classmate) ----
    Avg_VOADP      = mean(VOADP, na.rm = TRUE),   # per-pick efficiency
    Total_VOADP    = sum(VOADP,  na.rm = TRUE),   # raw haul of surplus
    Median_VOADP   = median(VOADP, na.rm = TRUE),
    BreakEvenRate  = mean(VOADP > 0, na.rm = TRUE), # % picks above 0
    
    # --- ΔEPV metrics (value vs expected-pick curve) ----
    Avg_dEPV       = mean(dEPV, na.rm = TRUE),
    Total_dEPV     = sum(dEPV,  na.rm = TRUE),
    
    # ── late-pick weighted surplus ──────────────────────────────────────────
    Total_WdEPV    = sum(WdEPV, na.rm = TRUE),
    Avg_WdEPV      = mean(WdEPV, na.rm = TRUE),
    # z-score adjustments
    Avg_z_dEPV      = mean(z_dEPV,      na.rm = TRUE),   # pick-mix fixed
    Avg_z_dEPV_dec  = mean(z_dEPV_decade,  na.rm = TRUE),   # + era fixed
    
    .groups = "drop"
  ) |> 
  arrange(desc(Avg_VOADP))  

# Save the team_summary table to a CSV file
write_csv(team_summary, "team_summary_2004_2024.csv")

# Save the drafts_w dataset to a CSV file
write_csv(drafts_w, "drafts_WdEPV_2004_2024.csv")