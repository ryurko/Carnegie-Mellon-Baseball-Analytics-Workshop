# This script contains the starter code for analyzing each hitter on the Reds lineup
# as well as selected Pirates pitchers.

# First install and access the necessary packages and access them:
# install.packages('readr')
library(readr)

# install.packages('dplyr')
library(dplyr)

# install.packages('ggplot2')
library(ggplot2)

# install.packages('mgcv')
library(mgcv)

# The following information will be useful for working with the pitch-by-pitch data,

# Pitch Type Abbreviations:

# FASTBALLS:
# - FF = four-seam fastball (most common pitch in baseball)
# - FT = two-seam fastball (more movement than FF)
# - FC = cutter (look up Mariano Rivera)
# - FS / SI / SF = sinker / split-fingered 

# OFFSPEED:
# - SL = slider 
# - CH = changeup
# - CB / CU = curveball
# - KC = knuckle-curve
# - KN = knuckleball
# - EP = eephus

# OTHER:
# - UN / XX = unidentified
# - PO / FO = pitch out

# The `type` column has the following values:
# - X = ball in play
# - S = strike 
# - B = ball

# Pitch locations:
# plate_x = the left/right distance, in feet, of the pitch 
#           from the middle of the plate as it crossed home plate from
#           the catcher's point of view with distances to the right as 
#           positive, left as negative.
# plate_z = height of the pitch in feet as it crossed the front of home plate


# Each player section is divided by ------ marks. The first player, Joey Votto, 
# has example code for different visualizations to generate providing insight
# into his strengths and weaknesses as a hitter (HINT: he's probably a future
# Hall of Fame player, expect few weaknesses...)

# ----------------------------------------------------------------------------------
# CINCINATTI REDS HITTERS
# ----------------------------------------------------------------------------------

# Load the Joey Votto (id: 458015) data:
votto_data <- read_csv("https://raw.github.com/ryurko/Carnegie-Mellon-Baseball-Analytics-Workshop/master/pitch_level_data/reds_hitters/joey_votto_2017.csv")

# What pitches did Votto face this season?
ggplot(votto_data, 
       aes(x = pitch_type,
           # Barchart with percent along y-axis
           y = (..count..) / sum(..count..))) +
  geom_bar() +
  labs(title = "Types of Pitches Thrown Against Joey Votto in 2017",
       x = "Pitch Type",
       y = "Proportion of Pitches",
       caption = "Data courtesy of MLBAM") + 
  theme_bw()

# Let's consider the most frequent pitches thrown against Joey Votto,
# how does he perform against them:

votto_pitch_summary_stats <- votto_data %>%
  # First filter the votto dataset to be the most common pitches:
  filter(pitch_type %in% c("CH", "CU", "FC", "FF", "FT", "SI", "SL")) %>%
  
  # Next, using the description, type, and events columns - let's define some useful
  # columns that will help us calculate stats for each of the different pitch types:
  
  mutate(# Create an indicator for whether or not Votto made an attempt at the pitch:
    swing = ifelse(description %in%
                     c("foul", "foul_bunt",
                       "foul_tip", "hit_into_play",
                       "hit_into_play_no_out",
                       "hit_into_play_score",
                       "missed_bunt", "swinging_strike",
                       "swinging_strike_blocked"), 
                   1, 0),
    
    # Create an indicator for whether or not Votto missed in his attempt:
    miss = ifelse(description %in%
                    c("missed_bunt", "swinging_strike",
                      "swinging_strike_blocked"), 
                  1, 0),
    
    # Using the type and events column, create indicators for each hit type:
    single = ifelse(type == "X" & events == "single", 1, 0),
    double = ifelse(type == "X" & events == "double", 1, 0),
    triple = ifelse(type == "X" & events == "triple", 1, 0),
    home_run = ifelse(type == "X" & events == "home_run", 1, 0)) %>%
  
  # Now can calculate various stats at the pitch type level:
  group_by(pitch_type) %>%
  
  # Use the summarise function to calculate the frequencies for each of
  # these indicators:
  summarise(n_pitches = n(),
            n_swings = sum(swing, na.rm = TRUE),
            n_miss = sum(miss, na.rm = TRUE),
            n_singles = sum(single, na.rm = TRUE),
            n_doubles = sum(double, na.rm = TRUE),
            n_triples = sum(triple, na.rm = TRUE),
            n_home_runs = sum(home_run, na.rm = TRUE)) %>%
  
  # Using these sums can calculate commonly seen baseball stats:
  mutate(swing_rate = round(n_swings / n_pitches, 3),
         miss_rate = round(n_miss / n_swings, 3),
         batting_average = round((n_singles + n_doubles + n_triples + n_home_runs) / n_swings, 3),
         slugging_percentage = round((n_singles + 2 * n_doubles + 3 * n_triples + 4 * n_home_runs) / n_swings, 3),
         ops = round(batting_average + slugging_percentage, 3))

# Visual summaries of Votto's performance against different pitches:
votto_pitch_summary_stats %>%
  ggplot(aes(x = pitch_type, y = miss_rate, fill = pitch_type)) + 
  scale_fill_brewer(palette = "Set1", guide = FALSE) +
  geom_bar(stat = "identity") + 
  labs(title = "Joey Votto Whiff Rate by Pitch Type in 2017",
       # Must give credit to where credit is due...
       caption = "Data courtesy of MLBAM",
       x = "Pitch Type",
       y = "Whiff Rate") +
  theme_bw()

votto_pitch_summary_stats %>%
  ggplot(aes(x = pitch_type, y = ops, fill = pitch_type)) + 
  scale_fill_brewer(palette = "Set1", guide = FALSE) +
  geom_bar(stat = "identity") + 
  labs(title = "Joey Votto OPS by Pitch Type in 2017",
       caption = "Data courtesy of MLBAM",
       x = "Pitch Type",
       y = "OPS") +
  theme_bw()

# Can revisit Jim Albert's talk - exit velocity by pitch type:
votto_data %>%
  # First filter the votto dataset to be the most common pitches:
  filter(pitch_type %in% c("CH", "CU", "FC", "FF", "FT", "SI", "SL"),
         type == "X") %>%
  ggplot(aes(x = pitch_type, y = launch_speed, fill = pitch_type)) +
  scale_fill_brewer(palette = "Set1", guide = FALSE) + 
  geom_violin(alpha = 0.8, color = "black") + 
  labs(title = "Joey Votto's Exit Velocity by Pitch Type in 2017",
       caption = "Data courtesy of MLBAM",
       x = "Pitch Type",
       y = "Exit Velocity (mph)") +
  coord_flip() + 
  theme_bw()

# Useful for statisticians, but if you're working for a team you need to be able
# to effectively communicate with non-quantitative people - like baseball coaches.
# Try to make visuals that they're familiar with, like strike zone plots!

# Strize zones technically vary by batter (and umpire...) but we'll use an average
# definition. The `plate_x` and `plate_z` variables are the horizontal and vertical
# locations of the pitches, in feet from center of strike zone.

# Create a data frame of the strike zone corners:
top_zone <- 3.5
bot_zone <- 1.6
left_zone <- -0.95
right_zone <- 0.95
strike_zone_df <- data.frame(
  x = c(left_zone, left_zone, right_zone, right_zone, left_zone),
  y = c(bot_zone, top_zone, top_zone, bot_zone, bot_zone)
)

# Plot all pitches Votto saw in 2017 with the strike zone over top:
ggplot(votto_data, aes(x = plate_x, y = plate_z)) + 
  geom_point(alpha = 0.5) + 
  geom_path(data = strike_zone_df,aes(x, y), lwd = 1.5, color = "red") + 
  labs(title = "Location of All Pitches Thrown Against Joey Votto in 2017 ",
       caption = "Data courtesy of MLBAM",
       x = "Horizontal Location (feet)",
       y = "Vertical Location (feet)") +
  theme_bw()

# Most common types of pitches?
votto_data %>%
  # First filter the votto dataset to be the most common pitches:
  filter(pitch_type %in% c("CH", "CU", "FC", "FF", "FT", "SI", "SL")) %>%
  ggplot(aes(x = plate_x, y = plate_z, color = pitch_type)) + 
  geom_point(alpha = 0.5) + 
  scale_color_brewer(palette = "Set1", "Pitch Type") + 
  geom_path(data = strike_zone_df,aes(x, y), lwd = 1.5, color = "black") + 
  labs(title = "Location of Pitches Thrown Against Joey Votto in 2017 by Pitch Type",
       caption = "Data courtesy of MLBAM",
       x = "Horizontal Location (feet)",
       y = "Vertical Location (feet)") +
  theme_bw()

# Let's try to model Joey Votto's swing and whiff tendencies across the strike zone:
votto_data <- votto_data %>%
  mutate(swing = ifelse(description %in%
                          c("foul", "foul_bunt",
                            "foul_tip", "hit_into_play",
                            "hit_into_play_no_out",
                            "hit_into_play_score",
                            "missed_bunt", "swinging_strike",
                            "swinging_strike_blocked"), 
                        1, 0),
         miss = ifelse(description %in%
                         c("missed_bunt", "swinging_strike",
                           "swinging_strike_blocked"), 
                       1, 0))

# Using the pitch location, we can predict the probability of 
# Joey Votto swinging at a pitch:

swing_model_fit <- gam(swing ~ s(plate_x, plate_z), family = binomial, data = votto_data)

# Find predicted probabilities over a 50 x 50 grid
x <- seq(-1.5, 1.5, length.out=50)
z <- seq(0.5, 5, length.out=50)
swing_predict_data <- data.frame(plate_x = c(outer(x, z * 0 + 1)),
                           plate_z = c(outer(x * 0 + 1, z)))

# Get the predicted values from the model and convert to probability values:
swing_model_preds <- predict(swing_model_fit, swing_predict_data)
swing_predict_data <- swing_predict_data %>%
  mutate(swing_prob = exp(swing_model_preds) / (1 + exp(swing_model_preds)))

# Now plot Votto's predicted probabilities of swinging with the strike zone:
ggplot(swing_predict_data) +
  geom_tile(aes(x = plate_x, y = plate_z, fill = swing_prob)) +
  scale_fill_gradient(low = "darkblue", high = "darkorange1", "Swing Probability") +
  geom_path(data = strike_zone_df, aes(x, y), lwd = 1.5, color = "white") + 
  # coord_fixed just makes sure the axes are scaled properly in relation to each other
  coord_fixed() +
  theme_bw() + 
  labs(title = "Joey Votto's Swing Probability in 2017",
       subtitle = "From the Catcher's POV: Against RHP & LHP",
       x = "Horizontal Location (feet)",
       y = "Vertical Location (feet)",
       caption = "Data courtesy of MLBAM") 

# Swing and miss probability:
miss_model_fit <- gam(miss ~ s(plate_x, plate_z), family=binomial, 
                      data = filter(votto_data, swing == 1))

# Get the predicted values from the model and convert to probability values:
miss_model_preds <- predict(miss_model_fit, swing_predict_data)
swing_predict_data <- swing_predict_data %>%
  mutate(miss_prob = exp(miss_model_preds) / (1 + exp(miss_model_preds)))

ggplot(swing_predict_data) +
  geom_tile(aes(x = plate_x, y = plate_z, fill = miss_prob)) +
  scale_fill_gradient(low = "darkblue", high = "darkorange1", "Whiff Probability") +
  geom_path(data = strike_zone_df, aes(x, y), lwd = 1.5, color = "white") + 
  coord_fixed() +
  theme_bw() + 
  labs(title = "Joey Votto's Whiff Probability in 2017",
       subtitle = "From the Catcher's POV: Against RHP & LHP",
       x = "Horizontal Location (feet)",
       y = "Vertical Location (feet)",
       caption = "Data courtesy of MLBAM") 

# What about exit velocity?
velo_model_fit <- gam(launch_speed ~ s(plate_x, plate_z), 
                      data = filter(votto_data, type == "X"))

# Get the predicted values from the model and convert to probability values:
velo_model_preds <- predict(velo_model_fit, swing_predict_data)
swing_predict_data <- swing_predict_data %>%
  mutate(exit_velo = velo_model_preds)

ggplot(swing_predict_data) +
  geom_tile(aes(x = plate_x, y = plate_z, fill = exit_velo)) +
  scale_fill_gradient(low = "darkblue", high = "darkorange1", "Exit Velocity (mph)") +
  geom_path(data = strike_zone_df, aes(x, y), lwd = 1.5, color = "white") + 
  coord_fixed() +
  theme_bw() + 
  labs(title = "Joey Votto's Exit Velocity in 2017",
       subtitle = "From the Catcher's POV: Against RHP & LHP",
       x = "Horizontal Location (feet)",
       y = "Vertical Location (feet)",
       caption = "Data courtesy of MLBAM") 

# What about the location of Votto's batted balls?

# Need to adjust the columns provided to us that represent the batted ball coordinates:
votto_data <- votto_data %>%
  mutate(hit_x = hc_x - 125.42, 
         hit_y = 198.27 - hc_y)

# Now let's create a visual showing the density of Votto's batted balls:
votto_data %>%
  filter(type == "X") %>%
  ggplot(aes(x = hit_x, y = hit_y)) + 
  stat_density_2d(aes(fill = ..level..), geom="polygon")+
  scale_fill_gradient(low="darkblue", high="darkorange1", "Density") +
  #geom_point(aes(x = hit_x, y = hit_y, color = events), fill = "black", shape = 21) +
  geom_segment(x=0, xend = 100, y=0, yend = 100, color = "white") +
  geom_segment(x=0, xend = -100, y=0, yend = 100, color = "white") +
  geom_curve(x = -45, xend = 45, y = 53, yend = 53, curvature = -.65, linetype = "dotted", color = "white") +
  theme_bw() + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()) #+ 
  #facet_grid(. ~ p_throws)

# ----------------------------------------------------------------------------------
# Load the Billy Hamilton (id: 571740) data:
hamilton_data <- read_csv("https://raw.github.com/ryurko/Carnegie-Mellon-Baseball-Analytics-Workshop/master/pitch_level_data/reds_hitters/billy_hamilton_2017.csv")



# ----------------------------------------------------------------------------------
# Load the Eugenio Suarez (id: 553993) data:
suarez_data <- read_csv("https://raw.github.com/ryurko/Carnegie-Mellon-Baseball-Analytics-Workshop/master/pitch_level_data/reds_hitters/eugenio_suarez_2017.csv")



# ----------------------------------------------------------------------------------
# Load the Scooter Gennett (id: 571697) data:
gennett_data <- read_csv("https://raw.github.com/ryurko/Carnegie-Mellon-Baseball-Analytics-Workshop/master/pitch_level_data/reds_hitters/scooter_gennett_2017.csv")



# ----------------------------------------------------------------------------------
# Load the Adam Duvall (id: 594807) data:
duvall_data <- read_csv("https://raw.github.com/ryurko/Carnegie-Mellon-Baseball-Analytics-Workshop/master/pitch_level_data/reds_hitters/adam_duvall_2017.csv")



# ----------------------------------------------------------------------------------
# Load the Jose Peraza (id: 606299) data:
peraza_data <- read_csv("https://raw.github.com/ryurko/Carnegie-Mellon-Baseball-Analytics-Workshop/master/pitch_level_data/reds_hitters/jose_peraza_2017.csv")



# ----------------------------------------------------------------------------------
# Load the Jesse Winker (id: 608385) data:
winker_data <- read_csv("https://raw.github.com/ryurko/Carnegie-Mellon-Baseball-Analytics-Workshop/master/pitch_level_data/reds_hitters/jesse_winker_2017.csv")



# ----------------------------------------------------------------------------------
# Load the Tucker Barnhart (id: 571466) data:
barnhart_data <- read_csv("https://raw.github.com/ryurko/Carnegie-Mellon-Baseball-Analytics-Workshop/master/pitch_level_data/reds_hitters/tucker_barnhart_2017.csv")



# ----------------------------------------------------------------------------------
# Load the Scott Schebler (id: 594988) data:
schebler_data <- read_csv("https://raw.github.com/ryurko/Carnegie-Mellon-Baseball-Analytics-Workshop/master/pitch_level_data/reds_hitters/scott_schebler_2017.csv")



# ----------------------------------------------------------------------------------
# Load the Phillip Ervin (id: 640447) data:
ervin_data <- read_csv("https://raw.github.com/ryurko/Carnegie-Mellon-Baseball-Analytics-Workshop/master/pitch_level_data/reds_hitters/phillip_ervin_2017.csv")



# ----------------------------------------------------------------------------------
# Load the Phil Gosselin (id: 594838) data:
gosselin_data <- read_csv("https://raw.github.com/ryurko/Carnegie-Mellon-Baseball-Analytics-Workshop/master/pitch_level_data/reds_hitters/phil_gosselin_2017.csv")



# ----------------------------------------------------------------------------------
# Load the Cliff Pennington (id: 460060) data:
pennington_data <- read_csv("https://raw.github.com/ryurko/Carnegie-Mellon-Baseball-Analytics-Workshop/master/pitch_level_data/reds_hitters/cliff_pennington_2017.csv")



# ----------------------------------------------------------------------------------
# Load the Devin Mesoraco (id: 519023) data:
mesoraco_data <- read_csv("https://raw.github.com/ryurko/Carnegie-Mellon-Baseball-Analytics-Workshop/master/pitch_level_data/reds_hitters/devin_mesoraco_2017.csv")



# ----------------------------------------------------------------------------------
# PITTSBURGH PIRATES PITCHERS
# ----------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------
# Load the Chad Kuhl (id: 641771) data:
kuhl_data <- read_csv("https://raw.github.com/ryurko/Carnegie-Mellon-Baseball-Analytics-Workshop/master/pitch_level_data/pirates_pitchers/chad_kuhl_2017.csv")



# ----------------------------------------------------------------------------------
# Load the Tyler Glasnow (id: 607192) data:
glasnow_data <- read_csv("https://raw.github.com/ryurko/Carnegie-Mellon-Baseball-Analytics-Workshop/master/pitch_level_data/pirates_pitchers/tyler_glasnow_2017.csv")



# ----------------------------------------------------------------------------------
# Load the George Kontos (id: 502004) data:
kontos_data <- read_csv("https://raw.github.com/ryurko/Carnegie-Mellon-Baseball-Analytics-Workshop/master/pitch_level_data/pirates_pitchers/george_kontos_2017.csv")



# ----------------------------------------------------------------------------------
# Load the Dovydas Neverauskas (id: 596720) data:
neverauskas_data <- read_csv("https://raw.github.com/ryurko/Carnegie-Mellon-Baseball-Analytics-Workshop/master/pitch_level_data/pirates_pitchers/dovydas_neverauskas_2017.csv")



# ----------------------------------------------------------------------------------
# Load the Felipe Rivero (id: 553878) data:
rivero_data <- read_csv("https://raw.github.com/ryurko/Carnegie-Mellon-Baseball-Analytics-Workshop/master/pitch_level_data/pirates_pitchers/felipe_rivero_2017.csv")


