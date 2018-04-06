# File contains the code used to initialize the PITCHf/x and Statcast data

# Access tidyverse:
# install.packages('tidyverse')
library(tidyverse)

# Access baseballr package:
# install.packages('devtools')
# devtools::install_github('BillPetti/baseballr')
library(baseballr)

# Following Jim Albert's code for scraping the entire 2017 season, we break
# it up into chunks to ensure we can grab data without running into any
# connectivity issues

s1 <- scrape_statcast_savant_batter_all("2017-04-02", 
                                        "2017-04-08")
s2 <- scrape_statcast_savant_batter_all("2017-04-09", 
                                        "2017-04-15")
s3 <- scrape_statcast_savant_batter_all("2017-04-16", 
                                        "2017-04-22")
s4 <- scrape_statcast_savant_batter_all("2017-04-23", 
                                        "2017-04-29")
s5 <- scrape_statcast_savant_batter_all("2017-04-30", 
                                        "2017-05-06")
s6 <- scrape_statcast_savant_batter_all("2017-05-07", 
                                        "2017-05-13")
s7 <- scrape_statcast_savant_batter_all("2017-05-14", 
                                        "2017-05-20")
s8 <- scrape_statcast_savant_batter_all("2017-05-21", 
                                        "2017-05-27")
s9 <- scrape_statcast_savant_batter_all("2017-05-28", 
                                        "2017-06-03")
s10 <- scrape_statcast_savant_batter_all("2017-06-04", 
                                         "2017-06-10")
s11 <- scrape_statcast_savant_batter_all("2017-06-11", 
                                         "2017-06-17")
s12 <- scrape_statcast_savant_batter_all("2017-06-18", 
                                         "2017-06-24")
s13 <- scrape_statcast_savant_batter_all("2017-06-25", 
                                         "2017-07-01")
s14 <- scrape_statcast_savant_batter_all("2017-07-02", 
                                         "2017-07-08")
s15 <- scrape_statcast_savant_batter_all("2017-07-09", 
                                         "2017-07-15")
s16 <- scrape_statcast_savant_batter_all("2017-07-16", 
                                         "2017-07-22")
s17 <- scrape_statcast_savant_batter_all("2017-07-23", 
                                         "2017-07-29")
s18 <- scrape_statcast_savant_batter_all("2017-07-30", 
                                         "2017-08-05")
s19 <- scrape_statcast_savant_batter_all("2017-08-06", 
                                         "2017-08-12")
s20 <- scrape_statcast_savant_batter_all("2017-08-13", 
                                         "2017-08-19")
s21 <- scrape_statcast_savant_batter_all("2017-08-20", 
                                         "2017-08-26")
s22 <- scrape_statcast_savant_batter_all("2017-08-27", 
                                         "2017-09-02")
s23 <- scrape_statcast_savant_batter_all("2017-09-03", 
                                         "2017-09-09")
s24 <- scrape_statcast_savant_batter_all("2017-09-10", 
                                         "2017-09-16")
s25 <- scrape_statcast_savant_batter_all("2017-09-17", 
                                         "2017-09-23")
s26 <- scrape_statcast_savant_batter_all("2017-09-24", 
                                         "2017-09-30")
s27 <- scrape_statcast_savant_batter_all("2017-10-01", 
                                         "2017-11-01")
sc1 <- rbind(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10,
             s11, s12, s13, s14)
sc2 <- rbind(s15, s16, s17, s18, s19, s20, s21,
             s22, s23, s24, s25, s26, s27)

statcast_2017 <- rbind(sc1, sc2)

# ----------------------------------------------------

# -----------------------------------------------
# Create datasets for each hitter in the Reds lineup and save to the 
# reds_hitters folder with following name convention: firstname_lastname_year.csv

# Billy Hamilton:
playerid_lookup(last_name = "Hamilton")
# 571740

statcast_2017 %>%
  filter(batter == 571740) %>%
  write_csv("/CMU_Baseball_Workshop/pitch_level_data/reds_hitters/billy_hamilton_2017.csv")

# Eugenio Suarez:
playerid_lookup(last_name = "Suarez")
# 553993

statcast_2017 %>%
  filter(batter == 553993) %>%
  write_csv("/CMU_Baseball_Workshop/pitch_level_data/reds_hitters/eugenio_suarez_2017.csv")

# Joey Votto:
playerid_lookup(last_name = "Votto")
# 458015

statcast_2017 %>%
  filter(batter == 458015) %>%
  write_csv("/CMU_Baseball_Workshop/pitch_level_data/reds_hitters/joey_votto_2017.csv")

# Scooter Gennett:
playerid_lookup(last_name = "Gennett")
# 571697

statcast_2017 %>%
  filter(batter == 571697) %>%
  write_csv("/CMU_Baseball_Workshop/pitch_level_data/reds_hitters/scooter_gennett_2017.csv")

# Adam Duvall:
playerid_lookup(last_name = "Duvall")
# 594807

statcast_2017 %>%
  filter(batter == 594807) %>%
  write_csv("/CMU_Baseball_Workshop/pitch_level_data/reds_hitters/adam_duvall_2017.csv")

# Jose Peraza:
playerid_lookup(last_name = "Peraza")
# 606299

statcast_2017 %>%
  filter(batter == 606299) %>%
  write_csv("/CMU_Baseball_Workshop/pitch_level_data/reds_hitters/jose_peraza_2017.csv")

# Jesse Winker:
playerid_lookup(last_name = "Winker")
# 608385

statcast_2017 %>%
  filter(batter == 608385) %>%
  write_csv("/CMU_Baseball_Workshop/pitch_level_data/reds_hitters/jesse_winker_2017.csv")

# Tucker Barnhart:
playerid_lookup(last_name = "Barnhart")
# 571466

statcast_2017 %>%
  filter(batter == 571466) %>%
  write_csv("/CMU_Baseball_Workshop/pitch_level_data/reds_hitters/tucker_barnhart_2017.csv")

# Scott Schebler:
playerid_lookup(last_name = "Schebler")
# 594988

statcast_2017 %>%
  filter(batter == 594988) %>%
  write_csv("/CMU_Baseball_Workshop/pitch_level_data/reds_hitters/scott_schebler_2017.csv")

# Phillip Ervin:
playerid_lookup(last_name = "Ervin")
# 640447

statcast_2017 %>%
  filter(batter == 640447) %>%
  write_csv("/CMU_Baseball_Workshop/pitch_level_data/reds_hitters/phillip_ervin_2017.csv")

# Phil Gosselin:
playerid_lookup(last_name = "Gosselin")
# 594838

statcast_2017 %>%
  filter(batter == 594838) %>%
  write_csv("/CMU_Baseball_Workshop/pitch_level_data/reds_hitters/phil_gosselin_2017.csv")

# Cliff Pennington:
playerid_lookup(last_name = "Pennington")
# 460060

statcast_2017 %>%
  filter(batter == 460060) %>%
  write_csv("/CMU_Baseball_Workshop/pitch_level_data/reds_hitters/cliff_pennington_2017.csv")

# Devin Mesoraco:
playerid_lookup(last_name = "Mesoraco")
# 519023

statcast_2017 %>%
  filter(batter == 519023) %>%
  write_csv("/CMU_Baseball_Workshop/pitch_level_data/reds_hitters/devin_mesoraco_2017.csv")

# Now save Pirates pitchers (given that Chad Kuhl is starting), save to the 
# pirates_pitchers folder with following name convention: firstname_lastname_year.csv

# Chad Kuhl:
playerid_lookup(last_name = "Kuhl")
# 641771

statcast_2017 %>%
  filter(pitcher == 641771) %>%
  write_csv("/CMU_Baseball_Workshop/pitch_level_data/pirates_pitchers/chad_kuhl_2017.csv")

# Tyler Glasnow:
playerid_lookup(last_name = "Glasnow")
# 607192

statcast_2017 %>%
  filter(pitcher == 607192) %>%
  write_csv("/CMU_Baseball_Workshop/pitch_level_data/pirates_pitchers/tyler_glasnow_2017.csv")

# George Kontos:
playerid_lookup(last_name = "Kontos")
# 502004

statcast_2017 %>%
  filter(pitcher == 502004) %>%
  write_csv("/CMU_Baseball_Workshop/pitch_level_data/pirates_pitchers/george_kontos_2017.csv")

# Dovydas Neverauskas:
playerid_lookup(last_name = "Neverauskas")
# 596720

statcast_2017 %>%
  filter(pitcher == 596720) %>%
  write_csv("/CMU_Baseball_Workshop/pitch_level_data/pirates_pitchers/dovydas_neverauskas_2017.csv")

# Felipe Rivero:
playerid_lookup(last_name = "Rivero")
# 553878

statcast_2017 %>%
  filter(pitcher == 553878) %>%
  write_csv("/CMU_Baseball_Workshop/pitch_level_data/pirates_pitchers/felipe_rivero_2017.csv")





