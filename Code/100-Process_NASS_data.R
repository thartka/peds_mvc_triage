############################################################
#
#  Project: Development of a reduced pediatric injury prediction model  
#
#  This script opens an R data file, extracts necessary features and 
#   outcomes from specific tables, then stores it as a CSV.
#
###########################################################

rm(list=ls())

library(tidyverse)
library(reshape2)

# load nass and ciss data

load("./Data/NASS/NASS.RData")

# create case key
occupants <- occupants %>% mutate(key = paste(year, psu, caseno, vehno, occno, sep = "_"),
                                  veh_key = paste(year, psu, caseno, vehno, sep = "_") )

# store to new dataframe and create case weights column
occ_bayes <- occupants %>% mutate(casewgt = ratwgt)

##############################################################
#  Filter data ------------------------------
#

# filter for age <= 18 years
occ_bayes <- occ_bayes %>% filter(age<=18)
occ_bayes %>% count()

# filter out cases with cases 2000-2015
occ_bayes <- occ_bayes %>% filter(year>=2000)
occ_bayes %>% count()

# filter cases with crash year >10 from model year after 2009
occ_bayes %>% filter(year>=2009 & ((occ_bayes$year - occ_bayes$modelyr) > 10)) %>% count()
occ_bayes <- occ_bayes %>% filter(year<2009 | ((occ_bayes$year - occ_bayes$modelyr) <= 10))
occ_bayes %>% count()

# filter for cases missing ISS
occ_bayes[is.na(occ_bayes$iss),] %>% count()
occ_bayes <- occ_bayes[!is.na(occ_bayes$iss),] 
occ_bayes %>% count()

# look at data
table(occ_bayes$dvtotal, occ_bayes$dvest, useNA = "ifany")


##############################################################
#  Extract direct features ------------------------------
#   (age,dvtotal, rollover, entrapment)
#

# rollover flag and quarter turns
occ_bayes$rolled <- ifelse(occ_bayes$rollover > 0, 1, 0)
occ_bayes$roll_turns <- ifelse(occ_bayes$rollover > 0, occ_bayes$rollover, 0)

# set end-over-end code (98 to 17)
occ_bayes$roll_turns <- ifelse(occ_bayes$roll_turns==98, 17, occ_bayes$roll_turns)


# Flag multiple collisions, this is a crude determination of multiple impacts
#  if the vehicle is specified as impacting a second object it is flagged.  
#  It is not known if this was a major or minor impact
occ_bayes$multicoll <- ifelse(is.na(occ_bayes$objcont2), 0, 1)

# flag entrapment
occ_bayes$entrapment <- ifelse(occ_bayes$entrap > 0, 1, 0)

##############################################################
#  Determine PDOF ------------------------------
#

# Create general cateregories for PDOF

occ_bayes$pdof_front <- ifelse((occ_bayes$pdof1>315 & occ_bayes$pdof1<=360) |
                            (occ_bayes$pdof1< 45), 1, 0)
occ_bayes$pdof_right <- ifelse(occ_bayes$pdof1>45 & occ_bayes$pdof1<135, 1, 0)
occ_bayes$pdof_rear <- ifelse(occ_bayes$pdof1>135 & occ_bayes$pdof1<225, 1, 0)
occ_bayes$pdof_left <- ifelse(occ_bayes$pdof1>225 & occ_bayes$pdof1<315, 1, 0)

# Nearside and farside flags
#  last digit of seatpos (1-left, 2-middle, 3-right)
#  any impact to the side that is not a nearside is a farside, so 
#  middle position considered farside for this analysis
occ_bayes$seat_side <- occ_bayes$seatpos %% 10
occ_bayes$seat_row <- floor(occ_bayes$seatpos / 10)

occ_bayes$pdof_nearside <- ifelse((occ_bayes$pdof_left == 1 & occ_bayes$seat_side == 1) 
                             |(occ_bayes$pdof_right == 1 & occ_bayes$seat_side == 3), 1, 0)
occ_bayes$pdof_farside <- ifelse((occ_bayes$pdof_left == 1 | occ_bayes$pdof_right == 1) & occ_bayes$pdof_nearside == 0, 1, 0)


# flag seat positions
occ_bayes$seat_dr <- ifelse(occ_bayes$seat_row == 1 & occ_bayes$seat_side == 1, 1, 0)
occ_bayes$seat_fmp <- ifelse(occ_bayes$seat_row == 1 & occ_bayes$seat_side == 2, 1, 0)
occ_bayes$seat_frp <- ifelse(occ_bayes$seat_row == 1 & occ_bayes$seat_side == 3, 1, 0)
occ_bayes$seat_rlp <- ifelse(occ_bayes$seat_row == 2 & occ_bayes$seat_side == 1, 1, 0)
occ_bayes$seat_rmp <- ifelse(occ_bayes$seat_row == 2 & occ_bayes$seat_side == 2, 1, 0)
occ_bayes$seat_rrp <- ifelse(occ_bayes$seat_row == 2 & occ_bayes$seat_side == 3, 1, 0)

##############################################################
#  Impute missing height/weight from age -----------------------
#

# get mean height weight for age
mean_measure <- occ_bayes %>% 
  select(age, weight, height) %>% 
  na.omit() %>% 
  group_by(age) %>% 
  summarise(wgt=mean(weight), hgt=mean(height))

# add imputed values to data
occ_bayes <- occ_bayes %>% 
  mutate(wgt_imp = mean_measure$wgt[age+1]) %>% 
  mutate(wgt_imp = ifelse(!is.na(weight), weight, wgt_imp))

occ_bayes <- occ_bayes %>% 
  mutate(hgt_imp = mean_measure$hgt[age+1]) %>% 
  mutate(hgt_imp = ifelse(!is.na(height), height, hgt_imp))

  
##############################################################
#  Proper restraint (Doud methodology) -----------------------
#
 
# default is properly restrained
occ_bayes$prop_restraint <- 1

# children 12 years or under in front seat are improperly restrained
occ_bayes <- occ_bayes %>% 
  mutate(prop_restraint = ifelse((age<=12) & (seat_row==1),0,prop_restraint))

# children 0-1 years should be in rear facing 
occ_bayes <- occ_bayes %>% 
  mutate(prop_restraint = ifelse((age<=1) & (chorient!=1),0,prop_restraint))
  
# children 2-4 years <18kg should be forward facing
occ_bayes <- occ_bayes %>% 
  mutate(prop_restraint = ifelse((age>=2) & (age<=4) & (wgt_imp<18) & (chorient!=2),0,prop_restraint))

# children 2-4 years >=18kg should use a booster 
occ_bayes <- occ_bayes %>% 
  mutate(prop_restraint = ifelse((age>=2) & (age<=4)  & (wgt_imp>=18) & !(chtype %in% c(4,5)),0,prop_restraint))

# children 5-7 years <36 kg or <145 cm should have booster
occ_bayes <- occ_bayes %>% 
  mutate(prop_restraint = ifelse((age>=5) & (age<=7) & ((wgt_imp<36)|(hgt_imp<145)) 
                                  & !(chtype %in% c(4,5)),0,prop_restraint))

# children 8 and above should be wearing lap and shoulder
occ_bayes <- occ_bayes %>% 
  mutate(prop_restraint = ifelse((age>=8) & (manuse!=4),0,prop_restraint))

# make flag for unrestrained
occ_bayes <- occ_bayes %>% 
  mutate(no_restraint = ifelse((manuse==0) & (chtype==0),1,0)) %>% 
  mutate(any_restraint = ifelse(no_restraint==1,0,1))

# if unrestrainted is 1, properly restrained is 0
occ_bayes <- occ_bayes %>% 
  mutate(prop_restraint = ifelse(no_restraint==1,0,prop_restraint))


##############################################################
#  Determine death and death in same compartment -----------------------
#

# if police report indicates patient died but we don't know when
#  then set death = 1
occ_bayes <- occ_bayes %>% mutate(death = ifelse(is.na(death) & injsev==4, 1, death))
occupants <- occupants %>% mutate(death = ifelse(is.na(death) & injsev==4, 1, death))

# make died flag
occ_bayes <- occ_bayes %>% mutate(died = ifelse((death>0 & death<96) | injsev==4, 1, 0))
occupants <- occupants %>% mutate(died = ifelse((death>0 & death<96) | injsev==4, 1, 0))

# if died is na, set to 0
occ_bayes <- occ_bayes %>% mutate(died = ifelse(is.na(died), 0, died))

# if occupant died and iss<24, set to 24
occ_bayes <- occ_bayes %>% mutate(iss = ifelse(died==1 & iss<24, 24, iss))

# number of occupants that died in each vehicle
vdied <- occupants %>% group_by(veh_key) %>% summarise(veh_death=sum(died), veh_occ_num=n()) %>% ungroup()

# merge into peds data
occ_bayes <- occ_bayes %>% left_join(vdied, by='veh_key')

# set other death flag (if someone else in the vehicle died)
occ_bayes <- occ_bayes %>% 
  mutate(other_death = ifelse(veh_death-died>0, 1, 0))

##############################################################
#  Determine intrusions -----------------------
#

# create key and make sure column names are unique
vehicles <- vehicles %>% 
  setNames(make.names(names(.), unique = TRUE)) %>% 
  mutate(veh_key = paste(year, psu, caseno, vehno, sep = "_")) %>% 
  replace(is.na(.), 0)

# add catastrophic flag
vehicles <- vehicles %>% 
  mutate(intcat = ifelse(inloc1==97 | inmag1==7,1,0)) 

# find max intrusion
vehicles$intmax <- vehicles %>% 
  select(contains('inmag')) %>% 
  mutate(intmax = do.call(pmax, (.))) %>% 
  .$intmax 

# set flag for 18 inch intrusion (45 cm)
vehicles <- vehicles %>% 
  mutate(int18 = ifelse(intmax>=5,1,0)) 

# add intrusion 18 and catastrophic intrusion to occupants
occ_bayes <- occ_bayes %>% 
  left_join(vehicles %>% select(veh_key, int18, intcat), by=c('veh_key')) 

# melt intrusion location
occ_intrusion <- vehicles %>% 
  select(veh_key, starts_with("inloc")) %>% 
  melt(id.ars='veh_key') %>% 
  rename(seatpos=value) %>% 
  select(-variable)

# melt intrusion magnitude 
int_mag <- vehicles %>% 
  select(veh_key, contains('inmag')) %>% 
  melt(id.ars='veh_key') %>% 
  rename(intmag=value)

# combine intrusion location and magnitude 
occ_intrusion$intmag <- int_mag$intmag

# select only max intrusion at seat position
occ_intrusion <- occ_intrusion %>% 
  group_by(veh_key,seatpos) %>% 
  arrange(desc(intmag)) %>% 
  slice(1) %>% 
  ungroup()

# join intrusions to seat positions 
occ_bayes <- occ_bayes %>% 
  left_join(occ_intrusion, by=c('veh_key','seatpos')) %>% 
  replace_na(list(intmag=0)) 
  
# add flag for intrusion of 12 inches at occupant position
occ_bayes <- occ_bayes %>% 
  mutate(int12occ = ifelse(intmag>=4,1,0)) 


##############################################################
#  Determine if any airbag deployed -----------------------
#

# create key 
airbag <- airbag %>% 
  mutate(veh_key = paste(year, psu, caseno, vehno, sep = "_")) %>% 
  replace(is.na(.), 0)

# make flag for airbag deployment at each location
airbag <- airbag %>% 
  mutate(deployed = ifelse(bagdeply==1, 1, 0))

# collapse for any deployment in vehicle
veh_ab <- airbag %>% 
  group_by(veh_key) %>% 
  summarise(sum(deployed)) %>% 
  mutate(abdeply = ifelse(.$'sum(deployed)'>=1,1,0)) %>% 
  select(-'sum(deployed)')

# merge into occupant bayes table
occ_bayes <- occ_bayes %>% 
  left_join(veh_ab, by=c('veh_key')) 

xtabs(~occ_bayes$dvtotal + is.na(occ_bayes$abdeply), addNA = TRUE)

xtabs(~occ_bayes$dvtotal + is.na(occ_bayes$bagdeply), addNA = TRUE)

xtabs(~is.na(occ_bayes$abdeply) + is.na(occ_bayes$bagdeply), addNA = TRUE)

occ_bayes %>% group_by(abdeply) %>% summarise(n())

# if NA first try to fill in with bagdeply, then replace with zero, finally make binary
occ_bayes <- occ_bayes %>% 
  mutate(abdeply = ifelse(is.na(abdeply), bagdeply, abdeply)) %>% 
  mutate(abdeply = ifelse(is.na(abdeply), 0, abdeply)) %>% 
  mutate(abdeply = ifelse(abdeply==1, 1, 0)) 


occ_bayes %>% group_by(abdeply) %>% summarise(n())

##############################################################
#  Determine ejection -----------------------
#

occ_bayes <- occ_bayes %>% 
  replace_na(list(ejection=0)) %>% 
  mutate(ejection_partial = ifelse(ejection==2 | ejection==3, 1, 0)) %>% 
  mutate(ejection_complete = ifelse(ejection==1, 1, 0)) %>% 
  mutate(ejection = ifelse(ejection==0, 0, 1)) 


##############################################################
#  Create response flags -----------------------
#

# ISS>=16
occ_bayes <- occ_bayes %>% 
  mutate(iss16 = ifelse(iss>=16, 1, 0))

# ISS>=24
occ_bayes <- occ_bayes %>% 
  mutate(iss24 = ifelse(iss>=24, 1, 0))

# create long table of injuries
injuries <- occ_bayes %>% 
  select(key, contains("ais90")) %>% 
  melt(id.vars = "key") %>% 
  mutate(region = floor(value/100000)) %>% 
  mutate(severity = round((value%%1)*10)) %>% 
  na.omit()

# MAIS head  
# get max head injury severity 
head_severity <- injuries %>% 
  group_by(key) %>% 
  filter(region==1) %>% 
  summarise(mais_head = max(severity))

# join to occupant data
occ_bayes <- occ_bayes %>% 
  left_join(head_severity,by="key") %>% 
  mutate(mais_head = ifelse(is.na(mais_head),0,mais_head))

# MAIS thorax
# get max thorax injury severity 
thorax_severity <- injuries %>% 
  group_by(key) %>% 
  filter(region==4) %>% 
  summarise(mais_thorax = max(severity))

# join to occupant data
occ_bayes <- occ_bayes %>% 
  left_join(thorax_severity,by="key") %>% 
  mutate(mais_thorax = ifelse(is.na(mais_thorax),0,mais_thorax))

# MAIS abd
# get max thorax injury severity 
abd_severity <- injuries %>% 
  group_by(key) %>% 
  filter(region==5) %>% 
  summarise(mais_abd = max(severity))

# join to occupant data
occ_bayes <- occ_bayes %>% 
  left_join(abd_severity,by="key") %>% 
  mutate(mais_abd = ifelse(is.na(mais_abd),0,mais_abd))


##############################################################
#  Identify occupants with target injuries ------------------
#

# read in target injury list
target <- read_csv("./Data/Peds-target_injury_list.csv")

# remove unneed columns
target <- target %>% 
  mutate(target_inj = on_til) %>% 
  select(crashType, ais_code, target_inj) 

# create table occupant injuries
occ_inj <- occ_bayes %>% 
  mutate(crashType = case_when(
    rolled == 1 ~ "Rollover",
    pdof_rear == 1 ~ "Rear",
    pdof_farside == 1 ~"SideFar",
    pdof_farside == 1 ~"SideNear",
    TRUE ~ "Frontal")) %>% 
  select(key, crashType, contains("ais90")) %>% 
  select(-contains("mais90")) 

# melt injuries
occ_inj <- occ_inj %>% 
  melt(id.ars=c('key','crashType')) %>% 
  select(-variable) %>%
  rename(ais_code=value) %>% 
  filter(!is.na(ais_code)) 

# filter for injuries in target list
tl_occ <- occ_inj %>% 
  left_join(target, by=c('crashType','ais_code')) %>% 
  filter(target_inj==1)

# find occupant ids for those with target injuries
tl_occ <- tl_occ %>% 
  select(key, target_inj) %>% 
  distinct() %>% 
  arrange(key) 

# set flag for target list injuries
occ_bayes <- occ_bayes %>% 
  left_join(tl_occ, by="key") %>% 
  mutate(target_inj=replace_na(target_inj,0)) 

##############################################################
#  Write to CSV -----------------------
#

# select necessary predictors and response
occ_bayes <- occ_bayes %>% 
  select(key, year, age, sex, wgt_imp, hgt_imp, prop_restraint, any_restraint, abdeply, seat_row, entrapment,
         dvtotal, splimit, multicoll, pdof_front, pdof_rear, pdof_nearside, pdof_farside, rolled, roll_turns, 
         intcat, int18, int12occ, ejection, ejection_partial, ejection_complete,
         died, other_death, iss, iss16, mais, mais_head, mais_thorax, mais_abd, target_inj, casewgt)

# write to file
occ_bayes %>% write.csv("./Data/NASS/NASSPeds-2000_2015-unfiltered.csv", row.names = FALSE)
