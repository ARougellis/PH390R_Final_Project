#####################################################################
#
#                 Public Health Final Project
#
#####################################################################

# Author:
#     Alexander Rougellis

# The Question:
#     What are people most likely to get arrested for? 

#### Tools and Data ####

#clear environment, if needed 
rm(list = ls())

#Some needed packages...
library(dplyr)
library(tidyverse)
library(knitr)
library(nycflights13)
library(babynames)
library(hms)
library(lubridate)
library(readxl)
library(broom)

# The data
arrests <- read.csv("~/Desktop/Spring 2021/PublHealth/FinalProject/arrests_national_adults.csv")

drug_arrest <- read.csv("~/Desktop/Spring 2021/PublHealth/FinalProject/arrests_national_drug.csv")

### Cleaning the Data ####

# Looking for missing variables
sum(is.na(arrests))
#       678

# Locating missing variables
summary(is.na(arrests))
#       - 644 in state_abbr
#       - 6 in f_55_59
#       - 18 in f_60_64
#       - 10 in f_65p

# Extracting the rows w/ missing variables. 
# 1.)
arrests %>% 
  filter(is.na(state_abbr))
#       It seems as if th estate abbreviations aren't shown in this data set and i cannot 
#       find a reason as to why. Looking at the other data sets in the website, 
#       it seems as if the majority of them are missing the same variables. 
#       Therefore, 
arrests_clean <- arrests %>% select(-state_abbr)
#       I will just ignore these variables and not take them into consideration 
#       as i am not able to determine which state belongs to which data points. 

arrests %>% 
  filter(is.na(f_55_59))
#       Valid for this and other ages w/ NA
#       Given that age and sex must be reported, Whenever there is  "NA", that 
#       just means no arrest were made for people w/in that sex and age group. 

# Now lets check drug arrest
sum(is.na(drug_arrest))
# Where
summary(is.na(drug_arrest))
# same place as main data set, state abbreviations. 



### General Visualization ####

#     Figure 1

# First I want to see total arrest per offense
arrests %>% 
  group_by(offense_name) %>% # group by this since its the topic of discussion
  mutate(total_arrest = total_male+total_female) %>% 
            # get total arrest by adding both male and female cases since agencies are 
            # required to state sex per arrest.
  summarize(total_arrest_overall = sum(total_arrest)) %>% # total arrest per offense
  arrange(desc(total_arrest_overall)) %>% 
  ggplot()+
    geom_col(aes(x=fct_reorder(offense_name,total_arrest_overall), y=total_arrest_overall))+
    coord_flip()+
    labs(x=NULL, 
         y="Number of Arrests", 
         title = "Total Arrest Per Offense (1994-2016)", 
         caption = "Figure 1")+
    theme(plot.title = element_text(hjust = 0.5))
# The greatest amount belongs to "All Other Offenses" which is considered *check 
#       Appendix A bookmark (includes citation)* 

# If i decide to exclude other offenses, use this, if else, disregard.
arrests2 <- arrests %>% 
  filter(offense_name != "All Other Offenses") %>% 
  group_by(offense_name) %>% 
  mutate(total_arrest = total_male+total_female) %>% 
  summarize(total_arrest_overall = sum(total_arrest)) %>% 
  arrange(desc(total_arrest_overall))
arrests2 %>% 
    ggplot()+
    geom_col(aes(x=fct_reorder(offense_name,total_arrest_overall), y=total_arrest_overall))+
    coord_flip()+
    labs(x=NULL, 
         y="Number of Arrests", 
         title = "Total Arrest Per Offense (1994-2016)")+
    theme(plot.title = element_text(hjust = 0.5))


# Distribution of Arrest per Drug Possession
#         Figure 2
drug_arrest %>% 
  select(year, contains("possess")) %>% # pickin gout deug possession variables
  group_by(year) %>% 
  gather(key = "DrugPossession", value = "arrest_per_drug", 3:6) %>% 
  mutate(DrugPossession = fct_recode(DrugPossession, 
                                   "Marijuana" = "marijuana_possess", 
                                   "Opioid" = "opioid_possess", 
                                   "Other" = "other_possess", 
                                   "Synthetic" = "synthetic_possess")) %>% # remaning variables
  ggplot()+
  geom_line(aes(x=year, y=arrest_per_drug, color = DrugPossession))+
    labs(x = "Year", 
         y = "Total Arrest", 
         title = "Total Arrests Per Drug Possession", 
         caption = "Figure 2")+ # creating labels
    theme(plot.title = element_text(hjust = 0.5), # centering title
          axis.text.x = element_text(angle = 90))+ # changin gangle of x axis labels
    scale_x_continuous(breaks = seq(1995, 2016, by = 1))

### Crime by Gender ####

# Since There is a great difference between the numbers, we need to look at 
#       PROPORTIONS
# Figure 3
arrests %>% 
  group_by(offense_name) %>% 
  mutate(total_arrest = total_male+total_female) %>% 
  summarize(total_arrest_overall = sum(total_arrest), 
            total_male_overall = sum(total_male), 
            total_female_overall = sum(total_female)) %>% 
  gather(key = "Sex", value = "arrest_per_sex",
         "total_male_overall", "total_female_overall") %>% 
  mutate(Sex = fct_recode(Sex, 
                            "Male" = "total_male_overall",
                            "Female" = "total_female_overall")) %>% 
  ggplot()+
  geom_col(aes(x=offense_name,
               y=arrest_per_sex,
               fill = Sex), 
           position = "fill")+
  labs(x = NULL, 
       y = NULL, 
       title = "Proportion of Arrest per Offense by Sex")+
  geom_hline(aes(yintercept = 0.50))+
  coord_flip()
# Greater proportion of men overall (minus Prositiution) 
#       Women seem to break 25% when it realtes to money (embez, fraud, counterfit)
#             while men ore dominating in more violent offenses. 


# Age and Sex distribution 

# Cleaning up data set to be easier to use
arrests_age_sex <- arrests %>% 
  select(-c(state_abbr, offense_code, agencies, id, race_agencies, population,
            black, white, asian_pacific_islander, american_indian, race_population)) %>%
  gather("codes", "number_arrests", 5:36) %>%
  filter(!is.na(number_arrests)) %>% #removing NA as it serves no use. 
  mutate(codes = fct_collapse(codes, 
                             "m_18_19" = c("m_18", "m_19"), 
                             "m_20_24" = c("m_20", "m_21", "m_22", "m_23", "m_24"),
                             "f_18_19" = c("f_18", "f_19"), 
                             "f_20_24" = c("f_20", "f_21", "f_22", "f_23", "f_24") )) %>% 
  separate(codes, c("Sex", "age1", "age2"), sep = "_") %>% 
  unite("Age", age1, age2, sep = "-") %>%
  mutate(Sex = fct_recode(Sex, 
                          "Male"= "m", 
                          "Female" = "f"), 
         Age = fct_recode(Age, 
                          "65+" = "65p-NA"))
# Ended up not goign about it this way. Instead....

# ...i used this 
#       Figure 4
arrests %>% 
  select(-c(state_abbr, offense_code, agencies, id, race_agencies, population,
            black, white, asian_pacific_islander, american_indian, race_population)) %>%
                # removing unnecessary variables
  gather("codes", "number_arrests", 5:36) %>%
  filter(!is.na(number_arrests)) %>% #removing NA as it serves no use. 
  mutate(codes = fct_collapse(codes, # collapsing individual ages into groups
                              "m_18_19" = c("m_18", "m_19"), 
                              "m_20_24" = c("m_20", "m_21", "m_22", "m_23", "m_24"),
                              "f_18_19" = c("f_18", "f_19"), 
                              "f_20_24" = c("f_20", "f_21", "f_22", "f_23", "f_24") )) %>% 
  separate(codes, c("Sex", "age1", "age2"), sep = "_") %>% # next two linea make data easy to understand and work with 
  unite("Age", age1, age2, sep = "-") %>%
  mutate(Sex = fct_recode(Sex, # renaming variables
                          "Male"= "m", 
                          "Female" = "f"), 
         Age = fct_recode(Age, 
                          "65+" = "65p-NA"))%>% 
  group_by(Age) %>% 
  summarize(total_arrest_age = sum(number_arrests), 
            total_male_overall = sum(total_male), 
            total_female_overall = sum(total_female)) %>% # some summary statistics
  gather(key = "Sex", value = "arrest_per_age",
         "total_male_overall", "total_female_overall") %>% # collapsing variables. 
  mutate(Sex = fct_recode(Sex,  # renaming 
                          "Male" = "total_male_overall",
                          "Female" = "total_female_overall")) %>% 
  ggplot()+
  geom_col(aes(x=Age, y=arrest_per_age, 
               fill = Sex ), position = "dodge" )+
  labs(y="Number of Arrests", 
       title = "Total Arrest per Age by Sex (1994-2016)") +
  theme(plot.title = element_text(hjust = 0.5), # cneters title
        axis.text.x = element_text(angle = 45), 
        legend.position = "none")+ 
  scale_y_continuous(position = "right")
# I decided to remove the legend and move the y axis tick to the right side to save space
# Found it useless to include two identical legends next to eachother. 
# So i provided one for both. 


#### Crime by Race ####

# Since There is a great difference between the numbers, we need to look at 
#           PROPORTIONS
# Given that officers are not required to record race, not all arrest will be 
#       associated with a race
# Figure 5
arrests %>% 
  group_by(offense_name) %>% 
  mutate(total_race_arrest = white+black+asian_pacific_islander+american_indian) %>% 
            # total arrested for race (by adding them al together) 
  summarize(total_race_arrest_overall = sum(total_race_arrest), 
            total_white = sum(white), 
            total_black = sum(black), 
            total_asian = sum(asian_pacific_islander), 
            total_native = sum(american_indian)) %>% # summary statistics
  gather(key = "race", value = "arrest_per_race",
         "total_white", "total_black", "total_asian", "total_native") %>% 
  mutate(race = fct_recode(race, 
                          "White" = "total_white",
                          "Black" = "total_black", 
                          "Pacific Islander" = "total_asian", 
                          "Native American" = "total_native")) %>% 
  ggplot()+
  geom_col(aes(x=offense_name,
               y=arrest_per_race,
               fill = race), 
           position = "fill")+
  labs(x = NULL, 
       y = NULL, 
       title = "Proportion of Race per Offense")+
  geom_hline(aes(yintercept = 0.50))+
  coord_flip()



arrests_age_race <- arrests %>% 
  select(-c(state_abbr, offense_code, agencies, id, race_agencies, population, 
            race_population, total_female, total_male)) %>%
  gather("codes", "number_arrests", 3:34) %>%
  filter(!is.na(number_arrests)) %>% #removing NA as it serves no use. 
  mutate(codes = fct_collapse(codes, 
                              "m_18_19" = c("m_18", "m_19"), 
                              "m_20_24" = c("m_20", "m_21", "m_22", "m_23", "m_24"),
                              "f_18_19" = c("f_18", "f_19"), 
                              "f_20_24" = c("f_20", "f_21", "f_22", "f_23", "f_24") )) %>% 
  separate(codes, c("Sex", "age1", "age2"), sep = "_") %>% 
  unite("Age", age1, age2, sep = "-") %>%
  mutate(Sex = fct_recode(Sex, 
                          "Male"= "m", 
                          "Female" = "f"), 
         Age = fct_recode(Age, 
                          "65+" = "65p-NA"))
# Ended up not going about it this way. 


# Figure 6

arrests %>% 
  select(-c(state_abbr, offense_code, agencies, id, race_agencies, population, 
            race_population, total_female, total_male)) %>%
  gather("codes", "number_arrests", 3:34) %>%
  filter(!is.na(number_arrests)) %>% #removing NA as it serves no use. 
  mutate(codes = fct_collapse(codes, 
                              "m_18_19" = c("m_18", "m_19"), 
                              "m_20_24" = c("m_20", "m_21", "m_22", "m_23", "m_24"),
                              "f_18_19" = c("f_18", "f_19"), 
                              "f_20_24" = c("f_20", "f_21", "f_22", "f_23", "f_24") )) %>% 
  separate(codes, c("Sex", "age1", "age2"), sep = "_") %>% 
  unite("Age", age1, age2, sep = "-") %>%
  mutate(Sex = fct_recode(Sex, 
                          "Male"= "m", 
                          "Female" = "f"), 
         Age = fct_recode(Age, 
                          "65+" = "65p-NA"))%>% 
  group_by(Age) %>% 
  mutate(total_race_arrest = white+black+asian_pacific_islander+american_indian) %>% 
  summarize(total_race_arrest_overall = sum(total_race_arrest), 
            total_white = sum(white), 
            total_black = sum(black), 
            total_asian = sum(asian_pacific_islander), 
            total_native = sum(american_indian)) %>% 
  gather(key = "Race", value = "arrest_per_race",
         "total_white", "total_black", "total_asian", "total_native") %>% 
  mutate(Race = fct_recode(Race, 
                           "White" = "total_white",
                           "Black" = "total_black", 
                           "Pacific Islander" = "total_asian", 
                           "Native American" = "total_native")) %>% 
  ggplot()+
  geom_col(aes(x=Age, y=arrest_per_race,
               fill = Race ), position = "dodge" )+
  labs(y="Number of Arrests", 
       title = "Total Arrest per Age by Race (1994-2016)") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 45),
        legend.position = "none")+ 
  scale_y_continuous(position = "right")
# I decided to remove the legend and move the y axis tick to the right side to save space
# Found it useless to include two identical legends next to eachother. 
# So i provided one for both. 



### Time Graphs ####

# Total Arrest by Year
# final figure
arrests %>% 
  mutate(total_arrest = total_male+total_female) %>% 
  group_by(year) %>% 
  summarize(arrests_per_year = sum(total_arrest)) %>% 
  ggplot()+
  geom_line(aes(x=year, 
                y=arrests_per_year))+
  scale_x_continuous(breaks = seq(1994, 2016, by = 1))+
  labs(x = "Year", 
       y = "Number of Arrest", 
       title = "Total Arrest per Year")+
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 45))




# Total Arrest by Year by Race
arrests %>% 
  select(year, white, black, asian_pacific_islander, american_indian)%>% 
  gather(key = "Race", value = "number_arrests", 2:5 ) %>%
  group_by(year, Race) %>% 
  mutate(total_arrest_race = sum(number_arrests), 
         prop = number_arrests/total_arrest_race) %>%
  ggplot()+
  geom_col(aes(x=year, 
               y=number_arrests))+
  #geom_point()+
  facet_wrap(~Race, scales = "free_y")+
  scale_x_continuous(breaks = seq(1994, 2016, by = 2))+
  labs(x = "Year", 
       y = "Number of Arrest", 
       title = "Total Arrest per Year by Race")+
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 90))
#             Seems as if no crazy variation in arrest by race over the years.
#             Therefore, 
#                 no need to include. 



# Total Arrest by Year by Sex
arrests %>% 
  select(year, total_male, total_female)%>% 
  gather(key = "Sex", value = "number_arrests", 2:3 ) %>%
  group_by(year, Sex) %>% 
  mutate(total_arrest_race = sum(number_arrests) ) %>%
  ggplot()+
  geom_col(aes(x=year, 
               y=number_arrests))+
  #geom_point()+
  facet_wrap(~Sex, scales = "free_y")+
  scale_x_continuous(breaks = seq(1994, 2016, by = 2))+
  labs(x = "Year", 
       y = "Number of Arrest", 
       title = "Total Arrest per Year by Sex")+
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 45))





# Drug Vs Race


# Creating a scatterplot of x=Drugs y=Year with race.  
arrests %>% 
  select(-starts_with("f"), -starts_with("m"), -state_abbr, -agencies,
         -id, -race_agencies, -offense_code, -population, -total_male,
         -total_female, -race_population ) %>% 
  filter(offense_name == "Drug Abuse Violations") %>% 
  mutate(total_race_drug_arrest = white+black+asian_pacific_islander+american_indian) %>%
  gather(key = "Race", value = "arrests_race", 3:6) %>% # collapsing variables to be easier to work with
  ggplot()+
  geom_line(aes(x=year, y=arrests_race, color = Race))


# Suspicion Vs Race
arrests %>% 
  select(-starts_with("f"), -starts_with("m"), -state_abbr, -agencies,
         -id, -race_agencies, -offense_code, -population, -total_male,
         -total_female, -race_population ) %>% 
  filter(offense_name == "Suspicion") %>% 
  mutate(total_race_drug_arrest = white+black+asian_pacific_islander+american_indian) %>%
  gather(key = "Race", value = "arrests_race", 3:6) %>% 
  ggplot()+
  geom_line(aes(x=year, y=arrests_race, color = Race))
#     Wanted to see if this was disproportionate amoungst races. 
#       Answer, no. 


# Trying to see if there is a spike around 9/11, but cannot tell which  month of the 
# year is which. 
arrests %>% 
  mutate(total_arrest = total_male+total_female) %>%
  select(total_arrest, year) %>% 
  filter(year == 2001)

