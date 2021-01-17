################################################
# Student name: Anjuman Alam
# Final Project
# Date due: January 17, 2021
#
# Attribution statement:
# 2. I did this project with help from the book and the professor and these Internet sources:
#

dev.off() # Clear the graph window
rm(list=ls()) # Clear user objects from the environment
cat('\014')   # Clear the console

# import packages
library(tidyverse)
library(ggplot2)
library(kernlab)
library(arules)
library(e1071)
library(arulesViz)
library(maps)
library(mapproj)
library(ggmap)
library(caret)

# User-defined functions



#---- DATA COLLECTION ----
#import data-set of results from poll on why Americans don't vote
nonvoters_data <- read_csv("nonvoters_data.csv")

#view information on data-set
summary(nonvoters_data)
glimpse(nonvoters_data)

#rename columns for clarification
nonvoters_data %>% 
  rename(
    Q1 = is_citizen,
    Q2_1 = voting_important,
    Q2_2 = jury_important,
    Q2_3 = follow_politics_important,
    Q2_4 = display_flag_important,
    Q2_5 = participate_in_census_important,
    Q2_6 = know_poa_important,
    Q2_7 = support_military_important,
    Q2_8 = respect_opposing_opinions_important,
    Q2_9 = believe_god_important,
    Q2_10 = protest_govt_if_wrong_important,
    Q3_1 = systemic_racism_problem,
    Q3_2 = systemic_racism_police_over_violent_protest,
    Q3_3 = society_soft_feminine,
    Q3_4 = media_prioritize_money_over_truth,
    Q3_5 = political_parties_dont_care_me,
    Q3_6 = change_speech_with_times,
    Q4_1 = impact_of_dc_officials,
    Q4_2 = impact_of_state_officials,
    Q4_3 = impact_of_city_officials,
    Q4_4 = impact_of_news,
    Q4_5 = impact_of_wall_st,
    Q4_6 = impact_of_law_enforcement,
    Q5 = election_importance_2020,
    Q6 = current_elected_office_similarity_to_me,
    Q7 = change_govt_structure,
    Q8_1 = trust_in_presidency,
    Q8_2 = trust_in_congress,
    Q8_3 = trust_in_sc,
    Q8_4 = trust_in_cdc,
    Q8_5 = trust_in_election_officials,
    Q8_6 = trust_in_fbi_cia,
    Q8_7 = trust_in_news,
    Q8_8 = trust_in_police,
    Q8_9 = trust_in_usps,
    Q9_1 = prefer_democracy_governance,
    Q9_2 = prefer_expert_governance,
    Q9_3 = prefer_leader_without_congress_governance,
    Q9_4 = prefer_army_rule_governance,
    Q10_1 = have_long-term_disability,
    Q10_2 = have_chronic_illness,
    Q10_3 = been_unemployed_mt_one_yr,
    Q10_4 = been_evicted_within_year,
    Q11_1 = lost_job_due_to_covid,
    Q11_2 = tested_positive_covid,
    Q11_3 = friend_family_positive_covid,
    Q11_4 = friend_family_die_covid,
    Q11_5 = worry_paying_living_expenses,
    Q11_6 = quit_job_care_for_family,
    Q14 = republican_attitude_toward_me,
    Q15 = democrat_attitude_toward_me,
    Q16 = easy_difficult_vote_national_elections,
    Q17_1 = inperson_machine_safe_from_fraud,
    Q17_2 = inperson_ballot_safe_from_fraud,
    Q17_3 = mail_ballot_safe_from_fraud,
    Q17_4 = electronic_vote_safe_from_fraud,
    Q18_1 = Sepal.Width,
    Q18_2 = ,
    Q18_3 = Sepal.Width,
    Q18_4 = ,
    Q18_5 = Sepal.Width,
    Q18_6 = ,
    Q18_7 = Sepal.Width,
    Q18_8 = ,
    Q18_9 = Sepal.Width,
    Q18_10 = ,
    Q19_1 = Sepal.Width,
    Q19_2 = ,
    Q19_3 = Sepal.Width,
    Q19_4 = ,
    Q19_5 = Sepal.Width,
    Q19_6 = ,
    Q19_7 = Sepal.Width,
    Q19_8 = ,
    Q19_9 = Sepal.Width,
    Q19_10 = ,
    Q20 = ,
    Q21 = ,
    Q22 = ,
    Q23 = ,
    Q24 = ,
    Q25 = ,
    Q26 = ,
    Q27_1 = Sepal.Width,
    Q27_2 = ,
    Q27_3 = Sepal.Width,
    Q27_4 = ,
    Q27_5 = Sepal.Width,
    Q27_6 = ,
    Q28_1 = Sepal.Width,
    Q28_2 = ,
    Q28_3 = Sepal.Width,
    Q28_4 = ,
    Q28_5 = Sepal.Width,
    Q28_6 = ,
    Q28_7 = Sepal.Width,
    Q28_8 = ,
    Q29_1 = Sepal.Width,
    Q29_2 = ,
    Q29_3 = Sepal.Width,
    Q29_4 = ,
    Q29_5 = Sepal.Width,
    Q29_6 = ,
    Q29_7 = Sepal.Width,
    Q29_8 = ,
    Q29_9 = Sepal.Width,
    Q29_10 = ,
    Q30 = ,
    Q31 = ,
    Q32 = ,
    Q33 = ,
    ppage = age
  )

#---- DATA MUNGING ----


#clean data to include only people we have voting history of



#separate voters into groups: nonvoters, sometimes voters, and always voters



#---- DATA EXPLORATION ----

#


#---- DATA MODELING ----

#



#---- INSIGHTS ----

#











