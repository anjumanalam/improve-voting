################################################
# Student name: Anjuman Alam
# Final Project
# Date due: January 17, 2021
#
# Attribution statement:
# 2. I did this project with help from the book and the professor and these Internet sources:
# https://www.statmethods.net/management/operators.html
# https://www.guru99.com/r-data-frames.html#:~:text=We%20can%20create%20a%20dataframe,the%20name%20of%20the%20variables.
# https://www.datanovia.com/en/lessons/rename-data-frame-columns-in-r/

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
# import data-set of results from poll on why Americans don't vote
nonvoters_data <- read_csv("nonvoters_data.csv")

# view information on data-set
summary(nonvoters_data)
glimpse(nonvoters_data)
hist(nonvoters_data$Q1)

# rename columns for clarification
# nonvoters_data %>%
#   rename(
#     Q1 = is_citizen,
#     Q2_1 = voting_important,
#     Q2_2 = jury_important,
#     Q2_3 = follow_politics_important,
#     Q2_4 = display_flag_important,
#     Q2_5 = participate_in_census_important,
#     Q2_6 = know_poa_important,
#     Q2_7 = support_military_important,
#     Q2_8 = respect_opposing_opinions_important,
#     Q2_9 = believe_god_important,
#     Q2_10 = protest_govt_if_wrong_important,
#     Q3_1 = systemic_racism_problem,
#     Q3_2 = systemic_racism_police_over_violent_protest,
#     Q3_3 = society_soft_feminine,
#     Q3_4 = media_prioritize_money_over_truth,
#     Q3_5 = political_parties_dont_care_me,
#     Q3_6 = change_speech_with_times,
#     Q4_1 = impact_of_dc_officials,
#     Q4_2 = impact_of_state_officials,
#     Q4_3 = impact_of_city_officials,
#     Q4_4 = impact_of_news,
#     Q4_5 = impact_of_wall_st,
#     Q4_6 = impact_of_law_enforcement,
#     Q5 = election_importance_2020, #remove
#     Q6 = current_elected_office_similarity_to_me,
#     Q7 = change_govt_structure,
#     Q8_1 = trust_in_presidency,
#     Q8_2 = trust_in_congress,
#     Q8_3 = trust_in_sc,
#     Q8_4 = trust_in_cdc,
#     Q8_5 = trust_in_election_officials,
#     Q8_6 = trust_in_fbi_cia,
#     Q8_7 = trust_in_news,
#     Q8_8 = trust_in_police,
#     Q8_9 = trust_in_usps,
#     Q9_1 = prefer_democracy_governance,
#     Q9_2 = prefer_expert_governance,
#     Q9_3 = prefer_leader_without_congress_governance,
#     Q9_4 = prefer_army_rule_governance,
#     Q10_1 = have_long-term_disability,
#     Q10_2 = have_chronic_illness,
#     Q10_3 = been_unemployed_mt_one_yr,
#     Q10_4 = been_evicted_within_year, #remove
#     Q11_1 = lost_job_due_to_covid, #remove
#     Q11_2 = tested_positive_covid, #remove
#     Q11_3 = friend_family_positive_covid, #remove
#     Q11_4 = friend_family_die_covid, #remove
#     Q11_5 = worry_paying_living_expenses, #remove
#     Q11_6 = quit_job_care_for_family, #remove
#     Q14 = republican_attitude_toward_me,
#     Q15 = democrat_attitude_toward_me,
#     Q16 = easy_difficult_vote_national_elections,
#     Q17_1 = inperson_machine_safe_from_fraud,
#     Q17_2 = inperson_ballot_safe_from_fraud,
#     Q17_3 = mail_ballot_safe_from_fraud,
#     Q17_4 = electronic_vote_safe_from_fraud,
#     Q18_1 = vote_incorrect_id,
#     Q18_2 = vote_cannot_find_polls,
#     Q18_3 = vote_miss_reg_deadline,
#     Q18_4 = vote_cannot_physically_access_polls,
#     Q18_5 = vote_no_assistance_fill_ballot,
#     Q18_6 = vote_cast_prov_ballot,
#     Q18_7 = vote_cannot_get_work_off,
#     Q18_8 = vote_waited_more_hr,
#     Q18_9 = vote_registered_but_name_not_listed,
#     Q18_10 = vote_absentee_ballot_late,
#     Q19_1 = outreach_from_candidates,
#     Q19_2 = unbiased_candidate_info,
#     Q19_3 = election_day_holiday,
#     Q19_4 = automatic_registration,
#     Q19_5 = automatic_mail_ballot_delivery,
#     Q19_6 = vote_in-person_b4_election_day,
#     Q19_7 = register_vote_same_day,
#     Q19_8 = vote_by_phone_online,
#     Q19_9 = increase_candidate_options,
#     Q19_10 = other_reason_to_increase_voting, #remove?
#     Q20 = is_registered,
#     Q21 = is_voting_nov2020, #remove
#     Q22 = why_not_registered,
#     Q23 = president_support_2020, #remove
#     Q24 = preferred_voting_method,
#     Q25 = how_close_follow_2020elections, #remove
#     Q26 = self_describe_voter_category,
#     Q27_1 = voted_congress_2018,
#     Q27_2 = voted_president_2016,
#     Q27_3 = voted_congress_2014,
#     Q27_4 = voted_president_2012,
#     Q27_5 = voted_congress_2010,
#     Q27_6 = voted_president_2008,
#     Q28_1 = voting_important_civic_duty,
#     Q28_2 = excited_about_candidate,
#     Q28_3 = disliked_candidate,
#     Q28_4 = support_political_party,
#     Q28_5 = specific_issue,
#     Q28_6 = enjoy_voting,
#     Q28_7 = voting_easy,
#     Q28_8 = other_reason_decided_to_vote, #remove?
#     Q29_1 = not_voted_not_like_candidates,
#     Q29_2 = not_voted_vote_not_matter,
#     Q29_3 = not_voted_nothing_change,
#     Q29_4 = not_voted_system_cannot_fix,
#     Q29_5 = not_voted_unavailable,
#     Q29_6 = not_voted_unsure_eligibility,
#     Q29_7 = not_voted_personal_important_issues_not_discussed,
#     Q29_8 = not_voted_candidates_same,
#     Q29_9 = not_voted_no_belief_voting,
#     Q29_10 = other_reason_not_voted, #remove?
#     Q30 = party_affiliation,
#     Q31 = is_strong_repulican,
#     Q32 = is_strong_democrat,
#     Q33 = is_closer_to_rep_or_dem,
#     ppage = age
#   )

#---- DATA MUNGING ----

# remove information related to 2020 election/Covid-19; we are analyzing general reason why Americans do not vote
clean_data <- select(nonvoters_data, -c(Q1, #all survey takers in data-set are citizens
                                        Q5, #2020
                                        Q10_4, #related to COVID-19 / "past year"
                                        Q11_1, #Q11 related to 2020
                                        Q11_2,
                                        Q11_3,
                                        Q11_4,
                                        Q11_5,
                                        Q11_6,
                                        Q21,   #2020 elections related
                                        Q23,   #2020 elections related
                                        Q25))  #2020 elections related

#take a peak at clean_data to ensure columns are deleted
glimpse(clean_data)


# separate voters into groups: nonvoters, always voters, sometimes voters
nonvoters <- subset(clean_data, clean_data$voter_category == "rarely/never")
alwaysvoters <- subset(clean_data, clean_data$voter_category == "always")
sometimesvoters <- subset(clean_data, clean_data$voter_category == "sporadic")

#create subset we want to study: people who rarely vote and sometimes vote
survey_data <- subset(clean_data, clean_data$voter_category != "always")

#---- DATA EXPLORATION ----

#


#---- DATA MODELING ----

#




#plot()


#---- INSIGHTS ----

#











