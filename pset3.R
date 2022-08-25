library(tidyverse)

nigeria <- read.csv('nigeria.csv')

# 3a How many respondents answered “yes” to the randomized response question about their connections with armed groups? 
# How many respondents answered “no” to this question?

nigeria %>% 
  count(rr.q1 == 1) # The answer is 831 "Yes", 1600 "No".

#OR

table(nigeria$rr.q1) # Same result.

#3b  How many female respondents answered “yes” to the question?
# How many married female respondents answered “no” to the question?

nigeria %>%
  filter(cov.female == 1, rr.q1 == 1) %>%  # filter by gender and positive answer, total is 334.
  count() 

nigeria %>%
  filter(cov.female == 1, cov.married == 1, rr.q1 == 0) %>%  # filter by gender, negative answer and marital status, total is 468.
  count() 

#3c Now we compute the posterior probability of a respondent’s true answer given the data.
# i. Take the 1569th respondent in the data set. What is the observed answer?

nigeria[1569,] # No connection, female, single.

# ii. Using Bayes’ theorem, derive an expression for the posterior probability that this respondent in fact held social connections with armed group members.

# Using Bayes’ theorem, derive an expression for the posterior probability that this respondent in fact held social connections with armed group members.

nigeria %>%
  filter(cov.female == 1, cov.married == 0, rr.q1 == 0) %>%  # filter by female, single and negative answer.
  count() 

# nigeria %>%
#   filter(cov.female == 1, cov.married == 0) %>%  # filter by female, single.
#   count() 
# 
# nigeria %>%
#   filter(rr.q1 == 0) %>%  # filter by female, single and negative answer.
#   count() 

nigeria <- nigeria %>% 
  mutate(post_0.5 = case_when(rr.q1 == 0 ~ 1/6,
                           T ~ 5/6)) %>% 
  mutate(post_0.8 = case_when(rr.q1 == 0 ~ 4/9,
                           T ~ 20/21)) %>% 
  mutate(post_0.01 = case_when(rr.q1 == 0 ~ 1/496,
                              T ~ 5/104)) %>% 
  mutate(post_0 = 0) %>% 
  mutate(post_1 = 1)

mean(nigeria$post_0.5)
mean(nigeria$post_0.8)
mean(nigeria$post_0.01)
mean(nigeria$post_0)
mean(nigeria$post_1)

nigeria <- read.csv('nigeria.csv')


male <- nigeria %>%
  filter(cov.female == 0, cov.married == 0) 

male <- male %>% 
  mutate(post_0.5 = case_when(rr.q1 == 0 ~ 1/6,
                              T ~ 5/6)) %>% 
  mutate(post_0.8 = case_when(rr.q1 == 0 ~ 4/9,
                              T ~ 20/21)) %>% 
  mutate(post_0.01 = case_when(rr.q1 == 0 ~ 1/496,
                               T ~ 5/104)) %>% 
  mutate(post_0 = 0) %>% 
  mutate(post_1 = 1)

mean(male$post_0.5)
mean(male$post_0.8)
mean(male$post_0.01)
mean(male$post_0)
mean(male$post_1)
