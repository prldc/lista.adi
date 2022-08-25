# RUN AFTER preprocessing.py

require(tidyverse)
require(data.table)
require(textmineR)
require(openxlsx)
require(reticulate)

 plan <- read.csv('planilha_processada.csv')
plan2 <- plan %>% select(-acordao)
# FILTERING CASES JUDGED BEFORE 2020:

plan2020 <- plan %>% filter(plan$data_julgamento < as.Date('2020-01-01'))

# REMOVING DUPLICATES:

plan2020 <- plan2020 %>% distinct()

# FINDING CITED ADI CASES:

set_lists_to_chars <- function(x) {
  if(class(x) == 'list') {
    y <- paste(unlist(x[1]), sep='', collapse=', ')
  } else {
    y <- x
  }
  return(y)
}

plan2020 <- plan2020 %>% 
  mutate(ac = str_match_all(pattern = "ADI[\\s|-]\\d++(?![\\s|-][MC|QO|AgR|ED])", plan2020$obs))  # Finds all matches of ADI followed by a sequence of numbers and NOT followed by "MC", "QO, "AgR' or "ED".

acordaos_citados <- 1:2136
ac <- tibble(acordaos_citados)
for(i in 1:nrow(plan2020))
{ac[i] = set_lists_to_chars(plan2020$ac[i])}
acordaos_citados <- t(ac[1,])
acordaos_citados <- tibble(acordaos_citados)
plan2020 <- bind_cols(plan2020, acordaos_citados)
plan2020 <- plan2020 %>% select(-ac)

# FILTERING ADI BY WHETHER THEY CHALLENGE STATE OR FEDERAL LAW:

plan2020 <- plan2020 %>%
  mutate(origem = case_when(str_detect(pattern = "(LEG-EST)|(LEG-DIS)", plan2020$legislacao) ~ 'estadual',
                            !str_detect(pattern = "LEG-EST|LEG-DIS", plan2020$legislacao) ~ 'federal'))

# FINDING REMOTE TRIAL SESSIONS:

plan2020 <- plan2020 %>% 
  mutate(virtual = str_detect(plan2020$decisao, "Sessão Virtual"))

# CONVERTING 'ocr' AND 'lista' TO BOOLEAN:

plan2020 <- plan2020 %>%
  mutate(ocr = as.logical(ocr)) %>%
  mutate(lista = as.logical(lista))

# CLASSIFYING OPINIONS BY WHETHER THEY WERE JUDGED REMOTELY ("virtual"), FAST-TRACKED ("lista") OR NEITHER ("tradicional"):


plan2020 <- plan2020 %>%
  mutate(tipo_julgamento = case_when(lista ~ "lista",
                                     virtual ~ "virtual",
                                     T ~ 'tradicional'))

l <- plan2020 %>% filter(tipo_julgamento == "lista") %>% select(-acordao)
p2015 <- plan2020 %>% 
  filter(plan2020$data_julgamento > as.Date('2015-01-01'))

data <- p2015 %>%
  filter(p2015$tipo_julgamento != "virtual") # drops virtuais for plot

data <- p2015 %>%
  mutate(date = as.Date(data_julgamento))
data <- data %>% 
  group_by(year = lubridate::floor_date(date, "year")) 
data$year <- as.factor(data$year)
data$year <- recode_factor(data$year, "2015-01-01" = "2015", "2016-01-01" = "2016",
                          "2017-01-01" = "2017", "2018-01-01" = "2018",
                          "2019-01-01" = "2019")

p<- ggplot(data = data, 
       aes(x = year, group = tipo_julgamento, fill = tipo_julgamento)) +
  geom_bar(position = "stack") 
p + labs(x = "Ano", y = "Número de ADIs")

p<- ggplot(data = data,  aes(x = year, group = tipo_julgamento, fill = tipo_julgamento)) +
  geom_bar()
p<- ggplot(data = data,  aes(x = year, group = tipo_julgamento, fill = tipo_julgamento)) +
  geom_bar(position = "dodge")
p + labs(x = "Ano", y = "Número de ADIs")

# DROPPING 'acordaos_mesmo_sentido', SINCE 'acordaos_citados' CONTAINS MORE INFORMATION:

plan2020 <- plan2020 %>%
  select(-acordaos_mesmo_sentido)


# CLASSIFYING COURT OPINIONS BY PERIOD:

plan2020 <- plan2020 %>% mutate(periodo = 
                                  case_when(plan2020$data_julgamento < as.Date('1995-01-01') ~ 1,
                                            ((plan2020$data_julgamento > as.Date('1994-12-31'))&(plan2020$data_julgamento < as.Date('2003-01-01'))) ~ 2,
                                            ((plan2020$data_julgamento > as.Date('2002-12-31'))&(plan2020$data_julgamento < as.Date('2007-01-01'))) ~ 3,
                                            ((plan2020$data_julgamento > as.Date('2006-12-31'))&(plan2020$data_julgamento < as.Date('2013-01-01'))) ~ 4,
                                            ((plan2020$data_julgamento > as.Date('2012-12-31'))&(plan2020$data_julgamento < as.Date('2021-01-01'))) ~ 5
                                  ))



# EXPORTING TO CSV:

write.csv(plan2020, 'planilha2020_processada.csv', row.names = F)
write.xlsx(plan2020, 'planilha2020_processada.xlsx', row.names = F)

# NOW RUN text_normalizing.py TO NORMALIZE TEXT ENTRIES:



