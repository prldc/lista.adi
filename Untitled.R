library(tidyverse)
library(magrittr)

plan <- read_csv("planilha2020_processada*.csv")
plan %<>% select(-acordao)

p2015 <- plan %>% 
  filter(plan$data_julgamento > as.Date('2015-01-01'))

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
p + labs(x = "Ano", y = "Número de ADIs", fill = "Tipo de julgamento", title = "Número de julgamentos por tipo")


t<- p2015 %>%
  mutate(month = format(data_julgamento, "%m"), year = format(data_julgamento, "%Y")) %>%
  group_by(month, year) %>%
  count(tipo_julgamento)


t2 <- t %>% ggplot(aes(x = year)) + geom_bar(aes(fill = tipo_julgamento), position = "dodge")
t2 + labs(fill = "Tipo de julgamento", title = "Número de julgamentos por tipo", x="Ano")


quali <- read_delim("quali.csv", delim = ";", 
                   escape_double = FALSE, col_types = cols(inicio = col_time(format = "%H:%M:%S"), 
                                                           voto_relator_inicio = col_time(format = "%H:%M:%S"), 
                                                           voto_relator_fim = col_time(format = "%H:%M:%S"), 
                                                           voto_relator = col_double(), fim = col_time(format = "%H:%M:%S"), 
                                                           total = col_double()), trim_ws = TRUE)

p <- ggplot(data = qual,
            mapping = aes(x = total))
p + geom_histogram(bins = 30)

ggplot(qual, aes(voto_relator, total)) +
  geom_boxplot()

boxplot(quali$total, horizontal=T, col="steelblue", xlab="Duração em segundos", ylab="Julgamento")
boxplot(quali$voto_relator, horizontal=T, col="steelblue",xlab="Duração em segundos", ylab="Voto do relator")

text(x = 1:nrow(data_means),                               # Add text to plot
     y = data_means$x - 0.15,
     labels = paste("Mean:", round(data_means$x, 1)),
     col = "red")

library(reshape2)
dat.m <- melt(qual,id.vars='nome', measure.vars=c('voto_relator','total'))
library(ggplot2)
p <- ggplot(dat.m) +
  geom_boxplot(aes(x=ID, y=value))

p <- ggplot(qual, aes(x="", y=qual$total)) + 
  geom_boxplot()
