require(quanteda)
require(quanteda.textmodels)
require(caret)
require(tidyverse)
require(readtext)
require(tidytext)
require(viridis)
require(textmineR)
library(openxlsx)
require(e1071)

# CREATING THE NAIVE BAYES CLASSIFIER:

stop <- c("moreira", "alves", "tribunal", "britto", "ilmar",
          "gallotti", "galvão", "silveira", "sydney", "néri",
          "dr", "cezar", "velloso", "lewandowski", "ministro",
          "ministra", "paulo", "celso", "mello","luiz", 
          "roberto", "mendes", "sepúlveda", "peluso", "carlos",
          "ellen", "pertence", "dias", "aurélio", "marco", "ainda", "medida", "assim", "sobre", 
          "assim", "2º", "ii", "i", "1º", "mp", "sob", "porque", "teor", "nº", "conforme", 
          "inteiro", "gilmar", "moraes", "alexandre", "rosa", "lúcia", "cármen", "fachin", 
          "toffoli", "barroso", "pode", "fux", "página", "senha", "código", "2.200-2", 
          "chaves", "eletrônico", "endereço", "acessado", "documento", "REQTE", "ADV", 
          "DOS", "DO", "A", "MIN", "DA", "DAS", "INTDO", "Acórdão", "RELATORA","PLENÁRIO", 
          "RELATOR","INCONSTITUCIONALIDADE","DIRETA", "AÇÃO", "DE","EMENTA", "é", "e", "ser", 
          "assinado", "digitalmente", "art", "número", "icp-brasil", "portal", 
          "portal_autenticacao", "www_jus", 
          "autenticacao", "br_portal", "www", "http_www", "ministério_público", "jus_br",
          "autenticacao_autenticardocumento", "autenticardocumento", "autenticacao_autenticardocumento, 
          autenticardocumento", "autenticardocumento_asp", "net", "hdl", "https", "net_https", "https_hdl",
          "santa", "catarina", "santa_catarina", "sc", "paraíba", "ceará", "rondônia", "amapá", "mato", "grosso", 
          "mt", "mato_grosso", "handle_handle", "re_re", "norte", "grande_norte", "grande" , "rn", 
          "santo", "espírito", "espírito_santo", "es", "minas", "minas_gerais", "gerais", "mg", "sul", "grande_sul",
          "rs", "tocantins", "amazonas", "xx", "pp", "a", "b", "c", "d", "f", "g", "h", "j", "k", "l", "m",
          "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "rj", "i", "ii", "iii", "iv", "v",
          "vi", "vii", "viii", "ix", "xi", "xii", "xiii", "ba", "ce", "ac", "am", "rr", "pa", "ap", "xiv",
          "to", "ma", "pi", "pe", "al", "se", "sp", "pr", "go", "la", "ex", "ª")

plan2020_text <- readtext("planilha2020_processada*.csv", text_field = "acordao", docid_field="nome")# Indicates the column with the full extent of the judicial opinion as the text field.
# planilha202_processada_ <- read_csv("~/R Projects/stf_text_mining/planilha2020_processada*.csv")
# x <- plan2020_text %>% filter(nome %in% planilha2020_processada_$nome)
p2015b <- plan2020_text %>% 
  filter(plan2020_text$data_julgamento > as.Date('2015-01-01'))

plan2020_text <- plan2020_text %>%  filter(data_julgamento > as.Date('2015-01-01'))

corpus <- corpus(plan2020_text$acordao)
docnames(corpus) <- plan2020_text$nome

set.seed(pi)
# id_train <- sample(1:1404, 702, replace = FALSE)
id_train <- sample(1:452, 226, replace = FALSE)
head(id_train, 10)

# create docvar with ID
corpus$id_numeric <- 1:ndoc(corpus)

# get training set
dfmat_training <- corpus_subset(corpus, id_numeric %in% id_train) %>%
  dfm(remove = c(stop, stopwords('portuguese')), case_insensitive = T)


# get test set (documents not in id_train)
dfmat_test <- corpus_subset(corpus, !id_numeric %in% id_train) %>%
  dfm(remove = c(stop, stopwords('portuguese')), case_insensitive = T)

tmod_nb <- textmodel_nb(dfmat_training, dfmat_training$tipo_julgamento)
summary(tmod_nb)

dfmat_matched <- dfm_match(dfmat_test, features = featnames(dfmat_training))

actual_class <- dfmat_matched$tipo_julgamento
predicted_class <- predict(tmod_nb, newdata = dfmat_matched)
tab_class <- table(actual_class, predicted_class)
tab_class

(cM <- confusionMatrix(tab_class, mode = "everything"))


# DUMMY

plan2020_text$dummy <- sample(0:1, size = nrow(plan2020_text), replace = TRUE, prob = c((sum(plan2020_text$lista)/nrow(plan2020_text)), 1 - (sum(plan2020_text$lista)/nrow(plan2020_text))))
corpus <- corpus(plan2020_text)
docnames(corpus) <- plan2020_text$nome

set.seed(pi)
# id_train <- sample(1:1404, 702, replace = FALSE)
id_train <- sample(1:419, 210, replace = FALSE)
head(id_train, 10)

# create docvar with ID
corpus$id_numeric <- 1:ndoc(corpus)

# get training set
dfmat_training <- corpus_subset(corpus, id_numeric %in% id_train) %>%
  dfm(remove = c(stop, stopwords('portuguese')), case_insensitive = T)


# get test set (documents not in id_train)
dfmat_test <- corpus_subset(corpus, !id_numeric %in% id_train) %>%
  dfm(remove = c(stop, stopwords('portuguese')), case_insensitive = T)

tmod_nb <- textmodel_nb(dfmat_training, dfmat_training$dummy)
summary(tmod_nb)

dfmat_matched <- dfm_match(dfmat_test, features = featnames(dfmat_training))

actual_class <- dfmat_matched$dummy
predicted_class <- predict(tmod_nb, newdata = dfmat_matched)
tab_class <- table(actual_class, predicted_class)
tab_class

(cM <- confusionMatrix(tab_class, mode = "everything"))

# write.table(cMt, file = "cM.txt", sep = ",", quote = FALSE, row.names = F)
# cMt <- as.table(cM)

tocsv <- data.frame(cbind(t(cM$overall),t(cM$byClass)))

# You can then use
write.csv(tocsv,file="file.csv")
