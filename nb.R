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

# SEM A LIMPEZA

plan2020_text <- readtext("planilha2020_processada.csv", text_field = "acordao")# Indicates the column with the full extent of the judicial opinion as the text field.
plan2020_text <- plan2020_text %>%  filter(data_julgamento > as.Date('2015-01-01'))
plan2020_text <- plan2020_text %>%  filter(virtual == F)
corpus <- corpus(plan2020_text)
docnames(corpus) <- plan2020_text$nome
set.seed(pi)
id_train <- sample(1:413, 330, replace = FALSE) #80/20 split
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

draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('Matriz de confusão', cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, 'Lista', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, 'Tradicional', cex=1.2)
  text(125, 370, 'Categoria real', cex=1.3, srt=90, font=2)
  text(245, 450, 'Categoria prevista', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, 'Lista', cex=1.2, srt=90)
  text(140, 335, 'Tradicional', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "Detalhes", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[8]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[8]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.2, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 2), cex=1.2)
  text(70, 35, names(cm$overall[2]), cex=1.2, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 2), cex=1.2)
}  

draw_confusion_matrix(cM)

# COM DADOS LIMPOS

plan2020_text <- readtext("planilha2020_processada.csv", text_field = "acordao")# Indicates the column with the full extent of the judicial opinion as the text field.

# NÃO REMOVER ESSAS DUAS LINHAS OU NÃO RODA
planilha2020_processada_ <- read_csv("planilha2020_processada*.csv")
plan2020_text <- plan2020_text %>% filter(nome %in% planilha2020_processada_$nome)
plan2020_text <- plan2020_text %>%  filter(data_julgamento > as.Date('2015-01-01'))
plan2020_text <- plan2020_text %>%  filter(virtual == F)
corpus <- corpus(plan2020_text)
docnames(corpus) <- plan2020_text$nome
set.seed(pi)
id_train <- sample(1:315, 252, replace = FALSE) #80/20 split
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
draw_confusion_matrix(cM)
