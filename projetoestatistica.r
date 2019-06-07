# Flavio Braga e Silva Junior (Fbsj)
# Lucas Asafe Virginio do Nascimento (Lavn)
# Yuri Valenca Cunha (Yvc)



# setwd("C://Users/Desktop/estatistica/")


library(magrittr) # need to run every time you start R and want to use %>%
library(dplyr)

#Importando Planilha
planilha <- read.csv(file = 'data/PlanilhaGOT.csv', header = TRUE)

#Funcao para obter modas(questoes 2 e 3)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#Funcao para pegar notas maior ou igual a 9 (questao 4)
getNotesUpperTo9 <- function(df) {
  for (note in range(length(planilha$Episodio))){
    if (planilha$Nota(note) >= 9){
      print(planilha$Episodio[note])
    }
  } 
}


# Questao 1 - Apenas usando print
  print(planilha, quote = TRUE, row.names = FALSE)

#------------------------------------------------------------------------------------------
# Questao 2 - Nesta questao, foram usadas as funcoes de media, desvio padrao uma funcao de moda criada acima
  cat(paste("\nA media das notas eh : ", mean(planilha$Nota), "\nO desvio padrao das notas eh: ", sd(planilha$Nota), "\nA moda eh: ", getmode(planilha$Nota)))

#------------------------------------------------------------------------------------------
# Questao 3 - Essa seguiu a mesma linha de solucao da questao 2
  cat(paste("\nA media das Audiencias eh : ", mean(planilha$Audiencia.Em.milhoes.), "\nO desvio padrao das Audiencias eh: ", sd(planilha$Audiencia.Em.milhoes.), "\nA moda eh: ", getmode(planilha$Audiencia.Em.milhoes.)))

#------------------------------------------------------------------------------------------
# Questao 4 
  dfformatadoquarto <- planilha[planilha[, 3]>=9, ]
  respostaQuartaQuestao <- dfformatadoquarto[, 2]
  cat("O nome dos episodios cuja nota eh maior ou igual a 9 eh: \n")
  print(respostaQuartaQuestao, quote = TRUE, row.names = FALSE)

#------------------------------------------------------------------------------------------
# Questao 5

  #funcao para obtencao do nome do episodio com nota minima
  minimo <- function(temporada) {
    dftempo <- planilha[planilha[, 1] == temporada, ]
    mintempo <- min(dftempo[,3], na.rm=T)
    nametempo <- subset(dftempo, dftempo$Nota == mintempo)
    cat ("Nome do episodio minimo da temporada", temporada) 
    # print(nametempo, quote = TRUE, row.names = FALSE)
    return(nametempo['Episodio'])
  }
  
  #funcao para obtencao do nome do episodio com nota maxima
  maximo <- function(temporada) {
    dftempo <- planilha[planilha[, 1] == temporada, ]
    mintempo <- max(dftempo[,3], na.rm=T)
    nametempo <- subset(dftempo, dftempo$Nota == mintempo)
    cat (paste("O nome do episodio com maior nota da temporada:", temporada," Eh: \n")) 
    # print(nametempo, quote = TRUE, row.names = FALSE)
    return(nametempo['Episodio'])
  }
  
  
  for (i in 1:8) {
    print(minimo(i))
    print('----------------------------------------------------------------------')
    print(maximo(i))
    print('----------------------------------------------------------------------')
  }
  
#------------------------------------------------------------------------------------------
#Questão 06
  sdTemp <- function(dataframe) {
    lista <- list()
    for (i in min(dataframe$Temporada):max(dataframe$Temporada)){
      lista[i] <- subset(dataframe, dataframe$Temporada == i)
      return(data)
    }
  }  
#------------------------------------------------------------------------------------------
# Questao 7 - Usando as bibliotecas importadas acima, eu pego apenas as linhas que contem (Brienne) e retorno a media da coluna de medias
  dfformatadosetimo <- planilha[planilha[, 4] %like% "Brienne",]
  respostaSetimaQuestao <-  mean(dfformatadosetimo[, 3])
  cat ("A media das notas dos episodios em que Brienne of Tarth(Gwendoline Christie) participa eh:" , respostaSetimaQuestao)