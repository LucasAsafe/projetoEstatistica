# Autor: Samuel Lopes - Virtu-e


getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getNotesUpperTo9 <- function(df) {
  for (note in range(length(dataframe$Episodio))){
    if (dataframe$Nota(note) >= 9){
      print(dataframe$Episodio[note])
    }
  } 
}


##Questão 06
sdTemp <- function(dataframe) {
  lista <- list()
  for (i in min(dataframe$Temporada):max(dataframe$Temporada)){
    lista[i] <- subset(dataframe, dataframe$Temporada == i)
  return(data)
  }
}


dataframe = read.csv("planilhagot.csv", header = TRUE)
cat(paste("\nA média das notas é : ", mean(dataframe$Nota), "\nO desvio padrão das notas é: ", sd(dataframe$Nota), "\nA moda é: ", getmode(dataframe$Nota)))
cat(paste("\nA média das Audiência é : ", mean(dataframe$Audiencia.Em.milhoes.), "\nO desvio padrão das Audiência é: ", sd(dataframe$Audiencia.Em.milhoes.), "\nA moda é: ", getmode(dataframe$Audiencia.Em.milhoes.)))

data = sdTemp(dataframe)

length(dataframe$Temporada)
