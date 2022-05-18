data <- read.csv(file="data_2020-2015.csv", sep=";")

print(nrow(data))

index <- c()

for (i in 1:nrow(data)){
  if (data[i, 5] == "Accident Leger"){
    index[i] <- c(1)
  }
  else if (data[i, 5] == "Accident grave non mortel"){
    index[i] <- c(2)
  }
  else if (data[i, 5] == "Accident mortel"){
    index[i] <- c(3)
  }
}

print(index[35])
print(length(index))

data <- cbind(data, index)
head(data)

nb_accident_par_type <- table(factor(index, levels = 1:3))
'
nb_accident_par_annee <- table(factor(index, levels = 1:3))
nb_accident_par_categorie <- table(factor(index, levels = 1:3))
'

print(nb_accident_par_type)
print(data %>% select(Type_Accident, index) %>% group_by(Type_Accident) %>% summarize(nb_index_par_type = sum(index)))


library(ggplot2)

ggplot(data, aes(Type_Accident, index, fill=index)) + geom_bar(stat="identity") + labs(fill = "Les types d'accidents")

'
ggplot(data, aes(Type_Accident, y=index, fill=Type_Accident)) + geom_bar(stat="identity") +
  geom_text(aes(label=index), vjust=0)
'

ggplot(data, aes(Annee, index, fill=Annee)) + geom_bar(stat="identity") + labs(fill = "Année")

ggplot(data, aes(Categorie_Vehicule, index, fill=Categorie_Vehicule)) + geom_bar(stat="identity") + 
  labs(fill = "Catégorie de véhicule")

ggplot(data, aes(Age_Vehicule, index, fill=Age_Vehicule)) + geom_bar(stat="identity") + 
  labs(fill = "Age du véhicule")

#Etudier les accidents en fonction de l'âge des voitures
ggplot(data, aes(Age_Vehicule, Type_Accident, fill=Age_Vehicule)) + geom_bar(stat="identity") + 
  labs(fill = "Age du véhicule")

