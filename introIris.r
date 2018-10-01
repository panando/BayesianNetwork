library(bnlearn)
data(iris)

#Some exercices relative to the iris dataset to get used to using R

#?iris
#summary(iris)

is.setosa = (iris[,"Species"] == "setosa")
is.versi = (iris[,"Species"] == "versicolor")
is.virgi = !is.setosa & !is.versi

#summary(iris[is.setosa, "Sepal.Length"])

setosa = iris[iris$Species == "setosa",]
versi = iris[iris$Species == "versicolor",]
virgi = iris[iris$Species == "virginica",]

par(mfrow = c(1,3))

#Histograms comparing their lengths by species
barplot(setosa[,"Sepal.Length"], main = "Setosa sepal length")
barplot(versi[,"Sepal.Length"], main = "Versicolor sepal length")
barplot(virgi[,"Sepal.Length"], main = "Virginica sepal length")

#Boxplots comparing the same value
boxplot(setosa[,"Sepal.Length"], main = "Setosa sepal length")
boxplot(versi[,"Sepal.Length"], main = "Versicolor sepal length")
boxplot(virgi[,"Sepal.Length"], main = "Virginica sepal length")