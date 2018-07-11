installed.packages("ggplot2")
library(ggplot2)
iris

#單變數：類別型
ggplot(data = iris, aes(x =Species )) +
  geom_bar(fill = "red", colour = "black")
#單變數：連續型
ggplot(data = iris, aes(x = Sepal.Length)) +
  geom_histogram()
#雙變數：連續VS連續
ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width))+
  geom_point()
#雙變數：離散VS連續
ggplot(data=iris, aes(x=Species, y=Petal.Length))+
  geom_boxplot()
#unknown
library(GGally)
library(scales)
library(memisc)

