# Load the iris data set 
data(iris) 
library(ggplot2)
library(plotly)
# Perform MDS analysis 
mds_iris <- cbind(as.data.frame(cmdscale(dist(iris[,1:4]))), iris[,5])
colnames(mds_iris) <- c("x", "y", "variety")

ggplot(data = mds_iris, aes(x = x, y = y, color = variety)) + geom_point()

mds_iris_3d <- cbind(as.data.frame(cmdscale(dist(iris[,1:4]), k= 3)), iris[,5])
colnames(mds_iris_3d) <- c("x", "y", "z", "variety")
attach(mds_iris_3d)
library(lattice)
cloud(z ~ x + y, data = mds_iris_3d, scales = list(arrows = FALSE), group = variety)


fig <- ggplot(data = mds_iris, aes(x = x, y = y, z = z, color = variety)) + geom_point()
ggplotly(fig)


#measure of goodness of fit
# Load the Iris dataset
library(datasets)
data(iris)

# Extract the features (sepal length, sepal width, petal length, petal width)
X <- iris[, 1:4]

# Calculate the distance matrix using Euclidean distance
distance_matrix <- dist(X, method = "euclidean")

# Convert the distance matrix to a data frame for better visualization
distance_df <- as.data.frame(as.matrix(distance_matrix))
#rownames(distance_df) <- colnames(distance_df) <- iris$Species

# Print the distance matrix
print(distance_df)
X <- as.matrix(distance_df)
A <- (X)^2/(-2)
mat <- diag(rep(1,150)) - (rep(1,150)%*%t(rep(1,150)))/150
B <- H%*%A%*%H
lambda<- eigen(B)$values
#goodness of fit measure
a12 <- (lambda[1]+lambda[2])/(sum(abs(lambda)))*100
a22 <- (lambda[1]^2+lambda[2]^2)/(sum((lambda)^2))*100


#PCA vs MDS
#PCA
library(dplyr)
library(ggplot2)
library(gridExtra)
data(iris) 
mydata <- select(iris,c(1,2,3,4))
cor(mydata)
mean(cor(mydata))
#pricipal component
PCA <- princomp(mydata)
PCA$loadings
PC = PCA$scores
View(PC)
cor(PC)
Species <- iris$Species
PComp <- cbind(as.data.frame(PC), Species)

PComp
pca_plot <- ggplot(data = PComp) + geom_point(aes(x = Comp.1, y = Comp.2, col = Species))
pca_plot



# Perform MDS analysis 
mds_iris <- cbind(as.data.frame(cmdscale(dist(iris[,1:4]))), iris[,5])
colnames(mds_iris) <- c("x", "y", "Species")

ggplot(data = mds_iris, aes(x = x, y = y, color = Species)) + geom_point()

gridExtra::grid.arrange(pca_plot, mds_plot, nrow = 1) 
combined_plot <- grid.arrange(pca_plot, mds_plot, nrow = 1, top = "PCA vs MDS")

#shephard diagram
library(vegan)
library(tidyverse)
library(vegan)
set.seed(2)
community_matrix=read.csv("Data.csv", header = TRUE)

example_NMDS=metaMDS(community_matrix, k=2)
example_NMDS=metaMDS(community_matrix,k=2,trymax=100)

stressplot(example_NMDS)
plot(example_NMDS)
# Analyze the structure of the stressplot
# Notice there's an x, y and yf list
str(stressplot(example_NMDS))

# Create a tibble that contains the data from stressplot
df <- tibble(x = stressplot(example_NMDS)$x,
             y = stressplot(example_NMDS)$y,
             yf = stressplot(example_NMDS)$yf) %>%
  # Change data to long format
  pivot_longer(cols = c(y, yf),
               names_to = "var")

# Create plot
df %>%
  ggplot(aes(x = x,
             y = value)) +
  # Add points just for y values
  geom_point(data = df %>%
               filter(var == "y")) +
  # Add line just for yf values
  geom_step(data = df %>%
              filter(var == "yf"),
            col = "red",
            direction = "vh") +
  # Change axis labels
  labs(x = "Observed Dissimilarity", y = "Ordination Distance") +
  # Add bw theme
  theme_bw()
