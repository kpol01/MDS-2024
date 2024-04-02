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