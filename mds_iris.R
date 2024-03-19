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
