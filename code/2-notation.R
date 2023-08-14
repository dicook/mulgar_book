#| code-fold: false
X <- matrix(c(1.1, 1.3, 1.4, 1.2, 2.7, 2.6, 2.4, 2.5, 3.5, 3.4, 3.2, 3.6), ncol=4, byrow=TRUE)
X


#| code-fold: false
X[,2]


#| code-fold: false
X[2,]


#| code-fold: false
X[3,2]


#| code-fold: false
A <- matrix(c(0.707,0.707,0,0,0,0,0.707,0.707), ncol=2, byrow=FALSE)
A


#| code-fold: false
sum(A[,1]^2)
sum(A[,1]*A[,2])


#| code-fold: false
X %*% A


#| eval: false
#| echo: false
## library(tourr)
## library(mulgar)
## library(ggplot2)
## set.seed(110)
## m <- matrix(rnorm(10), ncol = 2)
## m <- orthonormalise(m)
## # with this seed components 2 and 4 contribute most to direction 1
## # and components 3 and 4 contribute most to direction 2
## sum(m[,1]^2) # = 1 if properly normalised
## sum(m[,2]^2) # = 1 if properly normalised
## sum(m[,1]*m[,2]) # = 0 if orthogonal
## clusters_proj <- as.matrix(clusters[,1:5]) %*% m # calculate projection
## colnames(clusters_proj) <- c("p1", "p2")
## clusters_proj <- as.data.frame(clusters_proj)
## ggplot(clusters_proj, aes(p1, p2, color = clusters$cl)) +
##   geom_point()
## # with this projection we can distinguish one cluster from the other two

