#| code-summary: "Code to simulate data examples"
# Toy examples
library(mulgar)
library(ggplot2)
library(geozoo)
library(tourr)

set.seed(1071)
n1 <- 162
vc1 <- matrix(c(1, -0.7, -0.7, 1), ncol=2, byrow=TRUE)
c1 <- rmvn(n=n1, p=2, mn=c(-2, -2), vc=vc1)
vc2 <- matrix(c(1, -0.4, -0.4, 1)*2, ncol=2, byrow=TRUE)
n2 <- 138
c2 <- rmvn(n=n2, p=2, mn=c(2, 2), vc=vc2)
df1 <- data.frame(x1=mulgar:::scale2(c(c1[,1], c2[,1])), 
                 x2=mulgar:::scale2(c(c1[,2], c2[,2])), 
                 cl = factor(c(rep("A", n1), 
                               rep("B", n2))))
c1 <- sphere.hollow(p=2, n=n1)$points*3 + 
  c(rnorm(n1, sd=0.3), rnorm(n1, sd=0.3))
c2 <- sphere.solid.random(p=2, n=n2)$points
df2 <- data.frame(x1=mulgar:::scale2(c(c1[,1], c2[,1])), 
                  x2=mulgar:::scale2(c(c1[,2], c2[,2])), 
                  cl = factor(c(rep("A", n1), 
                               rep("B", n2))))


#| code-fold: false
#| message: false
library(classifly)
library(e1071)
df1_svm <- svm(cl~., data=df1, 
                     probability=TRUE, 
                     kernel="linear", 
               scale=FALSE)
df1_svm_e <- explore(df1_svm, df1)

df2_svm <- svm(cl~., data=df2,  
                     probability=TRUE, 
                     kernel="radial")
df2_svm_e <- explore(df2_svm, df2)


#| label: fig-svm-toy
#| fig-cap: "SVM classifier fit overlaid on two simulated data examples: (a) groups with different variance-covariance, fitted using a linear kernel, (b) groups with non-linear separation, fitted using a radial kernel. The band of points shown as '+' mark the SVM boundary, and points marked by 'x' are the support vectors used to define the boundary. "
#| fig-width: 8
#| fig-height: 4
#| code-summary: "Code to make plots"
library(patchwork)
library(colorspace)
s1 <- ggplot() + 
  geom_point(data=df1, aes(x=x1, y=x2, colour=cl),
             shape=20) +
  scale_colour_discrete_divergingx(palette="Zissou 1") +
  geom_point(data=df1_svm_e[(!df1_svm_e$.BOUNDARY)&(df1_svm_e$.TYPE=="simulated"),], 
             aes(x=x1, y=x2, colour=cl), shape=3) +
  geom_point(data=df1[df1_svm$index,], 
             aes(x=x1, y=x2, colour=cl), 
             shape=4, size=4) +
  theme_minimal() +
  theme(aspect.ratio=1, legend.position = "none") +
  ggtitle("(a)")

s2 <- ggplot() + 
  geom_point(data=df2, aes(x=x1, y=x2, colour=cl), shape=20) +
  scale_colour_discrete_divergingx(palette="Zissou 1") +
  geom_point(data=df2_svm_e[(!df2_svm_e$.BOUNDARY)&(df2_svm_e$.TYPE=="simulated"),], 
             aes(x=x1, y=x2, colour=cl), 
             shape=3) +
  geom_point(data=df2[df2_svm$index,], 
             aes(x=x1, y=x2, colour=cl), 
             shape=4, size=4) +
  theme_minimal() +
  theme(aspect.ratio=1, legend.position = "none") +
  ggtitle("(b)")

s1+s2


#| code-fold: false
df1_svm$index


#| code-fold: false
df1_svm$coefs


#| code-fold: false
df1_svm$rho


#| code-fold: false
w = t(df1_svm$SV) %*% df1_svm$coefs
w


#| eval: false
#| code-fold: false
## s1 + geom_abline(intercept=df1_svm$rho/w[2],
##                  slope=-w[1]/w[2])


#| code-fold: false
#| warning: false
#| message: false
library(dplyr)
load("data/penguins_sub.rda")
chinstrap <- penguins_sub %>%
  filter(species == "Chinstrap") %>%
  select(-species) %>%
  mutate_if(is.numeric, mulgar:::scale2)
chinstrap_svm <- svm(sex~., data=chinstrap, 
                     kernel="linear",
                     probability=TRUE, 
                     scale=FALSE)
chinstrap_svm_e <- explore(chinstrap_svm, chinstrap)


#| eval: false
#| code-summary: "Code to make the tours"
## # Tour raw data
## animate_xy(chinstrap[,1:4], col=chinstrap$sex)
## # Add all SVs, including bounded
## c_pch <- rep(20, nrow(chinstrap))
## c_pch[chinstrap_svm$index] <- 4
## animate_xy(chinstrap[,1:4], col=chinstrap$sex, pch=c_pch)
## # Only show the SVs with |coefs| < 1
## c_pch <- rep(20, nrow(chinstrap))
## c_pch[chinstrap_svm$index[abs(chinstrap_svm$coefs)<1]] <- 4
## c_cex <- rep(1, nrow(chinstrap))
## c_cex[chinstrap_svm$index[abs(chinstrap_svm$coefs)<1]] <- 2
## animate_xy(chinstrap[,1:4], col=chinstrap$sex,
##            pch=c_pch, cex=c_cex)
## render_gif(chinstrap[,1:4],
##            grand_tour(),
##            display_xy(col=chinstrap$sex, pch=c_pch, cex=c_cex),
##            gif_file="gifs/chinstrap_svs.gif",
##            width=400,
##            height=400,
##            frames=500)
## 
## # Tour the separating hyperplane also
## symbols <- c(3, 20)
## c_pch <- symbols[as.numeric(chinstrap_svm_e$.TYPE[!chinstrap_svm_e$.BOUNDARY])]
## animate_xy(chinstrap_svm_e[!chinstrap_svm_e$.BOUNDARY,1:4],
##            col=chinstrap_svm_e$sex[!chinstrap_svm_e$.BOUNDARY],
##            pch=c_pch)
## render_gif(chinstrap_svm_e[!chinstrap_svm_e$.BOUNDARY,1:4],
##            grand_tour(),
##            display_xy(col=chinstrap_svm_e$sex[!chinstrap_svm_e$.BOUNDARY], pch=c_pch),
##            gif_file="gifs/chinstrap_svm.gif",
##            width=400,
##            height=400,
##            frames=500)


#| code-fold: false
t(chinstrap_svm$SV) %*% chinstrap_svm$coefs


#| code-fold: false
set.seed(1022)
prj1 <- mulgar::norm_vec(t(chinstrap_svm$SV) %*%
                           chinstrap_svm$coefs)
prj2 <- basis_random(4, 1)
prj <- orthonormalise(cbind(prj1, prj2))
prj


#| eval: false
#| code-summary: "Code to conduct the radial tours"
## animate_xy(chinstrap_svm_e[!chinstrap_svm_e$.BOUNDARY,1:4],
##            tour_path = radial_tour(start=prj, mvar = 2),
##            col=chinstrap_svm_e$sex[!chinstrap_svm_e$.BOUNDARY],
##            pch=c_pch)
## render_gif(chinstrap_svm_e[!chinstrap_svm_e$.BOUNDARY,1:4],
##            radial_tour(start=prj, mvar = 2),
##            display_xy(col=chinstrap_svm_e$sex[!chinstrap_svm_e$.BOUNDARY], pch=c_pch),
##            gif_file="gifs/chinstrap_rad_bd.gif",
##            apf = 1/30,
##            width=400,
##            height=400,
##            frames=500)
## render_gif(chinstrap_svm_e[!chinstrap_svm_e$.BOUNDARY,1:4],
##            radial_tour(start=prj, mvar = 1),
##            display_xy(col=chinstrap_svm_e$sex[!chinstrap_svm_e$.BOUNDARY], pch=c_pch),
##            gif_file="gifs/chinstrap_rad_bl.gif",
##            apf = 1/30,
##            width=400,
##            height=400,
##            frames=500)
## render_gif(chinstrap_svm_e[!chinstrap_svm_e$.BOUNDARY,1:4],
##            radial_tour(start=prj, mvar = 3),
##            display_xy(col=chinstrap_svm_e$sex[!chinstrap_svm_e$.BOUNDARY], pch=c_pch),
##            gif_file="gifs/chinstrap_rad_fl.gif",
##            apf = 1/30,
##            width=400,
##            height=400,
##            frames=500)
## render_gif(chinstrap_svm_e[!chinstrap_svm_e$.BOUNDARY,1:4],
##            radial_tour(start=prj, mvar = 4),
##            display_xy(col=chinstrap_svm_e$sex[!chinstrap_svm_e$.BOUNDARY], pch=c_pch),
##            gif_file="gifs/chinstrap_rad_bm.gif",
##            apf = 1/30,
##            width=400,
##            height=400,
##            frames=500)


#| eval: false
#| echo: false
## library(mulgar)
## library(tourr)
## library(tidyverse)
## library(MASS)
## library(e1071)
## library(classifly)
## data(bushfires)
## 
## bushfires_sub <- dplyr::select(bushfires, log_dist_cfa,
##                                log_dist_road,
##                                cause) %>%
##   filter(cause %in% c("lightning", "arson")) %>%
##   mutate(cause = as.factor(cause))
## 
## ggplot(bushfires_sub, aes(log_dist_cfa, log_dist_road, color = cause)) +
##   geom_point()
## 
## bf_lda <- lda(cause ~ ., bushfires_sub)#, prior = c(0.5, 0.5))
## bf_lda_b <- as.data.frame(explore(bf_lda, bushfires_sub))
## ggplot(filter(bf_lda_b, .BOUNDARY != TRUE)) +
##   geom_point(aes(x=log_dist_cfa, y=log_dist_road, colour=cause, shape=.TYPE)) +
##   #scale_color_discrete_divergingx("Zissou 1") +
##   scale_shape_manual(values=c(46, 16)) +
##   theme_minimal() +
##   theme(aspect.ratio = 1, legend.position = "none")
## 
## bf_svm <- svm(cause ~ ., bushfires_sub, kernel = "linear",
##               probability = TRUE)
## bf_svm_b <- as.data.frame(explore(bf_svm, bushfires_sub))
## ggplot(filter(bf_svm_b, .BOUNDARY != TRUE)) +
##   geom_point(aes(x=log_dist_cfa, y=log_dist_road, colour=cause, shape=.TYPE)) +
##   #scale_color_discrete_divergingx("Zissou 1") +
##   scale_shape_manual(values=c(46, 16)) +
##   theme_minimal() +
##   theme(aspect.ratio = 1, legend.position = "none")
## 
## bushfires_sub_2 <- dplyr::select(bushfires, #amaxt90,
##                                  amaxt180, amaxt720,
##                                  log_dist_cfa, log_dist_road,
##                                  cause) %>%
##   filter(cause %in% c("lightning", "arson")) %>%
##   mutate(cause = as.factor(cause))
## 
## bf_2_svm <- svm(cause ~ ., data = bushfires_sub_2,
##                 kernel = "linear", probability = TRUE)
## 
## b1_1 <- t(bf_2_svm$SV) %*% bf_2_svm$coefs
## # it seems most variables are important, maybe amaxt180
## 
## bf_2_svm_e <- explore(bf_2_svm, bushfires_sub_2, n = 50000)
## 
## symbols <- c(46, 20)
## c_pch <- symbols[as.numeric(bf_2_svm_e$.TYPE[!bf_2_svm_e$.BOUNDARY])]
## animate_xy(bf_2_svm_e[!bf_2_svm_e$.BOUNDARY,1:4],
##            col=as.factor(bf_2_svm_e$cause[!bf_2_svm_e$.BOUNDARY]),
##            pch = c_pch)
## # we see that the hyperplane is > 2D and that amaxt180 is less important
## # we also see that main feature in the data is distribution for one group (lightning)
## # and it is difficult to separate the groups because they fall along similar direction
## # now let's check with radial tour
## set.seed(383)
## b_start <- orthonormalise(cbind(b1_1, basis_random(4,1)))
## animate_xy(bf_2_svm_e[!bf_2_svm_e$.BOUNDARY,1:4],
##            tour_path = radial_tour(start = b_start,
##                                    mvar = c(3,4)),
##            col=bf_2_svm_e$cause[!bf_2_svm_e$.BOUNDARY],
##            pch=c_pch)
## # because we use a combination of variables we do not see
## # plane turning when rotating out only one of the variables,
## # but we can see that combinations of the last three is what is
## # important
## 
## data("sketches_train")
## banana_v_boomerang <- filter(sketches_train,
##                           word %in% c("banana", "boomerang"))
## bvb_pca <- prcomp(banana_v_boomerang[,1:784])
## ggscree(bvb_pca, q=25, guide=FALSE) # keeping first 5
## # keeping first 9 seems like a good idea, but would require very large n
## bvb_pc <- as.data.frame(bvb_pca$x[,1:5])
## bvb_pc$word <- droplevels(banana_v_boomerang$word)
## 
## bvb_pc <- mutate_if(bvb_pc, is.numeric, mulgar:::scale2)
## 
## animate_xy(bvb_pc[,1:5], col=bvb_pc$word)
## 
## 
## 
## bvb_svm_l <- svm(word ~ ., data = bvb_pc,
##                 kernel = "linear", probability = TRUE)
## sum(predict(bvb_svm_l) != bvb_pc$word) # compare accuracy as well
## bvb_svm_r <- svm(word ~ ., data = bvb_pc,
##                 kernel = "radial", probability = TRUE)
## sum(predict(bvb_svm_r) != bvb_pc$word)
## 
## bvb_svm_l_e <- explore(bvb_svm_l, bvb_pc, n = 20000)
## 
## symbols <- c(46, 20)
## c_pch <- symbols[as.numeric(bvb_svm_l_e$.TYPE[!bvb_svm_l_e$.BOUNDARY])]
## animate_xy(bvb_svm_l_e[!bvb_svm_l_e$.BOUNDARY,1:5],
##            col=bvb_svm_l_e$word[!bvb_svm_l_e$.BOUNDARY],
##            pch = c_pch)
## # for linear SVM we can use projections to understand the shape
## 
## bvb_svm_r_e <- explore(bvb_svm_r, bvb_pc, n = 20000)
## animate_slice(bvb_svm_r_e[bvb_svm_r_e$.TYPE=="simulated" &
##                             !bvb_svm_r_e$.BOUNDARY,1:5],
##               col=bvb_svm_r_e$word[bvb_svm_r_e$.TYPE=="simulated" &
##                                      !bvb_svm_r_e$.BOUNDARY], v_rel = 10)
## # for radial kernel better use slice tour to see shape of the decision boundary
## # here we only look at simulated points on the boundary, clear that this is not linear
## 

