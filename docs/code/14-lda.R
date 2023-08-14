#| message: false
#| code-fold: false
library(dplyr)
library(mulgar)
library(MASS)
load("data/penguins_sub.rda")

p_lda <- lda(species~bl+bd+fl+bm, data=penguins_sub, prior=c(1/3, 1/3, 1/3))
options(digits=2)
p_lda


#| code-fold: false
p_lda$means


#| code-fold: false
p_lda$scaling


#| code-fold: false
p_lda_pred <- predict(p_lda, penguins_sub)


#| label: fig-p-lda
#| warning: false
#| fig-cap: "Penguins projected into the 2D discriminant space, done two ways: (a) using the predicted values, (b) directly projecting using the model component. The scale is not quite the same but the projected data is identical in shape."
#| message: false
#| fig-width: 8
#| fig-height: 4
#| code-summary: "Code to generate LDA plots"
library(colorspace)
library(ggplot2)
library(ggpubr)
p_lda_pred_x1 <- data.frame(p_lda_pred$x)
p_lda_pred_x1$species <- penguins_sub$species
p_lda1 <- ggplot(p_lda_pred_x1, 
                 aes(x=LD1, y=LD2, 
                     colour=species)) + 
  geom_point() +
  xlim(-6, 8) + ylim(-6.5, 5.5) +
  scale_color_discrete_divergingx("Zissou 1") +
  ggtitle("(a)") +
  theme_minimal() +
  theme(aspect.ratio = 1, legend.title = element_blank()) 

p_lda_pred_x2 <- data.frame(as.matrix(penguins_sub[,1:4]) %*%
                              p_lda$scaling)
p_lda_pred_x2$species <- penguins_sub$species
p_lda2 <- ggplot(p_lda_pred_x2, 
                 aes(x=LD1, y=LD2, 
                     colour=species)) + 
  geom_point() +
  xlim(-6, 8) + ylim(-7, 5.5) +
  scale_color_discrete_divergingx("Zissou 1") +
  ggtitle("(b)") +
  theme_minimal() +
  theme(aspect.ratio = 1, legend.title = element_blank()) 
ggarrange(p_lda1, p_lda2, ncol=2, 
          common.legend = TRUE, legend = "bottom")



#| code-fold: false
p_vc_pool <- mulgar::pooled_vc(penguins_sub[,1:4],
                               penguins_sub$species)
p_vc_pool


#| echo: false
#| eval: false
## ginv(as.matrix(p_vc_pool))


#| label: fig-lda-assumptions1
#| fig-cap: "Scatterplot of flipper length by bill length of the penguins data, and corresponding variance-covariance ellipses. There is a small amount of difference between the ellipses, but they are similar enough to be confident in assuming the population variance-covariances are equal."
#| message: false
#| fig-width: 8
#| fig-height: 4
#| code-summary: "Code for penguins data and ellipse plots"
lda1 <- ggplot(penguins_sub, aes(x=bl, 
                         y=bd, 
                         colour=species)) +
  geom_point() +
  scale_color_discrete_divergingx("Zissou 1") +
  xlim(-2.5, 3) + ylim(-2.5, 2.5) +
  ggtitle("(a)") +
  theme_minimal() +
  theme(aspect.ratio = 1) 
p_ell <- NULL
for (i in unique(penguins_sub$species)) {
  x <- penguins_sub %>% dplyr::filter(species == i)
  e <- gen_xvar_ellipse(x[,1:2], n=150, nstd=1.5)
  e$species <- i
  p_ell <- bind_rows(p_ell, e)
}
lda2 <- ggplot(p_ell, aes(x=bl, 
                         y=bd, 
                         colour=species)) +
  geom_point() +
  scale_color_discrete_divergingx("Zissou 1") +
  xlim(-2.5, 3) + ylim(-2.5, 2.5) +
  ggtitle("(b)") +
  theme_minimal() +
  theme(aspect.ratio = 1)
ggarrange(lda1, lda2, ncol=2, 
          common.legend = TRUE, legend = "bottom")


#| label: fig-lda-assumptions2
#| fig-cap: "Scatterplot of distance to cfa and road for the bushfires data, and corresponding variance-covariance ellipses. There is a lot of difference between the ellipses, so it cannot be assumed that the population variance-covariances are equal."
#| message: false
#| fig-width: 8
#| fig-height: 4
#| code-summary: "Code for bushfires data and ellipse plots"
# Now repeat for a data set that violates assumptions
data(bushfires)
lda3 <- ggplot(bushfires, aes(x=log_dist_cfa, 
                         y=log_dist_road, 
                         colour=cause)) +
  geom_point() +
  scale_color_discrete_divergingx("Zissou 1") +
  xlim(6, 11) + ylim(-1, 10.5) +
  ggtitle("(a)") +
  theme_minimal() +
  theme(aspect.ratio = 1)
b_ell <- NULL
for (i in unique(bushfires$cause)) {
  x <- bushfires %>% dplyr::filter(cause == i)
  e <- gen_xvar_ellipse(x[,c(57, 59)], n=150, nstd=2)
  e$cause <- i
  b_ell <- bind_rows(b_ell, e)
}
lda4 <- ggplot(b_ell, aes(x=log_dist_cfa, 
                         y=log_dist_road, 
                         colour=cause)) +
  geom_point() +
  scale_color_discrete_divergingx("Zissou 1") +
  xlim(6, 11) + ylim(-1, 10.5) +
  ggtitle("(b)") +
  theme_minimal() +
  theme(aspect.ratio = 1)
ggarrange(lda3, lda4, ncol=2, 
          common.legend = TRUE, legend = "bottom")



#| message: false
#| eval: false
#| code-summary: "Code for making animated gifs"
## library(tourr)
## p_ell <- NULL
## for (i in unique(penguins_sub$species)) {
##   x <- penguins_sub %>% dplyr::filter(species == i)
##   e <- gen_xvar_ellipse(x[,1:4], n=150, nstd=1.5)
##   e$species <- i
##   p_ell <- bind_rows(p_ell, e)
## }
## p_ell$species <- factor(p_ell$species)
## load("data/penguins_tour_path.rda")
## animate_xy(p_ell[,1:4], col=factor(p_ell$species))
## render_gif(penguins_sub[,1:4],
##            planned_tour(pt1),
##            display_xy(half_range=0.9, axes="off", col=penguins_sub$species),
##            gif_file="gifs/penguins_lda1.gif",
##            frames=500,
##            loop=FALSE)
## render_gif(p_ell[,1:4],
##            planned_tour(pt1),
##            display_xy(half_range=0.9, axes="off", col=p_ell$species),
##            gif_file="gifs/penguins_lda2.gif",
##            frames=500,
##            loop=FALSE)


#| message: false
#| code-summary: "Code for adding ellipses to data"
library(tourr)
# Create an ellipse corresponding to pooled vc
pool_ell <- gen_vc_ellipse(p_vc_pool, xm=rep(0, ncol(p_vc_pool)))

# Add means to produce ellipses for each species
p_lda_pool <- data.frame(rbind(pool_ell +
                                 matrix(rep(p_lda$means[1,],
                                            each=nrow(pool_ell)),
                                        ncol=4),
                        pool_ell +
                                 matrix(rep(p_lda$means[2,],
                                            each=nrow(pool_ell)),
                                        ncol=4),
                        pool_ell +
                                 matrix(rep(p_lda$means[3,],
                                            each=nrow(pool_ell)),
                                        ncol=4)))
# Create one data set with means, data, ellipses
p_lda_pool$species <- factor(rep(levels(penguins_sub$species),
                          rep(nrow(pool_ell), 3)))
p_lda_pool$type <- "ellipse"
p_lda_means <- data.frame(p_lda$means,
                          species=factor(rownames(p_lda$means)),
                          type="mean")
p_data <- data.frame(penguins_sub[,1:5], type="data")
p_lda_all <- bind_rows(p_lda_means,
                       p_data,
                       p_lda_pool)
p_lda_all$type <- factor(p_lda_all$type, 
                         levels=c("mean", "data", "ellipse"))
shapes <- c(3, 4, 20)
p_pch <- shapes[p_lda_all$type]


#| eval: false
#| message: false
#| code-summary: "Code to generate animated gifs"
## # Code to run the tour
## animate_xy(p_lda_all[,1:4], col=p_lda_all$species, pch=p_pch)
## load("data/penguins_tour_path.rda")
## render_gif(p_lda_all[,1:4],
##            planned_tour(pt1),
##            display_xy(col=p_lda_all$species, pch=p_pch,
##                       axes="off", half_range = 0.7),
##            gif_file="gifs/penguins_lda_pooled1.gif",
##            frames=500,
##            loop=FALSE)
## 
## # Focus on one species
## render_gif(p_lda_all[p_lda_all$species == "Gentoo",1:4],
##            planned_tour(pt1),
##            display_xy(col="#F5191C",
##                       pch=p_pch[p_lda_all$species == "Gentoo"],
##                       axes="off", half_range = 0.7),
##            gif_file="gifs/penguins_lda_pooled2.gif",
##            frames=500,
##            loop=FALSE)


#| message: false
#| code-fold: false
library(classifly)

load("data/penguins_sub.rda")
p_bl_bd_lda <- lda(species~bl+bd, data=penguins_sub, 
                                  prior = c(1/3, 1/3, 1/3))


#| message: false
#| label: fig-lda-2D-boundary
#| fig-cap: "Boundary of the LDA model for two variables of the penguins data."
#| fig-width: 3
#| fig-height: 3
#| code-fold: false
p_bl_bd_lda_boundaries <- explore(p_bl_bd_lda, penguins_sub)
p_bl_bd_lda_m1 <- ggplot(p_bl_bd_lda_boundaries) +
  geom_point(aes(x=bl, y=bd, colour=species, shape=.TYPE)) + 
  scale_color_discrete_divergingx("Zissou 1") +
  scale_shape_manual(values=c(46, 16)) +
  theme_minimal() +
  theme(aspect.ratio = 1, legend.position = "none")

p_bl_bd_lda_means <- data.frame(p_bl_bd_lda$means, species=rownames(p_bl_bd_lda$means))



#| code-fold: false
p_lda <- lda(species ~ ., penguins_sub[,1:5], prior = c(1/3, 1/3, 1/3))
p_lda_boundaries <- explore(p_lda, penguins_sub)


#| eval: false
#| code-summary: "Code for generating slice tour"
## # Code to run the tour
## p_lda_boundaries$species
## animate_slice(p_lda_boundaries[p_lda_boundaries$.TYPE == "simulated",1:4], col=p_lda_boundaries$species[p_lda_boundaries$.TYPE == "simulated"], v_rel=0.02, axes="bottomleft")
## render_gif(p_lda_boundaries[p_lda_boundaries$.TYPE == "simulated",1:4],
##            planned_tour(pt1),
##            display_slice(v_rel=0.02,
##              col=p_lda_boundaries$species[p_lda_boundaries$.TYPE == "simulated"],
##              axes="bottomleft"),                     gif_file="gifs/penguins_lda_boundaries.gif",
##            frames=500,
##            loop=FALSE
##            )


#| code-summary: "Code for projecting into LDA space"
# Project the boundaries into the 2D discriminant space
p_lda_b_sub <- p_lda_boundaries[
  p_lda_boundaries$.TYPE == "simulated", 
  c(1:4, 6)]
p_lda_b_sub_ds <- data.frame(as.matrix(p_lda_b_sub[,1:4]) %*%
  p_lda$scaling)
p_lda_b_sub_ds$species <- p_lda_b_sub$species
p_lda_b_sub_ds_p <- ggplot(p_lda_b_sub_ds, 
       aes(x=LD1, y=LD2, 
           colour=species)) +
  geom_point(alpha=0.2) +  
  scale_color_discrete_divergingx("Zissou 1") +
  theme_minimal() +
  theme(aspect.ratio = 1, 
        legend.position = "bottom",
        legend.title = element_blank()) 

  


#| label: fig-lda-2D-boundaries
#| echo: false
#| fig-width: 5
#| fig-height: 5
#| fig-cap: "Discriminant space"
p_lda_b_sub_ds_p


#| eval: false
#| echo: false
## library(mulgar)
## library(dplyr)
## library(tourr)
## library(MASS)
## data("sketches_train")
## sketches_pca <- prcomp(sketches_train[,1:784])
## ggscree(sketches_pca, q=25, guide=FALSE)
## sketches_pc <- as.data.frame(sketches_pca$x[,1:12])
## sketches_pc$word <- sketches_train$word
## 
## animate_xy(sketches_pc[,1:12], col=sketches_pc$word)
## 
## sketches_lda <- lda(word~., data=sketches_pc)
## sketches_pred <- predict(sketches_lda, sketches_pc)
## sketches_ds <- data.frame(sketches_pred$x) %>%
##   mutate(word = sketches_pc$word)
## 
## animate_xy(sketches_ds[,1:5], col=sketches_ds$word)
## 
## # bushfires
## data(bushfires)
## bushfires_std <- bushfires %>%
##   dplyr::select(`rf`:`log_dist_road`) %>%
##   mutate_if(is.numeric, function(x) (x-
##       mean(x, na.rm=TRUE))/
##       sd(x, na.rm=TRUE))
## bushfires_pca <- prcomp(bushfires_std)
## bushfires_pc <- data.frame(bushfires_pca$x[,1:5]) %>%
##   mutate(cause = bushfires$cause)
## 
## bushfires_lda <- lda(cause~., data=bushfires_pc)
## bushfires_pred <- predict(bushfires_lda, bushfires_pc)
## bushfires_ds <- data.frame(bushfires_pred$x) %>%
##   mutate(cause = factor(bushfires$cause))
## animate_xy(bushfires_ds[,1:3], col=bushfires_ds$cause)
## 

