## ----echo=knitr::is_html_output()--------------------------------------------
#| code-summary: "Load libraries"
source("code/setup.R")


## ----------------------------------------------------------------------------
#| message: false
#| code-fold: false
# Code to fit the model
load("data/penguins_sub.rda")

p_lda <- lda(species~bl+bd+fl+bm, 
             data=penguins_sub,
             prior=c(1/3, 1/3, 1/3))
options(digits=2)
# p_lda


## ----------------------------------------------------------------------------
#| code-fold: false
# Extract the sample means
p_lda$means


## ----------------------------------------------------------------------------
#| code-fold: false
# Extract the discriminant space
p_lda$scaling


## ----------------------------------------------------------------------------
#| code-fold: false
# Extract the fitted values
p_lda_pred <- predict(p_lda, penguins_sub)


## ----echo=knitr::is_html_output()--------------------------------------------
#| label: fig-p-lda
#| warning: false
#| fig-cap: "Penguins projected into the 2D discriminant space, done two ways: (a) using the predicted values, (b) directly projecting using the model component. The scale is different but the projected data is identical in shape."
#| message: false
#| fig-width: 8
#| fig-height: 4
#| fig-alt: "Two plots labelled (a) and (b) that are the same scatterplot, except the scale in each is different. Plot (a) has x-axis LD1 with labels -4, 0, 4 and 8 has y-axis LD2 with labels -6, -3, 0, 3 and 6. Plot (b) has x-axis LD1 with labels -4, 0, 4 and 8 and y-axis LD2 with labels -4, 0 and 4. There is a legend indicating colour is used to show species, with 3 levels: Adelie shown as brilliant greenish blue colour, Chinstrap shown as brilliant yellow colour and Gentoo shown as vivid red colour. The chart is a set of 313 big solid circle points of which about 97% can be seen. Red is separated from the other two clusters, but green and yellow are overlapped."
#| code-summary: "Code to generate LDA plots"
# Check calculations from the fitted model, and equations
# Using the predicted values from the model object
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

# matches the calculations done manually
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



## ----------------------------------------------------------------------------
#| code-fold: false
# Compute pooled variance-covariance
p_vc_pool <- mulgar::pooled_vc(penguins_sub[,1:4],
                               penguins_sub$species)
p_vc_pool


## ----------------------------------------------------------------------------
#| echo: false
#| eval: false
# ginv(as.matrix(p_vc_pool))


## ----------------------------------------------------------------------------
# Generate ellipses for each group's variance-covariance
p_ell <- NULL
for (i in unique(penguins_sub$species)) {
  x <- penguins_sub |> dplyr::filter(species == i)
  e <- gen_xvar_ellipse(x[,1:2], n=150, nstd=1.5)
  e$species <- i
  p_ell <- bind_rows(p_ell, e)
}


## ----echo=knitr::is_html_output()--------------------------------------------
#| label: fig-lda-assumptions1
#| fig-cap: "Scatterplot of flipper length by bill length of the penguins data, and corresponding variance-covariance ellipses. There is a small amount of difference between the ellipses, but they are similar enough to be confident in assuming the population variance-covariances are equal."
#| message: false
#| fig-width: 8
#| fig-height: 4
#| fig-alt: "Two plots labelled (a) and (b). Both have x-axis bl with labels -2, -1, 0, 1, 2 and 3 and y-axis bd with labels -2, -1, 0, 1 and 2. There is a legend indicating colour is used to show species, with 3 levels: Adelie shown as brilliant greenish blue colour, Chinstrap shown as brilliant yellow colour and Gentoo shown as vivid red colour. Plot (a) is a set of 333 big solid circle points of which about 97% can be seen, where differences between the three groups are visible. Plot (b) is a set of 450 big solid circle points arranged into three very similar shaped and oriented non-overlapping ellipses."
#| code-summary: "Code for penguins data and ellipse plots"
#| code-fold: true
lda1 <- ggplot(penguins_sub, aes(x=bl, 
                         y=bd, 
                         colour=species)) +
  geom_point() +
  scale_color_discrete_divergingx("Zissou 1") +
  xlim(-2.5, 3) + ylim(-2.5, 2.5) +
  ggtitle("(a)") +
  theme_minimal() +
  theme(aspect.ratio = 1) 
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


## ----echo=knitr::is_html_output()--------------------------------------------
#| label: fig-lda-assumptions2
#| fig-cap: "Scatterplot of distance to cfa and road for the bushfires data, and corresponding variance-covariance ellipses. There is a lot of difference between the shapes and the orientation of the ellipses, so it cannot be assumed that the population variance-covariances are equal."
#| message: false
#| fig-width: 8
#| fig-height: 4
#| fig-alt: "Two plots labelled (a) and (b). Both have x-axis log_dist_cfa with labels 6, 7, 8, 9, 10 and 11 and y-axis log_dist_road with labels 0.0, 2.5, 5.0, 7.5 and 10.0. There is a legend indicating colour is used to show cause, with 4 levels: accident shown as brilliant greenish blue colour,  arson shown as light yellowish green colour,  burning_off shown as strong orange yellow colour and lightning shown as vivid red colour. Plot (a) is a set of 333 big solid circle points of which about 97% can be seen, with few differences between the four groups. Plot (b) is a set of 450 big solid circle points arranged into four differently shaped and oriented overlapping ellipses."
#| code-summary: "Code for bushfires data and ellipse plots"
#| code-fold: true
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
  x <- bushfires |> dplyr::filter(cause == i)
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



## ----echo=knitr::is_html_output()--------------------------------------------
#| message: false
#| eval: false
#| code-summary: "Code for making animated gifs"
#| code-fold: true
# p_ell <- NULL
# for (i in unique(penguins_sub$species)) {
#   x <- penguins_sub |> dplyr::filter(species == i)
#   e <- gen_xvar_ellipse(x[,1:4], n=150, nstd=1.5)
#   e$species <- i
#   p_ell <- bind_rows(p_ell, e)
# }
# p_ell$species <- factor(p_ell$species)
# load("data/penguins_tour_path.rda")
# animate_xy(p_ell[,1:4], col=factor(p_ell$species))
# render_gif(penguins_sub[,1:4],
#            planned_tour(pt1),
#            display_xy(half_range=0.9, axes="off", col=penguins_sub$species),
#            gif_file="gifs/penguins_lda1.gif",
#            frames=500,
#            loop=FALSE)
# render_gif(p_ell[,1:4],
#            planned_tour(pt1),
#            display_xy(half_range=0.9, axes="off", col=p_ell$species),
#            gif_file="gifs/penguins_lda2.gif",
#            frames=500,
#            loop=FALSE)


## ----------------------------------------------------------------------------
#| message: false
#| code-summary: "Code for adding ellipses to data"
#| code-fold: true
# Create an ellipse corresponding to pooled vc
pool_ell <- gen_vc_ellipse(p_vc_pool, 
                           xm=rep(0, ncol(p_vc_pool)))

# Add means to produce ellipses for each species
p_lda_pool <- data.frame(rbind(
  pool_ell +
    matrix(rep(p_lda$means[1,],
      each=nrow(pool_ell)), ncol=4),
  pool_ell +
    matrix(rep(p_lda$means[2,],
      each=nrow(pool_ell)), ncol=4),
  pool_ell +
    matrix(rep(p_lda$means[3,],
      each=nrow(pool_ell)), ncol=4)))
# Create one data set with means, data, ellipses
p_lda_pool$species <- factor(rep(levels(penguins_sub$species),
                          rep(nrow(pool_ell), 3)))
p_lda_pool$type <- "ellipse"
p_lda_means <- data.frame(
  p_lda$means,
  species=factor(rownames(p_lda$means)),
                          type="mean")
p_data <- data.frame(penguins_sub[,1:5], 
                     type="data")
p_lda_all <- bind_rows(p_lda_means,
                       p_data,
                       p_lda_pool)
p_lda_all$type <- factor(p_lda_all$type, 
   levels=c("mean", "data", "ellipse"))
shapes <- c(3, 4, 20)
p_pch <- shapes[p_lda_all$type]


## ----echo=knitr::is_html_output()--------------------------------------------
#| eval: false
#| message: false
#| code-summary: "Code to generate animated gifs"
#| code-fold: true
# # Code to run the tour
# animate_xy(p_lda_all[,1:4], col=p_lda_all$species, pch=p_pch)
# load("data/penguins_tour_path.rda")
# render_gif(p_lda_all[,1:4],
#            planned_tour(pt1),
#            display_xy(col=p_lda_all$species, pch=p_pch,
#                       axes="off", half_range = 0.7),
#            gif_file="gifs/penguins_lda_pooled1.gif",
#            frames=500,
#            loop=FALSE)
# 
# # Focus on one species
# render_gif(p_lda_all[p_lda_all$species == "Gentoo",1:4],
#            planned_tour(pt1),
#            display_xy(col="#F5191C",
#                       pch=p_pch[p_lda_all$species == "Gentoo"],
#                       axes="off", half_range = 0.7),
#            gif_file="gifs/penguins_lda_pooled2.gif",
#            frames=500,
#            loop=FALSE)


## ----------------------------------------------------------------------------
#| message: false
#| code-fold: false
p_bl_bd_lda <- lda(species~bl+bd, data=penguins_sub, 
                                  prior = c(1/3, 1/3, 1/3))


## ----------------------------------------------------------------------------
#| message: false
#| label: fig-lda-2D-boundary
#| fig-cap: "Prediction regions of the LDA model for two variables of the three species of penguins indicated by the small points. Large points are the observations, and the sample mean of each species is represented by the plus. The boundaries between groups can be seen to be roughly half-way between the means, taking the elliptical spread into account, and mostly distinguishes the three species."
#| fig-alt: "Square divided into three regions by the coloured points, primarily a wedge of yellow (Chinstrap) extending from the top right divides the other two groups. The regions mostly contain the observed values, with just a few over the boundary in the wrong region."
#| fig-width: 3
#| fig-height: 3
#| out-width: 70%
#| code-fold: false
# Compute points in domain of data and predict
p_bl_bd_lda_boundaries <- explore(p_bl_bd_lda, penguins_sub)
p_bl_bd_lda_m1 <- ggplot(p_bl_bd_lda_boundaries) +
  geom_point(aes(x=bl, y=bd, 
                 colour=species, 
                 shape=.TYPE), alpha=0.8) + 
  scale_color_discrete_divergingx("Zissou 1") +
  scale_shape_manual(values=c(46, 16)) +
  theme_minimal() +
  theme(aspect.ratio = 1, legend.position = "none")

p_bl_bd_lda_means <- data.frame(p_bl_bd_lda$means,
                        species=rownames(p_bl_bd_lda$means))
p_bl_bd_lda_m1 +   
  geom_point(data=p_bl_bd_lda_means, 
             aes(x=bl, y=bd), 
             colour="black", 
             shape=3,
             size=3) 


## ----echo=knitr::is_html_output()--------------------------------------------
#| code-fold: false
p_lda <- lda(species ~ ., penguins_sub[,1:5], prior = c(1/3, 1/3, 1/3))
p_lda_boundaries <- explore(p_lda, penguins_sub)


## ----echo=knitr::is_html_output()--------------------------------------------
#| eval: false
#| code-summary: "Code for generating slice tour"
# # Code to run the tour
# animate_slice(p_lda_boundaries[
#   p_lda_boundaries$.TYPE == "simulated", 1:4],
#               col=p_lda_boundaries$species[
#                 p_lda_boundaries$.TYPE == "simulated"],
#               v_rel=0.8,
#               axes="bottomleft")
# render_gif(p_lda_boundaries[p_lda_boundaries$.TYPE == "simulated",1:4],
#            planned_tour(pt1),
#            display_slice(v_rel=0.8,
#              col=p_lda_boundaries$species[
#                p_lda_boundaries$.TYPE == "simulated"],
#              axes="bottomleft"),
#            gif_file="gifs/penguins_lda_boundaries.gif",
#            frames=500,
#            loop=FALSE
#            )


## ----echo=knitr::is_html_output()--------------------------------------------
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
  geom_point(alpha=0.5) +  
  geom_point(data=p_lda_pred_x1, aes(x=LD1, 
                               y=LD2, 
                               shape=species),
             inherit.aes = FALSE) +
  scale_color_discrete_divergingx("Zissou 1") +
  scale_shape_manual(values=c(1, 2, 3)) +
  theme_minimal() +
  theme(aspect.ratio = 1, 
        legend.position = "bottom",
        legend.title = element_blank()) 


## ----------------------------------------------------------------------------
#| echo: false
#| label: fig-lda-2D-boundaries
#| fig-width: 5
#| fig-height: 5
#| fig-cap: "Discriminant space"
#| fig-alt: "Scatterplot of points divided neatly into three colour groups by linear boundaries. The regions are split at the middle, and with the boundary between blue and red, blue and yellow, and yellow and red, starting near 11, 4, and o'clock, respectively."
p_lda_b_sub_ds_p


## ----------------------------------------------------------------------------
#| echo: false
#| eval: false
#| fig-width: 5
#| fig-height: 5
#| out-width: 50%
#| fig-cap: "Discriminant space"
# # Note: had to use the image above between quarto compile
# # broke when this chunk was evaluated.
# p_lda_b_sub_ds_p


## ----------------------------------------------------------------------------
#| echo: false
#| message: false
# Prepare data
ds_proj <- tourr::orthonormalise(p_lda$scaling) |>
  as_tibble() |>
  rename(oLD1 = LD1, oLD2 = LD2) |>
  bind_cols(p_lda$scaling) |>
  mutate(var = rownames(p_lda$scaling)) |>
  dplyr::select(var, LD1, LD2, oLD1, oLD2)


## ----eval=knitr::is_html_output()--------------------------------------------
#| label: tbl-ds-coef-html
#| echo: false
#| tbl-cap: "Coefficients of the discriminant space, original and othonormalised. LD1 is the direction where Gentoo is separated from the others, and the coefficients suggest this is due to a contrast between bd and fl/bm. LD2 is where Adelie is different from Chinstrap, which is generated by a contrast between bl and bm."
knitr::kable(ds_proj, digits=2,
             col.names = c(" ", "LD1", "LD2", "LD1", "LD2")) |>
  add_header_above(c(" " = 1, "original"=2, "orthonormal"=2))


## ----eval=knitr::is_latex_output()-------------------------------------------
#| label: tbl-ds-coef-pdf
#| echo: false
#| tbl-cap: "Coefficients of the discriminant space, original and othonormalised. LD1 is the direction where Gentoo is separated from the others, and the coefficients suggest this is due to a contrast between bd and fl/bm. LD2 is where Adelie is different from Chinstrap, which is generated by a contrast between bl and bm."
# knitr::kable(ds_proj, digits=2, format="latex",
#              col.names = c(" ", "LD1", "LD2", "LD1", "LD2")) |>
#   add_header_above(c(" " = 1, "original"=2, "orthonormal"=2))


## ----------------------------------------------------------------------------
#| eval: false
#| echo: false
# animate_xy(penguins_sub[, 1:4],
#            radial_tour(as.matrix(ds_proj[,4:5]), 1),
#            col=penguins_sub$species
#            )
# render_gif(penguins_sub[, 1:4],
#            radial_tour(as.matrix(ds_proj[,4:5]), 1),
#            display_xy(col=penguins_sub$species),
#            gif_file = "gifs/penguins_vip.gif",
#            frames=100,
#            width=400,
#            height=400,
#            loop=FALSE
#            )


## ----------------------------------------------------------------------------
#| eval: false
#| echo: false
# set.seed(458)
# animate_xy(penguins_sub[, 1:4],
#            local_tour(as.matrix(ds_proj[,4:5]), pi/10),
#            col=penguins_sub$species
#            )
# set.seed(458)
# render_gif(penguins_sub[, 1:4],
#            local_tour(as.matrix(ds_proj[,4:5]), pi/10),
#            display_xy(col=penguins_sub$species),
#            gif_file = "gifs/penguins_local.gif",
#            frames=500,
#            width=400,
#            height=400,
#            loop=FALSE
#            )


## ----------------------------------------------------------------------------
#| echo: true
#| eval: false
# library(readr)
# library(dplyr)
# music <- read_csv("http://ggobi.org/book/data/music-sub.csv",
#                   show_col_types = FALSE) |>
#   rename(title = `...1`) |>
#   mutate(type = factor(type))


## ----------------------------------------------------------------------------
#| eval: false
# library(mulgar)
# data(aflw)
# aflw_sub <- aflw |>
#   dplyr::filter(position %in% c("BPL", "FF", "RR")) |>
#   dplyr::mutate(position = factor(position)) |>
#   dplyr::select(goals:tackles)


## ----------------------------------------------------------------------------
#| eval: false
#| echo: false
# data("sketches_train")
# sketches_pca <- prcomp(sketches_train[,1:784])
# ggscree(sketches_pca, q=25, guide=FALSE)
# sketches_pc <- as.data.frame(sketches_pca$x[,1:12])
# sketches_pc$word <- sketches_train$word
# 
# animate_xy(sketches_pc[,1:12], col=sketches_pc$word)
# 
# sketches_lda <- lda(word~., data=sketches_pc)
# sketches_pred <- predict(sketches_lda, sketches_pc)
# sketches_ds <- data.frame(sketches_pred$x) |>
#   mutate(word = sketches_pc$word)
# 
# animate_xy(sketches_ds[,1:5], col=sketches_ds$word)
# 
# # bushfires
# data(bushfires)
# bushfires_sub <- bushfires[,c(5, 8:45, 48:55, 57:60)] |>
#   mutate(cause = factor(cause))
# 
# bushfires_std <- bushfires_sub |>
#   dplyr::select(`rf`:`log_dist_road`) |>
#   mutate_if(is.numeric, function(x) (x-
#       mean(x, na.rm=TRUE))/
#       sd(x, na.rm=TRUE))
# bushfires_pca <- prcomp(bushfires_std)
# bushfires_pc <- data.frame(bushfires_pca$x[,1:5]) |>
#   mutate(cause = bushfires$cause)
# 
# bushfires_lda <- lda(cause~., data=bushfires_pc)
# bushfires_pred <- predict(bushfires_lda, bushfires_pc)
# bushfires_ds <- data.frame(bushfires_pred$x) |>
#   mutate(cause = factor(bushfires$cause))
# animate_xy(bushfires_ds[,1:3], col=bushfires_ds$cause)
# 


## ----------------------------------------------------------------------------
#| eval: false
#| echo: false
# library(uwot)
# data(aflw)
# aflw_sub <- aflw |>
#   filter(position %in% c("BPL", "FF", "RR")) |>
#   mutate(position = factor(position)) |>
#   select(goals:tackles, position)
# aflw_umap <- umap(aflw_sub[,1:7], scale="scale")
# aflw_umap <- aflw_umap |>
#   as_tibble() |>
#   rename(umap1 = V1, umap2 = V2) |>
#   mutate(position = aflw_sub$position)
# 
# ggplot(aflw_umap, aes(x=umap1, y=umap2, colour=position)) +
#   geom_point(alpha = 0.8) +
#   scale_color_discrete_divergingx(palette = "Zissou 1")
# 
# aflw_sub_std <- aflw_sub |>
#   mutate_if(is.numeric, function(x) (x-mean(x))/sd(x))
# aflw_lda <- lda(position~., data=aflw_sub_std)
# aflw_ds <- predict(aflw_lda, aflw_sub)$x |>
#   as_tibble() |>
#   mutate(position = aflw_sub$position)
# ggplot(aflw_ds, aes(x=LD1, y=LD2, colour=position)) +
#   geom_point(alpha = 0.8) +
#   scale_color_discrete_divergingx(palette = "Zissou 1")
# 
# aflw_lda$scaling
# 
# animate_xy(aflw_sub[,c(1, 3, 5)], col=aflw_sub$position, rescale=TRUE)
# 
# # LD1 is primarily kicks and disposals which distinguishes RR from FF.
# # LD2 is primarily goals and marks which distinguishes BPL from FF and RR.
# # Using goals, kicks and disposals, the three player types are virtually distinguishable.

