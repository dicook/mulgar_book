## ----echo=knitr::is_html_output()--------------------------------------------
#| code-summary: "Load libraries"
source("code/setup.R")


## ----echo=knitr::is_html_output()--------------------------------------------
#| message: false
#| code-summary: "Code to fit forest"
load("data/penguins_sub.rda")

penguins_rf <- randomForest(species~.,
                             data=penguins_sub[,1:5],
                             importance=TRUE)


## ----------------------------------------------------------------------------
#| code-fold: false
penguins_rf$confusion
penguins_errors <- penguins_sub |>
  mutate(err = ifelse(penguins_rf$predicted !=
                        penguins_rf$y, 1, 0))


## ----echo=knitr::is_html_output()--------------------------------------------
#| eval: false
#| code-summary: "Code to make animated gifs"
# symbols <- c(1, 16)
# p_pch <- symbols[penguins_errors$err+1]
# p_cex <- rep(1, length(p_pch))
# p_cex[penguins_errors$err==1] <- 2
# animate_xy(penguins_errors[,1:4],
#            col=penguins_errors$species,
#            pch=p_pch, cex=p_cex)
# render_gif(penguins_errors[,1:4],
#            grand_tour(),
#            display_xy(col=penguins_errors$species,
#                       pch=p_pch, cex=p_cex),
#            gif_file="gifs/p_rf_errors.gif",
#            frames=500,
#            width=400,
#            height=400)
# 
# animate_xy(penguins_errors[,1:4],
#            guided_tour(lda_pp(penguins_errors$species)),
#            col=penguins_errors$species,
#            pch=pch)
# 
# render_gif(penguins_errors[,1:4],
#            guided_tour(lda_pp(penguins_errors$species)),
#            display_xy(col=penguins_errors$species,
#                       pch=p_pch, cex=p_cex),
#            gif_file="gifs/p_rf_errors_guided.gif",
#            frames=500,
#            width=400,
#            height=400,
#            loop=FALSE)
# 


## ----echo=knitr::is_html_output(), eval=FALSE--------------------------------
#| label: fig-penguins-nn-boundaries
#| code-fold: true
# # Generate grid over explanatory variables
# p_grid <- tibble(
#   bl = runif(10000, min(penguins_sub$bl), max(penguins_sub$bl)),
#   bd = runif(10000, min(penguins_sub$bd), max(penguins_sub$bd)),
#   fl = runif(10000, min(penguins_sub$fl), max(penguins_sub$fl)),
#   bm = runif(10000, min(penguins_sub$bm), max(penguins_sub$bm))
# )
# # Predict grid
# p_grid_pred <- p_nn_model |>
#   predict(as.matrix(p_grid), verbose=0)
# p_grid_pred_cat <- levels(p_train$species)[apply(p_grid_pred, 1, which.max)]
# p_grid_pred_cat <- factor(p_grid_pred_cat,
#                           levels=levels(p_train$species))
# 
# # Project into weights from the two nodes
# p_grid_proj <- as.matrix(p_grid) %*% p_nn_wgts_on
# colnames(p_grid_proj) <- c("nn1", "nn2")
# p_grid_proj <- p_grid_proj |>
#   as_tibble() |>
#   mutate(species = p_grid_pred_cat)
# 
# # Plot
# ggplot(p_grid_proj, aes(x=nn1, y=nn2,
#                      colour=species)) +
#   geom_point(alpha=0.5) +
#   geom_point(data=p_all_m, aes(x=nn1,
#                                y=nn2,
#                                shape=species),
#              inherit.aes = FALSE) +
#   scale_colour_discrete_divergingx(palette="Zissou 1") +
#   scale_shape_manual(values=c(1, 2, 3)) +
#   theme_minimal() +
#   theme(aspect.ratio=1,
#         legend.position = "bottom",
#         legend.title = element_blank())


## ----echo=knitr::is_html_output()--------------------------------------------
#| message: false
# Split the data intro training and testing, as done in 17-nn chapter
library(rsample)
library(tidymodels)
library(keras)

load("data/penguins_sub.rda") # from mulgar book

set.seed(821)
p_split <- penguins_sub |> 
  select(bl:species) |>
  initial_split(prop = 2/3, 
                strata=species)
p_train <- training(p_split)
p_test <- testing(p_split)

# Data needs to be matrix, and response needs to be numeric
p_train_x <- p_train |>
  select(bl:bm) |>
  as.matrix()
p_train_y <- p_train |> pull(species) |> as.numeric() 
p_train_y <- p_train_y-1 # Needs to be 0, 1, 2
p_test_x <- p_test |>
  select(bl:bm) |>
  as.matrix()
p_test_y <- p_test |> pull(species) |> as.numeric() 
p_test_y <- p_test_y-1 # Needs to be 0, 1, 2


## ----echo=knitr::is_html_output()--------------------------------------------
#| message: false
load("data/p_train_pred.rda")
load("data/p_test_pred.rda")

p_train_pred_cat <- levels(p_train$species)[
  apply(p_train_pred, 1,
        which.max)]
p_train_pred_cat <- factor(
  p_train_pred_cat,
  levels=levels(p_train$species))

p_test_pred_cat <- levels(p_test$species)[
  apply(p_test_pred, 1, 
        which.max)]
p_test_pred_cat <- factor(
  p_test_pred_cat,
  levels=levels(p_test$species))


## ----eval=FALSE--------------------------------------------------------------
# # Explanations
# # https://www.r-bloggers.com/2022/08/kernel-shap/
# library(kernelshap)
# library(shapviz)
# p_explain <- kernelshap(
#     p_nn_model,
#     p_train_x,
#     bg_X = p_train_x,
#     verbose = FALSE
#   )
# p_exp_sv <- shapviz(p_explain)
# save(p_exp_sv, file="data/p_exp_sv.rda")


## ----echo=knitr::is_html_output()--------------------------------------------
#| code-fold: true
load("data/p_exp_sv.rda")
p_exp_gentoo <- p_exp_sv$Class_3$S
p_exp_gentoo <- p_exp_gentoo |>
  as_tibble() |>
  mutate(species = p_train$species,
         pspecies = p_train_pred_cat,
  ) |>
  mutate(error = ifelse(species == pspecies, 0, 1)) |>
  mutate(error = factor(error, labels=c("no", "yes")))


## ----echo=knitr::is_html_output()--------------------------------------------
#| label: tbl-p-shap
#| warning: false
#| tbl-cap: "SHAP values for the Gentoo penguin misclassified as Adelie. "
p_row_id <- c(1:nrow(p_exp_gentoo))[p_exp_gentoo$species == "Gentoo" &
                                      p_exp_gentoo$pspecies == "Adelie"]
p_outlier <- rbind(as.numeric(p_exp_sv$Class_1$S[p_row_id,]),
                     as.numeric(p_exp_sv$Class_2$S[p_row_id,]),
                     as.numeric(p_exp_sv$Class_3$S[p_row_id,])) |>
  as_tibble() |>
  rename(bl=V1, bd=V2, fl=V3, bm=V4) |>
  mutate(species = c("Adelie", "Chinstrap", "Gentoo")) |>
  select(species, bl:bm)
knitr::kable(p_outlier, digits=2)


## ----echo=knitr::is_html_output()--------------------------------------------
#| eval: false
#| code-fold: true
#| label: fig-shapley-dot
#| fig-width: 4
#| fig-height: 3
#| out-width: 80%
#| fig-cap: "SHAP values focused on Gentoo class, for each variable. The one misclassified penguin (orange) has a much lower value for body mass, suggesting that this variable is used differently for the prediction than for other penguins." 
#| fig-alt: "This is an untitled chart with no subtitle or caption. It has x-axis with labels bl, bd, fl and bm. It has y-axis 'SHAP' with labels -0.25, 0.00, 0.25 and 0.50. In this chart colour is used to show factor(error). The legend that would normally indicate this has been hidden. The chart is a set of 316 big solid circle points of which about 92% can be seen. It has alpha set to 0.8."
# p_exp_gentoo |>
#   filter(species == "Gentoo") |>
#   pivot_longer(bl:bm, names_to="var", values_to="shap") |>
#   mutate(var = factor(var, levels=c("bl", "bd", "fl", "bm"))) |>
#   ggplot(aes(x=var, y=shap, colour=factor(error))) +
#   geom_quasirandom(alpha=0.8) +
#   scale_colour_discrete_divergingx(palette="Geyser") +
#   #facet_wrap(~var) +
#   xlab("") + ylab("SHAP") +
#   theme_minimal() +
#   theme(legend.position = "none")


## ----echo=knitr::is_html_output()--------------------------------------------
#| code-fold: true
#| message: false
#| warning: false
p_pcp <- p_exp_gentoo |>
  filter(species == "Gentoo") |>
  pcp_select(1:4) |>
  ggplot(aes_pcp()) +
    geom_pcp_axes() + 
    geom_pcp_boxes(fill="grey80") + 
    geom_pcp(aes(colour = factor(error)), 
             linewidth = 1.5, alpha=0.3) +
  scale_colour_discrete_divergingx(palette="Geyser") +
  xlab("") + ylab("SHAP") +
  theme_minimal() +
  theme(legend.position = "none")
d <- p_exp_gentoo |>
  filter(species == "Gentoo") 
p_sm <- ggpairs(d, columns = 1:4, 
        upper = list(continuous = wrap("points", alpha = 0.8)), 
        lower = list(continuous = wrap("points", alpha = 0.8)), 
        diag = list(continuous = wrap("barDiag", alpha = 0.8, bins = 15)), 
        ggplot2::aes(colour = error, fill = error), 
        alpha = 0.5) +
    scale_colour_discrete_divergingx(palette="Geyser") +
    scale_fill_discrete_divergingx(palette="Geyser") +
    theme(aspect.ratio = 1,
          panel.background=element_rect(fill=NA, colour="black"),
          axis.text = element_blank(),
          axis.ticks = element_blank())


## ----------------------------------------------------------------------------
#| label: fig-shapley-pcp
#| echo: false
#| fig-width: 4
#| fig-height: 4
#| out-width: 90%
#| fig-cap: "Parallel coordinates" 
#| fig-alt: "This is an untitled chart with no subtitle or caption. It has x-axis with labels bl, bd, fl and bm, with vertical lines marking these positions. It has y-axis 'SHAP' with labels -0.25, 0.00, 0.25 and 0.50. In this chart colour is used to show the error, but there is no legend. It has linewidth set to 1.5, and alpha set to 0.3. There are 221 lines connecting points on each of the bl, bd, fl, bm axes, mostly concentrating around SHAP=0. The lines fan out on the bm axis, with the one orange-brown line dipping lower than all the green lines."
p_pcp


## ----------------------------------------------------------------------------
#| label: fig-shapley-sm
#| echo: false
#| fig-width: 5
#| fig-height: 5
#| out-width: 90%
#| fig-cap: "Scatterplot matrix" 
#| fig-alt: "A matrix of 16 plots. On the diagonal there are histograms showing the distributions of bl, bd, fl and bm, respectively. In both the upper and lower triangle the pairwise plots of the four variables are shown. Colour is mapped to error, with one point being orange-brown and the others all green. This point is most noticeably different whenever bm is displayed because it has a lower value. There are a few other outliers, that have low values for bl or high values of fl. Positive association can be seen in bl vs bd. Negative association can be seen in bl vs fl, bl vs bm, bd vs bm. Weak association is seen in bd vs fl. "
p_sm


## ----echo=knitr::is_html_output()--------------------------------------------
#| label: fig-penguins-bl-bm-bd
#| code-fold: true
#| fig-width: 8
#| fig-height: 6
#| out-width: 100%
#| fig-cap: "Plots of the training data with misclassified observations marked to help understand what the SHAP values. The misclassified Gentoo penguin has an unusually low body mass value which makes it appear to be more like an Adelie penguin, particularly when considered in relation to it's bill length." 
#| fig-alt: "Three scatterplots of bl, bd, fl vs bm. Colour is mapped to species. Shape is matched to error, with solid circles indicating a misclassification. There is one Gentoo error and five Adelie errors. In bd  vs bm species show a more linear separation, but some overlap in the boundary region between Adelie (blue) and Chinstrap (yellow). The misclassified Gentoo is at the bottom left edge of the Gentoo cluster. The Adelie errors are mostly in the overlapping regions between Adelie and Chinstrap, in bl vs bm, and intermixed in these clusters when they overlap in fl vs bm and bd vs bm."
# Check position on bm
shap_proj <- p_exp_gentoo |>
  filter(species == "Gentoo", error == "yes") |>
  select(bl:bm)
shap_proj <- tourr::normalise(shap_proj) #as.matrix(shap_proj/sqrt(sum(shap_proj^2)))
p_exp_gentoo_proj <- p_exp_gentoo |>
  rename(shap_bl = bl, 
         shap_bd = bd,
         shap_fl = fl, 
         shap_bm = bm) |>
  bind_cols(as_tibble(p_train_x)) |>
  mutate(shap1 = shap_proj[1]*bl+
           shap_proj[2]*bd+
           shap_proj[3]*fl+
           shap_proj[4]*bm)
sp1 <- ggplot(p_exp_gentoo_proj, aes(x=bm, y=bl, 
             colour=species, 
             shape=factor(error))) + #factor(1-error))) +
    geom_point(alpha=0.8) +
  scale_colour_discrete_divergingx(palette="Zissou 1") +
  scale_shape_manual("error", values=c(1, 19)) +
  theme_minimal() + 
  theme(aspect.ratio=1, 
        legend.position="bottom", 
        legend.direction="horizontal")
sp2 <- ggplot(p_exp_gentoo_proj, aes(x=bm, y=shap1, 
             colour=species, 
             shape=factor(error))) + #factor(1-error))) +
    geom_point(alpha=0.8) +
  scale_colour_discrete_divergingx(palette="Zissou 1") +
  scale_shape_manual("error", values=c(19, 1)) +
  ylab("SHAP") +
  theme_minimal() + 
  theme(aspect.ratio=1, 
        legend.position="bottom",
        legend.direction = "horizontal",
        axis.text = element_blank())
sp2 <- ggplot(p_exp_gentoo_proj, aes(x=shap1, 
             fill=species, colour=species)) +
  geom_density(alpha=0.5) +
  geom_vline(xintercept = p_exp_gentoo_proj$shap1[
    p_exp_gentoo_proj$species=="Gentoo" &
    p_exp_gentoo_proj$error==1], colour="black") +
  scale_fill_discrete_divergingx(palette="Zissou 1") +
  scale_colour_discrete_divergingx(palette="Zissou 1") +
  theme_minimal() + 
  theme(aspect.ratio=1, legend.position="bottom")
sp2 <- ggplot(p_exp_gentoo_proj, aes(x=bm, y=bd, 
             colour=species, 
             shape=factor(error))) + #factor(1-error))) +
    geom_point(alpha=0.8) +
  scale_colour_discrete_divergingx(palette="Zissou 1") +
  scale_shape_manual("error", values=c(1, 19)) +
  theme_minimal() + 
  theme(aspect.ratio=1, 
        legend.position="bottom",
        legend.direction = "horizontal",
        axis.text = element_blank())
sp3 <- ggplot(p_exp_gentoo_proj, aes(x=bm, y=fl, 
             colour=species, 
             shape=factor(error))) + #factor(1-error))) +
    geom_point(alpha=0.8) +
  scale_colour_discrete_divergingx(palette="Zissou 1") +
  scale_shape_manual("error", values=c(1, 19)) +
  theme_minimal() + 
  theme(aspect.ratio=1, 
        legend.position="bottom",
        legend.direction = "horizontal",
        axis.text = element_blank())
sp1 + sp2 + sp3 + plot_layout(ncol=3, guides = "collect") &
  theme(legend.position="bottom",
        legend.direction = "horizontal")


## ----echo=knitr::is_html_output()--------------------------------------------
#| eval: false
# # Need to do the predictions and save because saved model
# # appears to be machine-dependent
# n <- 10000
# p_sim <- tibble(bl = runif(n, min(penguins_sub$bl), max(penguins_sub$bl)),
#                 bd = runif(n, min(penguins_sub$bd), max(penguins_sub$bd)),
#                 fl = runif(n, min(penguins_sub$fl), max(penguins_sub$fl)),
#                 bm = runif(n, min(penguins_sub$bm), max(penguins_sub$bm))) |>
#   as.matrix()
# p_sim_pred <- p_nn_model |>
#   predict(p_sim, verbose = 0)
# colnames(p_sim_pred) <- c("Adelie", "Chinstrap", "Gentoo")
# save(p_sim_pred, file="data/p_sim_pred.rda")
# save(p_sim, file="data/p_sim.rda")


## ----echo=knitr::is_html_output()--------------------------------------------
#| label: fig-penguins-bndry
#| code-fold: true
#| fig-width: 8
#| fig-height: 6
#| out-width: 100%
#| fig-cap: "Pairwise plots of classification boundaries (pixel points) to examine where the misclassification happens. The observed training data is overlaid with solid circle indicating a classification error. The boundary of this model falls almost entirely in body mass, with small contribution of the other variables, as seen by the difference between the two classes being mostly visible in the vertical direction." 
#| fig-alt: "A set of three scatter plots showing relationships between bm (x-axis) and bl, bd, fl (y-axis), with data points color-coded by species: Adelie (blue) and Gentoo (red). Points are also marked by classification error, where error = yes (filled circles) and 'error = no' (open circles). In the first plot there is a dense distribution of data points, with a slightly curved decision boundary separating Adelie and Gentoo. Misclassified points appear mostly along this boundary. The second plot has a more linear decision boundary that does not neatly match the separation between the two species. Some misclassified points are along this boundary. If the boundary had been centred through the middle of the lower left to upper right gap then these points would be far from the boundary and unlikely to be misclassified. In the third plot the boundary is mostly linear and vertical. "
load("data/p_sim_pred.rda")
load("data/p_sim.rda")
p_sim_class <- apply(p_sim_pred, 1, which.max)
p_sim_class <- c("Adelie", "Chinstrap", "Gentoo")[p_sim_class]
p_sim_pred <- p_sim_pred |>
  as_tibble() |>
  mutate(species = factor(p_sim_class))
p_sim <- p_sim |>
  as_tibble() |>
  mutate(species = factor(p_sim_class))
# animate_slice(p_sim[,1:4], col=p_sim$species, v_rel=0.6, axes="bottomleft")

p_sim_a_g <- p_sim |>
  filter(species != "Chinstrap")
bd1 <- p_sim_a_g |>
  ggplot() + 
    geom_point(aes(x=bm, y=bl, colour=species), shape=20, size=0.01) +
#    geom_point(data=filter(p_exp_gentoo_proj, species != "Chinstrap"),
#               aes(x=bm, y=bl, 
#               colour=species, 
#               shape=factor(error)), alpha=0.8) +
  scale_colour_discrete_divergingx(palette="Zissou 1") +
  scale_shape_manual("error", values=c(1, 19)) +
  theme_minimal() + 
  theme(aspect.ratio=1, 
        legend.position="bottom", 
        legend.direction="horizontal",
        axis.text = element_blank())

bd2 <- p_sim_a_g |>
  ggplot() + 
    geom_point(aes(x=bm, y=bd, colour=species), shape=20, size=0.01) +
    geom_point(data=filter(p_exp_gentoo_proj, species != "Chinstrap"),
               aes(x=bm, y=bd, 
               colour=species, 
               shape=factor(error)), alpha=0.8) +
  scale_colour_discrete_divergingx(palette="Zissou 1") +
  scale_shape_manual("error", values=c(1, 19)) +
  theme_minimal() + 
  theme(aspect.ratio=1, 
        legend.position="bottom", 
        legend.direction="horizontal",
        axis.text = element_blank())

bd3 <- p_sim_a_g |>
  filter(species != "Chinstrap") |>
  ggplot() + 
    geom_point(aes(x=bm, y=fl, colour=species), shape=20, size=0.01) +
    geom_point(data=filter(p_exp_gentoo_proj, species != "Chinstrap"),
               aes(x=bm, y=fl, 
               colour=species, 
               shape=factor(error)), alpha=0.8) +
  scale_colour_discrete_divergingx(palette="Zissou 1") +
  scale_shape_manual("error", values=c(1, 19)) +
  theme_minimal() + 
  theme(aspect.ratio=1, 
        legend.position="bottom", 
        legend.direction="horizontal",
        axis.text = element_blank())

bd1 + bd2 + bd3 + plot_layout(ncol=3, guides = "collect") &
  theme(legend.position="bottom",
        legend.direction = "horizontal")



## ----------------------------------------------------------------------------
#| eval: false
#| echo: false
# prj <- tourr::basis_random(4, 2)
# prj[,1] <- as.numeric(shap_proj)
# prj[,2] <- c(1,0,0,0)
# prj <- tourr::orthonormalise(prj)
# p_sim_a_g <- p_sim_a_g |>
#   mutate(species = factor(species))
# p_bndry_path <- save_history(p_sim_a_g[,1:4], radial_tour(prj, mvar=1),
#                              max_bases = 12)
# p_bndry_path_i <- interpolate(p_bndry_path, angle=0.01)
# animate_xy(p_sim_a_g[,1:4], radial_tour(prj, mvar=1),
#            col=p_sim_a_g$species, axes="bottomleft")
# animate_xy(p_sim_a_g[,1:4], planned_tour(p_bndry_path),
#            col=p_sim_a_g$species, axes="bottomleft")
# render_gif(p_sim_a_g[,1:4],
#            radial_tour(prj, mvar=1),
#            display_xy(col=p_sim_a_g$species,
#                       axes="bottomleft"),
#            gif_file = "gifs/p_nn_bndry.gif",
#            frames = 1000,
#            width = 400,
#            height = 400
# )
# render_gif(p_sim_a_g[,1:4],
#            planned_tour(p_bndry_path),
#            display_xy(col=p_sim_a_g$species,
#                       axes="bottomleft"),
#            gif_file = "gifs/p_nn_bndry2.gif",
#            frames = 1000,
#            width = 400,
#            height = 400
# )
# 

