## ----echo=knitr::is_html_output()------------------------------------------
#| message: false
#| label: fig-p-bl-bd-tree
#| fig-width: 6
#| fig-height: 3
#| out-width: 100%
#| code-summary: "Draw tree and model boundaries"
#| fig-cap: "The association between variables in the penguins data causes problems for fitting a tree model. Although the model, computed using only bl and bd, is simple (left), the fit is poor (right) because it doesn't adequately utilise combinations of variables."
#| fig-alt: "Tree diagram with top split bl<0.3004, leading to Adelie branch, second split at bd >= -0.4138, leading to Gentoo branch, and final split at bl< 0.1476, leading to Adelie and Chinstrap branches. The scatterplot at right shows bd vs bl, with three predictive region partitions, and the data is overplotted. The elliptical spreads of data points crosses the rectangular partitions in places."
library(mulgar)
library(rpart)
library(rpart.plot)
library(colorspace)
library(classifly)
library(ggplot2)
library(ggdendro)
library(patchwork)
library(ggthemes)

load("data/penguins_sub.rda")
p_bl_bd_tree <- rpart(species~bl+bd, data=penguins_sub)
#f1 <- rpart.plot(p_bl_bd_tree, box.palette="Grays")
d <- dendro_data(p_bl_bd_tree)
f1 <- ggplot() +
  geom_segment(data = d$segments, 
               aes(x = x, y = y, 
                   xend = xend, yend = yend)) +
  geom_text(data = d$labels, 
            aes(x = x, y = y, 
                label = label), size = 2.7, 
            vjust = 1.2) +
  geom_text(data = d$leaf_labels,
            aes(x = x, y = y, 
                label = label), size = 2.5, 
            vjust = 2, hjust=c(0,0.5,0,0.5)) + 
  expand_limits(x=0.9, y=0) +
  theme_dendro()

p_bl_bd_tree_boundaries <- explore(p_bl_bd_tree, penguins_sub)
f2 <- ggplot(p_bl_bd_tree_boundaries) +
  geom_point(aes(x=bl, y=bd, colour=species, shape=.TYPE)) + 
  scale_color_discrete_divergingx(palette="Zissou 1") +
  scale_shape_manual(values=c(46, 16)) +
  theme_minimal() +
  theme(aspect.ratio = 1, legend.position = "none")

f1 + f2 + plot_layout(ncol=2)


## ----echo=knitr::is_html_output()------------------------------------------
#| eval: false
#| code-summary: "Code to make animated gifs of slice tour of boundaries"
# library(tourr)
# p_tree <- rpart(species~., data=penguins_sub[,1:5])
# rpart.plot(p_tree, box.palette="Grays")
# 
# p_tree_boundaries <- explore(p_tree, penguins_sub)
# animate_slice(p_tree_boundaries[p_tree_boundaries$.TYPE == "simulated",1:4], col=p_tree_boundaries[p_tree_boundaries$.TYPE == "simulated",6], v_rel=0.02, axes="bottomleft")
# load("data/penguins_tour_path.rda")
# render_gif(p_tree_boundaries[p_tree_boundaries$.TYPE == "simulated",1:4],
#            planned_tour(pt1),
#            display_slice(v_rel=0.02,
#              col=p_tree_boundaries[p_tree_boundaries$.TYPE == "simulated",6],
#              axes="bottomleft"),                     gif_file="gifs/penguins_tree_boundaries.gif",
#            frames=500,
#            loop=FALSE
#            )


## --------------------------------------------------------------------------
#| message: false
#| code-fold: false
library(randomForest)
library(dplyr)
penguins_rf <- randomForest(species~.,
                             data=penguins_sub[,1:5],
                             importance=TRUE)


## ----echo=knitr::is_html_output(), eval=knitr::is_html_output()------------
penguins_rf


## --------------------------------------------------------------------------
#| echo: false
options(digits=4)


## --------------------------------------------------------------------------
head(penguins_rf$votes, 5)


## --------------------------------------------------------------------------
#| code-summary: "Code to compute Helmert matrix"
geozoo::f_helmert(3)


## --------------------------------------------------------------------------
#| message: false
#| code-fold: false
# Project 4D into 3D
library(geozoo)
proj <- t(geozoo::f_helmert(3)[-1,])
p_rf_v_p <- as.matrix(penguins_rf$votes) %*% proj
colnames(p_rf_v_p) <- c("x1", "x2")
p_rf_v_p <- p_rf_v_p |>
  as.data.frame() |>
  mutate(species = penguins_sub$species)


## ----echo=knitr::is_html_output()------------------------------------------
#| code-fold: false
# Add simplex
simp <- simplex(p=2)
sp <- data.frame(cbind(simp$points), simp$points[c(2,3,1),])
colnames(sp) <- c("x1", "x2", "x3", "x4")
sp$species = sort(unique(penguins_sub$species))
p_ternary <- ggplot() +
  geom_segment(data=sp, aes(x=x1, y=x2, xend=x3, yend=x4)) +
  geom_text(data=sp, aes(x=x1, y=x2, label=species),
            nudge_x=c(-0.06, 0.07, 0),
            nudge_y=c(0.05, 0.05, -0.05)) +
  geom_point(data=p_rf_v_p, aes(x=x1, y=x2, 
                                colour=species), 
             size=2, alpha=0.5) +
  scale_color_discrete_divergingx(palette="Zissou 1") +
  theme_map() +
  theme(aspect.ratio=1, legend.position="none")


## ----echo=knitr::is_html_output()------------------------------------------
#| eval: false
#| code-summary: "Code to generate animated gifs"
# # Look at the votes matrix, in its 3D space
# animate_xy(penguins_rf$votes, col=penguins_sub$species)
# 
# # Save an animated gif
# render_gif(penguins_rf$votes,
#            grand_tour(),
#            display_xy(v_rel=0.02,
#              col=penguins_sub$species,
#              axes="bottomleft"),
#            gif_file="gifs/penguins_rf_votes.gif",
#            frames=500,
#            loop=FALSE
# )


## ----eval=knitr::is_html_output()------------------------------------------
#| echo: false
#| label: fig-p-votes-ggplot-html
#| fig-cap: 2D ternary diagram
#| fig-alt: "Black equilateral triangle surrounding points. Points are fall mostly on the edges of a 2D triangle in 3D. Red (Gentoo) is a tight cluster at one vertex. Blue (Adelie) and yellow (Chinstrap) concentrate at the other two vertices, with points intermixed spread all along the edge between the two vertices. There is one red point mixed with the yellow, and one blue point along the yellow-red edge, and one blue point in the middle of the triangle."
#| fig-width: 4
#| fig-height: 4
p_ternary


## --------------------------------------------------------------------------
#| code-fold: false
library(mulgar)
library(dplyr)
library(liminal)
ft_pca <- prcomp(fake_trees[,1:100], 
                 scale=TRUE, retx=TRUE)
ft_pc <- as.data.frame(ft_pca$x[,1:10])
ft_pc$branches <- fake_trees$branches
library(randomForest)
ft_rf <- randomForest(branches~., data=ft_pc, 
                            importance=TRUE)


## --------------------------------------------------------------------------
#| echo: false
options(digits=1)


## --------------------------------------------------------------------------
head(ft_rf$votes, 5)


## --------------------------------------------------------------------------
#| code-fold: false
ft_rf_votes <- ft_rf$votes |>
  as_tibble() |>
  mutate(branches = fake_trees$branches)

proj <- t(geozoo::f_helmert(10)[-1,])
f_rf_v_p <- as.matrix(ft_rf_votes[,1:10]) %*% proj
colnames(f_rf_v_p) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9")
f_rf_v_p <- f_rf_v_p |>
  as.data.frame() |>
  mutate(branches = fake_trees$branches)

simp <- geozoo::simplex(p=9)
sp <- data.frame(simp$points)
colnames(sp) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9")
sp$branches = ""
f_rf_v_p_s <- bind_rows(sp, f_rf_v_p) |>
  mutate(branches = factor(branches))
labels <- c("0" , "1", "2", "3", "4", "5", "6", "7", "8", "9",
                rep("", 3000))


## --------------------------------------------------------------------------
#| eval: false
#| code-summary: "Code to make animated gifs"
# animate_xy(f_rf_v_p_s[,1:9], col = f_rf_v_p_s$branches,
#            axes = "off", half_range = 0.8,
#            edges = as.matrix(simp$edges),
#            obs_labels = labels, palette = "Viridis")
# 
# render_gif(f_rf_v_p_s[,1:9],
#            grand_tour(),
#            display_xy(col = f_rf_v_p_s$branches,
#            axes = "off", half_range = 0.8,
#            edges = as.matrix(simp$edges),
#            obs_labels = labels, palette="Viridis"),
#            gif_file="gifs/ft_votes.gif",
#            frames=500)


## ----echo=knitr::is_html_output()------------------------------------------
#| code-fold: false
#| label: tbl-ft-importance
#| tbl-cap: Variable importance from the random forest fit to the fake trees data, for each of the 9 classes, and using the accuracy and Gini metrics.
library(gt)
ft_rf$importance |> 
  as_tibble(rownames="Var") |> 
  rename(Acc=MeanDecreaseAccuracy,
         Gini=MeanDecreaseGini) |>
  #arrange(desc(Gini)) |>
  gt() |>
  fmt_number(columns = c(`0`,`1`,`2`,`3`,`4`,
                         `5`,`6`,`7`,`8`,`9`),
             decimals = 1) |> 
  fmt_number(columns = Acc,
             decimals = 2) |>
  fmt_number(columns = Gini,
             decimals = 0)


## --------------------------------------------------------------------------
#| code-fold: false
ft_pc <- ft_pc |>
  mutate(cl1 = factor(case_when(
                 branches == "0" ~ "0",
                 branches == "1" ~ "1",
                 .default = "other"
  )))


## ----echo=knitr::is_html_output()------------------------------------------
#| eval: false
#| code-summary: "Code to make animated gifs"
# animate_xy(ft_pc[,c("PC1", "PC2", "PC4", "PC6")], col=ft_pc$cl1, palette="Viridis")
# render_gif(ft_pc[,c("PC1", "PC2", "PC4", "PC6")],
#            grand_tour(),
#            display_xy(col=ft_pc$cl1, palette="Viridis"),
#            gif_file="gifs/ft_cl1.gif",
#            frames=500)


## ----echo=knitr::is_html_output()------------------------------------------
#| code-summary: "Code to make plot"
ft_pc_cl1 <- ggplot(ft_pc, aes(x=PC4, y=PC1, col=cl1)) +
  geom_point(alpha=0.7, size=1) +
  scale_color_discrete_sequential(palette="Viridis", rev=FALSE) +
  theme_minimal() +
  theme(aspect.ratio = 1)


## --------------------------------------------------------------------------
#| echo: false
#| fig-width: 4
#| fig-height: 4
#| out-width: 100%
#| label: fig-ft-cl1-pc
#| fig-cap: PC1 and PC4 together reveal cluster 1.
#| fig-alt: "This is an untitled chart with no subtitle or caption. It has x-axis 'PC4' with labels -5, 0 and 5. It has y-axis 'PC1' with labels -5, 0 and 5. Points are coloured by class groupings, deep purple is class 0, the main trunk, green is class 1, a branch, and all other classes are yellow. The class 1 is distinct from the other points pointing to 5 o'clock, and connected to one end of cluster 0."
ft_pc_cl1 


## --------------------------------------------------------------------------
#| echo: false
options(digits=2)


## --------------------------------------------------------------------------
ft_rf$confusion


## ----echo=knitr::is_html_output()------------------------------------------
#| code-fold: false
ft_pc <- ft_pc |>
  mutate(cl8 = factor(case_when(
                 branches == "0" ~ "0",
                 branches == "6" ~ "6",
                 branches == "1" ~ "1",
                 branches == "8" ~ "8",
                 .default = "other"
  )))


## ----echo=knitr::is_html_output()------------------------------------------
#| eval: false
#| code-summary: "Code to make animated gif"
# animate_xy(ft_pc[,c("PC1", "PC2", "PC4", "PC5", "PC6")], col=ft_pc$cl8, palette="Viridis")
# render_gif(ft_pc[,c("PC1", "PC2", "PC4", "PC5", "PC6")],
#            grand_tour(),
#            display_xy(col=ft_pc$cl8, palette="Viridis"),
#            gif_file="gifs/ft_cl8.gif",
#            frames=500)


## ----echo=knitr::is_html_output()------------------------------------------
#| code-summary: "Code to make plot"
ft_pc_cl8 <- ggplot(ft_pc, aes(x=PC1, y=PC5, col=cl8)) +
  geom_point(alpha=0.7, size=1) +
  scale_color_discrete_sequential(palette="Viridis", rev=FALSE) +
  theme_minimal() +
  theme(aspect.ratio = 1)


## --------------------------------------------------------------------------
#| echo: false
#| fig-width: 4
#| fig-height: 4
#| out-width: 100%
#| label: fig-ft-cl8-pc
#| fig-cap: PC1 and PC5 together mostly reveal cluster 8.
#| fig-alt: "This is an untitled chart with no subtitle or caption. It has x-axis 'PC1' with labels -5, 0 and 5. It has y-axis 'PC6' with labels -6, -3, 0 and 3, 6. Points are coloured by class groupings, deep purple is class 0, the main trunk, the rest are branches, where light green is class 8, middle green is class 6 and dark green is class 1. All other classes are yellow. The classes 1, 6, 8 are each distinct from the other points and connected to one end of cluster 0."
ft_pc_cl8 


## --------------------------------------------------------------------------
library(mulgar)
data(bushfires)
bushfires_sub <- bushfires[,c(5, 8:45, 48:55, 57:60)] |>
  mutate(cause = factor(cause))


## --------------------------------------------------------------------------
#| eval: false
# library(mulgar)
# data(aflw)
# aflw_sub <- aflw |>
#   dplyr::filter(position %in% c("BPL", "FF", "RR")) |>
#   dplyr::mutate(position = factor(position)) |>
#   dplyr::select(goals:tackles)


## --------------------------------------------------------------------------
#| echo: true
#| eval: false
# library(readr)
# library(dplyr)
# music <- read_csv("http://ggobi.org/book/data/music-sub.csv",
#                   show_col_types = FALSE) |>
#   rename(title = `...1`) |>
#   mutate(type = factor(type))


## --------------------------------------------------------------------------
#| eval: false
#| echo: false
# library(mulgar)
# library(tourr)
# data(bushfires)
# 
# bushfires_sub <- bushfires[,c(5, 8:45, 48:55, 57:60)] |>
#   mutate(cause = factor(cause))
# 
# # Checking the dependencies between predictors
# bushfires_pca <- prcomp(bushfires_sub[,-51],
#                         scale=TRUE, retx=TRUE)
# ggscree(bushfires_pca)
# 
# bushfires_pcs <- bushfires_pca$x[,1:7] |>
#   as_tibble() |>
#   mutate(cause = factor(bushfires$cause))
# 
# library(tourr)
# animate_xy(bushfires_pcs[,1:7],
#            guided_tour(lda_pp(bushfires_pcs$cause)),
#            col=bushfires_pcs$cause)
# 
# bushfires_pca$rotation[,2]
# ggplot(bushfires, aes(x=FOR_CODE)) + geom_density()
# ggplot(bushfires, aes(x=COVER)) + geom_density()
# ggplot(bushfires, aes(x=HEIGHT)) + geom_density()
# ggplot(bushfires, aes(x=FOREST)) + geom_density()
# ggplot(bushfires, aes(x=arf28)) + geom_density()
# 
# library(randomForest)
# bushfires_rf <- randomForest(cause~.,
#                              data=bushfires_sub,
#                              importance=TRUE)
# bushfires_rf
# 
# # Create votes matrix data
# bushfires_rf_votes <- bushfires_rf$votes |>
#   as_tibble() |>
#   mutate(cause = bushfires_sub$cause)
# 
# # Project 4D into 3D
# library(geozoo)
# proj <- t(geozoo::f_helmert(4)[-1,])
# b_rf_v_p <- as.matrix(bushfires_rf_votes[,1:4]) %*% proj
# colnames(b_rf_v_p) <- c("x1", "x2", "x3")
# b_rf_v_p <- b_rf_v_p |>
#   as.data.frame() |>
#   mutate(cause = bushfires_sub$cause)
# 
# # Add simplex
# simp <- simplex(p=3)
# sp <- data.frame(simp$points)
# colnames(sp) <- c("x1", "x2", "x3")
# sp$cause = ""
# b_rf_v_p_s <- bind_rows(sp, b_rf_v_p) |>
#   mutate(cause = factor(cause))
# labels <- c("accident" , "arson",
#                 "burning_off", "lightning",
#                 rep("", nrow(b_rf_v_p)))
# 
# # Check votes matrix
# animate_xy(bushfires_rf_votes[,1:4],
#            col=bushfires_rf_votes$cause)
# 
# # Examine votes matrix with bounding simplex
# animate_xy(b_rf_v_p_s[,1:3], col = b_rf_v_p_s$cause,
#            axes = "off", half_range = 1.3,
#            edges = as.matrix(simp$edges),
#            obs_labels = labels)
# render_gif(b_rf_v_p_s[,1:3],
#            grand_tour(),
#            display_xy(col = b_rf_v_p_s$cause,
#            axes = "off", half_range = 1.3,
#            edges = as.matrix(simp$edges),
#            obs_labels = labels),
#            gif_file="gifs/bushfires_votes.gif",
#            frames=500)
# 
# library(gt)
# bushfires_rf$importance |>
#   as_tibble(rownames="Variable") |>
#   rename(Accuracy=MeanDecreaseAccuracy,
#          Gini=MeanDecreaseGini) |>
#   arrange(desc(Gini)) |>
#   gt() |>
#   fmt_number(columns = c(accident, arson, burning_off, lightning, Accuracy),
#              decimals = 4) |>
#   fmt_number(columns = Gini,
#              decimals = 2)
# 


## --------------------------------------------------------------------------
#| eval: false
#| echo: false
# # Answer to Q3
# There are four classes: accident, arson, burning_off, lightning. It is highly imbalanced, with most observations belonging to the lightning class, fires ignited by lightning.
# 
# We can see that most of the observations lie on the face of lightning, arson and accident. The handful of the burning_off observations lie off this plane, in the direction of burning-off, so are less confused with the other three classes. This could be expected because burning off is highly regulated, and tends to occur before the bushfire season is at risk of starting. The arson cases are hard to classify, frequently confused with lightning or accident, and occasionally burning off. Lightning and accident have many more observations that are confidently classified correctly.


## --------------------------------------------------------------------------
#| eval: false
#| echo: false
# library(mulgar)
# library(dplyr)
# data("sketches_train")
# sketches_pca <- prcomp(sketches_train[,1:784])
# ggscree(sketches_pca, q=25, guide=FALSE)
# sketches_pc <- as.data.frame(sketches_pca$x[,1:21])
# sketches_pc$word <- sketches_train$word
# 
# library(tourr)
# animate_xy(sketches_pc[,1:6],
#            tour=guided_tour(lda_pp(sketches_pc$word)),
#            col=sketches_pc$word)
# library(randomForest)
# sketches_rf <- randomForest(word~., data=sketches_pc,
#                             mtry=5, ntree=2500,
#                             importance=TRUE)
# sketches_rf$importance
# # This would be a good one to explain how to explore multiclass
# # Difference PCs are more important for some classes
# # Create new binary classes to explore.
# 
# sketches_rf_votes <- sketches_rf$votes |>
#   as_tibble() |>
#   mutate(word = sketches_train$word)
# 
# proj <- t(geozoo::f_helmert(6)[-1,])
# s_rf_v_p <- as.matrix(sketches_rf_votes[,1:6]) %*% proj
# colnames(s_rf_v_p) <- c("x1", "x2", "x3", "x4", "x5")
# s_rf_v_p <- s_rf_v_p |>
#   as.data.frame() |>
#   mutate(word = sketches_train$word)
# 
# simp <- geozoo::simplex(p=5)
# sp <- data.frame(simp$points)
# colnames(sp) <- c("x1", "x2", "x3", "x4", "x5")
# sp$word = ""
# s_rf_v_p_s <- bind_rows(sp, s_rf_v_p) |>
#   mutate(word = factor(word))
# labels <- c("banana" , "boomerang",
#                 "cactus", "crab", "flip flops", "kangaroo",
#                 rep("", 5998))
# animate_xy(s_rf_v_p_s[,1:5], col = s_rf_v_p_s$word,
#            axes = "off", half_range = 0.8,
#            edges = as.matrix(simp$edges),
#            obs_labels = labels)
# 
# render_gif(s_rf_v_p_s[,1:5],
#            grand_tour(),
#            display_xy(col = s_rf_v_p_s$cause,
#            axes = "off", half_range = 0.8,
#            edges = as.matrix(simp$edges),
#            obs_labels = labels),
#            gif_file="gifs/sketches_votes.gif",
#            frames=500)


## --------------------------------------------------------------------------
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
# aflw_rf <- randomForest(position~., data=aflw_sub)
# ggplot(aflw_sub, aes(x=disposals, y=kicks, colour=position)) +
#   geom_point(alpha = 0.8) +
#   scale_color_discrete_divergingx(palette = "Zissou 1")
# 
# animate_xy(aflw_sub[,c(1, 3, 5)], col=aflw_sub$position, rescale=TRUE)
# 
# # We learn that an average of kicks and disposals distinguishes RR from the other two positions, and that goals distinguishes FF and BPL.

