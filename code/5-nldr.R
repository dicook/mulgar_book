## ----echo=knitr::is_html_output()--------------------------------------------
#| code-summary: "Load libraries"
source("code/setup.R")


## ----echo=knitr::is_html_output()--------------------------------------------
#| message: false
#| code-summary: Code for spiral plots
library(Rdimtools)
library(cardinalR)
n_obs <- 1652  
set.seed(259)
swiss <- swiss_roll(n_obs, num_noise = 0)
# Standardising ruins structure
# swiss <- apply(swiss, 2, function(x) (x-mean(x))/sd(x))
swiss[,3] <- swiss[,3] * 3 # This scale produces more recognisable results
# Compute distance from (0,0) in first two coordinates - spiral
swiss <- cbind(swiss, sqrt(swiss[,1]^2 +
                           swiss[,2]^2))
colnames(swiss) <- c("x", "y", "z", "d")
swiss_tbl <- as_tibble(swiss)
swiss_pca <- prcomp(swiss[,1:3], scale=FALSE)
swiss_tbl <- bind_cols(swiss_tbl, as_tibble(swiss_pca$x))
spiral1 <- ggplot(swiss_tbl, 
                  aes(x=PC2, 
                      y=PC3, 
                      colour=d)) +
  geom_point() +
  scale_colour_continuous_divergingx(palette="Zissou 1",
       mid=median(swiss_tbl$d)) + 
  theme_minimal() +
  theme(aspect.ratio=1, 
        axis.text = element_blank(),
        legend.position = "none")

swiss_iso <- do.isomap(swiss[,1:3],
  ndim=2, type=c("knn", 10), 
  weight=FALSE)$Y
colnames(swiss_iso) <- c("iso1", "iso2")
swiss_iso <- as_tibble(swiss_iso) |>
  bind_cols(swiss_tbl)
spiral2 <- ggplot(swiss_iso, 
                  aes(x=iso1, 
                      y=iso2, 
                      colour=d)) + 
  geom_point() +
  scale_colour_continuous_divergingx(palette="Zissou 1",
       mid=median(swiss_iso$d)) + 
  theme_minimal() +
  theme(aspect.ratio=1, 
        axis.text = element_blank(),
        legend.position = "none")

#spiral1 + spiral2 + plot_layout(ncol=2)


## ----------------------------------------------------------------------------
#| fig-width: 4
#| fig-height: 4
#| out-width: 100%
#| fig-cap: "PCA"
#| fig-alt: "This is an untitled chart has x-axis 'PC2' and y-axis 'PC3' with no labels or legend. Colour is used to show a variable d. The chart is a set of 1652 solid circle points laid out in a spiral pattern, with colours starting with one end of the rainbow smoothly transitioning the the other end of the rainbow scale at the other end of the spiral."
#| echo: false
spiral1


## ----------------------------------------------------------------------------
#| fig-width: 4
#| fig-height: 4
#| out-width: 100%
#| fig-cap: "isomap"
#| fig-alt: "This is an untitled chart has x-axis 'PC2' and y-axis 'PC3' with no labels or legend. Colour is used to show a variable d. The chart is a set of 1652 solid circle points laid out in a slightly squashed rectangle, with colours starting with one end of the rainbow on the left and smoothly transitioning the the other end of the rainbow scale at the right."
#| echo: false
spiral2


## ----------------------------------------------------------------------------
#| fig-width: 4
#| fig-height: 4
#| out-width: 100%
#| fig-cap: "PCA"
#| fig-alt: "This is an untitled chart has x-axis 'PC2' and y-axis 'PC3' with no labels or legend. Colour is used to show a variable d. The chart is a set of 1652 solid circle points laid out in a spiral pattern, with colours starting with one end of the rainbow smoothly transitioning the the other end of the rainbow scale at the other end of the spiral."
#| echo: false
spiral1


## ----------------------------------------------------------------------------
#| fig-width: 4
#| fig-height: 4
#| out-width: 100%
#| fig-cap: "isomap"
#| fig-alt: "This is an untitled chart has x-axis 'PC2' and y-axis 'PC3' with no labels or legend. Colour is used to show a variable d. The chart is a set of 1652 solid circle points laid out in a slightly squashed rectangle, with colours starting with one end of the rainbow on the left and smoothly transitioning the the other end of the rainbow scale at the right."
#| echo: false
spiral2


## ----------------------------------------------------------------------------
#| eval: false
#| echo: false
# swiss_tbl <- as_tibble(swiss)
# spiral1 <- ggplot(swiss_tbl,
#                   aes(x=x,
#                       y=y,
#                       colour=d)) +
#   geom_point() +
#   scale_colour_continuous_divergingx(palette="Zissou 1",
#        mid=median(swiss_tbl$d)) +
#   theme_minimal() +
#   theme(aspect.ratio=1,
#         axis.text = element_blank(),
#         legend.position = "none")
# swiss_pca <- prcomp(swiss[,1:3], scale=FALSE)
# ggscatmat(swiss_pca$x)
# ggplot(swiss_pca$x, aes(x=PC1, y=PC2)) + geom_point()
# swiss_mds <- cmdscale(dist(swiss))
# colnames(swiss_mds) <- c("mds1", "mds2")
# swiss_mds <- as_tibble(swiss_mds) |>
#   bind_cols(as_tibble(swiss))
# ggplot(swiss_mds, aes(x=mds1, y=mds2, colour=d)) +
#   geom_point() +
#   scale_colour_continuous_divergingx(palette="Zissou 1",
#        mid=median(swiss_mds$d)) +
#   theme(aspect.ratio=1)
# swiss_nmds <- monoMDS(dist(swiss[,1:3]),
#                       model = "local")$points
# colnames(swiss_nmds) <- c("nmds1", "nmds2")
# swiss_nmds <- as_tibble(swiss_nmds) |>
#   bind_cols(swiss_tbl)
# ggplot(swiss_nmds, aes(x=nmds1, y=nmds2, colour=d)) +
#   geom_point() +
#   scale_colour_continuous_divergingx(palette="Zissou 1",
#        mid=median(swiss_mds$d)) +
#   theme(aspect.ratio=1)
# 
# animate_xy(swiss[,1:3])
# animate_xy(swiss[,1:3], guided_tour(holes()))
# set.seed(432)
# render_gif(swiss[,1:3],
#            grand_tour(),
#            display_xy(axes="bottomleft"),
#            gif_file = "gifs/swiss.gif",
#            frames = 500,
#            width = 300,
#            height = 300)
# 
# swiss_lle <- do.lle(swiss2,
#   ndim=2, type=c("knn", 20), #  type=c("enn", 0.5),
#   weight=FALSE)$Y
# colnames(swiss_lle) <- c("lle1", "lle2")
# swiss_lle <- as_tibble(swiss_lle) |>
#   bind_cols(swiss_tbl)
# ggplot(swiss_lle, aes(x=lle1, y=lle2, colour=d)) +
#   geom_point() +
#   scale_colour_continuous_divergingx(palette="Zissou 1",
#        mid=median(swiss_lle$d)) +
#   theme(aspect.ratio=1)
# 
# swiss_df <- bind_cols(swiss, swiss_iso, swiss_lle)
# shared_swiss_df <- SharedData$new(swiss_df)
# p1 <- ggplot(shared_swiss_df, aes(x=iso1, y=iso2)) +
#   geom_point()
# gp1 <- ggplotly(p1, width=500, height=500) |>
#   highlight(on = "plotly_selected",
#               off = "plotly_doubleclick")
# p2 <- ggplot(shared_swiss_df, aes(x=x, y=y)) +
#   geom_point()
# gp2 <- ggplotly(p2, width=500, height=500) |>
#   highlight(on = "plotly_selected",
#               off = "plotly_doubleclick")
# p3 <- ggplot(shared_swiss_df, aes(x=lle1, y=lle2)) +
#   geom_point()
# gp3 <- ggplotly(p3, width=500, height=500) |>
#   highlight(on = "plotly_selected",
#               off = "plotly_doubleclick")
# 
# bscols(
#      gp1, gp2, gp3,
#      widths = c(4, 4, 4)
#  )
# 
# swiss_df <- bind_cols(swiss, swiss_iso)
# shared_swiss_df <- SharedData$new(swiss_df)
# p1 <- ggplot(shared_swiss_df, aes(x=iso1, y=iso2)) +
#   geom_point()
# gp1 <- ggplotly(p1, width=300, height=300) |>
#   highlight(on = "plotly_selected",
#               off = "plotly_doubleclick")
# p2 <- ggplot(shared_swiss_df, aes(x=x, y=y)) +
#   geom_point()
# gp2 <- ggplotly(p2, width=300, height=300) |>
#   highlight(on = "plotly_selected",
#               off = "plotly_doubleclick")
# 
# bscols(
#      gp1, gp2,
#      widths = c(5, 5)
#  )


## ----echo=knitr::is_html_output()--------------------------------------------
#| label: fig-nldr-clusters
#| fig-cap: "Two non-linear embeddings of the non-linear clusters data: (a) t-SNE, (b) UMAP. One suggests five clusters and the other four, and also disagree on the cluster shapes."
#| fig-alt: "This chart has two plots titled '(a) t-SNE', with x-axis 'tsne1' and y-axis 'tsne2', and '(b) UMAP' with x-axis 'umap1' anf y-axis 'umap2'. The chart '(a)' is a set of 1268 solid circle points arranged with a strip going from bottom let to top right, two C-shapes above and below the line and two small concetration of dots at the middle bottom and top. The points in chart '(b)' are arranged like a tilted smiley face."
#| fig-width: 8
#| fig-height: 4
#| out-width: 100%
#| message: false
#| code-summary: "Code to generate the 2D non-linear representation"

library(Rtsne)
library(uwot)

set.seed(44)
cnl_tsne <- Rtsne(clusters_nonlin)
cnl_umap <- umap(clusters_nonlin)
n1 <- ggplot(as.data.frame(cnl_tsne$Y), aes(x=V1, y=V2)) +
  geom_point() + 
  xlab("tsne1") +
  ylab("tsne2") +
  ggtitle("(a) t-SNE") +
  theme_minimal() + 
  theme(aspect.ratio=1, 
        axis.text = element_blank())
n2 <- ggplot(as.data.frame(cnl_umap), aes(x=V1, y=V2)) +
  geom_point() + 
  xlab("umap1") +
  ylab("umap2") +
  ggtitle("(b) UMAP") +
  theme_minimal() + 
  theme(aspect.ratio=1, 
        axis.text = element_blank())
n1 + n2


## ----------------------------------------------------------------------------
#| eval: false
#| echo: false
#| code-summary: "Code to create animated gif"

# render_gif(clusters_nonlin,
#            grand_tour(),
#            display_xy(),
#            gif_file = "gifs/clusters_nonlin.gif",
#            frames = 500,
#            width = 300,
#            height = 300)


## ----------------------------------------------------------------------------
#| message: FALSE
#| eval: false
#| code-fold: false
# umap_df <- data.frame(umapX = cnl_umap[, 1],
#                       umapY = cnl_umap[, 2])
# limn_tour_link(
#   umap_df,
#   clusters_nonlin,
#   cols = x1:x4
# )


## ----------------------------------------------------------------------------
#| message: false
#| eval: false
#| code-fold: false

# umap_df <- data.frame(umapX = cnl_umap[, 1],
#                       umapY = cnl_umap[, 2])
# cnl_df <- bind_cols(clusters_nonlin, umap_df)
# shared_cnl <- SharedData$new(cnl_df)
# 
# detour_plot <- detour(shared_cnl, tour_aes(
#   projection = starts_with("x"))) |>
#     tour_path(grand_tour(2),
#                     max_bases=50, fps = 60) |>
#        show_scatter(alpha = 0.7, axes = FALSE,
#                     width = "100%", height = "450px")
# 
# umap_plot <- plot_ly(shared_cnl,
#                     x = ~umapX,
#                     y = ~umapY,
#                     color = I("black"),
#                     height = 450) %>%
#     highlight(on = "plotly_selected",
#               off = "plotly_doubleclick") %>%
#     add_trace(type = "scatter",
#               mode = "markers")
# 
# bscols(
#      detour_plot, umap_plot,
#      widths = c(5, 6)
#  )


## ----------------------------------------------------------------------------
#| message: false
#| eval: false
#| code-summary: "Code to run liminal on the fake trees data"

# data(fake_trees)
# set.seed(2020)
# tsne <- Rtsne::Rtsne(
#   dplyr::select(fake_trees,
#                 dplyr::starts_with("dim")))
# tsne_df <- data.frame(tsneX = tsne$Y[, 1],
#                       tsneY = tsne$Y[, 2])
# limn_tour_link(
#   tsne_df,
#   fake_trees,
#   cols = dim1:dim10,
#   color = branches
# )


## ----------------------------------------------------------------------------
#| message: false
#| eval: false
#| echo: false

# set.seed(1240)
# cnl_tsne <- Rtsne(clusters_nonlin)
# cnl_umap <- umap(clusters_nonlin)
# n1 <- ggplot(as.data.frame(cnl_tsne$Y), aes(x=V1, y=V2)) +
#   geom_point() +
#   ggtitle("(a) t-SNE") +
#   theme_minimal() +
#   theme(aspect.ratio=1)
# n2 <- ggplot(as.data.frame(cnl_umap), aes(x=V1, y=V2)) +
#   geom_point() +
#   ggtitle("(b) UMAP") +
#   theme_minimal() +
#   theme(aspect.ratio=1)
# n1 + n2


## ----------------------------------------------------------------------------
#| message: false
# Answer to Q2
library(Rdimtools)
library(cardinalR)

n_obs <- 1652  
set.seed(259)
swiss <- swiss_roll(n_obs, num_noise = 0)
# Standardise
swiss_std <- apply(swiss, 2, function(x) (x-mean(x))/sd(x))
# Compute distance from (0,0) in first two coordinates - spiral
swiss_std <- cbind(swiss_std, sqrt(swiss[,1]^2 +
                           swiss[,2]^2))
colnames(swiss_std) <- c("x", "y", "z", "d")
swiss_tbl <- as_tibble(swiss_std)
swiss_pca <- prcomp(swiss_std[,1:3], scale=FALSE)
swiss_tbl <- bind_cols(swiss_tbl, as_tibble(swiss_pca$x))
spiral1 <- ggplot(swiss_tbl, 
                  aes(x=PC1, 
                      y=PC3, 
                      colour=d)) +
  geom_point() +
  scale_colour_continuous_divergingx(palette="Zissou 1",
       mid=median(swiss_tbl$d)) + 
  theme_minimal() +
  theme(aspect.ratio=1, 
        axis.text = element_blank(),
        legend.position = "none")

swiss_iso <- do.isomap(swiss_std[,1:3],
  ndim=2, type=c("knn", 30), 
  weight=FALSE)$Y
colnames(swiss_iso) <- c("iso1", "iso2")
swiss_iso <- as_tibble(swiss_iso) |>
  bind_cols(swiss_tbl)
spiral2 <- ggplot(swiss_iso, 
                  aes(x=iso1, 
                      y=iso2, 
                      colour=d)) + 
  geom_point() +
  scale_colour_continuous_divergingx(palette="Zissou 1",
       mid=median(swiss_iso$d)) + 
  theme_minimal() +
  theme(aspect.ratio=1, 
        axis.text = element_blank(),
        legend.position = "none")

spiral1 + spiral2 + plot_layout(ncol=2)


## ----------------------------------------------------------------------------
#| message: false
# Answer to Q3
n_obs <- 1652  
set.seed(259)
swiss <- swiss_roll(n_obs, num_noise = 0)
swiss[,3] <- swiss[,3] * 3 
swiss <- cbind(swiss, sqrt(swiss[,1]^2 +
                           swiss[,2]^2))
colnames(swiss) <- c("x", "y", "z", "d")
swiss_tbl <- as_tibble(swiss)
set.seed(111)
swiss_tsne <- Rtsne::Rtsne(swiss[,1:3])$Y
colnames(swiss_tsne) <- c("tsne1", "tsne2")
swiss_tsne <- as_tibble(swiss_tsne) |>
  bind_cols(swiss_tbl)
swiss_tsne <- ggplot(swiss_tsne, 
                  aes(x=tsne1, 
                      y=tsne2, 
                      colour=d)) +
  geom_point() +
  scale_colour_continuous_divergingx(palette="Zissou 1",
       mid=median(swiss_tbl$d)) + 
  theme_minimal() +
  theme(aspect.ratio=1, 
        axis.text = element_blank(),
        legend.position = "none")

set.seed(111)
swiss_umap <- uwot::umap(swiss[,1:3])
colnames(swiss_umap) <- c("umap1", "umap2")
swiss_umap <- as_tibble(swiss_umap) |>
  bind_cols(swiss_tbl)
swiss_umap <- ggplot(swiss_umap, 
                  aes(x=umap1, 
                      y=umap2, 
                      colour=d)) +
  geom_point() +
  scale_colour_continuous_divergingx(palette="Zissou 1",
       mid=median(swiss_tbl$d)) + 
  theme_minimal() +
  theme(aspect.ratio=1, 
        axis.text = element_blank(),
        legend.position = "none")

swiss_tsne + swiss_umap + plot_layout(ncol=2)


## ----------------------------------------------------------------------------
#| message: false
#| eval: false
#| echo: false
# load("data/penguins_sub.rda")
# 
# set.seed(2022)
# p_tsne <- Rtsne::Rtsne(penguins_sub[,2:5])
# p_tsne_df <- data.frame(tsneX = p_tsne$Y[, 1], tsneY = p_tsne$Y[, 2])
# limn_tour_link(
#   p_tsne_df,
#   penguins_sub,
#   cols = bl:bm,
#   color = species
# )
# # The t-SNE mapping of the penguins data inaccurately splits one of the clusters. The three clusters are clearly distinct when viewed with the tour.


## ----------------------------------------------------------------------------
#| message: false
#| eval: false
#| echo: false
# data(multicluster)
# animate_xy(multicluster[,2:11])
# 
# set.seed(129)
# mc_umap <- uwot::umap(multicluster[,2:11])
# colnames(mc_umap) <- c("umap1", "umap2")
# mc_umap <- as_tibble(mc_umap)
# ggplot(mc_umap, aes(x=umap1,
#                     y=umap2)) +
#   geom_point() +
#   theme_minimal() +
#   theme(aspect.ratio=1,
#         axis.text = element_blank())


## ----------------------------------------------------------------------------
#| message: false
#| eval: false
#| echo: false
# data(sketches_train)
# #animate_xy(sketches_train[,1:784])
# 
# set.seed(137)
# sketches_umap <- uwot::umap(sketches_train[,1:784])
# colnames(sketches_umap) <- c("umap1", "umap2")
# sketches_umap <- as_tibble(sketches_umap) |>
#   mutate(word = sketches_train$word)
# ggplot(sketches_umap, aes(x=umap1,
#                           y=umap2,
#                           colour=word)) +
#   geom_point() +
#   scale_colour_discrete_divergingx(palette = "Zissou 1") +
#   theme_minimal() +
#   theme(aspect.ratio=1,
#         axis.text = element_blank())


## ----------------------------------------------------------------------------
#| label: pbmc
#| message: false
#| eval: false
#| echo: false
# pbmc <- readRDS("data/pbmc_pca_50.rds")
# 
# # t-SNE
# set.seed(1041)
# p_tsne <- Rtsne::Rtsne(pbmc[,1:15])
# p_tsne_df <- data.frame(tsneX = p_tsne$Y[, 1], tsneY = p_tsne$Y[, 2])
# ggplot(p_tsne_df, aes(x=tsneX, y=tsneY)) + geom_point()
# 
# # UMAP
# set.seed(1045)
# p_umap <- uwot::umap(pbmc[,1:15])
# p_umap_df <- data.frame(umapX = p_umap[, 1], umapY = p_umap[, 2])
# ggplot(p_umap_df, aes(x=umapX, y=umapY)) + geom_point()

