#| label: fig-nldr-clusters
#| fig-cap: "Two non-linear embeddings of the non-linear clusters data: (a) t-SNE, (b) UMAP. Both suggest four clusters, with two being non-linear in some form."
#| fig-width: 8
#| fig-height: 4
#| message: false
#| code-summary: "Code to generate the 2D non-linear representation"
library(mulgar)
library(Rtsne)
library(uwot)
library(ggplot2)
library(patchwork)
set.seed(42)
cnl_tsne <- Rtsne(clusters_nonlin)
cnl_umap <- umap(clusters_nonlin)
n1 <- ggplot(as.data.frame(cnl_tsne$Y), aes(x=V1, y=V2)) +
  geom_point() + 
  ggtitle("(a) t-SNE") +
  theme_minimal() + 
  theme(aspect.ratio=1)
n2 <- ggplot(as.data.frame(cnl_umap), aes(x=V1, y=V2)) +
  geom_point() + 
  ggtitle("(b) UMAP") +
  theme_minimal() + 
  theme(aspect.ratio=1)
n1 + n2


#| eval: false
#| code-summary: "Code to create animated gif"
## library(tourr)
## render_gif(clusters_nonlin,
##            grand_tour(),
##            display_xy(),
##            gif_file = "gifs/clusters_nonlin.gif",
##            frames = 500,
##            width = 300,
##            height = 300)


#| message: FALSE
#| eval: false
#| code-fold: false
## library(liminal)
## umap_df <- data.frame(umapX = cnl_umap[, 1],
##                       umapY = cnl_umap[, 2])
## limn_tour_link(
##   umap_df,
##   clusters_nonlin,
##   cols = x1:x4
## )


#| message: false
#| eval: false
#| code-fold: false
## library(detourr)
## library(dplyr)
## library(crosstalk)
## library(plotly)
## umap_df <- data.frame(umapX = cnl_umap[, 1],
##                       umapY = cnl_umap[, 2])
## cnl_df <- bind_cols(clusters_nonlin, umap_df)
## shared_cnl <- SharedData$new(cnl_df)
## 
## detour_plot <- detour(shared_cnl, tour_aes(
##   projection = starts_with("x"))) |>
##     tour_path(grand_tour(2),
##                     max_bases=50, fps = 60) |>
##        show_scatter(alpha = 0.7, axes = FALSE,
##                     width = "100%", height = "450px")
## 
## umap_plot <- plot_ly(shared_cnl,
##                     x = ~umapX,
##                     y = ~umapY,
##                     color = I("black"),
##                     height = 450) %>%
##     highlight(on = "plotly_selected",
##               off = "plotly_doubleclick") %>%
##     add_trace(type = "scatter",
##               mode = "markers")
## 
## bscols(
##      detour_plot, umap_plot,
##      widths = c(5, 6)
##  )


#| message: false
#| eval: false
#| code-summary: "Code to run liminal on the fake trees data"
## library(liminal)
## library(Rtsne)
## data(fake_trees)
## set.seed(2020)
## tsne <- Rtsne::Rtsne(dplyr::select(fake_trees, dplyr::starts_with("dim")))
## tsne_df <- data.frame(tsneX = tsne$Y[, 1],
##                       tsneY = tsne$Y[, 2])
## limn_tour_link(
##   tsne_df,
##   fake_trees,
##   cols = dim1:dim10,
##   color = branches
## )


#| label: penguins-tsne
#| message: false
#| eval: false
#| echo: false
## load("data/penguins_sub.rda")
## 
## set.seed(2022)
## p_tsne <- Rtsne::Rtsne(penguins_sub[,2:5])
## p_tsne_df <- data.frame(tsneX = p_tsne$Y[, 1], tsneY = p_tsne$Y[, 2])
## limn_tour_link(
##   p_tsne_df,
##   penguins_sub,
##   cols = bl:bm,
##   color = species
## )

