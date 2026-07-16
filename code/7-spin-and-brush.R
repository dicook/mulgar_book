## ----echo=knitr::is_html_output()--------------------------------------------
#| code-summary: "Load libraries"
source("code/setup.R")


## ----------------------------------------------------------------------------
#| eval: false
#| code-fold: false
# grDevices::hcl.colors(3, palette="Zissou 1")
# detour(penguins_sub[,1:4],
#        tour_aes(projection = bl:bm)) |>
#        tour_path(grand_tour(2), fps = 60,
#                  max_bases=20) |>
#        show_scatter(alpha = 0.7,
#                     axes = FALSE)


## ----------------------------------------------------------------------------
#| message: false
#| code-summary: "Code to make confusion matrix"
load("data/penguins_sub.rda")
detourr_penguins <- read_csv("data/detourr_penguins.csv")
table(penguins_sub$species, detourr_penguins$colour)


## ----------------------------------------------------------------------------
#| eval: false
#| code-fold: false

# # Use a random starting basis because the
# # first two variables make it too easy
# strt <- tourr::basis_random(10, 2)
# detour(multicluster,
#        tour_aes(projection = -group)) |>
#        tour_path(grand_tour(2),
#                  start=strt, fps = 60) |>
#        show_scatter(alpha = 0.7,
#                     axes = FALSE)


## ----------------------------------------------------------------------------
#| eval: false
#| echo: false
# data("fake_trees")
# 
# # Original data is 100D, so need to reduce dimension using PCA first
# ft_pca <- prcomp(fake_trees[,1:100],
#                  scale=TRUE, retx=TRUE)
# ggscree(ft_pca)
# detour(as.data.frame(ft_pca$x[,1:10]),
#        tour_aes(projection = PC1:PC10)) |>
#        tour_path(grand_tour(2),
#                  fps = 60,
#                  max_bases=50) |>
#        show_scatter(alpha = 0.7,
#                     axes = FALSE)
# 
# ft_sb <- read_csv("data/fake_trees_sb.csv")
# table(fake_trees$branches, ft_sb$colour)

