## ----echo=knitr::is_html_output()------------------------------------------
#| label: mc-libraries
#| message: FALSE
#| code-summary: "Load libraries"
library(dplyr)
library(kableExtra)
library(ggplot2)
library(mclust)
library(mulgar)
library(patchwork)
library(colorspace)
library(tourr)


## ----eval=knitr::is_html_output()------------------------------------------
#| label: tbl-covariances-html
#| tbl-cap: "Parameterizations of the covariance matrix."
#| echo: FALSE
#| message: FALSE
readr::read_csv('misc/mclust-covariances-html.csv') |>
  knitr::kable(align = c('c', 'c', 'c', 'c', 'c', 'c')) |>
  kableExtra::kable_styling(full_width = FALSE)


## ----eval=knitr::is_latex_output()-----------------------------------------
#| label: tbl-covariances-pdf
#| tbl-cap: "Parameterizations of the covariance matrix."
#| echo: FALSE
#| message: FALSE
# readr::read_csv('misc/mclust-covariances-latex.csv') |>
#   knitr::kable(align = c('c', 'c', 'c', 'c', 'c', 'c'),
#                format="latex", booktabs = T,
#                escape = FALSE) |>
#   kableExtra::kable_styling(full_width = FALSE)


## ----echo=knitr::is_html_output()------------------------------------------
#| label: fig-penguins-bl-fl
#| fig-cap: "Scatterplot of flipper length by bill length of the penguins data, overlaid with density contours. Examining 2D data using a 2D density plot can help assess whether the elliptical variance-covariance model is appropriate for the data. In this case it looks reasonable."
#| fig-alt: "The x axis is labelled bl and has tick marks at -2, -1, 0, 1, 2, 3, and the y axis is labelled fl with tick marks at -2, -1, 0, 1, 2. Black points and blue-green contour lines suggest three modes, one at bottom left, one at top right and one to the middle right. Contours are a little elliptical, except the third mode is only weakly formed."
#| fig-width: 5
#| fig-height: 5
#| out-width: 60%
#| code-summary: "Code to make plot"
load("data/penguins_sub.rda")
ggplot(penguins_sub, aes(x=bl, 
                         y=fl)) + #, 
                         #colour=species)) +
  geom_point() +
  geom_density2d(colour="#3B99B1") +
  theme_minimal() +
  theme(aspect.ratio = 1)


## --------------------------------------------------------------------------
#| label: fig-penguins-bl-fl-mc
#| message: FALSE
#| code-fold: false
#| fig-width: 8
#| fig-height: 4
#| out-width: 100%
#| fig-cap: "Summary plots from model-based clustering: (a) BIC values for clusters 2-9 of top four models, (b) variance-covariance ellipses and cluster means (+) corresponding to the best model. The best model is three-cluster EVE, which has differently shaped variance-covariances albeit the same volume and orientation."
#| fig-alt: "Two plots side by side. The left plot is a line plot, with x axis labelled Number of clusters and ticks at 2, 3, 4, 5, 6, 7, 8, 9, and y axis labelled BIC_vals and tick marks at -1650 and -1600. There are four coloured lines with different line types labelled EEE, EVE, EVV and VVE by the legend at top right. All peak at x=3 and decline rapidly at higher values. The highest point corresponds to EVE. The plot on the right has the same axes as the previous figure, but three colour groups with no legend. There are two main types of points with the darker ones forming three ellipses overlaying the scatter cloud, centred in each colour group. There is a plus in the middle of each ellipse shape."
# Fit the model, plot BIC, construct and plot ellipses
penguins_BIC <- mclustBIC(penguins_sub[,c(1,3)])
ggmc <- ggmcbic(penguins_BIC, cl=2:9, top=4) + 
  scale_color_discrete_divergingx(palette = "Roma") +
  ggtitle("(a)") +
  theme_minimal() 
penguins_mc <- Mclust(penguins_sub[,c(1,3)], 
                      G=3, 
                      modelNames = "EVE")
penguins_mce <- mc_ellipse(penguins_mc)
penguins_cl <- penguins_sub[,c(1,3)]
penguins_cl$cl <- factor(penguins_mc$classification)
ggell <- ggplot() +
   geom_point(data=penguins_cl, aes(x=bl, y=fl,
                                    colour=cl),
              alpha=0.3) +
   geom_point(data=penguins_mce$ell, aes(x=bl, y=fl,
                                         colour=cl),
              shape=16) +
   geom_point(data=penguins_mce$mn, aes(x=bl, y=fl,
                                        colour=cl),
              shape=3, size=2) +
  scale_color_discrete_divergingx(palette = "Zissou 1")  +
  theme_minimal() +
  theme(aspect.ratio=1, legend.position="none") +
  ggtitle("(b)")
ggmc + ggell + plot_layout(ncol=2)


## --------------------------------------------------------------------------
#| label: fig-penguins-bic
#| code-fold: false
#| fig-cap: "BIC values for the top models for 2-9 clusters on the penguins data. The interpretation is mixed: if one were to choose three clusters any of the variance-covariance models would be equally as good, but the very best model is the four-cluster VEE."
#| fig-alt: "This is a line plot, with x axis labelled Number of clusters and ticks at 2, 3, 4, 5, 6, 7, 8, 9, and y axis labelled BIC_vals and tick marks at -2600, -2550, -2500 and -2450. There are four coloured lines with different line types labelled EEE (solid brown), EVE (small yellow dashes), VEE (medium light blue dashes) and VVE (large dark blue dashes) by the legend at right. The peak is at x=4 and corresponds to VEE. This curve has high values at x=3 and 5 also. Most lines peak at x=3, with the EEE staying fairly flat up to x=9, and the other two declining."
#| fig-width: 6
#| fig-height: 4
#| fig-align: "center"
penguins_BIC <- mclustBIC(penguins_sub[,1:4])
ggmc <- ggmcbic(penguins_BIC, cl=2:9, top=7) + 
  scale_color_discrete_divergingx(palette = "Roma") +
  theme_minimal() 
ggmc


## --------------------------------------------------------------------------
#| label: best-mclust
#| code-fold: false
penguins_mc <- Mclust(penguins_sub[,1:4], 
                      G=4, 
                      modelNames = "VEE")
penguins_mce <- mc_ellipse(penguins_mc)
penguins_cl <- penguins_sub
penguins_cl$cl <- factor(penguins_mc$classification)

penguins_mc_data <- penguins_cl |>
  select(bl:bm, cl) |>
  mutate(type = "data") |>
  bind_rows(bind_cols(penguins_mce$ell,
                      type=rep("ellipse",
                               nrow(penguins_mce$ell)))) |>
  mutate(type = factor(type))


## ----echo=knitr::is_html_output()------------------------------------------
#| eval: FALSE
#| label: simpler-mclust
#| code-summary: "Code to make animated gifs"
# animate_xy(penguins_mc_data[,1:4],
#            col=penguins_mc_data$cl,
#            pch=c(4, 20 )[as.numeric(penguins_mc_data$type)],
#            axes="off")
# 
# load("data/penguins_tour_path.rda")
# render_gif(penguins_mc_data[,1:4],
#            planned_tour(pt1),
#            display_xy(col=penguins_mc_data$cl,
#                pch=c(4, 20)[
#                  as.numeric(penguins_mc_data$type)],
#                       axes="off",
#                half_range = 0.7),
#            gif_file="gifs/penguins_best_mc.gif",
#            frames=500,
#            loop=FALSE)
# 
# penguins_mc <- Mclust(penguins_sub[,1:4],
#                       G=3,
#                       modelNames = "EEE")
# penguins_mce <- mc_ellipse(penguins_mc)
# penguins_cl <- penguins_sub
# penguins_cl$cl <- factor(penguins_mc$classification)
# 
# penguins_mc_data <- penguins_cl |>
#   select(bl:bm, cl) |>
#   mutate(type = "data") |>
#   bind_rows(bind_cols(penguins_mce$ell,
#                       type=rep("ellipse",
#                                nrow(penguins_mce$ell)))) |>
#   mutate(type = factor(type))
# 
# animate_xy(penguins_mc_data[,1:4],
#            col=penguins_mc_data$cl,
#            pch=c(4, 20)[as.numeric(penguins_mc_data$type)],
#            axes="off")
# 
# # Save the animated gif
# load("data/penguins_tour_path.rda")
# render_gif(penguins_mc_data[,1:4],
#            planned_tour(pt1),
#            display_xy(col=penguins_mc_data$cl,
#                pch=c(4, 20)[
#                  as.numeric(penguins_mc_data$type)],
#                       axes="off",
#                half_range = 0.7),
#            gif_file="gifs/penguins_simpler_mc.gif",
#            frames=500,
#            loop=FALSE)


## --------------------------------------------------------------------------
#| eval: FALSE
#| echo: FALSE
# # Its surprising that the four cluster solution didn't
# # pick up the sexes of the Chinstraps, where there is
# # some bimodality. Instead it splits the well-separated
# # group into two! Something to raise in the recap chapter.
# penguins_cl |>
#   count(species, cl) |>
#   pivot_wider(names_from=cl,
#               values_from=n,
#               values_fill=0)

