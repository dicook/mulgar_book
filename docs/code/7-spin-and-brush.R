#| eval: false
#| message: false
## # Following https://github.com/plotly/plotly.R/blob/master/demo/animation-tour-basic.R
## # TURN INTO A FUNCTION TO MAKE IT EASIER
## library(tourr)
## library(plotly)
## load("data/penguins_sub.rda")
## p_mat <- as.matrix(penguins_sub[,1:4])
## tour <- new_tour(p_mat,
##                  grand_tour(), NULL)
## 
## tour_dat <- function(step_size) {
##   step <- tour(step_size)
##   proj <- center(p_mat %*% step$proj)
##   data.frame(x = proj[,1], y = proj[,2],
##              species = penguins_sub$species,
##              id = 1:nrow(penguins_sub))
## }
## 
## proj_dat <- function(step_size) {
##   step <- tour(step_size)
##   data.frame(
##     x = step$proj[,1], y = step$proj[,2], measure = colnames(p_mat)
##  )
## }
## 
## steps <- c(0, rep(1/15, 150))
## stepz <- cumsum(steps)
## 
## # tidy version of tour data
## tour_dats <- lapply(steps, tour_dat)
## tour_datz <- Map(function(x, y) cbind(x, step = y),
##                  tour_dats, stepz)
## tour_data <- dplyr::bind_rows(tour_datz)
## 
## tour_data <- highlight_key(tour_data, ~id)
## 
## # tidy version of tour projection data
## proj_dats <- lapply(steps, proj_dat)
## proj_datz <- Map(function(x, y) cbind(x, step = y), proj_dats, stepz)
## proj_data <- dplyr::bind_rows(proj_datz)
## proj_data$x <- proj_data$x*3
## proj_data$y <- proj_data$y*3
## 
## ax <- list(
##   title = "",
##   range = c(-3, 3),
##   zeroline = FALSE
## )
## 
## # Set colors
## clrs <- grDevices::hcl.colors(6, palette="Zissou 1")
## 
## # for nicely formatted slider labels
## options(digits = 2)
## 
## p_b_s <- proj_data %>%
##   plot_ly(x = ~x, y = ~y, frame = ~step,
##           color = I("gray80"),
##           width=600, height=600) %>%
##   config(displaylogo = FALSE,
##          modeBarButtonsToRemove = c("sendDataToCloud", "editInChartStudio", "zoom2d", "zoomIn2d", "zoomOut2d", "pan2d", "drawclosedpath", "drawopenpath", "autoScale2d", "hoverClosestCartesian", "hoverCompareCartesian", "resetScale2d")) %>%
##   add_segments(xend = 0, yend = 0) %>%
##   add_text(text = ~measure) %>%
##   add_markers(color = I("black"), data = tour_data, text = ~id, ids = ~id, hoverinfo = "text") %>%
##   layout(xaxis = ax, yaxis = ax) %>%
##   hide_legend() %>%
##   animation_opts(50, transition = 0, redraw = FALSE) %>%
##   highlight(on = "plotly_selected",
##             off = "plotly_doubleclick",
##             color = clrs,
##             persistent = TRUE,
##             dynamic = TRUE,
##             opacityDim = 0.5)
## htmlwidgets::saveWidget(p_b_s,
##           file="html/penguins_brush_and_spin.html",
##           selfcontained = TRUE)
## 


#| eval: false
#| code-fold: false
## library(detourr)
## grDevices::hcl.colors(3, palette="Zissou 1")
## detour(penguins_sub[,1:4],
##        tour_aes(projection = bl:bm)) |>
##        tour_path(grand_tour(2), fps = 60,
##                  max_bases=20) |>
##        show_scatter(alpha = 0.7,
##                     axes = FALSE)


#| message: false
#| code-summary: "Code to make confusion matrix"
library(readr)
load("data/penguins_sub.rda")
detourr_penguins <- read_csv("data/detourr_penguins.csv")
table(penguins_sub$species, detourr_penguins$colour)


#| eval: false
#| echo: false
## # remotes::install_github("pfh/langevitour")
## # remotes::install_github("plotly/plotly.R")
## library(langevitour)
## library(crosstalk)
## shared <- SharedData$new(penguins_sub)
## 
## langevitourWidget <- langevitour(
##     penguins_sub[,1:4],
##     link=shared,
##     pointSize=2,
##     width=700, height=700)
## 
## library(liminal)
## limn_tour(fake_trees, dim1:dim10)


#| eval: false
#| code-fold: false
## library(detourr)
## 
## # Use a random starting basis because the first two variables make it too easy
## strt <- tourr::basis_random(10, 2)
## detour(multicluster,
##        tour_aes(projection = -group)) |>
##        tour_path(grand_tour(2), start=strt, fps = 60) |>
##        show_scatter(alpha = 0.7, axes = FALSE)
## 


#| eval: false
#| echo: false
## library(detourr)
## library(liminal)
## library(mulgar)
## data("fake_trees")
## 
## # Original data is 100D, so need to reduce dimension using PCA first
## ft_pca <- prcomp(fake_trees[,1:100],
##                  scale=TRUE, retx=TRUE)
## ggscree(ft_pca)
## detour(as.data.frame(ft_pca$x[,1:10]),
##        tour_aes(projection = PC1:PC10)) |>
##        tour_path(grand_tour(2), fps = 60, max_bases=50) |>
##        show_scatter(alpha = 0.7, axes = FALSE)
## 
## ft_sb <- read_csv("data/fake_trees_sb.csv")
## table(fake_trees$branches, ft_sb$colour)

