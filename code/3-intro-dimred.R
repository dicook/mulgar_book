## ----echo=knitr::is_html_output()------------------------------------------
#| code-summary: "Code to produce 2D data examples"
library(tibble)
set.seed(6045)
x1 <- runif(123)
x2 <- x1 + rnorm(123, sd=0.1)
x3 <- rnorm(123, sd=0.2)
df <- tibble(x1 = (x1-mean(x1))/sd(x1), 
             x2 = (x2-mean(x2))/sd(x2),
             x3, 
             x3scaled = (x3-mean(x3))/sd(x3))


## --------------------------------------------------------------------------
#| echo: false
#| warning: false
#| message: false
library(ggplot2)
library(patchwork)
dp1 <- ggplot(df) + 
  geom_point(aes(x=x1, y=x2)) +
  xlim(-2.5, 2.5) + ylim(-2.5, 2.5) +
  annotate("segment", x=0, xend=2, y=0, yend=0) +
  annotate("segment", x=0, xend=0, y=0, yend=2) +
  annotate("text", x=2.3, y=0, label="x1") +
  annotate("text", x=0, y=2.3, label="x2") +
  ggtitle("(a) Reduced dimension") +
  theme_minimal() +
  theme(aspect.ratio=1,
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.border = element_rect(colour="black", fill=NA))
dp2 <- ggplot(df) + 
  geom_point(aes(x=x1, y=x3)) +
  xlim(-2.5, 2.5) + ylim(-2.5, 2.5) +
  annotate("segment", x=0, xend=2, y=0, yend=0) +
  annotate("segment", x=0, xend=0, y=0, yend=2) +
  annotate("text", x=2.3, y=0, label="x1") +
  annotate("text", x=0, y=2.3, label="x3") +
  ggtitle("(b) Reduced variance") +
  theme_minimal() +
  theme(aspect.ratio=1,
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.border = element_rect(colour="black", fill=NA))
dp3 <- ggplot(df) + 
  geom_point(aes(x=x1, y=x3scaled)) +
  xlim(-2.5, 2.5) + ylim(-3.5, 3.5) +
  annotate("segment", x=0, xend=2, y=0, yend=0) +
  annotate("segment", x=0, xend=0, y=0, yend=3) +
  annotate("text", x=2.3, y=0, label="x1") +
  annotate("text", x=0, y=3.3, label="x3") +
  ggtitle("(c) No reduced dimension") +
  theme_minimal() +
  theme(aspect.ratio=1,
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.border = element_rect(colour="black", fill=NA))


## --------------------------------------------------------------------------
#| label: fig-2D
#| echo: false
#| fig-width: 9
#| fig-height: 3
#| out-width: 100%
#| fig-cap: "Explanation of how dimension reduction is perceived in 2D, relative to variables: (a) Two variables with strong linear association. Both variables contribute to the association, as indicated by their axes extending out from the 'collapsed' direction of the points; (b) Two variables with no linear association. But x3 has less variation, so points collapse in this direction; (c) The situation in plot (b) does not arise in a tour because all variables are (usually) scaled.  When an axis extends out of a direction where the points are collapsed, it means that this variable is partially responsible for the reduced dimension."
#| fig-alt: "Three scatterplots: (a) points lie close to a straight line in the x=y direction, (b) points lie close to a horizontal line, (c) points spread out in the full plot region. There are no axis labels or scales."
dp1 + dp2 + dp3 + plot_layout(ncol=3)


## --------------------------------------------------------------------------
#| echo: false
#| warning: false
#| message: false
library(tidyr)
library(dplyr)
set.seed(1115)
d_form <- tibble(x = runif(100) - 0.5) |>
  mutate(
    linear = 4 * x + rnorm(100) * 0.5,
    nonlinear1 = 12 * x^2 + rnorm(100) * 0.5,
    nonlinear2 = 2 * x - 5 * x^2 + rnorm(100) * 0.1
  ) |>
  pivot_longer(
    cols = linear:nonlinear2, names_to = "form",
    values_to = "y"
  ) |>
  filter(form == "nonlinear2") |>
  select(form, x, y)

d_gaps <- tibble(x = runif(150)) |>
  mutate(y = runif(150))
d_gaps <- d_gaps |>
  filter(!(between(x + 2 * y, 1.2, 1.6)))

d_barrier <- tibble(x = runif(200)) |>
  mutate(y = runif(200))
d_barrier <- d_barrier |>
  filter(-x + 3 * y < 1.2)

l_shape <- tibble(
  x = c(rexp(50, 0.01), runif(50) * 20),
  y = c(runif(50) * 20, rexp(50, 0.01))
)

hetero <- tibble(x = runif(200) - 0.5) |>
  mutate(y = -2 * x + rnorm(200) * (x + 0.5))

d_clusters <- tibble(x = c(
  rnorm(50) / 6 - 0.5,
  rnorm(50) / 6,
  rnorm(50) / 6 + 0.5
)) |>
  mutate(y = c(
    rnorm(50) / 6,
    rnorm(50) / 6 + 1, rnorm(50) / 6
  ))



## --------------------------------------------------------------------------
#| label: fig-nonlin-2D
#| echo: false
#| fig-width: 9
#| fig-height: 6
#| out-width: 100%
#| fig-cap: "Other types of association: (a) non-linear, (b) gap between subspaces, (c) barrier beyond which no values are observed, perhaps a limiting inequality constraint, (d) L-shape where if one variable has a spread of values the other does not, (e) skewness or heterogeneous variance, (f) clustering."
pn1 <- ggplot(data=d_form, aes(x=x, y=y)) +
  geom_point() +
    ggtitle("(a) Nonlinear") +
  theme_minimal() +
  theme(aspect.ratio=1,
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.border = element_rect(colour="black", fill=NA))

pn2 <- ggplot(data=d_gaps, aes(x=x, y=y)) +
  geom_polygon(
    data = tibble(x = c(0, 1, 1, 0), y = c(1.2 / 2, 0.2 / 2, 0.6 / 2, 1.6 / 2)),
    fill = "#EAC024", alpha = 0.3
  ) +
  geom_point() +
    ggtitle("(b) Gaps") +
  theme_minimal() +
  theme(aspect.ratio=1,
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.border = element_rect(colour="black", fill=NA))

pn3 <- ggplot(data=d_barrier, aes(x=x, y=y)) +
  geom_polygon(
    data = tibble(x = c(0, 1, 1, 0), y = c(1.2 / 3, 2.2 / 3, 1, 1)),
    fill = "#EAC024", alpha = 0.3
  ) +
  geom_point() +
    ggtitle("(c) Barrier") +
  theme_minimal() +
  theme(aspect.ratio=1,
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.border = element_rect(colour="black", fill=NA))

pn4 <- ggplot(data=l_shape, aes(x=x, y=y)) +
  geom_point() +
    ggtitle("(d) L-shape") +
  theme_minimal() +
  theme(aspect.ratio=1,
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.border = element_rect(colour="black", fill=NA))

pn5 <- ggplot(data=hetero, aes(x=x, y=y)) +
  geom_point() +
    ggtitle("(e) Skewed") +
  theme_minimal() +
  theme(aspect.ratio=1,
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.border = element_rect(colour="black", fill=NA))

pn6 <- ggplot(data=d_clusters, aes(x=x, y=y)) +
  geom_point() +
    ggtitle("(f) Clusters") +
  theme_minimal() +
  theme(aspect.ratio=1,
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.border = element_rect(colour="black", fill=NA))

pn1 + pn2 + pn3 + pn4 + pn5 + pn6 + plot_layout(ncol=3)


## ----echo=knitr::is_html_output()------------------------------------------
#| eval: false
#| code-summary: "Code to make animated gifs"
# library(mulgar)
# data(plane)
# data(box)
# render_gif(plane,
#            grand_tour(),
#            display_xy(),
#            gif_file="gifs/plane.gif",
#            frames=500,
#            width=200,
#            height=200)
# render_gif(box,
#            grand_tour(),
#            display_xy(),
#            gif_file="gifs/box.gif",
#            frames=500,
#            width=200,
#            height=200)
# # Simulate full cube
# library(geozoo)
# cube5d <- data.frame(cube.solid.random(p=5, n=300)$points)
# colnames(cube5d) <- paste0("x", 1:5)
# cube5d <- data.frame(apply(cube5d, 2, function(x) (x-mean(x))/sd(x)))
# render_gif(cube5d,
#            grand_tour(),
#            display_xy(),
#            gif_file="gifs/cube5d.gif",
#            frames=500,
#            width=200,
#            height=200)


## ----echo=knitr::is_html_output()------------------------------------------
#| label: fig-plane-scatmat
#| fig-cap: "Scatterplot matrix of plane data. You can see that x1-x3 are strongly linearly associated, and also x4 and x5. When you watch the tour of this data, any time the data collapses into a line you should see only (x1, x2, x3) or (x4, x5). When combinations of x1 and x4 or x5 show, the data should be spread out." 
#| fig-alt: "A five-by-five scatterplot matrix, with scatterplots in the lower triangle, correlaton printed in the upper triangle and density plots shown on the diagonal. Plots of x1 vs x2, x1 vs x3, x2 vs x3, and x4 vs x5 have strong positive or negative correlation. The remaining pairs of variables have no association."
#| fig-width: 6
#| fig-height: 6
#| out-width: 80%
#| message: false
#| warning: false
#| code-summary: Code for scatterplot matrix
library(GGally)
library(mulgar)
data(plane)
ggscatmat(plane) +
  theme(panel.background = 
          element_rect(colour="black", fill=NA),
    axis.text = element_blank(),
    axis.ticks = element_blank())


## --------------------------------------------------------------------------
#| code-fold: false
# Add two pure noise dimensions to the plane
plane_noise <- plane
plane_noise$x6 <- rnorm(100)
plane_noise$x7 <- rnorm(100)
plane_noise <- data.frame(apply(plane_noise, 2, 
    function(x) (x-mean(x))/sd(x)))


## ----echo=knitr::is_html_output()------------------------------------------
#| label: fig-plane-noise-scatter
#| fig-cap: "Scatterplots showing two additional noise variables that are not associated with any of the first five variables."
#| fig-alt: "Two rows of scatterplots showing x6 and x7 against x1-x5. The points are spread out in the full plotting region, although x6 has one point with an unusually low value."
#| fig-height: 3
#| fig-width: 6
#| warning: false
ggduo(plane_noise, columnsX = 1:5, columnsY = 6:7, 
    types = list(continuous = "points")) +
  theme(aspect.ratio=1,
    panel.background = 
          element_rect(colour="black", fill=NA),
    axis.text = element_blank(),
    axis.ticks = element_blank())


## ----echo=knitr::is_html_output()------------------------------------------
#| label: plane-plotly
#| eval: false
#| code-fold: true
#| code-summary: "Code to generate animation"
# library(ggplot2)
# library(plotly)
# library(htmlwidgets)
# 
# set.seed(78)
# b <- basis_random(7, 2)
# pn_t <- tourr::save_history(plane_noise,
#                     tour_path = grand_tour(),
#                     start = b,
#                     max_bases = 8)
# pn_t <- interpolate(pn_t, 0.1)
# pn_anim <- render_anim(plane_noise,
#                          frames=pn_t)
# 
# pn_gp <- ggplot() +
#      geom_path(data=pn_anim$circle,
#                aes(x=c1, y=c2,
#                    frame=frame), linewidth=0.1) +
#      geom_segment(data=pn_anim$axes,
#                   aes(x=x1, y=y1,
#                       xend=x2, yend=y2,
#                       frame=frame),
#                   linewidth=0.1) +
#      geom_text(data=pn_anim$axes,
#                aes(x=x2, y=y2,
#                    frame=frame,
#                    label=axis_labels),
#                size=5) +
#      geom_point(data=pn_anim$frames,
#                 aes(x=P1, y=P2,
#                     frame=frame),
#                 alpha=0.8) +
#      xlim(-1,1) + ylim(-1,1) +
#      coord_equal() +
#      theme_bw() +
#      theme(axis.text=element_blank(),
#          axis.title=element_blank(),
#          axis.ticks=element_blank(),
#          panel.grid=element_blank())
# pn_tour <- ggplotly(pn_gp,
#                         width=500,
#                         height=550) |>
#        animation_button(label="Go") |>
#        animation_slider(len=0.8, x=0.5,
#                         xanchor="center") |>
#        animation_opts(easing="linear",
#                       transition = 0)
# 
# htmlwidgets::saveWidget(pn_tour,
#           file="html/plane_noise.html",
#           selfcontained = TRUE)


## --------------------------------------------------------------------------
# Add several outliers to the plane_noise data
plane_noise_outliers <- plane_noise
plane_noise_outliers[101,] <- c(2, 2, -2, 0, 0, 0, 0)
plane_noise_outliers[102,] <- c(0, 0, 0,-2, -2, 0, 0)


## ----echo=knitr::is_html_output()------------------------------------------
#| label: fig-plane-noise-outlier
#| fig-cap: "Scatterplot matrix of the plane with noise data, with two added outliers in variables with strong correlation."
#| fig-alt: "A five-by-five scatterplot matrix, with scatterplots in the lower triangle, correlaton printed in the upper triangle and density plots shown on the diagonal. Plots of x1 vs x2, x1 vs x3, x2 vs x3, and x4 vs x5 have strong positive or negative correlation, with an outlier in the corner of the plot. The remaining pairs of variables have no association, and thus also no outliers."
#| fig-height: 6
#| fig-width: 6
#| out-width: 80%
#| code-summary: "Code for scatterplot matrix"
ggscatmat(plane_noise_outliers, columns = 1:5) +
  theme(aspect.ratio=1,
    panel.background = 
          element_rect(colour="black", fill=NA),
    axis.text = element_blank(),
    axis.ticks = element_blank())


## ----echo=knitr::is_html_output()------------------------------------------
#| eval: false
#| code-summary: "Code to generate animated gif"
# render_gif(plane_noise_outliers,
#            grand_tour(),
#            display_xy(),
#            gif_file="gifs/pn_outliers.gif",
#            frames=500,
#            width=200,
#            height=200)
# 
# data(plane_nonlin)
# set.seed(508)
# render_gif(plane_nonlin,
#            grand_tour(),
#            display_xy(),
#            gif_file="gifs/plane_nonlin.gif",
#            frames=500,
#            width=400,
#            height=400)


## --------------------------------------------------------------------------
#| label: fig-plane-outliers
#| eval: false
#| code-fold: true
#| echo: false
# library(ggplot2)
# library(plotly)
# library(htmlwidgets)
# 
# set.seed(78)
# b <- basis_random(7, 2)
# pn_t <- tourr::save_history(plane_noise_outliers,
#                     tour_path = grand_tour(),
#                     start = b,
#                     max_bases = 20)
# pn_t <- interpolate(pn_t, 0.2)
# pn_anim <- render_anim(plane_noise_outliers,
#                          frames=pn_t)
# 
# pn_gp <- ggplot() +
#      geom_path(data=pn_anim$circle,
#                aes(x=c1, y=c2,
#                    frame=frame), linewidth=0.1) +
#      geom_segment(data=pn_anim$axes,
#                   aes(x=x1, y=y1,
#                       xend=x2, yend=y2,
#                       frame=frame),
#                   linewidth=0.1) +
#      geom_text(data=pn_anim$axes,
#                aes(x=x2, y=y2,
#                    frame=frame,
#                    label=axis_labels),
#                size=5) +
#      geom_point(data=pn_anim$frames,
#                 aes(x=P1, y=P2,
#                     frame=frame),
#                 alpha=0.8) +
#      xlim(-1,1) + ylim(-1,1) +
#      coord_equal() +
#      theme_bw() +
#      theme(axis.text=element_blank(),
#          axis.title=element_blank(),
#          axis.ticks=element_blank(),
#          panel.grid=element_blank())
# pn_tour <- ggplotly(pn_gp,
#                         width=500,
#                         height=550) |>
#        animation_button(label="Go") |>
#        animation_slider(len=0.8, x=0.5,
#                         xanchor="center") |>
#        animation_opts(easing="linear",
#                       transition = 0)
# 
# htmlwidgets::saveWidget(pn_tour,
#           file="html/plane_noise.html",
#           selfcontained = TRUE)


## --------------------------------------------------------------------------
#| eval: false
#| echo: false
# # Answer to Q2
# library(tourr)
# library(mvtnorm)
# 
# s1 <- diag(5)
# s2 <- diag(5)
# s2[3,4] <- 0.7
# s2[4,3] <- 0.7
# s3 <- s2
# s3[1,2] <- -0.7
# s3[2,1] <- -0.7
# 
# set.seed(1234)
# d1 <- as.data.frame(rmvnorm(500, sigma = s1))
# d2 <- as.data.frame(rmvnorm(500, sigma = s2))
# d3 <- as.data.frame(rmvnorm(500, sigma = s3))


## ----eval=FALSE------------------------------------------------------------
# library(tidyverse)
# library(tourr)
# library(GGally)
# set.seed(946)
# d <- tibble(x1=runif(200, -1, 1),
#             x2=runif(200, -1, 1),
#             x3=runif(200, -1, 1))
# d <- d |>
#   mutate(x4 = x3 + runif(200, -0.1, 0.1))
# # outlier is visible in d
# d <- bind_rows(d, c(x1=0, x2=0, x3=-0.5, x4=0.5))
# 
# # Point is hiding in d_r
# d_r <- d |>
#   mutate(x1 = cos(pi/6)*x1 + sin(pi/6)*x3,
#          x3 = -sin(pi/6)*x1 + cos(pi/6)*x3,
#          x2 = cos(pi/6)*x2 + sin(pi/6)*x4,
#          x4 = -sin(pi/6)*x2 + cos(pi/6)*x4)


## --------------------------------------------------------------------------
#| eval: false
#| echo: false
# library(mulgar)
# library(tibble)
# library(dplyr)
# library(tourr)
# library(GGally)
# library(colorspace)
# set.seed(901)
# mvnorm5 <- mulgar::rmvn(n=nrow(copnorm), vc=cov(copnorm))
# colnames(mvnorm5) <- paste0("x", 1:5)
# d <- bind_rows(as_tibble(mvnorm5), as_tibble(copnorm)) |>
#   mutate(gp = factor(c(rep("norm", nrow(mvnorm5)), rep("cop", nrow(copnorm)))))
# animate_xy(d[,1:5], col=d$gp)
# GGally::ggscatmat(d, columns=1:5, color="gp", alpha=0.5) +
#   scale_color_discrete_divergingx(palette="Zissou 1")
# 
# d <- bind_rows(as_tibble(mvnorm5), as_tibble(copclayton)) |>
#   mutate(gp = factor(c(rep("norm", nrow(mvnorm5)), rep("cop", nrow(copclayton)))))
# animate_xy(d[,1:5], col=d$gp)
# animate_xy(d[,1:5], guided_tour(lda_pp(d$gp)), col=d$gp)
# GGally::ggscatmat(d, columns=1:5, color="gp", alpha=0.5) +
#   scale_color_discrete_divergingx(palette="Zissou 1")
# 
# d <- bind_rows(as_tibble(mvnorm5), as_tibble(copjoe)) |>
#   mutate(gp = factor(c(rep("norm", nrow(mvnorm5)), rep("cop", nrow(copjoe)))))
# animate_xy(d[,1:5], col=d$gp)
# animate_xy(d[,1:5], guided_tour(lda_pp(d$gp)), col=d$gp)
# GGally::ggscatmat(d, columns=1:5, color="gp", alpha=0.5) +
#   scale_color_discrete_divergingx(palette="Zissou 1")
# 
# d <- bind_rows(as_tibble(mvnorm5), as_tibble(copfrank)) |>
#   mutate(gp = factor(c(rep("norm", nrow(mvnorm5)), rep("cop", nrow(copfrank)))))
# animate_xy(d[,1:5], col=d$gp)
# animate_xy(d[,1:5], guided_tour(lda_pp(d$gp)), col=d$gp)
# GGally::ggscatmat(d, columns=1:5, color="gp", alpha=0.5) +
#   scale_color_discrete_divergingx(palette="Zissou 1")

