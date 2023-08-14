#| code-summary: "Code to produce 2D data examples"
library(ggplot2)
library(tibble)
set.seed(6045)
x1 <- runif(123)
x2 <- x1 + rnorm(123, sd=0.1)
x3 <- rnorm(123, sd=0.2)
df <- tibble(x1 = (x1-mean(x1))/sd(x1), 
             x2 = (x2-mean(x2))/sd(x2),
             x3, 
             x3scaled = (x3-mean(x3))/sd(x3))
dp1 <- ggplot(df) + 
  geom_point(aes(x=x1, y=x2)) +
  xlim(-2.5, 2.5) + ylim(-2.5, 2.5) +
  annotate("segment", x=0, xend=2, y=0, yend=0) +
  annotate("segment", x=0, xend=0, y=0, yend=2) +
  annotate("text", x=2.1, y=0, label="x1") +
  annotate("text", x=0, y=2.1, label="x2") +
  theme_minimal() +
  theme(aspect.ratio=1)
dp2 <- ggplot(df) + 
  geom_point(aes(x=x1, y=x3)) +
  xlim(-2.5, 2.5) + ylim(-2.5, 2.5) +
  annotate("segment", x=0, xend=2, y=0, yend=0) +
  annotate("segment", x=0, xend=0, y=0, yend=2) +
  annotate("text", x=2.1, y=0, label="x1") +
  annotate("text", x=0, y=2.1, label="x3") +
  theme_minimal() +
  theme(aspect.ratio=1)
dp3 <- ggplot(df) + 
  geom_point(aes(x=x1, y=x3scaled)) +
  xlim(-2.5, 2.5) + ylim(-2.5, 2.5) +
  annotate("segment", x=0, xend=2, y=0, yend=0) +
  annotate("segment", x=0, xend=0, y=0, yend=2) +
  annotate("text", x=2.1, y=0, label="x1") +
  annotate("text", x=0, y=2.1, label="x3") +
  theme_minimal() +
  theme(aspect.ratio=1)


#| label: fig-2D-1
#| echo: false
#| fig-width: 3
#| fig-height: 3
#| fig-cap: Reduced dimension
dp1


#| label: fig-2D-2
#| echo: false
#| fig-width: 3
#| fig-height: 3
#| fig-cap: Reduced variance
dp2


#| label: fig-2D-3
#| echo: false
#| fig-width: 3
#| fig-height: 3
#| message: false
#| warning: false
#| fig-cap: No reduced dimension
dp3


#| eval: false
#| code-summary: "Code to make animated gifs"
## library(mulgar)
## data(plane)
## data(box)
## render_gif(plane,
##            grand_tour(),
##            display_xy(),
##            gif_file="gifs/plane.gif",
##            frames=500,
##            width=200,
##            height=200)
## render_gif(box,
##            grand_tour(),
##            display_xy(),
##            gif_file="gifs/box.gif",
##            frames=500,
##            width=200,
##            height=200)
## # Simulate full cube
## library(geozoo)
## cube5d <- data.frame(cube.solid.random(p=5, n=300)$points)
## colnames(cube5d) <- paste0("x", 1:5)
## cube5d <- data.frame(apply(cube5d, 2, function(x) (x-mean(x))/sd(x)))
## render_gif(cube5d,
##            grand_tour(),
##            display_xy(),
##            gif_file="gifs/cube5d.gif",
##            frames=500,
##            width=200,
##            height=200)


#| label: fig-plane-scatma
#| fig-cap: Scatterplot matrix of plane data. You can see that x1-x3 are strongly linearly associated, and also x4 and x5. When you watch the tour of this data, any time the data collapses into a line you should see only (x1, x2, x3) or (x4, x5). When combinations of x1 and x4 or x5 show, the data should be spread out.
#| message: false
#| warning: false
#| code-summary: Code for scatterplot matrix
library(GGally)
library(mulgar)
data(plane)
ggscatmat(plane)


#| label: fig-plane-noise-scatter
#| fig-cap: Additional noise variables are not associated with any of the first five variables.
#| code-fold: false
#| fig-height: 4
#| fig-width: 8
# Add two pure noise dimensions to the plane
plane_noise <- plane
plane_noise$x6 <- rnorm(100)
plane_noise$x7 <- rnorm(100)
plane_noise <- data.frame(apply(plane_noise, 2, function(x) (x-mean(x))/sd(x)))
ggduo(plane_noise, columnsX = 1:5, columnsY = 6:7, 
      types = list(continuous = "points")) +
  theme(aspect.ratio=1, axis.text = element_blank())


#| label: plane-plotly
#| eval: false
#| code-fold: true
#| code-summary: "Code to generate animation"
## library(ggplot2)
## library(plotly)
## library(htmlwidgets)
## 
## set.seed(78)
## b <- basis_random(7, 2)
## pn_t <- tourr::save_history(plane_noise,
##                     tour_path = grand_tour(),
##                     start = b,
##                     max_bases = 8)
## pn_t <- interpolate(pn_t, 0.1)
## pn_anim <- render_anim(plane_noise,
##                          frames=pn_t)
## 
## pn_gp <- ggplot() +
##      geom_path(data=pn_anim$circle,
##                aes(x=c1, y=c2,
##                    frame=frame), linewidth=0.1) +
##      geom_segment(data=pn_anim$axes,
##                   aes(x=x1, y=y1,
##                       xend=x2, yend=y2,
##                       frame=frame),
##                   linewidth=0.1) +
##      geom_text(data=pn_anim$axes,
##                aes(x=x2, y=y2,
##                    frame=frame,
##                    label=axis_labels),
##                size=5) +
##      geom_point(data=pn_anim$frames,
##                 aes(x=P1, y=P2,
##                     frame=frame),
##                 alpha=0.8) +
##      xlim(-1,1) + ylim(-1,1) +
##      coord_equal() +
##      theme_bw() +
##      theme(axis.text=element_blank(),
##          axis.title=element_blank(),
##          axis.ticks=element_blank(),
##          panel.grid=element_blank())
## pn_tour <- ggplotly(pn_gp,
##                         width=500,
##                         height=550) %>%
##        animation_button(label="Go") %>%
##        animation_slider(len=0.8, x=0.5,
##                         xanchor="center") %>%
##        animation_opts(easing="linear",
##                       transition = 0)
## 
## htmlwidgets::saveWidget(pn_tour,
##           file="html/plane_noise.html",
##           selfcontained = TRUE)


#| label: fig-plane-noise-outlier
#| fig-cap: Outliers added to the plane with noise data.
#| fig-height: 4
#| fig-width: 8
#| code-summary: "Code for scatterplot matrix"
# Add several outliers to the plane_noise data
plane_noise_outliers <- plane_noise
plane_noise_outliers[101,] <- c(2, 2, -2, 0, 0, 0, 0)
plane_noise_outliers[102,] <- c(0, 0, 0,-2, -2, 0, 0)

ggscatmat(plane_noise_outliers, columns = 1:5) +
  theme(aspect.ratio=1, axis.text = element_blank())


#| eval: false
#| code-summary: "Code to generate animated gif"
## render_gif(plane_noise_outliers,
##            grand_tour(),
##            display_xy(),
##            gif_file="gifs/pn_outliers.gif",
##            frames=500,
##            width=200,
##            height=200)
## 
## data(plane_nonlin)
## render_gif(plane_nonlin,
##            grand_tour(),
##            display_xy(),
##            gif_file="gifs/plane_nonlin.gif",
##            frames=500,
##            width=200,
##            height=200)


#| label: fig-plane-outliers
#| eval: false
#| code-fold: true
#| echo: false
## library(ggplot2)
## library(plotly)
## library(htmlwidgets)
## 
## set.seed(78)
## b <- basis_random(7, 2)
## pn_t <- tourr::save_history(plane_noise_outliers,
##                     tour_path = grand_tour(),
##                     start = b,
##                     max_bases = 20)
## pn_t <- interpolate(pn_t, 0.2)
## pn_anim <- render_anim(plane_noise_outliers,
##                          frames=pn_t)
## 
## pn_gp <- ggplot() +
##      geom_path(data=pn_anim$circle,
##                aes(x=c1, y=c2,
##                    frame=frame), linewidth=0.1) +
##      geom_segment(data=pn_anim$axes,
##                   aes(x=x1, y=y1,
##                       xend=x2, yend=y2,
##                       frame=frame),
##                   linewidth=0.1) +
##      geom_text(data=pn_anim$axes,
##                aes(x=x2, y=y2,
##                    frame=frame,
##                    label=axis_labels),
##                size=5) +
##      geom_point(data=pn_anim$frames,
##                 aes(x=P1, y=P2,
##                     frame=frame),
##                 alpha=0.8) +
##      xlim(-1,1) + ylim(-1,1) +
##      coord_equal() +
##      theme_bw() +
##      theme(axis.text=element_blank(),
##          axis.title=element_blank(),
##          axis.ticks=element_blank(),
##          panel.grid=element_blank())
## pn_tour <- ggplotly(pn_gp,
##                         width=500,
##                         height=550) %>%
##        animation_button(label="Go") %>%
##        animation_slider(len=0.8, x=0.5,
##                         xanchor="center") %>%
##        animation_opts(easing="linear",
##                       transition = 0)
## 
## htmlwidgets::saveWidget(pn_tour,
##           file="html/plane_noise.html",
##           selfcontained = TRUE)

