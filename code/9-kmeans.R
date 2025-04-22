## ----------------------------------------------------------------------------
#| code-fold: false
#| message: false
library(mulgar)
library(ggplot2)
library(dplyr)
library(colorspace)
library(patchwork)
data("simple_clusters")

set.seed(202305)
sc_km <- kmeans(simple_clusters[,1:2], centers=2, 
                     iter.max = 50, nstart = 5)
sc_km_means <- data.frame(sc_km$centers) |>
  mutate(cl = factor(rownames(sc_km$centers)))
sc_km_d <- simple_clusters[,1:2] |> 
  mutate(cl = factor(sc_km$cluster))


## ----echo=knitr::is_html_output()--------------------------------------------
#| label: fig-km-2D
#| fig-cap: "Examining $k$-means clustering results for simple clusters (a, c) and two variables of the penguins data (b, d). Top row is input data and bottom row shows one solution for each, with $k=2$ and $3$ respectively. The means are indicated by a $+$. Points are coloured by cluster label. The results are perfect for the simple clusters but not for the penguins data. The penguin data is multimodal, with small gaps visible from the top right mode and the other two, and the shap of the bigger clusters is more elliptical. The $k$-means produces roughly three equal sized clusters, where cluster 3 groups observations over small gaps in the point clouds."
#| fig-alt: "Four panels of scatterplots labelled rowwise a-d. The top two are uncoloured, and the bottom two have coloured points. Each coloured group has one point displayed as a plus. Plot c has two circular separated clusters located at lower left and upper right, which are coloured red and blue. They correspond to labels 1 and 2 as shown by the legend at the bottom. Plot d has three coloured groups, blue, yellow, red labelled as 1, 2, 3, respectively. The red group occupies the top right quadrant of the square plot space, and some of the points are on both sides of a small gap where there are no points. The yellow group occupy the bottom right quadrant, and the blue group occupy the top left quadrant. The botton left is empty of points."
#| fig-width: 6
#| fig-height: 6
#| out-width: 100%
#| code-summary: "Code to make plots"
sc_km_p1 <- ggplot() +
  geom_point(data=sc_km_d, 
             aes(x=x1, y=x2), 
             alpha=0.4, size=1) +
  ggtitle("(a)") +
  theme_minimal() +
  theme(aspect.ratio = 1, 
        legend.position = "bottom",
        legend.title = element_blank()) 
sc_km_p2 <- ggplot() +
  geom_point(data=sc_km_d, 
             aes(x=x1, y=x2, colour=cl), 
             shape=16, alpha=0.4) +
  geom_point(data=sc_km_means, 
             aes(x=x1, y=x2, colour=cl), 
             shape=3, size=5) +
    scale_color_discrete_divergingx("Zissou 1") +
  ggtitle("(c)") +
  theme_minimal() +
  theme(aspect.ratio = 1, 
        legend.position = "bottom",
        legend.title = element_blank()) 

load("data/penguins_sub.rda")
set.seed(1012)
p_bl_bd_km <- kmeans(penguins_sub[,c(1,2)], centers=3, 
                     iter.max = 50, nstart = 5)
p_bl_bd_km_means <- data.frame(p_bl_bd_km$centers) |>
  mutate(cl = factor(rownames(p_bl_bd_km$centers)))
p_bl_bd_km_d <- penguins_sub[,c(1,2)] |> 
  mutate(cl = factor(p_bl_bd_km$cluster))

p_bl_bd_km_p1 <- ggplot() +
  geom_point(data=p_bl_bd_km_d, 
             aes(x=bl, y=bd), 
             alpha=0.4, size=1) +
  ggtitle("(b)") +
  theme_minimal() +
  theme(aspect.ratio = 1, 
        legend.position = "bottom",
        legend.title = element_blank()) 
p_bl_bd_km_p2 <- ggplot() +
  geom_point(data=p_bl_bd_km_d, 
             aes(x=bl, y=bd, colour=cl), 
             shape=16, alpha=0.4) +
  geom_point(data=p_bl_bd_km_means, 
             aes(x=bl, y=bd, colour=cl), 
             shape=3, size=5) +
    scale_color_discrete_divergingx("Zissou 1") +
  ggtitle("(d)") +
  theme_minimal() +
  theme(aspect.ratio = 1, 
        legend.position = "bottom",
        legend.title = element_blank()) 

sc_km_p1 + p_bl_bd_km_p1  + sc_km_p2 + p_bl_bd_km_p2 + plot_layout(ncol=2)


## ----------------------------------------------------------------------------
#| code-fold: false
p_km <- kmeans(penguins_sub[,1:4], centers=3, 
                     iter.max = 50, nstart = 5)
p_km_means <- data.frame(p_km$centers) |>
  mutate(cl = factor(rownames(p_km$centers)))
p_km_d <- penguins_sub[,1:4] |> 
  mutate(cl = factor(p_km$cluster))


## ----echo=knitr::is_html_output()--------------------------------------------
#| eval: false
#| code-summary: "Code to make animated gifs"
# library(tourr)
# p_km_means <- p_km_means |>
#   mutate(type = "mean")
# p_km_d <- p_km_d |>
#   mutate(type = "data")
# p_km_all <- bind_rows(p_km_means, p_km_d)
# p_km_all$type <- factor(p_km_all$type, levels=c("mean", "data"))
# p_pch <- c(3, 20)[as.numeric(p_km_all$type)]
# p_cex <- c(3, 1)[as.numeric(p_km_all$type)]
# animate_xy(p_km_all[,1:4], col=p_km_all$cl,
#            pch=p_pch, cex=p_cex, axes="bottomleft")
# render_gif(p_km_all[,1:4],
#            grand_tour(),
#            display_xy(col=p_km_all$cl,
#                       pch=p_pch,
#                       cex=p_cex,
#                       axes="bottomleft"),
#            gif_file="gifs/p_km.gif",
#            width=400,
#            height=400,
#            frames=500)

