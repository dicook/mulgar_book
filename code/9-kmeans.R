#| code-fold: false
#| message: false
library(mulgar)
library(ggplot2)
library(dplyr)
library(colorspace)
library(patchwork)
data("simple_clusters")
load("data/penguins_sub.rda")

set.seed(202305)
sc_bl_bd_km <- kmeans(simple_clusters[,1:2], centers=2, 
                     iter.max = 50, nstart = 5)
sc_bl_bd_km_means <- data.frame(sc_bl_bd_km$centers) %>%
  mutate(cl = factor(rownames(sc_bl_bd_km$centers)))
sc_bl_bd_km_d <- simple_clusters[,1:2] %>% 
  mutate(cl = factor(sc_bl_bd_km$cluster))


#| label: fig-km-2D
#| fig-cap: "Examining $k$-means clustering results for simple clusters (a) and two variables of the penguins data (b). The means are indicated by a $+$. The results are perfect for the simple clusters but not for the penguins data. The penguin clusters are elliptically shaped which is not captured by $k$-means. Cluster 3 has observations grouped across a gap in the data."
#| fig-width: 6
#| fig-height: 4
#| code-summary: "Code to make plots"
sc_bl_bd_km_p <- ggplot() +
  geom_point(data=sc_bl_bd_km_d, 
             aes(x=x1, y=x2, colour=cl), 
             shape=16, alpha=0.4) +
  geom_point(data=sc_bl_bd_km_means, 
             aes(x=x1, y=x2, colour=cl), 
             shape=3, size=5) +
    scale_color_discrete_divergingx("Zissou 1") +
  ggtitle("(a)") +
  theme_minimal() +
  theme(aspect.ratio = 1, 
        legend.position = "bottom",
        legend.title = element_blank()) 

p_bl_bd_km <- kmeans(penguins_sub[,1:2], centers=3, 
                     iter.max = 50, nstart = 5)
p_bl_bd_km_means <- data.frame(p_bl_bd_km$centers) %>%
  mutate(cl = factor(rownames(p_bl_bd_km$centers)))
p_bl_bd_km_d <- penguins_sub[,1:2] %>% 
  mutate(cl = factor(p_bl_bd_km$cluster))

p_bl_bd_km_p <- ggplot() +
  geom_point(data=p_bl_bd_km_d, 
             aes(x=bl, y=bd, colour=cl), 
             shape=16, alpha=0.4) +
  geom_point(data=p_bl_bd_km_means, 
             aes(x=bl, y=bd, colour=cl), 
             shape=3, size=5) +
    scale_color_discrete_divergingx("Zissou 1") +
  ggtitle("(b)") +
  theme_minimal() +
  theme(aspect.ratio = 1, 
        legend.position = "bottom",
        legend.title = element_blank()) 

sc_bl_bd_km_p + p_bl_bd_km_p + plot_layout(ncol=2)


#| code-fold: false
p_km <- kmeans(penguins_sub[,1:4], centers=3, 
                     iter.max = 50, nstart = 5)
p_km_means <- data.frame(p_km$centers) %>%
  mutate(cl = factor(rownames(p_km$centers)))
p_km_d <- penguins_sub[,1:4] %>% 
  mutate(cl = factor(p_km$cluster))


#| eval: false
#| code-summary: "Code to make animated gifs"
## library(tourr)
## p_km_means <- p_km_means %>%
##   mutate(type = "mean")
## p_km_d <- p_km_d %>%
##   mutate(type = "data")
## p_km_all <- bind_rows(p_km_means, p_km_d)
## p_km_all$type <- factor(p_km_all$type, levels=c("mean", "data"))
## p_pch <- c(3, 20)[as.numeric(p_km_all$type)]
## p_cex <- c(3, 1)[as.numeric(p_km_all$type)]
## animate_xy(p_km_all[,1:4], col=p_km_all$cl,
##            pch=p_pch, cex=p_cex, axes="bottomleft")
## render_gif(p_km_all[,1:4],
##            grand_tour(),
##            display_xy(col=p_km_all$cl,
##                       pch=p_pch,
##                       cex=p_cex,
##                       axes="bottomleft"),
##            gif_file="gifs/p_km.gif",
##            width=400,
##            height=400,
##            frames=500)

