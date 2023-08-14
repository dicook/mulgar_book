#| code-fold: false
#| message: false
library(kohonen)
library(aweSOM)
library(mulgar)
library(dplyr)
library(ggplot2)
library(colorspace)
load("data/penguins_sub.rda")

set.seed(947)
p_grid <- kohonen::somgrid(xdim = 5, ydim = 5,
                           topo = 'rectangular')
p_init <- somInit(as.matrix(penguins_sub[,1:4]), 5, 5)
p_som <- som(as.matrix(penguins_sub[,1:4]), 
             rlen=2000,
             grid = p_grid,
             init = p_init)


#| code-fold: false
#| message: false

p_som_df_net <- som_model(p_som)
p_som_data <- p_som_df_net$data %>% 
  mutate(species = penguins_sub$species)
p_som_map_p <- ggplot() +
  geom_segment(data=p_som_df_net$edges_s, 
               aes(x=x, xend=xend, y=y, 
                   yend=yend)) +
  geom_point(data=p_som_data, 
             aes(x=map1, y=map2, 
                 colour=species), 
             size=3, alpha=0.5) +
  xlab("map 1") + ylab("map 2") +
  scale_color_discrete_divergingx(
    palette="Zissou 1") +
  theme_minimal() +
  theme(aspect.ratio = 1, 
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.text = element_blank())


#| eval: false
#| code-fold: false
## library(tourr)
## 
## # Set up data
## p_som_map <- p_som_df_net$net %>%
##   mutate(species = "0", type="net")
## p_som_data <- p_som_data %>%
##   select(bl:bm, species) %>%
##   mutate(type="data",
##          species = as.character(species))
## p_som_map_data <- bind_rows(p_som_map, p_som_data)
## p_som_map_data$type <- factor(p_som_map_data$type,
##   levels=c("net", "data"))
## p_som_map_data$species <- factor(p_som_map_data$species,
##   levels=c("0","Adelie","Chinstrap","Gentoo"))
## p_pch <- c(46, 16)[as.numeric(p_som_map_data$type)]
## p_col <- c("black", hcl.colors(3, "Zissou 1"))[as.numeric(p_som_map_data$species)]
## animate_xy(p_som_map_data[,1:4],
##            col=p_col,
##            pch=p_pch,
##            edges=as.matrix(p_som_df_net$edges),
##            edges.col = "black",
##            axes="bottomleft")


#| echo: false
#| eval: false
## render_gif(p_som_map_data[,1:4],
##            grand_tour(),
##            display_xy(col=p_col,
##              pch=p_pch,
##              edges=as.matrix(p_som_df_net$edges),
##              edges.col = "black",
##              axes="bottomleft"),
##            gif_file="gifs/p_som.gif",
##            frames=500,
##            width=400,
##            height=400
## )


#| echo: false
#| label: fig-p-som2
#| fig-cap: "2D map"
p_som_map_p

