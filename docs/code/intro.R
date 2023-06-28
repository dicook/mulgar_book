#| echo: false
library(mulgar)
library(ggplot2)
data("simple_clusters")

s_p <- ggplot(simple_clusters, aes(x=x1, y=x2)) +
  geom_point(size=2, alpha=0.8, colour="#EC5C00") +
  geom_abline(intercept=0, slope=1) +
  annotate("text", x=2.0, y=2.2, label="(0.707, 0.707)", angle=45) +
  annotate("text", x=2.2, y=2.0, label="most clustered", angle=45) +
  geom_abline(intercept=0, slope=-1) +
  annotate("text", x=-1.6, y=1.8, label="(-0.707, 0.707)", angle=-45) +
  annotate("text", x=-1.8, y=1.6, label="no clusters", angle=-45) +
  geom_abline(intercept=0, slope=0) +
  annotate("text", x=-1.6, y=0.15, label="(1, 0)") +
  annotate("text", x=-1.4, y=-0.1, label="some clustering") +
  xlim(-2, 2.5) + ylim(-2, 2.5) +
  theme_minimal() +
  theme(aspect.ratio=1)


#| echo: false
#| eval: false
## library(tourr)
## 
## explain_t1 <- save_history(simple_clusters[,1:2],
##                            grand_tour(d=1),
##                            max_bases=9)
## explain_t1[,,2] <- matrix(c(1/sqrt(2), 1/sqrt(2)),
##                           ncol=1)
## explain_t1[,,3] <- matrix(c(0, 1),
##                           ncol=1)
## explain_t1[,,4] <- matrix(c(-1/sqrt(2), 1/sqrt(2)),
##                           ncol=1)
## explain_t1[,,5] <- matrix(c(-1, 0),
##                           ncol=1)
## explain_t1[,,6] <- matrix(c(-1/sqrt(2), -1/sqrt(2)),
##                           ncol=1)
## explain_t1[,,7] <- matrix(c(0, -1),
##                           ncol=1)
## explain_t1[,,8] <- matrix(c(1/sqrt(2), -1/sqrt(2)),
##                           ncol=1)
## explain_t1[,,9] <- matrix(c(1, 0),
##                           ncol=1)
## 
## animate_dist(simple_clusters[,1:2],
##              planned_tour(explain_t1),
##              method="density", col="#EC5C00",
##              scale_density = TRUE,
##              half_range=0.8)
## render_gif(simple_clusters[,1:2],
##            planned_tour(explain_t1),
##            display_dist(method="density",
##                         col="#EC5C00",
##                         density_max = 2,
##                         scale_density = TRUE,
##              half_range=0.8),
##            gif_file = "gifs/explain_1d.gif",
##            apf = 1/100,
##            frames = 1000,
##            width = 400,
##            height = 300)


#| echo: false
#| label: fig-explain-1D-data
#| fig-cap: 2D data
#| fig-width: 4
#| fig-height: 4
s_p


#| echo: false
library(tourr)
library(geozoo)
set.seed(1351)
d <- torus(3, n=4304)$points
d <- apply(d, 2, function(x) (x-mean(x))/sd(x))
ang <- 15
d <- as.matrix(d) %*% matrix(c(cos(ang), 0, sin(ang), 0, 1, 0, -sin(ang), 0, cos(ang)), ncol=3, byrow=T)
colnames(d) <- paste0("x", 1:3)
d <- data.frame(d)


#| echo: false
#| eval: false
## animate_xy(d, little_tour(), aps=0.2)
## 
## explain_t2 <- save_history(d, little_tour(), 4)
## 
## animate_xy(d, planned_tour(explain_t2), half_range=0.7, axes="bottomleft")
## 
## render_gif(d,
##            planned_tour(explain_t2),
##            display_xy(col="#EC5C00",
##              half_range=0.7,
##              axes="bottomleft"),
##            gif_file = "gifs/explain_2d.gif",
##            apf = 1/75,
##            frames = 1000,
##            width = 400,
##            height = 300)


#| echo: false
explain_prj <- matrix(c(cos(ang), 0, -sin(ang), 0, 1, 0, sin(ang), 0, cos(ang)), ncol=3, byrow=T)[,1:2]

d_prj <- render_proj(d, explain_prj,
                     position="bottomleft",
                     limits=1.5)
d_prj_p <- ggplot() + 
  geom_path(data=d_prj$circle, aes(x=c1, y=c2), colour="grey70") +
    geom_segment(data=d_prj$axes, aes(x=x1, y=y1, xend=x2, yend=y2), colour="grey70") +
    geom_text(data=d_prj$axes, aes(x=x2, y=y2, label=rownames(d_prj$axes)), colour="grey50") +
    geom_point(data=d_prj$data_prj, aes(x=P1, y=P2), 
               col="#EC5C00", size=1.2) +
    xlim(-1.3,1.3) + ylim(-1.3, 1.3) +
    theme_bw() +
    theme(aspect.ratio=1,
       axis.text=element_blank(),
       axis.title=element_blank(),
       axis.ticks=element_blank(),
       panel.grid=element_blank())


#| echo: false
#| label: fig-explain-2D-data
#| fig-cap: A projection revealing the hole
#| fig-width: 4
#| fig-height: 4
d_prj_p


#| label: fig-dimension-cubes
#| echo: false
#| fig-cap: "Space can be considered to be a high-dimensional cube. Here we have pictured a sequence of increasing dimension cubes, from 1D to 5D, as wireframes, it can be seen that as the dimension increase by one, the cube doubles."
#| fig-width: 8
#| fig-height: 3
#| message: false
#| warning: false
# wire frame cubes
library(tidyverse)
library(ggthemes)
library(geozoo)
library(tourr)
library(patchwork)
library(ggrepel)

new_d_clr <- "#E87C00"
d_line_clr <- "#3B99B1"
set.seed(5)
c1 <- cube.iterate(p = 1)
c1$points <- as_tibble(c1$points)
c1$edges <- as_tibble(c1$edges)
c2 <- cube.iterate(p = 2)
c2$points <- as_tibble(c2$points)
c2$edges <- as_tibble(c2$edges)
c3 <- cube.iterate(p = 3)
proj <- basis_random(3,2) 
c3$points <- c3$points %*% proj
colnames(c3$points) <- c("Var1", "Var2")
c3$points <- as_tibble(c3$points)
c3$edges <- as_tibble(c3$edges)
c4 <- cube.iterate(p = 4)
proj <- basis_random(4,2) 
c4$points <- c4$points %*% proj
colnames(c4$points) <- c("Var1", "Var2")
c4$points <- as_tibble(c4$points)
c4$edges <- as_tibble(c4$edges) 
c4$edges.sub <- tibble(from = c(1,1,1,2,2,3,3,4,5,5,6,7), 
                       to = c(2,3,5,4,6,4,7,8,6,7,8,8))
c5 <- cube.iterate(p = 5)
proj <- basis_random(5, 2) 
c5$points <- c5$points %*% proj
colnames(c5$points) <- c("Var1", "Var2")
c5$points <- as_tibble(c5$points)
c5$edges <- as_tibble(c5$edges) 
c5$edges.sub <- tibble(from = c(1,1,1,1,2,2,2,3,3,3,4,4,5,5,5,6,6,7,7,8,9,9,9,10,10,11,11,12,13,13,14,15), 
                       to = c(2,3,5,9,4,6,10,4,7,11,8,12,6,7,13,8,14,8,15,16,10,11,13,12,14,12,15,16,14,15,16,16))

# plot
# 1D
p1 <- ggplot() +
  geom_point(data=c1$points, aes(x=Var1, y=1)) +
  geom_segment(data=c1$edges, 
               aes(x=c1$points$Var1[c1$edges$from], 
                   xend=c1$points$Var1[c1$edges$to],
                   y=1, yend=1), 
               linetype=3, colour = d_line_clr) + 
  geom_point(data=c1$points[1,], aes(x=Var1, y=1), colour = new_d_clr) +
  ggtitle("1D") +
  theme_void() +
  theme(title = element_text(colour = "black", size = 24),
        aspect.ratio = 1) +
  xlim(c(-0.2, 1.2))

# 2D
p2 <- ggplot() +
  geom_point(data=c2$points, aes(x=Var1, y=Var2)) +
  geom_segment(data=c2$edges[c(1,4),], 
               aes(x=c2$points$Var1[from], 
                   xend=c2$points$Var1[to],
                   y=c2$points$Var2[from], 
                   yend=c2$points$Var2[to])) + 
  geom_segment(data=c2$edges[c(2,3),], 
               aes(x=c2$points$Var1[from], 
                   xend=c2$points$Var1[to],
                   y=c2$points$Var2[from], 
                   yend=c2$points$Var2[to]), 
               linetype = 3, colour = d_line_clr) + # dashed connectors
  geom_point(data=c2$points[1:2,], aes(x=Var1, y=Var2), 
             colour = new_d_clr) +
  geom_segment(data=c2$edges[1,], 
               aes(x=c2$points$Var1[from], 
                   xend=c2$points$Var1[to],
                   y=c2$points$Var2[from], 
                   yend=c2$points$Var2[to]), 
               colour = new_d_clr) + 
  ggtitle("2D") +
  theme_void() +
  theme(title = element_text(colour = "black", size = 24),
        aspect.ratio = 1) +
  xlim(c(-0.15, 1.15)) + ylim(c(-0.15, 1.15))

# 3D
c_in <- c(1,2,4,6,9,10,11,12)
c_out <- c(3,5,7,8)
p3 <- ggplot() +
  geom_point(data=c3$points, aes(x=Var1, y=Var2)) +
  geom_segment(data=c3$edges[c_in,], 
               aes(x=c3$points$Var1[from], 
                   xend=c3$points$Var1[to],
                   y=c3$points$Var2[from], 
                   yend=c3$points$Var2[to])) + 
  geom_segment(data=c3$edges[c_out,], 
               aes(x=c3$points$Var1[from], 
                   xend=c3$points$Var1[to],
                   y=c3$points$Var2[from], 
                   yend=c3$points$Var2[to]), 
               linetype = 3, colour = d_line_clr) + 
  geom_point(data=c3$points[1:4,], aes(x=Var1, y=Var2), colour = new_d_clr) +
  geom_segment(data=c3$edges[c(1,2,4,6),], 
               aes(x=c3$points$Var1[from], 
                   xend=c3$points$Var1[to],
                   y=c3$points$Var2[from], 
                   yend=c3$points$Var2[to]), colour = new_d_clr) + 
  ggtitle("3D") +
  theme_void() +
  theme(title = element_text(colour = "black", size = 24),
        aspect.ratio = 1)
# p3 + geom_text_repel(data=c3$points, aes(x=Var1, y=Var2, label = 1:nrow(c3$points)), size=5) 
  
# 4D
c_out <- c(4, 7, 10, 12, 15, 17, 19, 20)
c_in <- c(1:nrow(c4$edges))[-c_out]
p4 <- ggplot() +
  geom_point(data=c4$points, aes(x=Var1, y=Var2)) +
  geom_segment(data=c4$edges[c_in,], 
               aes(x=c4$points$Var1[from], 
                   xend=c4$points$Var1[to],
                   y=c4$points$Var2[from], 
                   yend=c4$points$Var2[to])) + 
  geom_segment(data=c4$edges[c_out,], 
               aes(x=c4$points$Var1[from], 
                   xend=c4$points$Var1[to],
                   y=c4$points$Var2[from], 
                   yend=c4$points$Var2[to]),
               linetype = 3, colour = d_line_clr) + 
  geom_point(data=c4$points[1:8,], aes(x=Var1, y=Var2), colour = new_d_clr) +
  geom_segment(data=c4$edges.sub, 
               aes(x=c4$points$Var1[from], 
                   xend=c4$points$Var1[to],
                   y=c4$points$Var2[from], 
                   yend=c4$points$Var2[to]), colour = new_d_clr) + 
  ggtitle("4D") +
  theme_void() +
  theme(title = element_text(colour = "black", size = 24),
        aspect.ratio = 1)
# p4 + geom_text_repel(data=c4$points, aes(x=Var1, y=Var2, label = 1:nrow(c4$points)), size=5) 

# 5D
c_out <- c(5,9,13,16,20,23,26,28,32,35,38,
           40,43,45,47,48)
c_in <- c(1:nrow(c5$edges))[-c_out]
p5 <- ggplot() +
  geom_point(data=c5$points, aes(x=Var1, y=Var2)) +
  geom_segment(data=c5$edges[c_in,], 
               aes(x=c5$points$Var1[from], 
                   xend=c5$points$Var1[to],
                   y=c5$points$Var2[from], 
                   yend=c5$points$Var2[to])) + 
  geom_segment(data=c5$edges[c_out,], 
               aes(x=c5$points$Var1[from], 
                   xend=c5$points$Var1[to],
                   y=c5$points$Var2[from], 
                   yend=c5$points$Var2[to]),
               linetype = 3, colour = d_line_clr) + 
  geom_point(data=c5$points[1:16,], aes(x=Var1, y=Var2), colour = new_d_clr) +
  geom_segment(data=c5$edges.sub, 
               aes(x=c5$points$Var1[from], 
                   xend=c5$points$Var1[to],
                   y=c5$points$Var2[from], 
                   yend=c5$points$Var2[to]), colour = new_d_clr) + 
  ggtitle("5D") +
  theme_void() +
  theme(title = element_text(colour = "black", size = 24),
        aspect.ratio = 1)
# p5 + geom_text_repel(data=c5$points, aes(x=Var1, y=Var2, label = 1:nrow(c5$points)), size=5) 

p1 + p2 + p3 + p4 + p5 + 
  plot_layout(ncol = 5)



#| label: fig-density
#| fig-cap: "Illustration of data crowding in the low-dimensional projection as dimension increases, here from 3, 10, 100. Colour shows the number of points in each hexagon bin (pink is large, navy is small). As dimension increases the points concentrate near the centre."
#| out-width: 95%
#| fig-width: 6
#| fig-height: 2
#| fig-align: center
#| message: false
#| warning: false
#| echo: false
library(colorspace)
set.seed(212)
n <- 10000

# sample points, only keep first two components for 2D projection
p3 <- geozoo::sphere.solid.random(3, n)$points[, c(1,2)]
p10 <- geozoo::sphere.solid.random(10, n)$points[, c(1,2)]
p100 <- geozoo::sphere.solid.random(100, n)$points[, c(1,2)]
colnames(p3) <- c("x", "y")
colnames(p10) <- c("x", "y")
colnames(p100) <- c("x", "y")

proj_points <- as_tibble(rbind(p3, p10, p100)) %>%
  mutate(p = factor(c(rep("p = 3", n), rep("p = 10", n), rep("p = 100", n)), levels = c("p = 3", "p = 10", "p = 100")))


ggplot(proj_points, aes(x, y)) +
  geom_hex(bins = 20, aes(fill=log(..count..))) +
  scale_fill_continuous_sequential("Batlow", rev=FALSE) +
  facet_wrap(~p, scales = "free") +
  guides(fill = FALSE) +
  theme_bw() +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_blank(),
        aspect.ratio = 1)





#| label: fig-example-structure
#| fig-width: 10
#| fig-height: 3
#| out-width: 100%
#| fig-cap: "Example structures that might be visible in a 2D projection that imply presence of structure in high dimensions. These include clusters, linear and non-linear association, outliers and barriers."
#| echo: false
library(mulgar)
library(ggplot2)
library(patchwork)
library(geozoo)
library(dplyr)
data("clusters")
data("plane")
data("plane_nonlin")

plane_outliers <- plane
plane_outliers[101,] <- c(2, 2, -2, 0, 0)
plane_outliers[102,] <- c(0, 0, 0,-2, -2)

set.seed(314)
barrier <- data.frame(x1=runif(176)) %>%
  mutate(x2=runif(176, min=0, max=1-x1))

e1 <- ggplot(clusters[sample(1:300, 156),], aes(x=x3, y=x2)) +
  geom_point(colour="#EC5C00", size=2, alpha=0.8) +
  ggtitle("(a) gaps or clusters") +
  theme_void() +
  theme(aspect.ratio = 1,
        panel.border = element_rect(fill=NA,
                                    colour="black"),
        plot.margin = margin(2, 5, 0, 5))
e2 <- ggplot(plane_nonlin, aes(x=x1, y=x2)) +
  geom_point(colour="#EC5C00", size=2, alpha=0.8) +
  ggtitle("(b) non-linear association") +
  theme_void() +
  theme(aspect.ratio = 1,
        panel.border = element_rect(fill=NA,
                                    colour="black"),
        plot.margin = margin(0, 5, 0, 5))

e3 <- ggplot(plane_outliers, aes(x=x1, y=x2)) +
  geom_point(colour="#EC5C00", size=2, alpha=0.8) +
  ggtitle("(c) association + outlier") +
  theme_void() +
  theme(aspect.ratio = 1,
        panel.border = element_rect(fill=NA,
                                    colour="black"),
        plot.margin = margin(0, 5, 0, 5))

e4 <- ggplot(barrier, aes(x=x1, y=x2)) +
  geom_point(colour="#EC5C00", size=2, alpha=0.8) +
  ggtitle("(d) barrier") +
  theme_void() +
  theme(aspect.ratio = 1,
        panel.border = element_rect(fill=NA,
                                    colour="black"),
        plot.margin = margin(0, 5, 0, 5))

e1 + e2 + e3 + e4 + plot_layout(ncol=4)


#| echo: false
#| eval: false
## library(tourr)
## 
## set.seed(340)
## render_gif(clusters[,1:5],
##            grand_tour(),
##            display_trails(col="#EC5C00",
##                           axes="off",
##                           cex=2,
##                           half_range=0.8,
##                           past=5),
##            rescale=TRUE,
##            gif_file = "gifs/trails-clusters.gif",
##            frames=200,
##            width=400,
##            height=400)
## render_gif(clusters[,1:5],
##            grand_tour(),
##            display_xy(col="#EC5C00",
##                           axes="bottomleft",
##                           cex=2,
##                           half_range=0.8),
##            rescale=TRUE,
##            gif_file = "gifs/clusters-intro.gif",
##            apf = 1/50,
##            frames=500,
##            width=400,
##            height=400)
## 
## render_gif(plane_outliers[,1:5],
##            grand_tour(),
##            display_trails(col="#EC5C00",
##                           axes="off",
##                           cex=2,
##                           half_range=0.8),
##            rescale=TRUE,
##            gif_file = "gifs/trails-outlier.gif",
##            frames=200,
##            width=400,
##            height=400)
## render_gif(plane_outliers[,1:5],
##            grand_tour(),
##            display_xy(col="#EC5C00",
##                           axes="bottomleft",
##                           cex=2,
##                           half_range=0.8),
##            rescale=TRUE,
##            gif_file = "gifs/outlier-intro.gif",
##            apf = 1/50,
##            frames=500,
##            width=400,
##            height=400)


#| eval: false
#| echo: false
## # Answer to Q1
## library(tourr)
## library(geozoo)
## set.seed(1234)
## cube3 <- cube.solid.random(3, 500)$points
## cube5 <- cube.solid.random(5, 500)$points
## cube10 <- cube.solid.random(5, 500)$points
## 
## animate(cube3)
## animate(cube5)
## animate(cube10)


#| eval: false
#| echo: false
## # Answer to Q3
## library(tourr)
## library(mvtnorm)
## 
## s1 <- diag(5)
## s2 <- diag(5)
## s2[3,4] <- 0.7
## s2[4,3] <- 0.7
## s3 <- s2
## s3[1,2] <- 0.7
## s3[2,1] <- 0.7
## 
## set.seed(1234)
## d1 <- as.data.frame(rmvnorm(500, sigma = s1))
## d2 <- as.data.frame(rmvnorm(500, sigma = s2))
## d3 <- as.data.frame(rmvnorm(500, sigma = s3))
## 
## library(mulgar)
## animate_xy(c1)
## # four small clusters, two big clusters
## animate_xy(c2)
## # Six spherical clusters
## animate_xy(c3)
## # tetrahedron with lots of smaller triangles
## animate_xy(c4)
## # Two planes, and noise points
## animate_xy(c5)
## # Four connected curvilinear clusters
## animate_xy(c6)
## # Two curved clusters
## animate_xy(c7)
## # spiral

