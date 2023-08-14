#| label: hc-libraries
#| message: FALSE
#| code-summary: "Load libraries"
library(ggplot2)
library(mulgar)
library(ggdendro)
library(dplyr)
library(patchwork)
library(tourr)
library(plotly)
library(htmlwidgets)
library(colorspace)
library(GGally)


#| eval: true
#| code-fold: false
#| message: false
#| warning: false
data(simple_clusters)

# Compute hierarchical clustering with Ward's linkage
cl_hw <- hclust(dist(simple_clusters[,1:2]),
                method="ward.D2")
cl_ggd <- dendro_data(cl_hw, type = "triangle")

# Compute dendrogram in the data
cl_hfly <- hierfly(simple_clusters, cl_hw, scale=FALSE)

# Show result
simple_clusters <- simple_clusters %>%
  mutate(clw = factor(cutree(cl_hw, 2)))


#| label: fig-hc-sim
#| eval: true
#| code-summary: "Code to make the four plots"
#| message: false
#| warning: false
#| fig-cap: "Hierarchical clustering on simulated data: (a) data, (b) dendrogram, (c) dendrogram on the data, and (d) two cluster solution. The extra points corresponding to nodes of the dendrogram are indicated by + in (c). The last join in the dendrogram (b), can be seen to correspond to the edges connecting the gap, when displayed with the data (c). The other joins can be seen to be pulling together points within each clump."
#| fig-width: 7
#| fig-height: 7
#| out-width: 100%
# Plot the data
pd <- ggplot(simple_clusters, aes(x=x1, y=x2)) +
  geom_point(colour="#3B99B1", size=2, alpha=0.8) +
  ggtitle("(a)") + 
  theme_minimal() +
  theme(aspect.ratio=1) 

# Plot the dendrogram
ph <- ggplot() +
  geom_segment(data=cl_ggd$segments, 
               aes(x = x, y = y, 
                   xend = xend, yend = yend)) + 
  geom_point(data=cl_ggd$labels, aes(x=x, y=y),
             colour="#3B99B1", alpha=0.8) +
  ggtitle("(b)") + 
  theme_minimal() +
  theme_dendro()

# Plot the dendrogram on the data
pdh <- ggplot() +
  geom_segment(data=cl_hfly$segments, 
                aes(x=x, xend=xend,
                    y=y, yend=yend)) +
  geom_point(data=cl_hfly$data, 
             aes(x=x1, y=x2,
                 shape=factor(node),
                 colour=factor(node),
                 size=1-node), alpha=0.8) +
  xlab("x1") + ylab("x2") +
  scale_shape_manual(values = c(16, 3)) +
  scale_colour_manual(values = c("#3B99B1", "black")) +
  scale_size(limits=c(0,17)) +
  ggtitle("(c)") + 
  theme_minimal() +
  theme(aspect.ratio=1, legend.position="none")

# Plot the resulting clusters
pc <- ggplot(simple_clusters) +
  geom_point(aes(x=x1, y=x2, colour=clw), 
             size=2, alpha=0.8) +
  scale_colour_discrete_divergingx(palette = "Zissou 1",
                                   nmax=5, rev=TRUE) +
  ggtitle("(d)") + 
  theme_minimal() +
  theme(aspect.ratio=1, legend.position="none")

pd + ph + pdh + pc + plot_layout(ncol=2)


#| label: fig-problems
#| eval: true
#| message: false
#| warning: false
#| fig-cap: "Two examples of data structure that causes problems for hierarchical clustering. Nuisance observations can cause problems because the close observations between the two clusters can cause some chaining in the hierarchical joining of observations.  Nuisance variables can cause problems because observations across the gap can seem closer than observations at the end of each cluster."
#| fig-width: 8
#| fig-height: 4
#| code-summary: "Code to make plots"
# Nuisance observations
set.seed(20190514)
x <- (runif(20)-0.5)*4
y <- x
d1 <- data.frame(x1 = c(rnorm(50, -3), 
                            rnorm(50, 3), x),
                 x2 = c(rnorm(50, -3), 
                            rnorm(50, 3), y),
                 cl = factor(c(rep("A", 50), 
                             rep("B", 70))))
d1 <- d1 %>% 
  mutate_if(is.numeric, function(x) (x-mean(x))/sd(x))
pd1 <- ggplot(data=d1, aes(x=x1, y=x2)) + 
  geom_point() +
    ggtitle("Nuisance observations") + 
  theme_minimal() +
    theme(aspect.ratio=1) 

# Nuisance variables
set.seed(20190512)
d2 <- data.frame(x1=c(rnorm(50, -4), 
                            rnorm(50, 4)),
                 x2=c(rnorm(100)),
                 cl = factor(c(rep("A", 50), 
                             rep("B", 50))))
d2 <- d2 %>% 
  mutate_if(is.numeric, function(x) (x-mean(x))/sd(x))
pd2 <- ggplot(data=d2, aes(x=x1, y=x2)) + 
  geom_point() +
    ggtitle("Nuisance variables") + 
  theme_minimal() +
    theme(aspect.ratio=1)

pd1 + pd2 + plot_layout(ncol=2)


#| label: fig-d1-s
#| eval: true
#| message: false
#| warning: false
#| fig-cap: "The effect of nuisance observations on single linkage (a, b, c) and Ward's linkage hierarchical clustering (d, e, f). The single linkage dendrogram is very different to the Wards linkage dendrogram. When plotted with the data (b) we can see a pin cushion or asterisk pattern, where points are joined to others through a place in the middle of the line of nuisance observations. This results in the bad two cluster solution of a singleton cluster, and all the rest. Conversely, Ward's dendrogram (d) strongly suggests two clusters, although the final join corresponds to just a small gap when shown on the data (e) but results in two sensible clusters."
#| fig-width: 9
#| fig-height: 6
#| code-summary: "Code to make plots"
# Compute single linkage
d1_hs <- hclust(dist(d1[,1:2]),
                method="single")
d1_ggds <- dendro_data(d1_hs, type = "triangle")
pd1s <- ggplot() +
  geom_segment(data=d1_ggds$segments, 
               aes(x = x, y = y, 
                   xend = xend, yend = yend)) + 
  geom_point(data=d1_ggds$labels, aes(x=x, y=y),
             colour="#3B99B1", alpha=0.8) +
  theme_minimal() +
  ggtitle("(a) Single linkage dendrogram") +
  theme_dendro()

# Compute dendrogram in data
d1_hflys <- hierfly(d1, d1_hs, scale=FALSE)

pd1hs <- ggplot() +
  geom_segment(data=d1_hflys$segments, 
                aes(x=x, xend=xend,
                    y=y, yend=yend)) +
  geom_point(data=d1_hflys$data, 
             aes(x=x1, y=x2,
                 shape=factor(node),
                 colour=factor(node),
                 size=1-node), alpha=0.8) +
  scale_shape_manual(values = c(16, 3)) +
  scale_colour_manual(values = c("#3B99B1", "black")) +
  scale_size(limits=c(0,17)) +
  ggtitle("(b) Dendrogram in data space") + 
  theme_minimal() +
  theme(aspect.ratio=1, legend.position="none")

# Show result
d1 <- d1 %>%
  mutate(cls = factor(cutree(d1_hs, 2)))
pc_d1s <- ggplot(d1) +
  geom_point(aes(x=x1, y=x2, colour=cls), 
             size=2, alpha=0.8) +
  scale_colour_discrete_divergingx(palette = "Zissou 1",
                                   nmax=4, rev=TRUE) +
  ggtitle("(c) Two-cluster solution") + 
  theme_minimal() +
  theme(aspect.ratio=1, legend.position="none")

# Compute Wards linkage
d1_hw <- hclust(dist(d1[,1:2]),
                method="ward.D2")
d1_ggdw <- dendro_data(d1_hw, type = "triangle")
pd1w <- ggplot() +
  geom_segment(data=d1_ggdw$segments, 
               aes(x = x, y = y, 
                   xend = xend, yend = yend)) + 
  geom_point(data=d1_ggdw$labels, aes(x=x, y=y),
             colour="#3B99B1", alpha=0.8) +
  ggtitle("(d) Ward's linkage dendrogram") +
  theme_minimal() +
  theme_dendro()

# Compute dendrogram in data
d1_hflyw <- hierfly(d1, d1_hw, scale=FALSE)

pd1hw <- ggplot() +
  geom_segment(data=d1_hflyw$segments, 
                aes(x=x, xend=xend,
                    y=y, yend=yend)) +
  geom_point(data=d1_hflyw$data, 
             aes(x=x1, y=x2,
                 shape=factor(node),
                 colour=factor(node),
                 size=1-node), alpha=0.8) +
  scale_shape_manual(values = c(16, 3)) +
  scale_colour_manual(values = c("#3B99B1", "black")) +
  scale_size(limits=c(0,17)) +
  ggtitle("(e) Dendrogram in data space") + 
  theme_minimal() +
  theme(aspect.ratio=1, legend.position="none")

# Show result
d1 <- d1 %>%
  mutate(clw = factor(cutree(d1_hw, 2)))
pc_d1w <- ggplot(d1) +
  geom_point(aes(x=x1, y=x2, colour=clw), 
             size=2, alpha=0.8) +
  scale_colour_discrete_divergingx(palette = "Zissou 1",
                                   nmax=4, rev=TRUE) +
  ggtitle("(f) Two-cluster solution") + 
  theme_minimal() +
  theme(aspect.ratio=1, legend.position="none")

pd1s + pd1hs + pc_d1s + 
  pd1w + pd1hw + pc_d1w +
  plot_layout(ncol=3)


#| label: fig-d2-c
#| eval: true
#| message: false
#| warning: false
#| fig-cap: "Complete linkage clustering (a, b, c) on nuisance variables in comparison to Ward's linkage (d, e, f). The two dendrograms (a, d) look similar but when plotted on the data (b, e) we can see they are very different solutions. The complete linkage result breaks the data into clusters across the gap (c), which is a bad solution. It has been distract by the nuisance variables. Conversely, the Wards linkage two-cluster solution does as hoped, divided the data into two clusters separated by the gap (f)."
#| fig-width: 9
#| fig-height: 6
# Compute complete linkage
d2_hc <- hclust(dist(d2[,1:2]),
                method="complete")
d2_ggdc <- dendro_data(d2_hc, type = "triangle")
pd2c <- ggplot() +
  geom_segment(data=d2_ggdc$segments, 
               aes(x = x, y = y, 
                   xend = xend, yend = yend)) + 
  geom_point(data=d2_ggdc$labels, aes(x=x, y=y),
             colour="#3B99B1", alpha=0.8) +
  ggtitle("(a) Complete linkage dendrogram") +
  theme_minimal() +
  theme_dendro()

# Compute dendrogram in data
d2_hflyc <- hierfly(d2, d2_hc, scale=FALSE)

pd2hc <- ggplot() +
  geom_segment(data=d2_hflyc$segments, 
                aes(x=x, xend=xend,
                    y=y, yend=yend)) +
  geom_point(data=d2_hflyc$data, 
             aes(x=x1, y=x2,
                 shape=factor(node),
                 colour=factor(node),
                 size=1-node), alpha=0.8) +
  scale_shape_manual(values = c(16, 3)) +
  scale_colour_manual(values = c("#3B99B1", "black")) +
  scale_size(limits=c(0,17)) +
  ggtitle("(b) Dendrogram in data space") + 
  theme_minimal() +
  theme(aspect.ratio=1, legend.position="none")

# Show result
d2 <- d2 %>%
  mutate(clc = factor(cutree(d2_hc, 2)))
pc_d2c <- ggplot(d2) +
  geom_point(aes(x=x1, y=x2, colour=clc), 
             size=2, alpha=0.8) +
  scale_colour_discrete_divergingx(palette = "Zissou 1",
                                   nmax=4, rev=TRUE) +
  ggtitle("(c) Two-cluster solution") + 
  theme_minimal() +
  theme(aspect.ratio=1, legend.position="none")

# Compute Wards linkage
d2_hw <- hclust(dist(d2[,1:2]),
                method="ward.D2")
d2_ggdw <- dendro_data(d2_hw, type = "triangle")
pd2w <- ggplot() +
  geom_segment(data=d2_ggdw$segments, 
               aes(x = x, y = y, 
                   xend = xend, yend = yend)) + 
  geom_point(data=d2_ggdw$labels, aes(x=x, y=y),
             colour="#3B99B1", alpha=0.8) +
  ggtitle("(d) Ward's linkage dendrogram") +
  theme_minimal() +
  theme_dendro()

# Compute dendrogram in data
d2_hflyw <- hierfly(d2, d2_hw, scale=FALSE)

pd2hw <- ggplot() +
  geom_segment(data=d2_hflyw$segments, 
                aes(x=x, xend=xend,
                    y=y, yend=yend)) +
  geom_point(data=d2_hflyw$data, 
             aes(x=x1, y=x2,
                 shape=factor(node),
                 colour=factor(node),
                 size=1-node), alpha=0.8) +
  scale_shape_manual(values = c(16, 3)) +
  scale_colour_manual(values = c("#3B99B1", "black")) +
  scale_size(limits=c(0,17)) +
  ggtitle("(e) Dendrogram in data space") + 
  theme_minimal() +
  theme(aspect.ratio=1, legend.position="none")

# Show result
d2 <- d2 %>%
  mutate(clw = factor(cutree(d2_hw, 2)))
pc_d2w <- ggplot(d2) +
  geom_point(aes(x=x1, y=x2, colour=clw), 
             size=2, alpha=0.8) +
  scale_colour_discrete_divergingx(palette = "Zissou 1",
                                   nmax=4, rev=TRUE) +
  ggtitle("(f) Two-cluster solution") + 
  theme_minimal() +
  theme(aspect.ratio=1, legend.position="none")

pd2c + pd2hc + pc_d2c + 
  pd2w + pd2hw + pc_d2w +
  plot_layout(ncol=3)


#| label: fig-penguins-pairs
#| message: false
#| warning: false
#| fig-cap: "Make a scatterplot matrix to check for the presence of clustering, shape of clusters and presence of nuisance observations and variables. In the penguins it appears that there might be three elliptically shaped clusters, with some nuisance observations."
#| fig-width: 8
#| fig-height: 8
#| code-summary: "Code for scatterplot matrix"
load("data/penguins_sub.rda")
ggscatmat(penguins_sub[,1:4]) + 
  theme_minimal() +
  xlab("") + ylab("")


#| eval: false
#| code-summary: "Code to create tour"
## set.seed(20230329)
## b <- basis_random(4,2)
## pt1 <- save_history(penguins_sub[,1:4],
##                     max_bases = 500,
##                     start = b)
## save(pt1, file="data/penguins_tour_path.rda")
## 
## # To re-create the gifs
## load("data/penguins_tour_path.rda")
## animate_xy(penguins_sub[,1:4],
##            tour_path = planned_tour(pt1),
##            axes="off", rescale=FALSE,
##            half_range = 3.5)
## 
## render_gif(penguins_sub[,1:4],
##            planned_tour(pt1),
##            display_xy(half_range=0.9, axes="off"),
##            gif_file="gifs/penguins_gt.gif",
##            frames=500,
##            loop=FALSE)


#| label: penguins-hclust
#| code-fold: false
p_dist <- dist(penguins_sub[,1:4])
p_hcw <- hclust(p_dist, method="ward.D2")
p_hcs <- hclust(p_dist, method="single")

p_clw <- penguins_sub %>% 
  mutate(cl = factor(cutree(p_hcw, 3))) %>%
  as.data.frame()
p_cls <- penguins_sub %>% 
  mutate(cl = factor(cutree(p_hcs, 3))) %>%
  as.data.frame()

p_w_hfly <- hierfly(p_clw, p_hcw, scale=FALSE)
p_s_hfly <- hierfly(p_cls, p_hcs, scale=FALSE)


#| code-summary: "Code to draw dendrograms"
# Generate the dendrograms in 2D
p_hcw_dd <- dendro_data(p_hcw)
pw_dd <- ggplot() +
  geom_segment(data=p_hcw_dd$segments, 
               aes(x = x, y = y, 
                   xend = xend, yend = yend)) + 
  geom_point(data=p_hcw_dd$labels, aes(x=x, y=y),
             alpha=0.8) +
  theme_dendro()

p_hcs_dd <- dendro_data(p_hcs)
ps_dd <- ggplot() +
  geom_segment(data=p_hcs_dd$segments, 
               aes(x = x, y = y, 
                   xend = xend, yend = yend)) + 
  geom_point(data=p_hcs_dd$labels, aes(x=x, y=y),
             alpha=0.8) +
  theme_dendro()


#| label: penguins-hfly
#| eval: FALSE
#| code-summary: "Code to create tours of dendrogram in data"
## load("data/penguins_tour_path.rda")
## glyphs <- c(16, 46)
## pchw <- glyphs[p_w_hfly$data$node+1]
## pchs <- glyphs[p_s_hfly$data$node+1]
## 
## animate_xy(p_w_hfly$data[,1:4],
##            #col=colw,
##            tour_path = planned_tour(pt1),
##            pch = pchw,
##            edges=p_w_hfly$edges,
##            axes="bottomleft")
## 
## animate_xy(p_s_hfly$data[,1:4],
##            #col=colw,
##            tour_path = planned_tour(pt1),
##            pch = pchs,
##            edges=p_s_hfly$edges,
##            axes="bottomleft")
## 
## render_gif(p_w_hfly$data[,1:4],
##            planned_tour(pt1),
##            display_xy(half_range=0.9,
##                       pch = pchw,
##                       edges = p_w_hfly$edges,
##                       axes = "off"),
##            gif_file="gifs/penguins_hflyw.gif",
##            frames=500,
##            loop=FALSE)
## 
## render_gif(p_s_hfly$data[,1:4],
##            planned_tour(pt1),
##            display_xy(half_range=0.9,
##                       pch = pchs,
##                       edges = p_s_hfly$edges,
##                       axes = "off"),
##            gif_file="gifs/penguins_hflys.gif",
##            frames=500,
##            loop=FALSE)
## 
## # Show three cluster solutions
## clrs <- hcl.colors(3, "Zissou 1")
## w3_col <- clrs[p_w_hfly$data$cl[p_w_hfly$data$node == 0]]
## render_gif(p_w_hfly$data[p_w_hfly$data$node == 0, 1:4],
##            planned_tour(pt1),
##            display_xy(half_range=0.9,
##                       col=w3_col,
##                       axes = "off"),
##            gif_file="gifs/penguins_w3.gif",
##            frames=500,
##            loop=FALSE)
## 
## s3_col <- clrs[p_s_hfly$data$cl[p_w_hfly$data$node == 0]]
## render_gif(p_s_hfly$data[p_w_hfly$data$node == 0,1:4],
##            planned_tour(pt1),
##            display_xy(half_range=0.9,
##                       col=s3_col,
##                       axes = "off"),
##            gif_file="gifs/penguins_s3.gif",
##            frames=500,
##            loop=FALSE)


#| label: fig-penguins-ddw
#| fig-cap: Wards linkage
#| echo: FALSE
pw_dd


#| label: fig-penguins-dds
#| fig-cap: single linkage
#| echo: FALSE
ps_dd


#| label: penguins-hfly-plotly
#| eval: FALSE
#| code-summary: "Code to make html objects of the dendrogram in 4D"
## load("data/penguins_tour_path.rda")
## # Create a smaller one, for space concerns
## pt1i <- interpolate(pt1[,,1:5], 0.1)
## pw_anim <- render_anim(p_w_hfly$data,
##                        vars=1:4,
##                        frames=pt1i,
##                        edges = p_w_hfly$edges,
##              obs_labels=paste0(1:nrow(p_w_hfly$data),
##                                p_w_hfly$data$cl))
## 
## pw_gp <- ggplot() +
##      geom_segment(data=pw_anim$edges,
##                     aes(x=x, xend=xend,
##                         y=y, yend=yend,
##                         frame=frame)) +
##      geom_point(data=pw_anim$frames,
##                 aes(x=P1, y=P2,
##                     frame=frame,
##                     shape=factor(node),
##                     label=obs_labels),
##                 alpha=0.8, size=1) +
##      xlim(-1,1) + ylim(-1,1) +
##      scale_shape_manual(values=c(16, 46)) +
##      coord_equal() +
##      theme_bw() +
##      theme(legend.position="none",
##            axis.text=element_blank(),
##            axis.title=element_blank(),
##            axis.ticks=element_blank(),
##            panel.grid=element_blank())
## 
## pwg <- ggplotly(pw_gp, width=450, height=500,
##                 tooltip="label") %>%
##        animation_button(label="Go") %>%
##        animation_slider(len=0.8, x=0.5,
##                         xanchor="center") %>%
##        animation_opts(easing="linear", transition = 0)
## htmlwidgets::saveWidget(pwg,
##           file="html/penguins_cl_ward.html",
##           selfcontained = TRUE)
## 
## # Single
## ps_anim <- render_anim(p_s_hfly$data, vars=1:4,
##                          frames=pt1i,
##                        edges = p_s_hfly$edges,
##              obs_labels=paste0(1:nrow(p_s_hfly$data),
##                                p_s_hfly$data$cl))
## 
## ps_gp <- ggplot() +
##      geom_segment(data=ps_anim$edges,
##                     aes(x=x, xend=xend,
##                         y=y, yend=yend,
##                         frame=frame)) +
##      geom_point(data=ps_anim$frames,
##                 aes(x=P1, y=P2,
##                     frame=frame,
##                     shape=factor(node),
##                     label=obs_labels),
##                 alpha=0.8, size=1) +
##      xlim(-1,1) + ylim(-1,1) +
##      scale_shape_manual(values=c(16, 46)) +
##      coord_equal() +
##      theme_bw() +
##      theme(legend.position="none",
##            axis.text=element_blank(),
##            axis.title=element_blank(),
##            axis.ticks=element_blank(),
##            panel.grid=element_blank())
## 
## psg <- ggplotly(ps_gp, width=450, height=500,
##                 tooltip="label") %>%
##        animation_button(label="Go") %>%
##        animation_slider(len=0.8, x=0.5,
##                         xanchor="center") %>%
##        animation_opts(easing="linear", transition = 0)
## htmlwidgets::saveWidget(psg,
##           file="html/penguins_cl_single.html",
##           selfcontained = TRUE)


#| eval: false
#| echo: false
#| message: false
#| warning: false
## nl_wl <- hclust(dist(clusters_nonlin,
##                      method = "euclidean"),
##                   method = "ward.D2")
## nl_sl <- hclust(dist(clusters_nonlin,
##                          method = "euclidean"),
##                   method = "single")
## # assume 4 clusters
## nl_clw <- clusters_nonlin %>%
##   as_tibble() %>%
##   mutate(cl_wl = factor(cutree(nl_wl, 4))) %>%
##   mutate(cl_sl = factor(cutree(nl_sl, 4)))
## library(ggdendro)
## ggplot() +
##   geom_segment(data=dendro_data(nl_wl)$segments,
##                aes(x = x, y = y,
##                    xend = xend, yend = yend)) +
##   theme_dendro()
## ggplot() +
##   geom_segment(data=dendro_data(nl_sl)$segments,
##                aes(x = x, y = y,
##                    xend = xend, yend = yend)) +
##   theme_dendro()
## 
## nl_hfly_wl <- hierfly(nl_clw, nl_wl, scale=FALSE)
## nl_hfly_sl <- hierfly(nl_clw, nl_sl, scale=FALSE)
## glyphs <- c(16, 46)
## pchw_wl <- glyphs[nl_hfly_wl$data$node+1]
## pchw_sl <- glyphs[nl_hfly_sl$data$node+1]
## animate_xy(nl_hfly_wl$data[,1:4],
##            col=nl_clw$cl_wl,
##            tour_path = grand_tour(),
##            pch = pchw_wl,
##            edges=nl_hfly_wl$edges,
##            axes="bottomleft",
##            rescale=FALSE)
## animate_xy(nl_hfly_sl$data[,1:4],
##            col=nl_clw$cl_sl,
##            tour_path = grand_tour(),
##            pch = pchw_sl,
##            edges=nl_hfly_sl$edges,
##            axes="bottomleft",
##            rescale=FALSE)
## 

