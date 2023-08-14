#| message: false
#| code-summary: "Load libraries"
library(mclust) 
library(tidyr)
library(dplyr)
library(gt)
library(cxhull)
library(ggplot2)
library(colorspace)


#| message: false
#| code-summary: "Code to do clustering"
library(mclust) 
library(tidyr)
library(dplyr)
library(gt)
library(cxhull)
library(ggplot2)
library(colorspace)
load("data/penguins_sub.rda")
p_dist <- dist(penguins_sub[,1:4])
p_hcw <- hclust(p_dist, method="ward.D2")

p_cl <- data.frame(cl_w = cutree(p_hcw, 3))

penguins_mc <- Mclust(penguins_sub[,1:4], 
                      G=3, 
                      modelNames = "EEE")
p_cl <- p_cl %>% 
  mutate(cl_mc = penguins_mc$classification)

p_cl <- p_cl %>% 
  mutate(cl_w_j = jitter(cl_w),
         cl_mc_j = jitter(cl_mc))

# Arranging by cluster id is important to define edges 
penguins_cl <- penguins_sub %>%
  mutate(cl_w = p_cl$cl_w,
         cl_mc = p_cl$cl_mc) %>%
  arrange(cl_w)


#| code-summary: "Code for convex hulls in 2D"

# Penguins in 2D
# Duplicate observations need to be removed fo convex hull calculation
psub <- penguins_cl %>%
  select(bl, bd) 
dup <- duplicated(psub)
psub <- penguins_cl %>%
  select(bl, bd, cl_w) %>%
  filter(!dup) %>%
  arrange(cl_w)

ncl <- psub %>%
  count(cl_w) %>%
  arrange(cl_w) %>%
  mutate(cumn = cumsum(n))
phull <- NULL
for (i in unique(psub$cl_w)) {
  x <- psub %>%
    dplyr::filter(cl_w == i) %>%
    select(bl, bd) 
  ph <- cxhull(as.matrix(x))$edges
  if (i > 1) {
    ph <- ph + ncl$cumn[i-1]
  }
  ph <- cbind(ph, rep(i, nrow(ph)))
  phull <- rbind(phull, ph)
}
phull <- as.data.frame(phull)
colnames(phull) <- c("from", "to", "cl_w") 
phull_segs <- data.frame(x = psub$bl[phull$from],
                         y = psub$bd[phull$from],
                         xend = psub$bl[phull$to],
                         yend = psub$bd[phull$to],
                         cl_w = phull$cl_w)
phull_segs$cl_w <- factor(phull$cl_w) 
psub$cl_w <- factor(psub$cl_w)
p_chull2D <- ggplot() +
  geom_point(data=psub, aes(x=bl, y=bd, 
                            colour=cl_w)) + 
  geom_segment(data=phull_segs, aes(x=x, xend=xend,
                                    y=y, yend=yend,
                                    colour=cl_w)) +
  scale_colour_discrete_divergingx(palette = "Zissou 1") +
  theme_minimal() +
  theme(aspect.ratio = 1)


#| eval: false
#| code-summary: "Code to generate pD convex hull and view in tour"

## ncl <- penguins_cl %>%
##   count(cl_w) %>%
##   arrange(cl_w) %>%
##   mutate(cumn = cumsum(n))
## phull <- NULL
## for (i in unique(penguins_cl$cl_w)) {
##   x <- penguins_cl %>%
##     dplyr::filter(cl_w == i)
##   ph <- cxhull(as.matrix(x[,1:4]))$edges
##   if (i > 1) {
##     ph <- ph + ncl$cumn[i-1]
##   }
##   ph <- cbind(ph, rep(i, nrow(ph)))
##   phull <- rbind(phull, ph)
## }
## phull <- as.data.frame(phull)
## colnames(phull) <- c("from", "to", "cl_w")
## phull$cl_w <- factor(phull$cl_w)
## penguins_cl$cl_w <- factor(penguins_cl$cl_w)
## 
## animate_xy(penguins_cl[,1:4], col=penguins_cl$cl_w,
##            edges=as.matrix(phull[,1:2]), edges.col=phull$cl_w)
## render_gif(penguins_cl[,1:4],
##            tour_path = grand_tour(),
##            display = display_xy(col=penguins_cl$cl_w,
##                                 edges=as.matrix(phull[,1:2]),
##                                 edges.col=phull$cl_w),
##            gif_file = "gifs/penguins_chull.gif",
##            frames = 500,
##            width = 400,
##            height = 400)


#| eval: false
#| echo: false

## # This code is checking that convex hull works
## library(geozoo)
## 
## cb <- cube.solid.random(p=4, n=1000)$points
## phull <- cxhull(cb)$edges
## 
## animate_xy(cb, edges=phull)
## 
## smp <- simplex(p=4)$points
## smp_phull <- cxhull(smp)$edges
## animate_xy(smp, edges=smp_phull)
## 
## sph <- rbind(sphere.solid.random(p=3)$points,
##              sphere.hollow(p=3)$points)
## sph_phull <- cxhull(sph)$edges
## animate_xy(as.data.frame(sph), edges=sph_phull)


#| label: fig-penguin-hull-2D
#| echo: false
#| fig-width: 4
#| fig-height: 4
#| fig-cap: "2D"
p_chull2D 


#| code-summary: "Code for confusion table"
p_cl %>% 
  count(cl_w, cl_mc) %>% 
  pivot_wider(names_from = cl_mc, 
              values_from = n, 
              values_fill = 0) %>%
  gt() %>%
  tab_spanner(label = "cl_mc", columns=c(`2`, `3`, `1`)) %>%
  cols_width(everything() ~ px(60))


#| eval: false
#| message: false
#| code-summary: Code to do linked brushing with liminal
## library(liminal)
## limn_tour_link(
##   p_cl[,3:4],
##   penguins_cl,
##   cols = bl:bm,
##   color = cl_w
## )


#| eval: false
#| echo: false
#| message: false
#| warning: false
## # install.packages("https://homepage.boku.ac.at/leisch/MSA/packages/MSA_0.3-1.tar.gz", repos = NULL, type = "source")
## library(MSA)
## library(tidyverse)
## data("risk", package = "MSA")
## # looking at the data
## library(tourr)
## animate_xy(risk)
## # hierarchical clustering solutions
## risk_h_mc <- hclust(dist(risk, method = "manhattan"),
##                   method = "complete")
## risk_h_ms <- hclust(dist(risk, method = "manhattan"),
##                   method = "single")
## risk_h_ew <- hclust(dist(risk, method = "euclidean"),
##                   method = "ward.D2")
## # adding the clustering information to the data
## risk_clw <- risk %>%
##   as_tibble() %>%
##   mutate(cl_h_mc = factor(cutree(risk_h_mc, 6))) %>%
##   mutate(cl_h_ms = factor(cutree(risk_h_ms, 6))) %>%
##   mutate(cl_h_ew = factor(cutree(risk_h_ew, 6)))
## # drawing 2D dendrograms
## library(ggdendro)
## ggplot() +
##   geom_segment(data=dendro_data(risk_h_mc)$segments,
##                aes(x = x, y = y,
##                    xend = xend, yend = yend)) +
##   theme_dendro()
## ggplot() +
##   geom_segment(data=dendro_data(risk_h_ms)$segments,
##                aes(x = x, y = y,
##                    xend = xend, yend = yend)) +
##   theme_dendro()
## ggplot() +
##   geom_segment(data=dendro_data(risk_h_ew)$segments,
##                aes(x = x, y = y,
##                    xend = xend, yend = yend)) +
##   theme_dendro()
## # dendrogram in the data space
## library(mulgar)
## risk_hfly_mc <- hierfly(risk_clw, risk_h_mc, scale=FALSE)
## risk_hfly_ms <- hierfly(risk_clw, risk_h_ms, scale=FALSE)
## risk_hfly_ew <- hierfly(risk_clw, risk_h_ew, scale=FALSE)
## glyphs <- c(16, 46)
## pchw_mc <- glyphs[risk_hfly_mc$data$node+1]
## pchw_ms <- glyphs[risk_hfly_ms$data$node+1]
## pchw_ew <- glyphs[risk_hfly_ew$data$node+1]
## # manhattan + complete
## # we can see hierarchical structure, small groups at the
## # edges that get connected first and then combined into
## # larger cluster, looks like some of the clusters are
## # really spread out across the data space
## animate_xy(risk_hfly_mc$data[,1:6],
##            col=risk_clw$cl_h_mc,
##            tour_path = grand_tour(),
##            pch = pchw_mc,
##            edges=risk_hfly_mc$edges,
##            axes="bottomleft",
##            rescale=FALSE)
## # Manhattan + single
## # pretty much all the edges point in towards the center!
## animate_xy(risk_hfly_ms$data[,1:6],
##            col=risk_clw$cl_h_ms,
##            tour_path = grand_tour(),
##            pch = pchw_ms,
##            edges=risk_hfly_ms$edges,
##            axes="bottomleft",
##            rescale=FALSE)
## # euclidean + ward
## # at this stage looks mostly similar to mc case
## animate_xy(risk_hfly_ew$data[,1:6],
##            col=risk_clw$cl_h_ew,
##            tour_path = grand_tour(),
##            pch = pchw_ew,
##            edges=risk_hfly_ew$edges,
##            axes="bottomleft",
##            rescale=FALSE)
## 
## # comparison of two solutions
## # confusion table shows quite some disagreements
## risk_clw %>%
##   count(cl_h_mc, cl_h_ew) %>%
##   pivot_wider(names_from = cl_h_mc,
##               values_from = n,
##               values_fill = 0)
## # explore with liminal
## liminal::limn_tour_link(
##   tibble::as.tibble(cbind(jitter(as.numeric(risk_clw$cl_h_ew)),
##                           jitter(as.numeric(risk_clw$cl_h_mc)))),
##   risk_clw,
##   cols = 1:6,
##   color = cl_h_ew)
## # for example we can see cluster 2 for mc solution is very large,
## # in the ew solution this is split up into 3 larger clusters,
## # and in the tour we see how the smaller clusters 5 and 6 are different
## # and less compact than the largest cluster 2 for the ew solution
## 
## # k-means + tour with cluster means
## r_km <- kmeans(risk, centers=6,
##                      iter.max = 500, nstart = 5)
## r_km_means <- data.frame(r_km$centers) %>%
##   mutate(cl = factor(rownames(r_km$centers)))
## r_km_d <- as.tibble(risk) %>%
##   mutate(cl = factor(r_km$cluster))
## r_km_means <- r_km_means %>%
##   mutate(type = "mean")
## r_km_d <- r_km_d %>%
##   mutate(type = "data")
## r_km_all <- bind_rows(r_km_means, r_km_d)
## r_km_all$type <- factor(r_km_all$type, levels=c("mean", "data"))
## r_pch <- c(3, 46)[as.numeric(r_km_all$type)]
## r_cex <- c(3, 1)[as.numeric(r_km_all$type)]
## animate_xy(r_km_all[,1:6], col=r_km_all$cl,
##            pch=r_pch, cex=r_cex, axes="bottomleft")
## 
## # guided tour + interpretation in terms of variables
## set.seed(543)
## animate_xy(r_km_all[,1:6],
##            tour_path = guided_tour(lda_pp(r_km_all$cl)) ,
##            col=r_km_all$cl,
##            pch=r_pch, cex=r_cex, axes="bottomleft")
## # from this solution it seems there is an overall
## # risk behavior captured in the clustering, i.e. most
## # variable contribute to separating the clusters along the
## # x direction
## # health risks seem to be a bit different, with one cluster
## # containing average risk scores apart from high risk
## # scores for the health variable
## 
## # comparison k-means vs hierarchical clustering solution with liminal
## # we can see that the k-means grouping is very different
## liminal::limn_tour_link(
##   tibble::as.tibble(cbind(jitter(as.numeric(r_km_d$cl)),
##                           jitter(as.numeric(risk_clw$cl_h_mc)))),
##   r_km_d,
##   cols = 1:6,
##   color = cl)
## 
## # comparison k-means vs hierarchical clustering solution
## # using convex hull in the data space
## # need to do one at a time since there is too much going on
## # starting with hierarchical clustering solution
## library(cxhull)
## dup <- duplicated(risk_clw[,1:6])
## risk_clw <- risk_clw[!dup,]
## risk_clw$cl_h_ew <- as.numeric(risk_clw$cl_h_ew)
## ncl_h <- risk_clw %>%
##   count(cl_h_ew) %>%
##   arrange(cl_h_ew) %>%
##   mutate(cumn = cumsum(n))
## phull <- NULL
## risk_clw <- arrange(risk_clw, cl_h_ew) # this is important since this is
## # the sorting assumed when collecting the edges!
## for (i in unique(risk_clw$cl_h_ew)) {
##   x <- risk_clw %>%
##     dplyr::filter(cl_h_ew == i)
##   ph <- cxhull(as.matrix(x[,1:6]))$edges
##   if (i > 1) {
##     ph <- ph + ncl_h$cumn[i-1]
##   }
##   ph <- cbind(ph, rep(i, nrow(ph)))
##   phull <- rbind(phull, ph)
## }
## phull <- as.data.frame(phull)
## colnames(phull) <- c("from", "to", "cl_h_ew")
## phull$cl_h_ew <- factor(phull$cl_h_ew)
## risk_clw$cl_h_ew <- factor(risk_clw$cl_h_ew)
## animate_groupxy(risk_clw[,1:6], col=risk_clw$cl_h_ew, pch=".",
##            edges=as.matrix(phull[,1:2]), edges.col=phull$cl_h_ew,
##            group_by=risk_clw$cl_h_ew)
## 
## # repeat for kmeans
## dup <- duplicated(r_km_d[,1:6])
## r_km_d <- r_km_d[!dup,]
## r_km_d$cl <- as.numeric(r_km_d$cl)
## ncl_h <- r_km_d %>%
##   count(cl) %>%
##   arrange(cl) %>%
##   mutate(cumn = cumsum(n))
## phull <- NULL
## r_km_d <- arrange(r_km_d, cl) # this is important since this is
## # the sorting assumed when collecting the edges!
## for (i in unique(r_km_d$cl)) {
##   x <- r_km_d %>%
##     dplyr::filter(cl == i)
##   ph <- cxhull(as.matrix(x[,1:6]))$edges
##   if (i > 1) {
##     ph <- ph + ncl_h$cumn[i-1]
##   }
##   ph <- cbind(ph, rep(i, nrow(ph)))
##   phull <- rbind(phull, ph)
## }
## phull <- as.data.frame(phull)
## colnames(phull) <- c("from", "to", "cl")
## phull$cl <- factor(phull$cl)
## r_km_d$cl <- factor(r_km_d$cl)
## animate_groupxy(r_km_d[,1:6], col=r_km_d$cl, pch=".",
##            edges=as.matrix(phull[,1:2]), edges.col=phull$cl,
##            group_by=r_km_d$cl)
## # comparing the chull makes it easier to see what part of the space is
## # occupied by each of the clusters
## 
## # SOM
## library(kohonen)
## library(aweSOM)
## set.seed(947)
## r_grid <- somgrid(xdim = 5, ydim = 5,
##                            topo = 'rectangular')
## r_init <- somInit(risk, 5, 5)
## r_som <- som(risk,
##              rlen=500,
##              grid = r_grid,
##              init = r_init)
## r_som_df_net <- som_model(r_som)
## r_som_map <- r_som_df_net$net %>%
##   mutate(type="net")
## r_som_data <- mutate(as.tibble(risk), type = "data")
## r_som_map_data <- bind_rows(r_som_map, r_som_data)
## r_som_map_data$type <- factor(r_som_map_data$type,
##   levels=c("net", "data"))
## animate_xy(r_som_map_data[,1:6],
##            edges=as.matrix(r_som_df_net$edges),
##            pch = 46,
##            edges.col = "black",
##            axes="bottomleft")
## # We wouldn't expect SOM to work well for this data because it's
## # pretty fully spread in the 6D. So the net is fairly tangled
## # filling out the space. Each node is where a cluster mean is
## # placed, so you would see the placements of these means are
## # fairly well spread - if the model fit optimisation has worked
## # properly. Probably the best use is to check that the model was fit
## # well, and the final results should look similar to k-means
## # because there is no benefit from having the net structure here.

