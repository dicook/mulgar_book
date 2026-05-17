## ----echo=knitr::is_html_output()--------------------------------------------
#| code-summary: "Load libraries"
source("code/setup.R")


## ----echo=knitr::is_html_output()--------------------------------------------
#| message: false
#| code-summary: "Code to do clustering"
load("data/penguins_sub.rda")
p_dist <- dist(penguins_sub[,1:2])
p_hcw <- hclust(p_dist, method="ward.D2")
p_cl <- data.frame(cl_w = cutree(p_hcw, 3))


## ----echo=knitr::is_html_output()--------------------------------------------
#| code-summary: "Code for convex hulls in 2D"
psub <- penguins_sub |>
  select(bl, bd) |>
  mutate(cl = p_cl$cl_w)
psub <- psub[!duplicated(psub),]

phull <- gen_chull(psub[,1:2], psub$cl)
phull_segs <- data.frame(x = phull$data$bl[phull$edges[,1]],
                         y = phull$data$bd[phull$edges[,1]],
                         xend = phull$data$bl[phull$edges[,2]],
                         yend = phull$data$bd[phull$edges[,2]],
                         cl = phull$edge_clr)
p_chull2D <- ggplot() +
  geom_point(data=phull$data, aes(x=bl, y=bd, 
                            colour=cl)) + 
  geom_segment(data=phull_segs, aes(x=x, xend=xend,
                                    y=y, yend=yend,
                                    colour=cl)) +
  scale_colour_discrete_divergingx(palette = "Zissou 1") +
  theme_minimal() +
  theme(aspect.ratio = 1)


## ----echo=knitr::is_html_output()--------------------------------------------
#| message: false
#| code-summary: "Code to do clustering"
p_dist <- dist(penguins_sub[,1:4])
p_hcw <- hclust(p_dist, method="ward.D2")

p_cl <- data.frame(cl_w = cutree(p_hcw, 3))

penguins_mc <- Mclust(penguins_sub[,1:4], 
                      G=3, 
                      modelNames = "EEE")
p_cl <- p_cl |> 
  mutate(cl_mc = penguins_mc$classification)


## ----------------------------------------------------------------------------
#| code-summary: "Code to generate pD convex hull"
phull <- gen_chull(penguins_sub[,1:4], p_cl$cl_w)


## ----echo=knitr::is_html_output()--------------------------------------------
#| eval: false
#| code-summary: "Code to generate pD convex hull and view in tour"
# animate_xy(phull$data[,1:4],
#            col=phull$data$cl,
#            edges=phull$edges,
#            edges.col=phull$edge_clr)
# render_gif(phull$data[,1:4],
#            tour_path = grand_tour(),
#            display = display_xy(col=phull$data$cl,
#                                 edges=phull$edges,
#                                 edges.col=phull$edge_clr),
#            gif_file = "gifs/penguins_chull.gif",
#            frames = 500,
#            width = 400,
#            height = 400)


## ----------------------------------------------------------------------------
#| eval: false
#| echo: false

# # This code is checking that convex hull works
# cb <- cube.solid.random(p=4, n=1000)$points
# phull <- cxhull(cb)$edges
# 
# animate_xy(cb, edges=phull)
# 
# smp <- simplex(p=4)$points
# smp_phull <- cxhull(smp)$edges
# animate_xy(smp, edges=smp_phull)
# 
# sph <- rbind(sphere.solid.random(p=3)$points,
#              sphere.hollow(p=3)$points)
# sph_phull <- cxhull(sph)$edges
# animate_xy(as.data.frame(sph), edges=sph_phull)


## ----------------------------------------------------------------------------
#| label: fig-penguin-hull-2D-html
#| echo: false
#| fig-width: 4
#| fig-height: 4
#| fig-cap: "2D"
#| fig-alt: "Scatterplot with x axis labelled 'bl' and grid lines at -2, -1, 0, 1, 2, 3, and y axis labelled 'bd' and grid lines at -2, -1, 0, 1, 2. Points are coloured blue, yellow and red matching clusters 1, 2, 3, as indicated by the legend at top right. Lines connect points marking the convex hull of each cluster. Blue group is slightly rectangular at top left. Yellow group is wedge-shaped at bottom right, and red group is like a rounded square at top right. All convex hulls overlap in the middle of the plot."
p_chull2D 


## ----echo=knitr::is_html_output()--------------------------------------------
#| code-summary: "Code to make confusion table"
p_cl |> 
  count(cl_w, cl_mc) |> 
  pivot_wider(names_from = cl_mc, 
              values_from = n, 
              values_fill = 0) |>
  gt() |>
  tab_spanner(label = "cl_mc", columns=c(`2`, `3`, `1`)) |>
  cols_width(everything() ~ px(60))


## ----------------------------------------------------------------------------
#| eval: false
#| message: false
#| echo: false
#| code-summary: "Code to do linked brushing with liminal"
# penguins_cl <- bind_cols(penguins_sub, p_cl)
# p_cl <- p_cl |>
#   mutate(cl_w_j = jitter(cl_w),
#          cl_mc_j = jitter(cl_mc))
# limn_tour_link(
#   p_cl[,3:4],
#   penguins_cl,
#   cols = bl:bm,
#   color = cl_w
# )


## ----------------------------------------------------------------------------
#| eval: false
#| echo: true
#| code-summary: "Code to do interactive graphics"
# p_cl <- p_cl |>
#   mutate(cl_w_j = jitter(cl_w),
#          cl_mc_j = jitter(cl_mc))
# penguins_cl <- bind_cols(penguins_sub, p_cl)
# p_cl_shared <- SharedData$new(penguins_cl)
# 
# set.seed(1046)
# detour_plot <- detour(p_cl_shared, tour_aes(
#   projection = bl:bm,
#   colour = cl_mc)) |>
#     tour_path(grand_tour(2),
#                     max_bases=100, fps = 60) |>
#        show_scatter(alpha = 0.7, axes = FALSE,
#                     width = "100%", height = "450px")
# 
# conf_mat <- plot_ly(p_cl_shared,
#                     x = ~cl_mc_j,
#                     y = ~cl_w_j,
#                     color = ~cl_mc,
#                     colors = viridis_pal(option = "D")(3),
#                     height = 450) |>
#   highlight(on = "plotly_selected",
#               off = "plotly_doubleclick") |>
#     add_trace(type = "scatter",
#               mode = "markers")
# 
# bscols(
#      detour_plot, conf_mat,
#      widths = c(5, 6)
#  )


## ----------------------------------------------------------------------------
#| eval: false
#| echo: false
# animate_xy(c4[,c(1:4, 6)])
# c4_hc <- hclust(dist(c4[,c(1:4, 6)]), method="ward.D2")
# plot(c4_hc)
# cl <- cutree(c4_hc, 5)
# c4_cl <- c4 |>
#   select(x1:x4, x6) |>
#   mutate(cl = factor(cl))
# animate_xy(c4_cl[,1:5], col=c4_cl$cl)
# c5_chull <- gen_chull(c4_cl[,1:5], as.numeric(c4_cl$cl))
# animate_xy(c5_chull$data[,1:5],
#            col=c5_chull$data$cl,
#            edges=c5_chull$edges,
#            edges.col=c5_chull$edge_clr)
# 


## ----------------------------------------------------------------------------
#| eval: false
#| echo: false
#| message: false
#| warning: false
# risk <- readRDS("data/risk_MSA.rds")
# # looking at the data
# animate_xy(risk)
# # hierarchical clustering solutions
# risk_h_mc <- hclust(dist(risk, method = "manhattan"),
#                   method = "complete")
# risk_h_ms <- hclust(dist(risk, method = "manhattan"),
#                   method = "single")
# risk_h_ew <- hclust(dist(risk, method = "euclidean"),
#                   method = "ward.D2")
# # adding the clustering information to the data
# risk_clw <- risk |>
#   as_tibble() |>
#   mutate(cl_h_mc = factor(cutree(risk_h_mc, 6))) |>
#   mutate(cl_h_ms = factor(cutree(risk_h_ms, 6))) |>
#   mutate(cl_h_ew = factor(cutree(risk_h_ew, 6)))
# # drawing 2D dendrograms
# ggplot() +
#   geom_segment(data=dendro_data(risk_h_mc)$segments,
#                aes(x = x, y = y,
#                    xend = xend, yend = yend)) +
#   theme_dendro()
# ggplot() +
#   geom_segment(data=dendro_data(risk_h_ms)$segments,
#                aes(x = x, y = y,
#                    xend = xend, yend = yend)) +
#   theme_dendro()
# ggplot() +
#   geom_segment(data=dendro_data(risk_h_ew)$segments,
#                aes(x = x, y = y,
#                    xend = xend, yend = yend)) +
#   theme_dendro()
# # dendrogram in the data space
# risk_hfly_mc <- hierfly(risk_clw, risk_h_mc, scale=FALSE)
# risk_hfly_ms <- hierfly(risk_clw, risk_h_ms, scale=FALSE)
# risk_hfly_ew <- hierfly(risk_clw, risk_h_ew, scale=FALSE)
# glyphs <- c(16, 46)
# pchw_mc <- glyphs[risk_hfly_mc$data$node+1]
# pchw_ms <- glyphs[risk_hfly_ms$data$node+1]
# pchw_ew <- glyphs[risk_hfly_ew$data$node+1]
# # manhattan + complete
# # we can see hierarchical structure, small groups at the
# # edges that get connected first and then combined into
# # larger cluster, looks like some of the clusters are
# # really spread out across the data space
# animate_xy(risk_hfly_mc$data[,1:6],
#            col=risk_clw$cl_h_mc,
#            tour_path = grand_tour(),
#            pch = pchw_mc,
#            edges=risk_hfly_mc$edges,
#            axes="bottomleft",
#            rescale=FALSE)
# # Manhattan + single
# # pretty much all the edges point in towards the center!
# animate_xy(risk_hfly_ms$data[,1:6],
#            col=risk_clw$cl_h_ms,
#            tour_path = grand_tour(),
#            pch = pchw_ms,
#            edges=risk_hfly_ms$edges,
#            axes="bottomleft",
#            rescale=FALSE)
# # euclidean + ward
# # at this stage looks mostly similar to mc case
# animate_xy(risk_hfly_ew$data[,1:6],
#            col=risk_clw$cl_h_ew,
#            tour_path = grand_tour(),
#            pch = pchw_ew,
#            edges=risk_hfly_ew$edges,
#            axes="bottomleft",
#            rescale=FALSE)
# 
# # comparison of two solutions
# # confusion table shows quite some disagreements
# risk_clw |>
#   count(cl_h_mc, cl_h_ew) |>
#   pivot_wider(names_from = cl_h_mc,
#               values_from = n,
#               values_fill = 0)
# # explore with liminal
# liminal::limn_tour_link(
#   tibble::as.tibble(cbind(jitter(as.numeric(risk_clw$cl_h_ew)),
#                           jitter(as.numeric(risk_clw$cl_h_mc)))),
#   risk_clw,
#   cols = 1:6,
#   color = cl_h_ew)
# # for example we can see cluster 2 for mc solution is very large,
# # in the ew solution this is split up into 3 larger clusters,
# # and in the tour we see how the smaller clusters 5 and 6 are different
# # and less compact than the largest cluster 2 for the ew solution
# 
# # k-means + tour with cluster means
# r_km <- kmeans(risk, centers=6,
#                      iter.max = 500, nstart = 5)
# r_km_means <- data.frame(r_km$centers) |>
#   mutate(cl = factor(rownames(r_km$centers)))
# r_km_d <- as.tibble(risk) |>
#   mutate(cl = factor(r_km$cluster))
# r_km_means <- r_km_means |>
#   mutate(type = "mean")
# r_km_d <- r_km_d |>
#   mutate(type = "data")
# r_km_all <- bind_rows(r_km_means, r_km_d)
# r_km_all$type <- factor(r_km_all$type, levels=c("mean", "data"))
# r_pch <- c(3, 46)[as.numeric(r_km_all$type)]
# r_cex <- c(3, 1)[as.numeric(r_km_all$type)]
# animate_xy(r_km_all[,1:6], col=r_km_all$cl,
#            pch=r_pch, cex=r_cex, axes="bottomleft")
# 
# # guided tour + interpretation in terms of variables
# set.seed(543)
# animate_xy(r_km_all[,1:6],
#            tour_path = guided_tour(lda_pp(r_km_all$cl)) ,
#            col=r_km_all$cl,
#            pch=r_pch, cex=r_cex, axes="bottomleft")
# # from this solution it seems there is an overall
# # risk behavior captured in the clustering, i.e. most
# # variable contribute to separating the clusters along the
# # x direction
# # health risks seem to be a bit different, with one cluster
# # containing average risk scores apart from high risk
# # scores for the health variable
# 
# # comparison k-means vs hierarchical clustering solution with liminal
# # we can see that the k-means grouping is very different
# liminal::limn_tour_link(
#   tibble::as.tibble(cbind(jitter(as.numeric(r_km_d$cl)),
#                           jitter(as.numeric(risk_clw$cl_h_mc)))),
#   r_km_d,
#   cols = 1:6,
#   color = cl)
# 
# # comparison k-means vs hierarchical clustering solution
# # using convex hull in the data space
# # need to do one at a time since there is too much going on
# # starting with hierarchical clustering solution
# library(cxhull)
# dup <- duplicated(risk_clw[,1:6])
# risk_clw <- risk_clw[!dup,]
# risk_clw$cl_h_ew <- as.numeric(risk_clw$cl_h_ew)
# ncl_h <- risk_clw |>
#   count(cl_h_ew) |>
#   arrange(cl_h_ew) |>
#   mutate(cumn = cumsum(n))
# phull <- NULL
# risk_clw <- arrange(risk_clw, cl_h_ew) # this is important since this is
# # the sorting assumed when collecting the edges!
# for (i in unique(risk_clw$cl_h_ew)) {
#   x <- risk_clw |>
#     dplyr::filter(cl_h_ew == i)
#   ph <- cxhull(as.matrix(x[,1:6]))$edges
#   if (i > 1) {
#     ph <- ph + ncl_h$cumn[i-1]
#   }
#   ph <- cbind(ph, rep(i, nrow(ph)))
#   phull <- rbind(phull, ph)
# }
# phull <- as.data.frame(phull)
# colnames(phull) <- c("from", "to", "cl_h_ew")
# phull$cl_h_ew <- factor(phull$cl_h_ew)
# risk_clw$cl_h_ew <- factor(risk_clw$cl_h_ew)
# animate_groupxy(risk_clw[,1:6], col=risk_clw$cl_h_ew, pch=".",
#            edges=as.matrix(phull[,1:2]), edges.col=phull$cl_h_ew,
#            group_by=risk_clw$cl_h_ew)
# 
# # repeat for kmeans
# dup <- duplicated(r_km_d[,1:6])
# r_km_d <- r_km_d[!dup,]
# r_km_d$cl <- as.numeric(r_km_d$cl)
# ncl_h <- r_km_d |>
#   count(cl) |>
#   arrange(cl) |>
#   mutate(cumn = cumsum(n))
# phull <- NULL
# r_km_d <- arrange(r_km_d, cl) # this is important since this is
# # the sorting assumed when collecting the edges!
# for (i in unique(r_km_d$cl)) {
#   x <- r_km_d |>
#     dplyr::filter(cl == i)
#   ph <- cxhull(as.matrix(x[,1:6]))$edges
#   if (i > 1) {
#     ph <- ph + ncl_h$cumn[i-1]
#   }
#   ph <- cbind(ph, rep(i, nrow(ph)))
#   phull <- rbind(phull, ph)
# }
# phull <- as.data.frame(phull)
# colnames(phull) <- c("from", "to", "cl")
# phull$cl <- factor(phull$cl)
# r_km_d$cl <- factor(r_km_d$cl)
# animate_groupxy(r_km_d[,1:6], col=r_km_d$cl, pch=".",
#            edges=as.matrix(phull[,1:2]), edges.col=phull$cl,
#            group_by=r_km_d$cl)
# # comparing the chull makes it easier to see what part of the space is
# # occupied by each of the clusters
# 
# # SOM
# library(kohonen)
# library(aweSOM)
# set.seed(947)
# r_grid <- somgrid(xdim = 5, ydim = 5,
#                            topo = 'rectangular')
# r_init <- somInit(risk, 5, 5)
# r_som <- som(risk,
#              rlen=500,
#              grid = r_grid,
#              init = r_init)
# r_som_df_net <- som_model(r_som)
# r_som_map <- r_som_df_net$net |>
#   mutate(type="net")
# r_som_data <- mutate(as.tibble(risk), type = "data")
# r_som_map_data <- bind_rows(r_som_map, r_som_data)
# r_som_map_data$type <- factor(r_som_map_data$type,
#   levels=c("net", "data"))
# animate_xy(r_som_map_data[,1:6],
#            edges=as.matrix(r_som_df_net$edges),
#            pch = 46,
#            edges.col = "black",
#            axes="bottomleft")
# # We wouldn't expect SOM to work well for this data because it's
# # pretty fully spread in the 6D. So the net is fairly tangled
# # filling out the space. Each node is where a cluster mean is
# # placed, so you would see the placements of these means are
# # fairly well spread - if the model fit optimisation has worked
# # properly. Probably the best use is to check that the model was fit
# # well, and the final results should look similar to k-means
# # because there is no benefit from having the net structure here.

