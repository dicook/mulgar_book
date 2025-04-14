## --------------------------------------------------------------------------
#| code-fold: false
X <- matrix(c(1.1, 1.3, 1.4, 1.2, 
              2.7, 2.6, 2.4, 2.5, 
              3.5, 3.4, 3.2, 3.6), 
            ncol=4, byrow=TRUE)
X


## --------------------------------------------------------------------------
#| code-fold: false
X[,2]


## --------------------------------------------------------------------------
#| code-fold: false
X[2,]


## --------------------------------------------------------------------------
#| code-fold: false
X[3,2]


## --------------------------------------------------------------------------
#| code-fold: false
A <- matrix(c(0.707,0.707,0,0,0,0,0.707,0.707),
            ncol=2, byrow=FALSE)
A


## --------------------------------------------------------------------------
#| code-fold: false
sum(A[,1]^2)
sum(A[,1]*A[,2])


## --------------------------------------------------------------------------
#| code-fold: false
X %*% A


## --------------------------------------------------------------------------
#| eval: false
#| echo: false
# # This code is to generate the gifs, but not the best to
# # start users on.
# library(tourr)
# library(mulgar)
# library(ggplot2)
# library(colorspace)
# data("penguins_sub")
# set.seed(1148)
# p_t_grand <- save_history(penguins_sub[, 1:4],
#                     max_bases = 50)
# animate_xy(penguins_sub[, 1:4],
#            planned_tour(p_t_grand),
#            col=penguins_sub$species
#            )
# render_gif(penguins_sub[, 1:4],
#            planned_tour(p_t_grand),
#            display_xy(col=penguins_sub$species),
#            gif_file = "gifs/penguins_cl_grand.gif",
#            frames=1000,
#            width=400,
#            height=400
#            )
# set.seed(1209)
# p_t_guided <- save_history(
#   penguins_sub[, 1:4],
#   guided_tour(lda_pp(penguins_sub$species)),
#   max_bases = 50)
# animate_xy(penguins_sub[, 1:4],
#            planned_tour(p_t_guided),
#            col=penguins_sub$species
#            )
# render_gif(penguins_sub[, 1:4],
#            planned_tour(p_t_guided),
#            display_xy(col=penguins_sub$species),
#            gif_file = "gifs/penguins_cl_guided.gif",
#            frames=1000,
#            width=400,
#            height=400
#            )
# best_proj <- matrix(p_t_guided[,,5], ncol=2)
# p_t_radial <- save_history(
#   penguins_sub[, 1:4],
#   radial_tour(best_proj, 3),
#   max_bases = 3)
# animate_xy(penguins_sub[, 1:4],
#            planned_tour(p_t_radial),
#            col=penguins_sub$species
#            )
# render_gif(penguins_sub[, 1:4],
#            planned_tour(p_t_radial),
#            display_xy(col=penguins_sub$species),
#            gif_file = "gifs/penguins_cl_radial.gif",
#            frames=1000,
#            width=400,
#            height=400
#            )


## --------------------------------------------------------------------------
#| eval: false
#| echo: false
# # This code is generates sequences of projections to demonstrate differences between methods.
# # Function to make plots
# plot_tour_projection <- function(d) {
#   plt <- ggplot() +
#   geom_path(data=d$circle, aes(x=c1, y=c2)) +
#   geom_segment(data=d$axes,
#                aes(x=x1, y=y1, xend=x2, yend=y2)) +
#   geom_text(data=d$axes, aes(x=x2, y=y2,
#                              label=rownames(d$axes))) +
#   geom_point(data=d$data_prj,
#              aes(x=P1, y=P2,
#                  colour=species)) +
#     xlim(-1,1) + ylim(-1, 1) +
#     scale_color_discrete_divergingx(palette="Zissou 1") +
#     theme_minimal() +
#     theme(aspect.ratio=1,
#        legend.position = "none",
#        axis.text=element_blank(),
#        axis.title=element_blank(),
#        axis.ticks=element_blank(),
#        panel.grid=element_blank(),
#        panel.background = element_rect(fill=NA,
#                                        colour="black"))
#   plt
# }
# # Grand tour
# prj <- matrix(p_t_grand[,,3], ncol=2)
# gr1 <- render_proj(penguins_sub[, 1:4], prj)
# gr1$data_prj$species <- penguins_sub$species
# gr1_plt <- plot_tour_projection(gr1)
# prj <- matrix(p_t_grand[,,4], ncol=2)
# gr2 <- render_proj(penguins_sub[, 1:4], prj)
# gr2$data_prj$species <- penguins_sub$species
# gr2_plt <- plot_tour_projection(gr2)
# prj <- matrix(p_t_grand[,,6], ncol=2)
# gr3 <- render_proj(penguins_sub[, 1:4], prj)
# gr3$data_prj$species <- penguins_sub$species
# gr3_plt <- plot_tour_projection(gr3)
# # Guided tour
# prj <- matrix(p_t_guided[,,1], ncol=2)
# gd1 <- render_proj(penguins_sub[, 1:4], prj)
# gd1$data_prj$species <- penguins_sub$species
# gd1_plt <- plot_tour_projection(gd1)
# prj <- matrix(p_t_guided[,,3], ncol=2)
# gd2 <- render_proj(penguins_sub[, 1:4], prj)
# gd2$data_prj$species <- penguins_sub$species
# gd2_plt <- plot_tour_projection(gd2)
# prj <- matrix(p_t_guided[,,5], ncol=2)
# gd3 <- render_proj(penguins_sub[, 1:4], prj)
# gd3$data_prj$species <- penguins_sub$species
# gd3_plt <- plot_tour_projection(gd3)
# # Radial tour
# prj <- matrix(p_t_radial[,,1], ncol=2)
# rd1 <- render_proj(penguins_sub[, 1:4], prj)
# rd1$data_prj$species <- penguins_sub$species
# rd1_plt <- plot_tour_projection(rd1)
# prj <- matrix(p_t_radial[,,2], ncol=2)
# rd2 <- render_proj(penguins_sub[, 1:4], prj)
# rd2$data_prj$species <- penguins_sub$species
# rd2_plt <- plot_tour_projection(rd2)
# prj <- matrix(p_t_radial[,,3], ncol=2)
# rd3 <- render_proj(penguins_sub[, 1:4], prj)
# rd3$data_prj$species <- penguins_sub$species
# rd3_plt <- plot_tour_projection(rd3)
# # Make plots and save to png
# library(gridExtra)
# grid.arrange(gr1_plt, gr2_plt, gr3_plt, ncol=3)
# grid.arrange(gd1_plt, gd2_plt, gd3_plt, ncol=3)
# grid.arrange(rd1_plt, rd2_plt, rd3_plt, ncol=3)
# 
# # This code will plot the path of index values
# index_vals <- path_index(interpolate(p_t_guided),
#   lda_pp(penguins_sub$species))
# guided_plot <- plot(index_vals) +
#   scale_x_continuous(breaks=c(1, 17, 37, 47, 57)) +
#   scale_y_continuous(breaks=index_vals[c(1, 17, 37, 47, 57)],
#                      labels=round(index_vals[c(1, 17, 37, 47, 57)], 3)) +
#   xlab("projection sequence") +
#   theme_minimal() +
#   theme(panel.background = element_rect(fill=NA,
#                                              colour="black"),
#         panel.grid.minor = element_blank())


## --------------------------------------------------------------------------
#| echo: false
#| eval: false
# library(tourr)
# animate_dist(flea[, 1:6],
#              method="histogram",
#              bw=0.1, scale_density=TRUE)
# data(places)
# places_01 <- apply(places[1:9,1:9], 2, function(x) (x-min(x))/(max(x)-min(x)))
# b <- matrix(rep(1/sqrt(9), 9), ncol=1)
# places_init <- cbind(places_01, idx = as.vector(as.matrix(places_01) %*% b))
# places_sorted <- places_init[order(places_init[,10]), 1:9]
# animate_idx(places_sorted, tour_path = local_tour(b, angle=pi/8),
#             label=as.character(places$stnum[1:9]),
#             label_x_pos = 0)


## --------------------------------------------------------------------------
#| echo: false
#| eval: false
# library(tourr)
# library(geozoo)
# set.seed(1351)
# d <- torus(3, n=4304)$points
# d <- apply(d, 2, function(x) (x-mean(x))/sd(x))
# d <- data.frame(d)
# slice_t2 <- save_history(d, grand_tour(), 20)
# animate_xy(d, planned_tour(slice_t2), axes="bottomleft", col="#EC5C00")
# animate_slice(d, planned_tour(slice_t2), axes="bottomleft", col="#EC5C00")
# 
# render_gif(d,
#            planned_tour(slice_t2),
#            display_xy(col="#EC5C00",
#              axes="bottomleft", half_range=3.5),
#            gif_file = "gifs/torus_proj.gif",
#            frames = 1000,
#            width = 400,
#            height = 400)
# 
# render_gif(d,
#            planned_tour(slice_t2),
#            display_slice(col="#EC5C00",
#              axes="bottomleft", half_range=3.5),
#            gif_file = "gifs/torus_slice.gif",
#            frames = 1000,
#            width = 400,
#            height = 400)


## --------------------------------------------------------------------------
#| echo: false
#| eval: false
# library(tourr)
# library(mulgar)
# data(aflw)
# aflw_labels <- ifelse(aflw$surname %in%
#                         c("Vescio", "Bowers", "Davey"), aflw$surname, "")
# clrs <- ifelse(aflw$surname %in%
#                         c("Vescio", "Bowers", "Davey"), "yes", "no")
# animate_xy(aflw[,8:11], obs_labels=aflw_labels, col=clrs, rescale=TRUE)
# aflw_t2 <- save_history(aflw[,8:11], max_bases=50)
# render_gif(aflw[,8:11],
#            planned_tour(aflw_t2),
#            display_xy(obs_labels=aflw_labels,
#                       col=clrs),
#            rescale=TRUE,
#            gif_file = "gifs/aflw_labelled.gif",
#            frames=1000,
#            width=400,
#            height=400)


## --------------------------------------------------------------------------
#| echo: false
#| eval: false
# library(compositions)
# library(geozoo)
# library(dplyr)
# data("Firework")
# # Five components
# # Check
# apply(Firework[,1:5], 1, sum)
# clr <- factor(ifelse(Firework[,6]>17, "high", "low"))
# animate_xy(Firework[,1:5], col=clr)
# 
# # Project
# proj <- t(geozoo::f_helmert(5)[-1,])
# fireworks_p <- as.matrix(Firework[,1:5]) %*% proj
# colnames(fireworks_p) <- c("x1", "x2", "x3", "x4")
# fireworks_p <- as.data.frame(fireworks_p)
# animate_xy(fireworks_p)
# 
# simp <- geozoo::simplex(p=4)
# sp <- data.frame(simp$points)
# colnames(sp) <- c("x1", "x2", "x3", "x4")
# fireworks_p <- bind_rows(sp, fireworks_p)
# labels <- c("a" , "b", "c", "d", "e",
#                 rep("", 81))
# 
# animate_xy(fireworks_p,
#            col="#EC5C00",
#            axes = "off",
#            half_range = 0.8,
#            edges = as.matrix(simp$edges),
#            obs_labels = labels)
# 
# clrs <- c(rep("black", 5), rep("#EC5C00", 81))
# ptsize <- c(rep(0.1, 5), rep(3, 81))
# fireworks_t2 <- save_history(fireworks_p, max_bases=50)
# render_gif(fireworks_p,
#            planned_tour(fireworks_t2),
#            display_xy(
#              col = clrs,
#              cex = ptsize,
#              axes = "off",
#              half_range = 1,
#              edges = as.matrix(simp$edges),
#              edges.width = 0.1,
#              obs_labels = labels,
#              center = TRUE),
#            gif_file = "gifs/fireworks.gif",
#            frames=1000,
#            width=400,
#            height=400)
# 


## --------------------------------------------------------------------------
#| eval: false
#| echo: true
# rs <- geozoo::roman.surface()$points |>
#   scale() |>
#   as.data.frame()


## --------------------------------------------------------------------------
#| eval: false
#| echo: true
# s_solid <- geozoo::sphere.solid.random(4, 2000)$points |>
#   as.data.frame()


## --------------------------------------------------------------------------
#| eval: false
#| echo: true
# s_hollow <- geozoo::sphere.hollow(4, 2000)$points |>
#   as.data.frame()


## --------------------------------------------------------------------------
#| eval: false
#| echo: false
# library(tourr)
# library(mulgar)
# library(ggplot2)
# library(geozoo)
# # Answer to Q1
# set.seed(110)
# m <- matrix(rnorm(10), ncol = 2)
# m[3, 1]
# 
# # Answer to Q2
# is_orthonormal(m)
# m <- orthonormalise(m)
# is_orthonormal(m)
# sum(m[,1]^2) # = 1 if properly normalised
# sum(m[,2]^2) # = 1 if properly normalised
# sum(m[,1]*m[,2]) # = 0 if orthogonal
# m
# # with this seed components
# # 2 and 4 contribute most horizontally
# # 3 and 4 contribute most vertically
# 
# # Answer to Q3
# clusters_proj <- as.matrix(clusters[,1:5]) %*% m # calculate projection
# colnames(clusters_proj) <- c("p1", "p2")
# clusters_proj <- as.data.frame(clusters_proj)
# ggplot(clusters_proj, aes(p1, p2, color = clusters$cl)) +
#   geom_point() +
#   theme(aspect.ratio=1)
# # with this projection we can see a difference in all three clusters,
# # although there is some overlap between A and B
# 
# # Answer to Q4
# set.seed(1044)
# cl_t2 <- save_history(clusters[,1:5], max_bases=3)
# cl_t2[,,2]
# clusters_proj <- as.matrix(clusters[,1:5]) %*% matrix(cl_t2[,,2], ncol=2)
# colnames(clusters_proj) <- c("p1", "p2")
# clusters_proj <- as.data.frame(clusters_proj)
# ggplot(clusters_proj, aes(p1, p2, color = clusters$cl)) +
#   geom_point() +
#   theme(aspect.ratio=1)
# 
# # Answer to Q4
# cl_t2_i <- interpolate(cl_t2)
# dim(cl_t2_i)[3]
# clusters_proj <- as.matrix(clusters[,1:5]) %*% matrix(cl_t2_i[,,1], ncol=2)
# colnames(clusters_proj) <- c("p1", "p2")
# clusters_proj <- as.data.frame(clusters_proj)
# p1 <- ggplot(clusters_proj, aes(p1, p2, color = clusters$cl)) +
#   geom_point() +
#   theme(aspect.ratio=1,
#         legend.position="none")
# clusters_proj <- as.matrix(clusters[,1:5]) %*% matrix(cl_t2_i[,,2], ncol=2)
# colnames(clusters_proj) <- c("p1", "p2")
# clusters_proj <- as.data.frame(clusters_proj)
# p2 <- ggplot(clusters_proj, aes(p1, p2, color = clusters$cl)) +
#   geom_point() +
#   theme(aspect.ratio=1,
#         legend.position="none")
# clusters_proj <- as.matrix(clusters[,1:5]) %*% matrix(cl_t2_i[,,3], ncol=2)
# colnames(clusters_proj) <- c("p1", "p2")
# clusters_proj <- as.data.frame(clusters_proj)
# p3 <- ggplot(clusters_proj, aes(p1, p2, color = clusters$cl)) +
#   geom_point() +
#   theme(aspect.ratio=1,
#         legend.position="none")
# clusters_proj <- as.matrix(clusters[,1:5]) %*% matrix(cl_t2_i[,,4], ncol=2)
# colnames(clusters_proj) <- c("p1", "p2")
# clusters_proj <- as.data.frame(clusters_proj)
# p4 <- ggplot(clusters_proj, aes(p1, p2, color = clusters$cl)) +
#   geom_point() +
#   theme(aspect.ratio=1,
#         legend.position="none")
# library(patchwork)
# p1 + p2 + p3 + p4 + plot_layout(ncol=4)
# # They look ALMOST identical. These are very small interpolation steps, and the data changes very little between each one.
# 
# # Answer to Q6
# cl_t2_guided <- save_history(clusters[,1:5],
#                              tour_path = guided_tour(holes()))
# cl_t2_guided[,,14]
# clusters_proj <- as.matrix(clusters[,1:5]) %*% matrix(cl_t2_guided[,,14], ncol=2)
# colnames(clusters_proj) <- c("p1", "p2")
# clusters_proj <- as.data.frame(clusters_proj)
# ggplot(clusters_proj, aes(p1, p2, color = clusters$cl)) +
#   geom_point() +
#   theme(aspect.ratio=1)
# # This view shows three clusters, with some overlap between A and B.
# #
# 
# # Answer to Q7
# cl_t2_r <- save_history(clusters[,1:5],
#      tour_path = radial_tour(start=matrix(cl_t2_guided[,,14], ncol=2),
#                              mvar=1), max_bases=3)
# # Only three bases are needed, because it runs from starting projection
# # to a target where the chosen variable as coefficient equal to 0
# # The first and third are the same because it goes back to where it started
# # The second basis has the coefficients for variable 1 equal to zero
# 
# # Answer to Q8
# set.seed(1215)
# animate_xy(clusters[,1:5], col=clusters$cl)
# animate_xy(clusters[,1:5], tour_path=guided_tour(holes()), col=clusters$cl)
# animate_xy(clusters[,1:5],
#   radial_tour(start=matrix(cl_t2_guided[,,14], ncol=2), mvar=1),
#               col=clusters$cl)
# # x1 can be removed, and the clusters are still completely visible
# # x5 can be removed, and the clusters are still completely visible
# # When x4 is removed cluster C is not separated from the others
# 
# # Answer to Q9
# set.seed(1218)
# animate_dist(clusters[,1:5])
# animate_dist(clusters[,1:5], tour_path=guided_tour(holes()))
# animate_dist(clusters[,1:5], method="histogram", bw=0.1)
# # The three clusters cannot be seen so easily. Many projections have bimodal
# # density, so it is clear that there are two clusters, but detecting the
# # third is not possible.
# 
# # Answer to Q10
# rs <- geozoo::roman.surface()$points |> scale() |> as.data.frame()
# animate_xy(rs)
# animate_slice(rs)
# # The twisted hollow shape of the Roman Surface can be seen better with the slice tour
# s_solid <- geozoo::sphere.solid.random(4, 2000)$points |> as.data.frame()
# animate_xy(s_solid)
# animate_slice(s_solid)
# 
# s_hollow <- geozoo::sphere.hollow(4, 2000)$points |> as.data.frame()
# animate_xy(s_hollow)
# animate_slice(s_hollow)
# # The hollow sphere has a crisper edge than the solid sphere, and slices show the interior is empty, has no points, because the slices are all circles.

