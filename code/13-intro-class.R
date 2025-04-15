## --------------------------------------------------------------------------
#| label: fig-sup-example
#| fig-cap: "Examples of supervised classification patterns: (a) linearly separable, (b) linear but not completely separable, (c) non-linearly separable, (d) non-linear, but not completely separable."
#| fig-alt: "A set of four scatter plots labeled (a), (b), (c), and (d), each showing two groups of data points in red and blue. The distribution of points varies across the plots. In (a), the red and blue points are relatively well separated along a diagonal trend. In (b), the separation is less clear, with more overlap between the groups. In (c) and (d), the points follow a zig-zag pattern, separated in (c) but overlapping in (d)."
#| echo: false
#| message: false
#| fig-height: 2.5
#| fig-width: 7.5
#| out-width: 100%
library(ggplot2)
library(dplyr)
library(colorspace)
library(patchwork)
set.seed(524)
x1 <- runif(176) + 0.5
x1[1:61] <- x1[1:61] - 1.2
x2 <- 1 + 2*x1 + rnorm(176)
x2[1:61] <- 2 - 3*x1[1:61] + rnorm(61)
x3 <- runif(176) + 0.5
x3[1:61] <- x3[1:61] - 0.5
x4 <- 0.25 - x3 + rnorm(176)
x4[1:61] <- -0.25 + 3*x3[1:61] + rnorm(61)
cl <- factor(c(rep("A", 61), rep("B", 176-61)))
df <- data.frame(x1, x2, x3, x4, cl)
class1 <- ggplot(df, aes(x=x1, y=x2, colour = cl)) +
  geom_point(alpha=0.7) +
  scale_colour_discrete_divergingx(
      palette = "Zissou 1", nmax = 2, rev = TRUE) +
  annotate("text", -0.65, 6.6, label="a") +
  theme(aspect.ratio=1,
        legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect("white"),
        panel.border = element_rect("black", fill=NA, 
             linewidth = 0.5)) 
class2 <- ggplot(df, aes(x=x3, y=x4, colour = cl)) +
  geom_point(alpha=0.7) +
  scale_colour_discrete_divergingx(
      palette = "Zissou 1", nmax = 2, rev = TRUE) +
  annotate("text", 0.05, 4.1, label="b") +
  theme(aspect.ratio=1,
        legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect("white"),
        panel.border = element_rect("black", fill=NA, 
             linewidth = 0.5))
set.seed(826)
x5 <- 2*(runif(176) - 0.5)
x6 <- case_when(x5 < -0.4 ~ -1.2 - 3 * x5,
                x5 > 0.2 ~ 2.4 - 3 * x5,
                .default = 1.2 + 3 * x5)
x5 <- 2*x5
x6 <- x6 + rnorm(176) * 0.25
x6[1:83] <- x6[1:83] - 1.5
x7 <- 2*(runif(176) - 0.5)
x8 <- case_when(x7 < -0.4 ~ -1.2 - 3 * x7,
                x7 > 0.2 ~ 2.4 - 3 * x7,
                .default = 1.2 + 3 * x7)
x7 <- 2*x7
x8[x7 < -0.1] <- x8[x7 < -0.1] + rnorm(length(x8[x7 < -0.1])) * 0.25
x8[x7 >= -0.1] <- x8[x7 >= -0.1] + rnorm(length(x8[x7 >= -0.1])) * 0.5
x8[1:83] <- x8[1:83] - 1.5
cl2 <- factor(c(rep("A", 83), rep("B", 176-83)))
df2 <- data.frame(x5, x6, x7, x8, cl2)
class3 <- ggplot(df2, aes(x=x5, y=x6, colour = cl2)) +
  geom_point(alpha=0.7) +
  scale_colour_discrete_divergingx(
      palette = "Zissou 1", nmax = 2, rev = TRUE) +
  annotate("text", 1.92, 2.07, label="c") +
  theme(aspect.ratio=1,
        legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect("white"),
        panel.border = element_rect("black", fill=NA, 
             linewidth = 0.5))
class4 <- ggplot(df2, aes(x=x7, y=x8, colour = cl2)) +
  geom_point(alpha=0.7) +
  scale_colour_discrete_divergingx(
      palette = "Zissou 1", nmax = 2, rev = TRUE) +
  annotate("text", 1.95, 1.9, label="d") +
  theme(aspect.ratio=1,
        legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect("white"),
        panel.border = element_rect("black", fill=NA, 
             linewidth = 0.5))
print(class1 + class2 + class3 + class4 + plot_layout(ncol=4))


## --------------------------------------------------------------------------
#| label: fig-bias-variance
#| fig-cap: "Illustrating bias and variance using three different samples (black points) from the same distribution (in the columns) and two different model fits (rows). Colour indicates predictions from the resulting fitted model. The model used in a-c does not capture the zig-zag clusters, but is virtually the same regardless of training sample: it has high bias but low variance. The model used in d-f captures the zig-zag reasonably, but the boundary differs substantially between training samples: it has low bias but high variance."
#| fig-alt: "A set of six square plots labeled (a) to (f), arranged in a 2x3 grid. Data points are overlaid as black crosses and circles. Each group follows a zig-zag pattern with crosses at top and separated from circles. The colours, red and blue, display a binary classification boundary, which is linear in (a) to (c) and different step function shapes in (d) to (f). "
#| echo: false
#| message: false
#| fig-height: 4
#| fig-width: 6
#| out-width: 100%
library(MASS)
library(rpart)
library(classifly)
n <- 486
set.seed(826)
x1 <- 2*(runif(n) - 0.5)
x2 <- case_when(x1 < -0.4 ~ -1.2 - 3 * x1,
                x1 > 0.2 ~ 2.4 - 3 * x1,
                .default = 1.2 + 3 * x1)
x1 <- 2*x1
x2 <- x2 + rnorm(n) * 0.25
x2[1:(n/2)] <- x2[1:(n/2)] - 1.5
cl <- factor(c(rep("A", n/2), rep("B", n-n/2)))
df3 <- data.frame(x1, x2, cl)
df_lda <- lda(cl~x1+x2, data=df3)
df_lda_bnd <- explore(df_lda, df3)
bv1 <- ggplot() + 
  geom_point(data=filter(df_lda_bnd, 
                         .TYPE == "simulated"), 
       aes(x=x1, y=x2, colour=cl), alpha=0.6) +
  scale_colour_discrete_divergingx(palette = "Zissou 1") +
  geom_point(data=df3, aes(x=x1, y=x2, shape=cl),
             colour="black", alpha=0.5) +
  scale_shape_manual(values = c(1,4)) +
  annotate("text", 1.8, 1.9, label="a") +
  theme_minimal() +
  theme(aspect.ratio = 1,
        legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank())

df_tree <- rpart(cl~x1+x2, data=df3, 
   control = rpart.control(minsplit = 5, cp = 0.00001))
df_tree_bnd <- explore(df_tree, df3)
bv2 <- ggplot() + 
  geom_point(data=filter(df_tree_bnd, .TYPE == "simulated"), 
       aes(x=x1, y=x2, colour=cl), alpha=0.6) +
  scale_colour_discrete_divergingx(palette = "Zissou 1") +
  geom_point(data=df3, aes(x=x1, y=x2, shape=cl),
             colour="black", alpha=0.5) +
  scale_shape_manual(values = c(1,4)) +
  annotate("text", 1.8, 1.9, label="d") +
  theme_minimal() +
  theme(aspect.ratio = 1,
        legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank())

set.seed(104)
x1 <- 2*(runif(n) - 0.5)
x2 <- case_when(x1 < -0.4 ~ -1.2 - 3 * x1,
                x1 > 0.2 ~ 2.4 - 3 * x1,
                .default = 1.2 + 3 * x1)
x1 <- 2*x1
x2 <- x2 + rnorm(n) * 0.25
x2[1:(n/2)] <- x2[1:(n/2)] - 1.5
cl <- factor(c(rep("A", n/2), rep("B", n-n/2)))
df3 <- data.frame(x1, x2, cl)
df_lda <- lda(cl~x1+x2, data=df3)
df_lda_bnd <- explore(df_lda, df3)
bv3 <- ggplot() + 
  geom_point(data=filter(df_lda_bnd, 
                         .TYPE == "simulated"), 
       aes(x=x1, y=x2, colour=cl), alpha=0.6) +
  scale_colour_discrete_divergingx(palette = "Zissou 1") +
  geom_point(data=df3, aes(x=x1, y=x2, shape=cl),
             colour="black", alpha=0.5) +
  scale_shape_manual(values = c(1,4)) +
  annotate("text", 1.8, 1.9, label="b") +
  theme_minimal() +
  theme(aspect.ratio = 1,
        legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank())

df_tree <- rpart(cl~x1+x2, data=df3, 
   control = rpart.control(minsplit = 5, cp = 0.00001))
df_tree_bnd <- explore(df_tree, df3)
bv4 <- ggplot() + 
  geom_point(data=filter(df_tree_bnd, .TYPE == "simulated"), 
       aes(x=x1, y=x2, colour=cl), alpha=0.6) +
  scale_colour_discrete_divergingx(palette = "Zissou 1") +
  geom_point(data=df3, aes(x=x1, y=x2, shape=cl),
             colour="black", alpha=0.5) +
  scale_shape_manual(values = c(1,4)) +
  annotate("text", 1.8, 1.9, label="e") +
  theme_minimal() +
  theme(aspect.ratio = 1,
        legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank())

set.seed(601)
x1 <- 2*(runif(n) - 0.5)
x2 <- case_when(x1 < -0.4 ~ -1.2 - 3 * x1,
                x1 > 0.2 ~ 2.4 - 3 * x1,
                .default = 1.2 + 3 * x1)
x1 <- 2*x1
x2 <- x2 + rnorm(n) * 0.25
x2[1:(n/2)] <- x2[1:(n/2)] - 1.5
cl <- factor(c(rep("A", n/2), rep("B", n-n/2)))
df3 <- data.frame(x1, x2, cl)
df_lda <- lda(cl~x1+x2, data=df3)
df_lda_bnd <- explore(df_lda, df3)
bv5 <- ggplot() + 
  geom_point(data=filter(df_lda_bnd, 
                         .TYPE == "simulated"), 
       aes(x=x1, y=x2, colour=cl), alpha=0.6) +
  scale_colour_discrete_divergingx(palette = "Zissou 1") +
  geom_point(data=df3, aes(x=x1, y=x2, shape=cl),
             colour="black", alpha=0.5) +
  scale_shape_manual(values = c(1,4)) +
  annotate("text", 1.8, 1.9, label="c") +
  theme_minimal() +
  theme(aspect.ratio = 1,
        legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank())

df_tree <- rpart(cl~x1+x2, data=df3, 
   control = rpart.control(minsplit = 5, cp = 0.00001))
df_tree_bnd <- explore(df_tree, df3)
bv6 <- ggplot() + 
  geom_point(data=filter(df_tree_bnd, .TYPE == "simulated"), 
       aes(x=x1, y=x2, colour=cl), alpha=0.6) +
  scale_colour_discrete_divergingx(palette = "Zissou 1") +
  geom_point(data=df3, aes(x=x1, y=x2, shape=cl),
             colour="black", alpha=0.5) +
  scale_shape_manual(values = c(1,4)) +
  annotate("text", 1.8, 1.9, label="f") +
  theme_minimal() +
  theme(aspect.ratio = 1,
        legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank())

bv1 + bv3 + bv5 + bv2 + bv4 + bv6 + plot_layout(ncol=3)


## --------------------------------------------------------------------------
#| eval: false
#| echo: false
# library(mulgar)
# vr <- c(-0.5, -0.3, 0.0, 0.3, 0.5)
# set.seed(503)
# vc1 <- matrix(c(1.5, sample(vr, 1), sample(vr, 1), sample(vr, 1),
#                sample(vr, 1), 1.5, sample(vr, 1), sample(vr, 1),
#                sample(vr, 1), sample(vr, 1), 1.5, sample(vr, 1),
#                sample(vr, 1), sample(vr, 1), sample(vr, 1), 1.5),
#               ncol=4, byrow=TRUE)
# vc1[1,2] <- vc1[2,1]
# vc1[1,3] <- vc1[3,1]
# vc1[1,4] <- vc1[4,1]
# vc1[2,3] <- vc1[3,2]
# vc1[2,4] <- vc1[4,2]
# vc1[3,4] <- vc1[4,3]
# g1 <- rmvn(n=335, p=4, mn=c(3, 3, 3, 3), vc=vc1)
# vc2 <- matrix(c(1, 0.6, 0.6, 0.6,
#                0.6, 1, 0.6, 0.6,
#                0.6, 0.6, 1, 0.6,
#                0.6, 0.6, 0.6, 1), ncol=4, byrow=TRUE)
# g2 <- rmvn(n=335, p=4, mn=c(3, -3, 3, -3), vc=vc2)
# g1 <- bind_cols(as.data.frame(g1), rep("A", 335))
# colnames(g1) <- c("x1", "x2", "x3", "x4", "cl")
# g2 <- bind_cols(as.data.frame(g2), rep("B", 335))
# colnames(g2) <- c("x1", "x2", "x3", "x4", "cl")
# gd <- bind_rows(g1, g2) |>
#   mutate(cl = factor(cl))
# animate_xy(gd[,1:4], col=gd$cl)
# animate_xy(gd[,1:4], guided_tour(lda_pp(gd$cl)), col=gd$cl)
# GGally::ggscatmat(gd, columns=1:4, color="cl") +
#   scale_color_discrete_divergingx(palette="Zissou 1")
# 
# set.seed(645)
# render_gif(gd[,1:4],
#            grand_tour(),
#            display_xy(col=gd$cl,
#                           axes="bottomleft"),
#            gif_file = "gifs/intro_class1.gif",
#            frames=200,
#            width=400,
#            height=400)
# render_gif(gd[,1:4],
#            guided_tour(lda_pp(gd$cl)),
#            display_xy(col=gd$cl,
#                           axes="bottomleft"),
#            gif_file = "gifs/intro_class2.gif",
#            frames=200,
#            width=400,
#            height=400,
#            loop=FALSE)
# 
# # non-linear example
# knots <- c(-0.75, -0.5, 0.1, 0.7)
# n <- 1000
# # Piece 1 has x2=-0.75
# # Piece 2 has x2=c1+5*x1 has to pass through (-0.75, -0.75) so c1=3
# # Piece 3 has x2=c2-1.5*x1 has to pass through (-0.5, 0.5) so c2=-0.25
# # Piece 4 has x2=c3+2*x1 has to pass through (0.1, -0.4) so c3=-0.6
# # Piece 5 has x2=0.8
# x1 <- runif(n, -1, 1)
# x2 <- case_when(
#   x1 < (-0.75) ~ -0.75,
#   between(x1, -0.75, -0.5) ~ 3+5*x1,
#   between(x1, -0.5, 0.1) ~ -0.25-1.5*x1,
#   between(x1, 0.1, 0.7) ~ -0.6+2*x1,
#   x1 > 0.7 ~ 0.8)
# border <- tibble(x1, x2) |> arrange(x1)
# 
# ggplot(border, aes(x1, x2)) +
#   geom_line() +
#   xlim(c(-1,1)) +
#   ylim(c(-1,1))
# 
# n_obs <- 5000
# set.seed(401)
# d <- tibble(x1 = runif(n_obs, -1, 1),
#             x2 = runif(n_obs, -1, 1)) |>
#   mutate(cl = "A")
# d <- d |>
#   mutate(cl = case_when(
#     (x1 < (-0.75)) & (x2 < -0.75) ~ "B",
#     between(x1, -0.75, -0.5) & (x2 < 3+5*x1) ~ "B",
#     between(x1, -0.5, 0.1) & (x2 < -0.25-1.5*x1) ~ "B",
#     between(x1, 0.1, 0.7) & (x2 < -0.6+2*x1) ~ "B",
#     (x1 > 0.7) & (x2 < 0.8) ~ "B",
#     .default = "A")
#   ) |>
#   mutate(cl = factor(cl))
# 
# # Build gap
# gap <- 0.1
# d <- d |>
#   mutate(drop = case_when(
#     (x1 < (-0.75)) & (abs(x2 - (-0.75)) < gap) ~ "yes",
#     between(x1, -0.75, -0.5) & (abs(x2-(3+5*x1)) < 5*gap) ~ "yes",
#     between(x1, -0.5, 0.1) & (abs(x2 -(-0.25-1.5*x1)) < 1.5*gap) ~ "yes",
#     between(x1, 0.1, 0.7) & (abs(x2 -(-0.6+2*x1)) < 2*gap) ~ "yes",
#     (x1 > 0.7) & (abs(x2 - 0.8) < gap) ~ "yes",
#     .default = "no"))
# 
# d_gap <- d |>
#   filter(drop == "no")
# ggplot() +
#   geom_point(data=d_gap, aes(x1, x2, colour=cl), alpha=0.7) +
#   scale_colour_discrete_divergingx(palette = "Zissou 1") +
#   geom_line(data=border, aes(x1, x2)) +
#   theme_minimal() +
#   theme(aspect.ratio = 1)
# 
# # Raise into high-d
# d_high <- d |>
#   mutate(x3 = runif(n_obs, -1, 1),
#          x4 = runif(n_obs, -1, 1),
#          x5 = runif(n_obs, -1, 1),
#          x6 = runif(n_obs, -1, 1)) |>
#   dplyr::select(x1,x2,x3,x4,x5,x6,cl,drop)
# 
# d_high_gap <- d_high |>
#   filter(drop == "no")
# 
# # Make boundary a combination of variables: x1-x3, x2-x4
# theta <- pi/4
# d_high_comb <- d_high |>
#   mutate(x7 = cos(theta)*x1 + sin(theta)*x3,
#          x8 = -sin(theta)*x1 + cos(theta)*x3,
#          x9 = cos(theta)*x2 + sin(theta)*x4,
#          x10 = -sin(theta)*x2 + cos(theta)*x4) |>
#   dplyr::select(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,cl,drop)
# d_high_comb_gap <- d_high_comb |>
#   filter(drop == "no")
# 
# animate_slice(d_high_comb_gap[,6:10], col=d_high_gap$cl, axes="bottomleft", v_rel=0.2)
# 
# # Doesn't work
# r_breaks <- linear_breaks(5, 0, 1)
# a_breaks <- angular_breaks(10)
# eps <- estimate_eps(nrow(d_high_comb_gap),
#                     ncol(d_high_comb_gap), 0.1 / 1, 5 * 10, 10,
#                     r_breaks)
# idx <- slice_index(r_breaks, a_breaks, eps, bintype = "polar",
#                    power = 1, reweight = TRUE, p = 5)
# animate_slice(d_high_comb_gap[,6:10],
#               guided_section_tour(idx, v_rel = 0.2, max.tries = 50),
#               v_rel = 0.2)
# 
# # Try with spheres
# library(geozoo)
# set.seed(637)
# sg1 <- sphere.solid.random(n=313, p=4)$points
# sg2 <- sphere.hollow(n=323, p=4)$points * 2
# sg1 <- bind_cols(as.data.frame(sg1), as.data.frame(rep("A", 313)))
# colnames(sg1) <- c("x1", "x2", "x3", "x4", "cl")
# sg2 <- bind_cols(as.data.frame(sg2), as.data.frame(rep("B", 323)))
# colnames(sg2) <- c("x1", "x2", "x3", "x4", "cl")
# sg <- bind_rows(sg1, sg2) |>
#   mutate(cl = factor(cl))
# 
# animate_xy(sg[,1:4], col=sg$cl)
# animate_slice(sg[,1:4], col=sg$cl, v_rel=1.2)
# 
# set.seed(645)
# sph_path <- save_history(sg[,1:4])
# render_gif(sg[,1:4],
#            planned_tour(sph_path),
#            display_xy(col=sg$cl,
#                           axes="bottomleft"),
#            gif_file = "gifs/intro_class3.gif",
#            frames=200,
#            width=400,
#            height=400)
# render_gif(sg[,1:4],
#            planned_tour(sph_path),
#            display_slice(col=sg$cl, v_rel=1.2,
#                           axes="bottomleft"),
#            gif_file = "gifs/intro_class4.gif",
#            frames=200,
#            width=400,
#            height=400)


## --------------------------------------------------------------------------
#| echo: true
#| eval: false
# library(readr)
# library(dplyr)
# music <- read_csv("http://ggobi.org/book/data/music-sub.csv",
#                   show_col_types = FALSE) |>
#   rename(title = `...1`) |>
#   mutate(type = factor(type))


## --------------------------------------------------------------------------
#| eval: false
# library(mulgar)
# data(aflw)
# aflw_sub <- aflw |>
#   dplyr::filter(position %in% c("BPL", "FF", "RR")) |>
#   dplyr::mutate(position = factor(position)) |>
#   dplyr::select(goals:tackles)


## ----eval=FALSE------------------------------------------------------------
#| echo: false
# library(mulgar)
# library(tourr)
# library(dplyr)
# data(bushfires)
# b_sub <- bushfires |>
#   select(se, maxt, mint, log_dist_road, cause) |>
#   filter(cause %in% c("accident", "lightning")) |>
#   rename(ldr = log_dist_road) |>
#   mutate(cause = factor(cause))
# animate_xy(b_sub[,-5], col=b_sub$cause, rescale=TRUE)
# animate_xy(b_sub[,-5], guided_tour(lda_pp(b_sub$cause)), col=b_sub$cause, rescale=TRUE)
# 
# data(pisa)
# set.seed(441)
# pisa_sub <- pisa |>
#   group_by(CNT) |>
#   sample_frac(0.10)
# animate_xy(pisa_sub[,2:6], col=pisa_sub$CNT)
# 
# animate_xy(music[,4:8], guided_tour(lda_pp(music$type)), col=music$type, rescale=TRUE)
# 
# data(aflw)
# aflw_sub <- aflw |>
#   filter(position %in% c("BPL", "FF", "RR")) |>
#   mutate(position = factor(position)) |>
#   select(goals:tackles)
# animate_xy(aflw_sub[,1:7], col=aflw_sub$position, rescale=TRUE)
# animate_xy(aflw_sub[,1:7],
#            guided_tour(lda_pp(aflw_sub$position)),
#            col=aflw_sub$position,
#            rescale=TRUE)
# 

