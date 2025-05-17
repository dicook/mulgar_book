# Code to make image for front page
library(mulgar)
library(tourr)
library(colorspace)
library(ggplot2)
library(dplyr)
library(patchwork)
library(MASS)
library(randomForest)
library(e1071)
library(classifly)
data("penguins_sub")

set.seed(1209)
p_t_guided <- save_history(
  penguins_sub[, 1:4], 
  guided_tour(lda_pp(penguins_sub$species)),
  max_bases = 50)

# pie(rep(1, 12), col = hcl.colors(12, "viridis"), main = "HCL") 11, 8, 5
# pie(rep(1, 12), col = hcl.colors(12, "Zissou 1"), main = "HCL") 10, 7, 3
# pie(rep(1, 12), col = hcl.colors(12, "ag.Sunset"), main = "HCL") 11, 8, 5
# pie(rep(1, 12), col = hcl.colors(12, "plasma"), main = "HCL") 11, 8, 5
# pie(rep(1, 12), col = hcl.colors(12, "Dark 3"), main = "HCL") 10, 4, 1
clrs <- hcl.colors(12, palette="Zissou 1")[c(2, 7, 10)]
plot_tour_projection <- function(d) {
  plt <- ggplot() +
    geom_path(data=d$circle, aes(x=c1, y=c2), colour = "white") +
    geom_segment(data=d$axes, 
                 aes(x=x1, y=y1, xend=x2, yend=y2), colour = "white") +
    #geom_text(data=d$axes, aes(x=x2, y=y2,
    #                           label=rownames(d$axes))) +
    geom_point(data=d$data_prj, 
               aes(x=P1, y=P2, 
                   colour=species)) +
    xlim(-1,1) + ylim(-1, 1) +
    #scale_color_discrete_divergingx(palette="Zissou 1") +
    scale_color_manual("", values = clrs) +
    theme_minimal() +
    theme(aspect.ratio=1,
          legend.position = "none",
          axis.text=element_blank(),
          axis.title=element_blank(),
          axis.ticks=element_blank(),
          panel.grid=element_blank(),
          panel.background = element_rect(fill='transparent',
                                          colour="white"))
  plt
}

prj <- matrix(p_t_guided[,,1], ncol=2)
gd1 <- render_proj(penguins_sub[, 1:4], prj)
gd1$data_prj$species <- penguins_sub$species
rownames(gd1$axes) <- paste0("x", 1:4)
gd1_plt <- plot_tour_projection(gd1)
prj <- matrix(p_t_guided[,,3], ncol=2)
gd2 <- render_proj(penguins_sub[, 1:4], prj)
gd2$data_prj$species <- penguins_sub$species
rownames(gd2$axes) <- paste0("x", 1:4)
gd2_plt <- plot_tour_projection(gd2)
prj <- matrix(p_t_guided[,,5], ncol=2)
gd3 <- render_proj(penguins_sub[, 1:4], prj)
gd3$data_prj$species <- penguins_sub$species
rownames(gd3$axes) <- paste0("x", 1:4)
gd3_plt <- plot_tour_projection(gd3)
p1 <- gd1_plt + gd2_plt + gd3_plt + plot_layout(ncol=3)
ggsave("front-page/front-cover1.png", p1, 
       background = "transparent", 
       width=9, height=3, units="cm")
ggsave("front-page/front-cover1a.png", gd1_plt, 
       background = "transparent", 
       width=6, height=6, units="cm")
ggsave("front-page/front-cover1b.png", gd2_plt, 
       background = "transparent", 
       width=6, height=6, units="cm")
ggsave("front-page/front-cover1c.png", gd3_plt, 
       background = "transparent", 
       width=6, height=6, units="cm")

# LDA
p_lda <- lda(species~bl+bd+fl+bm, 
             data=penguins_sub,
             prior=c(1/3, 1/3, 1/3))
p_vc_pool <- mulgar::pooled_vc(penguins_sub[,1:4],
                               penguins_sub$species)
# Create an ellipse corresponding to pooled vc
pool_ell <- gen_vc_ellipse(p_vc_pool, 
                           xm=rep(0, ncol(p_vc_pool)))
# Add means to produce ellipses for each species
p_lda_pool <- data.frame(rbind(
  pool_ell +
    matrix(rep(p_lda$means[1,],
               each=nrow(pool_ell)), ncol=4),
  pool_ell +
    matrix(rep(p_lda$means[2,],
               each=nrow(pool_ell)), ncol=4),
  pool_ell +
    matrix(rep(p_lda$means[3,],
               each=nrow(pool_ell)), ncol=4)))
# Create one data set with means, data, ellipses
p_lda_pool$species <- factor(rep(levels(penguins_sub$species),
                                 rep(nrow(pool_ell), 3)))
p_lda_pool$type <- "ellipse"
p_lda_means <- data.frame(
  p_lda$means,
  species=factor(rownames(p_lda$means)),
  type="mean")
p_data <- data.frame(penguins_sub[,1:5], 
                     type="data")
p_lda_all <- bind_rows(p_lda_means,
                       p_data,
                       p_lda_pool)
p_lda_all$type <- factor(p_lda_all$type, 
                         levels=c("mean", "data", "ellipse"))
shapes <- c(3, 4, 20)
p_pch <- shapes[p_lda_all$type]

load("data/penguins_tour_path.rda") # pt1
prj <- matrix(pt1[,,350], ncol=2)
glda <- render_proj(p_lda_all[,1:4], prj)
glda$data_prj$species <- p_lda_all$species
glda$data_prj$type <- p_lda_all$type
rownames(gd1$axes) <- paste0("x", 1:4)
glda_plt <- ggplot() +
  geom_path(data=glda$circle, aes(x=c1, y=c2), colour = "white") +
  geom_segment(data=glda$axes, 
               aes(x=x1, y=y1, xend=x2, yend=y2), colour = "white") +
  geom_point(data=glda$data_prj, 
             aes(x=P1, y=P2, 
                 colour=species, shape=type)) +
  xlim(-1,1) + ylim(-1, 1) +
  scale_color_manual("", values = clrs) +
  scale_shape_manual("", values = c(3, 4, 20)) +
  theme_minimal() +
  theme(aspect.ratio=1,
        legend.position = "none",
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        panel.grid=element_blank(),
        panel.background = element_rect(fill='transparent',
                                        colour="white"))

ggsave("front-page/front-cover2a.png", glda_plt, 
       background = "transparent", 
       width=6, height=6, units="cm")

# Errors from a model
penguins_rf <- randomForest(species~.,
                            data=penguins_sub[,1:5],
                            importance=TRUE)
penguins_errors <- penguins_sub |>
  mutate(err = ifelse(penguins_rf$predicted !=
                        penguins_rf$y, 1, 0))
symbols <- c(1, 16)
p_pch <- symbols[penguins_errors$err+1]
p_cex <- rep(1, length(p_pch))
p_cex[penguins_errors$err==1] <- 2

rf_err_prj <- animate_xy(penguins_errors[,1:4],
                         guided_tour(lda_pp(penguins_errors$species)),
                         col=penguins_errors$species,
                         pch=p_pch, cex=p_cex)

rf_err_prj$basis[338][[1]]

grf_err <- render_proj(penguins_errors[,1:4], rf_err_prj$basis[338][[1]])
grf_err$data_prj$species <- penguins_errors$species
grf_err$data_prj$err <- penguins_errors$err
rownames(grf_err$axes) <- paste0("x", 1:4)
grf_err_plt <- ggplot() +
  geom_path(data=grf_err$circle, aes(x=c1, y=c2), colour = "white") +
  geom_segment(data=grf_err$axes, 
               aes(x=x1, y=y1, xend=x2, yend=y2), colour = "white") +
  geom_point(data=grf_err$data_prj, 
             aes(x=P1, y=P2, 
                 colour=species, 
                 shape=as.factor(err),
                 size=as.factor(err))) +
  xlim(-1,1) + ylim(-1, 1) +
  scale_color_manual("", values = clrs) +
  scale_shape_manual("", values = c(1, 16)) +
  scale_size_manual("", values = c(1, 3)) +
  theme_minimal() +
  theme(aspect.ratio=1,
        legend.position = "none",
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        panel.grid=element_blank(),
        panel.background = element_rect(fill='transparent',
                                        colour="white"))

ggsave("front-page/front-cover2b.png", grf_err_plt, 
       background = "transparent", 
       width=6, height=6, units="cm")

# Boundary between two classes
chinstrap <- penguins_sub |>
  filter(species == "Chinstrap") |>
  select(-species) |>
  mutate_if(is.numeric, mulgar:::scale2)
chinstrap_svm <- svm(sex~., data=chinstrap, 
                     kernel="linear",
                     probability=TRUE, 
                     scale=FALSE)
chinstrap_svm_e <- explore(chinstrap_svm, chinstrap)
set.seed(1022)
prj1 <- mulgar::norm_vec(t(chinstrap_svm$SV) %*%
                           chinstrap_svm$coefs)
prj2 <- basis_random(4, 1)
prj <- orthonormalise(cbind(prj1, prj2))
prj

gsvm <- render_proj(chinstrap_svm_e[,1:4], prj)
gsvm$data_prj$sex <- chinstrap_svm_e$sex
gsvm$data_prj$.BOUNDARY <- chinstrap_svm_e$.BOUNDARY
gsvm$data_prj$.TYPE <- chinstrap_svm_e$.TYPE
gsvm$data_prj <- gsvm$data_prj[!chinstrap_svm_e$.BOUNDARY,]
rownames(gsvm$axes) <- paste0("x", 1:4)
clrs2 <- hcl.colors(12, palette="Zissou 1")[c(1, 12)]

gsvm_plt <- ggplot() +
  geom_path(data=gsvm$circle, aes(x=c1, y=c2), colour = "white") +
  geom_segment(data=gsvm$axes, 
               aes(x=x1, y=y1, xend=x2, yend=y2), colour = "white") +
  geom_point(data=gsvm$data_prj, 
             aes(x=P1, y=P2, 
                 colour=sex, 
                 shape=.TYPE)) +
  xlim(-1,1) + ylim(-1, 1) +
  scale_color_manual("", values = clrs2) +
  scale_shape_manual("", values = c(3, 20)) +
  theme_minimal() +
  theme(aspect.ratio=1,
        legend.position = "none",
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        panel.grid=element_blank(),
        panel.background = element_rect(fill='transparent',
                                        colour="white"))

ggsave("front-page/front-cover2c.png", gsvm_plt, 
       background = "transparent", 
       width=6, height=6, units="cm")

# Boundary between three classes
p_lda_e <- explore(p_lda, penguins_sub)
prj <- matrix(p_t_guided[,,5], ncol=2)
glda <- render_proj(p_lda_e[, 1:4], prj)
glda$data_prj$species <- p_lda_e$species
glda$data_prj$.TYPE <- p_lda_e$.TYPE
glda$data_prj$.BOUNDARY <- p_lda_e$.BOUNDARY
rownames(glda$axes) <- paste0("x", 1:4)
glda$data_prj <- glda$data_prj[!p_lda_e$.BOUNDARY,]
glda_plt <- ggplot() +
  geom_path(data=glda$circle, aes(x=c1, y=c2), colour = "white") +
  geom_segment(data=glda$axes, 
               aes(x=x1, y=y1, xend=x2, yend=y2), colour = "white") +
  geom_point(data=glda$data_prj, 
             aes(x=P1, y=P2, 
                 colour=species, 
                 shape=.TYPE)) +
  xlim(-1,1) + ylim(-1, 1) +
  scale_color_manual("", values = clrs) +
  scale_shape_manual("", values = c(3, 20)) +
  theme_minimal() +
  theme(aspect.ratio=1,
        legend.position = "none",
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        panel.grid=element_blank(),
        panel.background = element_rect(fill='transparent',
                                        colour="white"))

ggsave("front-page/front-cover2c.png", glda_plt, 
       background = "transparent", 
       width=6, height=6, units="cm")

# Ternary diagram
proj <- t(geozoo::f_helmert(3)[-1,])
p_rf_v_p <- as.matrix(penguins_rf$votes) %*% proj
colnames(p_rf_v_p) <- c("x1", "x2")
p_rf_v_p <- p_rf_v_p |>
  as.data.frame() |>
  mutate(species = penguins_sub$species)
simp <- simplex(p=2)
sp <- data.frame(cbind(simp$points), simp$points[c(2,3,1),])
colnames(sp) <- c("x1", "x2", "x3", "x4")
sp$species = sort(unique(penguins_sub$species))
p_ternary <- ggplot() +
  geom_segment(data=sp, aes(x=x1, y=x2, xend=x3, yend=x4), 
               colour = "white") +
  #geom_text(data=sp, aes(x=x1, y=x2, label=paste0("g", 1:3)),
  #          nudge_x=c(-0.06, 0.07, 0),
  #          nudge_y=c(0.05, 0.05, -0.05), colour = "white") +
  geom_point(data=p_rf_v_p, aes(x=x1, y=x2, 
                                colour=species), 
             size=3, alpha=0.5) +
  scale_color_manual("", values = clrs) +
  xlim(c(-0.8, 0.8)) + ylim(c(-1, 0.5)) +
  theme_map() +
  theme(aspect.ratio=1, legend.position="none",
        panel.background = element_rect(fill='transparent',
                                        colour="white"))

ggsave("front-page/front-cover2d.png", p_ternary, 
       background = "transparent", 
       width=6, height=6, units="cm")
