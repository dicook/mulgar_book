#| message: FALSE
#| error: FALSE
#| warning: FALSE
#| code-fold: false
library(dplyr)
library(ggplot2)
library(mulgar)
data(plane)
data(box)
library(geozoo)
cube5d <- data.frame(cube.solid.random(p=5, n=300)$points)
colnames(cube5d) <- paste0("x", 1:5)
cube5d <- data.frame(apply(cube5d, 2, function(x) (x-mean(x))/sd(x)))
p_pca <- prcomp(plane)
b_pca <- prcomp(box)
c_pca <- prcomp(cube5d)
p_scree <- ggscree(p_pca, q = 5) + theme_minimal()

b_scree <- ggscree(b_pca, q = 5) + theme_minimal()
c_scree <- ggscree(c_pca, q = 5) + theme_minimal()


#| label: fig-2D-pca1
#| echo: false
#| fig-width: 3
#| fig-height: 3
#| fig-cap: 2D in 5D
p_scree


#| label: fig-2D-pca2
#| echo: false
#| fig-width: 3
#| fig-height: 3
#| fig-cap: 3D in 5D
b_scree


#| label: fig-2D-pca3
#| echo: false
#| fig-width: 3
#| fig-height: 3
#| fig-cap: fully 5D
c_scree


#| label: tbl-plane-pcs
#| tbl-cap: "Coefficients for the first two PCs for the plane data."
#| code-summary: "Code to print PC coefficients"
library(gt)
p_pca$rotation[,1:2] %>%
  as_tibble(rownames="Variable") %>% 
  gt() %>%
  fmt_number(columns = c(PC1, PC2),
             decimals = 2)


#| label: tbl-box-pcs
#| tbl-cap: "Coefficients for the first three PCs for the box data."
#| code-summary: "Code to print PC coefficients"
b_pca$rotation[,1:3] %>%
  as_tibble(rownames="Variable") %>% 
  gt() %>%
  fmt_number(columns = c(PC1, PC2, PC3),
             decimals = 2)


#| label: fig-plane-noise-scree
#| fig-cap: Additional noise variables expands the data to 4D.
#| code-fold: false
#| fig-width: 6
#| fig-height: 4
#| out-width: 80%
set.seed(5143)
plane_noise <- plane
plane_noise$x6 <- rnorm(100)
plane_noise$x7 <- rnorm(100)
plane_noise <- data.frame(apply(plane_noise, 2, function(x) (x-mean(x))/sd(x)))

pn_pca <- prcomp(plane_noise)
ggscree(pn_pca, q = 7) + theme_minimal()


#| label: tbl-plane-noise-pcs
#| tbl-cap: "Coefficients for the first four PCs for the box data."
#| code-summary: "Code to print PC coefficients"
pn_pca$rotation[,1:4] %>%
  as_tibble(rownames="Variable") %>% 
  gt() %>%
  fmt_number(columns = c(PC1, PC2, PC3, PC4),
             decimals = 2)


#| code-fold: false
data(pisa)
pisa_std <- pisa %>%
  filter(CNT == "Australia") %>%
  select(-CNT) %>%
  mutate_all(mulgar:::scale2)
pisa_pca <- prcomp(pisa_std)
pisa_scree <- ggscree(pisa_pca, q = 15) + theme_minimal()


#| eval: FALSE
#| code-fold: false
## animate_xy(pisa_std, half_range=1)


#| eval: FALSE
#| code-fold: false
## render_gif(pisa_std,
##            grand_tour(),
##            display_xy(half_range=0.9),
##            gif_file="gifs/pisa_gt.gif",
##            frames=500,
##            width=400,
##            height=400,
##            loop=FALSE)


#| label: fig-pisa-scree
#| eval: true
#| message: false
#| warning: false
#| fig-cap: "Scree plot for the PCA on the pisa data suggests that the data is 1D."
#| fig-width: 6
#| fig-height: 4
#| out-width: 80%
#| echo: false
pisa_scree


#| code-summary: "Code to print PC coefficients"
round(pisa_pca$rotation[,1], 2)


#| label: pca-libraries
#| eval: TRUE
#| echo: TRUE
#| message: FALSE
#| error: FALSE
#| warning: FALSE
#| code-fold: false
library(tourr)
data(aflw)
aflw_std <- aflw %>%
  mutate_if(is.numeric, function(x) (x-
      mean(x, na.rm=TRUE))/
      sd(x, na.rm=TRUE))


#| label: aflw-gt
#| eval: false
#| code-summary: "Code to generate tour"
## animate_xy(aflw_std[,7:35], half_range=0.9)
## render_gif(aflw_std[,7:35],
##            grand_tour(),
##            display_xy(half_range=0.9),
##            gif_file="gifs/aflw_gt.gif",
##            frames=500,
##            loop=FALSE)


#| label: fig-aflw-pca
#| fig-cap: "Scree plot showing decay in variance of PCs."
#| alt-text: "Scree plot showing variance vertically against PC number horizontally. Variance drops from close to 10 for PC 1 to about 1.2 for PC 4 then slowly decays through to PC 29"
#| fig-width: 6
#| fig-height: 4
#| out-width: 80%
#| code-summary: "Code to make screeplotr"

aflw_pca <- prcomp(aflw_std[,7:35], 
               scale = FALSE, 
               retx=TRUE)

ggscree(aflw_pca, q = 29) + theme_minimal()


#| label: tbl-aflw-pcs
#| tbl-cap: "Coefficients for the first four PCs."
#| code-summary: "Code to print PC coefficients"
library(gt)
aflw_pca$rotation[,1:4] %>%
  as_tibble(rownames="Variable") %>% 
  arrange(desc(PC1), desc(PC2), desc(PC3)) %>%
  gt() %>%
  fmt_number(columns = c(PC1, PC2, PC3, PC4),
             decimals = 2)


#| label: aflw-plotly
#| eval: false
#| code-fold: true
#| code-summary: "Code to make tour animation"
## library(plotly)
## library(htmlwidgets)
## set.seed(20)
## b <- basis_random(4, 2)
## aflw_pct <- tourr::save_history(aflw_pca$x[,1:4],
##                     tour_path = grand_tour(),
##                     start = b,
##                     max_bases = 5)
## # To reconstruct projected data plots, later
## save(aflw_pct, file="data/aflw_pct.rda")
## aflw_pcti <- interpolate(aflw_pct, 0.1)
## aflw_anim <- render_anim(aflw_pca$x[,1:4],
##                          frames=aflw_pcti,
##              obs_labels=paste0(aflw$surname,
##                                aflw$given_name))
## 
## aflw_gp <- ggplot() +
##      geom_path(data=aflw_anim$circle,
##                aes(x=c1, y=c2,
##                    frame=frame), linewidth=0.1) +
##      geom_segment(data=aflw_anim$axes,
##                   aes(x=x1, y=y1,
##                       xend=x2, yend=y2,
##                       frame=frame),
##                   linewidth=0.1) +
##      geom_text(data=aflw_anim$axes,
##                aes(x=x2, y=y2,
##                    frame=frame,
##                    label=axis_labels),
##                size=5) +
##      geom_point(data=aflw_anim$frames,
##                 aes(x=P1, y=P2,
##                     frame=frame,
##                     label=obs_labels),
##                 alpha=0.8) +
##      xlim(-1,1) + ylim(-1,1) +
##      coord_equal() +
##      theme_bw() +
##      theme(axis.text=element_blank(),
##          axis.title=element_blank(),
##          axis.ticks=element_blank(),
##          panel.grid=element_blank())
## aflw_pctour <- ggplotly(aflw_gp,
##                         width=500,
##                         height=550) %>%
##        animation_button(label="Go") %>%
##        animation_slider(len=0.8, x=0.5,
##                         xanchor="center") %>%
##        animation_opts(easing="linear", transition = 0)
## 
## htmlwidgets::saveWidget(aflw_pctour,
##           file="html/aflw_pca.html",
##           selfcontained = TRUE)


#| eval: false
#| echo: false
#| code-fold: false
## animate_pca(aflw_pca$x[,1:5],
##              pc_coefs = aflw_pca$rotation[,1:5],
##              col = "orange",
##              pch = 16,
##              cex = 0.5,
##              half_range=1.5)
##  animate_xy(aflw_pca$x[,1:5])


#| label: fig-aflw-pcaplots
#| eval: true
#| message: false
#| warning: false
#| fig-cap: "Frame 18 re-plotted so that players can be identified on mouse-over."
#| fig-width: 5
#| fig-height: 4
#| out-width: 80%
#| code-summary: "Code to generate interactive plot of frame 18"
library(plotly)
load("data/aflw_pct.rda")
aflw_pcti <- interpolate(aflw_pct, 0.1)
f18 <- matrix(aflw_pcti[,,18], ncol=2)
p18 <- render_proj(aflw_pca$x[,1:4], f18, 
                   obs_labels=paste0(aflw$surname,
                               aflw$given_name))
pg18 <- ggplot() +
  geom_path(data=p18$circle, aes(x=c1, y=c2)) +
  geom_segment(data=p18$axes, aes(x=x1, y=y1, xend=x2, yend=y2)) +
  geom_text(data=p18$axes, aes(x=x2, y=y2, label=rownames(p18$axes))) +
  geom_point(data=p18$data_prj, aes(x=P1, y=P2, label=obs_labels)) +
  xlim(-1,1) + ylim(-1, 1) +
  #ggtitle("Frame 18") +
  theme_bw() +
  theme(
    axis.text=element_blank(),
    axis.title=element_blank(),
    axis.ticks=element_blank(),
    panel.grid=element_blank())
ggplotly(pg18, width=500, height=500)


#| label: fig-plane-biplot
#| fig-cap: "Biplots of the plane (a) and plane + noise (b) data. All five variables contribute strongly to the two principal components in (a): PC1 is primarily `x1`, `x2` and `x3` and PC2 is primarily `x4` and `x5`. In (b) the same four variables contribute in almost the same way, with variables `x6` and `x7` contributing very little. The data was constructed this way, that these two dimensions were purely noise."
#| code-fold: false
#| fig-width: 8
#| fig-height: 4
library(ggfortify)
library(patchwork)
plane_pca <- prcomp(plane)
pl1 <- autoplot(plane_pca, loadings = TRUE, 
         loadings.label = TRUE) + 
  ggtitle("(a)") +
  theme_minimal() + 
  theme(aspect.ratio=1)
plane_noise_pca <- prcomp(plane_noise)
pl2 <- autoplot(plane_noise_pca, loadings = TRUE, 
         loadings.label = TRUE) + 
  ggtitle("(b)") +
  theme_minimal() + 
  theme(aspect.ratio=1)
pl1 + pl2


#| eval: false
#| code-fold: false
## plane_m <- pca_model(plane_pca)
## plane_m_d <- rbind(plane_m$points, plane)
## animate_xy(plane_m_d, edges=plane_m$edges,
##            axes="bottomleft",
##            edges.col="#E7950F",
##            edges.width=3)
## render_gif(plane_m_d,
##            grand_tour(),
##            display_xy(half_range=0.9,
##                       edges=plane_m$edges,
##                       edges.col="#E7950F",
##                       edges.width=3),
##            gif_file="gifs/plane_model.gif",
##            frames=500,
##            width=400,
##            height=400,
##            loop=FALSE)
## box_pca <- prcomp(box)
## box_m <- pca_model(box_pca, d=3)
## box_m_d <- rbind(box_m$points, box)
## animate_xy(box_m_d, edges=box_m$edges,
##            axes="bottomleft", edges.col="#E7950F", edges.width=3)
## render_gif(box_m_d,
##            grand_tour(),
##            display_xy(half_range=0.9,
##                       edges=box_m$edges,
##                       edges.col="#E7950F",
##                       edges.width=3),
##            gif_file="gifs/box_model.gif",
##            frames=500,
##            width=400,
##            height=400,
##            loop=FALSE)


#| eval: FALSE
#| code-fold: false
## pisa_model <- pca_model(pisa_pca, d=1, s=2)
## 
## pisa_all <- rbind(pisa_model$points, pisa_std)
## animate_xy(pisa_all, edges=pisa_model$edges,
##            edges.col="#E7950F", edges.width=3)
## render_gif(pisa_all,
##            grand_tour(),
##            display_xy(half_range=0.9,
##                       edges=pisa_model$edges,
##                       edges.col="#E7950F",
##                       edges.width=5),
##            gif_file="gifs/pisa_model.gif",
##            frames=500,
##            width=400,
##            height=400,
##            loop=FALSE)


#| eval: FALSE
#| code-fold: false
## aflw_model <- pca_model(aflw_pca, d=4, s=1)
## 
## aflw_all <- rbind(aflw_model$points, aflw_std[,7:35])
## animate_xy(aflw_all, edges=aflw_model$edges,
##            edges.col="#E7950F",
##            edges.width=3,
##            half_range=0.8,
##            axes="off")
## render_gif(aflw_all,
##            grand_tour(),
##            display_xy(half_range=0.8,
##                       edges=aflw_model$edges,
##                       edges.col="#E7950F",
##                       edges.width=3,
##                       axes="off"),
##            gif_file="gifs/aflw_model.gif",
##            frames=500,
##            width=400,
##            height=400,
##            loop=FALSE)


#| echo: false
plane_noise_outliers <- plane_noise
plane_noise_outliers[101,] <- c(2, 2, -2, 0, 0, 0, 0)
plane_noise_outliers[102,] <- c(0, 0, 0,-2, -2, 0, 0)


#| label: fig-plane-n-o-scree
#| code-fold: false
#| fig-cap: "Scree plot of the planar data with noise and an outlier. It is almost the same as the data without the outliers."
#| fig-width: 6
#| fig-height: 4
#| out-width: 80%
plane_n_o_pca <- prcomp(plane_noise_outliers)
ggscree(plane_n_o_pca, q = 7) + theme_minimal()


#| eval: false
#| #| code-summary: "Code to generate tours"
## clrs <- hcl.colors(12, "Zissou 1")
## p_col <- c(rep("black", 100), clrs[11], clrs[11])
## p_obs_labels <- c(rep("", 100), "1", "2")
## 
## animate_xy(plane_n_o_pca$x[,1:4],
##            col=p_col,
##            obs_labels=p_obs_labels)
## animate_xy(plane_noise_outliers,
##            col=p_col,
##            obs_labels=p_obs_labels)
## render_gif(plane_noise_outliers,
##            grand_tour(),
##            display_xy(half_range=0.8,
##                       col=p_col,
##              obs_labels=p_obs_labels),
##            gif_file="gifs/plane_n_o_clr.gif",
##            frames=500,
##            width=200,
##            height=200,
##            loop=FALSE)
## render_gif(plane_n_o_pca$x[,1:4],
##            grand_tour(),
##            display_xy(half_range=0.8,
##                       col=p_col,
##              obs_labels=p_obs_labels),
##            gif_file="gifs/plane_n_o_pca.gif",
##            frames=500,
##            width=200,
##            height=200,
##            loop=FALSE)


#| label: fig-plane-o-n-pairs
#| message: false
#| warning: false
#| fig-cap: "From the scatterplot matrix we can see that the outliers are present in PC5, PC6 and PC7. That means by reducing the dimensionality to the first four PCs the model has missed some important characteristics in the data."
#| code-summary: "Code to make scatterplot matrix"
library(GGally)
ggscatmat(plane_n_o_pca$x) + theme_minimal()


#| label: fig-plane-nonlin-scree
#| code-fold: false
#| fig-cap: "Scree plot of the non-linear data suggests three PCs."
#| fig-width: 6
#| fig-height: 4
#| out-width: 80%
data(plane_nonlin)
plane_nonlin_pca <- prcomp(plane_nonlin)
ggscree(plane_nonlin_pca, q = 5) + theme_minimal()


#| eval: false
#| code-summary: "Code to generate tour"
## animate_xy(plane_nonlin_pca$x[,1:3])
## render_gif(plane_nonlin_pca$x[,1:3],
##            grand_tour(),
##            display_xy(half_range=0.8),
##            gif_file="gifs/plane_nonlin_pca.gif",
##            frames=500,
##            width=200,
##            height=200)


#| label: fig-plane-nonlin-pairs
#| message: false
#| warning: false
#| fig-cap: "From the scatterplot matrix we can see that the there is a non-linear relationship visible in PC1 and PC2, with perhaps a small contribution from PC3. However, we can see that when the data is reduced to three PCs, it misses catching all on the non-linear relationships and also interestingly it seems that there is an unusual observation also."
#| code-summary: "Code to make scatterplot matrix"
ggscatmat(plane_nonlin_pca$x)


#| label: aflw-pairs
#| eval: false
#| echo: false
#| message: false
#| warning: false
## library(GGally)
## ggscatmat(aflw_pca$x, columns=1:4)


#| eval: false
#| echo: false
## library(mulgar)
## library(dplyr)
## data(bushfires)
## bushfires_std <- bushfires %>%
##   dplyr::select(`rf`:`log_dist_road`) %>%
##   mutate_if(is.numeric, function(x) (x-
##       mean(x, na.rm=TRUE))/
##       sd(x, na.rm=TRUE))
## bushfires_pca <- prcomp(bushfires_std)
## ggscree(bushfires_pca, q=20)
## bushfires_pc <- data.frame(bushfires_pca$x[,1:5])
## 
## bushfires_model <- pca_model(bushfires_pca, d=5, s=1)
## bushfires_all <- rbind(bushfires_model$points,
##                        bushfires_std)
## animate_xy(bushfires_all, edges=bushfires_model$edges,
##            edges.col="#E7950F",
##            edges.width=3,
##            half_range=12,
##            axes="off")
## 


#| eval: false
#| echo: false
#| message: false
#| warning: false
## library(dobin)
## library(GGally)
## dobin_pno_1 <- as.data.frame(dobin(plane_noise_outliers)$coords)
## dobin_pno_1$outlier <- c(rep("no", 100), rep("yes", 2))
## ggscatmat(dobin_pno_1, columns = 1:3, color = "outlier")
## # outliers not visible in V1 vs V2
## # with V3 we can identify one of the two outliers
## dobin_pno_2 <- as.data.frame(dobin(plane_noise_outliers, frac = 0.99)$coords)
## dobin_pno_2$outlier <- c(rep("no", 100), rep("yes", 2))
## ggscatmat(dobin_pno_2, columns = 1:3, color = "outlier")
## # with three dimensions we capture both outliers in our graph

