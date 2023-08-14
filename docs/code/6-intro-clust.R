#| label: fig-ideal-clusters
#| echo: FALSE
#| fig-width: 5
#| fig-height: 5
#| out-width: "100%"
#| fig-cap: "Different structures in data impact cluster analysis.  When there are well-separated groups (a), it is simple to group similar observations. Even when there are not, partitioning observations into groups may still be useful. There may be nuisance observations (b) or nuisance variables (c) that affect the interpoint distance calculations and distract the clustering algorithm, and there may oddly shaped clusters (d) which are hard to numerically describe."
#| message: false
stdd <- function(x) (x-mean(x))/sd(x)
set.seed(20230513)
d_sep_cl <- matrix(rnorm(99*2), ncol=2)
d_sep_cl[1:33,1] <-
  d_sep_cl[1:33,1]+8
d_sep_cl[34:66,2] <-
  d_sep_cl[34:66,2]+8
d_sep_cl[34:66,1] <-
  d_sep_cl[34:66,1]+4
d_sep_cl <- data.frame(x1=stdd(d_sep_cl[,1]), 
                       x2=stdd(d_sep_cl[,2]))

x <- (runif(20)-0.5)*4
y <- x
d_nuis_pts <- data.frame(x1 = stdd(c(rnorm(50, -3), 
                            rnorm(50, 3), x)),
                 x2 = stdd(c(rnorm(50, -3), 
                            rnorm(50, 3), y)))

d_nuis_vars <- matrix(rnorm(99*2), ncol=2)
d_nuis_vars[1:49,1] <- d_nuis_vars[1:49,1]+8
d_nuis_vars <-
  data.frame(x1=stdd(d_nuis_vars[,1]),
             x2=stdd(d_nuis_vars[,2]))

d_odd_shapes <- matrix(rnorm(99*2),ncol=2)
d_odd_shapes[1:66,2] <- (d_odd_shapes[1:66,1])^2-5 + rnorm(66)*0.6
d_odd_shapes[1:66,1] <- d_odd_shapes[1:66,1]*3
d_odd_shapes <-
  data.frame(x1=stdd(d_odd_shapes[,1]),
             x2=stdd(d_odd_shapes[,2]))

library(ggplot2)
library(patchwork)
p1 <- ggplot(d_sep_cl, aes(x=x1, y=x2)) + 
  geom_point(colour="#3B99B1", alpha=0.7) + 
  annotate("text", -2.5, 2.5, label="a") +
  xlim(-2.8, 2.8) + ylim(-2.8, 2.8) +
  theme(aspect.ratio=1,
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect("white"),
        panel.border = element_rect("black", fill=NA, 
             linewidth = 0.5))
p2 <- ggplot(d_nuis_pts, aes(x=x1, y=x2)) + 
  geom_point(colour="#3B99B1", alpha=0.7) + 
  annotate("text", -2.5, 2.5, label="b") +
  xlim(-2.8, 2.8) + ylim(-2.8, 2.8) +
  theme(aspect.ratio=1,
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect("white"),
        panel.border = element_rect("black", fill=NA, 
             linewidth = 0.5))
p3 <- ggplot(d_nuis_vars, aes(x=x1, y=x2)) + 
  geom_point(colour="#3B99B1", alpha=0.7) + 
  annotate("text", -2.5, 2.5, label="c") +
  xlim(-2.8, 2.8) + ylim(-2.8, 2.8) +
  theme(aspect.ratio=1,
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect("white"),
        panel.border = element_rect("black", fill=NA, 
             linewidth = 0.5))
p4 <- ggplot(d_odd_shapes, aes(x=x1, y=x2)) + 
  geom_point(colour="#3B99B1", alpha=0.7) + 
  annotate("text", -2.5, 2.5, label="d") +
  xlim(-2.8, 2.8) + ylim(-2.8, 2.8) +
  theme(aspect.ratio=1,
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect("white"),
        panel.border = element_rect("black", fill=NA, 
             linewidth = 0.5))
print(p1 + p2 + p3 + p4 + plot_layout(ncol=2))


#| message: FALSE
#| warning: false
#| fig-width: 4
#| fig-height: 4
#| fig-align: center
#| code-summary: "Code for plot"
x <- data.frame(V1 = c(7.3, 7.4, 4.1),
                    V2 = c(7.6, 7.2, 4.6),
                    V3 = c(7.7, 7.3, 4.6),
                    V4 = c(8.0, 7.2, 4.8),
                    point = factor(c("a1", "a2", "a3")))
library(GGally)
library(colorspace)
library(gridExtra)
pscat <- ggpairs(x, columns=1:4,
                 upper=list(continuous="points"),
                 diag=list(continuous="blankDiag"),
                 axisLabels="internal",
                 ggplot2::aes(colour=point)) +
    scale_colour_discrete_divergingx(
      palette = "Zissou 1", nmax=4) +
    xlim(3.7, 8.5) + ylim(3.7, 8.5) + 
    theme_minimal() +
    theme(aspect.ratio=1)
pscat

#| fig-width: 3
#| fig-height: 3.4
#| code-summary: "Code for plot"
ppar <- ggparcoord(x, columns=1:4, 
                   groupColumn = 5, 
                   scale = "globalminmax") +
    scale_colour_discrete_divergingx(
      palette = "Zissou 1", nmax=4) +
  xlab("") + ylab("") + 
  theme_minimal() + 
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank())
ppar


#| echo: false
library(mulgar)
vc <- matrix(c(1,0.8,0.8,0.8,1,0.8,0.8,0.8,1), ncol=3, byrow=TRUE)
set.seed(9044)
x <- data.frame(matrix(0, 4, 3))
x[1,] <- c(0.13, 0.21, 0.09)
x[2,] <- c(0.91, 0.95, 0.85)
x[3,] <- c(0.62, 0.73, 0.65)
x[4,] <- c(0.21, 0.92, 0.43)
rownames(x) <- paste0("a", 1:4)
colnames(x) <- paste0("x", 1:3)
x 


#| echo: false
rownames(vc) <- paste0("x", 1:3)
colnames(vc) <- paste0("x", 1:3)
vc


#| eval: false
#| echo: false
## # Answers
## library(patchwork)
## # Euclidean
## dist(x)
## # Mahalanobis
## mahalanobis(x, center=FALSE, cov=vc)
## # manhattan
## dist(x, "manhattan")
## # Chebychev
## d_ch <- matrix(0, 4, 4)
## for (i in 1:4) {
##   for (j in 1:4) {
##     if (i != j)
##       d_ch[i,j] <- abdiv::chebyshev(x[i,], x[j,])
##   }}
## as.dist(d_ch)
## # Bray-Curtis
## vegdist(x, "bray")
## # Plot
## x <- data.frame(x)
## p1 <- ggplot(x) + geom_text(aes(x=x1, y=x2,
##                                 label=rownames(x))) +
##   geom_abline(slope=1, intercept=0) +
##   theme(aspect.ratio=1)
## p2 <- ggplot(x) + geom_text(aes(x=x1, y=x3,
##                                 label=rownames(x))) +
##   geom_abline(slope=1, intercept=0) +
##   theme(aspect.ratio=1)
## p3 <- ggplot(x) + geom_text(aes(x=x2, y=x3,
##                                 label=rownames(x))) +
##   geom_abline(slope=1, intercept=0) +
##   theme(aspect.ratio=1)
## p1+p2+p3+plot_layout(ncol=3)

