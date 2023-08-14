#| label: fig-sup-example
#| fig-cap: "Examples of supervised classification patterns: (a) linearly separable, (b) linear but not completely separable, (c) non-linearly separable, (d) non-linear, but not completely separable."
#| echo: false
#| message: false
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
  annotate("text", -0.6, 6.5, label="a") +
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
  annotate("text", -1.95, 2.15, label="c") +
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
  annotate("text", -1.95, 2.8, label="d") +
  theme(aspect.ratio=1,
        legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect("white"),
        panel.border = element_rect("black", fill=NA, 
             linewidth = 0.5))
print(class1 + class2 + class3 + class4 + plot_layout(ncol=2))

