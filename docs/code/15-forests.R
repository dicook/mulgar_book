#| message: false
#| label: fig-p-bl-bd-tree1
#| fig-cap: Default tree fit
#| fig-width: 3
#| fig-height: 3
library(mulgar)
library(rpart)
library(rpart.plot)
library(colorspace)
library(classifly)
library(ggplot2)

load("data/penguins_sub.rda")
p_bl_bd_tree <- rpart(species~bl+bd, data=penguins_sub)
rpart.plot(p_bl_bd_tree, box.palette="Grays")


#| message: false
#| label: fig-p-bl-bd-tree2
#| fig-cap: Boundaries of tree fit
#| fig-width: 3
#| fig-height: 3
#| code-summary: "Code to draw tree and model fit"
p_bl_bd_tree_boundaries <- explore(p_bl_bd_tree, penguins_sub)
ggplot(p_bl_bd_tree_boundaries) +
  geom_point(aes(x=bl, y=bd, colour=species, shape=.TYPE)) + 
  scale_color_discrete_divergingx(palette="Zissou 1") +
  scale_shape_manual(values=c(46, 16)) +
  theme_minimal() +
  theme(aspect.ratio = 1, legend.position = "none")


#| eval: false
#| code-summary: "Code to make animated gifs of slice tour of boundaries"
## p_tree <- rpart(species~., data=penguins_sub[,1:5])
## rpart.plot(p_tree, box.palette="Grays")
## 
## p_tree_boundaries <- explore(p_tree, penguins_sub)
## animate_slice(p_tree_boundaries[p_tree_boundaries$.TYPE == "simulated",1:4], col=p_tree_boundaries[p_tree_boundaries$.TYPE == "simulated",6], v_rel=0.02, axes="bottomleft")
## load("data/penguins_tour_path.rda")
## render_gif(p_tree_boundaries[p_tree_boundaries$.TYPE == "simulated",1:4],
##            planned_tour(pt1),
##            display_slice(v_rel=0.02,
##              col=p_tree_boundaries[p_tree_boundaries$.TYPE == "simulated",6],
##              axes="bottomleft"),                     gif_file="gifs/penguins_tree_boundaries.gif",
##            frames=500,
##            loop=FALSE
##            )


#| message: false
#| code-fold: false
library(randomForest)
library(dplyr)
penguins_rf <- randomForest(species~.,
                             data=penguins_sub[,1:5],
                             importance=TRUE)
penguins_rf


#| code-summary: "Code to compute Helmert matrix"
geozoo::f_helmert(3)


#| message: false
#| code-fold: false
# Project 4D into 3D
library(geozoo)
proj <- t(geozoo::f_helmert(3)[-1,])
p_rf_v_p <- as.matrix(penguins_rf$votes) %*% proj
colnames(p_rf_v_p) <- c("x1", "x2")
p_rf_v_p <- p_rf_v_p %>%
  as.data.frame() %>%
  mutate(species = penguins_sub$species)


#| code-fold: false
# Add simplex
simp <- simplex(p=2)
sp <- data.frame(cbind(simp$points), simp$points[c(2,3,1),])
colnames(sp) <- c("x1", "x2", "x3", "x4")
sp$species = sort(unique(penguins_sub$species))
library(ggthemes)
p_ternary <- ggplot() +
  geom_segment(data=sp, aes(x=x1, y=x2, xend=x3, yend=x4)) +
  geom_text(data=sp, aes(x=x1, y=x2, label=species),
            nudge_x=c(-0.06, 0.07, 0),
            nudge_y=c(0.05, 0.05, -0.05)) +
  geom_point(data=p_rf_v_p, aes(x=x1, y=x2, colour=species), size=2, alpha=0.5) +
  scale_color_discrete_divergingx(palette="Zissou 1") +
  theme_map() +
  theme(aspect.ratio=1, legend.position="none")


#| eval: false
#| code-summary: "Code to generate animated gifs"
## # Look at the votes matrix, in its 3D space
## animate_xy(penguins_rf$votes, col=penguins_sub$species)
## 
## # Save an animated gif
## render_gif(penguins_rf$votes,
##            grand_tour(),
##            display_xy(v_rel=0.02,
##              col=penguins_sub$species,
##              axes="bottomleft"),
##            gif_file="gifs/penguins_rf_votes.gif",
##            frames=500,
##            loop=FALSE
## )


#| echo: false
#| label: fig-p-votes-ggplot
#| fig-cap: Votes matrix in its 2D space, a ternary diagram.
#| fig-width: 4
#| fig-height: 4
p_ternary


#| code-fold: false
library(mulgar)
library(dplyr)
library(liminal)
ft_pca <- prcomp(fake_trees[,1:100], 
                 scale=TRUE, retx=TRUE)
ft_pc <- as.data.frame(ft_pca$x[,1:10])
ft_pc$branches <- fake_trees$branches
library(randomForest)
ft_rf <- randomForest(branches~., data=ft_pc, 
                            importance=TRUE)
ft_rf


#| code-fold: false
ft_rf_votes <- ft_rf$votes %>%
  as_tibble() %>%
  mutate(branches = fake_trees$branches)

proj <- t(geozoo::f_helmert(10)[-1,])
f_rf_v_p <- as.matrix(ft_rf_votes[,1:10]) %*% proj
colnames(f_rf_v_p) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9")
f_rf_v_p <- f_rf_v_p %>%
  as.data.frame() %>%
  mutate(branches = fake_trees$branches)

simp <- geozoo::simplex(p=9)
sp <- data.frame(simp$points)
colnames(sp) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9")
sp$branches = ""
f_rf_v_p_s <- bind_rows(sp, f_rf_v_p) %>%
  mutate(branches = factor(branches))
labels <- c("0" , "1", "2", "3", "4", "5", "6", "7", "8", "9",
                rep("", 3000))


#| eval: false
#| code-summary: "Code to make animated gifs"
## animate_xy(f_rf_v_p_s[,1:9], col = f_rf_v_p_s$branches,
##            axes = "off", half_range = 0.8,
##            edges = as.matrix(simp$edges),
##            obs_labels = labels, palette = "Viridis")
## 
## render_gif(f_rf_v_p_s[,1:9],
##            grand_tour(),
##            display_xy(col = f_rf_v_p_s$branches,
##            axes = "off", half_range = 0.8,
##            edges = as.matrix(simp$edges),
##            obs_labels = labels, palette="Viridis"),
##            gif_file="gifs/ft_votes.gif",
##            frames=500)


#| code-fold: false
#| label: tbl-ft-importance
#| tbl-cap: Variable importance from the random forest fit to the fake_trees data.
library(gt)
ft_rf$importance %>% 
  as_tibble(rownames="Variable") %>% 
  rename(Accuracy=MeanDecreaseAccuracy,
         Gini=MeanDecreaseGini) %>%
  #arrange(desc(Gini)) %>%
  gt() %>%
  fmt_number(columns = c(`0`,`1`,`2`,`3`,`4`,`5`,`6`,`7`,`8`,`9`, Accuracy),
             decimals = 2) %>%
  fmt_number(columns = Gini,
             decimals = 0)


#| code-fold: false
ft_pc <- ft_pc %>%
  mutate(cl1 = factor(case_when(
                 branches == "0" ~ "0",
                 branches == "1" ~ "1",
                 .default = "other"
  )))


#| eval: false
#| code-summary: "Code to make animated gifs"
## animate_xy(ft_pc[,c("PC1", "PC2", "PC4", "PC6")], col=ft_pc$cl1, palette="Viridis")
## render_gif(ft_pc[,c("PC1", "PC2", "PC4", "PC6")],
##            grand_tour(),
##            display_xy(col=ft_pc$cl1, palette="Viridis"),
##            gif_file="gifs/ft_cl1.gif",
##            frames=500)


#| code-summary: "Code to make plot"
ft_pc_cl1 <- ggplot(ft_pc, aes(x=PC4, y=PC2, col=cl1)) +
  geom_point(alpha=0.7, size=1) +
  scale_color_discrete_sequential(palette="Viridis", rev=FALSE) +
  theme_minimal() +
  theme(aspect.ratio = 1)


#| echo: false
#| fig-width: 4
#| fig-height: 4
#| out-width: 100%
#| label: fig-ft-cl1-pc
#| fig-cap: PC2 and PC4 together reveal cluster 1.
ft_pc_cl1 


#| code-fold: false
ft_pc <- ft_pc %>%
  mutate(cl8 = factor(case_when(
                 branches == "0" ~ "0",
                 branches == "6" ~ "6",
                 branches == "1" ~ "1",
                 branches == "8" ~ "8",
                 .default = "other"
  )))


#| eval: false
#| code-summary: "Code to make animated gif"
## animate_xy(ft_pc[,c("PC1", "PC2", "PC4", "PC5", "PC6")], col=ft_pc$cl8, palette="Viridis")
## render_gif(ft_pc[,c("PC1", "PC2", "PC4", "PC5", "PC6")],
##            grand_tour(),
##            display_xy(col=ft_pc$cl8, palette="Viridis"),
##            gif_file="gifs/ft_cl8.gif",
##            frames=500)


#| code-summary: "Code to make plot"
ft_pc_cl8 <- ggplot(ft_pc, aes(x=PC1, y=PC5, col=cl8)) +
  geom_point(alpha=0.7, size=1) +
  scale_color_discrete_sequential(palette="Viridis", rev=FALSE) +
  theme_minimal() +
  theme(aspect.ratio = 1)


#| echo: false
#| fig-width: 4
#| fig-height: 4
#| out-width: 100%
#| label: fig-ft-cl8-pc
#| fig-cap: PC1 and PC5 together mostly reveal cluster 8.
ft_pc_cl8 


#| eval: false
#| echo: false
## library(mulgar)
## library(tourr)
## data(bushfires)
## 
## bushfires_sub <- bushfires[,c(5, 8:45, 48:55, 57:60)] %>%
##   mutate(cause = factor(cause))
## 
## # Checking the dependencies between predictors
## bushfires_pca <- prcomp(bushfires_sub[,-51],
##                         scale=TRUE, retx=TRUE)
## ggscree(bushfires_pca)
## 
## bushfires_pcs <- bushfires_pca$x[,1:7] %>%
##   as_tibble() %>%
##   mutate(cause = factor(bushfires$cause))
## 
## library(tourr)
## animate_xy(bushfires_pcs[,1:7],
##            guided_tour(lda_pp(bushfires_pcs$cause)),
##            col=bushfires_pcs$cause)
## 
## bushfires_pca$rotation[,2]
## ggplot(bushfires, aes(x=FOR_CODE)) + geom_density()
## ggplot(bushfires, aes(x=COVER)) + geom_density()
## ggplot(bushfires, aes(x=HEIGHT)) + geom_density()
## ggplot(bushfires, aes(x=FOREST)) + geom_density()
## ggplot(bushfires, aes(x=arf28)) + geom_density()
## 
## library(randomForest)
## bushfires_rf <- randomForest(cause~.,
##                              data=bushfires_sub,
##                              importance=TRUE)
## bushfires_rf
## 
## # Create votes matrix data
## bushfires_rf_votes <- bushfires_rf$votes %>%
##   as_tibble() %>%
##   mutate(cause = bushfires_sub$cause)
## 
## # Project 4D into 3D
## library(geozoo)
## proj <- t(geozoo::f_helmert(4)[-1,])
## b_rf_v_p <- as.matrix(bushfires_rf_votes[,1:4]) %*% proj
## colnames(b_rf_v_p) <- c("x1", "x2", "x3")
## b_rf_v_p <- b_rf_v_p %>%
##   as.data.frame() %>%
##   mutate(cause = bushfires_sub$cause)
## 
## # Add simplex
## simp <- simplex(p=3)
## sp <- data.frame(simp$points)
## colnames(sp) <- c("x1", "x2", "x3")
## sp$cause = ""
## b_rf_v_p_s <- bind_rows(sp, b_rf_v_p) %>%
##   mutate(cause = factor(cause))
## labels <- c("accident" , "arson",
##                 "burning_off", "lightning",
##                 rep("", nrow(b_rf_v_p)))
## 
## # Check votes matrix
## animate_xy(bushfires_rf_votes[,1:4],
##            col=bushfires_rf_votes$cause)
## 
## # Examine votes matrix with bounding simplex
## animate_xy(b_rf_v_p_s[,1:3], col = b_rf_v_p_s$cause,
##            axes = "off", half_range = 1.3,
##            edges = as.matrix(simp$edges),
##            obs_labels = labels)
## render_gif(b_rf_v_p_s[,1:3],
##            grand_tour(),
##            display_xy(col = b_rf_v_p_s$cause,
##            axes = "off", half_range = 1.3,
##            edges = as.matrix(simp$edges),
##            obs_labels = labels),
##            gif_file="gifs/bushfires_votes.gif",
##            frames=500)
## 
## library(gt)
## bushfires_rf$importance %>%
##   as_tibble(rownames="Variable") %>%
##   rename(Accuracy=MeanDecreaseAccuracy,
##          Gini=MeanDecreaseGini) %>%
##   arrange(desc(Gini)) %>%
##   gt() %>%
##   fmt_number(columns = c(accident, arson, burning_off, lightning, Accuracy),
##              decimals = 4) %>%
##   fmt_number(columns = Gini,
##              decimals = 2)


#| eval: false
#| echo: false
## # Answer to Q3
## There are four classes: accident, arson, burning_off, lightning. It is highly imbalanced, with most observations belonging to the lightning class, fires ignited by lightning.
## 
## We can see that most of the observations lie on the face of lightning, arson and accident. The handful of the burning_off observations lie off this plane, in the direction of burning-off, so are less confused with the other three classes. This could be expected because burning off is highly regulated, and tends to occur before the bushfire season is at risk of starting. The arson cases are hard to classify, frequently confused with lightning or accident, and occasionally burning off. Lightning and accident have many more observations that are confidently classified correctly.


#| eval: false
#| echo: false
## library(mulgar)
## library(dplyr)
## data("sketches_train")
## sketches_pca <- prcomp(sketches_train[,1:784])
## ggscree(sketches_pca, q=25, guide=FALSE)
## sketches_pc <- as.data.frame(sketches_pca$x[,1:21])
## sketches_pc$word <- sketches_train$word
## 
## library(tourr)
## animate_xy(sketches_pc[,1:6],
##            tour=guided_tour(lda_pp(sketches_pc$word)),
##            col=sketches_pc$word)
## library(randomForest)
## sketches_rf <- randomForest(word~., data=sketches_pc,
##                             mtry=5, ntree=2500,
##                             importance=TRUE)
## sketches_rf$importance
## # This would be a good one to explain how to explore multiclass
## # Difference PCs are more important for some classes
## # Create new binary classes to explore.
## 
## sketches_rf_votes <- sketches_rf$votes %>%
##   as_tibble() %>%
##   mutate(word = sketches_train$word)
## 
## proj <- t(geozoo::f_helmert(6)[-1,])
## s_rf_v_p <- as.matrix(sketches_rf_votes[,1:6]) %*% proj
## colnames(s_rf_v_p) <- c("x1", "x2", "x3", "x4", "x5")
## s_rf_v_p <- s_rf_v_p %>%
##   as.data.frame() %>%
##   mutate(word = sketches_train$word)
## 
## simp <- geozoo::simplex(p=5)
## sp <- data.frame(simp$points)
## colnames(sp) <- c("x1", "x2", "x3", "x4", "x5")
## sp$word = ""
## s_rf_v_p_s <- bind_rows(sp, s_rf_v_p) %>%
##   mutate(word = factor(word))
## labels <- c("banana" , "boomerang",
##                 "cactus", "crab", "flip flops", "kangaroo",
##                 rep("", 5998))
## animate_xy(s_rf_v_p_s[,1:5], col = s_rf_v_p_s$word,
##            axes = "off", half_range = 0.8,
##            edges = as.matrix(simp$edges),
##            obs_labels = labels)
## 
## render_gif(s_rf_v_p_s[,1:5],
##            grand_tour(),
##            display_xy(col = s_rf_v_p_s$cause,
##            axes = "off", half_range = 0.8,
##            edges = as.matrix(simp$edges),
##            obs_labels = labels),
##            gif_file="gifs/sketches_votes.gif",
##            frames=500)

