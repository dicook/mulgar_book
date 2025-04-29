## ----echo=knitr::is_html_output()--------------------------------------------
#| code-summary: "Load libraries"
source("code/setup.R")


## ----eval=FALSE--------------------------------------------------------------
#| echo: true
# library(keras)
# tensorflow::set_random_seed(211)
# 
# # Define model
# p_nn_model <- keras_model_sequential()
# p_nn_model |>
#   layer_dense(units = 2, activation = 'relu',
#               input_shape = 4) |>
#   layer_dense(units = 3, activation = 'softmax')
# p_nn_model |> summary
# 
# loss_fn <- loss_sparse_categorical_crossentropy(
#   from_logits = TRUE)
# 
# p_nn_model |> compile(
#   optimizer = "adam",
#   loss      = loss_fn,
#   metrics   = c('accuracy')
# )


## ----eval=FALSE--------------------------------------------------------------
#| echo: false
# # tidymodels approach does not allow extracting weights
# library(keras)
# library(tidymodels)
# 
# # Define model
# p_nn_spec <-
#     mlp(hidden_units = 2,
#         activation = 'relu',
#         penalty = 0,
#         dropout = 0,
#         epochs = 500) |>
#     # This model can be used for classification or regression, so set mode
#     set_mode("classification") |>
#     set_engine("keras")
# 
# set.seed(821)
# p_split <- penguins_sub |>
#   select(bl:species) |>
#   initial_split(prop = 2/3,
#                 strata=species)
# p_train <- training(p_split)
# p_test <- testing(p_split)
# 
# set.seed(834)
# p_nn_model <- p_nn_spec |> fit(species~., p_train)
# 
# p_nn_pred <- bind_cols(
#     predict(p_nn_model, p_test),
#     predict(p_nn_model, p_test, type = "prob")
# )
# 
# p_nn_wgts <- keras::get_weights(p_nn_model, trainable=TRUE)
# 


## ----echo=knitr::is_html_output()--------------------------------------------
#| message: false
# Split the data intro training and testing
library(rsample)
load("data/penguins_sub.rda") # from mulgar book

set.seed(821)
p_split <- penguins_sub |> 
  select(bl:species) |>
  initial_split(prop = 2/3, 
                strata=species)
p_train <- training(p_split)
p_test <- testing(p_split)

# Check training and test split
p_split_check <- bind_rows(
  bind_cols(p_train, type = "train"), 
  bind_cols(p_test, type = "test")) |>
  mutate(type = factor(type))


## ----echo=knitr::is_html_output(), eval=FALSE--------------------------------
#| code-fold: true
#| code-summary: "Code to run tours"
# animate_xy(p_split_check[,1:4],
#            col=p_split_check$species,
#            pch=p_split_check$type,
#            shapeset=c(16,1))
# animate_xy(p_split_check[,1:4],
#            guided_tour(lda_pp(p_split_check$species)),
#            col=p_split_check$species,
#            pch=p_split_check$type,
#            shapeset=c(16,1))
# render_gif(p_split_check[,1:4],
#            grand_tour(),
#            display_xy(
#              col=p_split_check$species,
#              pch=p_split_check$type,
#              shapeset=c(16,1),
#              cex=1.5,
#              axes="bottomleft"),
#            gif_file="gifs/p_split.gif",
#            frames=500,
#            loop=FALSE
# )
# render_gif(p_split_check[,1:4],
#            guided_tour(lda_pp(p_split_check$species)),
#            display_xy(
#              col=p_split_check$species,
#              pch=p_split_check$type,
#              shapeset=c(16,1),
#              cex=1.5,
#              axes="bottomleft"),
#            gif_file="gifs/p_split_guided.gif",
#            frames=500,
#            loop=FALSE
# )


## ----------------------------------------------------------------------------
# Data needs to be matrix, and response needs to be numeric
p_train_x <- p_train |>
  select(bl:bm) |>
  as.matrix()
p_train_y <- p_train |> pull(species) |> as.numeric() 
p_train_y <- p_train_y-1 # Needs to be 0, 1, 2
p_test_x <- p_test |>
  select(bl:bm) |>
  as.matrix()
p_test_y <- p_test |> pull(species) |> as.numeric() 
p_test_y <- p_test_y-1 # Needs to be 0, 1, 2


## ----echo=FALSE--------------------------------------------------------------
#| message: false
library(keras)
p_nn_model <- load_model_tf("data/penguins_cnn")
p_nn_model


## ----eval=FALSE--------------------------------------------------------------
#| message: false
# # Fit model
# p_nn_fit <- p_nn_model |> keras::fit(
#   x = p_train_x,
#   y = p_train_y,
#   epochs = 200,
#   verbose = 0
# )


## ----eval=FALSE, echo=FALSE--------------------------------------------------
# # Check
# p_nn_model |> evaluate(p_test_x, p_test_y, verbose = 0)
# plot(p_nn_fit)
# 
# keras::get_weights(p_nn_model, trainable=TRUE)


## ----echo=knitr::is_html_output()--------------------------------------------
#| code-fold: true
library(keras)

# load fitted model
p_nn_model <- load_model_tf("data/penguins_cnn")


## ----------------------------------------------------------------------------
p_nn_model |> evaluate(p_test_x, p_test_y, verbose = 0)


## ----eval=FALSE--------------------------------------------------------------
# save_model_tf(p_nn_model, "data/penguins_cnn")


## ----------------------------------------------------------------------------
# Extract hidden layer model weights
p_nn_wgts <- keras::get_weights(p_nn_model, trainable=TRUE)
p_nn_wgts 


## ----------------------------------------------------------------------------
# Orthonormalise the weights to make 2D projection
p_nn_wgts_on <- tourr::orthonormalise(p_nn_wgts[[1]])
p_nn_wgts_on


## ----echo=knitr::is_html_output()--------------------------------------------
#| code-fold: false
#| label: fig-hidden-layer
#| fig-cap: "Plot of the data in the linear combinations from the two nodes in the hidden layer. The three species are clearly different, although with some overlap between all three. A main issue to notice is that there isn't a big gap between Gentoo and the other species, which we know exists in 4D based on our data exploration done in other chapters. This suggests that this fitted model is sub-optimal."
#| fig-alt: "A scatter plot of a projection showing clusters of three penguin species—Adélie (blue), Chinstrap (yellow), and Gentoo (red)—based on multiple variables. Data points are marked as either training data (open circles) or test data (filled circles). Both have x-axis 'nn1' with labels -2, -1, 0, 1, 2 and 3 and y-axis 'nn2' with labels -2, -1, 0, 1 and 2. The clusters are  different but not quite separated. There is little difference between training and test sets. A legend in the top-right corner indicates species colors, while another legend at the bottom-right differentiates between training (open circles) and test (filled circles) data."
#| fig-width: 5
#| fig-height: 4
#| out-width: 60%
# Hidden layer
p_train_m <- p_train |>
  mutate(nn1 = as.matrix(p_train[,1:4]) %*%
           as.matrix(p_nn_wgts_on[,1], ncol=1),
         nn2 = as.matrix(p_train[,1:4]) %*%
           matrix(p_nn_wgts_on[,2], ncol=1))

# Now add the test points on.
p_test_m <- p_test |>
  mutate(nn1 = as.matrix(p_test[,1:4]) %*%
           as.matrix(p_nn_wgts_on[,1], ncol=1),
         nn2 = as.matrix(p_test[,1:4]) %*%
           matrix(p_nn_wgts_on[,2], ncol=1))
p_train_m <- p_train_m |>
  mutate(set = "train")
p_test_m <- p_test_m |>
  mutate(set = "test")
p_all_m <- bind_rows(p_train_m, p_test_m)
ggplot(p_all_m, aes(x=nn1, y=nn2, 
                     colour=species, shape=set)) + 
  geom_point() +
  scale_colour_discrete_divergingx(palette="Zissou 1") +
  scale_shape_manual(values=c(16, 1)) +
  theme_minimal() +
  theme(aspect.ratio=1)


## ----echo=knitr::is_html_output()--------------------------------------------
#| message: false
#| eval: false
# # Predict training and test set
# # This looks like there is some machine dependency on the
# # saved model, so predictions are saved for loading across platforms
# p_nn_model <- load_model_tf("data/penguins_cnn")
# p_train_pred <- p_nn_model |>
#   predict(p_train_x, verbose = 0)
# p_test_pred <- p_nn_model |>
#   predict(p_test_x, verbose = 0)
# 
# save(p_train_pred, file="data/p_train_pred.rda")
# save(p_test_pred, file="data/p_test_pred.rda")


## ----------------------------------------------------------------------------
# Predict training and test set
load("data/p_train_pred.rda")
p_train_pred_cat <- levels(p_train$species)[
  apply(p_train_pred, 1,
        which.max)]
p_train_pred_cat <- factor(
  p_train_pred_cat,
  levels=levels(p_train$species))
table(p_train$species, p_train_pred_cat)

load("data/p_test_pred.rda")
p_test_pred_cat <- levels(p_test$species)[
  apply(p_test_pred, 1, 
        which.max)]
p_test_pred_cat <- factor(
  p_test_pred_cat,
  levels=levels(p_test$species))
table(p_test$species, p_test_pred_cat)


## ----echo=FALSE, eval=FALSE--------------------------------------------------
# # predict() causes the problem, use p_nn_model(p_test_x) instead


## ----echo=knitr::is_html_output()--------------------------------------------
#| code-fold: true
# Set up the data to make the ternary diagram
# Join data sets
colnames(p_train_pred) <- c("Adelie", "Chinstrap", "Gentoo")
colnames(p_test_pred) <- c("Adelie", "Chinstrap", "Gentoo")
p_train_pred <- as_tibble(p_train_pred)
p_train_m <- p_train_m |>
  mutate(pspecies = p_train_pred_cat) |>
  bind_cols(p_train_pred) |>
  mutate(set = "train")
p_test_pred <- as_tibble(p_test_pred)
p_test_m <- p_test_m |>
  mutate(pspecies = p_test_pred_cat) |>
  bind_cols(p_test_pred) |>
  mutate(set = "test")
p_all_m <- bind_rows(p_train_m, p_test_m)

# Add simplex to make ternary
proj <- t(geozoo::f_helmert(3)[-1,])
p_nn_v_p <- as.matrix(p_all_m[,c("Adelie", "Chinstrap", "Gentoo")]) %*% proj
colnames(p_nn_v_p) <- c("x1", "x2")
p_nn_v_p <- p_nn_v_p |>
  as.data.frame() |>
  mutate(species = p_all_m$species,
         set = p_all_m$set)

simp <- geozoo::simplex(p=2)
sp <- data.frame(cbind(simp$points), simp$points[c(2,3,1),])
colnames(sp) <- c("x1", "x2", "x3", "x4")
sp$species = sort(unique(penguins_sub$species))


## ----echo=knitr::is_html_output()--------------------------------------------
#| code-fold: true
#| fig-width: 4
#| fig-height: 3
#| out-width: 70%
#| fig-cap: "Ternary diagram for the three groups of the predictive probabilities of both training and test sets. From what we already know about the penguins data this fit is not good. Both Chinstrap and Gentoo penguins are confused with Adelie, or at risk of it. Gentoo is very well-separated from the other two species when several variables are used, and this fitted model is blind to it. One useful finding is that there is little difference between training and test sets, so the model has not been over-fitted."
#| fig-alt: "A scatterplot where all points fall inside a 2D triangle, marked by black lines. The triangle is upside down and is equilateral. Each vertex is labelled: Chinstrap (top left), Adelie (top right), Gentoo (bottom left). Each points corresponds to a penguin, and is coloured by species. The legend at right shows the color labelling as blue (Adelie), yellow (Chinstrap), red (Gentoo), and shape labels as open circles (training) and filled circles (test). Chinstrap points are a tight cluster at their vertex. Adelie and Gentoo are not centered at the vertex but part way along towards the others vertex. This indicates the confusion between these two species. There are some yellow points mixed with blue points but not with red."
# Plot it
ggplot() +
  geom_segment(data=sp, aes(x=x1, y=x2, xend=x3, yend=x4)) +
  geom_text(data=sp, aes(x=x1, y=x2, label=species),
            nudge_x=c(-0.1, 0.15, 0),
            nudge_y=c(0.05, 0.05, -0.05)) +
  geom_point(data=p_nn_v_p, aes(x=x1, y=x2, 
                                colour=species,
                                shape=set), 
             size=2, alpha=0.5) +
  scale_color_discrete_divergingx(palette="Zissou 1") +
  scale_shape_manual(values=c(19, 1)) +
  theme_map() +
  theme(aspect.ratio=1, legend.position = "right")


## ----------------------------------------------------------------------------
library(keras)

# download the data
fashion_mnist <- dataset_fashion_mnist()

# split into input variables and response
c(train_images, train_labels) %<-% fashion_mnist$train
c(test_images, test_labels) %<-% fashion_mnist$test

# for interpretation we also define the category names
class_names = c('T-shirt/top',
                'Trouser',
                'Pullover',
                'Dress',
                'Coat',
                'Sandal',
                'Shirt',
                'Sneaker',
                'Bag',
                'Ankle boot')

# rescaling to the range (0,1)
train_images <- train_images / 255
test_images <- test_images / 255


## ----eval=FALSE--------------------------------------------------------------
# # defining the model
# model_fashion_mnist <- keras_model_sequential()
# model_fashion_mnist |>
#   # flatten the image data into a long vector
#   layer_flatten(input_shape = c(28, 28)) |>
#   # hidden layer with 128 units
#   layer_dense(units = 128, activation = 'relu') |>
#   # output layer for 10 categories
#   layer_dense(units = 10, activation = 'softmax')
# 
# model_fashion_mnist |> compile(
#   optimizer = 'adam',
#   loss = 'sparse_categorical_crossentropy',
#   metrics = c('accuracy')
# )
# 
# # fitting the model, if we did not know the model yet we
# # would add a validation split to diagnose the training
# model_fashion_mnist |> fit(train_images,
#               train_labels,
#               epochs = 5)
# save_model_tf(model_fashion_mnist, "data/fashion_nn")


## ----------------------------------------------------------------------------
#| warning: false
# get the fitted model
model_fashion_mnist <- load_model_tf("data/fashion_nn")
# observed response labels in the test set
test_tags <- factor(class_names[test_labels + 1],
                    levels = class_names)

# calculate activation for the hidden layer, this can be done
# within the keras framework
activations_model_fashion <- keras_model(
  inputs = model_fashion_mnist$input,
  outputs = model_fashion_mnist$layers[[2]]$output
)
activations_fashion <- predict(
  activations_model_fashion,
  test_images, verbose = 0)

# PCA for activations
activations_pca <- prcomp(activations_fashion)
activations_pc <- as.data.frame(activations_pca$x)

# PCA on the original data
# we first need to flatten the image input
test_images_flat <- test_images
dim(test_images_flat) <- c(nrow(test_images_flat), 784)
images_pca <- prcomp(as.data.frame(test_images_flat))
images_pc <- as.data.frame(images_pca$x)


## ----echo=knitr::is_html_output(), fig.format='png'--------------------------
#| code-fold: true
#| code-summary: "Code to run tours"
#| warning: false
#| fig-cap: "First two PCs of the original data (left) and the activations (right). The activation space has done a useful transformation of the 784 variables into a reduced dimension that reveals class clusters, because some cluster differences can be seen well in the first two PCs."
#| fig-alt: "A side-by-side comparison of two scatter plots, each showing a PC1 vs PC2 of data points color-coded by category. The left plot, titled 'Input space', represents the raw input data projected into two principal components. Data points are densely packed and overlapping, with no clear separation between categories. The right plot, titled 'Activations', shows the same data after being transformed by a neural network (e.g., through hidden layer activations). In this plot, the points are more clearly separated into distinct groups, with elongated and curved cluster structures suggesting that the network has learned to organize the data in a more class-discriminative way. Axes in both plots are labeled PC1 (x-axis) and PC2 (y-axis), and the same color scheme is used across both plots to represent consistent categories."
#| out-width: 100%
p2 <- ggplot(activations_pc,
       aes(PC1, PC2, color = test_tags)) +
  geom_point(size = 0.1) +
  ggtitle("Activations") +
  scale_color_discrete_qualitative(palette = "Dynamic") +
  theme_bw() +
  theme(legend.position = "none", aspect.ratio = 1)

p1 <- ggplot(images_pc,
       aes(PC1, PC2, color = test_tags)) +
  geom_point(size = 0.1) +
  ggtitle("Input space") +
  scale_color_discrete_qualitative(palette = "Dynamic") +
  theme_bw() +
  theme(legend.position = "none", aspect.ratio = 1)

legend_labels <- cowplot::get_legend(
  p1 + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
    guides(color = guide_legend(override.aes = list(size = 1))) 
)
# hide plotting code
cowplot::plot_grid(cowplot::plot_grid(p1, p2), legend_labels,
                   rel_heights = c(1, .3), nrow = 2)


## ----echo=knitr::is_html_output(), eval=FALSE--------------------------------
#| code-fold: true
#| code-summary: "Code to run tours"
# animate_xy(images_pc[,1:5], col = test_tags,
#         cex=0.2, palette = "Dynamic")
# animate_xy(activations_pc[,1:5], col = test_tags,
#         cex=0.2, palette = "Dynamic")
# 
# render_gif(images_pc[,1:5],
#            grand_tour(),
#            display_xy(
#              col=test_tags,
#              cex=0.2,
#              palette = "Dynamic",
#              axes="bottomleft"),
#            gif_file="gifs/fashion_images_gt.gif",
#            frames=500,
#            loop=FALSE
# )
# render_gif(activations_pc[,1:5],
#            grand_tour(),
#            display_xy(
#              col=test_tags,
#              cex=0.2,
#              palette = "Dynamic",
#              axes="bottomleft"),
#            gif_file="gifs/fashion_activations_gt.gif",
#            frames=500,
#            loop=FALSE
# )


## ----------------------------------------------------------------------------
fashion_test_pred <- predict(model_fashion_mnist,
                             test_images, verbose = 0)
fashion_test_pred_cat <- levels(test_tags)[
  apply(fashion_test_pred, 1,
        which.max)]
predicted <- factor(
  fashion_test_pred_cat,
  levels=levels(test_tags)) |>
  as.numeric() - 1
observed <- as.numeric(test_tags) -1
table(observed, predicted)


## ----echo=knitr::is_html_output(), eval=FALSE--------------------------------
#| code-fold: true
#| code-summary: "Code to visualize probabilities"
# # getting the probabilities from the output layer
# fashion_test_pred <- predict(model_fashion_mnist,
#                              test_images, verbose = 0)
# 
# # this is the same code as was used in the RF chapter
# proj <- t(geozoo::f_helmert(10)[-1,])
# f_nn_v_p <- as.matrix(fashion_test_pred) %*% proj
# colnames(f_nn_v_p) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9")
# 
# f_nn_v_p <- f_nn_v_p |>
#   as.data.frame() |>
#   mutate(class = test_tags)
# 
# simp <- geozoo::simplex(p=9)
# sp <- data.frame(simp$points)
# colnames(sp) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9")
# sp$class = ""
# f_nn_v_p_s <- bind_rows(sp, f_nn_v_p) |>
#   mutate(class = ifelse(class %in% c("T-shirt/top",
#                                      "Pullover",
#                                      "Shirt",
#                                      "Coat"), class, "Other")) |>
#   mutate(class = factor(class, levels=c("Other", "T-shirt/top",
#                                         "Pullover",
#                                         "Shirt",
#                                         "Coat")))
# labels <- c("0" , "1", "2", "3", "4", "5", "6", "7", "8", "9",
#                 rep("", 10000))
# 
# animate_xy(f_nn_v_p_s[,1:9], col = f_nn_v_p_s$class,
#            axes = "off", cex=0.2,
#            edges = as.matrix(simp$edges),
#            edges.width = 0.05,
#            obs_labels = labels,
#            palette = "Viridis")
# 
# render_gif(f_nn_v_p_s[,1:9],
#            grand_tour(),
#            display_xy(
#              col=f_nn_v_p_s$class,
#              cex=0.2,
#              palette = "Viridis",
#              axes="off",
#              obs_labels = labels,
#              edges = as.matrix(simp$edges),
#              edges.width = 0.05),
#            gif_file="gifs/fashion_confusion_gt.gif",
#            frames=500,
#            loop=FALSE
# )


## ----------------------------------------------------------------------------
#| eval: false
#| echo: false
# data("sketches_train")
# load("data/sketches_test_labelled.rda")
# library(keras)
# classes <- unique(sketches_train$word)
# formatData <- function(data, flat = TRUE){
#   # extracting the label and convert to one-hot encoding
#   # first map to integer numbers, starting from 0
#   data_y <- sapply(data$word, function(x) which(x == classes)-1)
#   data_y <- keras::to_categorical(data_y, num_classes = 6)
#   # format predictors as matrix and normalize
#   data_x <- as.matrix(data[, 1:(ncol(data)-2)])
#   data_x = data_x / 255.
#   if(flat != TRUE){
#     dim(data_x) <- c(nrow(data), 28, 28, 1)
#   }
#   return(list(x = data_x, y = data_y))
# }
# sketches_flat <- formatData(sketches_train)
# sketches_img <- formatData(sketches_train, flat = FALSE)
# 
# # with a flat NN the validation error is large
# model_flat <- keras_model_sequential()
# model_flat |>
#   layer_dense(units = 64, activation = 'relu', input_shape = c(784)) |>
#   layer_dense(units = 32, activation = 'relu') |>
#   layer_dense(units = 6, activation = 'softmax')
# model_flat |> compile(
#   loss = 'categorical_crossentropy',
#   optimizer = optimizer_adam(), # could pass in learning rate here
#   metrics = c('accuracy')
# )
# history_flat <- model_flat |> fit(
#   sketches_flat$x, sketches_flat$y,
#   epochs = 100, batch_size = 128,
#   validation_split = 0.2
# )
# 
# # with quite a bit of work we can get something better (but still not great)
# data_augmentation <-
#   keras_model_sequential() |>
#   layer_random_flip("horizontal") |>
#   layer_random_rotation(0.1) |>
#   layer_random_zoom(0.1)
# 
# library(tfdatasets)
# 
# # Create a tf_dataset pipeline of augmented images (and their labels)
# train_dataset <- tensor_slices_dataset(list(sketches_img$x, sketches_img$y)) |>
#   dataset_batch(32) |>
#   dataset_map( ~ list(data_augmentation(.x), .y)) # see ?purrr::map to learn about ~ notation
# 
# model_cnn <- keras_model_sequential()
# model_cnn |>
#   layer_conv_2d(32, kernel_size= c(3, 3),
#                 activation='relu',
#                 input_shape = dim(sketches_img$x)[-1]) |>
#   layer_max_pooling_2d(2, 2) |> #
#   layer_batch_normalization() |>
#   layer_conv_2d(16, c(3, 3), activation='relu') |>
#   layer_max_pooling_2d(2, 2) |>
#   layer_batch_normalization() |>
#   layer_flatten() |>
#   layer_dense(128, activation='relu') |>
#   layer_dropout(0.2) |>
#   layer_dense(6, activation='softmax') |>
#   compile(
#     loss = 'categorical_crossentropy',
#     optimizer = optimizer_adam(),
#     metrics = c('accuracy')
#   )
# 
# # cannot have validation split in this setup, better to get test set
# history_cnn <- model_cnn |> fit(
#   train_dataset,
#   epochs = 200
# )
# 
# sketches_img_test <- formatData(sketches_test, flat = FALSE)
# evaluate(model_cnn, x = sketches_img_test$x, y = sketches_img_test$y)

