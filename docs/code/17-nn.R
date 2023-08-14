#| eval: false
#| echo: false
## # Following https://parsnip.tidymodels.org/articles/Examples.html
## library(nnet)
## library(mulgar)
## library(tidymodels)
## tidymodels_prefer()
## 
## data(sketches_train)
## sketches_sub <- sketches_train %>%
##   #filter(word %in% c("crab", "flip flops")) %>%
##   #mutate(word = factor(word)) %>%
##   select(-id)
## 
## set.seed(423)
## sketches_split <- initial_split(sketches_sub)
## sk_tr <- training(sketches_split)
## sk_ts <- testing(sketches_split)
## 
## mlp_cls_spec <-
##     mlp(hidden_units = 12,
##         penalty = 0.1,
##         epochs = 1000) %>%
##     set_mode("classification") %>%
##     set_engine("nnet", MaxNWts = 10000)
## sk_wf <- workflow() %>%
##   add_model(mlp_cls_spec) %>%
##   add_formula(word ~ .)
## sk_grid <- grid_regular(dials::hidden_units(),
##                         dials::penalty(),
##                         levels=3)
## 
##   #expand_grid(hidden_units = c(6, 10, 12), epochs = c(500, 1000), penalty=c(0, 0.1))
## sk_folds <- vfold_cv(sk_tr, v = 5)
## sk_res <-
##   sk_wf %>%
##   tune_grid(
##     resamples = sk_folds,
##     grid = sk_grid
##     )
## sk_res %>%
##   collect_metrics()
## sk_res %>%
##   show_best("accuracy")
## 
## mlp_cls_fit <- mlp_cls_spec %>%
##     fit(word ~ ., data = sk_tr)
## #mlp_cls_fit
## sk_pred <- bind_cols(sk_ts,
##     predict(mlp_cls_fit, sk_ts),
##     predict(mlp_cls_fit, sk_ts, type = "prob")
##   )
## bal_accuracy(sk_pred, word, .pred_class)
## 
## sk_pred %>% count(word, .pred_class) %>%
##   pivot_wider(names_from = `.pred_class`, values_from = n)
## 
## 

