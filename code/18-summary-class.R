#| message: false
#| code-summary: "Code to fit forest"
library(randomForest)
library(dplyr)
load("data/penguins_sub.rda")

penguins_rf <- randomForest(species~.,
                             data=penguins_sub[,1:5],
                             importance=TRUE)
penguins_rf


#| code-fold: false
penguins_errors <- penguins_sub %>%
  mutate(err = ifelse(penguins_rf$predicted != penguins_rf$y, 1, 0))


#| eval: false
#| code-summary: "Code to make animated gifs"
## library(tourr)
## symbols <- c(1, 16)
## p_pch <- symbols[penguins_errors$err+1]
## p_cex <- rep(1, length(p_pch))
## p_cex[penguins_errors$err==1] <- 2
## animate_xy(penguins_errors[,1:4],
##            col=penguins_errors$species,
##            pch=p_pch, cex=p_cex)
## render_gif(penguins_errors[,1:4],
##            grand_tour(),
##            display_xy(col=penguins_errors$species,
##                       pch=p_pch, cex=p_cex),
##            gif_file="gifs/p_rf_errors.gif",
##            frames=500,
##            width=400,
##            height=400)
## 
## animate_xy(penguins_errors[,1:4],
##            guided_tour(lda_pp(penguins_errors$species)),
##            col=penguins_errors$species,
##            pch=pch)
## 
## render_gif(penguins_errors[,1:4],
##            guided_tour(lda_pp(penguins_errors$species)),
##            display_xy(col=penguins_errors$species,
##                       pch=p_pch, cex=p_cex),
##            gif_file="gifs/p_rf_errors_guided.gif",
##            frames=500,
##            width=400,
##            height=400,
##            loop=FALSE)
## 

