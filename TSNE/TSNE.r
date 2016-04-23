packages <- c('Rtsne', 'ggplot2', 'plotly','tsne')
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

library('Rtsne')
library('ggplot2')
library('plotly')
library('tsne')


wine_red <- read.csv(
  'http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv',
  header = TRUE,
  sep = ";"
)
wine_white <- read.csv(
  'http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv',
  header = TRUE,
  sep = ";"
)
set.seed(2016)
wine_red$Class <- 1
wine_white$Class <- 2
wine <- rbind(wine_red, wine_white)
features <- wine[, !names(wine) %in% c("Class")]


tsne <- Rtsne(
  as.matrix(features),
  check_duplicates = FALSE,
  perplexity = 30,
  theta = 0.5,
  dims = 2,
  verbose = TRUE
)

embedding <- as.data.frame(tsne$Y)
embedding$Class <- as.factor(wine$Class)


ggplot(embedding, aes(x = V1, y = V2, color = Class)) 
                  + geom_point(size = 1.25) + xlab('') 
                  + ylab('') 
                  + ggtitle('Wine t-SNE 2D embedding') 
                  + theme_light(base_size = 20) 
                  + theme(strip.background = element_blank(),
                      strip.text.x     = element_blank(),
                      axis.text.x      = element_blank(),
                      axis.text.y      = element_blank(),
                      axis.ticks       = element_blank(),
                      axis.line        = element_blank(),
                      panel.border     = element_blank()
)

embedding$Class <- factor(embedding$Class, labels = c('Red Wine', 'White Wine'))

ax <- list(title = ""
           ,zeroline = FALSE)

p <- plot_ly(
  data = embedding
  ,x = V1
  ,y = V2
  ,color = Class
  ,type = "scattergl"
  ,mode = 'markers'
  ,marker = list(line = list(width = 2))
  ,colors = c("#e5f5f9", "#dd1c77")
  ,symbols = c("cross", "square", "triangle-down")
) %>% layout(xaxis = ax, yaxis = ax)
