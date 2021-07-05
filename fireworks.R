library(tidyverse)
library(ggplot2)
library(gganimate)

# initial code from https://www.bigger-tree.org/2018/12/22/r-fireworks/

# creates a dataframe for firework points and establishes bounds of plot
df <- data.frame(x = runif(100000),
                 y= runif(100000))
xmax <- max(df$x)
xmin <- min(df$x)
ymax <- max(df$y)
ymin <- min(df$y)


## Loop for n fireworks
# initialize fireworks, mean time of fireworks and groups
fireworks <- list()
mean_t <- 1
begin_gr <- 400

for (i in 1:30){
  
  mean_t <- mean_t + 2
  
  ## random amount of clusters 1 - 10 
  nclust <- runif(1, min = 1, max = 10)
  
  ## random time intervals for fireworks to display
  
  t <- data_frame(t = round(rnorm(n = nclust, mean = mean_t, sd = 0.3), 1),
                  cl = 1:nclust)
  
  df_s <- data.frame(x = sample(df$x, 150),
                     y = sample(df$y, 150))
  
  
  ## all points to different groups for the animation
  begin_gr <- begin_gr + nrow(df_s) + 1
  end_gr <- begin_gr + nrow(df_s) - 1
  
  # kmeans cluster for each firework
  km <- kmeans(df_s, nclust, iter.max = 50)
  
  cluster_info <- data.frame(cl = km$cluster)
  
  fw  <-  df_s %>% 
    bind_cols(cluster_info) %>% 
    arrange(cl) %>% 
    mutate(gr = begin_gr:end_gr)
  
  df_cl <- fw %>% 
    group_by(cl) %>% 
    summarise(nr = n())
  
  ntimes <- df_cl$nr
  
  ## starting position from middle of each cluster
  centre_fw <- data.frame(x = rep(km$centers[,1], ntimes),
                          y = rep(km$centers[,2], ntimes),
                          cl = rep(1:nrow(km$centers), ntimes),
                          gr = begin_gr:end_gr)
  
  fireworks[[i]] <- fw %>% 
    left_join(t, by = "cl") %>% 
    ## add starting position with t - 0.5
    bind_rows(centre_fw %>% left_join(t, by = "cl") %>% mutate(t = t-0.5)) %>%
    mutate(cl = as.factor(cl))
}

fireworks <- bind_rows(fireworks)

# animates the fireworks
ani <-fireworks %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_point(data = fireworks %>% filter(cl == 1), aes(group = gr, col = cl), size = 2) +
  geom_point(data = fireworks %>% filter(cl == 2), aes(group = gr, col = cl, shape = cl), size = 2) +
  geom_point(data = fireworks %>% filter(cl == 3), aes(group = gr, col = cl, shape = cl), size = 2) +
  geom_point(data = fireworks %>% filter(cl == 4), aes(group = gr, col = cl, shape = cl), size = 2) +
  geom_point(data = fireworks %>% filter(cl == 5), aes(group = gr, col = cl, shape = cl), size = 2) +
  xlab( "Happy 4th of July!") +
  theme(axis.line = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = "black", colour = "black"), # to simulate night sky
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        axis.title.x = element_text(face = "bold", colour = "red", size = 18)) +
  scale_x_continuous(limits = c(xmin, xmax)) +
  scale_y_continuous(limits = c(ymin, ymax)) +
  ## gganimate options
  transition_time(t) +
  shadow_wake(wake_length = 0.01, alpha = FALSE)+ # to simulate diffraction
  ease_aes("quadratic-out") +
  exit_fade()

animate(ani, nframes = 700)
anim_save("fireworks.gif")
