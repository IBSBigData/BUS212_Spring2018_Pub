# big cities kmeans Horton et al
library(mdsr)

str(WorldCities)

BigCities <- WorldCities %>%
     arrange(desc(population)) %>%
     head(4000) %>%
     select(longitude, latitude)
glimpse(BigCities)

set.seed(15)
library(mclust)
city_clusts <- BigCities %>%
     kmeans(centers = 6) %>%
     fitted("classes") %>%
     as.character()

BigCities <- BigCities %>% mutate(cluster = city_clusts)
BigCities %>% ggplot(aes(x = longitude, y = latitude)) +
     geom_point(aes(color = cluster), alpha = 0.5)
