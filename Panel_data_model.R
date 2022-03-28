# Load packages and import data -----------------------------------------------------------
library(tidyverse)
library(ggdist)
library(car)
library(gplots)
library(plm)
library(gtsummary)

model_data <- readr::read_csv('model_data.csv')

#Check distribution of scores per variable -----------------------------------------------------------
# model_data %>% 
#   ggplot(aes(idh)) +
#   stat_slab(aes(scale = 0.7),
#             fill = '#808A9F') +
#   stat_dotsinterval(side = "bottom", scale = 0.7, slab_size = NA,
#                     fill = '#808A9F') 
# 
# model_data %>% 
#   ggplot(aes(gdp_growth)) +
#   stat_slab(aes(scale = 0.7),
#             fill = '#808A9F') +
#   stat_dotsinterval(side = "bottom", scale = 0.7, slab_size = NA,
#                     fill = '#808A9F') 
# 
# model_data %>% 
#   ggplot(aes(rule)) +
#   stat_slab(aes(scale = 0.7),
#             fill = '#808A9F') +
#   stat_dotsinterval(side = "bottom", scale = 0.7, slab_size = NA,
#                     fill = '#808A9F')
# 
# model_data %>% 
#   ggplot(aes(enrollment_rate)) +
#   stat_slab(aes(scale = 0.7),
#             fill = '#808A9F') +
#   stat_dotsinterval(side = "bottom", scale = 0.7, slab_size = NA,
#                     fill = '#808A9F')
# 
# model_data %>% 
#   ggplot(aes(individuals_using_the_internet_of_population)) +
#   stat_slab(aes(scale = 0.7),
#             fill = '#808A9F') +
#   stat_dotsinterval(side = "bottom", scale = 0.7, slab_size = NA,
#                     fill = '#808A9F') 

# model_data %>%
#   ggplot(aes(pcgdp)) +
#   stat_slab(aes(scale = 0.7),
#             fill = '#808A9F') +
#   stat_dotsinterval(side = "bottom", scale = 0.7, slab_size = NA,
#                     fill = '#808A9F')

View(model_data)


# Look at possible heterogeneity ------------------------------------------
x11() #Opens a new window for the plot
coplot(idh ~ year|entity, 
       type="b", 
       data = model_data) 
# idh en funcion de año y por individuo
# We see different trends across the years for each country. Indicates some heterogeneity

x11()
scatterplot(idh ~ year|entity, 
            boxplots = FALSE, 
            smooth = TRUE, 
            data = model_data)
#Again we see different trends across the years for each country. Indicates some heterogeneity

# Heterogeneidad:
x11()
plotmeans(idh ~ entity, 
          main = "Heterogeneidad por País", 
          data = model_data)

plotmeans(idh ~ year, 
          main = "Heterogeneidad por Años", 
          data = model_data) 
#We could claim homogeneity if we see a completely horizontal line and really similar confidence intervals


# Fixed effects model -----------------------------------------------------
fijo_log <- plm(log(idh) ~ 
              log(individuals_using_the_internet_of_population) + 
              log(rule) + 
              log(enrollment_rate) + 
              log(gdp_growth),
            data = model_data,
            index = c ("entity","year"), 
            model = "within")
#rule and gdp growth have negative values. Using log() introduces NAs.
#That might reduce the number of observations, thus giving fewer than 30 obs for the model.

fijo <- plm(idh ~ 
              individuals_using_the_internet_of_population + 
              rule + 
              enrollment_rate + 
              gdp_growth,
            data = model_data,
            index = c ("entity","year"), 
            model = "within")

#Model results
broom::tidy(fijo) %>%
  dplyr::mutate_if(is.numeric, round, 5)
#estimates might be too low
#individuals_using_internet is significant for a p-value < 0.05
#rule is not significant 
#enrollment_rate is significant for a p-value < 0.1
#gdp_growth is significant for a p-value < 0.1

broom::glance(fijo) %>%
  dplyr::mutate_if(is.numeric, round, 5)
#F statistic. Model is significant for a p-value < 0.05

broom::tidy(fijo_log) %>%
  dplyr::mutate_if(is.numeric, round, 5)
#Only individuals_using_internet is significant for a p-value < 0.05

broom::glance(fijo_log) %>%
  dplyr::mutate_if(is.numeric, round, 5)
#F statistic. Model is significant for a p-value < 0.05

#Non tidy model summaries
summary(fijo_log)
summary(fijo)


# Random effects model ----------------------------------------------------
aleatorio_log <- plm(log(idh) ~ 
                    log(individuals_using_the_internet_of_population) + 
                    log(rule) + 
                    log(enrollment_rate) + 
                    log(gdp_growth),
                  data = model_data,
                  index = c("entity","year"),
                  model = "random",
                  random.method = 'amemiya')
#rule and gdp growth have negative values. Using log() introduces NAs.
#That might reduce the number of observations, thus giving fewer than 30 obs for the model.

aleatorio <- plm(idh ~ 
                   individuals_using_the_internet_of_population + 
                    rule + 
                    enrollment_rate + 
                    gdp_growth,
                  data = model_data,
                  index = c("entity","year"),
                  model = "random",
                 random.method = 'amemiya')

#Model results
broom::tidy(aleatorio) %>%
  dplyr::mutate_if(is.numeric, round, 5)
#estimates might be too low
#Intercept is significant for a p-value < 0.1
#individuals_using_internet is significant for a p-value < 0.05
#rule is not significant 
#enrollment_rate is significant for a p-value < 0.1
#gdp_growth is significant for a p-value < 0.1

broom::glance(aleatorio) %>%
  dplyr::mutate_if(is.numeric, round, 5)
#F statistic. Model is significant for a p-value < 0.05

broom::tidy(aleatorio_log) %>%
  dplyr::mutate_if(is.numeric, round, 5)
#better estimates 
#Intercept is significant for a p-value < 0.05. It is negative now though
#individuals_using_internet is significant for a p-value < 0.05
#rule is not significant 
#enrollment_rate is not significant 
#gdp_growth is not significant 

broom::glance(aleatorio_log) %>%
  dplyr::mutate_if(is.numeric, round, 5)
#F statistic. Model is significant for a p-value < 0.05

#Non tidy model results
summary(aleatorio_log)
summary(aleatorio)


# Hausman test ------------------------------------------------------------
phtest(fijo,aleatorio)
#Should use random effects model

phtest(fijo_log, aleatorio_log)
#Should use random effects

#el p-value es mayor a 5%, no se rechaza la Ho o rechazas la Ha,

#si mi p-value es menor al 5%, entonces se deberia usar efectos fijos
