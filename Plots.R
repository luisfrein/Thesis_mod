#Load packages
library(tidyverse)
library(ggtext)
library(showtext)
library(viridis)

#Show text is useful for adding different fonts
font_add_google('Roboto', family = 'Roboto')

#Forces aspect ratio. Run this before saving the plots
showtext_auto()
showtext_opts(dpi = 300)

#Import Data
model_data <- read_csv("model_data_trans.csv")

# Custom theme for the plots ----------------------------------------------
th_theme <- theme(plot.margin = margin(25, 15, 10, 15),
                  plot.title.position = "plot",
                  plot.caption.position = "plot",
                  panel.background = element_rect(color = "white",
                                                  fill = "white"),
                  plot.background = element_rect(color = "white",
                                                 fill = "white"),
                  panel.grid.major.y = element_line(color = 'darkgrey'),  
                  plot.caption = element_markdown(size = 15,
                                                  color = "black",
                                                  family = "Roboto"),
                  axis.title = element_markdown(color = "black",
                                                family = "Roboto",
                                                size = 19),
                  axis.text = element_markdown(color = "black",
                                               family = "Roboto",
                                               size = 19),
                  plot.title = element_markdown(size = 21,
                                                color = "black",
                                                family = "Roboto"),
                  plot.subtitle = element_markdown(size = 18,
                                                   family = 'Roboto'))

#Colors for the plots
red <- "#B33951"
green <- "#5FB49C"
blue <- "#355691"
gray <- "#7F7979"

# 
# # IDH plot ----------------------------------------------------------------
model_data %>%
  filter(year == 2017) -> model_data_label
# 
# model_data %>% 
#   ggplot(aes(year, idh, color = entity)) +
#   geom_line(size = 2) +
#   geom_point(aes(color = entity),
#              pch = 21, 
#              size = 4, 
#              stroke = 1.5,  
#              fill = 'white') +
#   scale_color_manual(values = c(gray,
#                                 green,
#                                 blue,
#                                 red)) +
#   geom_text(model_data_label, 
#             mapping = aes(year + .7 , idh, label = entity),
#             size = 7,
#             fontface = 'bold') +
#   scale_x_continuous(breaks = seq(2010, 2017.5, 2)) +
#   labs(x = NULL,
#        y = "Índice de Desarrollo Humano",
#        title = "**Figura 1**<br><br>Evolución del IDH (2010-2017)",
#        caption = "Elaboración Propia | Fuente: PNUD (2017)") +
#   coord_cartesian(clip = 'off') +
#   th_theme +
#   theme(axis.ticks.y = element_blank(),
#         legend.position = 'none')
# 
# ggsave("fig1.png",
#        width = 26.5,
#        height = 16.5,
#        units = "cm",
#        dpi = 320,
#        type = "cairo-png")
# 
# # gdp growth plot ---------------------------------------------------------
model_data_label[1, 5] <- 2.82 + 1.125
model_data_label[3, 5] <- 1.62 - 0.9

model_data %>%
  ggplot(aes(year, per_capita, color = entity)) +
  geom_line(size = 2) +
  geom_point(aes(color = entity),
             pch = 21,
             size = 4,
             stroke = 1.5,
             fill = 'white') +
  scale_color_manual(values = c(gray,
                                green,
                                blue,
                                red)) +
  scale_x_continuous(breaks = seq(2010, 2017.5, 2)) +
  geom_text(model_data_label,
            mapping = aes(year + .7 , per_capita, label = entity),
            size = 7,
            fontface = 'bold') +
  labs(x = NULL,
       y = "PIB per cápita",
       title = "**Figura 3**<br><br>Evolución del PIB per cápita (2010-2017)",
       caption = "Elaboración Propia | Fuente: Banco Mundial (2017a)") +
  coord_cartesian(clip = 'off') +
  th_theme +
  theme(axis.ticks.y = element_blank(),
        legend.position = 'none')

ggsave("fig3.png",
       width = 26.5,
       height = 16.5,
       units = "cm",
       dpi = 320,
       type = "cairo-png")
# 
# 
# # Internet users plot -----------------------------------------------------
# model_data %>% 
#   ggplot(aes(year, individuals_using_the_internet_of_population, color = entity)) +
#   geom_line(size = 2) +
#   geom_point(aes(color = entity),
#              pch = 21, 
#              size = 4, 
#              stroke = 1.5,  
#              fill = 'white') +
#   scale_color_manual(values = c(gray,
#                                 green,
#                                 blue,
#                                 red)) +
#   geom_text(model_data_label, 
#             mapping = aes(year + .7 , individuals_using_the_internet_of_population, label = entity),
#             size = 7,
#             fontface = 'bold') +
#   scale_x_continuous(breaks = seq(2010, 2017.5, 2)) +
#   labs(x = NULL,
#        y = "Porcentaje de Individuos con\nAcceso a Internet",
#        title = "**Figura 2**<br><br>Evolución del Porcentaje de Individuos con Acceso a Internet (2010-2017)",
#        caption = "Elaboración Propia | Fuente: Banco Mundial (2017a)") +
#   coord_cartesian(clip = 'off') +
#   th_theme +
#   theme(axis.ticks.y = element_blank(),
#         legend.position = 'none')
# 
# ggsave("fig2.png",
#        width = 26.5,
#        height = 16.5,
#        units = "cm",
#        dpi = 320,
#        type = "cairo-png")
# 
# # Enrollment rate plot ----------------------------------------------------
# model_data_label[3, 7] <- 99.6 + 0.5
# 
# model_data %>% 
#   ggplot(aes(year, enrollment_rate, color = entity)) +
#   geom_line(size = 2) +
#   geom_point(aes(color = entity),
#              pch = 21, 
#              size = 4, 
#              stroke = 1.5,  
#              fill = 'white') +
#   scale_color_manual(values = c(gray,
#                                 green,
#                                 blue,
#                                 red)) +
#   geom_text(model_data_label, 
#             mapping = aes(year + .7 , enrollment_rate, label = entity),
#             size = 7,
#             fontface = 'bold') +
#   scale_x_continuous(breaks = seq(2010, 2017.5, 2)) +
#   labs(x = NULL,
#        y = "Tasa",
#        title = "**Figura 4**<br><br>Evolución de la Tasa Neta de Matrícula en Escuelas Primarias (2010-2017)",
#        caption = "Elaboración Propia | Fuente: Banco Mundial (2017a)") +
#   coord_cartesian(clip = 'off') +
#   th_theme +
#   theme(axis.ticks.y = element_blank(),
#         legend.position = 'none')
# 
# ggsave("fig4.png",
#        width = 26.5,
#        height = 16.5,
#        units = "cm",
#        dpi = 320,
#        type = "cairo-png")
# 
# # Rule plot ---------------------------------------------------------------
# #Labels
# model_data %>% 
#   filter(year == 2017) -> model_data_label
# 
# model_data %>% 
#   ggplot(aes(year, rule, color = entity)) +
#   geom_line(size = 2) +
#   geom_point(aes(color = entity),
#              pch = 21, 
#              size = 4, 
#              stroke = 1.5,  
#              fill = 'white') +
#   scale_color_manual(values = c(gray,
#                                 green,
#                                 blue,
#                                 red)) +
#   geom_text(model_data_label, 
#             mapping = aes(year + .7 , rule, label = entity),
#             size = 7,
#             fontface = 'bold') +
#   scale_x_continuous(breaks = seq(2010, 2017.5, 2)) +
#   labs(x = NULL,
#        y = "Estado de Derecho",
#        title = "**Figura 5**<br><br>Evolución del Estado de Derecho (2010-2017)",
#        caption = "Elaboración Propia | Fuente: Banco Mundial (2017b)") +
#   coord_cartesian(clip = 'off') +
#   th_theme +
#   theme(axis.ticks.y = element_blank(),
#         legend.position = 'none')
# 
# ggsave("fig5.png",
#        width = 26.5,
#        height = 16.5,
#        units = "cm",
#        dpi = 320,
#        type = "cairo-png")
# 
# 
# # Correlation -------------------------------------------------------------
cor_data <- model_data %>% 
  select(-1, -2, -3) %>% 
  mutate(across(everything(), as.numeric)) %>% 
  rename('Coeficiente de Correlación' = 'idh',
         'CPIB' = 'per_capita',
         'ED' = 'rule',
         'TNMP' = 'enrollment_rate',
         'PHAI' = 'individuals_using_the_internet_of_population')

M <- as.data.frame(cor(cor_data)) 

M <- M %>% mutate('Variable' = row.names(M)) %>% 
  select(6, 1: 5) %>% 
  slice(-1) %>%
  rename('Grado de Correlación' = 'CPIB') %>% 
  select(-4, -5, -6)

M[1, 3] <- 'Correlación débil'
M[2, 3] <- 'Correlación moderada'
M[3, 3] <- 'Correlación fuerte'
M[4, 3] <- 'Correlación fuerte'

set_flextable_defaults(
  font.size = 10, theme_fun = theme_vanilla,
  padding = 20)

ft <- flextable(M)

ft <- add_header_lines(ft, "Magnitudes del Coeficiente de Correlación de Pearson")
ft <- add_footer_lines(ft, "Daily air quality measurements in New York, May to September 1973.")
ft <- color(ft, part = "footer", color = "#666666")
ft <- set_caption(ft, caption = "New York Air Quality Measurements")