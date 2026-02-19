
library(readxl)
library(ggplot2)
library(ggrepel)

options(scipen = 999)

passat <- readxl::read_excel("C:/Users/Zabri/OneDrive/Desktop/Dataanalyse - Dania/Datavisualisering/Datavisualisering_gruppe3/K2 - passat.xls")

passat <- read_excel("passat.xlsx")

vwpas <- passat

ggplot(vwpas, aes(x = km_per_liter, y = price)) +
  geom_point()

ggplot(data = mpg,
       aes(x =displ,
           y =hwy,
           color =drv)) +
  geom_point()

ggplot(vwpas, aes(x = km_per_liter,
                  y = price,
                  color = dealer_type,
                  size = motor_size)) +
  geom_point(alpha = 0.7) +
  labs(title = "Pris vs brændstoføkonomi",
       subtitle = "Det er billigere at købe privat",
       x = "Kilometer pr. liter",
       y = "Pris (DKK)",
       color = "Forhandler-type",
       size = "Motorstørrelse") +
  scale_color_manual(values = c("Forhandler" = "#F8766D",
                                "Privat" = "#00BFC4")) +
  scale_size(range = c(2, 8)) +
  theme_minimal()
