# Case 1 - Pernille

pacman::p_load("ggplot2", "readxl", "ggrepel")

options(scipen = 999)

passat <- readxl::read_excel("passat.xlsx")

str(passat)

ggplot(passat, aes(x = km_per_liter,
                   y = price,
                   size = motor_size,
                   shape = dealer_type)) +
  geom_point(alpha = 0.2) +
  geom_point(
    data = subset(passat, motor_size >1.5 & year > 2014),
    colour = "red",
    size = 3,
    alpha = 0.8
  ) +
  geom_text_repel(
    data = subset(passat, motor_size > 1.5 & year > 2014),
    aes(label = car),
    size = 2,
    max.overlaps = 10,
    colour = "black"
  ) +
  labs(
    x = "Kilometer pr. liter",
    y = "Pris (DKK)",
    title = "Pris vs brændstoføkonomi",
    subtitle = "De røde punkter er biler produceret efter 2014",
    colour = "Forhandler-type",
    size = "Motorstørrelse"
  ) +
  theme_bw()

             
               


