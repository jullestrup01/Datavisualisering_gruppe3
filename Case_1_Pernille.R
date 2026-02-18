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

#----------------------------------------------------
# Case 2
#----------------------------------------------------

pacman::p_load("tidyverse")
             
vffkort <- read_rds("vffkort01.rds")              

str(vffkort)

vffkort_renset <- vffkort |> 
  mutate(
    modstander = str_split_fixed(hold, "-", 2)[,2]
  ) |> 
  rename(aar = år)

str(vffkort_renset)


#----------------------------------------------------
# Visualiseringer: Tilskuertal gennem årene
#----------------------------------------------------

# Line plot med facetter - Udviklingen for hver modstander
ggplot(vffkort_renset, aes(x = aar, y = tilskuere)) +
  geom_line(linewidth = 0.8, alpha = 0.7, colour = "darkblue") +
  geom_point(size = 2, alpha = 0.6, colour = "darkblue") +
  facet_wrap(~modstander, scales = "fixed", ncol = 4) +
  labs(
    x = "År",
    y = "Antal tilskuere",
    title = "Tilskuertal gennem årene"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_rect(fill = "lightblue", colour = "black"),
    legend.position = "none"
  )

# Ridge plot - Hver modstander som separat "bølge" med min/max værdier på ridges
pacman::p_load("ggridges")

# Beregn min og max tilskuere for hver modstander + hvilket år det skete
modstander_stats <- vffkort_renset |> 
  group_by(modstander) |> 
  summarise(
    min_tilskuere = min(tilskuere, na.rm = TRUE),
    max_tilskuere = max(tilskuere, na.rm = TRUE),
    aar_min = aar[which.min(tilskuere)][1],
    aar_max = aar[which.max(tilskuere)][1],
    .groups = "drop"
  )

ggplot(vffkort_renset, aes(x = aar, y = modstander, height = tilskuere, fill = modstander)) +
  geom_density_ridges(stat = "identity", alpha = 0.7, scale = 2.5) +
  geom_text(data = modstander_stats, aes(x = aar_min, y = modstander, label = paste0("Min: ", min_tilskuere)), 
            vjust = -1.2, size = 2.5, inherit.aes = FALSE, fontface = "bold", colour = "darkblue") +
  geom_text(data = modstander_stats, aes(x = aar_max, y = modstander, label = paste0("Max: ", max_tilskuere)), 
            vjust = -1.2, size = 2.5, inherit.aes = FALSE, fontface = "bold", colour = "darkred") +
  labs(
    x = "År",
    y = "Modstander",
    title = "Tilskuertal gennem årene - Ridge plot"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Bar + line plot - Total vs Gennemsnit
vffkort_summary <- vffkort_renset |> 
  group_by(aar) |> 
  summarise(
    total_tilskuere = sum(tilskuere, na.rm = TRUE),
    antal_kampe = n(),
    middeltal = mean(tilskuere, na.rm = TRUE),
    min_tilskuere = min(tilskuere, na.rm = TRUE),
    max_tilskuere = max(tilskuere, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(vffkort_summary, aes(x = aar)) +
  geom_col(aes(y = total_tilskuere), fill = "lightblue", alpha = 0.7, width = 0.6) +
  geom_text(aes(y = total_tilskuere, label = format(total_tilskuere, big.mark = ".", decimal.mark = ",")), vjust = -0.5, size = 3) +
  geom_text(aes(y = total_tilskuere * 0.5, label = paste0("Min: ", min_tilskuere, " | Max: ", max_tilskuere)), 
            vjust = 0.5, size = 2.5, colour = "darkblue", fontface = "italic") +
  geom_line(aes(y = middeltal), colour = "darkred", linewidth = 1.2, group = 1) +
  geom_point(aes(y = middeltal), colour = "darkred", size = 3) +
  geom_text(aes(y = middeltal, label = format(round(middeltal), big.mark = "")), 
            colour = "darkred", vjust = -0.8, size = 3, fontface = "bold") +
  scale_x_continuous(
    name = "År",
    breaks = unique(vffkort_summary$aar)
  ) +
  scale_y_continuous(
    name = "Total tilskuere",
    sec.axis = sec_axis(~.*100, name = "Gennemsnitligt tilskuertal")
  ) +
  labs(
    title = "Tilskuertal gennem årene - Total vs Gennemsnit",
    subtitle = "Blå søjler = total tilskuere (fra venstre akse), Rød linje = gennemsnitligt tilskuertal (fra højre akse),\nKursiv tekst = laveste og højeste tilskuertal for en enkelt kamp per år"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_blank(),
    axis.title.y = element_text(colour = "lightblue"),
    axis.title.y.right = element_text(colour = "darkred")
  )

# Bar + line plot på modstander niveau - Total vs Gennemsnit
# Vælg modstander her:
valgt_modstander <- "BIF"

modstander_summary <- vffkort_renset |> 
  filter(modstander == valgt_modstander) |> 
  group_by(aar) |> 
  summarise(
    total_tilskuere = sum(tilskuere, na.rm = TRUE),
    antal_kampe = n(),
    middeltal = mean(tilskuere, na.rm = TRUE),
    min_tilskuere = min(tilskuere, na.rm = TRUE),
    max_tilskuere = max(tilskuere, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(modstander_summary, aes(x = aar)) +
  geom_col(aes(y = total_tilskuere), fill = "lightblue", alpha = 0.7, width = 0.6) +
  geom_text(aes(y = total_tilskuere, label = format(total_tilskuere, big.mark = ".", decimal.mark = ",")), vjust = -0.5, size = 3) +
  geom_text(aes(y = total_tilskuere * 0.5, label = paste0("Min: ", min_tilskuere, " | Max: ", max_tilskuere)), 
            vjust = 0.5, size = 2.5, colour = "darkblue", fontface = "italic") +
  geom_line(aes(y = middeltal), colour = "darkred", linewidth = 1.2, group = 1) +
  geom_point(aes(y = middeltal), colour = "darkred", size = 3) +
  geom_text(aes(y = middeltal, label = format(round(middeltal), big.mark = "")), 
            colour = "darkred", vjust = -0.8, size = 3, fontface = "bold") +
  scale_x_continuous(
    name = "År",
    breaks = unique(modstander_summary$aar)
  ) +
  scale_y_continuous(
    name = "Total tilskuere",
    sec.axis = sec_axis(~.*100, name = "Gennemsnitligt tilskuertal")
  ) +
  labs(
    title = paste0("Tilskuertal gennem årene mod ", valgt_modstander, " - Total vs Gennemsnit"),
    subtitle = "Blå søjler = total tilskuere (fra venstre akse), Rød linje = gennemsnitligt tilskuertal (fra højre akse),\nKursiv tekst = laveste og højeste tilskuertal for en enkelt kamp per år"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_blank(),
    axis.title.y = element_text(colour = "lightblue"),
    axis.title.y.right = element_text(colour = "darkred")
  )
