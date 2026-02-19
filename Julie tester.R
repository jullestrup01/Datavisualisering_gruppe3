# Case 1 ------------------------------------------------------------------


library(readxl)
library(ggplot2)
library(ggrepel)
library(tidyverse)

passat <- read_excel("passat.xlsx")
View(passat)
names(passat)

options(scipen = 999)

ggplot(data = passat) + 
      aes(x = km_per_liter, y = price, size = motor_size, colour = dealer_type) + 
      geom_point(alpha =0.3) + 
      labs(x = "Kilometer pr. liter", y = "Pris",
            title = "Du spare penge ved at købe din bil af en privat",
            subtitle = "Forhandlere er i gennemsnit X procent dyrer end privat",
            caption = "Priser på biler privat køb vs. forhandler" ) +
      theme_classic() + 
  ggrepel::geom_text_repel(
    data = transform(passat, label = ifelse(motor_size > 1.5 & year > 2014, car, "")),
    aes(label = label),
    show.legend = FALSE  
  )


# Case 2 ------------------------------------------------------------------

vffkort01 <- readRDS("C:/Users/julie/OneDrive/Dataanalyse studie/Datavisualisering/Github/Datavisualisering_gruppe3/vffkort01.rds")
view(vffkort01)

trend_data <- vffkort01 |> 
  group_by(år) |> 
  summarise(
    gennemsnit_tilskuere = mean(tilskuere, na.rm = TRUE),
    total_tilskuere = sum(tilskuere, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(år)

ggplot(data=trend_data) + 
  aes(x = år, y = gennemsnit_tilskuere) + 
  geom_jitter(width = 0.2, alpha = 0.6, size = 1.5, color = "darkgreen") + 
  geom_smooth(method = "lm",
              se = TRUE,
              color = "#008800",
              fill = "lightgreen") +
  labs(x = "år", y = "Gennemsnitlig antal tilskuer pr. kamp",
       title = "Udvikling i antal tilskuere fra 2000 til 2025", 
       subtitle = "Generel positiv tendens i antallet af tilskuere over årene") +
  theme_classic()

              