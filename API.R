library(pxweb)
library(ggplot2)
library(dplyr)
library(stringr)
library(patchwork)

# Sätt URL till den specifika tabellen, url = antal bilar, url2 nyregistr elbilar
url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/TK/TK1001/TK1001A/PersBilarA"
url2 <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/TK/TK1001/TK1001A/PersBilarDrivMedel"

# Skapa queryn som korrekt pxweb-format.00 = Hela riket.000 = Totalt
query <- list("Region" = c("00"), "Agarkategori" = c("000"))

query2 <- list("Region" = c("00"),
  "Drivmedel" = c("120"),
  "ContentsCode" = "TK1001AA",
  "Tid" = "*")

# Hämtar info om API
# metadata <- pxweb_get(url2)
# str(metadata)

# Hämta datan
bilar_per_ar <- pxweb_get_data(
  url = url,
  query = query,
  column.name.type = "text",
  variable.value.type = "text")

elbilar_per_manad <- pxweb_get_data(
  url = url2,
  query = query2,
  column.name.type = "text",
  variable.value.type = "text")

# Visa de första raderna
# head(bilar_per_ar)
# head(elbilar_per_ar_agg)
# head(elbilar_per_ar)
# head(elbilar_per_manad)
# colnames(elbilar_per_manad)

# Lägger ihop resultatet per månad till år
elbilar_per_ar <- elbilar_per_manad %>%
  mutate(År = str_sub(månad, 1, 4)) %>%
  group_by(År) %>%
  summarise(Antal = sum(Antal, na.rm = TRUE))

# Aggregerar varje år med de innan
elbilar_per_ar_agg <- elbilar_per_manad %>%
  filter(str_detect(månad, "^\\d{4}M\\d{2}$")) %>%
  mutate(År = str_sub(månad, 1, 4)) %>%
  group_by(År) %>%
  summarise(Antal = sum(Antal, na.rm = TRUE)) %>%
  arrange(År) %>%  # sortera så cumsum blir rätt
  mutate(Ackumulerat = cumsum(Antal))
# str(elbilar_per_ar_agg)

# konverterar år till numeringskt värde
bilar_per_ar$år <- as.numeric(bilar_per_ar$år)
elbilar_per_ar_agg$År <- as.numeric(as.character(elbilar_per_ar_agg$År))

# Plottar siffrorna
ggplot(bilar_per_ar, aes(x=år, y=Antal)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(color = "darkred", size = 3) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(min(bilar_per_ar$år), max(bilar_per_ar$år), by = 2)) +
  labs(title = "Antal bilar per år", x = "År", y = "Bilar") +
  theme_minimal()

# Plottar siffrorna aggregerat
ggplot(elbilar_per_ar_agg, aes(x=År, y=Ackumulerat)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(color = "darkred", size = 3) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Antal elbilar per år", x = "År", y = "Elbilar") +
  theme_minimal()

# p1 + p2  # sida vid sida





library(dplyr)

# Se till att båda dataramarna har samma kolumnnamn och numeriska år
bilar_per_ar <- bilar_per_ar %>%
  mutate(typ = "Alla bilar", år = as.numeric(år))

elbilar_per_ar_agg <- elbilar_per_ar_agg %>%
  # rename(Antal = Ackumulerat) %>%
  mutate(typ = "Elbilar ackumulerat", år = as.numeric(År)) %>%
  select(år, Antal = Ackumulerat, typ)

# Kombinera
combined <- bind_rows(bilar_per_ar, elbilar_per_ar_agg)

# Plotta båda linjerna
library(ggplot2)

ggplot(combined, aes(x = år, y = Antal, color = typ)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = c("Alla bilar" = "steelblue", "Elbilar ackumulerat" = "darkgreen")) +
  labs(title = "Antal bilar vs ackumulerade elbilar", x = "År", y = "Antal", color = "Kategori") +
  theme_minimal()








p1 <- ggplot(bilar_per_ar, aes(x=år, y=Antal)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(color = "darkred", size = 3) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Antal bilar per år", x = "År", y = "Bilar") +
  theme_minimal()

p2 <- ggplot(elbilar_per_ar_agg, aes(x=År, y=Ackumulerat)) +
  geom_line(color = "seagreen", linewidth = 1.2) +
  geom_point(color = "darkgreen", size = 3) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Ackumulerade elbilar per år", x = "År", y = "Elbilar") +
  theme_minimal()

# Kombinera dem
p1 + p2  # sida vid sida

