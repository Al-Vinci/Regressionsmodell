# Laddar in paket
library(ggcorrplot)
library(tidyverse)
library(skimr)
library(ggplot2)
library(caret)
library(patchwork) 
library(dplyr)
library(janitor) # För att kunna använda städade namn
library(scales)
library(lmtest)
library(car)
library(dplyr)

# Läs in CSV-filen (semikolonseparerad)
bilar_data <- read_delim("C:\\Users\\Alvin\\OneDrive\\Dokument\\Python_DS24\\R uppgift\\Bilar_data.csv", delim = ";") %>%
  clean_names()  # Städar kolumnnamn: t.ex. versaler till gemener åäö till aao etc

# Kolla strukturen 
glimpse(bilar_data)

# Första raderna 
head(bilar_data)

# Ändrar datatyp
bilar_data$dragkrok <- as.factor(bilar_data$dragkrok)
bilar_data$drift <- as.factor(bilar_data$drift)
bilar_data$marke <- as.factor(bilar_data$marke)
bilar_data$modell <- as.factor(bilar_data$modell)
bilar_data$kaross <- as.factor(bilar_data$kaross)

# Ändrar årsmodell till ålder
bilar_data <- bilar_data %>%
  mutate(alder = 2025 - my)
bilar_data_filt <- bilar_data %>%
  filter(miltal >= 100, miltal <= 20000, pris < 650000, alder < 8, batteristorlek > 20)

########################################

# Delar upp data till träning och test
set.seed(1) # Gör det reproducerbart
sample <- sample(c(TRUE, FALSE), nrow(bilar_data_filt), replace=TRUE, prob=c(0.7,0.3))
train_data  <- bilar_data_filt[sample, ]
test_data   <- bilar_data_filt[!sample, ]


# Kort statistik
skim(bilar_data_filt)
glimpse(bilar_data_filt)

# Kontroll av unika värden
bilar_data_filt %>%
  summarise(across(everything(), n_distinct))

# Kolla NA-värden. Hittade 16 som saknade pris men kollade upp priset och uppdaterade i Excel
colSums(is.na(bilar_data_filt))

# Plottar fördelningen av miltal, pris, batteristorlek och ålder
# Kolumner och färger
num_cols <- c("miltal", "pris", "batteristorlek", "alder")
num_cols <- intersect(num_cols, names(bilar_data_filt))
farger <- c("steelblue", "tomato", "forestgreen", "purple")  # valfria färger

# Skapa en lista av plots med olika färger
plots <- mapply(function(col, farg) {
  ggplot(bilar_data_filt, aes_string(x = col)) +
    geom_histogram(fill = farg, bins = 30) +
    labs(title = paste("Histogram av", col), x = col, y = "Antal bilar") +
    theme_minimal()
}, col = num_cols, farg = farger, SIMPLIFY = FALSE)

# Visa alla plots sida vid sida
wrap_plots(plots, ncol = 2)



# Fördelning av Märke, top 15
bilar_data_filt %>%
  count(marke, sort = TRUE) %>%
  slice_max(n, n = 15) %>%
  ggplot(aes(x = reorder(marke, n), y = n)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "Topp 15 bilmärken", x = "Märke", y = "Antal bilar") +
  theme_minimal()


# Fördelning av Dragkrok
bilar_data_filt %>%
  mutate(dragkrok = if_else(dragkrok == 1, "Ja", "Nej")) %>%
  count(dragkrok) %>%
  ggplot(aes(x = dragkrok, y = n, fill = dragkrok)) +
  geom_col() +
  labs(title = "Andel bilar med dragkrok", x = "", y = "Antal") +
  theme_minimal() +
  scale_fill_manual(values = c("Ja" = "steelblue", "Nej" = "grey60")) +
  theme(legend.position = "none")

# Korrelation mellan numeriska kolumner
num_vars <- bilar_data_filt %>%
  dplyr::select(where(is.numeric)) %>%
  dplyr::select(-nyckel, -my)

cor_matrix <- cor(num_vars, use = "complete.obs")
print(cor_matrix)

# Värmekarta
ggcorrplot::ggcorrplot(cor_matrix, lab = TRUE, type = "lower", colors = c("red", "white", "blue")) +
  ggtitle("Korrelationsmatris för numeriska variabler")

# Pris per märke
bilar_data_filt %>%
  filter(!is.na(pris)) %>%
  group_by(marke) %>%
  summarise(medelpris = median(pris), n = n()) %>%
  arrange(desc(n)) %>%
  slice_max(n, n = 10) %>%
  ggplot(aes(x = reorder(marke, medelpris), y = medelpris)) +
  geom_col(fill = "purple") +
  coord_flip() +
  labs(title = "Medianpris per märke (topp 10)", x = "Märke", y = "Pris (kr)") +
  scale_y_continuous(labels = label_comma(big.mark = " ")) +
  theme_minimal()

# Batteristorlek och pris
ggplot(bilar_data_filt, aes(x = batteristorlek, y = pris)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  labs(title = "Pris beroende på batteristorlek", x = "Batteristorlek (kWh)", y = "Pris (kr)") +
  scale_y_continuous(labels = label_comma(big.mark = " ")) +
  theme_minimal()

# Årsmodell och pris
ggplot(bilar_data_filt, aes(x = my, y = pris)) +
  geom_point(alpha = 0.5, color = "lightblue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  labs(title = "Pris beroende på modellår", x = "Årsmodell", y = "Pris (kr)") +
  scale_y_continuous(labels = label_comma(big.mark = " ")) +
  theme_minimal()

# Miltal och pris, hittade outliers här som berodde på felskrivning i annons
ggplot(bilar_data_filt, aes(x = miltal, y = pris)) +
  geom_point(alpha = 0.5, color = "lightgreen") +
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen") +
  labs(title = "Pris beroende på miltal", x = "Miltal", y = "Pris (kr)") +
  scale_y_continuous(labels = label_comma(big.mark = " ")) +
  scale_x_continuous(labels = label_comma(big.mark = " ")) +
  theme_minimal()

# Miltal och pris, baserad på märke modell, tex Tesla Model 3
bilar_marke <- bilar_data_filt %>%
  filter(marke == "Tesla", modell == "Model 3")

ggplot(bilar_marke, aes(x = miltal, y = pris)) +
  geom_point(alpha = 0.5, color = "lightgreen") +
  geom_smooth(se = FALSE, color = "darkgreen") +
  labs(title = "Pris beroende på miltal", x = "Miltal", y = "Pris (kr)") +
  scale_y_continuous(labels = label_comma(big.mark = " ")) +
  scale_x_continuous(labels = label_comma(big.mark = " ")) +
  theme_minimal()
