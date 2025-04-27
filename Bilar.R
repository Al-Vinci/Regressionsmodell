# Laddar in paket
library(ggcorrplot)
library(tidyverse)
library(skimr)
library(ggplot2)
library(caret)
library(dplyr)
library(janitor) # För att kunna använda städade namn
library(scales)
library(lmtest)
library(car)
library(broom)
library(sandwich)


# Läs in CSV-filen (semikolonseparerad)
bilar_data <- read_delim("C:\\Users\\Alvin\\OneDrive\\Dokument\\Python_DS24\\R uppgift\\Bilar_data.csv", delim = ";") %>%
  clean_names()  # Städar kolumnnamn: t.ex. versaler till gemener åäö till aao etc

# Ändrar datatyp
bilar_data$dragkrok <- as.factor(bilar_data$dragkrok)
bilar_data$drift <- as.factor(bilar_data$drift)
bilar_data$marke <- as.factor(bilar_data$marke)
bilar_data$modell <- as.factor(bilar_data$modell)
bilar_data$kaross <- as.factor(bilar_data$kaross)

# Ändrar årsmodell till ålder
bilar_data <- bilar_data %>%
  mutate(alder = 2025 - my)

#Filtrerar datan
bilar_data_filt <- bilar_data %>%
  filter(miltal >= 100, miltal <= 20000, pris < 650000, alder < 8, batteristorlek > 20)
#glimpse(bilar_data_filt)

# Delar upp data till träning och test
set.seed(1) # Gör det reproducerbart
sample <- sample(c(TRUE, FALSE), nrow(bilar_data_filt), replace=TRUE, prob=c(0.7,0.3))
train_data  <- bilar_data_filt[sample, ]
test_data   <- bilar_data_filt[!sample, ]

# Test att filtrera bort okända kategorier i testdatan
# Behåll bara rader där alla faktor-nivåer finns med i träningen
for (col in c("marke", "modell")) {
  train_levels <- unique(train_data[[col]])
  test_data <- test_data[test_data[[col]] %in% train_levels, ]
}
######################################################################
# Påbörjar regressionsmodellering 
modell <- lm(pris ~ miltal+alder+kaross+marke+modell+drift+dragkrok+batteristorlek, data = train_data)
# modell <- lm(pris ~ miltal+my+kaross+marke+modell+drift+dragkrok+batteristorlek, data = train_data)

# Sammanfattar modellen
summary(modell)

# Prediktion på testdata
test_data$prediktion <- predict(modell, newdata = test_data)

# Utvärdering 
results <- postResample(pred = test_data$prediktion, obs = test_data$pris)
options(scipen = 10)
# jamfor <- results
print(results)
# print(jamfor)

# Testar regression med bara en bilmodell Tesla model 3
train_data_tesla <- train_data
test_data_tesla <- test_data

train_data_tesla <- train_data_tesla %>%
  filter(modell == "Model 3")
test_data_tesla <- test_data_tesla %>%
  filter(modell == "Model 3")
head(train_data_tesla)

modell3 <- lm(pris ~ miltal+my+drift+dragkrok+batteristorlek, data =train_data_tesla)

summary(modell3)

test_data_tesla$prediktion <- predict(modell3, newdata = test_data_tesla)

results3 <- postResample(pred = test_data_tesla$prediktion, obs = test_data_tesla$pris)
options(scipen = 10)
print(results3)

######## Kontroll ##############
# Kategorin marke visar samma saker som modell men på en högre nivå

# Teoretiska antaganden, som bryts eller hålls
# Ickelinjäritet
par(mfrow = c(2, 2))
plot(modell, which = 1) # y-Residual (avvikelserna mellan de faktiska värdena och de förutsagda värdena) vs x-fitted (predikterade)

plot(train_data$alder, train_data$pris, xlab = "Ålder", ylab = "Pris")
plot(train_data$miltal, train_data$pris, xlab = "Miltal", ylab = "Pris")
plot(train_data$batteristorlek, train_data$pris, xlab = "Batteristorlek", ylab = "Pris")

# Punkterna bör vara mer centrerad runt 0 och inte visa en kurvformad trend.
# Detta kan antyda att en viktig icke-linjär relation mellan variablerna saknas.
# En möjlig lösning kan vara att lägga till polynom eller log-transformationer.
# Konsistenta avvikelser åt ett håll: Om punkterna konsekvent avviker över eller
# under 0-linjen i vissa områden, kan det tyda på bias i modellen – till exempel
# att vissa variabler saknas. Outliers? Miltal

# Autokorrelation
dwtest(modell)
plot(resid(modell), type = "p", ylab = "Residuals", xlab = "Index")
abline(h = 0, col = "red")

# Heteroskedasticitet/Homoskedasticitet 
plot(modell, which = 3)
bptest(modell)
# Det verkar finnas mönster
# Lågt p-värde innebär problem med heteroskedacitet, logga pris och miltal

# Autokorrelation i residualer. Värden långt från 2 indikerar autokorrelation.
dwtest(modell)

# Residualernas normalfördelning. QQ plot
plot(modell, which = 2)
hist(modell$residuals, breaks = 30, main = "Histogram över residualer", xlab = "Residualer", col = "steelblue")

######## Förbättringar ##############
 
# Multikollinjaritet
cor(train_data[, c("miltal", "my", "batteristorlek")], use = "complete.obs")

vif(modell) # Variance Inflation Factor (VIF),
# Går inte att köra, verkar tyda på aliasing troligen modell och märke?

# ----Modifierar datan-----
# Transformerar variabler
bilar_data_filt$log_pris <- log(bilar_data_filt$pris)
bilar_data_filt$log_miltal <- log(bilar_data_filt$miltal)

# Dela upp i träning och test
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(bilar_data_filt), replace=TRUE, prob=c(0.7,0.3))
train_data_nya  <- bilar_data_filt[sample, ]
test_data_nya   <- bilar_data_filt[!sample, ]


#### OBS #### Här verkar jag råkat ta bort kod för modellering när jag städade koden.
# Jag är osäker på vart jag var i processen och vad som är borttaget, men vissa plottar är
# är försvunna gentemot vad jag visar i arbetet. Återskapade en modell för att kunna köra
# vidare, men är osäker på om det egentligen blev rätt utifrån processen. Rensade efter projektets slut.
modell_nya <- lm(log_pris ~ log_miltal+poly(alder, 2)+kaross+modell+drift+dragkrok+batteristorlek, data = train_data_nya)
summary(modell_nya)
test_data_nya$prediktion <- predict(modell_nya, newdata = test_data_nya)
results_nya <- postResample(pred = test_data_nya$prediktion, obs = test_data_nya$pris)
options(scipen = 10)
print(results_nya)
###################################


# Hitta och eventuellt exkludera outliers och high leverage

# Kollar Cook’s distance
plot(modell_nya, which = 4)

# Ta ut observationer med hög Cook’s distance
cook_d <- cooks.distance(modell_nya)
cutoff <- 4 / nrow(train_data_nya)

# Index på rader med för högt inflytande
influential_points <- which(cook_d > cutoff)

# Antal punkter som påverkar mycket
length(influential_points)

# Skapa ny version av träningsdatan utan dem
train_data_clean <- train_data_nya[-influential_points, ]

# Filtrerar bort okända från modell och  i testdata
for (col in c("modell")) {
  train_levels_clean <- unique(train_data_clean[[col]])
  test_data_nya <- test_data_nya[test_data_nya[[col]] %in% train_levels_clean, ]}

# Träna om modellen
modell_clean <- lm(log_pris ~ log_miltal + poly(alder,2) + modell + drift + dragkrok + batteristorlek,
                   data = train_data_clean)

summary(modell_clean)

test_data_nya$prediktion <- predict(modell_clean, newdata = test_data_nya)

# Utvärdering 
results_clean <- postResample(pred = exp(test_data_nya$prediktion), obs = test_data_nya$pris)
print(results_clean)

# Normalitet i residualer
plot(modell_clean, 1)

# Testar om multikollinaritet är ett problem.
vif(modell_clean)
alias(modell_clean)

# Homoskedasticitet
bptest(modell_clean)

augment(modell_clean) %>%
  ggplot(aes(sample = .std.resid)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q-plot av standardiserade residualer")


########## Modell där jag tar bort outliers, bättre QQ men sämre RMSE och MAE

# Lägg till residualer
train_data_clean$residuals <- rstandard(modell_clean)

# Ta bort outliers
train_data_no_outliers <- train_data_clean %>%
  filter(abs(residuals) <= 3)

for (col in c("modell")) {
  train_levels_out <- unique(train_data_no_outliers[[col]])
  test_data_nya <- test_data_nya[test_data_nya[[col]] %in% train_levels_out, ]}
# Träna ny modell utan outliers
modell_utan_outliers <- lm(log_pris ~ log_miltal + modell + drift + dragkrok,
                         data = train_data_no_outliers)

summary(modell_utan_outliers)

# Prediktion
test_data_nya$prediktion_no_outliers <- predict(modell_utan_outliers, newdata = test_data_nya)

# Utvärdering
results_no_outliers <- postResample(pred = exp(test_data_nya$prediktion_no_outliers), obs = test_data_nya$pris)
print(results_no_outliers)


# Testar om multikollinaritet är ett problem.
vif(modell_utan_outliers)

# Homoskedasticitet
bptest(modell_utan_outliers)

#### Slutliga modellen ####


# Robust inferens för en befintlig linjär modell
coeftest(modell_utan_outliers, vcov = vcovHC(modell_utan_outliers, type = "HC1"))

tidy(coeftest(modell_utan_outliers, vcov = vcovHC(modell_utan_outliers, type = "HC1")))

# Kollar MAPE (Mean Absolute Percentage Error) för slutresultatet 
mape <- mean(abs((test_data_nya$pris - exp(test_data_nya$prediktion_no_outliers)) / test_data_nya$pris)) * 100
print(paste("MAPE:", round(mape, 2), "%"))
