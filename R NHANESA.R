rm(list=ls())
install.packages("NHANES")
install.packages("devtools")
install.packages("nhanesA")
install.packages("arrow")
library(devtools)
require(devtools)
install_github("cjendres1/nhanes") # dependency

library(nhanesA)
library(NHANES)
library(arrow)
library(dplyr)

nhanesSearch("Cholesterol", ystart = 2013, ystop = 2018)

nhanesTables('DEMO', 2013)

nhanesTables("DIET", 2013)

# citation("NHANES")
# citation("nhanesA")

nhanes <- read_feather("C:/Users/LENOVO/Desktop/STAT 525/nhanes.feather")
head(nhanes)

nhanes$BPX <- (nhanes$BPXSY1 + nhanes$BPXDI1) / 2
nhanes$Smoke <- ifelse(nhanes$SMQ020 == 1, 10, 0)
nhanes <- nhanes %>%
  mutate(Physical = ntile(PAD680, 100))
nhanes <- nhanes %>%
  mutate(
    Diet = (
      ntile(DR1TSODI, 100) +
        ntile(DR1TSFAT, 100) +
        ntile(DR1TCHOL, 100) +
        ntile(DR1TSUGR, 100)
    ) / 4
  )
nhanes <- nhanes %>%
  mutate(Alcohol = ntile(ALQ130, 100))

filtered_data <- nhanes %>%
  filter(
    !is.na(RIDAGEYR) &
      !is.na(LBXTC) &
      !is.na(RIAGENDR) &
      !is.na(RIDRETH3) &
      !is.na(BMXBMI) &
      !is.na(LBXGLU) &
      !is.na(SLD010H) &
      !is.na(BPX) &
      !is.na(Smoke) &
      !is.na(Physical) &
      !is.na(Diet) &
      !is.na(Alcohol)
  )
ldl <- filtered_data$LBXTC
ldl

category_mapping <- c("Male" = 0, "Female" = 1)

RIAGENDR_encoded <- category_mapping[filtered_data$RIAGENDR]

category_mapping <- c(
  "Mexican American" = 0,
  "Other Hispanic" = 1,
  "Non-Hispanic White" = 2,
  "Non-Hispanic Black" = 3,
  "Non-Hispanic Asian" = 4,
  "Other Race - Including Multi-Racial" = 5
)

RIDRETH3_encoded <- category_mapping[filtered_data$RIDRETH3]

X <- as.matrix(filtered_data[, c('RIDAGEYR', 'RIAGENDR', 'RIDRETH3', 'BMXBMI', 'LBXGLU', 'SLD010H', 'BPX', 'Smoke', 'Physical', 'Diet', 'Alcohol')])

new_var <- c('RIDAGEYR', 'BMXBMI', 'LBXGLU', 'SLD010H', 'BPX', 'Smoke', 'Physical', 'Diet', 'Alcohol')
X <- cbind(RIAGENDR_encoded, RIDRETH3_encoded, filtered_data[, new_var])
X <- cbind(X, ldl)
head(X)

mod1 <- lm(ldl ~ as.factor(RIAGENDR_encoded) + as.factor(RIDRETH3_encoded) + RIDAGEYR + BMXBMI +
             LBXGLU + SLD010H + BPX + Smoke + Physical + Diet + Alcohol, data = X)
stepwise_AIC <- step(mod1, direction = "both")

stepwise_BIC <- step(mod1, direction = "both", k = log(nrow(X)))

summary(lm(ldl ~ as.factor(RIAGENDR_encoded) + as.factor(RIDRETH3_encoded) + 
          RIDAGEYR + LBXGLU + BPX, data = X))

summary(lm(ldl ~ as.factor(RIAGENDR_encoded) + RIDAGEYR + BPX, data = X))
