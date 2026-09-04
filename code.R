library(foreign)
library(dplyr)

DATA <- read.csv(
  "C:/Users/王娅/Desktop/DATA.csv")
library(survey)
library(arsenal)

options(survey.lonely.psu = "adjust")
study_design <- svydesign(data=DATA, 
                          id=~SDMVPSU, 
                          strata=~SDMVSTRA, 
                          weights=~WT, nest=TRUE)
####Cd_median####
Cd_median <- svyquantile(
  ~Cd,
  design = study_design,
  quantiles = 0.50,
  na.rm = TRUE
)

Cd_median_value <- coef(Cd_median)
Cd_median_value
####Cd_median####
Cd_median <- svyquantile(
  ~Cd,
  design = study_design,
  quantiles = 0.50,
  na.rm = TRUE
)

Cd_median_value <- coef(Cd_median)
Cd_median_value

####Cd_median=3.2####
Cd_median_value <- 3.2

DATA$Cd_binary <- ifelse(
  DATA$Cd < Cd_median_value,
  "Low",
  "High"
)

DATA$Cd_binary <- factor(
  DATA$Cd_binary,
  levels = c("Low", "High")
)
# quantile
Cd_Q1_new <- svyquantile(
  ~Cd,
  design = subset(study_design, Cd < 3.2),
  quantiles = 0.50,
  na.rm = TRUE
)

# 
Cd_Q3_new <- svyquantile(
  ~Cd,
  design = subset(study_design, Cd >= 3.2),
  quantiles = 0.50,
  na.rm = TRUE
)

Cd_Q1_new <- as.numeric(coef(Cd_Q1_new))
Cd_Q3_new <- as.numeric(coef(Cd_Q3_new))

Cd_Q1_new
Cd_Q3_new
DATA$Cd_quartile <- cut(
  DATA$Cd,
  breaks = c(
    -Inf,
    Cd_Q1_new,
    3.2,
    Cd_Q3_new,
    Inf
  ),
  labels = c("Q1", "Q2", "Q3", "Q4"),
  include.lowest = TRUE,
  right = FALSE
)
table(DATA$Cd_binary, DATA$Cd_quartile, useNA = "ifany")
#### PIV_median#### 
PIV_median <- svyquantile(
  ~PIV,
  design = study_design,
  quantiles = 0.50,
  na.rm = TRUE
)

PIV_median_value <- coef(PIV_median)
PIV_median_value
####PIV_median=276.25 ####
PIV_median_value <- 276.25

DATA$PIV_binary <- ifelse(
  DATA$PIV < PIV_median_value,
  "Low",
  "High"
)

DATA$PIV_binary <- factor(
  DATA$PIV_binary,
  levels = c("Low", "High")
)
table(DATA$PIV_binary, useNA = "ifany")

#### SII_median#### 
SII_median <- svyquantile(
  ~SII,
  design = study_design,
  quantiles = 0.50,
  na.rm = TRUE
)

SII_median_value <- coef(SII_median)
SII_median_value
####SII_median=501.28 ####
SII_median_value <- 501.28

DATA$SII_binary <- ifelse(
  DATA$SII < SII_median_value,
  "Low",
  "High"
)

DATA$SII_binary <- factor(
  DATA$SII_binary,
  levels = c("Low", "High")
)
table(DATA$SII_binary, useNA = "ifany")

#### NLR_median#### 
NLR_median <- svyquantile(
  ~NLR,
  design = study_design,
  quantiles = 0.50,
  na.rm = TRUE
)

NLR_median_value <- coef(NLR_median)
NLR_median_value
####NLR_median=2.19 ####
NLR_median_value <- 2.19 

DATA$NLR_binary <- ifelse(
  DATA$NLR < NLR_median_value,
  "Low",
  "High"
)

DATA$NLR_binary <- factor(
  DATA$NLR_binary,
  levels = c("Low", "High")
)
table(DATA$NLR_binary, useNA = "ifany")
#### MLR_median#### 
MLR_median <- svyquantile(
  ~MLR,
  design = study_design,
  quantiles = 0.50,
  na.rm = TRUE
)

MLR_median_value <- coef(MLR_median)
MLR_median_value
####MLR_median=0.3 ####
MLR_median_value <- 0.3

DATA$MLR_binary <- ifelse(
  DATA$MLR < MLR_median_value,
  "Low",
  "High"
)

DATA$MLR_binary <- factor(
  DATA$MLR_binary,
  levels = c("Low", "High")
)
table(DATA$MLR_binary, useNA = "ifany")

#### PLR_median#### 
PLR_median <- svyquantile(
  ~PLR,
  design = study_design,
  quantiles = 0.50,
  na.rm = TRUE
)

PLR_median_value <- coef(PLR_median)
PLR_median_value
####PLR_median=125.6 ####
PLR_median_value <- 125.6

DATA$PLR_binary <- ifelse(
  DATA$PLR < PLR_median_value,
  "Low",
  "High"
)

DATA$PLR_binary <- factor(
  DATA$PLR_binary,
  levels = c("Low", "High")
)
table(DATA$PLR_binary, useNA = "ifany")
#### SIRI_median#### 
SIRI_median <- svyquantile(
  ~SIRI,
  design = study_design,
  quantiles = 0.50,
  na.rm = TRUE
)

SIRI_median_value <- coef(SIRI_median)
SIRI_median_value
####SIRI_median=1.2####
SIRI_median_value <- 1.2

DATA$SIRI_binary <- ifelse(
  DATA$SIRI < SIRI_median_value,
  "Low",
  "High"
)

DATA$SIRI_binary <- factor(
  DATA$SIRI_binary,
  levels = c("Low", "High")
)
table(DATA$SIRI_binary, useNA = "ifany")

####Table 2####

library(survey)
library(dplyr)
library(tidyr)
library(stringr)

covars_model2 <- c(
  "Gender",
  "Race",
  "Edu",
  "Marital1",
  "Age2",
  "PIR1"
)

covars_model3 <- c(
  "Gender",
  "Race",
  "Edu",
  "Marital1",
  "Age2",
  "PIR1",
  "BMI1",
  "Smoke",
  "Alcohol",
  "sleepdisorder",
  "Diabetes",
  "Hypertension"
)


extract_cox <- function(model, indicator){
  
  coef_table <- summary(model)$coefficients
  
  ci <- suppressWarnings(confint(model))
  
  
  row_id <- grep(
    paste0("^", indicator),
    rownames(coef_table)
  )
  
  
  coef_table <- coef_table[row_id, , drop = FALSE]
  ci <- ci[row_id, , drop = FALSE]
  
  result <- data.frame(
    Variable = rownames(coef_table),
    HR = exp(coef_table[, "coef"]),
    Lower95 = exp(ci[, 1]),
    Upper95 = exp(ci[, 2]),
    P = coef_table[, "Pr(>|z|)"],
    stringsAsFactors = FALSE
  )
  
  result
}



get_p_trend <- function(indicator){
  
  trend_var <- paste0(indicator, "_trend")
  
  # Model 1
  formula1 <- as.formula(
    paste(
      "Surv(permthint, mortstat) ~",
      trend_var
    )
  )
  
  trend_model1 <- svycoxph(
    formula1,
    design = study_design
  )
  
  p1 <- summary(trend_model1)$coefficients[
    trend_var,
    "Pr(>|z|)"
  ]
  
  
  # Model 2
  formula2 <- as.formula(
    paste(
      "Surv(permthint, mortstat) ~",
      trend_var,
      "+ Gender + Race + Edu + Marital1 + Age2 + PIR1"
    )
  )
  
  trend_model2 <- svycoxph(
    formula2,
    design = study_design
  )
  
  p2 <- summary(trend_model2)$coefficients[
    trend_var,
    "Pr(>|z|)"
  ]
  
  
  # Model 3
  formula3 <- as.formula(
    paste(
      "Surv(permthint, mortstat) ~",
      trend_var,
      "+ Gender + Race + Edu + Marital1 + Age2 + PIR1",
      "+ BMI1 + Smoke + Alcohol + sleepdisorder",
      "+ Diabetes + Hypertension"
    )
  )
  
  trend_model3 <- svycoxph(
    formula3,
    design = study_design
  )
  
  p3 <- summary(trend_model3)$coefficients[
    trend_var,
    "Pr(>|z|)"
  ]
  
  
  data.frame(
    P_trend_Model1 = p1,
    P_trend_Model2 = p2,
    P_trend_Model3 = p3
  )
}




run_indicator <- function(indicator){
  
  # Model 1
  formula1 <- as.formula(
    paste(
      "Surv(permthint, mortstat) ~",
      indicator
    )
  )
  
  model1 <- svycoxph(
    formula1,
    design = study_design
  )
  
  
  # Model 2
  formula2 <- as.formula(
    paste(
      "Surv(permthint, mortstat) ~",
      indicator,
      "+ Gender + Race + Edu + Marital1 + Age2 + PIR1"
    )
  )
  
  model2 <- svycoxph(
    formula2,
    design = study_design
  )
  
  
  # Model 3
  formula3 <- as.formula(
    paste(
      "Surv(permthint, mortstat) ~",
      indicator,
      "+ Gender + Race + Edu + Marital1 + Age2 + PIR1",
      "+ BMI1 + Smoke + Alcohol + sleepdisorder",
      "+ Diabetes + Hypertension"
    )
  )
  
  model3 <- svycoxph(
    formula3,
    design = study_design
  )
  
  
  # 
  res1 <- extract_cox(model1, indicator)
  res2 <- extract_cox(model2, indicator)
  res3 <- extract_cox(model3, indicator)
  
  
  # 8. 整理成论文 Table 2 格式
  
  
  Table2_wide <- Table2_results %>%
    select(
      Indicator,
      Variable,
      Model,
      HR_95CI,
      P_value
    ) %>%
    pivot_wider(
      names_from = Model,
      values_from = c(HR_95CI, P_value)
    )
  
  Table2_wide
  ####Table2-final####
  trend <- get_p_trend(indicator)
  res1$Model <- "Model 1"
  res2$Model <- "Model 2"
  res3$Model <- "Model 3"
  
  result <- bind_rows(
    res1,
    res2,
    res3
  )
  
  result$Indicator <- indicator
  
  result$HR_95CI <- sprintf(
    "%.2f (%.2f–%.2f)",
    result$HR,
    result$Lower95,
    result$Upper95
  )
  
  result$P_value <- ifelse(
    result$P < 0.001,
    "<0.001",
    sprintf("%.3f", result$P)
  )
  
  result
}

indicators <- c(
  "CdPIV",
  "CdPLR",
  "CdMLR",
  "CdNLR",
  "CdSII",
  "CdSIRI"
)

all_results <- lapply(
  indicators,
  run_indicator
)

Table2_results <- bind_rows(all_results)

Table2_results

Table2_final <- Table2_results %>%
  select(
    Indicator,
    Variable,
    Model,
    HR_95CI,
    P_value
  )

Table2_final

####______P for trend####
DATA$CdPIV_trend <- as.numeric(
  factor(DATA$CdPIV,
         levels = c("Group1", "Group2", "Group3", "Group4"))
)

DATA$CdPLR_trend <- as.numeric(
  factor(DATA$CdPLR,
         levels = c("Group1", "Group2", "Group3", "Group4"))
)

DATA$CdMLR_trend <- as.numeric(
  factor(DATA$CdMLR,
         levels = c("Group1", "Group2", "Group3", "Group4"))
)

DATA$CdNLR_trend <- as.numeric(
  factor(DATA$CdNLR,
         levels = c("Group1", "Group2", "Group3", "Group4"))
)

DATA$CdSII_trend <- as.numeric(
  factor(DATA$CdSII,
         levels = c("Group1", "Group2", "Group3", "Group4"))
)

DATA$CdSIRI_trend <- as.numeric(
  factor(DATA$CdSIRI,
         levels = c("Group1", "Group2", "Group3", "Group4"))
)

options(survey.lonely.psu = "adjust")

study_design <- svydesign(
  ids = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WT,
  nest = TRUE,
  data = DATA
)

indicators <- c(
  "CdPIV",
  "CdPLR",
  "CdMLR",
  "CdNLR",
  "CdSII",
  "CdSIRI"
)

P_trend_table <- data.frame()

for(indicator in indicators){
  
  trend <- get_p_trend(indicator)
  
  temp <- data.frame(
    Indicator = indicator,
    Model1 = trend$P_trend_Model1,
    Model2 = trend$P_trend_Model2,
    Model3 = trend$P_trend_Model3
  )
  
  P_trend_table <- bind_rows(
    P_trend_table,
    temp
  )
}

P_trend_table
####Table S1####
# Table S1
# Outcome: CVD
# Survey-weighted Cox regression#

library(survey)
library(dplyr)
library(tidyr)

options(survey.lonely.psu = "adjust")

DATA$CdPIV_trend <- as.numeric(
  factor(
    DATA$CdPIV,
    levels = c("Group1", "Group2", "Group3", "Group4")
  )
)

DATA$CdPLR_trend <- as.numeric(
  factor(
    DATA$CdPLR,
    levels = c("Group1", "Group2", "Group3", "Group4")
  )
)

DATA$CdMLR_trend <- as.numeric(
  factor(
    DATA$CdMLR,
    levels = c("Group1", "Group2", "Group3", "Group4")
  )
)

DATA$CdNLR_trend <- as.numeric(
  factor(
    DATA$CdNLR,
    levels = c("Group1", "Group2", "Group3", "Group4")
  )
)

DATA$CdSII_trend <- as.numeric(
  factor(
    DATA$CdSII,
    levels = c("Group1", "Group2", "Group3", "Group4")
  )
)

DATA$CdSIRI_trend <- as.numeric(
  factor(
    DATA$CdSIRI,
    levels = c("Group1", "Group2", "Group3", "Group4")
  )
)



study_design <- svydesign(
  ids = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WT,
  nest = TRUE,
  data = DATA
)



formula_model1 <- function(indicator) {
  as.formula(
    paste(
      "Surv(permthint, CVD) ~",
      indicator
    )
  )
}

formula_model2 <- function(indicator) {
  as.formula(
    paste(
      "Surv(permthint, CVD) ~",
      indicator,
      "+ Gender + Race + Edu + Marital1 + Age2 + PIR1"
    )
  )
}

formula_model3 <- function(indicator) {
  as.formula(
    paste(
      "Surv(permthint, CVD) ~",
      indicator,
      "+ Gender + Race + Edu + Marital1 + Age2 + PIR1",
      "+ BMI1 + Smoke + Alcohol + sleepdisorder",
      "+ Diabetes + Hypertension"
    )
  )
}

extract_cox <- function(model, indicator, model_name) {
  
  s <- summary(model)
  
  coef_table <- s$coefficients
  
  rows <- grep(
    paste0("^", indicator),
    rownames(coef_table)
  )
  
  coef_table2 <- coef_table[rows, , drop = FALSE]
  
  # confint
  ci <- suppressWarnings(confint(model))
  
  ci2 <- ci[rows, , drop = FALSE]
  
  result <- data.frame(
    Variable = rownames(coef_table2),
    HR = exp(coef_table2[, "coef"]),
    Lower95 = exp(ci2[, 1]),
    Upper95 = exp(ci2[, 2]),
    P = coef_table2[, "Pr(>|z|)"],
    Model = model_name,
    stringsAsFactors = FALSE
  )
  
  result
}
####_____P for trend####
get_p_trend <- function(indicator) {
  
  trend_var <- paste0(indicator, "_trend")
  
  
  # Model 1
  trend_model1 <- svycoxph(
    as.formula(
      paste(
        "Surv(permthint, CVD) ~",
        trend_var
      )
    ),
    design = study_design
  )
  
  p1 <- summary(trend_model1)$coefficients[
    trend_var,
    "Pr(>|z|)"
  ]
  
  
  # Model 2
  trend_model2 <- svycoxph(
    as.formula(
      paste(
        "Surv(permthint, CVD) ~",
        trend_var,
        "+ Gender + Race + Edu + Marital1 + Age2 + PIR1"
      )
    ),
    design = study_design
  )
  
  p2 <- summary(trend_model2)$coefficients[
    trend_var,
    "Pr(>|z|)"
  ]
  
  
  # Model 3
  trend_model3 <- svycoxph(
    as.formula(
      paste(
        "Surv(permthint, CVD) ~",
        trend_var,
        "+ Gender + Race + Edu + Marital1 + Age2 + PIR1",
        "+ BMI1 + Smoke + Alcohol + sleepdisorder",
        "+ Diabetes + Hypertension"
      )
    ),
    design = study_design
  )
  
  p3 <- summary(trend_model3)$coefficients[
    trend_var,
    "Pr(>|z|)"
  ]
  
  
  data.frame(
    P_trend_Model1 = p1,
    P_trend_Model2 = p2,
    P_trend_Model3 = p3
  )
}

indicators <- c(
  "CdPIV",
  "CdPLR",
  "CdMLR",
  "CdNLR",
  "CdSII",
  "CdSIRI"
)

####Model 1–3####

all_results <- list()

for (indicator in indicators) {
  
  cat("\nRunning:", indicator, "\n")
  
  
  # Model 1
  model1 <- svycoxph(
    formula_model1(indicator),
    design = study_design
  )
  
  
  # Model 2
  model2 <- svycoxph(
    formula_model2(indicator),
    design = study_design
  )
  
  
  # Model 3
  model3 <- svycoxph(
    formula_model3(indicator),
    design = study_design
  )
  
  res1 <- extract_cox(
    model1,
    indicator,
    "Model 1"
  )
  
  res2 <- extract_cox(
    model2,
    indicator,
    "Model 2"
  )
  
  res3 <- extract_cox(
    model3,
    indicator,
    "Model 3"
  )
  
  
  result <- bind_rows(
    res1,
    res2,
    res3
  )
  
  result$Indicator <- indicator
  
  all_results[[indicator]] <- result
}


TableS1_results <- bind_rows(
  all_results
)


TableS1_results <- TableS1_results %>%
  
  mutate(
    
    HR_95CI = sprintf(
      "%.2f (%.2f–%.2f)",
      HR,
      Lower95,
      Upper95
    ),
    
    P_value = ifelse(
      P < 0.001,
      "<0.001",
      sprintf("%.3f", P)
    )
    
  )


P_trend_table <- data.frame()

for (indicator in indicators) {
  
  trend <- get_p_trend(indicator)
  
  temp <- data.frame(
    
    Indicator = indicator,
    
    P_trend_Model1 =
      trend$P_trend_Model1,
    
    P_trend_Model2 =
      trend$P_trend_Model2,
    
    P_trend_Model3 =
      trend$P_trend_Model3
    
  )
  
  P_trend_table <- bind_rows(
    P_trend_table,
    temp
  )
}

P_trend_table <- P_trend_table %>%
  
  mutate(
    
    Model1 = ifelse(
      P_trend_Model1 < 0.001,
      "<0.001",
      sprintf("%.3f", P_trend_Model1)
    ),
    
    Model2 = ifelse(
      P_trend_Model2 < 0.001,
      "<0.001",
      sprintf("%.3f", P_trend_Model2)
    ),
    
    Model3 = ifelse(
      P_trend_Model3 < 0.001,
      "<0.001",
      sprintf("%.3f", P_trend_Model3)
    )
    
  ) %>%
  
  select(
    Indicator,
    Model1,
    Model2,
    Model3
  )


####TableS1-final####

TableS1_Cox <- TableS1_results %>%
  
  select(
    Indicator,
    Variable,
    Model,
    HR_95CI,
    P_value
  )


cat("\n==============================\n")
cat("P for trend\n")
cat("==============================\n\n")

print(P_trend_table)

cat("\n==============================\n")
cat("Table S1 Cox results\n")
cat("==============================\n\n")

print(TableS1_Cox)
####Table S2####
# Table S2
# Outcome: Cancer mortality (CA)
# Survey-weighted Cox regression

library(survey)
library(dplyr)
library(tidyr)

options(survey.lonely.psu = "adjust")

DATA$CdPIV_trend <- as.numeric(
  factor(
    DATA$CdPIV,
    levels = c("Group1", "Group2", "Group3", "Group4")
  )
)

DATA$CdPLR_trend <- as.numeric(
  factor(
    DATA$CdPLR,
    levels = c("Group1", "Group2", "Group3", "Group4")
  )
)

DATA$CdMLR_trend <- as.numeric(
  factor(
    DATA$CdMLR,
    levels = c("Group1", "Group2", "Group3", "Group4")
  )
)

DATA$CdNLR_trend <- as.numeric(
  factor(
    DATA$CdNLR,
    levels = c("Group1", "Group2", "Group3", "Group4")
  )
)

DATA$CdSII_trend <- as.numeric(
  factor(
    DATA$CdSII,
    levels = c("Group1", "Group2", "Group3", "Group4")
  )
)

DATA$CdSIRI_trend <- as.numeric(
  factor(
    DATA$CdSIRI,
    levels = c("Group1", "Group2", "Group3", "Group4")
  )
)

study_design <- svydesign(
  ids = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WT,
  nest = TRUE,
  data = DATA
)

formula_model1 <- function(indicator) {
  
  as.formula(
    paste(
      "Surv(permthint, CA) ~",
      indicator
    )
  )
}


formula_model2 <- function(indicator) {
  
  as.formula(
    paste(
      "Surv(permthint, CA) ~",
      indicator,
      "+ Gender + Race + Edu + Marital1 + Age2 + PIR1"
    )
  )
}


formula_model3 <- function(indicator) {
  
  as.formula(
    paste(
      "Surv(permthint, CA) ~",
      indicator,
      "+ Gender + Race + Edu + Marital1 + Age2 + PIR1",
      "+ BMI1 + Smoke + Alcohol + sleepdisorder",
      "+ Diabetes + Hypertension"
    )
  )
}

extract_cox <- function(model, indicator, model_name) {
  
  coef_table <- summary(model)$coefficients
  
  #
  rows <- grep(
    paste0("^", indicator),
    rownames(coef_table)
  )
  
  coef_table2 <- coef_table[
    rows,
    ,
    drop = FALSE
  ]
  
  # 95% CI
  ci <- suppressWarnings(
    confint(model)
  )
  
  ci2 <- ci[
    rows,
    ,
    drop = FALSE
  ]
  
  result <- data.frame(
    
    Variable = rownames(coef_table2),
    
    HR = exp(
      coef_table2[, "coef"]
    ),
    
    Lower95 = exp(
      ci2[, 1]
    ),
    
    Upper95 = exp(
      ci2[, 2]
    ),
    
    P = coef_table2[
      ,
      "Pr(>|z|)"
    ],
    
    Model = model_name,
    
    stringsAsFactors = FALSE
  )
  
  result
}

get_p_trend <- function(indicator) {
  
  trend_var <- paste0(
    indicator,
    "_trend"
  )
  

  
  trend_model1 <- svycoxph(
    
    as.formula(
      paste(
        "Surv(permthint, CA) ~",
        trend_var
      )
    ),
    
    design = study_design
  )
  
  p1 <- summary(
    trend_model1
  )$coefficients[
    trend_var,
    "Pr(>|z|)"
  ]
  

  
  trend_model2 <- svycoxph(
    
    as.formula(
      paste(
        "Surv(permthint, CA) ~",
        trend_var,
        "+ Gender + Race + Edu + Marital1 + Age2 + PIR1"
      )
    ),
    
    design = study_design
  )
  
  p2 <- summary(
    trend_model2
  )$coefficients[
    trend_var,
    "Pr(>|z|)"
  ]
  
  
  trend_model3 <- svycoxph(
    
    as.formula(
      paste(
        "Surv(permthint, CA) ~",
        trend_var,
        "+ Gender + Race + Edu + Marital1 + Age2 + PIR1",
        "+ BMI1 + Smoke + Alcohol + sleepdisorder",
        "+ Diabetes + Hypertension"
      )
    ),
    
    design = study_design
  )
  
  p3 <- summary(
    trend_model3
  )$coefficients[
    trend_var,
    "Pr(>|z|)"
  ]
  
  
  data.frame(
    
    P_trend_Model1 = p1,
    
    P_trend_Model2 = p2,
    
    P_trend_Model3 = p3
    
  )
}

indicators <- c(
  "CdPIV",
  "CdPLR",
  "CdMLR",
  "CdNLR",
  "CdSII",
  "CdSIRI"
)


####Model 1–3####

all_results <- list()


for (indicator in indicators) {
  
  cat(
    "\nRunning:",
    indicator,
    "\n"
  )
  
  
  # Model 1
  model1 <- svycoxph(
    formula_model1(indicator),
    design = study_design
  )
  
  
  # Model 2
  model2 <- svycoxph(
    formula_model2(indicator),
    design = study_design
  )
  
  
  # Model 3
  model3 <- svycoxph(
    formula_model3(indicator),
    design = study_design
  )
  
  res1 <- extract_cox(
    model1,
    indicator,
    "Model 1"
  )
  
  res2 <- extract_cox(
    model2,
    indicator,
    "Model 2"
  )
  
  res3 <- extract_cox(
    model3,
    indicator,
    "Model 3"
  )
  
  result <- bind_rows(
    res1,
    res2,
    res3
  )
  
  result$Indicator <- indicator
  
  all_results[[indicator]] <- result
}


TableS2_results <- bind_rows(
  all_results
)

TableS2_results <- TableS2_results %>%
  
  mutate(
    
    HR_95CI = sprintf(
      "%.2f (%.2f–%.2f)",
      HR,
      Lower95,
      Upper95
    ),
    
    P_value = ifelse(
      P < 0.001,
      "<0.001",
      sprintf("%.3f", P)
    )
    
  )


P_trend_table <- data.frame()


for (indicator in indicators) {
  
  trend <- get_p_trend(
    indicator
  )
  
  temp <- data.frame(
    
    Indicator = indicator,
    
    P_trend_Model1 =
      trend$P_trend_Model1,
    
    P_trend_Model2 =
      trend$P_trend_Model2,
    
    P_trend_Model3 =
      trend$P_trend_Model3
    
  )
  
  P_trend_table <- bind_rows(
    P_trend_table,
    temp
  )
}


P_trend_table <- P_trend_table %>%
  
  mutate(
    
    Model1 = ifelse(
      P_trend_Model1 < 0.001,
      "<0.001",
      sprintf(
        "%.3f",
        P_trend_Model1
      )
    ),
    
    Model2 = ifelse(
      P_trend_Model2 < 0.001,
      "<0.001",
      sprintf(
        "%.3f",
        P_trend_Model2
      )
    ),
    
    Model3 = ifelse(
      P_trend_Model3 < 0.001,
      "<0.001",
      sprintf(
        "%.3f",
        P_trend_Model3
      )
    )
    
  ) %>%
  
  select(
    Indicator,
    Model1,
    Model2,
    Model3
  )


TableS2_Cox <- TableS2_results %>%
  
  select(
    Indicator,
    Variable,
    Model,
    HR_95CI,
    P_value
  )
cat(
  "\n====================================\n"
)

cat(
  "Table S2: P for trend\n"
)

cat(
  "====================================\n\n"
)

print(
  P_trend_table
)

cat(
  "\n====================================\n"
)

cat(
  "Table S2: Cox regression results\n"
)

cat(
  "====================================\n\n"
)

print(
  TableS2_Cox
)

TableS2_wide <- TableS2_results %>%
  
  select(
    Indicator,
    Variable,
    Model,
    HR_95CI,
    P_value
  ) %>%
  
  pivot_wider(
    names_from = Model,
    values_from = c(
      HR_95CI,
      P_value
    )
  )



TableS2_final <- TableS2_wide %>%
  
  left_join(
    P_trend_table,
    by = "Indicator"
  )


####Table S2-final####
cat(
  "\n====================================\n"
)

cat(
  "FINAL Table S2\n"
)

cat(
  "====================================\n\n"
)

print(
  TableS2_final
)

####Table S3####
# Table S3
# Outcome: All-cause mortality
# Survey-weighted Cox regression
# Indicators: PIV, PLR, MLR, NLR, SII, SIRI, Cd
# Reference: Low

library(survey)
library(dplyr)
library(tidyr)

options(survey.lonely.psu = "adjust")

indicators <- c(
  "PIVm",
  "PLRm",
  "MLRm",
  "NLRm",
  "SIIm",
  "SIRIm",
  "Cdm"
)

for (indicator in indicators) {
  
  study_design$variables[[indicator]] <- relevel(
    factor(study_design$variables[[indicator]]),
    ref = "Low"
  )
}

formula_model1 <- function(indicator) {
  
  as.formula(
    paste(
      "Surv(permthint, mortstat) ~",
      indicator
    )
  )
}


formula_model2 <- function(indicator) {
  
  as.formula(
    paste(
      "Surv(permthint, mortstat) ~",
      indicator,
      "+ Gender + Race + Edu + Marital1 + Age2 + PIR1"
    )
  )
}


formula_model3 <- function(indicator) {
  
  as.formula(
    paste(
      "Surv(permthint, mortstat) ~",
      indicator,
      "+ Gender + Race + Edu + Marital1 + Age2 + PIR1",
      "+ BMI1 + Smoke + Alcohol + sleepdisorder",
      "+ Diabetes + Hypertension"
    )
  )
}

extract_cox <- function(model, indicator, model_name) {
  
  coef_table <- summary(model)$coefficients
  
  # 
  rows <- grep(
    paste0("^", indicator),
    rownames(coef_table)
  )
  
  coef_table2 <- coef_table[
    rows,
    ,
    drop = FALSE
  ]
  
  # 95% CI
  ci <- suppressWarnings(
    confint(model)
  )
  
  ci2 <- ci[
    rows,
    ,
    drop = FALSE
  ]
  
  result <- data.frame(
    
    Variable = rownames(coef_table2),
    
    HR = exp(
      coef_table2[, "coef"]
    ),
    
    Lower95 = exp(
      ci2[, 1]
    ),
    
    Upper95 = exp(
      ci2[, 2]
    ),
    
    P = coef_table2[
      ,
      "Pr(>|z|)"
    ],
    
    Model = model_name,
    
    stringsAsFactors = FALSE
  )
  
  return(result)
}

all_results <- list()


for (indicator in indicators) {
  
  cat(
    "\n====================================\n"
  )
  
  cat(
    "Running:",
    indicator,
    "\n"
  )
  
  cat(
    "====================================\n"
  )
  
  # Model 1
  model1 <- svycoxph(
    formula_model1(indicator),
    design = study_design
  )
  
  # Model 2
  
  model2 <- svycoxph(
    formula_model2(indicator),
    design = study_design
  )

  # Model 3
  model3 <- svycoxph(
    formula_model3(indicator),
    design = study_design
  )

  
  res1 <- extract_cox(
    model1,
    indicator,
    "Model 1"
  )
  
  res2 <- extract_cox(
    model2,
    indicator,
    "Model 2"
  )
  
  res3 <- extract_cox(
    model3,
    indicator,
    "Model 3"
  )
  
  result <- bind_rows(
    res1,
    res2,
    res3
  )
  
  
  result$Indicator <- indicator
  
  all_results[[indicator]] <- result
}


TableS3_results <- bind_rows(
  all_results
)

TableS3_results <- TableS3_results %>%
  
  mutate(
    
    HR_95CI = sprintf(
      "%.2f (%.2f–%.2f)",
      HR,
      Lower95,
      Upper95
    ),
    
    P_value = ifelse(
      P < 0.001,
      "<0.001",
      sprintf(
        "%.3f",
        P
      )
    )
  )
####TableS3-final####

TableS3_Cox <- TableS3_results %>%
  
  select(
    Indicator,
    Variable,
    Model,
    HR_95CI,
    P_value
  )


cat(
  "\n====================================\n"
)

cat(
  "Table S3: Cox regression results\n"
)

cat(
  "====================================\n\n"
)

print(
  TableS3_Cox
)


TableS3_wide <- TableS3_results %>%
  
  select(
    Indicator,
    Variable,
    Model,
    HR_95CI,
    P_value
  ) %>%
  
  pivot_wider(
    
    names_from = Model,
    
    values_from = c(
      HR_95CI,
      P_value
    )
  )

TableS3_final <- TableS3_wide

cat(
  "\n====================================\n"
)

cat(
  "FINAL Table S3\n"
)

cat(
  "====================================\n\n"
)

print(
  TableS3_final
)

#### Table S4 ####
options(survey.lonely.psu = "adjust")
study_design <- svydesign(data=DATA, 
                          id=~SDMVPSU, 
                          strata=~SDMVSTRA, 
                          weights=~WT, nest=TRUE)
Cd_median <- svyquantile(
  ~Cd,
  design = study_design,
  quantiles = 0.50,
  na.rm = TRUE
)

Cd_median_value <- coef(Cd_median)
Cd_median_value
####Cd_median=3.2####
Cd_median_value <- 3.2

DATA$Cd_binary <- ifelse(
  DATA$Cd < Cd_median_value,
  "Low",
  "High"
)

DATA$Cd_binary <- factor(
  DATA$Cd_binary,
  levels = c("Low", "High")
)
# quantile
Cd_Q1_new <- svyquantile(
  ~Cd,
  design = subset(study_design, Cd < 3.2),
  quantiles = 0.50,
  na.rm = TRUE
)

# 
Cd_Q3_new <- svyquantile(
  ~Cd,
  design = subset(study_design, Cd >= 3.2),
  quantiles = 0.50,
  na.rm = TRUE
)

Cd_Q1_new <- as.numeric(coef(Cd_Q1_new))
Cd_Q3_new <- as.numeric(coef(Cd_Q3_new))

Cd_Q1_new
Cd_Q3_new
DATA$Cd_quartile <- cut(
  DATA$Cd,
  breaks = c(
    -Inf,
    Cd_Q1_new,
    3.2,
    Cd_Q3_new,
    Inf
  ),
  labels = c("Q1", "Q2", "Q3", "Q4"),
  include.lowest = TRUE,
  right = FALSE
)
table(DATA$Cd_binary, DATA$Cd_quartile, useNA = "ifany")

#Q4
model1 <- svycoxph(Surv(permthint, mortstat) ~ Cd_quartile , design = study_design)
model2 <- svycoxph(Surv(permthint, mortstat) ~  Cd_quartile+Gender+Race+Edu+Marital1+Age2+PIR1 , design = study_design)
model3 <- svycoxph(Surv(permthint, mortstat) ~ Cd_quartile+Gender+Race+Edu+Marital1+Age2+PIR1+BMI1+Smoke+Alcohol+sleepdisorder+Diabetes+Hypertension , design = study_design)
summary(model1)
summary(model2)
summary(model3)

####Deaths/Participants####
death_table <- DATA %>%
  filter(!is.na(Cd_quartile),
         !is.na(mortstat)) %>%
  group_by(Cd_quartile) %>%
  summarise(
    Participants = n(),
    Deaths = sum(mortstat == 1, na.rm = TRUE),
    Death_percent = Deaths / Participants * 100,
    .groups = "drop"
  )

death_table
####______P for trend####
DATA$Cd_quartile_trend <- as.numeric(DATA$Cd_quartile)

study_design <- update(
  study_design,
  Cd_quartile_trend = DATA$Cd_quartile_trend
)

model1_trend <- svycoxph(
  Surv(permthint, mortstat) ~ Cd_quartile_trend,
  design = study_design
)

summary(model1_trend)

model2_trend <- svycoxph(
  Surv(permthint, mortstat) ~ Cd_quartile_trend+Gender + Race + Edu + Marital1 + Age2 + PIR1,
  design = study_design
)

summary(model2_trend)

model3_trend <- svycoxph(
  Surv(permthint, mortstat) ~ Cd_quartile_trend+Gender + Race + Edu + Marital1 + Age2 + PIR1
  + BMI1 + Smoke + Alcohol + sleepdisorder +
    Diabetes + Hypertension,
  design = study_design
)

summary(model3_trend)
