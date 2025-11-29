# 🏅 Olympic Medals Prediction — Statistical Learning (R Project)  
### 📊 Regression Modelling using GDP, Population & Log-Transformations  
#### 👩‍💻 Project by: **Yuvashri Rajendran**

This project develops multiple regression models to predict **2012 Olympic medal counts** for different countries using **GDP, population size and transformed variables**.  
The analysis compares several statistical models, evaluates performance using RMSE, AIC & Adjusted R², and identifies the best model for predicting medal outcomes.

---

## 🎯 Objective

To analyze how **GDP and population influence Olympic medal performance**, and to build & compare predictive regression models using:

✔ Raw variables  
✔ Log-transformed outputs  
✔ Polynomial feature engineering  

---

## 🛠 Tech Stack (R)

| Task | Tools / Libraries |
|---|---|
| Data Handling | `tidyverse`, `dplyr` |
| EDA & Visualisation | `ggplot2`, `corrplot` |
| Regression Modelling | `lm()`, `poly()` |
| Model Evaluation | RMSE, Adjusted R², AIC |
| Model Selection | `leaps`, AIC scoring |

---

## 🔍 Analysis Workflow

### 1. 📄 Data Exploration
- Summary statistics & structure inspection  
- Boxplots for GDP, Population & Medal count  
- Correlation matrix to analyse relationships  
- Scatter plots with regression lines

### 2. 🧮 Model 1 — Base Linear Regression  
**Medal Count ~ Population + GDP**

Outputs:  
- Coefficients  
- Significance levels  
- R² & residual behaviour  

---

### 3. 🔄 Model 2 — Log-Transformed Output  
`log(Medal2012 + 1)` used to correct skewness & zero values

Steps included:
- Histogram before vs. after log transform  
- Updated regression model  
- Improved interpretability + residual behaviour analysis  

---

### 4. 🧩 Model 3 — Custom Polynomial Regression  
Incorporates non-linear relationships:

```r
log_Medal2012 ~ poly(Population, 2) + poly(GDP, 3)
