# Mastercard MEI Analytics Platform

Enterprise-grade machine learning analytics platform built using **R Shiny**, **Plotly**, and advanced machine learning workflows for interactive financial analytics and predictive intelligence.

This project simulates a production-quality analytics engineering platform inspired by systems used in fintech and enterprise analytics environments such as the Mastercard Economics Institute (MEI).

---

# Features

## Executive Analytics Dashboard
- KPI monitoring
- Interactive executive overview
- Radar charts for model comparison
- Gauge charts for accuracy and AUC
- Prediction distribution analysis

## Spam Classification Analytics
- Logistic Regression
- Ridge Regression using glmnet
- ROC curve analysis
- Confusion matrix heatmaps
- Feature importance visualization
- Prediction explorer

## Loan Approval Analytics
- K-Nearest Neighbors (KNN)
- Logistic Regression
- K-value optimization
- Correlation heatmaps
- Loan approval insights
- Interactive prediction simulator

## Diagnostics & Monitoring
- Model drift monitoring
- KPI trend analysis
- Class imbalance analysis
- Feature correlation diagnostics

## Data Explorer
- Interactive tables
- Filtering and search
- CSV export functionality

---

# Tech Stack

## Core Technologies
- R
- Shiny
- shinydashboard
- Plotly
- DT

## Machine Learning
- caret
- glmnet
- class
- e1071
- pROC

## Data Engineering
- dplyr
- tidyr

## Visualization
- Plotly
- Correlation Heatmaps
- Interactive KPI dashboards

---

# Machine Learning Models

## Spam Classification
- Logistic Regression
- Ridge Regression (glmnet)

## Loan Prediction
- K-Nearest Neighbors (KNN)
- Logistic Regression

---

# Dashboard Modules

| Module | Description |
|---|---|
| Executive Dashboard | Enterprise KPI monitoring and executive analytics |
| Spam Analytics | ROC analysis, confusion matrix, feature importance |
| Loan Analytics | Loan prediction insights and optimization |
| Diagnostics | Drift monitoring and correlation analysis |
| Data Explorer | Interactive filtering and exports |

---

# Architecture

```text
app.R
global.R
www/style.css
data/
```

- `app.R` → UI + Server logic
- `global.R` → Data pipelines + ML workflows
- `style.css` → Enterprise dashboard styling
- `data/` → Datasets

---

# Installation

## Install Packages

```r
install.packages(c(
  "shiny",
  "shinydashboard",
  "shinydashboardPlus",
  "plotly",
  "DT",
  "dplyr",
  "tidyr",
  "caret",
  "glmnet",
  "class",
  "pROC",
  "corrplot",
  "e1071"
))
```

---

# Run the Application

```r
shiny::runApp()
```

---

# Key Highlights

- Enterprise-grade analytics engineering platform
- Interactive machine learning dashboards
- Advanced visualization system
- Real-time prediction simulator
- Production-quality UI/UX
- Executive analytics storytelling
- Scalable modular architecture

---

# Business Impact

This platform demonstrates how machine learning and analytics engineering can be integrated into financial systems for:
- fraud/spam detection
- credit risk analysis
- approval prediction
- KPI monitoring
- decision intelligence

---

# Future Improvements

- Real-time API integration
- Database connectivity
- User authentication
- Cloud deployment
- Live streaming analytics
- Quarto report generation

---

# Author

## Sukumar Erugadindla

B.Tech — Computer Science Engineering  
Machine Learning Developer | Analytics Engineering Enthusiast | Full Stack Developer

GitHub:
https://github.com/Sukumar5705

---

# License

This project is licensed under the MIT License.
