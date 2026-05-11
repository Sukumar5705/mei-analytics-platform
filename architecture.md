# Architecture Overview

## System Design

The platform follows a modular analytics engineering architecture.

## Components

### app.R
Contains:
- UI layout
- dashboard routing
- server logic
- reactive visualizations

### global.R
Contains:
- data preprocessing
- machine learning pipelines
- model training
- KPI computation
- analytics utilities

### style.css
Contains:
- enterprise UI styling
- dark mode components
- responsive layouts
- dashboard themes

### data/
Stores:
- spam dataset
- loan dataset

---

# Machine Learning Pipeline

## Spam Classification
1. Data preprocessing
2. Logistic Regression
3. Ridge Regression
4. ROC/AUC evaluation
5. Confusion matrix analysis

## Loan Prediction
1. Data preprocessing
2. KNN optimization
3. Logistic Regression
4. Performance evaluation
5. Prediction simulation

---

# Visualization Pipeline

The platform uses:
- Plotly
- Heatmaps
- Gauge charts
- KPI dashboards
- Radar charts
- Interactive tables

---

# Analytics Engineering Concepts

Implemented concepts:
- reactive programming
- ML monitoring
- KPI tracking
- drift analysis
- feature importance
- model diagnostics
