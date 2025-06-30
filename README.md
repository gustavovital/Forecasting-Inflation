# Forecasting Brazilian Inflation Dynamics  
**A Time Series Framework for Statistical and Structural Modeling**

This repository provides a comprehensive framework for short-term inflation forecasting in Brazil, combining statistical factor-augmented models and economically motivated vector autoregressions. The modeling architecture reflects the structure of institutional forecasting systems and is designed for transparency, robustness, and empirical rigor.

---

## Project Scope

The project implements and compares distinct classes of inflation forecasting models using a consistent empirical environment. The objective is to evaluate the predictive performance of alternative model structures across horizons, forecast origins, and variable groupings. Forecasts are generated for both headline IPCA inflation and its decomposition into regulated and free prices.

---

## Model Classes

### Statistical Models

Statistical models are based on factor-augmented vector autoregressions (FAVARs). These models reduce dimensionality by extracting principal components from thematic blocks of macroeconomic indicators. Two specifications are implemented:

- **Class I:** Models organized around six fixed thematic groups (activity, external, financial, price, monetary, and shock variables). Forecasts are generated across all combinations of strategy, lag order, and forecast origin.
  
- **Class II:** Models constructed from all possible combinations of three out of the six thematic groups, allowing for more targeted information sets. Each model is estimated at multiple lag orders and factor strategies, generating 1,440 models per forecast origin.

### Economic Models

Economic models are specified as traditional vector autoregressions using a small set of macroeconomic variables selected for theoretical relevance. These models include observable indicators such as interest rates, inflation expectations, exchange rates, and activity variables, and are estimated at both monthly and quarterly frequencies.

### Forecast Combination Models

In addition to individual model forecasts, the framework includes combined forecasts obtained by aggregating predictions from different models. These are used to assess whether combining across specifications improves accuracy or stability, especially under model uncertainty.

---

## Forecasting Strategy

- Forecasts are generated recursively using expanding windows, simulating real-time conditions.
- Multiple forecast horizons are evaluated (one quarter, two quarters, and one year ahead).
- Both point forecasts and full predictive intervals (80% and 95%) are stored for evaluation.

---

## Evaluation and Outputs

- Forecasts are compared against observed values using standard forecast evaluation criteria.
- Tabular outputs include point predictions, uncertainty bounds, and accuracy statistics across all models and horizons.
- All forecasts are organized for integration into reports, figures, or further statistical analysis.