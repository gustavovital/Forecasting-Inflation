# Forecasting Brazilian Inflation Dynamics: A Bayesian and Classical Time Series Approach

This repository presents an integrated empirical framework for short-term inflation forecasting in Brazil using both Bayesian and classical multivariate time series models. The approach is designed to replicate and extend methodologies inspired by canonical work from the Central Bank of Brazil and the academic literature, with full transparency and reproducibility.

---

## 🎯 Objective

To construct, estimate, and compare the forecasting performance of:

- **Bayesian Vector Autoregressions (BVARs)** with Minnesota priors
- **Vector Error Correction Models (VECMs)** based on Johansen’s cointegration methodology

The focus lies on forecasting both **headline IPCA inflation** and its **components**, alongside a system of relevant macroeconomic indicators.

---

## 📚 Methodological Summary

### 1. **Data Acquisition & Preprocessing**

- Macroeconomic time series are sourced from:
  - BCB SGS (`rbcb`)
  - IPEADATA
  - Yahoo Finance (e.g. exchange rates, commodity indices)
  - IBGE/SIDRA (specific sectoral indicators)
- All series are aligned, seasonally adjusted (when necessary), and aggregated to monthly or quarterly frequency.

### 2. **Model Estimation**

#### 🔵 *Bayesian VAR (BVAR)* – via [`BMR`](https://github.com/kthohr/BMR)

- Estimated using the Minnesota conjugate prior with shrinkage hyperparameter `λ`
- Prior means for AR(1) coefficients are calibrated empirically
- Posterior inference via Gibbs sampling
- Forecasts (point and density) and impulse response functions computed via `forecast()` and `IRF()`

#### ⚫ *Vector Error Correction Model (VECM)* – via `urca` and `vars`

- Cointegration rank selected using Johansen's trace statistic
- VECM estimated via `cajorls()`, transformed to a level VAR via `vec2var()`
- Structural dynamics analyzed through impulse response functions

---

## 🔧 Software and Dependencies

Developed in **R (≥ 4.3)** with the following core packages:

```r
library(BMR)         # Bayesian VAR (Minnesota prior, Gibbs sampling)
library(ipeadatar)   # Interface to IPEADATA time series
library(lubridate)   # Flexible date manipulation
library(quantmod)    # Financial and market data (Yahoo Finance)
library(rbcb)        # Access to Central Bank of Brazil (SGS) data
library(seasonal)    # Seasonal adjustment via X-13ARIMA-SEATS
library(sidrar)      # Interface to IBGE/SIDRA datasets
library(tidyverse)   # Data wrangling, plotting, and functional tools
library(tsibble)     # Tidy time series management and temporal indexing
library(urca)        # Johansen cointegration test and unit root tests
library(vars)        # Classical VAR and VECM estimation
