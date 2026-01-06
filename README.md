# WiFi-Based Occupancy Detection

## Overview
**Lab Data Challenge** – University of Milano-Bicocca (2024)

**Goal:** Classify room occupancy status from WiFi signal characteristics using machine learning.

## Problem
Detect whether a room is:
| Class | Label | Description |
|-------|-------|-------------|
| 0 | Empty | No person in the room |
| 1 | Static | Person standing still |
| 2 | Moving | Person in motion |

This is a **multiclass classification** problem using WiFi Channel State Information (CSI).

## Dataset

| Property | Training Set | Test Set |
|----------|--------------|----------|
| Observations | ~430,000 | ~112,000 |
| Features | 112 | 112 |
| Classes | 3 | 3 |

**Feature structure:**
- Odd-numbered features (V1, V3, ..., V111): Signal **amplitude**
- Even-numbered features (V2, V4, ..., V112): Signal **phase**

## Approach

### Preprocessing
```r
# Moving average to smooth noisy WiFi signals
features <- apply(features, 2, function(x) 
    rollmean(x, k = 15000, fill = "extend", align = "center"))
# Window = 15,000 observations (~3.62% of training data)
```

### Dimensionality Reduction
- **PCA** applied to reduce 112 features → 10 principal components
- First 10 components explain ~95% of variance

### Models Compared

#### 1. Random Forest (Main Model) ✅
```r
fit <- ranger(
    x = X_scores,        # PCA-transformed features
    y = Y_train,
    num.trees = 500,
    max.depth = 50,
    mtry = 10,
    importance = "impurity"
)
```

#### 2. LSTM Neural Network (Experimental) ❌
```r
model <- keras_model_sequential() %>%
    layer_lstm(units = 128, return_sequences = TRUE, recurrent_dropout = 0.3) %>%
    layer_dropout(rate = 0.3) %>%
    layer_lstm(units = 64, return_sequences = TRUE, recurrent_dropout = 0.3) %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = 3, activation = "softmax")
```
- Sequence length: 500
- Differencing transformation instead of moving average
- Showed overfitting issues, not used in final submission

## Results

| Model | Validation Accuracy | Test Accuracy |
|-------|---------------------|---------------|
| Random Forest | **99.9%** | **42.4%** |
| LSTM | ~33% | Not submitted |

### Confusion Matrix (Test Set)
```
           Prediction
Reference    0      1      2
    0      6858  15847  15469
    1      4077  23268   9609
    2       798  18141  17735
```

**Key observation:** Model predicts "static presence" (class 1) best, struggles most with "empty room" (class 0).

## Key Findings & Lessons Learned

1. **Distribution Shift:** Massive gap between validation (99.9%) and test (42.4%) accuracy indicates training and test data come from different distributions (likely different time periods or room conditions)

2. **Temporal Structure:** Random sampling for train/validation split may not be appropriate for time-series data — future work should use proper temporal splits

3. **Signal Noise:** WiFi signals are inherently noisy; moving average helps but may smooth out important transient patterns

4. **Model Selection:** Despite RNN being theoretically better for sequential data, Random Forest performed better in practice due to overfitting issues with LSTM

> Note: LSTM RNN was explored but not used in final predictions due to overfitting.

## Team
| Name | Role |
|------|------|
| **Luca Iaria** | Università Milano-Bicocca |
| Marco Emanuele Saini | Università Milano-Bicocca |
| Alessandro Colello | Università Milano-Bicocca |

## My Contribution
- Preprocessing WiFi CSI signals using moving average smoothing
- PCA dimensionality reduction
- Random Forest model design, hyperparameter tuning, and evaluation
- Analysis of distribution shift and lessons learned for sequential data

## Repository Structure
```
wifi-occupancy-detection/
├── README.md                  # Project overview & methodology
├── Random_Forest.R            # Main model (final submission)
├── RNN.R                      # LSTM experimental model
└── Presentazione.pdf           # Presentation slides
```

## Requirements
```r
# Core
library(dplyr)
library(ranger)      # Fast Random Forest implementation
library(zoo)         # Rolling statistics
library(caret)       # ML utilities & confusion matrix

# Visualization
library(ggplot2)
library(tidyr)
library(gridExtra)

# Deep Learning (for RNN.R)
library(keras)
library(reticulate)
```

## How to Run
1. Set correct file paths for training/test CSV files
2. Run `Random_Forest.R` for main analysis
3. Output: `gruppo3_rf.csv` with predictions

## Hyperparameter Tuning (Random Forest)
Tested configurations:
| Trees | Max Depth | Validation Accuracy |
|-------|-----------|---------------------|
| 50 | 10 | 99.75% |
| 100 | 50 | 99.98% |
| **500** | **50** | **99.98%** |
| 1000 | 150 | 99.98% |

Optimal: 500 trees, depth 50 (best trade-off between performance and computation)

---
*Code developed for a university lab challenge. Focus was on methodology comparison and understanding model limitations rather than production-ready code.*

![R](https://img.shields.io/badge/R-276DC3?style=flat&logo=r&logoColor=white)
![ML](https://img.shields.io/badge/Machine_Learning-3498DB?style=flat&logo=python&logoColor=white)
