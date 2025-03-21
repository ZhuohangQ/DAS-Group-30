Income Prediction with Random Forest & GLM

Overview  
This project aims to predict whether an individual's income exceeds $50,000/year using **1994 U.S. Census data**. We compare the performance of two models:
Generalized Linear Model (GLM)
Random Forest (RF)

Data  
- Features: Age, Education, Marital Status, Occupation, Sex, Hours per Week  
- Target: Income (`<=50K` or `>50K`)

Methods  
- GLM: Logistic regression with stepwise AIC  
- Random Forest: 100 trees, variable importance via minimal depth  
- 5-Fold Cross-Validation used for model validation

Results  
| Metric       | GLM   | RF    |
|--------------|-------|-------|
| Accuracy     | 85.4% | 82.9% |
| Sensitivity  | 94.9% | 91.2% |
| Specificity  | 55.2% | 57.8% |
| AUC          | 0.868 | 0.802 |

Insights  
- Marital Status and Education are top predictors.  
- GLM slightly outperforms RF in accuracy and AUC.  
- RF provides better interpretability and specificity.
