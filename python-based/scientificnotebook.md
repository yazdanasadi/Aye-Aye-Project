### Scientific Notebook of Aye Aye Project Update 23.08.2024

This notebook outlines the data preprocessing steps performed by the `LEK_data_grouped` module. To utilize this module, ensure the installation of the following packages: NumPy, Pandas, scikit-learn, XGBoost, Seaborn, and SHAP **(using either Conda or pip package managers)**.

**Procedure:**

1. **Preprocessing:** Encode primary sociodemographic variables, including Gender, Region, Age Group, and Education Level.
2. **Feature Selection:** Identify top features using Spearman correlation, both with and without the "Don't Know" score.
3. **Model Training and Feature Importance:** Train Logistic Regression and evaluate feature importance using Logistic Regression, XGBoost, and Random Forest models.
4. **Optional:** Assess feature importance using Permutation Importance.
5. **Model Interpretation:** Apply LIME (Local Interpretable Model-agnostic Explanations) for interpreting XGBoost and Logistic Regression predictions.
