import pickle
def save_xgboost_model(model, model_info, filepath):
    """
    Save an XGBoost model and its metadata to a .datamdl file.

    Parameters:
        model: XGBoost model object
            The trained XGBoost model to be saved.
        model_info: dict
            Dictionary containing metadata about the model.
        filepath: str
            Path to save the .datamdl file.
    """
    try:
        with open(filepath, 'wb') as f:
            pickle.dump({'model': model, 'model_info': model_info}, f)
        print("XGBoost model and metadata saved successfully.")
    except Exception as e:
        print("Error saving XGBoost model:", e)



#### usage
#save_xgboost_model(model, model_info, "model.datamdl")
