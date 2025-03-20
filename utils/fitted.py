import numpy as np
import pandas as pd

def create_fitted_df(actuals: pd.DataFrame, fitted, model_names) -> pd.DataFrame:
    """
    Combines actuals and fitted values into a long-format DataFrame.

    Parameters:
    - actuals (pd.DataFrame): DataFrame with columns ['unique_id', 'ds', 'y'].
    - fitted: statsforecast fitted object.
    - models (list): List of model names corresponding to fitted values.

    Returns:
    - pd.DataFrame: Long-format DataFrame with actual and fitted values.
    """

    # Ensure actuals are sorted properly
    actuals_sorted = actuals.sort_values(by=['unique_id', 'ds']).reset_index(drop=True)

    # Get unique time series identifiers
    unique_ids = actuals_sorted['unique_id'].unique()

    # List to store dataframes for each unique_id
    fitted_dfs = []

    # Iterate through time series (unique_id)
    for i, id in enumerate(unique_ids):  
        # Filter once instead of multiple queries
        ts_df = actuals_sorted[actuals_sorted['unique_id'] == id].copy()
        
        # Get fitted values and store them in a dictionary by model names
        fitted_values = {
            model: (
                fitted.fitted_[i][j].model_.get('fitted', np.nan).tolist()
                if isinstance(fitted.fitted_[i][j].model_, dict) and 'fitted' in fitted.fitted_[i][j].model_
                else np.nan
                )
            for j, model in enumerate(model_names)
        }

        # Assign fitted values to dataframe
        ts_df = ts_df.assign(**fitted_values)

        # Append to the list
        fitted_dfs.append(ts_df)

    # Combine all fitted data
    df_fitted = pd.concat(fitted_dfs, ignore_index=True)

    return df_fitted


# Similar to the above function but uses residuals instead of fitted values
def create_res_df(actuals: pd.DataFrame, fitted, model_names) -> pd.DataFrame:
    """
    Combines actuals and fitted values into a long-format DataFrame.

    Parameters:
    - actuals (pd.DataFrame): DataFrame with columns ['unique_id', 'ds', 'y'].
    - fitted: statsforecast fitted object.
    - models (list): List of model names corresponding to fitted values.

    Returns:
    - pd.DataFrame: Long-format DataFrame with actual and fitted values.
    """

    # Ensure actuals are sorted properly
    actuals_sorted = actuals.sort_values(by=['unique_id', 'ds']).reset_index(drop=True)

    # Get unique time series identifiers
    unique_ids = actuals_sorted['unique_id'].unique()

    # List to store dataframes for each unique_id
    fitted_dfs = []

    # Iterate through time series (unique_id)
    for i, id in enumerate(unique_ids):  
        # Filter once instead of multiple queries
        ts_df = actuals_sorted[actuals_sorted['unique_id'] == id].copy()
        
        # Get fitted values and store them in a dictionary by model names
        fitted_values = {
            model: (
                fitted.fitted_[i][j].model_.get('residuals', np.nan).tolist()
                if isinstance(fitted.fitted_[i][j].model_, dict) and 'residuals' in fitted.fitted_[i][j].model_
                else np.nan
                )
            for j, model in enumerate(model_names)
        }

        # Assign fitted values to dataframe
        ts_df = ts_df.assign(**fitted_values)

        # Append to the list
        fitted_dfs.append(ts_df)

    # Combine all fitted data
    df_fitted = pd.concat(fitted_dfs, ignore_index=True)

    return df_fitted
