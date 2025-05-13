import numpy as np
import pandas as pd
from typing import Dict, List, Any, Union, Tuple
import warnings

def predict_submodels(new_data: pd.DataFrame, 
                      submodels_object: Dict, 
                      prediction_type: str = "response") -> float:
    """
    Makes predictions using submodels based on missingness patterns
    
    Args:
        new_data: DataFrame with predictors for new cases
        submodels_object: Object containing submodels for different missingness patterns
        prediction_type: Type of prediction to return ("response" or "linear")
        
    Returns:
        Predicted value(s)
    """
    # Extract components from the submodels object
    submodels = submodels_object.get("submodels", {})
    variable_info = submodels_object.get("variable.info", {})
    patterns = submodels_object.get("patterns", [])
    
    # Handle missingness in the new data
    pattern_info = missingness_pattern(new_data)
    tmp_pattern = pattern_info["tmp.pattern"]
    
    # Find the appropriate submodel for this pattern
    if str(tmp_pattern[0]) in patterns:
        idx = patterns.index(str(tmp_pattern[0]))
        submodel = submodels[idx]
        
        # Prepare the data for prediction
        X = prepare_prediction_data(new_data, submodel, variable_info)
        
        # Make prediction
        if prediction_type == "response":
            # For logistic models, transform through inverse logit
            linear_pred = X @ submodel["coefficients"]
            return 1 / (1 + np.exp(-linear_pred))
        else:
            # Return linear predictor
            return X @ submodel["coefficients"]
    else:
        warnings.warn(f"No submodel found for pattern {tmp_pattern[0]}")
        return np.nan

def prepare_prediction_data(new_data: pd.DataFrame, 
                           submodel: Dict, 
                           variable_info: Dict) -> np.ndarray:
    """
    Prepares data for prediction by creating design matrix
    
    Args:
        new_data: New data for prediction
        submodel: Specific submodel to use
        variable_info: Information about variables
        
    Returns:
        Design matrix X for prediction
    """
    # Get model formula terms
    formula_terms = submodel.get("terms", [])
    
    # Initialize design matrix with intercept column
    X = np.ones((new_data.shape[0], 1))
    
    # Add columns for each term
    for term in formula_terms:
        if term == "(Intercept)":
            continue
            
        # Check if it's an interaction term
        if ":" in term:
            var_names = term.split(":")
            interaction_value = np.ones(new_data.shape[0])
            
            for var in var_names:
                if var in new_data.columns:
                    interaction_value *= new_data[var].values
                else:
                    # Handle derived variables or factors
                    # This is a simplified approach - might need enhancement
                    base_var = var.rstrip("0123456789")
                    if base_var in new_data.columns:
                        # Assume it's a factor level
                        level = var[len(base_var):]
                        interaction_value *= (new_data[base_var] == level).astype(float).values
            
            X = np.column_stack((X, interaction_value))
        
        # Regular term
        elif term in new_data.columns:
            X = np.column_stack((X, new_data[term].values))
        else:
            # Check if it's a factor level
            for col in new_data.columns:
                if term.startswith(col):
                    level = term[len(col):]
                    X = np.column_stack((X, (new_data[col] == level).astype(float).values))
                    break
    
    return X

def print_submodels(submodels_object: Dict) -> Dict:
    """
    Extract and format coefficient information from submodels
    
    Args:
        submodels_object: Object containing submodels
        
    Returns:
        Dictionary with coefficient information
    """
    submodels = submodels_object.get("submodels", {})
    patterns = submodels_object.get("patterns", [])
    
    # Initialize results
    result = {"coefficients": pd.DataFrame()}
    
    # Extract coefficients for each pattern
    for i, pattern in enumerate(patterns):
        submodel = submodels[i]
        
        # Get coefficients and variable names
        coefs = submodel.get("coefficients", [])
        var_names = ["(Intercept)"] + submodel.get("terms", [])[1:]  # Skip intercept in terms
        
        # Create dataframe
        pattern_df = pd.DataFrame({
            "var": var_names[:len(coefs)],
            "coef": coefs,
            "pattern": pattern
        })
        
        # Append to results
        result["coefficients"] = pd.concat([result["coefficients"], pattern_df])
    
    return result

def model_breakdown(new_data: pd.DataFrame, 
                   submodels_object: Dict, 
                   prediction_type: str = "response") -> Dict:
    """
    Provide detailed breakdown of model prediction
    
    Args:
        new_data: New data for prediction
        submodels_object: Object containing submodels
        prediction_type: Type of prediction
        
    Returns:
        Dictionary with model breakdown information
    """
    # Get missingness pattern
    pattern_info = missingness_pattern(new_data)
    tmp_pattern = pattern_info["tmp.pattern"]
    
    # Extract components
    submodels = submodels_object.get("submodels", {})
    patterns = submodels_object.get("patterns", [])
    
    # Find matching pattern
    if str(tmp_pattern[0]) in patterns:
        idx = patterns.index(str(tmp_pattern[0]))
        submodel = submodels[idx]
        
        # Get coefficients and terms
        coefs = submodel.get("coefficients", [])
        terms = ["(Intercept)"] + submodel.get("terms", [])[1:]
        
        # Prepare data
        X = prepare_prediction_data(new_data, submodel, submodels_object.get("variable.info", {}))
        
        # Calculate contribution of each term
        contributions = []
        for i, (term, coef) in enumerate(zip(terms, coefs)):
            contributions.append({
                "term": term,
                "coefficient": coef,
                "value": X[0, i],
                "contribution": coef * X[0, i]
            })
        
        # Calculate linear predictor
        linear_pred = X @ coefs
        
        # Calculate response if requested
        response = None
        if prediction_type == "response":
            response = 1 / (1 + np.exp(-linear_pred))
        
        return {
            "pattern": tmp_pattern[0],
            "contributions": contributions,
            "linear_predictor": linear_pred,
            "response": response
        }
    
    return {"error": f"No submodel found for pattern {tmp_pattern[0]}"}

def missingness_pattern(data: pd.DataFrame) -> Dict:
    """
    Determine the missingness pattern in the data
    
    Args:
        data: DataFrame to check for missing values
        
    Returns:
        Dictionary with missingness pattern information
    """
    # Check which columns have missing values
    is_missing = data.isnull().any(axis=0)
    
    # Create pattern string (1 for missing, 0 for present)
    pattern = ''.join(['1' if is_missing[col] else '0' for col in data.columns])
    
    return {
        "missing_cols": is_missing.index[is_missing].tolist(),
        "present_cols": is_missing.index[~is_missing].tolist(),
        "tmp.pattern": [pattern]
    }

def remove_missing_vars(data: pd.DataFrame) -> pd.DataFrame:
    """
    Remove variables with missing values
    
    Args:
        data: DataFrame to process
        
    Returns:
        DataFrame with complete cases only
    """
    return data.dropna(axis=1)

def unpack(obj: Dict) -> None:
    """
    Make variables in dictionary available in global namespace
    This mimics R's list2env or with() functionality
    
    Args:
        obj: Dictionary with variables to unpack
    """
    # In Python we can't easily add to the caller's namespace
    # This is a simplified version that doesn't truly unpack
    # but makes the object available for use
    globals().update(obj)