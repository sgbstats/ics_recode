import pandas as pd
import numpy as np
from typing import Dict, List, Any, Union

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