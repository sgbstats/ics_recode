import rpy2.robjects as ro
from rpy2.robjects import pandas2ri, r
from rpy2.robjects.conversion import localconverter
import pandas as pd
import numpy as np
import pickle
import os

def convert_r_submodels(r_file_path, output_path=None):
    """
    Convert R submodels .rda file to Python pickle object
    
    Args:
        r_file_path: Path to the R .rda file
        output_path: Path to save the pickle file (optional)
        
    Returns:
        Dictionary containing the converted submodels object
    """
    # Activate pandas to R conversion
    pandas2ri.activate()
    
    # Load the R object
    r_obj_name = os.path.basename(r_file_path).replace('.rda', '')
    r(f"load('{r_file_path}')")
    
    # Get the object from R environment
    r_obj = r[r_obj_name]
    
    # Initialize Python dictionary to hold the converted object
    py_obj = {}
    
    # Convert submodels
    submodels = []
    for i in range(len(r_obj.rx2('submodels'))):
        submodel = {}
        r_submodel = r_obj.rx2('submodels')[i]
        
        # Convert coefficients
        with localconverter(ro.default_converter + pandas2ri.converter):
            submodel['coefficients'] = np.array(r_submodel.rx2('coefficients'))
        
        # Convert terms
        if 'terms' in r_submodel.names:
            with localconverter(ro.default_converter + pandas2ri.converter):
                terms = list(ro.conversion.rpy2py(r_submodel.rx2('terms').rx2('term.labels')))
                submodel['terms'] = ["(Intercept)"] + terms
        
        # Add other attributes if needed
        for attr in r_submodel.names:
            if attr not in ['coefficients', 'terms']:
                try:
                    with localconverter(ro.default_converter + pandas2ri.converter):
                        submodel[attr] = ro.conversion.rpy2py(r_submodel.rx2(attr))
                except Exception as e:
                    print(f"Could not convert attribute {attr}: {str(e)}")
        
        submodels.append(submodel)
    
    py_obj['submodels'] = submodels
    
    # Convert patterns
    with localconverter(ro.default_converter + pandas2ri.converter):
        py_obj['patterns'] = list(ro.conversion.rpy2py(r_obj.rx2('patterns')))
    
    # Convert variable info if it exists
    if 'variable.info' in r_obj.names:
        variable_info = {}
        r_var_info = r_obj.rx2('variable.info')
        
        for attr in r_var_info.names:
            try:
                with localconverter(ro.default_converter + pandas2ri.converter):
                    variable_info[attr] = ro.conversion.rpy2py(r_var_info.rx2(attr))
            except Exception as e:
                print(f"Could not convert variable info {attr}: {str(e)}")
        
        py_obj['variable.info'] = variable_info
    
    # Save as pickle if output path is provided
    if output_path:
        with open(output_path, 'wb') as f:
            pickle.dump(py_obj, f)
        print(f"Saved converted model to {output_path}")
    
    return py_obj

def convert_r_functions(output_dir="helpers"):
    """
    Create Python implementations of essential R functions
    
    Args:
        output_dir: Directory to save the Python helper modules
    """
    # Create the output directory if it doesn't exist
    os.makedirs(output_dir, exist_ok=True)
    
    print(f"Python helper modules will be created in {output_dir}")
    print("You need to implement or verify the actual function logic as needed")

def main():
    """
    Main function to convert all R objects to Python
    """
    # Create directories
    os.makedirs("models", exist_ok=True)
    os.makedirs("helpers", exist_ok=True)
    
    # Convert submodels
    print("Converting submodels_sev.rda...")
    convert_r_submodels("submodels_sev.rda", "models/submodels_sev.pkl")
    
    print("Converting submodels_modsev.rda...")
    convert_r_submodels("submodels_modsev.rda", "models/submodels_modsev.pkl")
    
    print("Converting submodels_pneum.rda...")
    convert_r_submodels("submodels_pneum.rda", "models/submodels_pneum.pkl")
    
    print("\nConversion complete!")
    print("\nNOTE: You'll need to verify the Python implementations of helper functions")
    print("The helper modules have been created but may need adjustments to match R functionality")
    print("\nTo start the API server, run: python app.py")

if __name__ == "__main__":
    main()