from fastapi import FastAPI, Request, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel
from typing import Dict, Any, List, Optional
import pandas as pd
import numpy as np
import pickle
import json
import logging
from datetime import datetime
import sys
import os

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='[%(asctime)s] %(levelname)s: %(message)s',
    datefmt='%Y-%m-%d %H:%M:%S'
)
logger = logging.getLogger(__name__)

# Add helper module path if needed
sys.path.append(os.path.join(os.path.dirname(__file__), "helpers"))

# Import helper functions
from helpers.submodels import predict_submodels, print_submodels, model_breakdown
from helpers.data_utils import missingness_pattern, remove_missing_vars, unpack

# Load models
with open('models/submodels_sev.pkl', 'rb') as f:
    submodels_object_sev = pickle.load(f)

with open('models/submodels_modsev.pkl', 'rb') as f:
    submodels_object_modsev = pickle.load(f)

with open('models/submodels_pneum.pkl', 'rb') as f:
    submodels_object_pneum = pickle.load(f)

# Get coefficients
coef_modsev = print_submodels(submodels_object_modsev)
coef_sev = print_submodels(submodels_object_sev)
coef_pneum = print_submodels(submodels_object_pneum)

# Create interpretation dataframe
interpretation = pd.DataFrame({
    "var": ["(Intercept)", "age_imp", "arm_ipdICS", "arm_ipdICS:eos_bl", 
            "eos_bl", "exac_bl", "fev1_bl", "sexM", "smoking_blCurrent"],
    "interpretation": ["Intercept", "Age", "ICS-Yes", "Eos Interaction", 
                      "Eos/10^9", "Per Exac", "FEV1/L", "Sex-Male", "Smoking-Current"]
})

# Create FastAPI app
app = FastAPI(
    title="Submodel Prediction API",
    description="API for making predictions using submodels.",
    version="1.0.0"
)

# Add CORS middleware
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Input model for prediction
class PredictionInput(BaseModel):
    data: Dict[str, Any]

# Request logging middleware
@app.middleware("http")
async def log_requests(request: Request, call_next):
    timestamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    logger.info(f"[{timestamp}] {request.method} {request.url.path}")
    response = await call_next(request)
    return response

@app.post("/predict")
async def predict(input_data: PredictionInput):
    try:
        # Convert input data to dataframe
        newdata = pd.DataFrame(input_data.data, index=[0])
        
        # Make predictions
        exac_modsev = predict_submodels(newdata, submodels_object_modsev, prediction_type="response")
        exac_sev = predict_submodels(newdata, submodels_object_sev, prediction_type="response")
        exac_pneum = predict_submodels(newdata, submodels_object_pneum, prediction_type="response")
        
        # Get missingness pattern
        pattern_info = missingness_pattern(newdata)
        tmp_pattern = pattern_info["tmp.pattern"]
        
        # Process coefficients
        coefs_modsev = coef_modsev["coefficients"].rename(columns={"coef": "modsev"})
        coefs_sev = coef_sev["coefficients"].rename(columns={"coef": "sev"})
        coefs_pneum = coef_pneum["coefficients"].rename(columns={"coef": "pneum"})
        
        coefs = pd.merge(coefs_modsev, coefs_sev, on=["var", "pattern"])
        coefs = pd.merge(coefs, coefs_pneum, on=["var", "pattern"])
        
        coefs["pattern"] = coefs["pattern"].astype(str)
        coefs = coefs[coefs["pattern"] == str(tmp_pattern[0])]
        
        coefs = pd.merge(coefs, interpretation, on="var")
        coefs = coefs[["interpretation", "modsev", "sev", "pneum"]]
        
        coefs["modsev"] = coefs["modsev"].apply(lambda x: f"{x:.3f}")
        coefs["sev"] = coefs["sev"].apply(lambda x: f"{x:.3f}")
        coefs["pneum"] = coefs["pneum"].apply(lambda x: f"{x:.3f}")
        
        # Return results
        return {
            "modsev": exac_modsev,
            "sev": exac_sev,
            "pneum": exac_pneum,
            "coefs": coefs.to_dict(orient="records")
        }
        
    except Exception as e:
        logger.error(f"Error in prediction: {str(e)}")
        raise HTTPException(status_code=400, detail=f"Error in prediction: {str(e)}")

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)
    
