import numpy as np
import pandas as pd
import lightgbm as lgb
import joblib
import shap
import matplotlib.pyplot as plt

def load_model():
    model = lgb.Booster(model_file='models/xgb/lightgbm_model.txt')
    plt.figure(figsize=(20, 10))
    lgb.plot_tree(
        model,                # sklearn LGBMClassifier or LGBMRegressor
        tree_index=0,         # first tree
        show_info=['split_gain', 'internal_value', 'internal_count', 'leaf_count']
    )
    plt.title("LightGBM Tree 0 (Graphviz)")
    plt.show()
    label_encoder = joblib.load('models/xgb/label_encoder.joblib')
    feature_cols = joblib.load('models/xgb/feature_columns.joblib')
    explainer = shap.TreeExplainer(model)
    return model, label_encoder, feature_cols, explainer

def explain_prediction(explainer, df, predicted_class, feature_cols):
    shap_values = explainer.shap_values(df)
    
    # ✅ Handle both formats
    if isinstance(shap_values, list):
        # Old format
        class_shap = shap_values[predicted_class][0]
    else:
        # New format (3D array)
        class_shap = shap_values[0, :, predicted_class]
    
    feature_values = df.iloc[0]
    
    contributions = []
    
    for i, col in enumerate(feature_cols):
        contributions.append({
            "feature": col,
            "value": float(feature_values[col]),
            "impact": float(class_shap[i])
        })
    
    contributions = sorted(contributions, key=lambda x: abs(x["impact"]), reverse=True)
    
    positive = [c for c in contributions if c["impact"] > 0][:]
    negative = [c for c in contributions if c["impact"] < 0][:]
    
    return {
        "top_positive": positive,
        "top_negative": negative
    }

def is_near_decision_boundary(probabilities, threshold=0.1):
    """
    Check if prediction is near decision boundary based on probabilities
    
    For 3 classes (0=Poor, 1=Standard, 2=Good):
    - Boundary 1: Between Poor(0) and Standard(1)
    - Boundary 2: Between Standard(1) and Good(2)
    
    A sample is "near boundary" if:
    1. The top two probabilities are close
    2. OR the winning probability is low
    """
    # Sort probabilities
    sorted_probs = np.sort(probabilities)[::-1]
    
    # Calculate gaps
    gap_top_two = sorted_probs[0] - sorted_probs[1]
    gap_all = sorted_probs[0] - sorted_probs[2]
    
    # Check conditions
    is_boundary = False
    boundary_type = None
    confidence_gap = gap_top_two
    
    # Condition 1: Top two probabilities are close
    if gap_top_two < threshold:
        is_boundary = True
        
        # Determine which boundary
        predicted_class = np.argmax(probabilities)
        second_class = np.argsort(probabilities)[-2]
        
        if abs(predicted_class - second_class) == 1:
            boundary_type = f"Between classes {predicted_class} and {second_class}"
        else:
            boundary_type = "Ambiguous (top classes not adjacent)"
    
    # Condition 2: Low winning probability
    elif sorted_probs[0] < 0.6:  # Less than 60% confidence
        is_boundary = True
        boundary_type = "Low confidence prediction"
    
    return is_boundary, boundary_type, confidence_gap

# Example usage
def check_boundary_for_customer(customer_data, boundary_threshold=0.1):
    """
    Check if a customer's prediction is near decision boundary
    """
    # Load model
    model, label_encoder, feature_cols, explainer = load_model()
    
    # Prepare data
    df = pd.DataFrame([customer_data])
    for col in feature_cols:
        if col not in df.columns:
            df[col] = 0
    df = df[feature_cols].fillna(0)
    
    # Get probabilities
    probabilities = model.predict(df)[0]
    predicted_class = np.argmax(probabilities)
    predicted_label = label_encoder.inverse_transform([predicted_class])[0]
    
    # Check boundary
    is_boundary, boundary_type, confidence_gap = is_near_decision_boundary(
        probabilities, boundary_threshold
    )
    
    explanation = explain_prediction(
        explainer, df, predicted_class, feature_cols
    )
        
    result = {
        'prediction': predicted_label,
        'prediction_code': int(predicted_class),
        'probabilities': {
            'Poor': float(probabilities[0]),
            'Standard': float(probabilities[1]),
            'Good': float(probabilities[2])
        },
        'confidence': float(np.max(probabilities)),
        'is_near_boundary': is_boundary,
        'boundary_info': boundary_type,
        'confidence_gap': float(confidence_gap),
        'boundary_distance': float(1 - confidence_gap),  # How close to boundary
        'explanation': explanation
    }
    
    return result

# Test with sample customer
if __name__ == "__main__":
    # Create a borderline customer (mix of features)
    borderline_customer = {
        'Age': 35,
        'Annual_Income': 45000,  # Middle income
        'Num_Bank_Accounts': 2,
        'Num_Credit_Card': 3,
        'Interest_Rate': 6,
        'Num_of_Loan': 1,
        'Delay_from_due_date': 30,
        'Num_of_Delayed_Payment': 1,
        'Changed_Credit_Limit': 3.0,
        'Num_Credit_Inquiries': 2,
        'Credit_Mix': 1,  # Mixed credit history
        'Outstanding_Debt': 1200,
        'Credit_Utilization_Ratio': 35.0,  # Middle utilization
        'Credit_History_Age': 150,
        'Total_EMI_per_month': 200,
        'Amount_invested_monthly': 100,
        'Monthly_Balance': 800
    }
    
    result = check_boundary_for_customer(borderline_customer)

    print("\n🔍 SHAP Explanation:")

    print("\n🟢 Factors pushing prediction UP:")
    for item in result['explanation']['top_positive']:
        print(f"  + {item['feature']} ({item['value']}) → {item['impact']:.4f}")

    print("\n🔴 Factors pushing prediction DOWN:")
    for item in result['explanation']['top_negative']:
        print(f"  - {item['feature']} ({item['value']}) → {item['impact']:.4f}")

    print("📊 Decision Boundary Analysis:")
    print(f"Prediction: {result['prediction']}")
    print(f"Confidence: {result['confidence']:.2%}")
    print(f"Near boundary: {result['is_near_boundary']}")
    
    if result['is_near_boundary']:
        print(f"⚠️  BOUNDARY DETECTED: {result['boundary_info']}")
        print(f"   Confidence gap: {result['confidence_gap']:.4f}")
        print(f"   Distance to boundary: {result['boundary_distance']:.4f}")
    
    print(f"\nProbabilities:")
    for cls, prob in result['probabilities'].items():
        print(f"  {cls}: {prob:.4f}")