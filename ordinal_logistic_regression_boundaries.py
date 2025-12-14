import numpy as np
import pandas as pd
import joblib
from scipy.special import expit  # Sigmoid function

def analyze_decision_boundaries(model_dir='models'):
    """
    Analyze decision boundaries for ordinal logistic regression
    """
    
    # Load model components
    model = joblib.load(f'{model_dir}/ordinal_logistic_model.joblib')
    thetas = joblib.load(f'{model_dir}/thetas_mord.joblib')
    feature_cols = joblib.load(f'{model_dir}/feature_columns.joblib')
    mapping = joblib.load(f'{model_dir}/ordinal_mapping.joblib')
    
    print("=" * 60)
    print("ORDINAL LOGISTIC REGRESSION DECISION BOUNDARY ANALYSIS")
    print("=" * 60)
    
    # Extract intercepts and coefficients
    n_classes = 3  # Poor, Standard, Good
    n_boundaries = n_classes - 1  # 2 boundaries
    n_features = len(feature_cols)
    
    # The first n_boundaries values are intercepts
    intercepts = thetas[:n_boundaries]
    # The remaining values are feature coefficients
    coefficients = model.coef_
    
    print(f"\nNumber of classes: {n_classes}")
    print(f"Number of decision boundaries: {n_boundaries}")
    print(f"Number of features: {n_features}")
    
    print(f"\nIntercepts (boundary parameters):")
    for i, intercept in enumerate(intercepts):
        print(f"  Boundary {i+1} (between {list(mapping.keys())[i]} and {list(mapping.keys())[i+1]}): θ₀_{i+1} = {intercept:.6f}")
    
    print(f"\nFeature Coefficients:")
    for feat, coef in zip(feature_cols, coefficients):
        print(f"  {feat}: {coef:.6f}")
    
    return intercepts, coefficients, feature_cols

def predict_with_boundaries(x_scaled, intercepts, coefficients, threshold=0.5):
    """
    Make ordinal predictions manually using decision boundaries
    
    Parameters:
    -----------
    x_scaled : array-like, shape (n_samples, n_features)
        Scaled feature values
    intercepts : array, shape (n_boundaries,)
        Boundary intercepts
    coefficients : array, shape (n_features,)
        Feature coefficients
    threshold : float
        Probability threshold for boundary decisions
        
    Returns:
    --------
    predictions : array, shape (n_samples,)
        Predicted class labels (0, 1, 2, ...)
    boundary_distances : array, shape (n_samples, n_boundaries)
        Distance to each boundary
    probabilities : array, shape (n_samples, n_classes)
        Probability for each class
    """
    
    x_scaled = np.array(x_scaled)
    if x_scaled.ndim == 1:
        x_scaled = x_scaled.reshape(1, -1)
    
    n_samples = x_scaled.shape[0]
    n_boundaries = len(intercepts)
    n_classes = n_boundaries + 1
    
    # Calculate linear combination for each sample
    linear_pred = np.dot(x_scaled, coefficients)
    
    # Calculate boundary distances
    # For boundary i: distance = intercepts[i] - linear_pred
    boundary_distances = intercepts - linear_pred.reshape(-1, 1)
    
    # Calculate probabilities using proportional odds model
    # P(y ≤ k | x) = sigmoid(θ₀ₖ - θ·x)
    probabilities = np.zeros((n_samples, n_classes))
    
    for k in range(n_boundaries):
        # P(y ≤ k | x)
        prob_le_k = 1 / (1 + np.exp(-(intercepts[k] - linear_pred)))
        probabilities[:, k] = prob_le_k - (probabilities[:, k-1] if k > 0 else 0)
    
    # P(y = K-1 | x) = 1 - P(y ≤ K-2 | x)
    if n_boundaries > 0:
        probabilities[:, -1] = 1 - probabilities[:, :-1].sum(axis=1)
    
    # Make predictions based on probabilities
    predictions = np.argmax(probabilities, axis=1)
    
    return predictions, boundary_distances, probabilities

def is_on_decision_boundary(x_scaled, intercepts, coefficients, tolerance=1e-6):
    """
    Check if a point is on any decision boundary
    
    Parameters:
    -----------
    x_scaled : array-like
        Scaled feature values for one sample
    intercepts : array
        Boundary intercepts
    coefficients : array
        Feature coefficients
    tolerance : float
        Numerical tolerance
        
    Returns:
    --------
    on_boundary : bool
        True if on any boundary
    boundary_idx : int or None
        Which boundary (0-indexed)
    distances : array
        Distance to each boundary
    """
    
    # Calculate linear combination
    linear_pred = np.dot(x_scaled, coefficients)
    
    # Calculate distance to each boundary
    # boundary_distance = intercept - linear_pred
    # If distance ≈ 0, we're on that boundary
    distances = intercepts - linear_pred
    
    # Check if any distance is within tolerance
    on_boundary_mask = np.abs(distances) <= tolerance
    
    if np.any(on_boundary_mask):
        boundary_idx = np.where(on_boundary_mask)[0][0]
        return True, boundary_idx, distances
    else:
        return False, None, distances

# Example usage
def example_usage():
    # Load your data for testing
    df = pd.read_csv('resources/model_training/train_data_combined.csv')
    
    # Load model components
    model = joblib.load('models/ordinal_logistic_model.joblib')
    scaler = joblib.load('models/scaler.joblib')
    feature_cols = joblib.load('models/feature_columns.joblib')
    mapping = joblib.load('models/ordinal_mapping.joblib')
    
    # Analyze boundaries
    intercepts, coefficients, features = analyze_decision_boundaries()
    
    # Take a test sample
    test_sample = df.iloc[0:110000].copy()
    test_sample_features = test_sample[feature_cols].fillna(test_sample[feature_cols].median())
    test_sample_scaled = scaler.transform(test_sample_features)
    
    print("\n" + "="*60)
    print("TESTING A SPECIFIC SAMPLE")
    print("="*60)
    
    for entry in test_sample_scaled:
        # Check if this sample is on boundary
        on_boundary, boundary_idx, distances = is_on_decision_boundary(
            entry, intercepts, coefficients, tolerance=0.01
        )

        print(f"\nSample details:")
        print(f"Scaled features: {entry}")
        print(f"\nDistance to boundaries:")
        for i, dist in enumerate(distances):
            boundary_name = f"{list(mapping.keys())[i]}-{list(mapping.keys())[i+1]}"
            print(f"  Boundary {i+1} ({boundary_name}): distance = {dist:.6f}")

        if on_boundary:
            boundary_names = list(mapping.keys())
            print(f"\n⚠️  SAMPLE IS ON DECISION BOUNDARY!")
            print(f"   Between {boundary_names[boundary_idx]} and {boundary_names[boundary_idx+1]}")
        else:
            print(f"\n✓ Sample is NOT on decision boundary")

    # Get full prediction details
    predictions, boundary_dists, probs = predict_with_boundaries(
        test_sample_scaled, intercepts, coefficients
    )

    predicted_class = list(mapping.keys())[predictions[0]]
    actual_class = test_sample['Credit_Score'].values[0]

    print(f"\nPrediction:")
    print(f"  Actual: {actual_class}")
    print(f"  Predicted: {predicted_class}")
    print(f"  Probabilities: Poor={probs[0][0]:.3f}, Standard={probs[0][1]:.3f}, Good={probs[0][2]:.3f}")

# Run the analysis
if __name__ == "__main__":
    # First, analyze your boundaries
    example_usage()