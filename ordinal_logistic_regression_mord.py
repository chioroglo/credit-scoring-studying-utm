import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import classification_report, confusion_matrix, accuracy_score
import matplotlib.pyplot as plt
import seaborn as sns
from mord import LogisticAT  # For ordinal logistic regression
import joblib
import os
import warnings
warnings.filterwarnings('ignore')

def main() -> None:
    analyze_decision_boundaries()
    return
    train_logistic_regression()
        # Take a sample from test data
    df = pd.read_csv('resources/model_training/train_data_combined.csv')

    X_input = df.iloc[109910:109920:1].copy()
    
    predictions, probs = predict_new_data(X_input)

    for i, pred in enumerate(predictions):
        print(f"Sample {i+1}: Predicted = {pred}, Actual = {X_input['Credit_Score'].values[i]}")
        if probs is not None:
            print(f"  Probabilities: Poor={probs[i][0]:.3f}, Standard={probs[i][1]:.3f}, Good={probs[i][2]:.3f}")
    pass


def train_logistic_regression() -> None:
    # Load your data
    df = pd.read_csv('resources/model_training/train_data_combined.csv')

    # Display basic info
    print("Dataset shape:", df.shape)
    print("\nTarget distribution:")
    print(df['Credit_Score'].value_counts())

    # Step 1: Define ordinal mapping for target
    ordinal_mapping = {'Poor': 0, 'Standard': 1, 'Good': 2}
    df['Credit_Score_encoded'] = df['Credit_Score'].map(ordinal_mapping)

    # Step 2: Identify feature columns
    exclude_cols = ['Credit_Score', 'Credit_Score_encoded']
    feature_cols = [col for col in df.columns if col not in exclude_cols]
    print(f"\nNumber of features: {len(feature_cols)}")

    # Step 3: Separate features and target
    X = df[feature_cols]
    y = df['Credit_Score_encoded']

    # Step 4: Handle missing values
    X = X.fillna(X.median())
    y = y.fillna(y.mode()[0])

    # Step 5: Split the data
    X_train, X_test, y_train, y_test = train_test_split(
        X, y, test_size=0.15, random_state=100, stratify=y)

    print(f"\nTraining set shape: {X_train.shape}")
    print(f"Test set shape: {X_test.shape}")

    # Step 6: Scale the features
    scaler = StandardScaler()
    X_train_scaled = scaler.fit_transform(X_train)
    X_test_scaled = scaler.transform(X_test)

    X_train_scaled = pd.DataFrame(X_train_scaled, columns=feature_cols)
    X_test_scaled = pd.DataFrame(X_test_scaled, columns=feature_cols)

    # Step 7: Train Ordinal Logistic Regression
    print("\n" + "="*25)
    print("Training Ordinal Logistic Regression")
    print("="*50)
    model = LogisticAT(alpha=0, max_iter=1000)  # No regularization
    model.fit(X_train_scaled, y_train)

    # Step 8: Make predictions
    y_pred = model.predict(X_test_scaled)

    # Step 9: Evaluate the model
    print("\nModel Evaluation:")
    print("-" * 30)

    accuracy = accuracy_score(y_test, y_pred)
    print(f"Overall Accuracy: {accuracy:.4f}")
    
    # Convert back to labels for reporting
    y_pred_labels = [list(ordinal_mapping.keys())[list(ordinal_mapping.values()).index(val)] for val in y_pred]
    y_test_labels = [list(ordinal_mapping.keys())[list(ordinal_mapping.values()).index(val)] for val in y_test]

    print("\nClassification Report:")
    print(classification_report(y_test_labels, y_pred_labels, target_names=['Poor', 'Standard', 'Good']))

    print("\nConfusion Matrix")
    cm = confusion_matrix(y_test, y_pred, labels=[0, 1, 2])
    print(cm)
    # Plot the confusion matrix
    plt.figure(figsize=(8, 6))
    sns.heatmap(cm, annot=True, fmt='d', cmap='Blues',
                xticklabels=['Poor', 'Standard', 'Good'],
                yticklabels=['Poor', 'Standard', 'Good'])
    plt.xlabel('Predicted')
    plt.ylabel('Actual')
    plt.title('Confusion Matrix LOGISTIC REGRESSION')
    plt.show()


    # Step 10: Save all components as joblib files
    os.makedirs('models', exist_ok=True)
    
    # Save with .joblib extension
    joblib.dump(model.theta_, 'models/thetas_mord.joblib')
    joblib.dump(model, 'models/ordinal_logistic_model.joblib')
    joblib.dump(scaler, 'models/scaler.joblib')
    joblib.dump(ordinal_mapping, 'models/ordinal_mapping.joblib')
    joblib.dump(feature_cols, 'models/feature_columns.joblib')

    print("\n" + "="*50)
    print("MODEL SAVED AS JOBLIB FILES")
    print("="*50)
    print("Files saved in 'models/' directory:")
    print("✓ ordinal_logistic_model.joblib - Trained ordinal model")
    print("✓ scaler.joblib - Feature scaler")
    print("✓ ordinal_mapping.joblib - Label encoding")
    print("✓ feature_columns.joblib - Feature column names")

    # Step 11: Load and test the saved model
    print("\n" + "="*25)
    print("Testing Saved Model Load")
    print("="*25)

    # Load all components
    loaded_model = joblib.load('models/ordinal_logistic_model.joblib')
    loaded_scaler = joblib.load('models/scaler.joblib')
    loaded_mapping = joblib.load('models/ordinal_mapping.joblib')
    loaded_features = joblib.load('models/feature_columns.joblib')

    # Make prediction with loaded model
    X_test_sample = X_test_scaled.iloc[0:1]
    prediction_encoded = loaded_model.predict(X_test_sample)
    prediction_label = list(loaded_mapping.keys())[list(loaded_mapping.values()).index(prediction_encoded[0])]

    print(f"Test prediction: {prediction_label}")
    print("Model loaded successfully!")

# Step 12: Create a complete prediction function
def predict_new_data(new_data_df, model_dir='models'):
    """
    Predict credit scores for new data
    
    Parameters:
    -----------
    new_data_df : pandas DataFrame
        New data with the same features as training
    model_dir : str
        Directory containing the joblib files
        
    Returns:
    --------
    predictions : list
        List of predicted credit scores
    probabilities : numpy array (if available)
        Prediction probabilities for each class
    """
    
    # Load all components
    model = joblib.load(f'{model_dir}/ordinal_logistic_model.joblib')
    scaler = joblib.load(f'{model_dir}/scaler.joblib')
    mapping = joblib.load(f'{model_dir}/ordinal_mapping.joblib')
    feature_cols = joblib.load(f'{model_dir}/feature_columns.joblib')
    thetas = joblib.load(f'{model_dir}/thetas_mord.joblib')
    
    # Ensure new data has all required features
    missing_features = set(feature_cols) - set(new_data_df.columns)
    if missing_features:
        raise ValueError(f"Missing features in new data: {missing_features}")
    
    # Reorder columns to match training
    new_data_df = new_data_df[feature_cols]
    
    # Handle missing values
    new_data_df = new_data_df.fillna(new_data_df.median())
    
    # Scale features
    new_data_scaled = scaler.transform(new_data_df)
    
    # Predict
    predictions_encoded = model.predict(new_data_scaled)
    
    # Convert to labels
    predictions = []
    for code in predictions_encoded:
        predictions.append(list(mapping.keys())[list(mapping.values()).index(code)])
    
    # Get probabilities if available
    probabilities = None
    try:
        probabilities = model.predict_proba(new_data_scaled)
    except:
        pass
    
    return predictions, probabilities



main()