import numpy as np
import pandas as pd
from sklearn.metrics import accuracy_score
from sklearn.preprocessing import LabelEncoder  # Fixed import
from sklearn.model_selection import train_test_split
import lightgbm as lgb
import joblib
import json
import os

def train_and_save_lightgbm():
    df = pd.read_csv('resources/model_training/train_data_combined.csv')
    
    # Prepare data
    label_encoder = LabelEncoder()
    y = label_encoder.fit_transform(df['Credit_Score'])
    
    feature_cols = [col for col in df.columns if col != 'Credit_Score']
    X = df[feature_cols].fillna(df[feature_cols].median())
    
    # Split
    X_train, X_test, y_train, y_test = train_test_split(
        X, y, test_size=0.15, random_state=100, stratify=y
    )
    
    # Create LightGBM dataset
    train_data = lgb.Dataset(X_train, label=y_train)
    test_data = lgb.Dataset(X_test, label=y_test, reference=train_data)
    
    # Parameters
    params = {
        'objective': 'multiclass',
        'num_class': 3,
        'metric': 'multi_logloss',
        'boosting_type': 'gbdt',
        'learning_rate': 0.05,
        'num_leaves': 31,
        'max_depth': -1,
        'min_child_samples': 20,
        'reg_alpha': 0.1,
        'reg_lambda': 0.1,
        'random_state': 100,
        'n_jobs': -1
    }
    
    # Train
    model = lgb.train(
        params,
        train_data,
        valid_sets=[test_data],
        num_boost_round=1000,
        callbacks=[lgb.early_stopping(50), lgb.log_evaluation(50)]
    )
    
    # Predict
    y_pred_proba = model.predict(X_test, num_iteration=model.best_iteration)
    y_pred = np.argmax(y_pred_proba, axis=1)
    
    accuracy = accuracy_score(y_test, y_pred)
    print(f"LightGBM Accuracy: {accuracy:.4f}")
    
    # ⭐⭐⭐ SAVE THE MODEL ⭐⭐⭐
    os.makedirs('models/xgb', exist_ok=True)
    
    # Save model
    model.save_model('models/xgb/lightgbm_model.txt')
    
    # Save label encoder
    joblib.dump(label_encoder, 'models/xgb/label_encoder.joblib')
    
    # Save feature columns
    joblib.dump(feature_cols, 'models/xgb/feature_columns.joblib')
    
    # Save metadata
    metadata = {
        'accuracy': float(accuracy),
        'n_classes': len(label_encoder.classes_),
        'classes': label_encoder.classes_.tolist(),
        'n_features': len(feature_cols)
    }
    with open('models/xgb/metadata.json', 'w') as f:
        json.dump(metadata, f, indent=2)
    
    print("✅ Model saved to 'models/xgb/' folder")
    print(f"   Files: lightgbm_model.txt, label_encoder.joblib, feature_columns.joblib")
    
    return model, label_encoder, feature_cols

# Run training and saving
if __name__ == "__main__":
    train_and_save_lightgbm()