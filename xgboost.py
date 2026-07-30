from xml.parsers.expat import model

import shap
import numpy as np
import pandas as pd
from sklearn.metrics import accuracy_score
from sklearn.preprocessing import LabelEncoder  # Fixed import
from sklearn.model_selection import train_test_split
import lightgbm as lgb
import joblib
import json
import os

import shap
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt


import shap
import numpy as np
import matplotlib.pyplot as plt


def shap_summary_plot_per_class(model, X_test, label_encoder, background_size=100):

    import shap
    import matplotlib.pyplot as plt

    print("\n🔍 Creating SHAP summary plots per class...")

    background = X_test.sample(min(background_size, len(X_test)), random_state=42)

    explainer = shap.TreeExplainer(
        model,
        data=background,
        feature_perturbation="tree_path_dependent"
    )

    shap_values = explainer.shap_values(X_test)

    class_names = label_encoder.classes_

    for i, class_name in enumerate(class_names):

        print("\n" + "=" * 60)
        print(f"📊 SHAP Summary for class: {class_name}")

        # -------------------------------
        # extract class-specific SHAP
        # -------------------------------
        if isinstance(shap_values, list):
            sv = shap_values[i]
        else:
            sv = shap_values[:, :, i] if shap_values.ndim == 3 else shap_values

        shap.summary_plot(
            sv,
            X_test,
            show=True
        )

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
    
    # print("✅ Model saved to 'models/xgb/' folder")
    # print(f"   Files: lightgbm_model.txt, label_encoder.joblib, feature_columns.joblib")
    # print("🔍 Generating SHAP values for test set...")
    # explainer = shap.TreeExplainer(model)
    # shap_values = explainer.shap_values(X_test)

    # # Convert to readable format
    # shap_summary = []

    # # Use enumerate to get 0-based index
    # for row_idx, row_values in enumerate(X_test.values):
    #     row_shap = []
    #     for j, col in enumerate(feature_cols):
    #         if isinstance(shap_values, list):
    #             # Old SHAP format: list of arrays [n_samples, n_features]
    #             impact = shap_values[np.argmax(y_pred[row_idx])][row_idx, j]
    #         else:
    #             # New SHAP format: array of shape [n_samples, n_features, n_classes]
    #             impact = shap_values[row_idx, j, np.argmax(y_pred[row_idx])]
    #         row_shap.append({
    #             "feature": col,
    #             "value": float(row_values[j]),
    #             "impact": float(impact)
    #         })
    #     shap_summary.append(row_shap)
    # print("✅ SHAP explanations generated for test set.")

    # first_sample = shap_summary[0]
    # top_features = sorted(first_sample, key=lambda x: abs(x['impact']), reverse=True)[:]
    # print("\nTop 5 features influencing prediction for first test sample:")
    # for f in top_features:
    #     print(f" + {f['feature']} ({f['value']}) → {f['impact']:.4f}")
    X_small = X_test.sample(500, random_state=42)
    shap_summary_plot_per_class(model, X_small, label_encoder)

    return model, label_encoder, feature_cols, _

# Run training and saving
if __name__ == "__main__":
    train_and_save_lightgbm()