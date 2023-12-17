import pandas as pd
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler, LabelEncoder
from sklearn.metrics import accuracy_score, classification_report
from tensorflow import keras
from keras.optimizers import Adamax # tried SGD (0.50), RMSprop(0.70, LR=0.01), Adam(0.69,LR=0.001)
from keras.models import Sequential
from keras.layers import Dense
import seaborn as sns
import matplotlib.pyplot as plt


def main() -> None:
    logistic_regression()
    pass

def logistic_regression() -> None:
    train_data = pd.read_csv("resources/model_training/train_model.csv")
    test_data = pd.read_csv("resources/model_training/test_model.csv")
    
    X_train = train_data.drop("Credit_Score", axis=1)
    y_train = train_data["Credit_Score"]
    
    X_test = test_data.drop("Credit_Score", axis=1)
    y_test = test_data["Credit_Score"]
    
    # Standardize the input features
    scaler = StandardScaler()
    X_train_scaled = scaler.fit_transform(X_train)
    X_test_scaled = scaler.transform(X_test)
    
    model = LogisticRegression(max_iter=1000, random_state=42)
    model.fit(X_train_scaled, y_train)
    
    # Make predictions on the test set
    y_pred = model.predict(X_test_scaled)

    # Evaluate the model
    accuracy = accuracy_score(y_test, y_pred)
    print(f"Accuracy on the test set: {accuracy}")

    # Display classification report
    print("Classification Report:")
    print(classification_report(y_test, y_pred))
    
    coefficients = model.coef_
    feature_names = X_train.columns
    coefficients_df = pd.DataFrame({'Feature': feature_names, 'Coefficient': coefficients[0]})
    coefficients_df = coefficients_df.reindex(coefficients_df['Coefficient'].abs().sort_values(ascending=False).index)
    coefficients_df['Is_Positive'] = coefficients_df['Coefficient'] >= 0
    coefficients_df['Coefficient'] = coefficients_df['Coefficient'].abs()
    # Display the most valuable variablez
    print(coefficients_df)
    plt.figure(figsize=(10,6))
    palette = {False:'red',True:'green'}
    sns.barplot(x='Feature',y='Coefficient',palette=palette,data=coefficients_df,hue='Is_Positive')
    plt.xlabel('Category')
    plt.ylabel('Absolute value')
    plt.title('Most valuable values for linear regression model')
    plt.xticks(rotation=45)
    
    plt.show()
    plt.savefig('plots/Boxplot_Valuable_Variables_Model.png')
    

def neural_network() -> None:
    train_data = pd.read_csv("resources/model_training/train_model.csv")
    # Load the test data
    test_data = pd.read_csv("resources/model_training/test_model.csv")

    # Encode the "Credit_Score" column to numerical values
    label_encoder = LabelEncoder()
    train_data['Credit_Score'] = label_encoder.fit_transform(train_data['Credit_Score'])

    # Separate features (X) and target variable (y) for training data
    X_train = train_data.drop("Credit_Score", axis=1)
    y_train = train_data["Credit_Score"]

    # Standardize numerical features using StandardScaler
    scaler = StandardScaler()
    X_train_scaled = scaler.fit_transform(X_train)

    # Split the data into training and validation sets
    X_train_split, X_val_split, y_train_split, y_val_split = train_test_split(X_train_scaled, y_train, test_size=0.2, random_state=42)

    # Build a neural network model
    model = Sequential()
    model.add(Dense(64, input_dim=21, activation='sigmoid'))
    model.add(Dense(32, activation='sigmoid'))
    model.add(Dense(16, activation='sigmoid'))
    model.add(Dense(3, activation='softmax'))

    # Compile the model
    optimizer = Adamax(learning_rate=0.01)
    model.compile(loss='sparse_categorical_crossentropy', optimizer=optimizer, metrics=['accuracy'])

    # Train the model
    model.fit(X_train_split, y_train_split, epochs=5, batch_size=32, validation_data=(X_val_split, y_val_split))

    # Evaluate the model on the test data
    test_data['Credit_Score'] = label_encoder.transform(test_data['Credit_Score'])
    X_test = test_data.drop("Credit_Score", axis=1)
    X_test_scaled = scaler.transform(X_test)
    y_test = test_data["Credit_Score"].to_numpy()

    # Make predictions
    y_pred = model.predict(X_test_scaled)
    y_pred_labels = label_encoder.inverse_transform(y_pred.argmax(axis=1))
    # Evaluate accuracy on the test set
    
    accuracy = accuracy_score(y_test, label_encoder.transform(y_pred_labels))
    print(f"Accuracy on the test set: {accuracy}")

    # Add predicted labels to the test_data DataFrame
    test_data['Predicted_Credit_Score'] = label_encoder.inverse_transform(y_pred.argmax(axis=1))

    # Save the DataFrame with predicted values to a new CSV file
    test_data.to_csv("test_with_predictions.csv", index=False)  
    # print(classification_report(y_test, y_pred))
    return



main()