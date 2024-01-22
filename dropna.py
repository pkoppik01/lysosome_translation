import pandas as pd

# Load your CSV file into a pandas DataFrame
df = pd.read_csv('/Users/koppikarps/Python Development/lys_trans/files/topquad/ptq50.150.300.csv')  # Replace 'your_file.csv' with the actual filename/path for your CSV file

# Drop rows with NA values in any column
df_cleaned = df.dropna()

# Save the cleaned data to a new CSV file
df_cleaned.to_csv('cleanptq.csv', index=False)  # Replace 'cleaned_data.csv' with your desired output file name
