import pandas as pd

# Read the two CSV files
df1 = pd.read_csv('/Users/koppikarps/Python Development/lys_trans/p300_g2_Aimputed_contrast_1.19.24.csv')
df2 = pd.read_csv('p300_proteingroups_proteingenes.csv')

# Merge the dataframes based on the 'PG.ProteinGroups' column
merged_df = pd.merge(df1, df2, on='PG.ProteinGroups', how='inner')

# Write the merged dataframe to a new CSV file
merged_df.to_csv('merged_result.csv', index=False)
