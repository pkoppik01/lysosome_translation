import pandas as pd

# Read the first CSV file
df1 = pd.read_csv('/Users/koppikarps/Python Development/lys_trans/files/p300overlayfiles/50150300/50_150.csv')

# Read the second CSV file with one column
df2 = pd.read_csv('/Users/koppikarps/Python Development/lys_trans/files/p300overlayfiles/50150300/300.csv')
print(df2)

# Merge the two dataframes based on 'PG.Genes'
merged_df = pd.merge(df1, df2, on='PG.Genes')

print(merged_df)

# Create a new column 'NES Assoc' and populate it with 1 if 'PG.Genes_check' is not NaN, else 0
#merged_df['tc'] = merged_df['PG.Genes_check'].notna().astype(int)

# Drop the temporary column 'PG.Genes_check'
#merged_df = merged_df.drop(columns=['PG.Genes_check'])

# Print or save the resulting dataframe
print(merged_df)

# Save the result to a new CSV file
merged_df.to_csv('50_150_300.csv', index=False)
