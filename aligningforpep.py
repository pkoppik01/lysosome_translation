import pandas as pd

# Load the first CSV
df1 = pd.read_csv('/Users/koppikarps/Python Development/lys_trans/p300_g2_Aimputed_contrast_1.19.24.csv', sep=',')  # Adjust the file path and separator accordingly

# Load the second CSV with only the 'PG.Genes' column
df2 = pd.read_csv('/Users/koppikarps/Python Development/lys_trans/p300_proteingroups_proteingenes.csv')['PG.ProteinGroups']  # Adjust the file path and column name accordingly

# Filter rows in the first CSV based on genes present in the second CSV
#result_df = df1[df1['PG.ProteinGroups'].isin(df2)]
result2 = df2[df1['PG.ProteinGroups'].isin(df2)]

# Save the result to a new CSV
#result_df.to_csv('result.csv', index=False)  # Adjust the file path accordingly
result2.to_csv('result2.csv', index=False)
