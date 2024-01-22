import pandas as pd

# Load your two CSV files into pandas DataFrames
df1 = pd.read_csv('/Users/koppikarps/Python Development/lys_trans/files/apex/eBayes_results_LysoAPEX-RNC-MS.csv') 
df2 = pd.read_csv('/Users/koppikarps/Python Development/lys_trans/files/apex/gran_pos.csv') 

merged_df = pd.merge(df1, df2, on='Protein', how='outer')

merged_df.fillna('NA', inplace=True)

merged_df.to_csv('gran_align.csv', index=False)
