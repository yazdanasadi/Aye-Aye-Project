import pandas as pd
import pickle
import os

def read_excel_file(filepath):
    if not os.path.exists(filepath):
        print("File not found.")
        return None
    with open(filepath, 'rb') as f:
        content = f.read(200)  
        if b',' in content:  
            return pd.read_csv(filepath)
    
    # If not comma-separated, convert Excel to CSV
    csv_file = filepath.replace('.xlsx', '.csv')
    if not os.path.exists(csv_file):
        excel_data = pd.read_excel(filepath)
        excel_data.to_csv(csv_file, index=False)
    
    df = pd.read_csv(csv_file)    
    with open(csv_file, 'rb') as f:
        encoding = pd.read_csv(f, encoding='utf-8').encoding    
    print("Encoding of CSV file:", encoding)
    return df

