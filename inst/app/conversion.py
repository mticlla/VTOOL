import json
import pandas as pd

def excel_to_json(filepath):
    df = pd.read_excel(filepath)
    df = df.fillna("TODO")
    outf =  "/".join(filepath.split(".")[:-1]) + ".json"
    translations = df.to_dict(orient="records")
    lang = df.columns.to_list()
    lang.pop("es")
    # needs to be first! 
    lang.insert(0, "es")
    data = dict(
        languages = lang,
        translation = translations
    )
    with open(outf, "w", encoding="utf-8") as f:
        json.dump(data, f, ensure_ascii=False, indent=4)

        
def json_to_excel(filepath):
    with open(filepath) as f:
        data = json.load(f)
    df = pd.DataFrame(data["translation"])
    outf = "/".join(filepath.split(".")[:-1]) + ".xlsx"
    df.to_excel(outf, index=False)

if __name__ == "__main__":
    import sys
    import os
    if len(sys.argv) < 2:
        print("Please choose a file to convert")
        exit(0)
    filepath = sys.argv[1]
    if not os.path.exists(filepath):
        print(f"File {filepath} does not exist")
        exit(0)
    
    ext = filepath.split(".")[-1]
    
    if ext == 'json':
        json_to_excel(filepath)
    elif ext == "xlsx":
        excel_to_json(filepath)
    else:
        raise ValueError(f"Filetype {ext} not supported")
    
