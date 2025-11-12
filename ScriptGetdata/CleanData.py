import logging
import pandas as pd
import re
import os
from striprtf.striprtf import rtf_to_text
from multiprocessing import Pool
from datetime import datetime
from pathlib import Path
from typing import Optional, Tuple, List

# Setup logging
logging.basicConfig(filename='./CleanData.log', level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')


def read_rtf(file_path: Path) -> Tuple[str, str]:
    """Read an RTF file and return its plain text and disease name (parent folder)."""
    try:
        with file_path.open('r', encoding='utf-8', errors='ignore') as f:
            rtf_content = f.read()
    except Exception:
        # fallback: open as binary and decode best-effort
        with file_path.open('rb') as f:
            rtf_content = f.read().decode('latin-1', errors='ignore')

    text = rtf_to_text(rtf_content)
    disease = file_path.parent.name
    return text, disease


def get_year(text: str) -> Optional[int]:
    year_pattern = r'\b(19\d{2}|20\d{2}|25\d{2})\b'
    years = re.findall(year_pattern, text)
    if not years:
        return None
    current_year = datetime.now().year
    years = [int(y) for y in years if int(y) <= current_year]
    return years[0] if years else None


def clean_text(text: str) -> Tuple[List[List[str]], Optional[int]]:
    lines = text.split('\n')
    data_lines = [line for line in lines if line.strip()]
    data: List[List[str]] = []
    for line in data_lines:
        parsed_line = [x.strip() for x in line.split('\t')]
        if not parsed_line:
            continue
        # drop leading empty cell
        if parsed_line and parsed_line[0] == '':
            parsed_line = parsed_line[1:]
        # normalize Zone tokens
        if parsed_line and parsed_line[0] in ('Zone', 'Zone:'):
            if len(parsed_line) > 1:
                parsed_line[1] = 'Zone:' + parsed_line[1]
            parsed_line = parsed_line[1:]
        if parsed_line:
            data.append(parsed_line)

    year = get_year(text)
    # guard: if not enough lines, return empty
    if len(data) >= 3:
        return data[1:-1], year
    return data, year


def fill_name_ac(col_names: List[str]) -> List[str]:
    cleaned_names: List[str] = []
    for i, name in enumerate(col_names):
        n = name.strip()
        if 'Area' in n:
            cleaned_names.append('Areas')
        elif 'Total' in n:
            cleaned_names.append('Total')
        elif '<1' in n or '<1ปี' in n:
            cleaned_names.append('<1')
        elif '65+' in n:
            cleaned_names.append('65+')
        elif 'ไม่ทรา' in n:
            cleaned_names.append('Unknown')
        elif '+' in n:
            current = n.strip('+').strip()
            try:
                next_age = int(re.sub(r'\D', '', col_names[i + 1]))
                cleaned_names.append(f"{current}-{next_age - 1}")
            except Exception:
                cleaned_names.append(f"{current}-{current}")
        elif n.endswith('-'):
            current = n.strip('-').strip()
            try:
                next_age = int(re.search(r'\d+', col_names[i + 1]).group())
                cleaned_names.append(f"{current}-{next_age - 1}")
            except Exception:
                cleaned_names.append(current)
        else:
            cleaned_names.append(n)

    # best-effort fix
    try:
        total_index = cleaned_names.index('Total')
        under_one_index = cleaned_names.index('<1')
        if under_one_index - total_index == 2:
            cleaned_names[total_index + 1] = '0'
    except ValueError:
        pass

    return cleaned_names


def likely_table(row: List[str]) -> bool:
    numeric_count = sum(str(x).replace('.', '', 1).isdigit() for x in row if x)
    return numeric_count < max(1, len(row) / 2)


def text_to_dataframe_ac(data: List[List[str]], year: Optional[int], disease: str) -> pd.DataFrame:
    if not data:
        return pd.DataFrame(columns=['Areas', 'Age', 'Cases', 'Year', 'Disease'])

    data = [[re.sub('ปี', '', x) for x in row] for row in data]
    try:
        df = pd.DataFrame(data[1:], columns=data[0])
    except Exception:
        # fallback: create empty
        return pd.DataFrame(columns=['Areas', 'Age', 'Cases', 'Year', 'Disease'])

    df = df.loc[:, df.apply(lambda col: not all(x == "" for x in col))]
    df.columns = fill_name_ac(list(df.columns))
    df = df[~df.apply(likely_table, axis=1)]
    if 'Areas' not in df.columns:
        return pd.DataFrame(columns=['Areas', 'Age', 'Cases', 'Year', 'Disease'])
    df = df.melt(id_vars=['Areas'], var_name='Age', value_name='Cases')
    df['Year'] = year
    df['Disease'] = disease
    df['Cases'] = df['Cases'].astype(str).str.replace(',', '').replace('', '0')
    df['Cases'] = pd.to_numeric(df['Cases'], errors='coerce').fillna(0).astype(int)
    return df[['Areas', 'Age', 'Cases', 'Year', 'Disease']]


def text_to_dataframe_mcd(data: List[List[str]], year: Optional[int], disease: str) -> pd.DataFrame:
    if len(data) < 2:
        return pd.DataFrame()
    headers = data[0]
    labels = data[1] if len(data) > 1 else []
    adjusted_headers = ['Areas']
    for i, header in enumerate(headers[1:]):
        left = labels[i * 2 - 2] if i * 2 - 2 >= 0 and i * 2 - 2 < len(labels) else ''
        right = labels[i * 2 - 1] if i * 2 - 1 >= 0 and i * 2 - 1 < len(labels) else ''
        adjusted_headers.append(f'{header}_{left}')
        adjusted_headers.append(f'{header}_{right}')

    df = pd.DataFrame(data[2:], columns=adjusted_headers)
    df = df.loc[:, df.apply(lambda col: not all(x == "" for x in col))]
    df = df.melt(id_vars=['Areas'], var_name='Period_Type', value_name='Count')
    split = df['Period_Type'].str.split('_', expand=True)
    if split.shape[1] >= 2:
        df[['Month', 'Type']] = split[[0, 1]]
    else:
        df[['Month']] = split[[0]]
        df['Type'] = ''
    df.drop('Period_Type', axis=1, inplace=True)
    df['Type'] = df['Type'].apply(lambda x: 'Cases' if 'cas' in str(x).lower() else ('Deaths' if 'dea' in str(x).lower() else x))
    df['Year'] = year
    df['Disease'] = disease
    df = df.dropna()
    df['Count'] = pd.to_numeric(df['Count'], errors='coerce')
    df = df.dropna(subset=['Count'])
    df['Count'] = df['Count'].astype(int)
    return df


def fill_name_rate(col_names: List[str]) -> List[str]:
    cleaned_names: List[str] = []
    for name in col_names:
        n = name.strip()
        if 'area' in n.lower():
            cleaned_names.append('Areas')
        elif 'จำนวน' in n:
            cleaned_names.append('Population')
        elif 'cas' in n.lower():
            cleaned_names.append('Cases')
        elif 'orbidity' in n.lower():
            cleaned_names.append('Incidence')
        elif 'dea' in n.lower():
            cleaned_names.append('Deaths')
        elif 'ortality' in n.lower():
            cleaned_names.append('Mortality')
        elif 'cfr' in n.upper():
            cleaned_names.append('CFR')
        else:
            cleaned_names.append(n)

    # filter to useful columns
    cleaned_names = [name for name in cleaned_names if name in ['Areas', 'Cases', 'Incidence', 'Deaths', 'Mortality', 'CFR', 'Population']]
    return cleaned_names


def text_to_dataframe_rate(data: List[List[str]], year: Optional[int], disease: str) -> pd.DataFrame:
    if not data:
        return pd.DataFrame()
    headers = data[0]
    headers = fill_name_rate(headers)
    data_rows = [row for row in data if len(row) == len(headers)]
    if not data_rows:
        return pd.DataFrame()
    df = pd.DataFrame(data_rows, columns=headers)
    df = df.loc[:, df.apply(lambda col: not all(x == "" for x in col))]
    df = df[~df.apply(lambda row: likely_table(list(row)), axis=1)]
    df['Year'] = year
    df['Disease'] = disease
    return df


def _process_disease_mode(disease: str, mode: str) -> Tuple[str, str, str]:
    """Process one disease for a given mode ('ac','ad','mcd','rate')."""
    try:
        logging.info(f'Start processing {mode} for {disease}')
        data_root = Path(__file__).resolve().parents[1] / 'Data' / 'GetData'
        base = data_root / disease
        if not base.is_dir():
            logging.info(f'No folder for {disease}, skipping {mode}')
            return (disease, mode, 'no_folder')

        prefix = {'ac': 'ac_', 'ad': 'ad_', 'mcd': 'mcd_', 'rate': 'rate_'}[mode]
        file_list = [f for f in os.listdir(base) if f.startswith(prefix)]
        if not file_list:
            logging.info(f'No files for {disease} with prefix {prefix}')
            return (disease, mode, 'no_files')

        frames = []
        for file in file_list:
            file_path = base / file
            try:
                text, name = read_rtf(file_path)
                data, year = clean_text(text)
                if mode in ('ac', 'ad'):
                    df = text_to_dataframe_ac(data, year, name)
                    frames.append(df)
                elif mode == 'mcd':
                    df = text_to_dataframe_mcd(data, year, name)
                    frames.append(df)
                else:  # rate
                    df = text_to_dataframe_rate(data, year, name)
                    frames.append(df)
            except Exception as e:
                logging.exception(f'Failed to parse {file_path}: {e}')
                continue

        if not frames:
            return (disease, mode, 'no_parsed_frames')

        outcome = pd.concat(frames, ignore_index=True)
        out_dir = Path(__file__).resolve().parents[1] / 'Data' / 'CleanData'
        out_dir.mkdir(parents=True, exist_ok=True)
        out_name = f'{disease}_{mode}.csv'
        outcome.to_csv(out_dir / out_name, index=False)
        logging.info(f'Finished processing {mode} for {disease}')
        return (disease, mode, 'success')
    except Exception as e:
        logging.exception(f'Error processing {mode} for {disease}: {e}')
        return (disease, mode, f'error: {e}')


def process_mode_parallel(mode: str, workers: Optional[int] = None):
    data_root = Path(__file__).resolve().parents[1] / 'Data' / 'GetData'
    if not data_root.exists():
        logging.error(f'Data root not found: {data_root}')
        return []
    disease_list = [p.name for p in data_root.iterdir() if p.is_dir()]
    logging.info(f'Start processing {mode} data with {workers or os.cpu_count()} workers')
    args = [(d, mode) for d in disease_list]
    with Pool(processes=workers) as p:
        results = p.starmap(_process_disease_mode, args)
    success = sum(1 for r in results if r[2] == 'success')
    logging.info(f'Completed mode {mode}: {success}/{len(results)} succeeded')
    return results


if __name__ == '__main__':
    # Windows-safe multiprocessing entrypoint
    process_mode_parallel('ac')
    process_mode_parallel('ad')
    process_mode_parallel('mcd')
    process_mode_parallel('rate')


logging.info('Finished processing all rate data')
