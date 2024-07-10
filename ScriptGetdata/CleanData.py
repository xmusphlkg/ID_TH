import logging
import pandas as pd
import re
import os
from striprtf.striprtf import rtf_to_text
from datetime import datetime

# Setup logging
logging.basicConfig(filename='./CleanData.log', level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

## Function to clean the data
def read_rtf(file_path):
    with open(file_path, 'r') as file:
        rtf_content = file.read()
    text = rtf_to_text(rtf_content)

    # get name of disease from file name
    disease = file_path.split('/')[-2]

    return text, disease

def get_year(text):
    year_pattern = r'\b(19\d{2}|20\d{2}|25\d{2})\b'
    years = re.findall(year_pattern, text)

    # find year lower than current year
    current_year = datetime.now().year
    years = [year for year in years if int(year) <= current_year]

    return years[0]

def clean_text(text):
    lines = text.split('\n')
    data_lines = [line for line in lines if line.strip()]
    data = []
    for line in data_lines:
        parsed_line = [x.strip() for x in line.split('\t')]
        # if first element of line is empty, remove it
        if not parsed_line[0]:
            parsed_line = parsed_line[1:]
        # if first element of line is Zone, combine it with next element
        if parsed_line[0] == 'Zone':
            parsed_line[1] = 'Zone:' + parsed_line[1]
            parsed_line = parsed_line[1:]
        #
        if parsed_line[0] == 'Zone:':
            parsed_line[1] = 'Zone:' + parsed_line[1]
            parsed_line = parsed_line[1:]
        # if first element of line is Age, combine it with next element
        if parsed_line:
            data.append(parsed_line)

    # remove first and last line
    year = get_year(text)
    data = data[1:-1]
    return data, year

## ac data
def fill_name(col_names):
    cleaned_names = []

    for i, name in enumerate(col_names):
        name = name.strip()
        if 'Area' in name:
            cleaned_names.append('Areas')
        elif 'Total' in name:
            cleaned_names.append('Total')
        elif '<1' in name or '<1ปี' in name:
            cleaned_names.append('<1')
        elif '65+' in name:
            cleaned_names.append('65+')
        elif 'ไม่ทรา' in name:
            cleaned_names.append('Unknown')
        elif '+' in name:
            current = name.strip('+').strip()
            if i + 1 < len(col_names) and '+' in col_names[i + 1] and not col_names[i + 1].endswith('+'):
                try:
                    next_age = int(re.sub(r'\D', '', col_names[i + 1]))
                    cleaned_names.append(f"{current}-{next_age - 1}")
                except ValueError:
                    cleaned_names.append(f"{current}-{current}")  # if conversion fails, use current as is
            else:
                cleaned_names.append(f"{current}-{current}")  # No next range, so make it a self range
        elif name.endswith('-'):
            current = name.strip('-').strip()
            if i + 1 < len(col_names):
                # Extract the first sequence of digits from the next column's name
                next_age_match = re.search(r'\d+', col_names[i + 1])
                if next_age_match:
                    next_age = int(next_age_match.group())
                    cleaned_names.append(f"{current}-{next_age - 1}")
                else:
                    cleaned_names.append(current)  # Use current if no digits found
            else:
                cleaned_names.append(current)  # Use current as is if no next info
        else:
            cleaned_names.append(name)  # Directly use clear ranges like '10-14'
    
    total_index = cleaned_names.index('Total')
    under_one_index = cleaned_names.index('<1')
    if under_one_index - total_index == 2:
        cleaned_names[total_index + 1] = '0'

    return cleaned_names

def likely_table(row):
    numeric_count = sum(x.replace('.', '', 1).isdigit() for x in row if x)
    
    return numeric_count < len(row) / 2
    
def text_to_dataframe(data, year, disease):
    
    data = [[re.sub('ปี', '', x) for x in row] for row in data]

    # convert to dataframe
    data = pd.DataFrame(data[1:], columns=data[0])
    data = data.loc[:, data.apply(lambda col: not all(x == "" for x in col))]
    data.columns = fill_name(data.columns)

    # remove rows which likely contain header
    data = data[~data.apply(likely_table, axis=1)]

    # convert to longer format
    data = data.melt(id_vars=['Areas'], var_name='Age', value_name='Cases')
    data['Year'] = year
    data['Disease'] = disease

    # remove spaces in Cases column and convert to integer
    data['Cases'] = data['Cases'].str.replace(',', '')
    data['Cases'] = data['Cases'].astype(int)

    return data

disease_list = os.listdir('../Data/GetData/')
logging.info('Start processing ac data')
for disease in disease_list:
    logging.info(f'Start processing {disease}')
    file_list = os.listdir('../Data/GetData/' + disease)
    file_list = [file for file in file_list if file.startswith('ac_')]
    # if no file, skip the disease
    if not file_list:
        logging.info(f'No file found in {disease}')
        continue

    logging.info(f'File list: {file_list}')
    outcome = []
    try:
        for file in file_list:
            file_path = f'../Data/GetData/{disease}/{file}'
            text,name = read_rtf(file_path)
            data, year = clean_text(text)
            df = text_to_dataframe(data, year, name)
            outcome.append(df)

        outcome = pd.concat(outcome)
        outcome.to_csv(f'../Data/CleanData/{disease}_ac.csv', index=False)
        logging.info(f'Finished processing {disease}')
    except Exception as e:
        logging.error(f'Error in {file_path}: {e}')
        continue
logging.info('Finished processing all ac data')

# ad data
disease_list = os.listdir('../Data/GetData/')
logging.info('Start processing ad data')
for disease in disease_list:
    logging.info(f'Start processing {disease}')
    file_list = os.listdir('../Data/GetData/' + disease)
    file_list = [file for file in file_list if file.startswith('ad_')]
    # if no file, skip the disease
    if not file_list:
        logging.info(f'No file found in {disease}')
        continue
    logging.info(f'File list: {file_list}')
    outcome = []
    try:
        for file in file_list:
            file_path = f'../Data/GetData/{disease}/{file}'
            text,name = read_rtf(file_path)
            data, year = clean_text(text)
            df = text_to_dataframe(data, year, name)
            outcome.append(df)

        outcome = pd.concat(outcome)
        outcome.to_csv(f'../Data/CleanData/{disease}_ad.csv', index=False)
        logging.info(f'Finished processing {disease}')
    except Exception as e:
        logging.error(f'Error in {file_path}: {e}')
        continue
logging.info('Finished processing all ad data')

# mcd data
def text_to_dataframe(data, year, disease):
    # create headers
    headers = data[0]
    labels = data[1]
    adjusted_headers = ['Areas']
    for i, header in enumerate(headers[1:]):
        adjusted_headers.append(f'{header}_{labels[i*2-2]}')
        adjusted_headers.append(f'{header}_{labels[i*2-1]}')

    # convert to dataframe
    df = pd.DataFrame(data[2:], columns=adjusted_headers)
    df = df.loc[:, df.apply(lambda col: not all(x == "" for x in col))]
    
    # Melt the DataFrame to longer format
    df = df.melt(id_vars=['Areas'], var_name='Period_Type', value_name='Count')
    df[['Month', 'Type']] = df['Period_Type'].str.split('_', expand=True)
    df.drop('Period_Type', axis=1, inplace=True)

    # if Type contains cas, replace it with Cases
    df['Type'] = df['Type'].apply(lambda x: 'Cases' if 'cas' in x.lower() else x)
    df['Type'] = df['Type'].apply(lambda x: 'Deaths' if 'dea' in x.lower() else x)
    
    # Assign static values for 'Year' and 'Disease'
    df['Year'] = year
    df['Disease'] = disease

    # remove including NAN values
    df = df.dropna()

    # remove rows which not number in Count
    df = df[df['Count'].apply(lambda x: x.isnumeric())]
    df = df.astype({'Count': 'int32'})

    return df

disease_list = os.listdir('../Data/GetData/')
logging.info('Start processing mcd data')
for disease in disease_list:
    logging.info(f'Start processing {disease}')
    file_list = os.listdir('../Data/GetData/' + disease)
    file_list = [file for file in file_list if file.startswith('mcd_')]
    # if no file, skip the disease
    if not file_list:
        logging.info(f'No file found in {disease}')
        continue
    logging.info(f'File list: {file_list}')
    outcome = []
    try:
        for file in file_list:
            file_path = f'../Data/GetData/{disease}/{file}'
            text,name = read_rtf(file_path)
            data, year = clean_text(text)
            df = text_to_dataframe(data, year, name)
            outcome.append(df)

        outcome = pd.concat(outcome)
        outcome.to_csv(f'../Data/CleanData/{disease}_mcd.csv', index=False)
        logging.info(f'Finished processing {disease}')
    except Exception as e:
        logging.error(f'Error in {file_path}: {e}')
        continue
logging.info('Finished processing all mcd data')

# rate
def fill_name(col_names):
    cleaned_names = []

    for i, name in enumerate(col_names):
        name = name.strip()
        if 'area' in name:
            cleaned_names.append('Areas')
        elif 'จำนวน' in name:
            cleaned_names.append('Population')
        elif 'cas' in name:
            cleaned_names.append('Cases')
        elif 'orbidity' in name:
            cleaned_names.append('Incidence')
        elif 'dea' in name:
            cleaned_names.append('Deaths')
        elif 'ortality' in name:
            cleaned_names.append('Mortality')
        elif 'CFR' in name:
            cleaned_names.append('CFR')
        elif 'dea' in name:
            cleaned_names.append('Deaths')
        else:
            cleaned_names.append(name)

    return cleaned_names
    
def text_to_dataframe(data, year, disease):
    # create headers
    headers = data[0]
    headers = fill_name(headers)

    # convert to dataframe
    df = pd.DataFrame(data[2:], columns=headers)
    df = df.loc[:, df.apply(lambda col: not all(x == "" for x in col))]
    
    # Assign static values for 'Year' and 'Disease'
    df['Year'] = year
    df['Disease'] = disease

    return df

# get disease list
disease_list = os.listdir('../Data/GetData/')
logging.info('Start processing rate data')
for disease in disease_list:
    logging.info(f'Start processing {disease}')
    file_list = os.listdir('../Data/GetData/' + disease)
    file_list = [file for file in file_list if file.startswith('rate_')]
    # if no file, skip the disease
    if not file_list:
        logging.info(f'No file found in {disease}')
        continue
    logging.info(f'File list: {file_list}')
    outcome = []
    try:
        for file in file_list:
            file_path = f'../Data/GetData/{disease}/{file}'
            text,name = read_rtf(file_path)
            data, year = clean_text(text)
            df = text_to_dataframe(data, year, name)
            outcome.append(df)

        outcome = pd.concat(outcome)
        outcome.to_csv(f'../Data/CleanData/{disease}_rate.csv', index=False)
        logging.info(f'Finished processing {disease}')
    except Exception as e:
        logging.error(f'Error in {file_path}: {e}')
        continue

logging.info('Finished processing all rate data')
