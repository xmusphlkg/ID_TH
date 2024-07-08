import requests
from bs4 import BeautifulSoup
from urllib.parse import urljoin
import os
import time
import logging
from multiprocessing import Pool

# Setup logging
logging.basicConfig(filename='./GetData.log', level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

def get_diseases(url):
    """
    Fetch the list of diseases from the main page.
    """
    try:
        response = requests.get(url)
        response.encoding = 'utf-8'
        response.raise_for_status()
    except requests.RequestException as e:
        logging.error(f"Error fetching the page: {e}")
        return []
    
    soup = BeautifulSoup(response.text, 'html.parser')
    disease_list = []

    # Extract disease name and URL
    for link in soup.find_all('a', class_='e1'):
        disease_name = link.text.strip()
        disease_url = link['href']
        full_url = requests.compat.urljoin(url, disease_url)
        historical_url = full_url.replace('disease.php?', 'disease.php?dcontent=old&')
        disease_list.append((disease_name, full_url, historical_url))
    
    # Make folder for each disease
    for disease_name, _, _ in disease_list:
        file_path = '../Data/GetData/' + disease_name.replace('/', '_')
        if not os.path.exists(file_path):
            os.makedirs(file_path)
    
    return disease_list

def add_full_url(full_url, historical_url):
    """
    Add the full URL which is missing in the disease URL.
    """
    # get file name from historical URL
    file_name = historical_url.split('/')[-1]

    # get id from full URL, which between last _ and .
    id = full_url.split('_')[-1].split('.')[0]

def get_file_url(page_url, file_type = '.rtf'):
    """
    Fetch file download URLs from a historical page URL.
    """
    try:
        response = requests.get(page_url)
        response.raise_for_status()  # Check for HTTP request errors
    except requests.RequestException as e:
        logging.error(f"Error fetching the page: {e}")
        return []
    
    soup = BeautifulSoup(response.text, 'html.parser')
    file_urls = []

    for link in soup.find_all('a', target="_blank"):
        file_url = link.get('href')
        if file_url and file_url.endswith(file_type):
            full_file_url = urljoin(page_url, file_url)
            file_name = file_url.split('/')[-1]
            file_urls.append((file_name, full_file_url))
    
    return file_urls

def download_file(disease_name, file_data, overwrite=False):
    """
    Download a file with error handling.
    """
    file_name, file_url = file_data
    file_path = '../Data/GetData/' + disease_name.replace('/', '_')
    retries = 3
    delay = 1

    # check file exist
    if os.path.exists(os.path.join(file_path, file_name)) and not overwrite:
        logging.info(f"File {file_name} already exists.")
        return
    
    # Download the file with retries
    for attempt in range(retries):
        try:
            response = requests.get(file_url)
            response.raise_for_status()
            # Ensure the directory exists
            os.makedirs(file_path, exist_ok=True)
            with open(os.path.join(file_path, file_name), 'wb') as file:
                file.write(response.content)
                break
        except requests.RequestException as e:
            logging.error(f"Failed to download {file_name}: {e}")
            if attempt < retries - 1:
                time.sleep(delay)
            else:
                logging.error(f"Failed to download {file_name} after {retries} attempts.")

def download_files(disease_name, full_url, history_url):
    """
    Download all files from a given historical URL using multiple processes.
    """
    file_urls_his = get_file_url(history_url)
    file_urls_new = get_file_url(full_url)

    with Pool(30) as p:
        # p.starmap(download_file, [(disease_name, file_data, False) for file_data in file_urls_his])
        p.starmap(download_file, [(disease_name, file_data, True) for file_data in file_urls_new])

# Run the script
if __name__ == '__main__':
    # Target URL
    url = 'http://doe.moph.go.th/surdata/index.php'
    disease_list = get_diseases(url)
    for disease_name, full_url, history_url in disease_list:
        download_files(disease_name, full_url, history_url)
