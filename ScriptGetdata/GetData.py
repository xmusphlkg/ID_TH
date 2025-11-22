import requests
from bs4 import BeautifulSoup
from urllib.parse import urljoin
import os

import logging
from multiprocessing import Pool
from requests.adapters import HTTPAdapter
from urllib3.util.retry import Retry
import csv
from pathlib import Path

# Setup logging
_LOG_PATH = Path(__file__).resolve().parent / 'GetData.log'
logging.basicConfig(filename=str(_LOG_PATH), filemode='a', level=logging.INFO,
                    format='%(asctime)s - %(levelname)s - %(message)s')

def get_diseases(url):
    """
    Fetch the list of diseases from the main page.
    """
    # use a session with retries for transient errors
    session = requests.Session()
    retry = Retry(total=5, backoff_factor=0.5, status_forcelist=[429,500,502,503,504], allowed_methods=frozenset(["GET","HEAD"]))
    session.mount('https://', HTTPAdapter(max_retries=retry))
    session.mount('http://', HTTPAdapter(max_retries=retry))
    try:
        response = session.get(url, timeout=15)
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



def get_file_url(page_url, file_type = '.rtf'):
    """
    Fetch file download URLs from a historical page URL.
    """
    # use session with retries
    session = requests.Session()
    retry = Retry(total=5, backoff_factor=0.5, status_forcelist=[429,500,502,503,504], allowed_methods=frozenset(["GET","HEAD"]))
    session.mount('https://', HTTPAdapter(max_retries=retry))
    session.mount('http://', HTTPAdapter(max_retries=retry))
    try:
        response = session.get(page_url, timeout=15)
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
    
    # Download the file with retries using a session (safe in child process)
    session = requests.Session()
    retry = Retry(total=retries, backoff_factor=delay, status_forcelist=[429,500,502,503,504], allowed_methods=frozenset(["GET","HEAD"]))
    session.mount('https://', HTTPAdapter(max_retries=retry))
    session.mount('http://', HTTPAdapter(max_retries=retry))

    try:
        response = session.get(file_url, timeout=30)
        response.raise_for_status()
        # Ensure the directory exists
        os.makedirs(file_path, exist_ok=True)
        with open(os.path.join(file_path, file_name), 'wb') as file:
            file.write(response.content)
    except requests.RequestException as e:
        logging.error(f"Failed to download {file_name}: {e}")
        logging.error(f"Gave up after {retries} attempts for {file_name}")

def download_files(disease_name, full_url, history_url):
    """
    Download all files from a given historical URL using multiple processes.
    """
    file_urls_his = get_file_url(history_url)
    file_urls_new = get_file_url(full_url)
    all_files = file_urls_his + file_urls_new

    with Pool(30) as p:
        # Download all discovered files (historical + current). overwrite=True for current files.
        p.starmap(download_file, [(disease_name, file_data, True) for file_data in all_files])


def load_sync_downloads(csv_path: Path) -> dict:
    """Read SyncDataStatus CSV and return mapping disease -> list of (file_name, remote_url).
    Only rows with tag == 'download' are returned.
    """
    downloads: dict = {}
    if not csv_path.exists():
        logging.error("Sync CSV not found: %s", csv_path)
        return downloads

    with csv_path.open('r', encoding='utf-8') as f:
        reader = csv.DictReader(f)
        for row in reader:
            tag = (row.get('tag') or '').strip().lower()
            if tag != 'download':
                continue
            disease = (row.get('disease') or '').strip()
            file_name = (row.get('file_name') or '').strip()
            remote_url = (row.get('remote_url') or '').strip()
            if not (disease and file_name and remote_url):
                continue
            downloads.setdefault(disease, []).append((file_name, remote_url))
    return downloads


def download_from_sync_csv(csv_path: Path):
    """Download all files listed as 'download' in the sync CSV using multiprocessing."""
    downloads = load_sync_downloads(csv_path)
    if not downloads:
        logging.info("No files to download found in %s", csv_path)
        return

    tasks = []
    for disease, files in downloads.items():
        for file_data in files:
            tasks.append((disease, file_data, True))

    with Pool(30) as p:
        p.starmap(download_file, tasks)


def download_one(disease: str, file_name: str, url: str, overwrite: bool = True, retries: int = 3, delay: float = 1.0) -> str:
    """Download a single file and return 'success' or an error code/message."""
    file_path = Path('../Data/GetData') / disease.replace('/', '_')
    dest = file_path / file_name
    if dest.exists() and not overwrite:
        return 'success'

    file_path.mkdir(parents=True, exist_ok=True)

    session = requests.Session()
    retry = Retry(total=retries, backoff_factor=delay, status_forcelist=[429,500,502,503,504], allowed_methods=frozenset(["GET","HEAD"]))
    session.mount('https://', HTTPAdapter(max_retries=retry))
    session.mount('http://', HTTPAdapter(max_retries=retry))

    try:
        with session.get(url, timeout=30, stream=True) as r:
            r.raise_for_status()
            with open(dest, 'wb') as f:
                for chunk in r.iter_content(chunk_size=8192):
                    if chunk:
                        f.write(chunk)
        return 'success'
    except requests.HTTPError as e:
        code = getattr(e.response, 'status_code', None)
        return f'HTTP {code}' if code else 'HTTP error'
    except Exception as e:
        return type(e).__name__ + (f': {e}' if str(e) else '')


def create_download_status(csv_in: Path, csv_out: Path):
    """Read sync CSV, attempt downloads for rows with tag=='download', and write a new CSV
    identical to the input but with an extra 'download_result' column containing 'success' or an error string.
    """
    if not csv_in.exists():
        logging.error('Input sync CSV not found: %s', csv_in)
        return

    rows = []
    with csv_in.open('r', encoding='utf-8') as f:
        reader = csv.DictReader(f)
        fieldnames = list(reader.fieldnames or [])
        if 'download_result' not in fieldnames:
            fieldnames.append('download_result')
        for row in reader:
            tag = (row.get('tag') or '').strip().lower()
            if tag == 'download':
                disease = (row.get('disease') or '').strip()
                file_name = (row.get('file_name') or '').strip()
                remote_url = (row.get('remote_url') or '').strip()
                if disease and file_name and remote_url:
                    logging.info("Attempting download from CSV: disease=%s file=%s tag=%s url=%s", disease, file_name, tag, remote_url)
                        result = download_one(disease, file_name, remote_url, overwrite=True, retries=3)
                    logging.info("Result for %s/%s -> %s", disease, file_name, result)
                else:
                    result = 'invalid_row'
                row['download_result'] = result
            else:
                # Not marked for download; leave empty and log
                logging.debug("Skipping download from CSV: tag=%s row=%s", tag, row)
                row['download_result'] = ''
            rows.append(row)

    csv_out.parent.mkdir(parents=True, exist_ok=True)
    with csv_out.open('w', encoding='utf-8', newline='') as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        for r in rows:
            writer.writerow(r)

# Run the script
if __name__ == '__main__':
    # Prefer to download files listed by SyncData
    csv_path = Path(__file__).resolve().parents[1] / 'Data' / 'SyncDataStatus.csv'
    # download_from_sync_csv(csv_path)
    outcome_path = Path(__file__).resolve().parents[1] / 'Outcome' / 'DownloadDataStatus.csv'
    create_download_status(csv_path, outcome_path)