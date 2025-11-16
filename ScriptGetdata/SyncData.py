"""SyncData.py

Crawl the same site as `GetData.py`, collect downloadable file URLs for each disease,
compare with local files under Data/GetData/<disease>/ and tag whether the file should be
downloaded ('download') or skipped ('skip').

Output CSV columns:
  disease, file_name, remote_url, remote_size, local_path, local_exists, local_size, tag

Usage:
    python SyncData.py --index-url 'http://doe1.moph.go.th/surdata/index.php' --data-dir ../Data/GetData --output ../Data/file_sync_status.csv

Options:
    --check-size    When set, perform HEAD/GET to obtain remote Content-Length and compare sizes (slower). Default: off.
"""
from __future__ import annotations

import argparse
import csv
import logging
from pathlib import Path
from typing import Iterable, List, Optional, Tuple

import requests
from bs4 import BeautifulSoup
from urllib.parse import urljoin, urlparse, parse_qs, urlencode, urlunparse
import re
import os
from requests.adapters import HTTPAdapter
from urllib3.util.retry import Retry


# Log to a file next to this script for easier inspection
_LOG_PATH = Path(__file__).resolve().parent / 'SyncData.log'
logging.basicConfig(filename=str(_LOG_PATH), filemode='a', level=logging.INFO,
                    format="%(asctime)s - %(levelname)s - %(message)s")


def get_diseases(index_url: str, session: Optional[requests.Session] = None) -> List[Tuple[str, str, str]]:
    """Return list of (disease_name, full_url, historical_url) similar to GetData.get_diseases.
    This will try to find anchors with class 'e1'.
    """
    s = session or requests
    try:
        resp = s.get(index_url, timeout=15)
        resp.encoding = 'utf-8'
        resp.raise_for_status()
    except requests.RequestException as e:
        logging.error("Failed to fetch index page: %s", e)
        return []

    soup = BeautifulSoup(resp.text, 'html.parser')
    disease_list: List[Tuple[str, str, str]] = []

    def set_dcontent(u: str, value: str) -> str:
        p = urlparse(u)
        qs = parse_qs(p.query, keep_blank_values=True)
        qs['dcontent'] = [value]
        new_q = urlencode(qs, doseq=True)
        return urlunparse((p.scheme, p.netloc, p.path, p.params, new_q, p.fragment))

    for link in soup.find_all('a', class_='e1'):
        name = link.text.strip()
        href = link.get('href')
        if not href:
            continue
        full_raw = urljoin(index_url, href)
        # construct explicit URLs for situation (current) and old (historical)
        situation = set_dcontent(full_raw, 'situation')
        historical = set_dcontent(full_raw, 'old')
        disease_list.append((name, situation, historical))
    return disease_list


def get_file_links(page_url: str, file_types: Iterable[str], session: Optional[requests.Session] = None) -> List[Tuple[str, str]]:
    """Return list of (file_name, full_url) found on the page that match file_types.
    This searches hrefs (case-insensitive contains/endswith) and also scans onclick/javascript attributes for file references.
    """
    s = session or requests
    try:
        resp = s.get(page_url, timeout=15)
        resp.raise_for_status()
    except requests.RequestException as e:
        logging.error("Failed to fetch page %s: %s", page_url, e)
        return []

    soup = BeautifulSoup(resp.text, 'html.parser')
    file_urls: List[Tuple[str, str]] = []

    lower_types = [t.lower() for t in file_types]

    # Scan all anchor hrefs
    for a in soup.find_all('a'):
        href = a.get('href')
        if not href:
            continue
        low = href.lower()
        for t in lower_types:
            if low.endswith(t) or t in low:
                full = urljoin(page_url, href)
                name = href.split('/')[-1]
                file_urls.append((name, full))
                break

    # Scan onclick/javascript attributes for file references
    for tag in soup.find_all(attrs={'onclick': True}):
        onclick = tag.get('onclick') or ''
        for part in re.findall(r"['\"]([^'\"]+\.(?:" + '|'.join([re.escape(t.lstrip('.')) for t in lower_types]) + r"))['\"]", onclick, flags=re.IGNORECASE):
            full = urljoin(page_url, part)
            name = Path(part).name
            file_urls.append((name, full))

    # Deduplicate while preserving order
    seen = set()
    unique: List[Tuple[str, str]] = []
    for fn, fu in file_urls:
        if fu not in seen:
            seen.add(fu)
            unique.append((fn, fu))

    logging.info('Scanned %s -> found %d candidate files', page_url, len(unique))
    return unique


def remote_size(url: str, session: Optional[requests.Session] = None) -> Optional[int]:
    """Try HEAD then GET (stream) to determine remote Content-Length. Return None if unknown."""
    s = session or requests
    try:
        h = s.head(url, allow_redirects=True, timeout=15)
        if h.status_code < 400:
            cl = h.headers.get('Content-Length')
            if cl and cl.isdigit():
                return int(cl)
    except requests.RequestException:
        pass
    # Fallback to GET with stream for safety (will not read body fully)
    try:
        g = s.get(url, stream=True, timeout=20)
        g.raise_for_status()
        cl = g.headers.get('Content-Length')
        if cl and cl.isdigit():
            return int(cl)
    except requests.RequestException:
        pass
    return None


def inspect_local(path: Path) -> Tuple[bool, Optional[int]]:
    if not path.exists():
        return False, None
    try:
        size = path.stat().st_size
    except OSError:
        return True, None
    return True, size


def build_rows(index_url: str, data_dir: Path, file_types: Iterable[str], check_size: bool = False, session: Optional[requests.Session] = None) -> List[dict]:
    rows: List[dict] = []
    diseases = get_diseases(index_url, session=session)
    logging.info("Found %d diseases", len(diseases))

    for name, full_url, his_url in diseases:
        logging.info("Processing disease: %s", name)
        # check both historical and current pages
        links = get_file_links(his_url, file_types, session=session) + get_file_links(full_url, file_types, session=session)
        # deduplicate by URL
        seen = set()
        deduped: List[Tuple[str, str]] = []
        for fn, url in links:
            if url in seen:
                continue
            seen.add(url)
            deduped.append((fn, url))

        for fn, url in deduped:
            # Optionally check remote size (slow). If check_size is False, remote_sz stays None
            remote_sz = remote_size(url, session=session) if check_size else None
            local_path = data_dir / name.replace('/', '_') / fn
            local_exists, local_sz = inspect_local(local_path)

            # Store local_path as a relative path (repo-root relative). Use os.path.relpath so
            # we always produce a relative string even if the file is outside the repo root.
            repo_root = Path(__file__).resolve().parents[2]
            try:
                # os.path.relpath always returns a relative path string
                rel = os.path.relpath(str(local_path), start=str(repo_root))
            except Exception:
                # fallback to relative to cwd
                rel = os.path.relpath(str(local_path), start=str(Path.cwd()))
            local_path_rel = Path(rel)

            if not local_exists:
                tag = 'download'
            else:
                if not check_size:
                    # Fast mode: local exists -> skip
                    tag = 'skip'
                else:
                    # check_size == True: use remote size to decide
                    if remote_sz is None:
                        # can't verify remote size; conservatively skip if local exists
                        tag = 'skip'
                    else:
                        # compare sizes if known
                        if local_sz is None:
                            tag = 'download'
                        elif remote_sz != local_sz:
                            tag = 'download'
                        else:
                            tag = 'skip'

            rows.append(
                {
                    'disease': name,
                    'file_name': fn,
                    'remote_url': url,
                    'remote_size': '' if remote_sz is None else str(remote_sz),
                    'local_path': str(local_path_rel.as_posix()),
                    'local_exists': 'yes' if local_exists else 'no',
                    'local_size': '' if local_sz is None else str(local_sz),
                    'tag': tag,
                }
            )

    return rows


def write_csv(out: Path, rows: List[dict]):
    out.parent.mkdir(parents=True, exist_ok=True)
    fieldnames = ['disease', 'file_name', 'remote_url', 'remote_size', 'local_path', 'local_exists', 'local_size', 'tag']
    with out.open('w', newline='', encoding='utf-8') as f:
        w = csv.DictWriter(f, fieldnames=fieldnames)
        w.writeheader()
        for r in rows:
            w.writerow(r)


def main(argv: Optional[list[str]] = None) -> int:
    parser = argparse.ArgumentParser(description='Crawl site and tag files for download vs skip')
    parser.add_argument('--index-url', default='http://doe1.moph.go.th/surdata/index.php')
    parser.add_argument('--data-dir', type=Path, default=Path(__file__).resolve().parents[1] / 'Data' / 'GetData')
    parser.add_argument('--output', type=Path, default=Path(__file__).resolve().parents[1] / 'Data' / 'SyncDataStatus.csv')
    parser.add_argument('--types', default='.rtf,.pdf,.doc,.docx,.xls,.xlsx', help='Comma-separated file extensions to look for')
    parser.add_argument('--check-size', action='store_true', help='Enable remote size checking (slow). By default this is off.')
    args = parser.parse_args(argv)

    types = [t.strip() for t in args.types.split(',') if t.strip()]
    # Create a requests Session with retries to handle transient errors (e.g., 503)
    def make_session(retries: int = 5, backoff: float = 0.5) -> requests.Session:
        s = requests.Session()
        retry = Retry(total=retries, backoff_factor=backoff,
                      status_forcelist=[429, 500, 502, 503, 504],
                      allowed_methods=frozenset(["HEAD", "GET", "OPTIONS"]))
        adapter = HTTPAdapter(max_retries=retry)
        s.mount('https://', adapter)
        s.mount('http://', adapter)
        return s

    session = make_session()
    rows = build_rows(args.index_url, args.data_dir, types, check_size=bool(args.check_size), session=session)
    write_csv(args.output, rows)
    logging.info('Wrote %d rows to %s', len(rows), args.output)
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
