import pandas as pd
from tableauscraper import TableauScraper as TS
import os
import logging
from pathlib import Path
import re

DEFAULT_LOG = Path(__file__).resolve().parent / 'GetNewData.log'


def get_worksheet(url: str, worksheet_index: int = 0):
    """Load the workbook from `url` and return the worksheet object at worksheet_index.

    Prints `t.getFilters()` (best-effort) and logs filter metadata. Returns None if no worksheets found.
    """

    ts = TS()
    ts.loads(url)

    workbook = ts.getWorkbook()
    if not workbook.worksheets:
        logging.warning("No worksheets found in workbook.")
        return None

    if worksheet_index < 0 or worksheet_index >= len(workbook.worksheets):
        raise IndexError(f"worksheet_index {worksheet_index} out of range (0..{len(workbook.worksheets)-1})")

    t = workbook.worksheets[worksheet_index]
    # print filters to console as requested, and also log them
    try:
        filters = t.getFilters()
        if filters is not None:
            for item in filters:
                logging.info(f'Filter: {item}')
        logging.info(f'Worksheet filters metadata: {filters}')
    except Exception as e:
        print(f'Could not retrieve worksheet filters via t.getFilters(): {e}')
        logging.info('Could not retrieve worksheet filters via t.getFilters()')

    return t


def save_filtered_csv(filtered: pd.DataFrame, output_dir: str, filename: str) -> str:
    """Save DataFrame to CSV in output_dir/filename, ensuring directory exists.

    Returns the output path.
    """
    os.makedirs(output_dir, exist_ok=True)
    outpath = os.path.join(output_dir, filename)
    filtered.to_csv(outpath, index=False)
    logging.info(f'  Saved: {outpath} (rows={len(filtered)})')
    return outpath


def parse_filter_args(filter_args):
    """Parse list of filter argument strings like ['field=a,b', 'other=x'] into dict."""
    res = {}
    if not filter_args:
        return res
    for f in filter_args:
        if '=' not in f:
            logging.warning(f"Ignoring malformed filter argument: {f}")
            continue
        key, vals = f.split('=', 1)
        values = [v for v in vals.split(',') if v != '']
        res[key] = values
    return res


def safe_filename_component(s: str) -> str:
    """Make a safe filename component from a string: keep ascii alnum, replace others with underscore."""
    if s is None:
        return 'NA'
    s = str(s)
    s = s.strip()
    # replace any sequence of non-alnum with underscore
    return re.sub(r'[^A-Za-z0-9]+', '_', s)
