import pandas as pd
from tableauscraper import TableauScraper as TS
import os
import logging
from pathlib import Path
import re

DEFAULT_LOG = Path(__file__).resolve().parent / 'GetNewData.log'


def get_worksheet(url: str, worksheet_index: int = 0, parameters: list = None):
    """Load the workbook from `url` and return a tuple (workbook, worksheet).

    If `parameters` is provided, it should be a list of (parameterName, value)
    pairs which will be applied sequentially to the worksheet (via `setParameter`).

    Prints `t.getFilters()` and `workbook.getParameters()` (best-effort) and logs metadata.
    Returns (workbook, worksheet) or (None, None) on failure.
    """

    ts = TS()
    ts.loads(url)

    workbook = ts.getWorkbook()
    if not getattr(workbook, 'worksheets', None):
        logging.warning("No worksheets found in workbook.")
        return None, None

    if worksheet_index < 0 or worksheet_index >= len(workbook.worksheets):
        raise IndexError(f"worksheet_index {worksheet_index} out of range (0..{len(workbook.worksheets)-1})")

    t = workbook.worksheets[worksheet_index]

    # print filters to console as requested, and also log them
    try:
        filters = t.getFilters()
        if filters is not None:
            for item in filters:
                logging.info(f'Filter: {item}')
    except Exception as e:
        print(f'Could not retrieve worksheet filters via t.getFilters(): {e}')
        logging.info('Could not retrieve worksheet filters via t.getFilters()')

    # try to log workbook parameters if available
    try:
        params = None
        if hasattr(workbook, 'getParameters'):
            params = workbook.getParameters()
        elif hasattr(t, 'getParameters'):
            params = t.getParameters()
        if params is not None:
            for p in params:
                logging.info(f'Parameter: {p}')
    except Exception:
        logging.info('Could not retrieve workbook parameters via getParameters()')

    # If parameters were requested, attempt to set them sequentially
    if parameters:
        original_name = getattr(t, 'name', None)
        for pname, pval in parameters:
            try:
                # prefer worksheet.setParameter if available
                if hasattr(t, 'setParameter'):
                    wb = t.setParameter(pname, pval)
                elif hasattr(workbook, 'setParameter'):
                    wb = workbook.setParameter(pname, pval)
                else:
                    logging.warning('No setParameter() available on workbook or worksheet; skipping parameter set')
                    wb = None

                if wb is not None:
                    # try to find the same worksheet within returned workbook
                    try:
                        t = wb.getWorksheet(original_name)
                    except Exception:
                        try:
                            wss = getattr(wb, 'worksheets', None)
                            if wss:
                                t = next((w for w in wss if getattr(w, 'name', None) == original_name), t)
                        except Exception:
                            logging.exception('Error finding worksheet after setParameter')
            except Exception:
                logging.exception(f'Error applying parameter {pname}={pval}')

    return workbook, t


def save_filtered_csv(filtered: pd.DataFrame, output_dir: str, filename: str) -> str:
    """Save DataFrame to CSV in output_dir/filename, ensuring directory exists.

    Returns the output path.
    """
    os.makedirs(output_dir, exist_ok=True)
    outpath = os.path.join(output_dir, filename)
    filtered.to_csv(outpath, index=False)
    logging.info(f'  Saved: {filename} (rows={len(filtered)})')
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
    # Replace any sequence of characters that are NOT word characters (Unicode letters/digits/_)
    # with a single underscore. This preserves non-Latin letters (e.g., Thai) which are
    # valid in filenames on Linux, while removing punctuation and spaces.
    comp = re.sub(r"[^\w]+", "_", s, flags=re.UNICODE)
    # collapse multiple underscores and strip leading/trailing underscores
    comp = re.sub(r'_+', '_', comp).strip('_')
    return comp if comp else 'NA'
