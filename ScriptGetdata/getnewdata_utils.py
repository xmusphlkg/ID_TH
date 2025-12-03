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
        print("No worksheets found in workbook.")
        logging.warning("No worksheets found in workbook.")
        return None, None

    # list available worksheets (names + indices) for debugging and logging
    try:
        worksheet_list = [getattr(w, 'name', '<unnamed>') for w in workbook.worksheets]
        print('Available worksheets:')
        for i, nm in enumerate(worksheet_list):
            print(f'  [{i}] {nm}')
        logging.info(f'Available worksheets: {worksheet_list}')
    except Exception:
        logging.info("Could not list available worksheets")
        worksheet_list = None

    # allow caller to pass either an int index or a worksheet name (string)
    t = None
    try:
        if isinstance(worksheet_index, str):
            # look up worksheet by name (exact match first, then case-insensitive)
            for w in workbook.worksheets:
                if getattr(w, 'name', None) == worksheet_index:
                    t = w
                    break
            if t is None:
                for w in workbook.worksheets:
                    if getattr(w, 'name', '').lower() == worksheet_index.lower():
                        t = w
                        break
            # relaxed match: substring (either direction)
            if t is None:
                for w in workbook.worksheets:
                    name = getattr(w, 'name', '')
                    try:
                        if worksheet_index.lower() in name.lower() or name.lower() in worksheet_index.lower():
                            t = w
                            break
                    except Exception:
                        continue
            # normalized match: remove non-word characters and compare
            if t is None:
                try:
                    import re as _re
                    norm_req = _re.sub(r"[^\w]+", "", worksheet_index).lower()
                    for w in workbook.worksheets:
                        name = getattr(w, 'name', '')
                        norm_name = _re.sub(r"[^\w]+", "", name).lower()
                        if norm_req and norm_name and (norm_req == norm_name):
                            t = w
                            break
                except Exception:
                    pass

            if t is None:
                avail = worksheet_list if worksheet_list is not None else [getattr(w, 'name', '<unnamed>') for w in workbook.worksheets]
                suggestion = ''
                if len(avail) == 1:
                    suggestion = " Try using --worksheet-index 0 to select the only available worksheet."
                msg = f"No worksheet named '{worksheet_index}' found in workbook. Available worksheets: {avail}." + suggestion
                print(msg)
                logging.error(msg)
                raise ValueError(msg)
        else:
            # assume integer index
            if worksheet_index < 0 or worksheet_index >= len(workbook.worksheets):
                raise IndexError(f"worksheet_index {worksheet_index} out of range (0..{len(workbook.worksheets)-1})")
            t = workbook.worksheets[worksheet_index]
    except Exception:
        # re-raise after logging for visibility
        logging.exception('Error selecting worksheet by index or name')
        raise

    # print worksheet name and filters to console (helpful for adapting to dashboard changes)
    try:
        print(f'Processing worksheet: {getattr(t, "name", "<unnamed>")}')
        logging.info(f'Processing worksheet: {getattr(t, "name", "<unnamed>")}')
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
    abs_out = os.path.abspath(outpath)
    try:
        print(f'Saved CSV: {abs_out} (rows={len(filtered)})')
    except Exception:
        # printing should not raise the process
        pass
    return abs_out

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
