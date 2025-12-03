import pandas as pd
from tableauscraper import TableauScraper as TS
import os
import logging
from pathlib import Path
import re
import warnings

# Suppress all tableauscraper logging output
logging.getLogger('tableauScraper').setLevel(logging.CRITICAL)
logging.getLogger('tableauScraper').propagate = False

# Suppress all warnings from tableauscraper
warnings.filterwarnings('ignore', module='tableauscraper')
warnings.filterwarnings('ignore', module='tableauScraper')

DEFAULT_LOG = Path(__file__).resolve().parent / 'GetNewData.log'


def get_worksheet(url: str, worksheet_index: int = 0, parameters: list = None, verbose: bool = False):
    """Load the workbook from `url` and return a tuple (workbook, worksheet).

    If `parameters` is provided, it should be a list of (parameterName, value)
    pairs which will be applied sequentially to the worksheet (via `setParameter`).

    Args:
        url: Tableau dashboard URL
        worksheet_index: Index or name of worksheet to load
        parameters: Optional list of (parameterName, value) tuples
        verbose: If True, print metadata to console; otherwise only log to file
        
    Returns (workbook, worksheet) or (None, None) on failure.
    """

    ts = TS()
    ts.loads(url)

    workbook = ts.getWorkbook()
    if not getattr(workbook, 'worksheets', None):
        logging.warning("No worksheets found in workbook.")
        return None, None

    # list available worksheets (names + indices) for debugging and logging
    try:
        worksheet_list = [getattr(w, 'name', '<unnamed>') for w in workbook.worksheets]
        if verbose:
            print('Available worksheets:')
            for i, nm in enumerate(worksheet_list):
                print(f'  [{i}] {nm}')
            logging.info(f'Available worksheets: {worksheet_list}')
        else:
            logging.debug(f'Available worksheets: {worksheet_list}')
    except Exception:
        logging.debug("Could not list available worksheets")
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

    # Log worksheet name and filters
    try:
        if verbose:
            print(f'Processing worksheet: {getattr(t, "name", "<unnamed>")}')
            logging.info(f'Processing worksheet: {getattr(t, "name", "<unnamed>")}')
        else:
            logging.debug(f'Processing worksheet: {getattr(t, "name", "<unnamed>")}')
        
        # Only log filters in detail when verbose=True (initial metadata fetch)
        if verbose:
            filters = t.getFilters()
            if filters is not None:
                for item in filters:
                    logging.info(f'Filter: {item}')
    except Exception as e:
        logging.debug(f'Could not retrieve worksheet filters via t.getFilters(): {e}')

    # try to log workbook parameters if available (only when verbose=True)
    if verbose:
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
            logging.debug('Could not retrieve workbook parameters via getParameters()')
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
            except Exception as e:
                # Parameter setting failed - this dashboard may not support setParameter properly
                # Just log and continue with original worksheet (year parameter may not be critical)
                logging.error(f'Could not apply parameter {pname}={pval}: {e}. Continuing with original worksheet.')

    return workbook, t

def save_filtered_csv(filtered: pd.DataFrame, output_dir: str, filename: str, verbose: bool = False, overwrite: bool = True) -> str:
    """Save DataFrame to CSV in output_dir/filename, ensuring directory exists.

    Args:
        filtered: DataFrame to save
        output_dir: Output directory path
        filename: CSV filename
        verbose: If True, print to console; otherwise only log to file
        overwrite: If False, skip saving if file already exists
        
    Returns the absolute output path, or None if skipped.
    """
    os.makedirs(output_dir, exist_ok=True)
    outpath = os.path.join(output_dir, filename)
    abs_out = os.path.abspath(outpath)
    
    # Check if file exists and should not be overwritten
    if not overwrite and os.path.exists(abs_out):
        msg = f'Skipped (exists): {abs_out}'
        if verbose:
            print(msg)
        logging.info(msg)
        return abs_out
    
    filtered.to_csv(outpath, index=False)
    msg = f'Saved: {abs_out} ({len(filtered)} rows)'
    print(msg)  # Always print when saving
    logging.info(msg)
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


def extract_split_domains_from_filters(worksheet, split_by: list) -> list:
    """Extract domain values for split dimensions from worksheet filter metadata.
    
    Returns: list of (filter_name, [values]) tuples
    """
    if not split_by:
        return []
    
    try:
        filters = worksheet.getFilters()
        filter_map = {}
        for f in (filters or []):
            if isinstance(f, dict) and 'column' in f:
                filter_map[f['column'].lower()] = f
        # allow non-dict filter objects: map by lowercased column name when possible
        for f in (filters or []):
            try:
                if not isinstance(f, dict):
                    # try attribute access or mapping-like access
                    col = None
                    if hasattr(f, 'column'):
                        col = getattr(f, 'column')
                    elif hasattr(f, 'get') and callable(f.get):
                        col = f.get('column')
                    if col:
                        filter_map[str(col).lower()] = f
            except Exception:
                continue

        all_domains = []
        for s in split_by:
            sname = s
            key = sname.lower()
            fobj = filter_map.get(key)
            # try relaxed matching if exact key not found
            if fobj is None:
                for k, v in filter_map.items():
                    try:
                        if key in k or k in key:
                            fobj = v
                            break
                    except Exception:
                        continue

            if fobj is None:
                logging.warning(f'Could not find filter {sname}; available: {list(filter_map.keys())}')
                all_domains.append((sname, []))
                continue

            domain_vals = []
            try:
                # dict-like filter
                if isinstance(fobj, dict):
                    for candidate in ('domain', 'values', 'members'):
                        if candidate in fobj and isinstance(fobj[candidate], (list, tuple)):
                            domain_vals = list(fobj[candidate])
                            break
                    if not domain_vals:
                        # fallback single value
                        v = fobj.get('value') or fobj.get('default')
                        if isinstance(v, (list, tuple)):
                            domain_vals = list(v)
                        elif v is not None:
                            domain_vals = [v]
                else:
                    # object-like filter: try common attributes/methods
                    if hasattr(fobj, 'domain'):
                        dv = getattr(fobj, 'domain')
                        if isinstance(dv, (list, tuple)):
                            domain_vals = list(dv)
                    if not domain_vals and hasattr(fobj, 'values'):
                        dv = getattr(fobj, 'values')
                        if isinstance(dv, (list, tuple)):
                            domain_vals = list(dv)
                    if not domain_vals and hasattr(fobj, 'getDomain'):
                        try:
                            dv = fobj.getDomain()
                            if isinstance(dv, (list, tuple)):
                                domain_vals = list(dv)
                        except Exception:
                            pass
                    # last-ditch: convert to dict if possible and search
                    if not domain_vals and hasattr(fobj, '__dict__'):
                        try:
                            fd = dict(getattr(fobj, '__dict__') or {})
                            for candidate in ('domain', 'values', 'members'):
                                if candidate in fd and isinstance(fd[candidate], (list, tuple)):
                                    domain_vals = list(fd[candidate])
                                    break
                        except Exception:
                            pass
            except Exception:
                logging.exception(f'Error reading domain values for filter {sname}')

            logging.debug(f'Found {len(domain_vals)} values for filter {sname}')
            all_domains.append((sname, domain_vals))

        return all_domains
    except Exception as e:
        logging.exception(f'Error extracting filter domains: {e}')
        return [(s, []) for s in split_by]


def fetch_other_worksheet_after_server_filters(original_ws, filter_pairs: list, target_ws_name: str):
    """Apply server-side filters to original_ws and return DataFrame from target worksheet.

    Args:
        original_ws: worksheet object to apply filters on
        filter_pairs: list of (filter_name, value) tuples
        target_ws_name: name of target worksheet to extract data from
        
    Returns:
        pandas.DataFrame
    """
    ws = original_ws
    try:
        wb = None
        for name, val in filter_pairs:
            wb = ws.setFilter(name, val)
            try:
                ws = wb.getWorksheet(original_ws.name)
            except Exception:
                try:
                    wss = getattr(wb, 'worksheets', None)
                    if wss:
                        ws = next((w for w in wss if getattr(w, 'name', None) == original_ws.name), ws)
                except Exception:
                    pass

        if wb is None:
            logging.error('No workbook returned after applying filters')
            return pd.DataFrame()

        try:
            try:
                target_ws = wb.getWorksheet(target_ws_name)
            except Exception:
                target_ws = None

            if target_ws is None:
                wss = getattr(wb, 'worksheets', [])
                target_ws = next((w for w in wss if getattr(w, 'name', '') == target_ws_name), None)
                if target_ws is None:
                    target_ws = next((w for w in wss if getattr(w, 'name', '').lower() == target_ws_name.lower()), None)
                if target_ws is None:
                    target_ws = next((w for w in wss if target_ws_name.lower() in getattr(w, 'name', '').lower() or getattr(w, 'name', '').lower() in target_ws_name.lower()), None)

            if target_ws is None:
                logging.error(f'Could not find target worksheet "{target_ws_name}" after applying filters')
                return pd.DataFrame()

            df = target_ws.data
            return pd.DataFrame(df) if df is not None else pd.DataFrame()
        except Exception as e:
            logging.exception(f'Error extracting target worksheet {target_ws_name}: {e}')
            return pd.DataFrame()
    except Exception as e:
        logging.exception(f'Error applying server filters {filter_pairs}: {e}')
        return pd.DataFrame()
