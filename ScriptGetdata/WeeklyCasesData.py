#!/usr/bin/env python3
"""WeeklyCasesData.py

Enhanced version with multiprocessing support for faster data extraction.
Fetches worksheets from Tableau dashboard, applies filters, and splits data by dimensions.

Usage:
    python ID_TH/ScriptGetdata/WeeklyCasesData.py --worksheet-name 'แผนที่ระดับจังหวัด' \
        --years 2568 --split-by 'โรค' --also-fetch 'ตารางการกระจายผู้ป่วยจังหวัด' \
        --output-dir ID_TH/Data/WeeklyCasesData --workers 4
"""

from tableauscraper import TableauScraper as TS
import pandas as pd
import os
import argparse
import sys
import logging
from pathlib import Path
import itertools
from typing import List, Tuple, Dict, Any
from multiprocessing import Pool, cpu_count
from functools import partial
import warnings

# Suppress all tableauscraper logging output
logging.getLogger('tableauScraper').setLevel(logging.CRITICAL)
logging.getLogger('tableauScraper').propagate = False

# Suppress all warnings from tableauscraper
warnings.filterwarnings('ignore', module='tableauscraper')
warnings.filterwarnings('ignore', module='tableauScraper')

try:
    from tqdm import tqdm
    TQDM_AVAILABLE = True
except ImportError:
    TQDM_AVAILABLE = False
    logging.warning('tqdm not available; progress bar will not be shown. Install with: pip install tqdm')

# Import helper functions from the same package or fallback to local module imports
try:
    from ID_TH.ScriptGetdata.WeeklyCasesDataFun import (
        get_worksheet, save_filtered_csv, parse_filter_args, safe_filename_component,
        extract_split_domains_from_filters, fetch_other_worksheet_after_server_filters
    )
except Exception:
    try:
        # If running from project root, try module path without top-level package
        from ScriptGetdata.WeeklyCasesDataFun import (
            get_worksheet, save_filtered_csv, parse_filter_args, safe_filename_component,
            extract_split_domains_from_filters, fetch_other_worksheet_after_server_filters
        )
    except Exception:
        # Finally, try importing the helper module from the same directory
        from WeeklyCasesDataFun import (
            get_worksheet, save_filtered_csv, parse_filter_args, safe_filename_component,
            extract_split_domains_from_filters, fetch_other_worksheet_after_server_filters
        )

DEFAULT_URL = "https://dvis3.ddc.moph.go.th/t/DDC_CENTER_DOE/views/DDS2/sheet127?%3Aembed=y&%3AisGuestRedirectFromVizportal=y"

RENAME_MAP = {
    "ชื่อโรค-value": "disease_value",
    "ชื่อโรค-alias": "disease_alias",
    "MIN(Number of Records)-alias": "min_records_alias",
    "สัปดาห์-value": "week_value",
    "สัปดาห์-alias": "week_alias",
    "ATTR(week_number)-alias": "week_number_alias",
    "ATTR(disease (copy))-alias": "disease_copy_alias",
    "ATTR(disease_th)-alias": "disease_th_alias",
    "AGG(จำนวนป่วย (แทน null))-alias": "cases_alias",
    "AGG(Formatted Percent Change)-alias": "pct_change_alias",
    "AGG(Arrow Type)-alias": "arrow_type_alias",
    "AGG(Consecutive Weeks)-alias": "consecutive_weeks_alias"
}


_LOG_PATH = Path(__file__).resolve().parent / 'WeeklyCasesData.log'
logging.basicConfig(filename=str(_LOG_PATH), filemode='a', level=logging.INFO,
                    format='%(asctime)s - %(levelname)s - %(message)s')

# Suppress tableauscraper warnings
logging.getLogger('tableauScraper').setLevel(logging.ERROR)


def process_single_task(task: Dict[str, Any]) -> Dict[str, Any]:
    """Process a single data extraction task (worker function for multiprocessing).
    
    Args:
        task: dict with keys:
            - url: dashboard URL
            - map_worksheet: name of map worksheet
            - target_worksheet: name of target worksheet to extract
            - year: year value (optional)
            - year_param: (param_name, param_value) tuple (optional)
            - filter_pairs: list of (filter_name, value) tuples
            - output_dir: output directory
            - filename: output filename
            - overwrite: whether to overwrite existing files (optional)
            
    Returns:
        dict with result info (name, filename, rows, path, error, skipped)
    """
    try:
        url = task['url']
        map_ws_name = task['map_worksheet']
        target_ws_name = task['target_worksheet']
        filter_pairs = task['filter_pairs']
        output_dir = task['output_dir']
        filename = task['filename']
        # support arbitrary parameters list (previously single year_param)
        parameters = task.get('parameters')
        overwrite = task.get('overwrite', True)

        # Check if file exists and should not be overwritten
        outpath = os.path.join(output_dir, filename)
        abs_out = os.path.abspath(outpath)
        if not overwrite and os.path.exists(abs_out):
            # Get file size to return rows estimate
            try:
                import pandas as pd
                existing_df = pd.read_csv(abs_out)
                rows = len(existing_df)
            except:
                rows = 0
            return {
                'name': target_ws_name,
                'filename': filename,
                'rows': rows,
                'path': abs_out,
                'skipped': True
            }
        
        # Load map worksheet with provided parameters (quiet mode)
        params = parameters if parameters else None
        _, map_ws = get_worksheet(url, map_ws_name, parameters=params, verbose=False)
        
        if map_ws is None:
            return {'error': f'Could not load map worksheet {map_ws_name}', 'path': None, 'rows': 0}
        
        # Apply filters and fetch target worksheet
        fetched_df = fetch_other_worksheet_after_server_filters(map_ws, filter_pairs, target_ws_name)
        
        if fetched_df.empty:
            logging.debug(f'Empty data for {filename} with filters {filter_pairs}')
        
        # Save to CSV (quiet mode)
        out_path = save_filtered_csv(fetched_df, output_dir, filename, verbose=False, overwrite=overwrite)
        
        return {
            'name': target_ws_name,
            'filename': filename,
            'rows': len(fetched_df),
            'path': out_path
        }
    except Exception as e:
        logging.exception(f'Error processing task: {task}')
        return {
            'error': str(e),
            'filename': task.get('filename', 'unknown'),
            'path': None,
            'rows': 0
        }


def generate_tasks_for_split(
    url: str,
    map_worksheet: str,
    target_worksheet: str,
    split_cols: List[str],
    all_domains: List[Tuple[str, List[str]]],
    output_dir: str,
    parameters: List[Tuple[str, str]] = None,
    overwrite: bool = True
) -> List[Dict[str, Any]]:
    """Generate task list for multiprocessing based on filter combinations.
    
    Args:
        url: Tableau dashboard URL
        map_worksheet: Name of map worksheet
        target_worksheet: Name of target worksheet
        split_cols: List of split column names
        all_domains: List of (filter_name, [values]) tuples
        output_dir: Base output directory
        year_param: Optional (param_name, param_value) tuple
        overwrite: Whether to overwrite existing files
        
    Returns:
        List of task dictionaries for process_single_task
    """
    tasks = []
    
    if not all_domains or not all(vals for _, vals in all_domains):
        # No valid domains, create single task with no filters
        task = {
            'url': url,
            'map_worksheet': map_worksheet,
            'target_worksheet': target_worksheet,
            'filter_pairs': [],
            'output_dir': output_dir,
            'filename': f"{safe_filename_component(target_worksheet)}.csv",
            'parameters': parameters,
            'overwrite': overwrite
        }
        tasks.append(task)
        return tasks
    
    # Generate cartesian product of all dimensions
    value_lists = [vals for _, vals in all_domains]
    filter_names = [name for name, _ in all_domains]
    
    for combo in itertools.product(*value_lists):
        server_pairs = [(name, val) for name, val in zip(filter_names, combo)]
        
        # Build output directory and filename
        if len(combo) == 1:
            # Single dimension: use value directly as filename, no subdirectory
            fname = f"{safe_filename_component(combo[0])}.csv"
            out_dir = output_dir
        else:
            # Multi-dimensional: create subdirectories for all dimensions EXCEPT the first one
            # e.g., output_dir/กลุ่มอายุ/<disease>_<age_group>.csv (not output_dir/โรค/กลุ่มอายุ/...)
            out_dir = Path(output_dir)
            for dim_name in filter_names[1:]:  # Skip first dimension
                out_dir = out_dir / dim_name
            fname_parts = [safe_filename_component(v) for v in combo]
            fname = '__'.join(fname_parts) + '.csv'
            out_dir = str(out_dir)
        
        task = {
            'url': url,
            'map_worksheet': map_worksheet,
            'target_worksheet': target_worksheet,
            'filter_pairs': server_pairs,
            'output_dir': out_dir,
            'filename': fname,
            'parameters': parameters,
            'overwrite': overwrite
        }
        tasks.append(task)
    
    return tasks


def fetch_dataframe_with_server_filters(original_ws, filter_pairs: List[Tuple[str, str]]):
    """Apply server-side filters (Tableau) by calling `setFilter` on the worksheet sequentially.

    original_ws: worksheet object returned by get_worksheet
    filter_pairs: list of (filter_name, value)

    Returns a pandas.DataFrame or None on error.
    """
    ws = original_ws
    try:
        for name, val in filter_pairs:
            wb = ws.setFilter(name, val)
            try:
                ws = wb.getWorksheet(original_ws.name)
            except Exception:
                try:
                    wss = getattr(wb, 'worksheets', None)
                    if wss:
                        ws = next((w for w in wss if getattr(w, 'name', None) == original_ws.name), None)
                    if ws is None:
                        logging.error(f'Could not find worksheet {original_ws.name} in workbook after setFilter')
                        return None
                except Exception as e:
                    logging.exception(f'Error locating worksheet after setFilter: {e}')
                    return None

        df = ws.data
        return pd.DataFrame(df) if df is not None else pd.DataFrame()
    except Exception as e:
        logging.exception(f'Error applying server filters {filter_pairs}: {e}')
        return None


def process_first_worksheet(url: str, output_dir: str, worksheet_index: int = 0, filename: str = "all_weekly_priority_cases.csv", parameters: list = None):
    export_summary = []
    workbook, t = get_worksheet(url, worksheet_index, parameters=parameters)
    if t is None:
        return export_summary
    logging.info(f'Processing worksheet [{worksheet_index}]: {t.name}')

    try:
        df = t.data
        if df is None:
            logging.info(f'  No data for: {t.name}')
            export_summary.append({'name': t.name, 'filename': filename, 'rows': 0, 'path': None})
            return export_summary

        pdf = pd.DataFrame(df)

        # Keep all columns from the scraped sheet. Rename any that match our RENAME_MAP.
        cols_to_rename = {c: RENAME_MAP[c] for c in pdf.columns if c in RENAME_MAP}
        filtered = pdf.rename(columns=cols_to_rename).copy()

        # Ensure canonical columns exist so later operations won't raise KeyError
        for col in ('disease_copy_alias', 'disease_value'):
            if col not in filtered.columns:
                filtered[col] = pd.NA

        # create mapping for disease_copy_alias and disease_value only if possible
        if 'disease_copy_alias' in filtered.columns and 'disease_value' in filtered.columns:
            bad_mask = filtered['disease_copy_alias'].astype(str).str.contains('%', regex=False, na=False)
            valid = filtered[~bad_mask & filtered['disease_copy_alias'].notna() & filtered['disease_value'].notna()]
            disease_map = dict(zip(valid['disease_value'].astype(str), valid['disease_copy_alias']))

            if bad_mask.any():
                filtered.loc[bad_mask, 'disease_copy_alias'] = (
                    filtered.loc[bad_mask, 'disease_value'].astype(str).map(disease_map)
                )

            still_missing = filtered['disease_copy_alias'].isna()
            if still_missing.any():
                filtered.loc[still_missing, 'disease_copy_alias'] = (
                    filtered.loc[still_missing, 'disease_value'].astype(str).map(disease_map)
                )
        else:
            logging.warning("  Warning: canonical columns 'disease_copy_alias' or 'disease_value' missing; skipping mapping step.")

        outpath = save_filtered_csv(filtered, output_dir, filename)
        export_summary.append({'name': t.name, 'filename': filename, 'rows': len(filtered), 'path': outpath})

    except Exception as e:
        logging.exception(f'  Error processing {t.name}: {e}')
        export_summary.append({'name': t.name, 'filename': filename, 'rows': None, 'path': None, 'error': str(e)})

    return export_summary

def build_argparser():
    p = argparse.ArgumentParser(description="Fetch Tableau worksheet and export as CSV")
    p.add_argument('--url', default=DEFAULT_URL, help='Tableau dashboard URL')
    logging.info(f'Dashboard URL: {DEFAULT_URL}')
    default_output = str(Path(__file__).resolve().parent.parent / 'Data' / 'WeeklyCasesData')
    p.add_argument('--output-dir', default=default_output, help='Output directory for CSV')
    p.add_argument('--worksheet-index', type=int, default=0, help='Index of worksheet to process (0-based)')
    p.add_argument('--worksheet-name', help='Name of worksheet to process (preferred over index)')
    p.add_argument('--filter', action='append', help='Filter condition(s) like "field=a,b" (can repeat)')
    p.add_argument('--filename', default='data.csv', help='Base filename for outputs (used as template)')
    p.add_argument('--years', action='append', help='Years to fetch (e.g., 2563 or 2563,2564). Can repeat.')
    p.add_argument('--indices', action='append', help='Index/indicator values to fetch (e.g., ลักษณะข้อมูล values). Can repeat.')
    p.add_argument('--also-fetch', action='append', help='Additional worksheet name(s) to fetch after applying parameters/filters (can repeat)')
    p.add_argument('--workers', type=int, default=1, help='Number of parallel workers for multiprocessing (default: 1, 0 or -1 for CPU count)')
    p.add_argument('--no-overwrite', action='store_true', help='Skip files that already exist (default: overwrite existing files)')
    p.add_argument('--split-by', action='append', help='Column name to split output by; will save one file per value (can repeat)')
    return p

def download_with_filters(url: str, filter_conditions: dict, output_dir: str, worksheet_index: int = 0, filename_template: str = "data_{suffix}.csv", split_by: list = None, parameters: list = None, save_overall: bool = True):
    """Download data from the worksheet and apply filter_conditions to the extracted DataFrame.

    filter_conditions: dict where keys are filter field names (as reported by t.getFilters() or DataFrame columns)
    and values are lists of allowed values. The function will try to map filter names to DataFrame columns
    (including using RENAME_MAP where applicable) and then save a CSV for the filtered result.
    If filter_conditions is empty, this behaves like process_first_worksheet and saves the full sheet.
    """
    workbook, t = get_worksheet(url, worksheet_index, parameters=parameters)
    if t is None:
        return []
    logging.info(f'Processing worksheet for filtered download [{worksheet_index}]: {t.name}')

    try:
        df = t.data
        if df is None:
            logging.info(f'  No data for: {t.name}')
            return []

        pdf = pd.DataFrame(df)

        try:
            available_filters = t.getFilters()
        except Exception:
            available_filters = None
            logging.info('Could not retrieve worksheet filters via t.getFilters()')

        logging.info(f'Available dataframe columns: {list(pdf.columns)}')
        mapped_conditions = {}
        for f_name, allowed_values in (filter_conditions or {}).items():
            # direct match
            if f_name in pdf.columns:
                mapped_conditions[f_name] = allowed_values
                continue
            # try rename map (user may pass a logical name)
            if f_name in RENAME_MAP:
                mapped = RENAME_MAP[f_name]
                if mapped in pdf.columns:
                    mapped_conditions[mapped] = allowed_values
                    continue
            # try inverse of rename_map: user might pass english name
            inv_map = {v: k for k, v in RENAME_MAP.items()}
            if f_name in inv_map and inv_map[f_name] in pdf.columns:
                mapped_conditions[inv_map[f_name]] = allowed_values
                continue
            # last resort: try case-insensitive match
            matches = [c for c in pdf.columns if c.lower() == f_name.lower()]
            if matches:
                mapped_conditions[matches[0]] = allowed_values
                continue

            logging.warning(f"Requested filter '{f_name}' not found in worksheet columns; skipping this filter.")
        logging.debug(f'Mapped conditions: {mapped_conditions}')

        wf_map = {}
        if available_filters:
            for f in available_filters:
                if isinstance(f, dict) and 'column' in f:
                    wf_map[f['column']] = f
        base_mask = pd.Series([True] * len(pdf))
        for col, vals in mapped_conditions.items():
            if not vals:
                continue
            if col in pdf.columns:
                base_mask &= pdf[col].astype(str).isin([str(v) for v in vals])

        base_df = pdf[base_mask].copy()
        results = []
        overall_suffix = 'filtered' if mapped_conditions else 'full'
        if save_overall:
            out = save_filtered_csv(base_df, output_dir, filename_template.format(suffix=overall_suffix))
            results.append({'name': t.name, 'filename': os.path.basename(out), 'rows': len(base_df), 'path': out})

        # If no split requested, return after saving overall
        if not split_by:
            return results

        inv_map = {v: k for k, v in RENAME_MAP.items()}
        df_split_cols: List[str] = []
        server_split_cols: List[str] = []
        for sname in (split_by or []):
            if sname in pdf.columns:
                df_split_cols.append(sname)
                continue
            if sname in RENAME_MAP and RENAME_MAP[sname] in pdf.columns:
                df_split_cols.append(RENAME_MAP[sname])
                continue
            # inverse rename (user passed english name)
            if sname in inv_map and inv_map[sname] in pdf.columns:
                df_split_cols.append(inv_map[sname])
                continue
            # case-insensitive match
            matches = [c for c in pdf.columns if c.lower() == sname.lower()]
            if matches:
                df_split_cols.append(matches[0])
                continue

            # try server-side filters (worksheet filters)
            wf_matches = [k for k in wf_map.keys() if k.lower() == sname.lower()]
            if wf_matches:
                server_split_cols.append(wf_matches[0])
                continue

            logging.warning(
                f"Requested split-by '{sname}' not found in worksheet columns or worksheet filters; skipping this split column. "
                f"Available dataframe columns: {list(pdf.columns)}; worksheet filter names: {list(wf_map.keys())}"
            )

        if not df_split_cols and not server_split_cols:
            logging.warning('No valid split columns found; falling back to single filtered file.')
            filtered = pdf[base_mask].copy()
            out = save_filtered_csv(filtered, output_dir, filename_template.format(suffix='filtered'))
            results.append({'name': t.name, 'filename': os.path.basename(out), 'rows': len(filtered), 'path': out})
            return results

        value_lists = []
        split_keys: List[Tuple[str, str]] = []

        for col in server_split_cols:
            df_col = None
            if col in base_df.columns:
                df_col = col
            else:
                # try rename map inverse or localized->renamed
                inv_map = {v: k for k, v in RENAME_MAP.items()}
                if col in RENAME_MAP and RENAME_MAP[col] in base_df.columns:
                    df_col = RENAME_MAP[col]
                elif col in inv_map and inv_map[col] in base_df.columns:
                    df_col = inv_map[col]
                else:
                    matches = [c for c in base_df.columns if c.lower() == col.lower()]
                    if matches:
                        df_col = matches[0]

            if df_col is not None:
                vals = [v for v in pd.unique(base_df[df_col].dropna().astype(str))]
            else:
                entry = wf_map.get(col)
                vals = [v for v in entry.get('values', [])] if entry else []

            if not vals:
                vals = ['NA']
            value_lists.append(vals)
            split_keys.append(('df' if df_col is not None else 'server', df_col if df_col is not None else col))
        for col in df_split_cols:
            if col in mapped_conditions and mapped_conditions[col]:
                vals = [str(v) for v in mapped_conditions[col]]
            else:
                vals = [v for v in pd.unique(pdf[col].dropna().astype(str))]
            if not vals:
                vals = ['NA']
            value_lists.append(vals)
            split_keys.append(('df', col))

        server_cache = {}

        for combo in itertools.product(*value_lists):
            parts = []
            server_pairs = []
            df_mask = None
            for (kind, name), val in zip(split_keys, combo):
                parts.append(f"{(name if name is not None else 'unknown')}-{safe_filename_component(val)}")
                if kind == 'server':
                    server_pairs.append((name, val))
                else:
                    if df_mask is None:
                        df_mask = pd.Series([True] * len(base_df))
                    df_mask &= base_df[name].astype(str) == str(val)
            if server_pairs:
                key = tuple(server_pairs)
                if key in server_cache:
                    fetched_df = server_cache[key]
                else:
                    fetched_df = fetch_dataframe_with_server_filters(t, server_pairs)
                    if fetched_df is None:
                        fetched_df = pd.DataFrame()
                    server_cache[key] = fetched_df
            else:
                fetched_df = base_df.copy()
            if fetched_df is base_df or fetched_df.equals(base_df):
                combined = fetched_df
            else:
                local_mask = pd.Series([True] * len(fetched_df))
                for col, vals in mapped_conditions.items():
                    if col in fetched_df.columns:
                        local_mask &= fetched_df[col].astype(str).isin([str(v) for v in vals])
                combined = fetched_df[local_mask].copy()

            if df_mask is not None and not combined.empty:
                idx_mask = df_mask.reindex(combined.index, fill_value=False)
                subset = combined[idx_mask].copy()
            else:
                subset = combined.copy()

            var_names = [(name if name is not None else 'unknown') for (_k, name) in split_keys]
            out_dir_sub = Path(output_dir)
            for vn in var_names:
                out_dir_sub = out_dir_sub / vn
            file_vals = [safe_filename_component(v) for v in combo]
            fname_base = '__'.join(file_vals)
            if not fname_base:
                fname_base = 'all'
            fname = f"{fname_base}.csv"
            out = save_filtered_csv(subset, str(out_dir_sub), fname)
            results.append({'name': t.name, 'filename': os.path.basename(out), 'rows': len(subset), 'path': out})

        return results
    except Exception as e:
        logging.exception(f'Error in download_with_filters: {e}')
        return []


def main(argv=None):
    logging.info('----------------------------------------')
    logging.info('Starting collection of new data from Tableau dashboard')
    print('Fetching dashboard metadata...')

    argv = argv if argv is not None else sys.argv[1:]
    args = build_argparser().parse_args(argv)

    # Parse filters and decide whether to run grouped download or single worksheet
    filter_conditions = parse_filter_args(args.filter)

    # Normalize years argument: can be repeated or comma-separated
    years = []
    if getattr(args, 'years', None):
        for y in args.years:
            for part in str(y).split(','):
                part = part.strip()
                if part:
                    years.append(part)
    # Determine number of workers
    num_workers = args.workers if args.workers > 0 else cpu_count()
    overwrite = not args.no_overwrite
    
    logging.info(f'Using {num_workers} workers for parallel processing')
    logging.info(f'Overwrite mode: {overwrite}')
    if not overwrite:
        print(f'Skip mode enabled: existing files will not be overwritten')
    
    results = []
    results = []

    # choose worksheet identifier: prefer name if provided, otherwise index
    worksheet_identifier = args.worksheet_name if getattr(args, 'worksheet_name', None) else args.worksheet_index

    # Retrieve workbook metadata (parameters, worksheets, filters) once at the beginning
    logging.info('Fetching dashboard metadata (parameters, worksheets, filters)...')
    workbook, sample_ws = get_worksheet(args.url, worksheet_identifier, verbose=True)
    year_param_name = None
    year_param_column = None
    year_param_values = None
    index_param_name = None
    index_param_column = None
    index_param_values = None
    try:
        params = None
        if workbook is not None and hasattr(workbook, 'getParameters'):
            params = workbook.getParameters()
        elif sample_ws is not None and hasattr(sample_ws, 'getParameters'):
            params = sample_ws.getParameters()
        if params:
            # params may be a list of dicts; find an entry that references year/ปี
            for p in params:
                try:
                    col = p.get('column') if isinstance(p, dict) else None
                    pname = p.get('parameterName') if isinstance(p, dict) else None
                    vals = p.get('values') if isinstance(p, dict) else None
                    # prefer the human-friendly column name (e.g., 'ปี') for year parameter
                    col_l = str(col).lower() if col is not None else ''
                    pname_l = str(pname).lower() if pname is not None else ''
                    # Detect year parameter
                    if col is not None and ('ปี' in col_l or 'year' in col_l or 'ปี' in pname_l or 'year' in pname_l):
                        year_param_column = col
                        year_param_name = pname
                        year_param_values = [str(v) for v in (vals or [])]
                        # don't break: also try to find index param in remaining params
                        continue
                    # Detect index/indicator parameter (Thai: 'ลักษณะ' or 'ลักษณะข้อมูล')
                    if col is not None and ('ลักษณะ' in col_l or 'ลักษณะข้อมูล' in col_l or 'indicator' in col_l or 'index' in col_l or 'ลักษณะ' in pname_l):
                        index_param_column = col
                        index_param_name = pname
                        index_param_values = [str(v) for v in (vals or [])]
                        continue
                    # fallback: if not yet set, pick first parameter that has values and a name
                    if year_param_name is None and index_param_name is None and (col or pname) and vals:
                        # prefer a year-like fallback first
                        if 'year' in col_l or 'year' in pname_l or 'ปี' in col_l or 'ปี' in pname_l:
                            year_param_column = col
                            year_param_name = pname
                            year_param_values = [str(v) for v in vals]
                        else:
                            # assign to index if we haven't found a year
                            index_param_column = col
                            index_param_name = pname
                            index_param_values = [str(v) for v in vals]
                except Exception:
                    continue
    except Exception:
        logging.exception('Error reading workbook parameters')

    # If user didn't pass years but we discovered parameter values, default to those
    if not years and year_param_values:
        years = year_param_values
    # Parse indices argument and default to discovered values if not provided
    indices = []
    if getattr(args, 'indices', None):
        for it in args.indices:
            for part in str(it).split(','):
                part = part.strip()
                if part:
                    indices.append(part)
    if not indices and index_param_values:
        indices = index_param_values
    
    # Log the processing plan
    logging.info(f'Running for years: {years if years else ["(no year filter)"]}')
    logging.info(f'Filters: {filter_conditions}, split_by={args.split_by}')
    if getattr(args, 'also_fetch', None):
        logging.info(f'Additional worksheets to fetch: {args.also_fetch}')
    
    # If no years discovered or provided, run without per-year loop
    if not years:
        # Single run (no year parameter). If indices were provided, loop over indices similarly.
        if indices:
            # Process per-index when no year provided
            for idx in indices:
                # If the user specified a single index only, avoid creating an extra
                # subdirectory for the index and save directly under the output dir.
                if len(indices) == 1:
                    per_index_output = str(Path(args.output_dir))
                else:
                    per_index_output = str(Path(args.output_dir) / safe_filename_component(idx))
                param_identifier = index_param_column if index_param_column else index_param_name
                index_param = (param_identifier, idx) if param_identifier else None
                params_to_apply = [index_param] if index_param else None

                if getattr(args, 'also_fetch', None):
                    logging.info(f'Fetching additional worksheets for index: {idx}')
                    print(f'Fetching worksheets for index: {idx}')
                    _, t_map = get_worksheet(args.url, worksheet_identifier, parameters=params_to_apply, verbose=False)
                    if t_map is None:
                        logging.error(f'Failed to load map worksheet for index {idx}')
                        continue
                    split_cols = args.split_by if args.split_by else []
                    all_domains = extract_split_domains_from_filters(t_map, split_cols)
                    all_tasks = []
                    for extra in args.also_fetch:
                        tasks = generate_tasks_for_split(
                            url=args.url,
                            map_worksheet=worksheet_identifier,
                            target_worksheet=extra,
                            split_cols=split_cols,
                            all_domains=all_domains,
                            output_dir=per_index_output,
                            parameters=params_to_apply,
                            overwrite=overwrite
                        )
                        all_tasks.extend(tasks)

                    total_tasks = len(all_tasks)
                    logging.info(f'Generated {total_tasks} tasks for parallel execution (index={idx})')
                    print(f'Processing {total_tasks} tasks with {num_workers} worker(s)...')
                    if num_workers > 1:
                        with Pool(processes=num_workers) as pool:
                            if TQDM_AVAILABLE:
                                results = list(tqdm(pool.imap(process_single_task, all_tasks), total=total_tasks, desc='Progress'))
                            else:
                                results = pool.map(process_single_task, all_tasks)
                    else:
                        if TQDM_AVAILABLE:
                            results = [process_single_task(task) for task in tqdm(all_tasks, desc='Progress')]
                        else:
                            results = [process_single_task(task) for task in all_tasks]
                else:
                    # No also_fetch: either filtered split or full worksheet per index
                    if filter_conditions or args.split_by:
                        results = download_with_filters(
                            args.url,
                            filter_conditions,
                            per_index_output,
                            worksheet_identifier,
                            filename_template=args.filename.replace('.csv', '_{suffix}.csv'),
                            split_by=args.split_by,
                            parameters=params_to_apply,
                            save_overall=False if args.split_by else True,
                        )
                    else:
                        r = process_first_worksheet(args.url, per_index_output, worksheet_identifier, args.filename, parameters=params_to_apply)
                        results = r if isinstance(r, list) else [r]
            # finished indices loop

        else:
            # Single run (no year or index parameter)
            if getattr(args, 'also_fetch', None):
                logging.info(f'Fetching additional worksheets: {args.also_fetch}')
                print(f'Fetching worksheets: {args.also_fetch}')

                # Load map worksheet once to extract filter metadata
                _, t_map = get_worksheet(args.url, worksheet_identifier, verbose=False)
                if t_map is None:
                    logging.error('Failed to load map worksheet')
                    return

                # Extract split columns and their domain values
                split_cols = args.split_by if args.split_by else []
                all_domains = extract_split_domains_from_filters(t_map, split_cols)

                # Generate all tasks for all target worksheets
                all_tasks = []
                for extra in args.also_fetch:
                    tasks = generate_tasks_for_split(
                        url=args.url,
                        map_worksheet=worksheet_identifier,
                        target_worksheet=extra,
                        split_cols=split_cols,
                        all_domains=all_domains,
                        output_dir=args.output_dir,
                        parameters=None,
                        overwrite=overwrite
                    )
                    all_tasks.extend(tasks)

                total_tasks = len(all_tasks)
                logging.info(f'Generated {total_tasks} tasks for parallel execution')
                print(f'Processing {total_tasks} tasks with {num_workers} worker(s)...')

                # Execute tasks in parallel with progress bar
                if num_workers > 1:
                    with Pool(processes=num_workers) as pool:
                        if TQDM_AVAILABLE:
                            results = list(tqdm(pool.imap(process_single_task, all_tasks), total=total_tasks, desc='Progress'))
                        else:
                            results = pool.map(process_single_task, all_tasks)
                else:
                    if TQDM_AVAILABLE:
                        results = [process_single_task(task) for task in tqdm(all_tasks, desc='Progress')]
                    else:
                        results = [process_single_task(task) for task in all_tasks]
            else:
                if filter_conditions or args.split_by:
                    logging.info(f'Filter conditions: {filter_conditions}, split_by: {args.split_by}')
                    results = download_with_filters(
                        args.url,
                        filter_conditions,
                        args.output_dir,
                        worksheet_identifier,
                        filename_template=args.filename.replace('.csv', '_{suffix}.csv'),
                        split_by=args.split_by,
                        parameters=None,
                        save_overall=False if args.split_by else True,
                    )
                else:
                    # No filters/split, fetch full worksheet
                    r = process_first_worksheet(args.url, args.output_dir, worksheet_identifier, args.filename, parameters=None)
                    results = r if isinstance(r, list) else [r]
    else:
        # Iterate years and process with multiprocessing
        logging.info(f'Processing years: {years}')
        print(f'Processing years: {years} with {num_workers} worker(s)...')
        
        all_year_tasks = []
        for y in years:
            per_year_output = str(Path(args.output_dir) / str(y))
            param_identifier = year_param_column if year_param_column else year_param_name
            year_param = (param_identifier, y) if param_identifier else None

            # If indices are provided, nest index loop inside year loop
            if indices:
                for idx in indices:
                    # If only one index requested, don't add an index subdirectory
                    if len(indices) == 1:
                        per_combo_output = str(Path(per_year_output))
                    else:
                        per_combo_output = str(Path(per_year_output) / safe_filename_component(idx))
                    param_identifier_idx = index_param_column if index_param_column else index_param_name
                    index_param = (param_identifier_idx, idx) if param_identifier_idx else None
                    # Compose parameters list for this (year, index) pair
                    params_to_apply = []
                    if year_param:
                        params_to_apply.append(year_param)
                    if index_param:
                        params_to_apply.append(index_param)

                    if filter_conditions or args.split_by:
                        if getattr(args, 'also_fetch', None):
                            # Load map worksheet with parameters to extract filter domains
                            _, t_map = get_worksheet(args.url, worksheet_identifier, parameters=params_to_apply, verbose=False)
                            if t_map is None:
                                logging.error(f'Failed to load map worksheet for year {y} index {idx}')
                                continue
                            split_cols = args.split_by if args.split_by else []
                            all_domains = extract_split_domains_from_filters(t_map, split_cols)
                            logging.info(f'Found {sum(len(vals) for _, vals in all_domains)} total values across {len(all_domains)} split dimension(s) for year {y} index {idx}')
                            for extra in args.also_fetch:
                                tasks = generate_tasks_for_split(
                                    url=args.url,
                                    map_worksheet=worksheet_identifier,
                                    target_worksheet=extra,
                                    split_cols=split_cols,
                                    all_domains=all_domains,
                                    output_dir=per_combo_output,
                                    parameters=params_to_apply,
                                    overwrite=overwrite
                                )
                                all_year_tasks.extend(tasks)
                        else:
                            r = download_with_filters(
                                args.url,
                                filter_conditions,
                                per_combo_output,
                                worksheet_identifier,
                                filename_template=args.filename.replace('.csv', '_{suffix}.csv'),
                                split_by=args.split_by,
                                parameters=params_to_apply,
                                save_overall=False if args.split_by else True,
                            )
                            results.extend(r)
                    else:
                        # No filters/split: fetch full worksheet for this year/index
                        r = process_first_worksheet(args.url, per_combo_output, worksheet_identifier, args.filename, parameters=params_to_apply)
                        results.extend(r)

                        if getattr(args, 'also_fetch', None):
                            for extra in args.also_fetch:
                                try:
                                    _, t_extra = get_worksheet(args.url, extra, parameters=params_to_apply, verbose=False)
                                    if t_extra is not None:
                                        df_extra = pd.DataFrame(t_extra.data) if t_extra.data is not None else pd.DataFrame()
                                        extra_template = f"{safe_filename_component(extra)}_{args.filename}"
                                        out = save_filtered_csv(df_extra, per_combo_output, extra_template, verbose=False)
                                        results.append({'name': getattr(t_extra, 'name', extra), 'filename': os.path.basename(out), 'rows': len(df_extra), 'path': out})
                                except Exception:
                                    logging.exception(f'Year={y} Index={idx}: Failed to fetch worksheet {extra}')
            else:
                # No indices: original per-year behavior
                if filter_conditions or args.split_by:
                    if getattr(args, 'also_fetch', None):
                        # Load map worksheet with year parameter to extract filter domains
                        params_to_apply = [year_param] if year_param else None
                        _, t_map = get_worksheet(args.url, worksheet_identifier, parameters=params_to_apply, verbose=False)
                        if t_map is None:
                            logging.error(f'Failed to load map worksheet for year {y}')
                            continue

                        # Extract split columns and their domain values
                        split_cols = args.split_by if args.split_by else []
                        all_domains = extract_split_domains_from_filters(t_map, split_cols)

                        logging.info(f'Found {sum(len(vals) for _, vals in all_domains)} total values across {len(all_domains)} split dimension(s) for year {y}')

                        # Generate tasks for this year
                        for extra in args.also_fetch:
                            tasks = generate_tasks_for_split(
                                url=args.url,
                                map_worksheet=worksheet_identifier,
                                target_worksheet=extra,
                                split_cols=split_cols,
                                all_domains=all_domains,
                                output_dir=per_year_output,
                                parameters=[year_param] if year_param else None,
                                overwrite=overwrite
                            )
                            all_year_tasks.extend(tasks)
                    else:
                        # Use existing download_with_filters (no multiprocessing for this path)
                        params_to_apply = [year_param] if year_param else None
                        r = download_with_filters(
                            args.url,
                            filter_conditions,
                            per_year_output,
                            worksheet_identifier,
                            filename_template=args.filename.replace('.csv', '_{suffix}.csv'),
                            split_by=args.split_by,
                            parameters=params_to_apply,
                            save_overall=False if args.split_by else True,
                        )
                        results.extend(r)
                else:
                    # No filters/split, fetch full worksheet
                    params_to_apply = [year_param] if year_param else None
                    r = process_first_worksheet(args.url, per_year_output, worksheet_identifier, args.filename, parameters=params_to_apply)
                    results.extend(r)
                    
                    if getattr(args, 'also_fetch', None):
                        for extra in args.also_fetch:
                            try:
                                _, t_extra = get_worksheet(args.url, extra, parameters=params_to_apply, verbose=False)
                                if t_extra is not None:
                                    df_extra = pd.DataFrame(t_extra.data) if t_extra.data is not None else pd.DataFrame()
                                    extra_template = f"{safe_filename_component(extra)}_{args.filename}"
                                    out = save_filtered_csv(df_extra, per_year_output, extra_template, verbose=False)
                                    results.append({'name': getattr(t_extra, 'name', extra), 'filename': os.path.basename(out), 'rows': len(df_extra), 'path': out})
                            except Exception:
                                logging.exception(f'Year={y}: Failed to fetch worksheet {extra}')
        
        # Execute all year tasks in parallel if any were generated
        if all_year_tasks:
            logging.info(f'Generated {len(all_year_tasks)} total tasks across all years')
            print(f'\nExecuting {len(all_year_tasks)} task(s)...')
            if num_workers > 1:
                if TQDM_AVAILABLE:
                    with Pool(processes=num_workers) as pool:
                        year_results = list(tqdm(pool.imap(process_single_task, all_year_tasks), total=len(all_year_tasks), desc='Processing tasks'))
                else:
                    with Pool(processes=num_workers) as pool:
                        year_results = pool.map(process_single_task, all_year_tasks)
            else:
                if TQDM_AVAILABLE:
                    year_results = [process_single_task(task) for task in tqdm(all_year_tasks, desc='Processing tasks')]
                else:
                    year_results = [process_single_task(task) for task in all_year_tasks]
            
            results.extend(year_results)

    # Summary
    success_count = sum(1 for r in results if 'path' in r and r['path'] and 'error' not in r)
    skipped_count = sum(1 for r in results if r.get('skipped', False))
    error_count = sum(1 for r in results if 'error' in r)
    total_rows = sum(r.get('rows', 0) for r in results if 'rows' in r)
    
    print(f'\n{"="*60}')
    print(f'Data collection complete!')
    print(f'  Total tasks: {len(results)}')
    print(f'  Successful: {success_count}')
    if skipped_count > 0:
        print(f'  Skipped (exists): {skipped_count}')
    print(f'  Failed: {error_count}')
    print(f'  Total rows: {total_rows:,}')
    print(f'{"="*60}')
    
    logging.info(f'Data collection complete: {success_count}/{len(results)} successful, {skipped_count} skipped, {total_rows:,} total rows')
    
    # Log failed tasks if any
    if error_count > 0:
        print(f'\nFailed tasks:')
        for r in results:
            if 'error' in r:
                print(f"  - {r.get('filename', 'unknown')}: {r['error']}")
                logging.error(f"Failed task {r.get('filename', 'unknown')}: {r['error']}")

    for r in results:
        if 'path' in r and r['path']:
            logging.info(f"  Saved: {r['path']} (rows={r.get('rows', 0)})")
    logging.info('Data collection complete.')

if __name__ == '__main__':
    main()