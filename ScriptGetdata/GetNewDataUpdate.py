#!/usr/bin/env python3
"""GetNewData.py

Fetches the first worksheet from a Tableau dashboard using tableauscraper,
selects/renames columns robustly, attempts to fill disease aliases, and
writes a CSV to the Data/Tableau folder by default.

Usage:
    python ID_TH/ScriptGetdata/GetNewData.py --split-by age_group
"""

from tableauscraper import TableauScraper as TS
import pandas as pd
import os
import argparse
import sys
import logging
from pathlib import Path
import itertools
from typing import List, Tuple

# utility helpers moved to separate module for reuse
try:
    # when running as script from this directory, this module is importable by name
    from getnewdata_utils import get_worksheet, save_filtered_csv, parse_filter_args, safe_filename_component
except Exception:
    # fallback to package-style import when available
    from ScriptGetdata.getnewdata_utils import get_worksheet, save_filtered_csv, parse_filter_args, safe_filename_component

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


_LOG_PATH = Path(__file__).resolve().parent / 'GetNewDataUpdate.log'
logging.basicConfig(filename=str(_LOG_PATH), filemode='a', level=logging.INFO,
                    format='%(asctime)s - %(levelname)s - %(message)s')


def fetch_dataframe_with_server_filters(original_ws, filter_pairs: List[Tuple[str, str]]):
    """Apply server-side filters (Tableau) by calling `setFilter` on the worksheet sequentially.

    original_ws: worksheet object returned by get_worksheet
    filter_pairs: list of (filter_name, value)

    Returns a pandas.DataFrame or None on error.
    """
    ws = original_ws
    try:
        for name, val in filter_pairs:
            # setFilter returns a workbook-like object per tableauscraper docs
            wb = ws.setFilter(name, val)
            # try to get the same worksheet (by name) from returned workbook
            try:
                ws = wb.getWorksheet(original_ws.name)
            except Exception:
                # fallback: search returned workbook worksheets by name attribute
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


def fetch_other_worksheet_after_server_filters(original_ws, filter_pairs: List[Tuple[str, str]], target_ws_name: str):
    """Apply server-side filters to original_ws (usually the map) and return DataFrame from target worksheet.

    original_ws: worksheet object to call setFilter on
    filter_pairs: list of (filter_name, value) to apply sequentially
    target_ws_name: name (or partial name) of the worksheet to extract from the returned workbook
    """
    ws = original_ws
    try:
        wb = None
        for name, val in filter_pairs:
            wb = ws.setFilter(name, val)
            # after first setFilter, subsequent setFilter should be called on the returned workbook's worksheet
            try:
                ws = wb.getWorksheet(original_ws.name)
            except Exception:
                # fallback: try to find by name in workbook worksheets
                try:
                    wss = getattr(wb, 'worksheets', None)
                    if wss:
                        ws = next((w for w in wss if getattr(w, 'name', None) == original_ws.name), ws)
                except Exception:
                    pass

        # wb should now be the last returned workbook-like object
        if wb is None:
            logging.error('No workbook returned after applying filters')
            return pd.DataFrame()

        # try to locate target worksheet in wb
        try:
            # exact match
            try:
                target_ws = wb.getWorksheet(target_ws_name)
            except Exception:
                target_ws = None

            if target_ws is None:
                # fallback: search worksheets list
                wss = getattr(wb, 'worksheets', [])
                # try exact, case-insensitive, substring
                target_ws = next((w for w in wss if getattr(w, 'name', '') == target_ws_name), None)
                if target_ws is None:
                    target_ws = next((w for w in wss if getattr(w, 'name', '').lower() == target_ws_name.lower()), None)
                if target_ws is None:
                    target_ws = next((w for w in wss if target_ws_name.lower() in getattr(w, 'name', '').lower() or getattr(w, 'name', '').lower() in target_ws_name.lower()), None)

            if target_ws is None:
                logging.error(f'Could not find target worksheet "{target_ws_name}" in workbook after applying filters')
                return pd.DataFrame()

            df = target_ws.data
            return pd.DataFrame(df) if df is not None else pd.DataFrame()
        except Exception as e:
            logging.exception(f'Error extracting target worksheet {target_ws_name} after filters: {e}')
            return pd.DataFrame()
    except Exception as e:
        logging.exception(f'Error applying server filters {filter_pairs} to fetch other worksheet: {e}')
        return pd.DataFrame()



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
    default_output = str(Path(__file__).resolve().parent.parent / 'Data' / 'GetNewDataUpdate')
    p.add_argument('--output-dir', default=default_output, help='Output directory for CSV')
    p.add_argument('--worksheet-index', type=int, default=0, help='Index of worksheet to process (0-based)')
    p.add_argument('--worksheet-name', type=str, default=None, help='Worksheet name to process (overrides --worksheet-index when provided)')
    p.add_argument('--filename', default='all_weekly_priority_cases.csv', help='Output filename')
    p.add_argument('--filter', action='append', help='Filter in the form field=value1,value2 (can repeat)')
    p.add_argument('--split-by', action='append', help='Column name to split output by; will save one file per value (can repeat)')
    p.add_argument('--years', action='append', help='Years to fetch (e.g., 2563 or 2563,2564). Can repeat.')
    p.add_argument('--also-fetch', action='append', help='Additional worksheet name(s) to fetch after applying parameters/filters (can repeat)')
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

    results = []

    # choose worksheet identifier: prefer name if provided, otherwise index
    worksheet_identifier = args.worksheet_name if getattr(args, 'worksheet_name', None) else args.worksheet_index

    # Retrieve workbook parameters (best-effort) to discover year parameter name/values
    workbook, sample_ws = get_worksheet(args.url, worksheet_identifier)
    year_param_name = None
    year_param_column = None
    year_param_values = None
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
                    # prefer the human-friendly column name (e.g., 'ปี') for setting parameters
                    if col is not None and (str(col).lower().find('ปี') >= 0 or str(col).lower().find('year') >= 0):
                        year_param_column = col
                        year_param_name = pname
                        year_param_values = [str(v) for v in (vals or [])]
                        break
                    # fallback: pick first parameter that has values and name
                    if year_param_name is None and (col or pname) and vals:
                        year_param_column = col
                        year_param_name = pname
                        year_param_values = [str(v) for v in vals]
                except Exception:
                    continue
    except Exception:
        logging.exception('Error reading workbook parameters')

    # If user didn't pass years but we discovered parameter values, default to those
    if not years and year_param_values:
        years = year_param_values

    # If no years discovered or provided, run without per-year loop
    if not years:
        # Single run
        # If user requested `also_fetch`, they want only the additional worksheet(s) saved
        if getattr(args, 'also_fetch', None):
            logging.info(f'User requested additional worksheets: {args.also_fetch}; skipping main worksheet save')
            results = []
            for extra in args.also_fetch:
                logging.info(f'Also fetching worksheet {extra} (no year parameters)')
                # Load map worksheet to extract filter metadata (not dataframe columns)
                wb_map, t_map = get_worksheet(args.url, worksheet_identifier)
                if t_map is None:
                    logging.error('Could not load map worksheet')
                    continue

                split_cols = args.split_by or []
                if not split_cols:
                    logging.warning('No split_by provided; saving full target worksheet once')
                    fetched = fetch_other_worksheet_after_server_filters(t_map, [], extra)
                    out = save_filtered_csv(fetched, args.output_dir, f"{safe_filename_component(extra)}_{args.filename}")
                    results.append({'name': extra, 'filename': os.path.basename(out), 'rows': len(fetched), 'path': out})
                    continue

                # Extract domain values for ALL split dimensions from map worksheet's FILTER metadata
                try:
                    map_filters = t_map.getFilters()
                    # Build map of filter name -> filter metadata
                    filter_map = {}
                    for f in (map_filters or []):
                        if isinstance(f, dict) and 'column' in f:
                            filter_map[f['column'].lower()] = f
                    
                    # For each split column, extract domain values
                    all_domains = []  # list of (filter_name, [values])
                    for sname in split_cols:
                        split_filter = filter_map.get(sname.lower())
                        if split_filter and 'values' in split_filter:
                            # Filter out special values
                            domain_vals = [v for v in split_filter['values'] if v and str(v) != 'เลือกทุกครั้งเพื่อแสดงอัตราป่วย']
                            all_domains.append((sname, domain_vals))
                            logging.info(f'Found {len(domain_vals)} values for filter {sname} from map worksheet')
                        else:
                            logging.warning(f'Could not find filter {sname} in map worksheet filters; available filters: {list(filter_map.keys())}')
                            all_domains.append((sname, []))
                except Exception as e:
                    logging.exception(f'Error extracting filter values from map worksheet: {e}')
                    all_domains = [(s, []) for s in split_cols]

                # Check if we have valid domains
                if not all_domains or not all(vals for _, vals in all_domains):
                    logging.warning(f'No valid domain values found for split columns; saving full target worksheet once')
                    fetched = fetch_other_worksheet_after_server_filters(t_map, [], extra)
                    out = save_filtered_csv(fetched, args.output_dir, f"{safe_filename_component(extra)}_{args.filename}")
                    results.append({'name': extra, 'filename': os.path.basename(out), 'rows': len(fetched), 'path': out})
                    continue

                value_lists = [vals for _, vals in all_domains]
                filter_names = [name for name, _ in all_domains]
                
                for combo in itertools.product(*value_lists):
                    # Build server filter pairs for this combination
                    server_pairs = [(name, val) for name, val in zip(filter_names, combo)]
                    logging.info(f'Applying server filters {server_pairs} on map to fetch {extra}')
                    fetched = fetch_other_worksheet_after_server_filters(t_map, server_pairs, extra)
                    
                    # Build filename from combination values (multi-dimensional case)
                    if len(combo) == 1:
                        # Single dimension: use value directly as filename
                        fname = f"{safe_filename_component(combo[0])}.csv"
                        out_dir = args.output_dir
                    else:
                        # Multi-dimensional: create subdirectories for dimensions
                        # e.g., output_dir/โรค/กลุ่มอายุ/<disease>_<age_group>.csv
                        out_dir = Path(args.output_dir)
                        for dim_name in filter_names:
                            out_dir = out_dir / dim_name
                        fname_parts = [safe_filename_component(v) for v in combo]
                        fname = '__'.join(fname_parts) + '.csv'
                        out_dir = str(out_dir)
                    
                    out = save_filtered_csv(fetched, out_dir, fname)
                    results.append({'name': extra, 'filename': os.path.basename(out), 'rows': len(fetched), 'path': out})
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
                )
            else:
                results = process_first_worksheet(args.url, args.output_dir, worksheet_identifier, args.filename)
    else:
        # iterate years and save into per-year subfolders
        logging.info(f'Running for years: {years}')
        for y in years:
            per_year_output = str(Path(args.output_dir) / str(y))
            param_identifier = year_param_column if year_param_column else year_param_name
            params_to_apply = [(param_identifier, y)] if param_identifier else None

            if filter_conditions or args.split_by:
                logging.info(f'Year={y}: filters={filter_conditions}, split_by={args.split_by}')
                if getattr(args, 'also_fetch', None):
                    r = []
                    for extra in args.also_fetch:
                        logging.info(f'Year={y}: also fetching worksheet {extra} via map filters')
                        # Load map worksheet (with year parameter already applied) to extract filter metadata
                        wb_map, t_map = get_worksheet(args.url, worksheet_identifier, parameters=params_to_apply)
                        if t_map is None:
                            logging.error('Could not load map worksheet to derive split values')
                            continue

                        split_cols = args.split_by or []
                        if not split_cols:
                            logging.warning('No split_by provided; saving full target worksheet once')
                            fetched = fetch_other_worksheet_after_server_filters(t_map, [], extra)
                            out = save_filtered_csv(fetched, per_year_output, f"{safe_filename_component(extra)}_{args.filename}")
                            r.append({'name': extra, 'filename': os.path.basename(out), 'rows': len(fetched), 'path': out})
                            continue

                        # Extract domain values for ALL split dimensions from map worksheet's FILTER metadata
                        try:
                            map_filters = t_map.getFilters()
                            # Build map of filter name -> filter metadata
                            filter_map = {}
                            for f in (map_filters or []):
                                if isinstance(f, dict) and 'column' in f:
                                    filter_map[f['column'].lower()] = f
                            
                            # For each split column, extract domain values
                            all_domains = []  # list of (filter_name, [values])
                            for sname in split_cols:
                                split_filter = filter_map.get(sname.lower())
                                if split_filter and 'values' in split_filter:
                                    # Filter out special values
                                    domain_vals = [v for v in split_filter['values'] if v and str(v) != 'เลือกทุกครั้งเพื่อแสดงอัตราป่วย']
                                    all_domains.append((sname, domain_vals))
                                    logging.info(f'Year={y}: Found {len(domain_vals)} values for filter {sname}')
                                else:
                                    logging.warning(f'Year={y}: Could not find filter {sname} in map worksheet filters; available: {list(filter_map.keys())}')
                                    all_domains.append((sname, []))
                        except Exception as e:
                            logging.exception(f'Year={y}: Error extracting filter values from map worksheet: {e}')
                            all_domains = [(s, []) for s in split_cols]

                        # Check if we have valid domains
                        if not all_domains or not all(vals for _, vals in all_domains):
                            logging.warning(f'Year={y}: No valid domain values found for split columns; saving full target worksheet once')
                            fetched = fetch_other_worksheet_after_server_filters(t_map, [], extra)
                            out = save_filtered_csv(fetched, per_year_output, f"{safe_filename_component(extra)}_{args.filename}")
                            r.append({'name': extra, 'filename': os.path.basename(out), 'rows': len(fetched), 'path': out})
                            continue

                        # Generate cartesian product of all split dimensions
                        import itertools
                        value_lists = [vals for _, vals in all_domains]
                        filter_names = [name for name, _ in all_domains]
                        
                        for combo in itertools.product(*value_lists):
                            # Build server filter pairs for this combination
                            server_pairs = [(name, val) for name, val in zip(filter_names, combo)]
                            logging.info(f'Year={y}: applying server filters {server_pairs} on map to fetch {extra}')
                            fetched = fetch_other_worksheet_after_server_filters(t_map, server_pairs, extra)
                            
                            # Build filename from combination values (multi-dimensional case)
                            if len(combo) == 1:
                                # Single dimension: use value directly as filename
                                fname = f"{safe_filename_component(combo[0])}.csv"
                                out_dir = per_year_output
                            else:
                                # Multi-dimensional: create subdirectories for dimensions
                                # e.g., per_year_output/โรค/กลุ่มอายุ/<disease>_<age_group>.csv
                                out_dir = Path(per_year_output)
                                for dim_name in filter_names:
                                    out_dir = out_dir / dim_name
                                fname_parts = [safe_filename_component(v) for v in combo]
                                fname = '__'.join(fname_parts) + '.csv'
                                out_dir = str(out_dir)
                            
                            out = save_filtered_csv(fetched, out_dir, fname)
                            r.append({'name': extra, 'filename': os.path.basename(out), 'rows': len(fetched), 'path': out})
                else:
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
            else:
                logging.info(f'Year={y}: fetching full worksheet into {per_year_output}')
                r = process_first_worksheet(args.url, per_year_output, worksheet_identifier, args.filename, parameters=params_to_apply)
                if getattr(args, 'also_fetch', None):
                    for extra in args.also_fetch:
                        logging.info(f'Year={y}: also fetching worksheet {extra} (full)')
                        try:
                            wb, t_extra = get_worksheet(args.url, extra, parameters=params_to_apply)
                            if t_extra is not None:
                                df_extra = pd.DataFrame(t_extra.data) if t_extra.data is not None else pd.DataFrame()
                                extra_template = f"{safe_filename_component(extra)}_{args.filename}"
                                out = save_filtered_csv(df_extra, per_year_output, extra_template)
                                r.append({'name': getattr(t_extra, 'name', extra), 'filename': os.path.basename(out), 'rows': len(df_extra), 'path': out})
                        except Exception:
                            logging.exception(f'Failed to fetch additional worksheet {extra} for year {y}')

            results.extend(r)

    for r in results:
        logging.info(f"  Saved: {r['path']} (rows={r['rows']})")
    logging.info('Data collection complete.')

if __name__ == '__main__':
    main()