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

# Default URL from the notebook
DEFAULT_URL = "https://dvis3.ddc.moph.go.th/t/DDC_CENTER_DOE/views/priority_v2/Dashboard2?%3Aembed=y&%3AisGuestRedirectFromVizportal=y"

# columns from the prompt (keep these if present)
KEEP_COLS = [
    "ชื่อโรค-value",
    "ชื่อโรค-alias","MIN(Number of Records)-alias",
    "สัปดาห์-value",
    "สัปดาห์-alias",
    "ATTR(week_number)-alias",
    "ATTR(disease (copy))-alias",
    "ATTR(disease_th)-alias",
    "AGG(จำนวนป่วย (แทน null))-alias",
    "AGG(Formatted Percent Change)-alias","AGG(Arrow Type)-alias","AGG(Consecutive Weeks)-alias"
]

# optional rename map to English-friendly names
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


# Setup logging
_LOG_PATH = Path(__file__).resolve().parent / 'GetNewData.log'
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

        # select only columns that exist in the scraped data
        present = [c for c in KEEP_COLS if c in pdf.columns]
        if not present:
            # fallback: rename any columns that match our rename_map keys, otherwise keep all
            logging.info("  None of the expected columns found; saving full sheet as fallback.")
            cols_to_rename = {c: RENAME_MAP[c] for c in pdf.columns if c in RENAME_MAP}
            filtered = pdf.rename(columns=cols_to_rename).copy()
        else:
            filtered = pdf[present].rename(columns={c: RENAME_MAP.get(c, c) for c in present}).copy()

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
    # default output dir: repository-relative `Data/GetNewData` next to this script's parent
    default_output = str(Path(__file__).resolve().parent.parent / 'Data' / 'GetNewData')
    p.add_argument('--output-dir', default=default_output, help='Output directory for CSV')
    p.add_argument('--worksheet-index', type=int, default=0, help='Index of worksheet to process (0-based)')
    p.add_argument('--filename', default='all_weekly_priority_cases.csv', help='Output filename')
    p.add_argument('--filter', action='append', help='Filter in the form field=value1,value2 (can repeat)')
    p.add_argument('--split-by', action='append', help='Column name to split output by; will save one file per value (can repeat)')
    p.add_argument('--years', action='append', help='Years to fetch (e.g., 2563 or 2563,2564). Can repeat.')
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

        # inspect available filters from the worksheet (best-effort)
        try:
            available_filters = t.getFilters()
            # logging.info(f'Worksheet filters metadata: {available_filters}')
        except Exception:
            available_filters = None
            logging.info('Could not retrieve worksheet filters via t.getFilters()')

        # log actual dataframe columns to help debug mapping between worksheet filters
        logging.info(f'Available dataframe columns: {list(pd.DataFrame(df).columns)}')

        # map requested filter names to actual DataFrame columns
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

        # Build available worksheet filter metadata map (column name -> entry)
        wf_map = {}
        try:
            if available_filters:
                for f in available_filters:
                    if isinstance(f, dict) and 'column' in f:
                        wf_map[f['column']] = f
        except Exception:
            wf_map = {}


        # apply filters on the DataFrame to produce the base dataset once (the "总的")
        base_mask = pd.Series([True] * len(pdf))
        for col, vals in mapped_conditions.items():
            if not vals:
                continue
            if col in pdf.columns:
                base_mask &= pdf[col].astype(str).isin([str(v) for v in vals])

        # base_df is the single dataset we will save first and from which we'll derive split values where possible
        base_df = pdf[base_mask].copy()
        results = []

        # save the overall dataset first (suffix 'filtered' if filters provided, otherwise 'full')
        overall_suffix = 'filtered' if mapped_conditions else 'full'
        if save_overall:
            out = save_filtered_csv(base_df, output_dir, filename_template.format(suffix=overall_suffix))
            results.append({'name': t.name, 'filename': os.path.basename(out), 'rows': len(base_df), 'path': out})

        # If no split requested, return after saving overall
        if not split_by:
            return results

        # Otherwise, split_by was requested. Map split_by names to actual dataframe columns (reuse mapping logic)
        inv_map = {v: k for k, v in RENAME_MAP.items()}
        # We will separate split columns into dataframe-split and server-split (tableau filters)
        df_split_cols: List[str] = []
        server_split_cols: List[str] = []
        for sname in (split_by or []):
            # direct dataframe column
            if sname in pdf.columns:
                df_split_cols.append(sname)
                continue
            # rename map (user passed original localized name)
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

        # use shared safe filename helper from utils

        # build domains for df split columns and server split columns separately
        value_lists = []
        split_keys: List[Tuple[str, str]] = []  # list of (type, name) where type is 'df' or 'server'

        # server split columns: prefer values present in base_df (to avoid re-fetching); otherwise fall back to worksheet metadata
        for col in server_split_cols:
            # try to find a matching dataframe column in base_df for this server filter (case-insensitive and rename maps)
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
            # if we derived values from base_df, treat as df split to avoid server fetch; otherwise server
            split_keys.append(('df' if df_col is not None else 'server', df_col if df_col is not None else col))

        # dataframe split columns
        for col in df_split_cols:
            if col in mapped_conditions and mapped_conditions[col]:
                vals = [str(v) for v in mapped_conditions[col]]
            else:
                vals = [v for v in pd.unique(pdf[col].dropna().astype(str))]
            if not vals:
                vals = ['NA']
            value_lists.append(vals)
            split_keys.append(('df', col))

        # iterate cartesian product of all split columns
        # cache for server fetches to avoid repeated setFilter calls for the same server_pairs
        server_cache = {}

        for combo in itertools.product(*value_lists):
            parts = []

            # build server filter pairs and df mask for this combo
            server_pairs = []
            df_mask = None
            for (kind, name), val in zip(split_keys, combo):
                parts.append(f"{(name if name is not None else 'unknown')}-{safe_filename_component(val)}")
                if kind == 'server':
                    server_pairs.append((name, val))
                else:
                    # dataframe split column mask relative to base_df
                    if df_mask is None:
                        df_mask = pd.Series([True] * len(base_df))
                    df_mask &= base_df[name].astype(str) == str(val)

            # Start with server-side fetch if needed
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
                # no server filters, use base_df
                fetched_df = base_df.copy()

            # apply base_mask (local mapped_conditions that were matched to df columns)
            # mapped_conditions were already applied to produce base_df; if fetched_df came from server, it may need local filtering
            if fetched_df is base_df or fetched_df.equals(base_df):
                combined = fetched_df
            else:
                local_mask = pd.Series([True] * len(fetched_df))
                for col, vals in mapped_conditions.items():
                    if col in fetched_df.columns:
                        local_mask &= fetched_df[col].astype(str).isin([str(v) for v in vals])
                combined = fetched_df[local_mask].copy()

            # apply df_mask (split-by df columns) if present
            if df_mask is not None and not combined.empty:
                # df_mask was constructed relative to base_df; remap to combined by matching indexes
                idx_mask = df_mask.reindex(combined.index, fill_value=False)
                subset = combined[idx_mask].copy()
            else:
                subset = combined.copy()

            # Build output subdirectory based on variable NAMES (one folder per split variable)
            # e.g. output_dir/<var1>/<var2>/... and filename will be the concatenated values
            var_names = [(name if name is not None else 'unknown') for (_k, name) in split_keys]
            out_dir_sub = Path(output_dir)
            for vn in var_names:
                out_dir_sub = out_dir_sub / vn

            # filename: use safe-converted value(s) only (no base filename prefix)
            # join values with double underscore for multi-split cases
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

    # Retrieve workbook parameters (best-effort) to discover year parameter name/values
    workbook, sample_ws = get_worksheet(args.url, args.worksheet_index)
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
        if filter_conditions or args.split_by:
            logging.info(f'Filter conditions: {filter_conditions}, split_by: {args.split_by}')
            results = download_with_filters(
                args.url,
                filter_conditions,
                args.output_dir,
                args.worksheet_index,
                filename_template=args.filename.replace('.csv', '_{suffix}.csv'),
                split_by=args.split_by,
            )
        else:
            results = process_first_worksheet(args.url, args.output_dir, args.worksheet_index, args.filename)
    else:
        # iterate years and save into per-year subfolders
        logging.info(f'Running for years: {years}')
        for y in years:
            per_year_output = str(Path(args.output_dir) / str(y))
            # build parameters list: prefer the parameter column (e.g., 'ปี') if available,
            # otherwise fall back to the parameterName
            param_identifier = year_param_column if year_param_column else year_param_name
            params_to_apply = [(param_identifier, y)] if param_identifier else None

            if filter_conditions or args.split_by:
                logging.info(f'Year={y}: filters={filter_conditions}, split_by={args.split_by}')
                # when splitting, avoid saving the overall "_full" file to prevent duplicates
                r = download_with_filters(
                    args.url,
                    filter_conditions,
                    per_year_output,
                    args.worksheet_index,
                    filename_template=args.filename.replace('.csv', '_{suffix}.csv'),
                    split_by=args.split_by,
                    parameters=params_to_apply,
                    save_overall=False if args.split_by else True,
                )
            else:
                logging.info(f'Year={y}: fetching full worksheet into {per_year_output}')
                r = process_first_worksheet(args.url, per_year_output, args.worksheet_index, args.filename, parameters=params_to_apply)

            results.extend(r)

    # logging summary
    for r in results:
        logging.info(f"  Saved: {r['path']} (rows={r['rows']})")

    logging.info('Data collection complete.')

if __name__ == '__main__':
    main()