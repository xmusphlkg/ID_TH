# Introduction

## Requirements
- Python 3.12.3
- R >= 4.3.2
- pip 21.1.2
- Joinpoint CMD 5200: [Application for Windows Batch/Callable Version of Joinpoint Regression Software](https://surveillance.cancer.gov/joinpoint/callable/)

# Local setting

## Calendar of updates

| Buddhist year in Thailand | Gregorian year |
| --- | --- |
| 2551 | 2008 |
| 2552 | 2009 |
| 2553 | 2010 |
| 2554 | 2011 |
| 2555 | 2012 |
| 2556 | 2013 |
| 2557 | 2014 |
| 2558 | 2015 |
| 2559 | 2016 |
| 2560 | 2017 |
| 2561 | 2018 |
| 2562 | 2019 |
| 2563 | 2020 |
| 2564 | 2021 |
| 2565 | 2022 |
| 2566 | 2023 |
| 2567 | 2024 |
| 2568 | 2025 |


## Health zones in Thailand, 2024

| Health Zone | Provinces Included |
| --- | --- |
| Zone 1 | Chiang Mai, Chiang Rai, Lamphun, Lampang, Mae Hong Son, Nan, Phayao, Phrae |
| Zone 2 | Phitsanulok, Phetchabun, Sukhothai, Tak, Uttaradit |
| Zone 3 | Chai Nat, Kamphaeng Phet, Nakhon Sawan, Uthai Thani |
| Zone 4 | Ang Thong, Lop Buri, Nakhon Nayok, P.Nakhon S.Ayutthaya, Pathum Thani, Saraburi, Sing Buri |
| Zone 5 | Kanchanaburi, Nakhon Pathom, Ratchaburi, Prachuap Khiri Khan, Ratchaburi, Samut Sakhon, Samut Songkhram, Suphan Buri |
| Zone 6 | Chachoengsao, Chanthaburi, Chon Buri, Prachin Buri, Rayong, Sa Kaeo, Samut Prakan, Trat |
| Zone 7 | Kalasin, Khon Kaen, Maha Sarakham, Roi Et |
| Zone 8 | Bungkan, Loei, Nakhon Phanom, Nong Bua Lam Phu, Nong Khai, Sakon Nakhon, Udon Thani |
| Zone 9 | Buri Ram, Chaiyaphum, Surin, Nakhon Ratchasima, Surin |
| Zone 10 | Amnat Charoen, Mukdahan, Si Sa Ket, Ubon Ratchathani, Yasothon |
| Zone 11 | Chumphon, Krabi, Nakhon Si Thammarat, Phangnga, Phuket, Ranong, Surat Thani |
| Zone 12 | Narathiwat, Pattani, Phatthalung, Satun, Songkhla, Yala |
| Zone 13 | Bangkok |

# Data Description

Dashboard list: https://doe.moph.go.th/app01/?page_id=764

## From 2008 to 2024: monthly infectious disease data in Thailand

Data Source: [Bureau of Epidemiology, Ministry of Public Health, Thailand](https://doe1.moph.go.th/surdata/index.php).

Files available for download from the Bureau of Epidemiology website are in RTF format.

| Raw file name | Cleaned file name | File Description                                             |
| --- | --- | --- |
| mcd_disease_xx.rtf | disease_mcd.csv | Number of cases and deaths by month and province |
| rate_disease_xx.rtf | disease_rate.csv | Number and rate per 100,000 population of cases and deaths by province |
| ac_disease_xx.rtf | disease_ac.csv | Number of infections: Number of cases by age grouped and province |
| ad_disease_xx.rtf | disease_ad.csv | Number of deaths: Number of deaths by age grouped and province	|
| race_disease_xx.rtf | Not cleaned | Number of cases and deaths: Number of cases and deaths by nationality and province|
| c_occ_disease_xx.rtf | Not cleaned | Number of patients: Number of cases by occupation |
| d_occ_disease_xx.rtf | Not cleaned | Number of deaths: Number of deaths by occupation |

## From 2025 onwards: weekly infectious disease data in Thailand

Data Source: [Bureau of Epidemiology, Ministry of Public Health, Thailand](https://dvis3.ddc.moph.go.th/t/DDC_CENTER_DOE/views/priority_v2/Dashboard2?%3Aembed=y&%3AisGuestRedirectFromVizportal=y).

https://dvis3.ddc.moph.go.th/t/DDC_CENTER_DOE/views/DDS2/Dashboard_table?%3Aembed=y&%3AisGuestRedirectFromVizportal=y

# Data collection scripts

Data collection scripts are in the `ScriptGetdata` folder. The main script is `GetNewData.py` and `GetData.py`, which collect data from the above two sources respectively.

## Monthly data source (2008-2024)

```python
## Sync data status from monthly data source (2008-2024)
python3 ID_TH/ScriptGetdata/SyncData.py

## Get data from monthly data source (2008-2024)
python3 ID_TH/ScriptGetdata/GetData.py
```

## Weekly data source (2025 onwards)

Drop

```python
## collect data from weekly data source (2025 onwards)
python3 ID_TH/ScriptGetdata/GetNewData.py --years 2568 --url 'https://dvis3.ddc.moph.go.th/t/DDC_CENTER_DOE/views/DDS2/Dashboard_table?%3Aembed=y&%3AisGuestRedirectFromVizportal=y'

## collect data from weekly data source (2025 onwards) with splitting by age group
python3 ID_TH/ScriptGetdata/GetNewData.py --years 2568 --split-by age_group

## collect data from weekly data source (2025 and 2024) with splitting by chw (province)
python3 ID_TH/ScriptGetdata/GetNewData.py --years 2568,2567 --split-by chw

## collect data from weekly data source (2025) with splitting by health service region (SKR)
python3 ID_TH/ScriptGetdata/GetNewData.py --years 2568 --split-by SKR
```

```
# collect data from weekly data source (2025) with also fetching the province-level distribution table
python3 ID_TH/ScriptGetdata/GetNewDataUpdate.py \
  --worksheet-name 'แผนที่ระดับจังหวัด' \
  --years 2568 \
  --split-by 'โรค' \
  --also-fetch 'ตารางการกระจายผู้ป่วยจังหวัด' \
  --output-dir ID_TH/Data/GetNewDataUpdate
```

```
# collect data from weekly data source (2025) with splitting by age group and also fetching the province-level distribution table
python3 ID_TH/ScriptGetdata/GetNewDataUpdate.py \
  --worksheet-name 'แผนที่ระดับจังหวัด' \
  --years 2568 \
  --split-by 'โรค' \
  --split-by 'กลุ่มอายุ' \
  --also-fetch 'ตารางการกระจายผู้ป่วยจังหวัด' \
  --output-dir ID_TH/Data/GetNewDataUpdate
```

# GetNewDataUpdate.py - Multiprocessing Enhancement

## Overview

The refactored `GetNewDataUpdate.py` supports multiprocessing for significantly faster data extraction. Key improvements include:

### Architecture Optimization
- **Function Modularization**: Common functions moved to `GetNewDataFunction.py`
  - `extract_split_domains_from_filters()`: Extract filter domain values
  - `fetch_other_worksheet_after_server_filters()`: Apply filters and fetch target worksheet

### Multiprocessing Support
- **Pre-generate Task List**: Fetch all metadata (parameters, worksheets, filters) first, then generate complete task list
- **Parallel Execution**: Uses Python `multiprocessing.Pool` to process all tasks in parallel
- **Worker Function**: `process_single_task()` handles individual extraction tasks

### User Experience Optimization
- **Progress Bar**: Real-time progress display using `tqdm` (optional)
- **Silent Mode**: Reduced repetitive console output, metadata shown only at start
- **Log Level Control**: Detailed info logged to file, console shows only critical messages
- **Result Summary**: Display success/failure statistics upon completion

### Installation

```bash
# Install progress bar support (recommended)
pip install tqdm
```

*Note: tqdm is optional - the script works without it, just without progress bars*

### Workflow

```
1. Fetch dashboard metadata
   ├─ Load workbook and worksheet
   ├─ Extract parameters (year parameter)
   └─ Get filters metadata

2. Generate task list
   ├─ Iterate through all years
   ├─ Extract split-by dimension domain values
   ├─ Generate Cartesian product combinations
   └─ Create task dictionary for each combination

3. Parallel execution
   ├─ Use multiprocessing.Pool
   ├─ Each worker loads worksheet independently
   ├─ Apply filters and extract data
   └─ Save to CSV

4. Collect results
   └─ Aggregate all task results and log
```

## Usage

### Basic Usage (single-process, backward compatible)
```bash
python ID_TH/ScriptGetdata/GetNewDataUpdate.py \
  --worksheet-name 'แผนที่ระดับจังหวัด' \
  --years 2568 \
  --split-by 'โรค' \
  --also-fetch 'ตารางการกระจายผู้ป่วยจังหวัด' \
  --output-dir ID_TH/Data/GetNewDataUpdate \
  --workers 1
```

### Multiprocessing (recommended)
```bash
# Use 4 worker processes
python ID_TH/ScriptGetdata/GetNewDataUpdate.py \
  --worksheet-name 'แผนที่ระดับจังหวัด' \
  --years 2568 \
  --split-by 'โรค' \
  --also-fetch 'ตารางการกระจายผู้ป่วยจังหวัด' \
  --output-dir ID_TH/Data/GetNewDataUpdate \
  --workers 4

# Auto-use all CPU cores
python ID_TH/ScriptGetdata/GetNewDataUpdate.py \
  --worksheet-name 'แผนที่ระดับจังหวัด' \
  --years 2568 \
  --split-by 'โรค' \
  --also-fetch 'ตารางการกระจายผู้ป่วยจังหวัด' \
  --output-dir ID_TH/Data/GetNewDataUpdate \
  --workers 0
```

### Multi-year + Multi-dimension Split
```bash
# Split two dimensions (disease × age group), use 8 workers
python ID_TH/ScriptGetdata/GetNewDataUpdate.py \
  --worksheet-name 'แผนที่ระดับจังหวัด' \
  --years 2566,2567,2568 \
  --split-by 'โรค' \
  --split-by 'กลุ่มอายุ' \
  --also-fetch 'ตารางการกระจายผู้ป่วยจังหวัด' \
  --output-dir ID_TH/Data/GetNewDataUpdate \
  --workers 8
```

### Resume Interrupted Downloads
```bash
# Skip already downloaded files to resume interrupted work
python ID_TH/ScriptGetdata/GetNewDataUpdate.py \
  --worksheet-name 'แผนที่ระดับจังหวัด' \
  --years 2566,2565,2564,2563 \
  --split-by 'โรค' \
  --split-by 'กลุ่มอายุ' \
  --also-fetch 'ตารางการกระจายผู้ป่วยจังหวัด' \
  --output-dir ID_TH/Data/GetNewDataUpdate \
  --workers 20 \
  --no-overwrite
```

## Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `--workers` | int | 1 | Number of worker processes. `0` or `-1` uses all CPU cores |
| `--worksheet-name` | str | - | Map worksheet name |
| `--years` | str | - | Year list, comma-separated or repeated parameter |
| `--split-by` | str | - | Split dimension (repeatable), e.g., `โรค`, `กลุ่มอายุ` |
| `--also-fetch` | str | - | Target worksheet name |
| `--output-dir` | str | `Data/GetNewDataUpdate` | Output directory |
| `--no-overwrite` | flag | False | Skip files that already exist (default: overwrite existing files) |

## Performance Comparison

Assuming 56 diseases need to be extracted:

| Workers | Estimated Time | Speedup |
|---------|---------------|---------|
| 1 | ~56 minutes | 1x |
| 4 | ~14 minutes | 4x |
| 8 | ~7 minutes | 8x |
| 16 | ~3.5 minutes | 16x |

*Actual performance depends on network speed and server response time*

For 504 combinations (56 diseases × 9 age groups):

| Workers | Estimated Time | Speedup |
|---------|---------------|---------|
| 1 | ~504 minutes | 1x |
| 4 | ~126 minutes | 4x |
| 8 | ~63 minutes | 8x |

## Notes

1. **Memory Usage**: Each worker loads an independent worksheet; more workers = more memory
2. **Network Limits**: Too many concurrent requests may trigger server throttling; start with 4-8 workers
3. **Logging**: All workers share one log file; log entries may be interleaved
4. **Error Handling**: Single task failure won't affect other tasks; errors recorded in results

## Example Output

### Console Output (concise mode)
```
Fetching dashboard metadata...
Available worksheets:
  [0] ชื่อโรคการกระจาย
  [1] ตารางการกระจายผู้ป่วยจังหวัด
  [2] แผนที่ระดับจังหวัด
Processing worksheet: แผนที่ระดับจังหวัด
Processing 56 tasks with 4 worker(s)...
Progress: 100%|████████████████████████| 56/56 [15:23<00:00, 16.5s/it]

============================================================
Data collection complete!
  Total tasks: 56
  Successful: 56
  Failed: 0
  Total rows: 221,200
============================================================
```

# Changelog

## 2025-12-03 - v2.0 Multiprocessing Optimization

### 🚀 Main Improvements

#### 1. Multiprocessing Parallel Processing
- **Task Pre-generation Mechanism**: First fetch all metadata (parameters, worksheets, filters), then generate complete task list
- **Parallel Execution**: Uses `multiprocessing.Pool` for parallel processing with customizable worker count
- **Performance Boost**: 4 workers achieve 4x speedup, 8 workers achieve 8x speedup

**New Parameter:**
```bash
--workers N    # N>0: use N workers; N=0 or -1: use all CPU cores; default=1
```

#### 2. Code Modularization Refactoring
- **Function Migration**: Moved common functions to `GetNewDataFunction.py`
  - `extract_split_domains_from_filters()`: Extract filter domain values
  - `fetch_other_worksheet_after_server_filters()`: Apply filters and fetch data
  
- **Parameter Enhancement**:
  - `get_worksheet(..., verbose=False)`: Control console output
  - `save_filtered_csv(..., verbose=False)`: Control console output

#### 3. User Experience Optimization

##### 3.1 Silent Mode
- ✅ Removed duplicate console output
- ✅ Display worksheet info only on first load
- ✅ Worker processes run silently, no console output
- ✅ Detailed logs written to file only

##### 3.2 Progress Bar Display
- ✅ Real-time progress using `tqdm`
- ✅ Shows task count, completion percentage, estimated time remaining
- ✅ Works without tqdm (no progress bar)

```
Progress: 100%|████████████████████████| 56/56 [15:23<00:00, 16.5s/it]
```

##### 3.3 Result Summary
Statistics displayed after completion:
```
============================================================
Data collection complete!
  Total tasks: 56
  Successful: 56
  Failed: 0
  Total rows: 221,200
============================================================
```

#### 4. Log Level Optimization
- ✅ Disabled tableauscraper warnings: `logging.getLogger('tableauScraper').setLevel(logging.ERROR)`
- ✅ Metadata details use DEBUG level (reduces log file size)
- ✅ Important info uses INFO level
- ✅ Errors and warnings logged normally

### 🐛 Fixed Issues

1. ✅ **Duplicate Output**: Each worker printed worksheet info
   - Fix: Added verbose parameter, workers use silent mode

2. ✅ **Excessive Logging**: Full metadata logged on each worksheet load
   - Fix: Metadata uses DEBUG level, reduced INFO logs

3. ✅ **tableauscraper Warnings**: Many "no data dictionary" warnings
   - Fix: Set tableauScraper logger to ERROR level

4. ✅ **Missing Progress Feedback**: Long runs without feedback
   - Fix: Added tqdm progress bar

### 📝 Compatibility Notes

- ✅ Fully backward compatible with v1.0 usage
- ✅ Default `--workers 1` equals single-process mode
- ✅ Works without tqdm installation
- ✅ All existing parameters unchanged

## v1.0 - Initial Version

### Features
- Extract data from Tableau dashboard
- Support dimension splitting (split-by)
- Support multi-worksheet extraction (also-fetch)
- Support year parameters
- Cartesian product combination generation
- Auto-create directory structure

# To do list

- [ ] Fixed 0 cases of scarlat fever, TB, EM and Trichomoniasis in 2024
- [ ] Collect TB data from new weekly data source (2025 onwards)
- [ ] Transition from weekly data to monthly data aggregation
- [ ] Add cross-validation for time series model selection
