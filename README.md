# Introduction

## Requirements
- Python 3.10.4
- R >= 4.3.2
- pip 21.1.2
- Joinpoint CMD 5200: [Application for Windows Batch/Callable Version of Joinpoint Regression Software](https://surveillance.cancer.gov/joinpoint/callable/)

---

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

```python
## collect data from weekly data source (2025 onwards)
python3 ID_TH/ScriptGetdata/GetNewData.py --years 2568

## collect data from weekly data source (2025 onwards) with splitting by age group
python3 ID_TH/ScriptGetdata/GetNewData.py --years 2568 --split-by age_group

## collect data from weekly data source (2025 and 2024) with splitting by chw (province)
python3 ID_TH/ScriptGetdata/GetNewData.py --years 2568,2567 --split-by chw

## collect data from weekly data source (2025) with splitting by health service region (SKR)
python3 ID_TH/ScriptGetdata/GetNewData.py --years 2568 --split-by SKR
```