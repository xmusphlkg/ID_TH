# ShinyAnalysis

ShinyAnalysis is the upload-first companion app to the Thailand Shiny dashboard.

Purpose:
- Run counterfactual recovery analysis on user-provided monthly disease surveillance data.
- Keep bundled Thailand data as an internal example dataset and demonstration library.

Standalone deployment:
- Deploy the entire `ShinyAnalysis` folder.
- The app is self-contained and reads bundled example files from `data/temp/` and `data/Outcome/`.

Upload schema:
- Required columns: `date`, `disease`, `cases`
- Accepted date formats: `YYYY-MM-DD` or `YYYY-MM`
- Duplicate rows within the same disease-month are summed automatically.

Bundled example:
- The Upload Analysis tab can load the Thailand example dataset directly.
- The Thailand Example menu contains fixed demonstration pages based on bundled cached outputs.