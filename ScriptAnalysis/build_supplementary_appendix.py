from __future__ import annotations

import argparse
import os
import re
import shutil
import subprocess
import sys
from pathlib import Path
from urllib.parse import quote

import pandas as pd


PROJECT_ROOT = Path(__file__).resolve().parents[1]
SCRIPT_DIR = Path(__file__).resolve().parent
APPENDIX_DIR = PROJECT_ROOT / "Outcome" / "Appendix"
TABLES_DIR = APPENDIX_DIR / "Tables"
TEMPLATES_DIR = SCRIPT_DIR / "templates"
OUTPUT_PATH = APPENDIX_DIR / "Supplementary_Appendix.md"
SOURCE_TABLE_SCRIPT = SCRIPT_DIR / "9_a_generate_appendix_source_tables.R"
SOURCE_TABLE_OUTPUTS = [
    TABLES_DIR / "Appendix_S2_excluded_series.csv",
    TABLES_DIR / "Appendix_S4_predictor_definitions.csv",
    TABLES_DIR / "Appendix_S6_overlap_summary.csv",
    TABLES_DIR / "Appendix_S7_overlap_examples.csv",
    TABLES_DIR / "Appendix_overlap_monthly_validation.csv",
]

TITLE_BLOCK = """<div style="text-align:center;">
  <h3 style="font-family: inherit; font-weight: normal; margin-bottom: 0;">Supplementary Appendix:</h3>
  <h1 style="font-family: inherit; font-weight: bold; font-size: 1.5em;">A counterfactual framework for post-disruption recovery assessment in infectious disease surveillance in Thailand</h1>
  <br>
  <br>
  Kangguo Li et al. (2026)
</div>
"""

ORDERED_DISEASES = [
    "Pneumonia", "Influenza", "Chickenpox", "Mumps", "Measles", "Scarlet fever",
    "Rubella", "Pertussis", "Leprosy", "Diphtheria", "Dengue fever", "Malaria",
    "Scrub Typhus", "Chikungunya", "Leptospirosis", "Melioidosis", "S. suis", "Zika virus",
    "Filariasis", "Trichinosis", "Brucellosis", "JE", "Leishmaniasis", "HFMD",
    "Amebiasis", "Shigellosis", "Typhoid", "Liver fluke", "HAV", "Paratyphoid",
    "Cholera", "HEV", "Enterovirus", "Gonorrhoea", "Syphilis", "HBV",
    "CA (HPV)", "Genital herpes", "Chancroid", "HCV", "HDV", "Other meningitis",
    "Encephalitis",
]

DISEASE_ORDER = {name.lower(): idx for idx, name in enumerate(ORDERED_DISEASES)}

PART4_CATEGORY_LABELS = {
    "Gastrointestinal IDs": "gastrointestinal infectious diseases",
    "Other IDs": "other infectious diseases",
    "Respiratory IDs": "respiratory infectious diseases",
    "Sexually IDs": "sexually transmitted infectious diseases",
    "Vector-borne and zoonotic IDs": "vector-borne and zoonotic infectious diseases",
}

MODELLED_DISEASES = [
    "Pneumonia",
    "Influenza",
    "Chickenpox",
    "Mumps",
    "Scarlet fever",
    "Rubella",
    "Dengue fever",
    "Malaria",
    "Scrub Typhus",
    "Leptospirosis",
    "Melioidosis",
    "S. suis",
    "HFMD",
    "Amebiasis",
    "Shigellosis",
    "Typhoid",
    "HAV",
    "Gonorrhoea",
    "Syphilis",
    "HBV",
    "CA (HPV)",
    "Genital herpes",
    "Chancroid",
    "HCV",
]


def read_text(path: Path) -> str:
    return path.read_text(encoding="utf-8").lstrip("\ufeff")


def write_text(path: Path, text: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8", newline="\n") as handle:
        handle.write(text)


def detect_rscript(explicit: str | None = None) -> str | None:
    candidates: list[str] = []
    if explicit:
        candidates.append(explicit)

    candidates.append(r"C:\Program Files\R\R-4.5.2\bin\Rscript.exe")

    env_rscript = os.environ.get("RSCRIPT")
    if env_rscript:
        candidates.append(env_rscript)

    which_rscript = shutil.which("Rscript")
    if which_rscript:
        candidates.append(which_rscript)

    windows_r_roots = [Path(r"C:/Program Files/R"), Path(r"C:/Program Files (x86)/R")]
    for root in windows_r_roots:
        if not root.exists():
            continue
        for match in sorted(root.glob("R-*/bin/Rscript.exe"), reverse=True):
            candidates.append(str(match))
        for match in sorted(root.glob("*/bin/Rscript.exe"), reverse=True):
            candidates.append(str(match))

    seen: set[str] = set()
    for candidate in candidates:
        resolved = str(Path(candidate).expanduser())
        if resolved in seen:
            continue
        seen.add(resolved)
        if Path(resolved).exists():
            return resolved

    return None


def ensure_source_tables_exist() -> None:
    missing = [path.name for path in SOURCE_TABLE_OUTPUTS if not path.exists()]
    if missing:
        raise FileNotFoundError(
            "Missing appendix source table file(s): " + ", ".join(sorted(missing))
        )


def run_source_table_refresh(rscript: str | None = None) -> None:
    if not SOURCE_TABLE_SCRIPT.exists():
        raise FileNotFoundError(f"Missing prerequisite script: {SOURCE_TABLE_SCRIPT}")

    rscript_path = detect_rscript(rscript)
    if not rscript_path:
        raise RuntimeError(
            "Could not find Rscript for appendix source-table refresh. "
            "Install R or rerun with --skip-source-refresh to use cached table files."
        )

    print(f"Running appendix source-table refresh: {SOURCE_TABLE_SCRIPT.name}")
    try:
        subprocess.run([rscript_path, str(SOURCE_TABLE_SCRIPT)], check=True)
    except subprocess.CalledProcessError as exc:
        raise RuntimeError(
            "Appendix source-table refresh failed. "
            "If the cached CSVs are still valid, rerun with --skip-source-refresh."
        ) from exc
    print("Completed appendix source-table refresh.")


def page_break() -> str:
    return '<div style="page-break-after: always;"></div>'


def is_missing(value) -> bool:
    return value is None or pd.isna(value)


def fmt_num(value, digits: int = 3, trim: bool = False, na: str = "NA") -> str:
    if is_missing(value):
        return na
    text = f"{float(value):.{digits}f}"
    if trim:
        text = text.rstrip("0").rstrip(".")
    return text


def fmt_int(value, na: str = "NA") -> str:
    if is_missing(value):
        return na
    return str(int(round(float(value))))


def fmt_bool(value, na: str = "NA") -> str:
    if is_missing(value):
        return na
    if isinstance(value, str):
        normalized = value.strip().lower()
        if normalized in {"true", "t", "yes", "y", "1"}:
            return "Yes"
        if normalized in {"false", "f", "no", "n", "0"}:
            return "No"
    return "Yes" if bool(value) else "No"


def fmt_date(value, na: str = "NA") -> str:
    if is_missing(value):
        return na
    return pd.to_datetime(value).strftime("%Y-%m-%d")


def fmt_date_or_not_reached(value, na: str = "Not reached") -> str:
    if is_missing(value):
        return na
    return fmt_date(value)


def escape_md(value) -> str:
    if is_missing(value):
        return "NA"
    return str(value).replace("|", "\\|").replace("\r", " ").replace("\n", " ")


def md_table(df: pd.DataFrame) -> str:
    if isinstance(df, pd.Series):
        df = df.to_frame().T

    headers = list(df.columns)
    lines = [
        "| " + " | ".join(headers) + " |",
        "| " + " | ".join(["---"] * len(headers)) + " |",
    ]

    for _, row in df.iterrows():
        lines.append("| " + " | ".join(escape_md(row[col]) for col in headers) + " |")

    return "\n".join(lines)


def extract_markdown_tables(text: str) -> list[str]:
    blocks: list[list[str]] = []
    current: list[str] = []

    for line in text.splitlines():
        if line.startswith("|"):
            current.append(line)
            continue
        if current:
            blocks.append(current)
            current = []

    if current:
        blocks.append(current)

    return ["\n".join(block) for block in blocks]


def join_blocks(blocks: list[str]) -> str:
    return "\n\n".join(block.strip() for block in blocks if block and block.strip())


def render_template(template: str, replacements: dict[str, str]) -> str:
    rendered = template
    for key, value in replacements.items():
        rendered = rendered.replace(f"{{{{{key}}}}}", value)

    unresolved = sorted(set(re.findall(r"\{\{[A-Z0-9_]+\}\}", rendered)))
    if unresolved:
        raise RuntimeError(f"Unresolved template placeholders: {', '.join(unresolved)}")

    return rendered


def ensure_template_has_placeholders(template_name: str, template: str, required_keys: list[str]) -> None:
    placeholders = set(re.findall(r"\{\{([A-Z0-9_]+)\}\}", template))
    missing = sorted(set(required_keys) - placeholders)
    if missing:
        raise RuntimeError(
            f"{template_name} is missing required placeholder(s): {', '.join(missing)}"
        )


def validate_tables_template(template: str) -> None:
    if re.search(r"(?m)^!\[\*\*Fig\. S", template):
        raise RuntimeError("tables.md should not contain figure image blocks.")


def validate_figures_template(template: str) -> None:
    if re.search(r"(?m)^(?:\*\*Table S\d+\.|### Table S\d+\.)", template):
        raise RuntimeError("figures.md should not contain table blocks.")
    if re.search(r"(?m)^## Part \d+:", template):
        raise RuntimeError("figures.md should use '### Part ...' headings inside the Figures section.")


def read_csv(name: str) -> pd.DataFrame:
    return pd.read_csv(TABLES_DIR / name)


def read_xlsx(name: str, sheet: str) -> pd.DataFrame:
    return pd.read_excel(TABLES_DIR / name, sheet_name=sheet)


def status_label(x: str) -> str:
    return {
        "Debt Repaid": "Balanced",
        "Recovered": "Recovered but not balanced",
        "Suppressed": "Suppressed",
        "No Deficit": "No deficit",
    }.get(x, x)


def status_label_rp(x: str) -> str:
    return {
        "Debt Repaid": "Balanced",
        "Recovered": "RP achieved without BP",
        "Suppressed": "Suppressed",
        "No Deficit": "No deficit",
    }.get(x, x)


def make_block(title: str, content_lines: list[str], note: str | None = None) -> str:
    lines = [f"### {title}", ""]
    lines.extend(content_lines)
    if note:
        lines.extend(["", note.strip()])
    lines.extend(["", page_break()])
    return "\n".join(lines).strip()


def make_table_block(title: str, df: pd.DataFrame, note: str | None = None) -> str:
    return make_block(title, [md_table(df)], note=note)


def make_text_block(title: str, text: str, note: str | None = None) -> str:
    return make_block(title, [text.strip()], note=note)


def format_figure_image(number: int, label: str, rel_path: str) -> str:
    alt = f"**Fig. S{number}. {label}.**"
    encoded_path = quote(rel_path.replace("\\", "/"), safe="/")
    return f"![{alt}]({encoded_path})"


def format_figure_entry(number: int, title: str, rel_path: str, caption: str) -> str:
    label = f"**Fig. S{number}. {title}.**"
    encoded_path = quote(rel_path.replace("\\", "/"), safe="/")
    return f"![{label}]({encoded_path})\n\n{label} {caption}"


def render_figure_sequence(
    entries: list[dict[str, str]],
    start_number: int,
    group_size: int = 2,
    intro: str | None = None,
) -> tuple[str, int]:
    lines: list[str] = []
    if intro:
        lines.extend([intro.strip(), ""])

    number = start_number
    for idx, entry in enumerate(entries):
        if idx and group_size and idx % group_size == 0:
            lines.extend(["", page_break(), ""])
        lines.append(format_figure_entry(number, entry["title"], entry["path"], entry["caption"]))
        number += 1

    lines.extend(["", page_break()])
    return "\n".join(lines).strip(), number


def render_single_figure(number: int, title: str, rel_path: str, caption: str) -> str:
    return "\n".join([format_figure_entry(number, title, rel_path, caption), "", page_break()]).strip()


def build_table_s1() -> str:
    flow_summary = read_xlsx("Disease_flow_summary.xlsx", "FlowSummary").copy()
    flow_summary.columns = ["Stage", "N"]
    note = (
        "The disease-selection flow is written here as a compact table so the appendix can keep the curation summary "
        "in the Tables section without duplicating a standalone figure."
    )
    return make_table_block(
        "Table S1. Disease flow from 72 monitored series to the 43-disease descriptive analysis and 24-disease counterfactual analysis.",
        flow_summary,
        note=note,
    )


def build_table_s2() -> str:
    appendix_s2 = read_csv("Appendix_S2_excluded_series.csv")
    counts = appendix_s2["Exclusion category"].value_counts().to_dict()
    note = (
        "These excluded series were concentrated in overlapping surveillance categories "
        f"({counts.get('Overlapping surveillance categories', 0)}), diseases outside the transmissible infectious-disease framework "
        f"({counts.get('Not aligned with the transmissible infectious-disease framework', 0)}), ill-defined or residual categories "
        f"({counts.get('Ill-defined or residual surveillance categories', 0)}), zero-incidence series "
        f"({counts.get('Zero reported incidence over the study period', 0)}), incompletely reported recent series "
        f"({counts.get('Incomplete reporting in the most recent surveillance year', 0)}), and one series with a structural surveillance-definition change."
    )
    return make_table_block(
        "Table S2. Excluded disease series and exclusion category.",
        appendix_s2,
        note=note,
    )


def build_table_s3() -> str:
    table = read_xlsx("Disease_flow_summary.xlsx", "Included43_Not24").copy()
    table = table[[
        "Disease",
        "Shortname",
        "Group",
        "Reason for descriptive-only retention",
    ]]
    counts = table["Reason for descriptive-only retention"].value_counts().to_dict()
    note = (
        f"Across these {len(table)} diseases, the main reasons for descriptive-only retention were insufficient prepandemic counts or sparse long-horizon signal "
        f"({counts.get('Insufficient cases', 0)} diseases), non-seasonal prepandemic structure "
        f"({counts.get('Non-seasonal trend', 0)} diseases), insufficient time coverage "
        f"({counts.get('Insufficient duration', 0)} diseases), and ill-defined residual categories "
        f"({counts.get('Unspecifed disease', 0)} diseases)."
    )
    return make_table_block(
        "Table S3. Diseases retained in the 43-disease descriptive analysis but not modelled counterfactually, with direct reason for descriptive-only retention.",
        table,
        note=note,
    )


def build_table_s4() -> str:
    table = read_csv("Appendix_S4_predictor_definitions.csv").copy()
    vaccine_counts = table["Vaccine"].value_counts().to_dict()
    note = (
        f"This lookup table covers all {len(table)} modelled diseases and supplied the disease-level predictors used in the recovery-timing analyses. "
        f"Vaccine status was classified as unavailable for {vaccine_counts.get('Unavailable', 0)} diseases, optional for {vaccine_counts.get('Optional', 0)} diseases, "
        f"and part of the national EPI schedule for {vaccine_counts.get('EPI', 0)} diseases."
    )
    return make_table_block(
        "Table S4. Predictor definitions used in time-to-recovery analyses",
        table,
        note=note,
    )


def build_table_s5() -> str:
    analysis = read_csv("Threshold_sensitivity_analysis.csv").copy()
    analysis["StatusLabel"] = analysis["Status"].map(status_label_rp)

    config_order = [
        "95% / 3 mo",
        "90% / 2 mo", "90% / 3 mo", "90% / 4 mo",
        "95% / 2 mo", "95% / 4 mo",
        "100% / 2 mo", "100% / 3 mo", "100% / 4 mo",
    ]
    analysis["Config"] = pd.Categorical(analysis["Config"], categories=config_order, ordered=True)
    analysis = analysis.sort_values(["Config", "Shortname"]).reset_index(drop=True)

    primary = analysis[analysis["Config"] == "95% / 3 mo"][["Shortname", "StatusLabel"]].rename(
        columns={"StatusLabel": "PrimaryStatusLabel"}
    )
    joined = analysis.merge(primary, on="Shortname", how="left", sort=False)

    rows: list[dict[str, str]] = []
    reclass_notes: list[str] = []
    for cfg in config_order:
        subset = joined[joined["Config"] == cfg].copy()
        if subset.empty:
            continue
        reclassified = subset.loc[subset["StatusLabel"] != subset["PrimaryStatusLabel"], "Shortname"].tolist()
        rows.append(
            {
                "Threshold": fmt_num(subset["Threshold"].iloc[0], 2, trim=True),
                "Consecutive months": fmt_int(subset["Persistence"].iloc[0]),
                "Balanced": fmt_int((subset["StatusLabel"] == "Balanced").sum()),
                "RP achieved without BP": fmt_int((subset["StatusLabel"] == "RP achieved without BP").sum()),
                "Suppressed": fmt_int((subset["StatusLabel"] == "Suppressed").sum()),
                "No deficit": fmt_int((subset["StatusLabel"] == "No deficit").sum()),
                "Diseases reclassified vs primary analysis": ", ".join(reclassified) if reclassified else "None",
            }
        )
        if reclassified:
            reclass_notes.append(f"{cfg}: {', '.join(reclassified)}")

    note = (
        "These sensitivity checks were computed from the disease-level RP/BP outputs underlying the recovery workflow. "
        + (
            "No disease changed RP/BP classification when the RP threshold was varied across 90%, 95%, and 100% with persistence requirements of 2, 3, or 4 months."
            if not reclass_notes
            else "RP/BP classifications were stable for most operational definitions; the only observed reclassifications relative to the primary 95% / 3 mo rule were: "
            + "; ".join(reclass_notes)
            + "."
        )
    )
    return make_table_block(
        "Table S5. Sensitivity of RP/BP classifications to alternative RP thresholds and persistence requirements.",
        pd.DataFrame(rows),
        note=note,
    )


def build_table_s6() -> str:
    table = read_csv("Appendix_S6_overlap_summary.csv").copy()
    note = (
        "These overlap-period validation summaries were recalculated directly from the disease-month comparison cache used to validate the weekly-to-monthly reconstruction. "
        f"The refreshed cache retains {table.loc[table['Metric'] == 'Disease-month observations', 'Value'].iloc[0]} disease-month pairs across "
        f"{table.loc[table['Metric'] == 'Overlap years retained in analytical cache', 'Value'].iloc[0]}."
    )
    return make_table_block(
        "Table S6. Summary metrics for overlap-period validation of weekly-to-monthly reconstruction.",
        table,
        note=note,
    )


def build_table_s7() -> str:
    table = read_csv("Appendix_S7_overlap_examples.csv").copy()
    for col in table.columns:
        if col == "Disease":
            continue
        digits = 4 if col == "Pearson correlation" else 2
        table[col] = table[col].map(lambda x, d=digits: fmt_num(x, d, trim=False))
    note = (
        f"High-burden diseases that materially contribute to the main analyses showed low relative reconstruction error across these {len(table)} illustrative examples, "
        "whereas some low-count series had larger percentage error because small absolute monthly differences inflate relative measures. "
        "Together with the disease-specific visual comparisons in the figures section, these summaries support the robustness of the reconstructed monthly series for the principal RP/BP and seasonal analyses."
    )
    return make_table_block(
        "Table S7. Illustrative disease-specific overlap-period reconstruction error metrics.",
        table,
        note=note,
    )


def build_table_s8() -> str:
    text = (
        "The legacy fixed-family robustness table is no longer rerun in the refreshed square-root, 5,000-path primary workflow. "
        "In the updated analysis, robustness emphasis shifts to uncertainty propagation (Table S9), interruption-date sensitivity (Table S10), and alternative model-selection aggregation rules (Table S11). "
        "Uniform exponential-smoothing and uniform seasonal autoregressive integrated moving-average refits are therefore not re-reported here."
    )
    return make_text_block(
        "Table S8. Legacy fixed-family robustness table not rerun in the refreshed square-root, 5,000-path primary workflow.",
        text,
    )


def build_table_s9() -> str:
    source = read_csv("Recovery_uncertainty_summary.csv").copy()
    table = source[[
        "Shortname",
        "Group",
        "PrimaryStatusLabel",
        "Pr_RP",
        "Pr_BP",
        "PrimaryStatusProb",
        "RP_MedianMonths",
        "RP_Q025Months",
        "RP_Q975Months",
        "BP_MedianMonths",
        "BP_Q025Months",
        "BP_Q975Months",
    ]].rename(columns={"PrimaryStatusLabel": "Primary deterministic phenotype"})

    table["Pr(RP)"] = table["Pr_RP"].map(lambda x: fmt_num(x, 3))
    table["Pr(BP)"] = table["Pr_BP"].map(lambda x: fmt_num(x, 3))
    table["Primary phenotype stability"] = table["PrimaryStatusProb"].map(lambda x: fmt_num(x, 3))
    table["RP month, median (95% interval)"] = table.apply(
        lambda row: "NA"
        if is_missing(row["RP_MedianMonths"])
        else f"{fmt_num(row['RP_MedianMonths'], 0, trim=True)} ({fmt_num(row['RP_Q025Months'], 0, trim=True)}-{fmt_num(row['RP_Q975Months'], 0, trim=True)})",
        axis=1,
    )
    table["BP month, median (95% interval)"] = table.apply(
        lambda row: "NA"
        if is_missing(row["BP_MedianMonths"])
        else f"{fmt_num(row['BP_MedianMonths'], 0, trim=True)} ({fmt_num(row['BP_Q025Months'], 0, trim=True)}-{fmt_num(row['BP_Q975Months'], 0, trim=True)})",
        axis=1,
    )
    table = table[[
        "Shortname",
        "Group",
        "Primary deterministic phenotype",
        "Pr(RP)",
        "Pr(BP)",
        "Primary phenotype stability",
        "RP month, median (95% interval)",
        "BP month, median (95% interval)",
    ]]

    sensitive = source.loc[source["PrimaryStatusProb"] < 0.80, "Shortname"].tolist()
    note = (
        "Here, primary phenotype stability denotes the probability that the deterministic median-based phenotype was retained across the simulated trajectories; "
        f"values below 0.80 were treated as uncertainty-sensitive in the revised main-text review layer. Under that pragmatic flag, {len(sensitive)} diseases were uncertainty-sensitive: {', '.join(sensitive)}."
    )
    return make_table_block(
        "Table S9. Uncertainty-aware RP/BP classification from 5000 simulated counterfactual trajectories.",
        table,
        note=note,
    )


def build_table_s10() -> str:
    xlsx = read_xlsx("New_robustness_operational_summaries.xlsx", "InterruptionSensitivity").copy()
    xlsx = xlsx[[
        "Shortname",
        "Group",
        "Status_2020_01",
        "Status_2020_03",
        "Status_2020_04",
        "Changed_vs_2020_01_for_2020_03",
        "Changed_vs_2020_01_for_2020_04",
    ]].rename(columns={
        "Status_2020_01": "2020-01 status",
        "Status_2020_03": "2020-03 status",
        "Status_2020_04": "2020-04 status",
        "Changed_vs_2020_01_for_2020_03": "Changed vs January in March analysis",
        "Changed_vs_2020_01_for_2020_04": "Changed vs January in April analysis",
    })
    xlsx["2020-01 status"] = xlsx["2020-01 status"].map(status_label)
    xlsx["2020-03 status"] = xlsx["2020-03 status"].map(status_label)
    xlsx["2020-04 status"] = xlsx["2020-04 status"].map(status_label)
    xlsx["Changed vs January in March analysis"] = xlsx["Changed vs January in March analysis"].map(fmt_bool)
    xlsx["Changed vs January in April analysis"] = xlsx["Changed vs January in April analysis"].map(fmt_bool)

    counts = read_xlsx("New_robustness_operational_summaries.xlsx", "InterruptionCounts")
    march_changes = int(counts.loc[counts["StartDate"] == "2020-03-01", "ChangedDiseasesVs2020_01"].iloc[0])
    april_changes = int(counts.loc[counts["StartDate"] == "2020-04-01", "ChangedDiseasesVs2020_01"].iloc[0])
    note = (
        f"No disease changed classification when the analytical start date was moved from January 2020 to March 2020 or April 2020 (March changes: {march_changes}; April changes: {april_changes}), "
        "supporting the use of January 2020 as a pragmatic portfolio-level interruption anchor."
    )
    return make_table_block(
        "Table S10. Sensitivity of deterministic RP/BP classification to alternative interruption dates.",
        xlsx,
        note=note,
    )


def build_table_s11() -> str:
    table = read_xlsx("New_robustness_operational_summaries.xlsx", "ModelSelectionSensitivity").copy()
    table = table[[
        "Shortname",
        "Group",
        "PrimaryBest",
        "RankBest",
        "SMAPEBest",
        "WeightedBest",
        "RankMatch",
        "SMAPEMatch",
        "WeightedMatch",
    ]].rename(columns={
        "PrimaryBest": "Primary best model",
        "RankBest": "Rank-aggregation best model",
        "SMAPEBest": "sMAPE-only best model",
        "WeightedBest": "Horizon-weighted best model",
        "RankMatch": "Match under rank aggregation",
        "SMAPEMatch": "Match under sMAPE-only",
        "WeightedMatch": "Match under horizon weighting",
    })
    for col in [
        "Primary best model",
        "Rank-aggregation best model",
        "sMAPE-only best model",
        "Horizon-weighted best model",
    ]:
        table[col] = table[col].astype(str).str.replace(r"\*\*", "", regex=True)
    for col in ["Match under rank aggregation", "Match under sMAPE-only", "Match under horizon weighting"]:
        table[col] = table[col].map(fmt_bool)

    counts = read_xlsx("New_robustness_operational_summaries.xlsx", "ModelSelectionCounts").copy()
    counts_lookup = dict(zip(counts["AlternativeRule"], counts["MatchesPrimary"]))
    note = (
        f"Across the 24 diseases, the primary selected family was also recovered for {counts_lookup.get('Rank aggregation', 0)} diseases under rank aggregation, "
        f"{counts_lookup.get('sMAPE only', 0)} diseases under sMAPE-only selection, and {counts_lookup.get('Horizon-weighted composite', 0)} diseases under the horizon-weighted composite. "
        "Most disagreements were concentrated in a small subset of diseases rather than a single model family, suggesting that the principal conclusions were not driven by one aggregation formula."
    )
    return make_table_block(
        "Table S11. Alternative model-selection rules compared with the primary equal-weight composite rule.",
        table,
        note=note,
    )


def build_table_s12() -> str:
    table = read_xlsx("New_robustness_operational_summaries.xlsx", "JointOperationalSummary").copy()
    table = table[[
        "Shortname",
        "Group",
        "PrimaryStatus",
        "shift_vs_pre",
        "shift_vs_pred",
        "SeasonalDisplacement",
        "FrameworkPriority",
    ]].rename(columns={
        "PrimaryStatus": "Primary deterministic phenotype",
        "shift_vs_pre": "Shift vs pre (months)",
        "shift_vs_pred": "Shift vs predicted (months)",
        "SeasonalDisplacement": "Seasonal displacement",
        "FrameworkPriority": "Operational priority",
    })
    table["Primary deterministic phenotype"] = table["Primary deterministic phenotype"].map(status_label)
    table["Shift vs pre (months)"] = table["Shift vs pre (months)"].map(fmt_int)
    table["Shift vs predicted (months)"] = table["Shift vs predicted (months)"].map(fmt_int)
    counts = read_xlsx("New_robustness_operational_summaries.xlsx", "DecisionUtilityCounts").copy()
    priority_lookup = dict(zip(counts["FrameworkPriority"], counts["Diseases"]))
    monthly_normalized = int((read_xlsx("New_robustness_operational_summaries.xlsx", "JointOperationalSummary")["MonthlyDashboardView"] == "Recovered on monthly incidence").sum())
    note = (
        f"This joint table clarifies the retrospective decision utility of the framework. A monthly-incidence-only interpretation would have marked {monthly_normalized} diseases as monthly-normalized, "
        f"but the integrated RP/BP-seasonality synthesis separated them into {priority_lookup.get('Low priority routine review', 0)} low-priority routine-review cases, "
        f"{priority_lookup.get('Cumulative review needed', 0)} cumulative-review cases, {priority_lookup.get('Recovered but recalibrate seasonality', 0)} recovered-but-recalibrate-seasonality cases, and "
        f"{priority_lookup.get('Recalibrate and monitor', 0)} recalibrate-and-monitor cases, while {priority_lookup.get('High priority manual review', 0)} disease remained in high-priority manual review and "
        f"{priority_lookup.get('No deficit monitoring', 0)} disease remained in no-deficit monitoring."
    )
    return make_table_block(
        "Table S12. Joint operational synthesis of recovery phenotype and seasonal displacement.",
        table,
        note=note,
    )


def build_table_s13() -> str:
    table = read_csv("Alternative_endpoint_sensitivity.csv").copy()
    table = table[[
        "Shortname",
        "Group",
        "PrimaryPhenotype",
        "Recovery_Months",
        "PI95_Months",
        "Ratio100_Months",
        "HalfDeficit_Months",
        "PI95_Achieved",
        "Ratio100_Achieved",
        "HalfDeficit_Achieved",
    ]].rename(columns={
        "PrimaryPhenotype": "Primary phenotype",
        "Recovery_Months": "Primary RP month",
        "PI95_Months": "PI95 month",
        "Ratio100_Months": "Ratio>=1 month",
        "HalfDeficit_Months": "Half-deficit month",
        "PI95_Achieved": "PI95 achieved",
        "Ratio100_Achieved": "Ratio>=1 achieved",
        "HalfDeficit_Achieved": "Half-deficit achieved",
    })
    for col in ["Primary RP month", "PI95 month", "Ratio>=1 month", "Half-deficit month"]:
        table[col] = table[col].map(fmt_int)
    for col in ["PI95 achieved", "Ratio>=1 achieved", "Half-deficit achieved"]:
        table[col] = table[col].map(fmt_bool)

    counts = read_xlsx("New_endpoint_context_usability_summaries.xlsx", "AlternativeEndpointCounts").copy()
    counts_lookup = dict(zip(counts["Metric"], counts["Value"]))
    rp_only = read_xlsx("New_endpoint_context_usability_summaries.xlsx", "RPOnlyAlternativeEndpoints").copy()
    rp_only_n = int((rp_only["PrimaryPhenotype"] == "Recovered but not balanced").sum())
    rp_only_half = int(rp_only.loc[rp_only["PrimaryPhenotype"] == "Recovered but not balanced", "HalfDeficit_Achieved"].sum())
    note = (
        f"Among the {int(counts_lookup.get('TotalDiseasesWithDeficit', 0))} diseases that entered a sustained cumulative deficit, "
        f"{int(counts_lookup.get('PI95_Achieved', 0))} re-entered the disease-specific 95% predictive interval for at least 3 months, "
        f"{int(counts_lookup.get('Ratio100_Achieved', 0))} met the sustained observed-to-expected ratio endpoint of at least 1.0, and "
        f"{int(counts_lookup.get('HalfDeficit_Achieved', 0))} halved their cumulative deficit by end follow-up. "
        f"The ratio endpoint preserved the same achieved-versus-not-achieved distinction as the primary RP definition for all {int(counts_lookup.get('PrimaryRP_Achieved', 0))} RP-achieved diseases, "
        f"whereas the half-deficit milestone was reached by {rp_only_half} of the {rp_only_n} recovered-but-not-balanced diseases."
    )
    return make_table_block(
        "Table S13. Alternative endpoint sensitivity analyses for the 24 modelled diseases. Month values are counted from January 2020, so month 0 corresponds to January 2020.",
        table,
        note=note,
    )


def build_table_s14() -> str:
    table = read_xlsx("New_endpoint_context_usability_summaries.xlsx", "ContextPeriods").copy()
    table = table[[
        "Period",
        "Mean_Portfolio_Ratio",
        "Median_Portfolio_Ratio",
        "Mean_Stringency",
        "Mean_SchoolClosing",
        "Mean_InternalMovement",
        "Mean_InternationalTravel",
        "Mean_TestingPolicy",
        "Mean_WHO_COVID_Cases",
    ]]
    for col in table.columns[1:]:
        table[col] = table[col].map(lambda x: fmt_num(x, 3))
    note = (
        "The portfolio-level observed-to-expected ratio rose across the restriction-intensive, transition, and post-PHSM periods. "
        "Policy indicators were available through December 2022, whereas WHO COVID-19 burden was available through June 2024."
    )
    return make_table_block(
        "Table S14. External contextual triangulation period summary.",
        table,
        note=note,
    )


def build_table_s15() -> str:
    cor = read_xlsx("New_endpoint_context_usability_summaries.xlsx", "ContextCorrelations").copy()
    cor["SpearmanRho"] = cor["SpearmanRho"].map(lambda x: fmt_num(x, 3))
    mil = read_xlsx("New_endpoint_context_usability_summaries.xlsx", "ContextMilestones").copy()
    mil["month"] = mil["month"].map(fmt_date)
    for col in ["PortfolioRatio", "StringencyIndex", "SchoolClosing", "InternalMovement", "InternationalTravel"]:
        mil[col] = mil[col].map(lambda x: fmt_num(x, 3))
    mil["WHO_COVID_Cases"] = mil["WHO_COVID_Cases"].map(lambda x: fmt_num(x, 0, trim=True))

    content = [
        "Panel A. Monthly correlation between the portfolio observed-to-expected ratio and external indicators.",
        "",
        md_table(cor),
        "",
        "Panel B. Selected milestone months from the contextual triangulation.",
        "",
        md_table(mil),
    ]
    note = (
        "These contextual summaries were used descriptively to anchor the timing of portfolio suppression and normalization. "
        "They were not used as predictive covariates and do not support causal attribution."
    )
    return make_block(
        "Table S15. External contextual triangulation correlations and milestone dates.",
        content,
        note=note,
    )


def build_table_s16() -> str:
    tasks = read_xlsx("New_endpoint_context_usability_summaries.xlsx", "HeuristicTasks").copy()
    table = tasks[[
        "TaskID",
        "PublicHealthTask",
        "PrimaryModule",
        "MinimumInteractions",
        "Discoverability",
        "Interpretability",
        "Auditability",
        "SupportStatus",
        "ResidualFriction",
        "MeanHeuristicScore",
    ]]
    table["MinimumInteractions"] = table["MinimumInteractions"].map(fmt_int)
    table["Discoverability"] = table["Discoverability"].map(fmt_int)
    table["Interpretability"] = table["Interpretability"].map(fmt_int)
    table["Auditability"] = table["Auditability"].map(fmt_int)
    table["MeanHeuristicScore"] = table["MeanHeuristicScore"].map(lambda x: fmt_num(x, 2))

    summary = read_xlsx("New_endpoint_context_usability_summaries.xlsx", "HeuristicSummary").copy()
    summary_lookup = dict(zip(summary["Metric"], summary["Value"]))
    tasks_assessed = int(round(float(summary_lookup.get("TasksAssessed", 0))))
    mean_score = fmt_num(pd.to_numeric(table["MeanHeuristicScore"], errors="coerce").mean(), 2)
    median_interactions = fmt_int(pd.to_numeric(table["MinimumInteractions"], errors="coerce").median())
    note = (
        f"All {tasks_assessed} prespecified surveillance-review tasks were directly supported in the final build, "
        f"with a mean heuristic score of {mean_score} and median minimum interaction count of {median_interactions}. "
        "This assessment documents functional interface coverage but should not be interpreted as a substitute for prospective end-user usability testing."
    )
    return make_table_block(
        "Table S16. Task-based heuristic assessment of the final dashboard build.",
        table,
        note=note,
    )


def build_table_s17() -> str:
    raw = read_csv("External_pertussis_country_median_pi_summary.csv").copy()
    table = raw[[
        "Country",
        "CadenceLabel",
        "BestModel",
        "MedianForecastAcrossFollowUp",
        "MedianLower95AcrossFollowUp",
        "MedianUpper95AcrossFollowUp",
        "FollowUpEnd",
        "ForecastMedianAtEnd",
        "Lower95AtEnd",
        "Upper95AtEnd",
        "NormalizationDate",
        "BalanceDate",
    ]].rename(columns={
        "CadenceLabel": "Cadence",
        "BestModel": "Best model",
        "MedianForecastAcrossFollowUp": "Follow-up median forecast",
        "MedianLower95AcrossFollowUp": "Follow-up lower 95% PI",
        "MedianUpper95AcrossFollowUp": "Follow-up upper 95% PI",
        "FollowUpEnd": "End date",
        "ForecastMedianAtEnd": "End forecast median",
        "Lower95AtEnd": "End lower 95% PI",
        "Upper95AtEnd": "End upper 95% PI",
        "NormalizationDate": "Normalization date",
        "BalanceDate": "Balance date",
    })

    for col in [
        "Follow-up median forecast",
        "Follow-up lower 95% PI",
        "Follow-up upper 95% PI",
        "End forecast median",
        "End lower 95% PI",
        "End upper 95% PI",
    ]:
        table[col] = table[col].map(lambda x: fmt_num(x, 1, trim=True))
    table["End date"] = table["End date"].map(fmt_date)
    table["Normalization date"] = table["Normalization date"].map(fmt_date)
    table["Balance date"] = table["Balance date"].map(fmt_date_or_not_reached)
    table["Follow-up median 95% PI"] = table.apply(
        lambda row: f"{row['Follow-up lower 95% PI']} to {row['Follow-up upper 95% PI']}",
        axis=1,
    )
    table["End 95% PI"] = table.apply(
        lambda row: f"{row['End lower 95% PI']} to {row['End upper 95% PI']}",
        axis=1,
    )
    table = table[[
        "Country",
        "Cadence",
        "Best model",
        "Follow-up median forecast",
        "Follow-up median 95% PI",
        "End date",
        "End forecast median",
        "End 95% PI",
        "Normalization date",
        "Balance date",
    ]]

    balanced = int((raw["Status"] == "Balanced").sum())
    no_bp = int((raw["Status"] == "RP achieved without BP").sum())
    note = (
        "This case study is a transportability demonstration rather than validation of the Thailand thresholds or a substitute for end-user testing in Thailand. "
        f"Across these six external series, {balanced} reached both normalization and balance within follow-up, whereas {no_bp} reached RP without BP by the end of follow-up."
    )
    return make_table_block(
        "Table S17. Country-level counterfactual median and 95% predictive-interval summary for the external pertussis case study.",
        table,
        note=note,
    )


def build_table_s18() -> str:
    raw = read_csv("Temporal_utility_freeze_summary.csv").copy()
    table = raw[[
        "FreezeDate",
        "ValidationEnd",
        "LaterReviewDiseases",
        "FrameworkCaptured",
        "IncidenceOnlyCaptured",
        "AvertedUnderTriageDiseases",
        "FrameworkAccuracy",
        "IncidenceOnlyAccuracy",
    ]].rename(columns={
        "FreezeDate": "Freeze point",
        "ValidationEnd": "Validation end",
        "LaterReviewDiseases": "Later review diseases",
        "FrameworkCaptured": "Framework captured",
        "IncidenceOnlyCaptured": "Incidence-only captured",
        "AvertedUnderTriageDiseases": "Averted under-triage",
        "FrameworkAccuracy": "Framework accuracy",
        "IncidenceOnlyAccuracy": "Incidence-only accuracy",
    })
    table["Freeze point"] = table["Freeze point"].map(fmt_date)
    table["Validation end"] = table["Validation end"].map(fmt_date)
    for col in [
        "Later review diseases",
        "Framework captured",
        "Incidence-only captured",
        "Averted under-triage",
    ]:
        table[col] = table[col].map(fmt_int)
    for col in ["Framework accuracy", "Incidence-only accuracy"]:
        table[col] = table[col].map(lambda x: fmt_num(x, 3))

    fp_dec_2023 = int(raw.loc[raw["FreezeDate"] == "2023-12-01", "FrameworkFalsePositives"].iloc[0])
    fp_jun_2024 = int(raw.loc[raw["FreezeDate"] == "2024-06-01", "FrameworkFalsePositives"].iloc[0])
    note = (
        "The framework captured more later-review diseases than the incidence-only queue at both freeze points, but not perfectly. "
        f"It generated {fp_dec_2023} false-positive review assignment at the December 2023 freeze and {fp_jun_2024} at the June 2024 freeze."
    )
    return make_table_block(
        "Table S18. Freeze-point temporal utility validation summary.",
        table,
        note=note,
    )


def build_table_s19() -> str:
    table = read_csv("Placebo_interruption_portfolio_summary.csv").copy()
    table = table[[
        "split_label",
        "DiseasesAssessed",
        "FalseAlerts",
        "FalseAlertRate",
        "FalseAlertsTempered",
        "FalseAlertRateTempered",
        "MeanCoverage80",
        "MeanCoverage95",
        "MeanIntervalScore95",
        "MeanWIS",
    ]].rename(columns={
        "split_label": "Pseudo interruption",
        "DiseasesAssessed": "Diseases assessed",
        "FalseAlerts": "False alerts",
        "FalseAlertRate": "False-alert rate",
        "FalseAlertsTempered": "Tempered false alerts",
        "FalseAlertRateTempered": "Tempered false-alert rate",
        "MeanCoverage80": "Mean 80% coverage",
        "MeanCoverage95": "Mean 95% coverage",
        "MeanIntervalScore95": "Mean 95% interval score",
        "MeanWIS": "Mean WIS",
    })
    for col in ["False-alert rate", "Tempered false-alert rate", "Mean 80% coverage", "Mean 95% coverage"]:
        table[col] = table[col].map(lambda x: fmt_num(x, 3))
    table["Mean 95% interval score"] = table["Mean 95% interval score"].map(lambda x: fmt_num(x, 2))
    table["Mean WIS"] = table["Mean WIS"].map(lambda x: fmt_num(x, 3))
    for col in ["Diseases assessed", "False alerts", "Tempered false alerts"]:
        table[col] = table[col].map(fmt_int)
    note = (
        "The tempered rule reduced false alerts relative to the deterministic rule in each placebo window, while calibration remained moderate rather than perfect."
    )
    return make_table_block(
        "Table S19. Portfolio-level placebo interruption and predictive-distribution calibration summary.",
        table,
        note=note,
    )


def build_table_s20() -> str:
    table = read_csv("Transform_rate_sensitivity_summary.csv").copy()
    table = table[[
        "Config",
        "DiseasesAssessed",
        "PhenotypeChanged",
        "StatusChanged",
        "MaterialTimingShift",
        "MedianAbsRPShift",
        "MedianAbsBPShift",
        "MaxAbsRPShift",
        "MaxAbsBPShift",
        "MeanDeltaPrRP",
        "MeanDeltaPrBP",
    ]]
    for col in [
        "DiseasesAssessed",
        "PhenotypeChanged",
        "StatusChanged",
        "MaterialTimingShift",
        "MedianAbsRPShift",
        "MedianAbsBPShift",
        "MaxAbsRPShift",
        "MaxAbsBPShift",
    ]:
        table[col] = table[col].map(fmt_int)
    for col in ["MeanDeltaPrRP", "MeanDeltaPrBP"]:
        table[col] = table[col].map(lambda x: fmt_num(x, 3))
    note = "These summaries compare the primary square-root count analysis with log-count and square-root rate variants."
    return make_table_block(
        "Table S20. Portfolio-level transform and denominator sensitivity summary.",
        table,
        note=note,
    )


def build_table_s21() -> str:
    table = read_csv("Transform_rate_sensitivity_comparison.csv").copy()
    changed = table[
        table["PhenotypeChanged"].astype(str).str.upper().eq("TRUE")
        | table["MaterialTimingShift"].astype(str).str.upper().eq("TRUE")
    ].copy()
    changed = changed[[
        "Shortname",
        "Config",
        "BaselinePhenotype",
        "PrimaryPhenotype",
        "RP_Month_Delta",
        "BP_Month_Delta",
        "Delta_Pr_RP",
        "Delta_Pr_BP",
        "Delta_StatusProb",
    ]].rename(columns={
        "BaselinePhenotype": "Baseline phenotype",
        "PrimaryPhenotype": "Sensitivity phenotype",
        "RP_Month_Delta": "RP month delta",
        "BP_Month_Delta": "BP month delta",
        "Delta_Pr_RP": "Delta Pr(RP)",
        "Delta_Pr_BP": "Delta Pr(BP)",
        "Delta_StatusProb": "Delta primary-status probability",
    })
    for col in ["RP month delta", "BP month delta"]:
        changed[col] = changed[col].map(fmt_int)
    for col in ["Delta Pr(RP)", "Delta Pr(BP)", "Delta primary-status probability"]:
        changed[col] = changed[col].map(lambda x: fmt_num(x, 3))
    note = "Only diseases with phenotype changes or material timing shifts are listed here; unchanged diseases remained stable across the sensitivity reruns."
    return make_table_block(
        "Table S21. Diseases with phenotype changes or material timing shifts in transform and denominator sensitivity analyses.",
        changed,
        note=note,
    )


def build_table_s22() -> str:
    table = read_csv("Seasonal_shift_bootstrap_summary.csv").copy()
    table = table[[
        "Shortname",
        "PointShift_vs_Pre",
        "CI025_vs_Pre",
        "CI975_vs_Pre",
        "PrAbsShiftGE2_vs_Pre",
        "PointShift_vs_Pred",
        "CI025_vs_Pred",
        "CI975_vs_Pred",
        "PrAbsShiftGE2_vs_Pred",
        "BorderlineShift",
        "COM_Max_Agree",
    ]].rename(columns={
        "PointShift_vs_Pre": "Point shift vs pre",
        "CI025_vs_Pre": "_ci_pre_lo",
        "CI975_vs_Pre": "_ci_pre_hi",
        "PrAbsShiftGE2_vs_Pre": "Pr(|shift|>=2) vs pre",
        "PointShift_vs_Pred": "Point shift vs pred",
        "CI025_vs_Pred": "_ci_pred_lo",
        "CI975_vs_Pred": "_ci_pred_hi",
        "PrAbsShiftGE2_vs_Pred": "Pr(|shift|>=2) vs pred",
        "BorderlineShift": "Borderline",
        "COM_Max_Agree": "COM/max agree",
    })
    table["Point shift vs pre"] = table["Point shift vs pre"].map(fmt_int)
    table["Point shift vs pred"] = table["Point shift vs pred"].map(fmt_int)
    table["Pr(|shift|>=2) vs pre"] = table["Pr(|shift|>=2) vs pre"].map(lambda x: fmt_num(x, 3))
    table["Pr(|shift|>=2) vs pred"] = table["Pr(|shift|>=2) vs pred"].map(lambda x: fmt_num(x, 3))
    table["95% CI vs pre"] = table.apply(
        lambda row: f"{fmt_num(row['_ci_pre_lo'], 2)} to {fmt_num(row['_ci_pre_hi'], 2)}",
        axis=1,
    )
    table["95% CI vs pred"] = table.apply(
        lambda row: f"{fmt_num(row['_ci_pred_lo'], 2)} to {fmt_num(row['_ci_pred_hi'], 2)}",
        axis=1,
    )
    table["Borderline"] = table["Borderline"].map(fmt_bool)
    table["COM/max agree"] = table["COM/max agree"].map(fmt_bool)
    table = table[[
        "Shortname",
        "Point shift vs pre",
        "95% CI vs pre",
        "Pr(|shift|>=2) vs pre",
        "Point shift vs pred",
        "95% CI vs pred",
        "Pr(|shift|>=2) vs pred",
        "Borderline",
        "COM/max agree",
    ]]
    note = "Bootstrap uncertainty is reported for the center-of-mass seasonal shift metric under both pre-pandemic and counterfactual references."
    return make_table_block(
        "Table S22. Bootstrap uncertainty for center-of-mass seasonal shift estimates.",
        table,
        note=note,
    )


def build_table_s23() -> str:
    table = read_csv("Seasonal_shift_reconstruction_sensitivity.csv").copy()
    queue_changes = table[table["QueueChanged"].astype(str).str.upper().eq("TRUE")]
    shift_changes = table[table["ShiftFlagChanged"].astype(str).str.upper().eq("TRUE")]
    pre_diff = ~(table["CurrentShiftPre"].eq(table["AltShiftPre"]) | (table["CurrentShiftPre"].isna() & table["AltShiftPre"].isna()))
    pred_diff = ~(table["CurrentShiftPred"].eq(table["AltShiftPred"]) | (table["CurrentShiftPred"].isna() & table["AltShiftPred"].isna()))
    point_changes = table[
        pre_diff | pred_diff
    ].copy()
    point_change_text = "0" if point_changes.empty else f"{len(point_changes)} ({', '.join(point_changes['Shortname'].tolist())})"
    summary = pd.DataFrame([
        {"Metric": "Diseases assessed", "Value": fmt_int(len(table))},
        {"Metric": "Queue changes detected", "Value": fmt_int(len(queue_changes))},
        {"Metric": "Shift flag changes detected", "Value": fmt_int(len(shift_changes))},
        {"Metric": "Diseases with point-estimate changes only", "Value": point_change_text},
    ])
    note = (
        "Alternative weekly-to-monthly reconstruction did not change any queue assignment or shift flag. "
        "Chancroid was the only disease with a one-month point-estimate change relative to both references, and that change was insufficient to alter the queue."
    )
    return make_table_block(
        "Table S23. Queue changes under alternative weekly-to-monthly reconstruction.",
        summary,
        note=note,
    )


def build_table_s24() -> str:
    table = read_csv("BP_segmented_comparator.csv").copy()
    table = table[[
        "Shortname",
        "PrimaryBPMonth",
        "SegmentedBPMonth",
        "BPMonthDelta",
        "AgreementLabel",
    ]].rename(columns={
        "PrimaryBPMonth": "Primary BP month",
        "SegmentedBPMonth": "Segmented BP month",
        "BPMonthDelta": "Month delta",
        "AgreementLabel": "Agreement",
    })
    table["Primary BP month"] = table["Primary BP month"].map(fmt_int)
    table["Segmented BP month"] = table["Segmented BP month"].map(fmt_int)
    table["Month delta"] = table["Month delta"].map(fmt_int)
    note = "The segmented comparator agreed with the primary BP call within 6 months for 12 diseases, left 10 unresolved, and differed materially for 2 diseases."
    return make_table_block(
        "Table S24. Agreement between the primary BP rule and the segmented cumulative-deviation comparator.",
        table,
        note=note,
    )


def build_tables_section() -> str:
    template = read_text(TEMPLATES_DIR / "tables.md")
    validate_tables_template(template)

    replacements: dict[str, str] = {}

    single_table_builders = {
        "TABLE_S1_BODY": build_table_s1,
        "TABLE_S2_BODY": build_table_s2,
        "TABLE_S3_BODY": build_table_s3,
        "TABLE_S4_BODY": build_table_s4,
        "TABLE_S5_BODY": build_table_s5,
        "TABLE_S6_BODY": build_table_s6,
        "TABLE_S7_BODY": build_table_s7,
        "TABLE_S9_BODY": build_table_s9,
        "TABLE_S10_BODY": build_table_s10,
        "TABLE_S11_BODY": build_table_s11,
        "TABLE_S12_BODY": build_table_s12,
        "TABLE_S13_BODY": build_table_s13,
        "TABLE_S14_BODY": build_table_s14,
        "TABLE_S16_BODY": build_table_s16,
        "TABLE_S17_BODY": build_table_s17,
        "TABLE_S18_BODY": build_table_s18,
        "TABLE_S19_BODY": build_table_s19,
        "TABLE_S20_BODY": build_table_s20,
        "TABLE_S21_BODY": build_table_s21,
        "TABLE_S22_BODY": build_table_s22,
        "TABLE_S23_BODY": build_table_s23,
        "TABLE_S24_BODY": build_table_s24,
    }

    for placeholder, builder in single_table_builders.items():
        tables = extract_markdown_tables(builder())
        if len(tables) != 1:
            raise RuntimeError(f"{placeholder} expected exactly one markdown table, found {len(tables)}")
        replacements[placeholder] = tables[0]

    s15_tables = extract_markdown_tables(build_table_s15())
    if len(s15_tables) != 2:
        raise RuntimeError(f"TABLE_S15 expected two markdown tables, found {len(s15_tables)}")
    replacements["TABLE_S15_PANEL_A_BODY"] = s15_tables[0]
    replacements["TABLE_S15_PANEL_B_BODY"] = s15_tables[1]

    ensure_template_has_placeholders("tables.md", template, list(replacements))
    return render_template(template, replacements)


def build_part1_figures() -> str:
    case_entries = [
        {
            "title": disease,
            "path": f"Supplementary Appendix 1_1/cases/{disease}.png",
            "caption": f"Comparison of observed and reconstructed (A) weekly and (B) monthly cases of {disease}, 2020-2025.",
        }
        for disease in ORDERED_DISEASES
    ]
    death_entries = [
        {
            "title": disease,
            "path": f"Supplementary Appendix 1_1/deaths/{disease}.png",
            "caption": f"Comparison of observed and reconstructed monthly deaths of {disease}, 2020-2025.",
        }
        for disease in ORDERED_DISEASES
    ]
    cases_block, _ = render_figure_sequence(case_entries, 1, group_size=2)
    deaths_block, _ = render_figure_sequence(death_entries, 44, group_size=2)
    return join_blocks([cases_block, deaths_block])


def build_part2_figures() -> str:
    entries = [
        {
            "title": disease,
            "path": f"Supplementary Appendix 1_5/{disease}.png",
            "caption": (
                f"Model selection and cross-validation performance for {disease}: multi-split forecasts and model comparison. "
                "Panels show the seven candidate forecasting families, spanning neural-network, exponential-smoothing, seasonal autoregressive integrated moving-average, trigonometric state-space, ensemble, Bayesian structural, and Fourier-harmonic regression specifications, together with split-specific forecast-accuracy comparison tables."
            ),
        }
        for disease in MODELLED_DISEASES
    ]
    block, _ = render_figure_sequence(entries, 87, group_size=2)
    return block


def build_part3_figures() -> str:
    entries = [
        {
            "title": "Biennial rankings of infectious diseases",
            "path": "Supplementary Appendix 1_3/ranking_all.png",
            "caption": "Cases and deaths within each time window (2008-2009 to 2024-2025). Nodes are coloured by disease category, and connecting arrows indicate changes in rank between consecutive windows (increase, decrease, or unchanged).",
        },
        {
            "title": "Shifts in age-specific disease patterns in Thailand",
            "path": "Supplementary Appendix 1_3/age_patterns_main.png",
            "caption": "2008-2025. (A) Biennial rankings of the top 10 diseases by reported cases within each time window (2008-2009 to 2024-2025). Nodes are coloured by disease category, and connecting arrows indicate changes in rank between consecutive windows (increase, decrease, or unchanged); diseases outside the top 10 are grouped as 'Others'. (B) Cumulative reported cases by age group for selected leading diseases across 2008-2025. (C) Dominant disease by cases for each age group and biennial window. (D) Biennial rankings of the top 10 diseases by reported deaths, displayed as in panel A. (E) Cumulative reported deaths by age group for selected leading diseases across 2008-2025 (inset shows expanded scale for younger age groups). (F) Dominant disease by deaths for each age group and biennial window. Disease categories are indicated by colour: respiratory, vector-borne and zoonotic, gastrointestinal, sexually transmitted, and other infectious diseases.",
        },
        {
            "title": "Validation of age-reconstruction",
            "path": "Supplementary Appendix 1_3/validation_age.png",
            "caption": "Agreement between estimated and observed age-stratified counts (2020-2023). This figure evaluates the performance of the age-reconstruction procedure by comparing harmonized estimates derived from historical grouped age reports with independently observed fine-scale age-stratified data over the 2020-2023 overlap period. Each panel presents a scatterplot of aggregated counts per disease-year-age group, one panel for cases and one for deaths; points are colored by seven target age bands, and a dashed 1:1 line indicates perfect agreement.",
        },
    ]
    block, _ = render_figure_sequence(entries, 111, group_size=2)
    return block


def build_part4_figures() -> str:
    specs = [
        (114, "Cases", "Gastrointestinal IDs"),
        (115, "Cases", "Other IDs"),
        (116, "Cases", "Respiratory IDs"),
        (117, "Cases", "Sexually IDs"),
        (118, "Cases", "Vector-borne and zoonotic IDs"),
        (119, "Deaths", "Gastrointestinal IDs"),
        (120, "Deaths", "Other IDs"),
        (121, "Deaths", "Respiratory IDs"),
        (122, "Deaths", "Sexually IDs"),
        (123, "Deaths", "Vector-borne and zoonotic IDs"),
    ]
    blocks: list[str] = []
    for number, kind, category in specs:
        label = PART4_CATEGORY_LABELS[category]
        if kind == "Cases":
            title = f"Temporal trends of {label} by category"
            caption = (
                "(A) Monthly observed case counts together with a smoothed long-term trend derived from decomposition of the monthly time series. "
                "(B) Heatmap of standardized incidence values (each disease scaled relative to its own historical distribution). "
                "The trajectory plots and standardized heatmaps are combined to highlight long-term trend behavior and temporal clustering of anomalies across disease groups."
            )
        else:
            title = f"Temporal trends of deaths from {label} by category"
            caption = (
                "(A) Monthly observed death counts together with a smoothed long-term trend derived from decomposition of the monthly time series. "
                "(B) Heatmap of standardized mortality values (each disease scaled relative to its own historical distribution). "
                "The trajectory plots and standardized heatmaps are combined to highlight long-term trend behavior and temporal clustering of anomalies across disease groups."
            )
        blocks.append(
            render_single_figure(
                number,
                title,
                f"Supplementary Appendix 1_2/{kind} {category}.png",
                caption,
            )
        )
    return join_blocks(blocks)


def build_part5_figures() -> str:
    blocks = [
        render_single_figure(
            124,
            "Leading disease by province-year for incidence",
            "Supplementary Appendix 1_4/incidence.png",
            "Panels summarize the infectious disease with the highest annual incidence in each Thai province for each calendar year included in the provincial dataset. Colors denote the dominant disease category assignment used in the spatial workflow, allowing visual comparison of how the leading incidence burden shifted across provinces and over time.",
        ),
        render_single_figure(
            125,
            "Leading disease by province-year for mortality",
            "Supplementary Appendix 1_4/mortality.png",
            "Panels summarize the infectious disease with the highest annual mortality in each Thai province for each calendar year included in the provincial dataset. Provinces with no recorded deaths in a given year are retained as a separate map class, so the figure distinguishes true zero-mortality settings from changes in the dominant fatal disease elsewhere.",
        ),
    ]
    return join_blocks(blocks)


def build_part6_figures() -> str:
    return render_single_figure(
        126,
        "External pertussis decision-support case study across six countries",
        "Supplementary Appendix 1_6/external_pertussis_decision_support.png",
        "Panel A compares candidate model performance using the same rolling hold-out composite-selection logic used in the main manuscript. Panels B-G show observed pertussis incidence and the selected counterfactual median forecast for Australia, China, Japan, New Zealand, Sweden, and the United States. Shaded blue and gold bars mark the recovery-review and balance-review windows implied by the RP/BP logic, while green/red fills mark months or weeks in which observed incidence is below or above the counterfactual median.",
    )


def build_part7_figures() -> str:
    return render_single_figure(
        127,
        "Temporal utility validation across two decision freeze points",
        "Supplementary Appendix 1_7/temporal_utility_validation.png",
        "Panel A shows the framework queue assigned to each disease at each freeze point, using the same prioritization logic as the main manuscript. Panel B compares how often the framework queue versus an incidence-only queue captured diseases that later required continued review, together with overall decision accuracy across the follow-up window.",
    )


def build_part8_figures() -> str:
    blocks = [
        render_single_figure(
            128,
            "Placebo interruption falsification and predictive-distribution calibration",
            "Supplementary Appendix 1_8/falsification_and_calibration.png",
            "Panel A shows the deterministic median-rule status assigned to each disease under three pre-pandemic placebo interruption dates using the disease-specific best model. Panel B summarizes empirical 80% and 95% coverage by forecast horizon bin. Panel C shows the randomized PIT histogram pooled across placebo disease-month forecasts. Panel D compares placebo false-alert rates under the deterministic median rule and the exploratory tempered rule with lower-80% predictive-interval corroboration.",
        ),
        render_single_figure(
            129,
            "Exploratory threshold-tolerance stress test for RP classification",
            "Supplementary Appendix 1_8/threshold_tolerance.png",
            "Heatmap cells show the proportion and count of diseases reclassified relative to the primary 95% / 3-month RP rule when the RP threshold is varied from 85% to 110% and persistence is varied from 2 to 4 months. No diseases changed phenotype in 16 of the 18 threshold-persistence combinations; only mumps reclassified under the strictest 110% threshold with 3- or 4-month persistence.",
        ),
    ]
    return join_blocks(blocks)


def build_figures_section() -> str:
    template = read_text(TEMPLATES_DIR / "figures.md")
    validate_figures_template(template)

    replacements: dict[str, str] = {}

    for number, disease in enumerate(ORDERED_DISEASES, start=1):
        replacements[f"FIG_S{number}_IMAGE"] = format_figure_image(
            number,
            disease,
            f"Supplementary Appendix 1_1/cases/{disease}.png",
        )

    for number, disease in enumerate(ORDERED_DISEASES, start=44):
        replacements[f"FIG_S{number}_IMAGE"] = format_figure_image(
            number,
            disease,
            f"Supplementary Appendix 1_1/deaths/{disease}.png",
        )

    for number, disease in enumerate(MODELLED_DISEASES, start=87):
        replacements[f"FIG_S{number}_IMAGE"] = format_figure_image(
            number,
            disease,
            f"Supplementary Appendix 1_5/{disease}.png",
        )

    fixed_figures = {
        111: ("ranking_all", "Supplementary Appendix 1_3/ranking_all.png"),
        112: ("ranking_age", "Supplementary Appendix 1_3/age_patterns_main.png"),
        113: ("validation_age", "Supplementary Appendix 1_3/validation_age.png"),
        114: ("Cases Gastrointestinal IDs", "Supplementary Appendix 1_2/Cases Gastrointestinal IDs.png"),
        115: ("Cases Other IDs", "Supplementary Appendix 1_2/Cases Other IDs.png"),
        116: ("Cases Respiratory IDs", "Supplementary Appendix 1_2/Cases Respiratory IDs.png"),
        117: ("Cases Sexually IDs", "Supplementary Appendix 1_2/Cases Sexually IDs.png"),
        118: ("Cases Vector-borne and zoonotic IDs", "Supplementary Appendix 1_2/Cases Vector-borne and zoonotic IDs.png"),
        119: ("Deaths Gastrointestinal IDs", "Supplementary Appendix 1_2/Deaths Gastrointestinal IDs.png"),
        120: ("Deaths Other IDs", "Supplementary Appendix 1_2/Deaths Other IDs.png"),
        121: ("Deaths Respiratory IDs", "Supplementary Appendix 1_2/Deaths Respiratory IDs.png"),
        122: ("Deaths Sexually IDs", "Supplementary Appendix 1_2/Deaths Sexually IDs.png"),
        123: ("Deaths Vector-borne and zoonotic IDs", "Supplementary Appendix 1_2/Deaths Vector-borne and zoonotic IDs.png"),
        124: ("incidence", "Supplementary Appendix 1_4/incidence.png"),
        125: ("mortality", "Supplementary Appendix 1_4/mortality.png"),
        126: ("External pertussis decision-support case study", "Supplementary Appendix 1_6/external_pertussis_decision_support.png"),
        127: ("Temporal utility validation", "Supplementary Appendix 1_7/temporal_utility_validation.png"),
        128: ("Placebo interruption falsification and predictive-distribution calibration", "Supplementary Appendix 1_8/falsification_and_calibration.png"),
        129: ("Exploratory threshold-tolerance stress test for RP classification", "Supplementary Appendix 1_8/threshold_tolerance.png"),
    }

    for number, (label, rel_path) in fixed_figures.items():
        replacements[f"FIG_S{number}_IMAGE"] = format_figure_image(number, label, rel_path)

    ensure_template_has_placeholders("figures.md", template, list(replacements))
    return render_template(template, replacements)


def build_appendix() -> str:
    methods = read_text(TEMPLATES_DIR / "methods.md").strip()
    tables = build_tables_section().strip()
    figures = build_figures_section().strip()
    appendix = "\n\n".join([methods, tables, figures]) + "\n"
    appendix = re.sub(r"(?m)^<!--.*?-->\r?\n?", "", appendix)
    appendix = re.sub(r"\n{3,}", "\n\n", appendix)
    return appendix


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Refresh appendix source tables and rebuild the supplementary appendix."
    )
    parser.add_argument(
        "--skip-source-refresh",
        action="store_true",
        help="Use the cached source-table CSV files in Outcome/Appendix/Tables.",
    )
    parser.add_argument(
        "--rscript",
        help="Optional path to the Rscript executable used to run 9_a_generate_appendix_source_tables.R.",
    )
    parser.add_argument(
        "--output",
        help="Optional output path for the rendered supplementary appendix markdown file.",
    )
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> None:
    args = parse_args(argv)

    if args.skip_source_refresh:
        print("Using existing cached appendix source tables.")
    else:
        run_source_table_refresh(args.rscript)

    ensure_source_tables_exist()

    appendix = build_appendix()
    output_path = Path(args.output).expanduser().resolve() if args.output else OUTPUT_PATH
    write_text(output_path, appendix)
    print(f"Wrote {output_path}")


if __name__ == "__main__":
    main(sys.argv[1:])
