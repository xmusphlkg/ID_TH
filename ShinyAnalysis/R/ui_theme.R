library(bslib)
library(htmltools)

#####################################
## @Description: ShinyAnalysis theme — upload-first companion app to ShinyDashboard
## @Author: Li Kangguo
#####################################
dashboard_theme <- bs_theme(
  version = 5,
  bg = "#F5EFE6",
  fg = "#22313F",
  primary = "#0A6762",
  secondary = "#C96A43",
  success = "#0B5F59",
  warning = "#D89A2B",
  danger = "#BE4C3A",
  base_font = font_collection(font_google("IBM Plex Sans"), "Segoe UI", "sans-serif"),
  heading_font = font_collection(font_google("Fraunces"), "Georgia", "serif")
)

app_header <- tags$head(
  tags$style(HTML("
    :root {
      --sand: #f5efe6;
      --paper: rgba(255, 252, 247, 0.92);
      --ink: #22313f;
      --teal: #0a6762;
      --coral: #c96a43;
      --gold: #d89a2b;
      --line: rgba(34, 49, 63, 0.12);
      --muted: #62707b;
    }
    body {
      background: radial-gradient(circle at top right, rgba(216,154,43,0.18), transparent 26%),
                  linear-gradient(180deg, #f3ece2 0%, #f8f3ec 44%, #f1ebe1 100%);
    }
    .navbar {
      backdrop-filter: blur(8px);
      background: rgba(245, 239, 230, 0.9) !important;
      border-bottom: 1px solid var(--line);
    }
    .navbar-nav .nav-link {
      border-radius: 999px;
      margin: 0 0.15rem;
      padding: 0.45rem 0.9rem;
      color: #41505c;
    }
    .navbar-nav .nav-link.active {
      background: rgba(10,103,98,0.10);
      color: var(--teal);
      font-weight: 600;
    }
    .app-title-wrap { display: flex; flex-direction: column; gap: 0.05rem; }
    .app-kicker {
      font-size: 0.72rem; letter-spacing: 0.12em;
      text-transform: uppercase; color: #5f6d79;
    }
    .app-title { font-size: 1.1rem; font-weight: 700; }
    .bslib-page-fill { gap: 1.1rem; }

    /* Cards */
    .card {
      border: 1px solid rgba(34,49,63,0.08);
      box-shadow: 0 12px 24px rgba(34,49,63,0.05);
      border-radius: 1rem;
    }
    .card-header {
      font-weight: 600; letter-spacing: 0.01em;
      background: rgba(255, 252, 247, 0.72);
      border-bottom: 1px solid rgba(34,49,63,0.08);
    }
    .sidebar {
      background: rgba(255, 252, 247, 0.72);
      border-right: 1px solid rgba(34,49,63,0.08);
      backdrop-filter: blur(6px);
    }

    /* Hero card */
    .hero-card {
      background: linear-gradient(135deg, rgba(10,103,98,0.98), rgba(104,126,64,0.92));
      color: white; border: none;
      box-shadow: 0 22px 50px rgba(34, 49, 63, 0.14);
    }
    .hero-card .card-body { padding: 1.5rem 1.5rem 1.35rem 1.5rem; }
    .hero-card h2, .hero-card p, .hero-card div {
      position: relative; z-index: 1; color: rgba(255,255,255,0.96);
    }
    .caption-chip {
      display: inline-block; padding: 0.25rem 0.6rem; border-radius: 999px;
      background: rgba(255,255,255,0.14); border: 1px solid rgba(255,255,255,0.18);
      color: rgba(255,255,255,0.95); font-size: 0.84rem; margin-bottom: 0.5rem;
    }
    .hero-lead {
      max-width: 56rem; font-size: 1rem; line-height: 1.7;
      color: rgba(255,255,255,0.9); margin-bottom: 0.9rem;
    }
    .hero-strip { display: flex; flex-wrap: wrap; gap: 0.55rem; margin-top: 0.25rem; }
    .hero-pill {
      padding: 0.42rem 0.8rem; border-radius: 999px;
      background: rgba(255,255,255,0.14); border: 1px solid rgba(255,255,255,0.18);
      font-size: 0.83rem; letter-spacing: 0.01em;
    }

    /* KPI boxes */
    .value-box { border-radius: 1rem; box-shadow: 0 10px 24px rgba(34,49,63,0.06); }
    .kpi-box {
      overflow: hidden; position: relative;
      border: 1px solid rgba(34,49,63,0.06);
      box-shadow: 0 16px 30px rgba(34,49,63,0.08);
    }
    .kpi-box::after {
      content: ''; position: absolute; inset: auto -18% -35% auto;
      width: 130px; height: 130px; border-radius: 999px;
      background: rgba(255,255,255,0.14); pointer-events: none;
    }
    .kpi-box .value-box-title {
      text-transform: uppercase; letter-spacing: 0.08em;
      font-size: 0.72rem; font-weight: 700; opacity: 0.9;
    }
    .kpi-box .value-box-title,
    .kpi-box .value-box-value,
    .kpi-box .value-box-showcase,
    .kpi-box .value-box-area p,
    .kpi-box p {
      color: rgba(255,255,255,0.97) !important;
      position: relative; z-index: 1;
      text-shadow: 0 1px 1px rgba(0,0,0,0.08);
    }
    .value-box .value-box-value { font-size: 1.7rem; line-height: 1.05; }
    .kpi-box .value-box-value { margin-top: 0.2rem; margin-bottom: 0.35rem; }
    .kpi-box p { margin-bottom: 0; font-size: 0.88rem; line-height: 1.45; opacity: 0.92; }
    .kpi-emerald { background: linear-gradient(135deg, #0a6762, #2d8b81); color: white; }
    .kpi-amber   { background: linear-gradient(135deg, #b87d1f, #d8a03a); color: white; }
    .kpi-coral   { background: linear-gradient(135deg, #b85738, #d77c56); color: white; }
    .kpi-slate   { background: linear-gradient(135deg, #2b3e4a, #48616e); color: white; }

    /* Text utilities */
    .section-copy { font-size: 0.96rem; line-height: 1.65; color: #33424f; }
    .section-copy.compact { font-size: 0.9rem; line-height: 1.55; color: var(--muted); margin-bottom: 0; }
    .metric-grid-copy { color: var(--muted); font-size: 0.9rem; line-height: 1.55; margin-bottom: 0.75rem; }
    .insight-list { margin-bottom: 0; padding-left: 1.15rem; color: #33424f; }
    .insight-list li { margin-bottom: 0.45rem; }
    .mini-kicker {
      display: inline-block; margin-bottom: 0.6rem; color: var(--teal);
      font-size: 0.76rem; font-weight: 700; letter-spacing: 0.08em; text-transform: uppercase;
    }
    .control-note {
      padding: 0.8rem 0.9rem; border-radius: 0.9rem;
      background: rgba(10,103,98,0.07); border: 1px solid rgba(10,103,98,0.12);
      color: #29434f; font-size: 0.88rem; line-height: 1.55;
    }
    .control-note strong { color: var(--teal); }
    .overview-note-card {
      background: rgba(255, 252, 247, 0.82);
      border: 1px solid rgba(34,49,63,0.08);
    }
    .overview-note-card .card-body { padding: 1.25rem; }
    .mode-card {
      background: rgba(255, 252, 247, 0.9);
      border: 1px solid rgba(34,49,63,0.08);
    }
    .mode-card h3,
    .process-card h3 {
      font-family: Fraunces, Georgia, serif;
      font-size: 1.1rem;
      margin-bottom: 0.45rem;
      color: #22313f;
    }
    .process-card .card-body {
      min-height: 100%;
    }
    .workflow-step {
      width: 2rem;
      height: 2rem;
      display: inline-flex;
      align-items: center;
      justify-content: center;
      border-radius: 999px;
      background: rgba(10,103,98,0.12);
      color: var(--teal);
      font-weight: 700;
      margin-bottom: 0.8rem;
    }
    .definition-list {
      display: grid;
      grid-template-columns: minmax(0, 180px) minmax(0, 1fr);
      gap: 0.65rem 1rem; margin: 0;
    }
    .definition-list dt { color: #22313f; font-weight: 600; margin: 0; }
    .definition-list dd { color: var(--muted); margin: 0; line-height: 1.55; }
    .source-list { margin: 0; padding-left: 1.1rem; color: var(--muted); }
    .source-list li { margin-bottom: 0.4rem; }

    /* Analyze tab styles */
    .upload-zone {
      border: 2px dashed rgba(10,103,98,0.35);
      border-radius: 1rem;
      padding: 1.4rem;
      background: rgba(10,103,98,0.04);
      text-align: center;
    }
    .upload-zone:hover { border-color: var(--teal); background: rgba(10,103,98,0.08); }
    .format-table th { background: rgba(10,103,98,0.08); font-weight: 600; }
    .format-table td, .format-table th { padding: 0.45rem 0.75rem; font-size: 0.88rem; }
    .status-badge {
      display: inline-block; padding: 0.2rem 0.65rem; border-radius: 999px;
      font-size: 0.8rem; font-weight: 600;
    }
    .badge-success { background: rgba(13,93,86,0.12); color: #0D5D56; }
    .badge-warning { background: rgba(216,154,43,0.15); color: #8A6010; }
    .badge-danger  { background: rgba(190,76,58,0.12);  color: #BE4C3A; }
    .badge-info    { background: rgba(76,106,146,0.12); color: #4C6A92; }
  "))
)

app_title <- div(
  class = "app-title-wrap",
  div(class = "app-kicker", "Upload-first surveillance workflow"),
  div(class = "app-title", "ShinyAnalysis Studio")
)
