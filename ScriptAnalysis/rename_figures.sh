#!/bin/bash
# ==============================================================================
# Figure Renumbering for npj Digital Medicine Submission
# ==============================================================================
# Mapping:
#   Fig 1 = Framework architecture diagram    (npjDM/fig1)
#   Fig 2 = Model selection heatmap           (npjDM/fig2)
#   Fig 3 = Recovery trajectories             (npjDM/fig3)
#   Fig 4 = Seasonal analysis                 (npjDM/fig4)
#   Fig 5 = Suppression/rebound analysis      (npjDM/fig5)
# ==============================================================================

cd ../Outcome/Publish

# Create submit directory if not exists
mkdir -p submit

echo "=== Copying figures for submission ==="

# Fig 1: Framework architecture
if [ -f npjDM/fig1.pdf ]; then
  cp npjDM/fig1.pdf submit/Figure1.pdf
  cp npjDM/fig1.png submit/Figure1.png
  echo "  Fig 1 (Framework) -> submit/Figure1.pdf"
else
  echo "  WARNING: npjDM/fig1.pdf not found. Generate Figure 1 externally using manuscript/figure1_ai_brief.md and place it in npjDM/."
fi

# Fig 2: Model selection heatmap
if [ -f npjDM/fig2.pdf ]; then
  cp npjDM/fig2.pdf submit/Figure2.pdf
  cp npjDM/fig2.png submit/Figure2.png
  echo "  Fig 2 (Model selection) -> submit/Figure2.pdf"
else
  echo "  WARNING: npjDM/fig2.pdf not found. Run fig2.R first."
fi

# Fig 3: Recovery trajectories
if [ -f npjDM/fig3.pdf ]; then
  cp npjDM/fig3.pdf submit/Figure3.pdf
  cp npjDM/fig3.png submit/Figure3.png
  echo "  Fig 3 (Recovery trajectories) -> submit/Figure3.pdf"
else
  echo "  WARNING: npjDM/fig3.pdf not found. Run fig3.R first."
fi

# Fig 4: Seasonal analysis
if [ -f npjDM/fig4.pdf ]; then
  cp npjDM/fig4.pdf submit/Figure4.pdf
  cp npjDM/fig4.png submit/Figure4.png
  echo "  Fig 4 (Seasonal analysis) -> submit/Figure4.pdf"
else
  echo "  WARNING: npjDM/fig4.pdf not found. Run fig4.R first."
fi

# Fig 5: Suppression/rebound analysis
if [ -f npjDM/fig5.pdf ]; then
  cp npjDM/fig5.pdf submit/Figure5.pdf
  cp npjDM/fig5.png submit/Figure5.png
  echo "  Fig 5 (Suppression/rebound) -> submit/Figure5.pdf"
else
  echo "  WARNING: npjDM/fig5.pdf not found. Run fig5.R first."
fi

echo ""
echo "=== Figure renumbering complete ==="
echo "Submit directory contents:"
ls -la submit/
