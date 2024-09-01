from reportlab.lib.pagesizes import A4
from reportlab.platypus import SimpleDocTemplate, Paragraph, Image
from reportlab.lib.styles import getSampleStyleSheet
from reportlab.lib.units import inch, cm
from reportlab.platypus import PageBreak
import pandas as pd
from reportlab.lib import utils

# read data
data_class = pd.read_csv('../Data/DiseaseClass.csv')
fig1_data_df = pd.read_excel('../Outcome/Appendix/figure_data/fig1.xlsx', sheet_name='panel A')

# empty pdf file
pdf_filename = '../Outcome/Appendix/Supplementary Appendix 1_2.pdf'
doc = SimpleDocTemplate(pdf_filename, pagesize=A4, leftMargin=20, rightMargin=20, topMargin=15, bottomMargin=20)
story = []

# setting style
styles = getSampleStyleSheet()
styles['Title'].fontName = 'Times-Bold'
styles['Heading1'].fontName = 'Times-Bold'
styles['Heading2'].fontName = 'Times-Bold'
styles['Heading3'].fontName = 'Times-Bold'
styles['BodyText'].fontName = 'Times-Roman'
styles['Normal'].fontName = 'Times-Roman'
styles['Normal'].fontSize = 12
styles['Normal'].leading = styles['Heading1'].leading

# insert image and add TOC entries

## test for one image
index = 0
row = fig1_data_df.iloc[index]

for index, row in fig1_data_df.iterrows():
    disease_name = row['Disease']
    label = data_class[data_class['Shortname'] == disease_name]['Fullname'].values[0]

    # add figure
    img_path = f'../Outcome/Appendix/Supplementary Appendix 1_2/{disease_name}.png'
    img = utils.ImageReader(img_path)
    image = Image(img_path, width=14*40, height=15*40)
    story.append(image)

    # add figure title
    title_text = f"Supplementary Fig. {index + 37}. Training and comparing variant time series models for {label}."
    title = Paragraph(title_text, styles['Heading2'])
    story.append(title)

    # add figure content
    title_content = """<b>(A)</b> Neural network model;
    <b>(B)</b> Exponential smoothing (ETS) model;
    <b>(C)</b> Seasonal autoregressive integrated moving average (SARIMA) model;
    <b>(D)</b> TBATS model (Exponential smoothing state space model with Box-Cox transformation, ARMA errors, Trend and Seasonal components);
    <b>(E)</b> Hybrid models: combining Neural Network, ETS, SARIMA, and TBATS model;
    <b>(F)</b> Bayesian structural model; <b>(G)</b> Root mean square error (RMSE) of variant models;
    <b>(H)</b> Symmetric mean absolute percentage error (SMAPE) of variant models; <b>(I)</b> Mean absolute scaled error (MASE)
    of variant models; <b>(J)</b> R-squared of variant models."""
    title = Paragraph(title_content, styles['Normal'])
    story.append(title)

    # add page break
    story.append(PageBreak())

doc.build(story)


