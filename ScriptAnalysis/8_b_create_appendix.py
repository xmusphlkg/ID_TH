from reportlab.lib.pagesizes import A4
from reportlab.platypus import SimpleDocTemplate, Paragraph, Image
from reportlab.lib.styles import getSampleStyleSheet
from reportlab.lib.units import inch, cm
from reportlab.platypus import PageBreak
import pandas as pd
from reportlab.lib import utils

# read data
data_class = pd.read_csv('../Data/DiseaseClass.csv')
fig1_data_df = pd.read_excel('../Outcome/Appendix/figure_data/fig1.xlsx', sheet_name='panel C')

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
    
    p1_length = 12

    # add figure
    img_path = f'../Outcome/Appendix/Supplementary Appendix 1_2/{disease_name}.png'
    img = utils.ImageReader(img_path)
    image = Image(img_path, width=14*47, height=15*47)
    story.append(image)

    # add figure title
    title_text = f"Supplementary Fig. {index + 1 + p1_length}. Geographic distribution of incidence and mortality of {label} in Thailand, 2007-2023"
    title = Paragraph(title_text, styles['Heading2'])
    story.append(title)

    # add page break
    story.append(PageBreak())

doc.build(story)


