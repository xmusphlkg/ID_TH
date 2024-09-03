from reportlab.lib.pagesizes import A4
from reportlab.platypus import SimpleDocTemplate, Paragraph, Image
from reportlab.lib.styles import getSampleStyleSheet
from reportlab.lib.units import inch, cm
from reportlab.platypus import PageBreak
import pandas as pd
from reportlab.lib import utils

# read data
fig1_data_df = pd.read_excel('../Outcome/Appendix/figure_data/fig1.xlsx', sheet_name='panel A')

# the unique value in the 'Gruop' column of fig1_data_df
value_group = fig1_data_df['Group'].unique()
df_group = pd.DataFrame(value_group, columns=['Group'])

# empty pdf file
pdf_filename = '../Outcome/Appendix/Supplementary Appendix 1_1.pdf'
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
for index, row in df_group.iterrows():
    group_name = row['Group']
    
    # add figure
    img_path = f'../Outcome/Appendix/Supplementary Appendix 1_1/Cases {group_name}.png'
    img = utils.ImageReader(img_path)
    image = Image(img_path, width=14*40, height=7*40)
    story.append(image)
    
    # add figure title
    title_text = f"Supplementary Fig. {index*2 + 1}. Monthly cases and trends of {group_name} in Thailand, 2007-2023"
    title = Paragraph(title_text, styles['Heading2'])
    story.append(title)
    
    # add empty line
    story.append(Paragraph('<br/>', styles['Normal']))
    
    # add figure
    img_path = f'../Outcome/Appendix/Supplementary Appendix 1_1/Deaths {group_name}.png'
    img = utils.ImageReader(img_path)
    image = Image(img_path, width=14*40, height=7*40)
    story.append(image)
    
    # add figure title
    title_text = f"Supplementary Fig. {index*2 + 2}. Monthly deaths and trends of {group_name} in Thailand, 2007-2023"
    title = Paragraph(title_text, styles['Heading2'])
    story.append(title)
    
    # add page break
    story.append(PageBreak())

doc.build(story)


