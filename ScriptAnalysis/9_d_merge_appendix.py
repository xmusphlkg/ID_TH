
# merge pdf
import PyPDF2
def merge_pdf(paths, output):
    pdf_writer = PyPDF2.PdfWriter()

    for path in paths:
        pdf_reader = PyPDF2.PdfReader(path)
        for page in range(len(pdf_reader.pages)):
            pdf_writer.add_page(pdf_reader.pages[page])
            
    with open(output, 'wb') as out:
        pdf_writer.write(out)

if __name__ == '__main__':
    paths_1 = [
        "../Outcome/Appendix/Supplementary Appendix 1_1.pdf",
        '../Outcome/Appendix/Supplementary Appendix 1_2.pdf',
        '../Outcome/Appendix/Supplementary Appendix 1_3.pdf'
        ]
    output_1 = '../Outcome/Appendix/Supplementary Appendix 1.pdf'
    merge_pdf(paths_1, output_1)
