if(require(renderthis)==FALSE) remotes::install_github("jhelvy/renderthis", dependencies = TRUE)
if(require(officer) == FALSE) install.packages('officer')

renderthis::to_pdf(from = 'lectures/03_ts_viz/03_ts_viz.html',
                   to = 'pdfs/03_ts_viz.pdf',
                   complex_slides = TRUE,
                   partial_slides = TRUE,
                   delay = 2)

# I am using my adobe to create the ppt since the text is editable (which is more preferable to students)
renderthis::to_pptx(from = 'lectures/03_ts_viz/03_ts_viz.html',
                    to = 'ppts/03_ts_viz.pptx',
                    complex_slides = TRUE,
                    partial_slides = TRUE,
                    delay = 2)
