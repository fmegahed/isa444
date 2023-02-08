if(require(renderthis)==FALSE) remotes::install_github("jhelvy/renderthis", dependencies = TRUE)
if(require(officer) == FALSE) install.packages('officer')

renderthis::to_pdf(from = 'lectures/06_forecast_accuracy/06_forecast_accuracy.html',
                   to = 'pdfs/06_forecast_accuracy.pdf',
                   complex_slides = TRUE,
                   partial_slides = TRUE,
                   delay = 2)

# I am using my adobe to create the ppt since the text is editable (which is more preferable to students)
renderthis::to_pptx(from = 'lectures/06_forecast_accuracy/06_forecast_accuracy.html',
                    to = 'ppts/06_forecast_accuracy.pptx',
                    complex_slides = TRUE,
                    partial_slides = TRUE,
                    delay = 2)
