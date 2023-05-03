# Sys.setenv(PAGEDOWN_CHROME = 'C:\\Program Files (x86)\\Google\\Chrome\\Application\\chrome.exe')
# Sys.setenv(CHROMOTE_CHROME = "C:\\Program Files (x86)\\Google\\Chrome\\Application\\chrome.exe")

if(require(renderthis)==FALSE) remotes::install_github("jhelvy/renderthis", dependencies = TRUE)
if(require(officer) == FALSE) install.packages('officer')

renderthis::to_pdf(from = 'lectures/28_short_intro_ml_ts/28_short_intro_ml_ts.html',
                   to = 'pdfs/28_short_intro_ml_ts.pdf',
                   complex_slides = TRUE,
                   partial_slides = TRUE,
                   delay = 1)

# I am using my adobe to create the ppt since the text is editable (which is more preferable to students)
renderthis::to_pptx(from = 'lectures/28_short_intro_ml_ts/28_short_intro_ml_ts.html',
                    to = 'ppts/28_short_intro_ml_ts.pptx',
                    complex_slides = TRUE,
                    partial_slides = TRUE,
                    delay = 1)
