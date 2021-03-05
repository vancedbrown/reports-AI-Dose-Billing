library(sendmailR)
library(tidyverse)

source('dosebilling.R')

sendmail_options(smtpServer="mail.spgenetics.com")

from <- "vbrown@smithfield.com"
to <- c("jheath@smithfield.com",
        "jhargrove@smithfield.com",
        "vbrown@smithfield.com")
subject <- "AI Dose Billing Weekly"
msg <- c("Dose billing report attached for last week.",
         mime_part("dosebilling.csv"),
         mime_part("totaldoses.csv"))
sendmail(from,to,subject,msg)