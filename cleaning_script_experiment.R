## cleaning_script_experiment.R
## Takes in the email and postings csvs and outputs a single data frame ready for analysis

library(lubridate)
library(stringr)
library(dplyr)

## setwd("~/Desktop/Coursework/W241_Field_Experiments/w241-project/R")

############################################################################################################
## Import and clean email data
############################################################################################################

## Load in Peter's and Tim's mail
t_mail <- read.csv("../timothybxt253_emails.csv",stringsAsFactors=FALSE)[,c("X","time_sent","persona","time_received")]

## Add in rows that weren't caught in the email sending CSV
t_mail2 <- rbind(t_mail,
                 c("4977681984","Mon, 13 Apr 2015 21:04:26 -0700 (PDT)","developer","Tue, 14 Apr 2015 09:59:36 -0700 (PDT)"), #lorraine / info
                 c("4973061015","Mon, 13 Apr 2015 20:49:26 -0700 (PDT)","analyst","Tue, 14 Apr 2015 08:50:25 -0700 (PDT)"), #crestathomewood / cfeltman@druckerandfalk.com
                 c("4959243449","Mon, 13 Apr 2015 21:02:05 -0700 (PDT)","analyst","Wed, 15 Apr 2015 16:21:43 -0700 (PDT)"), #dealmaker / apts4rentsf@gmail.com
                 c("4963138415","Mon, 13 Apr 2015 20:41:21 -0700 (PDT)","analyst","Mon, 13 Apr 2015 20:43:01 -0700 (PDT)"), #billharkinsbroker@gmail.com
                 c("4977355215","Mon, 13 Apr 2015 20:53:07 -0700 (PDT)","developer","Tue, 14 Apr 2015 10:30:17 -0700 (PDT)"), # FranciscanApts@gmail
                 c("4974567033","Mon, 13 Apr 2015 20:35:12 -0700 (PDT)","analyst","Wed, 15 Apr 2015 08:17:13 -0700 (PDT)") # thewilsonsf@riverstoneres
                 )

t_mail2$treat <- 1

p_mail <- read.csv("../peterj9224_emails.csv",stringsAsFactors=FALSE)[,c("X","time_sent","persona","time_received")]                 
p_mail$treat <- 0

## Fix times reporting on Saturdays when they were really Mondays
p_mail[p_mail$X == "4977607125","time_sent"] <- "Mon, 13 Apr 2015 21:00:26 -0700 (PDT)"
p_mail[p_mail$X == "4977485142","time_sent"] <- "Mon, 13 Apr 2015 20:35:13 -0700 (PDT)"

## Combine into one file
mail <- rbind(t_mail2, p_mail)
rm(t_mail, t_mail2, p_mail)
colnames(mail)[1] <- "posting_id"

## Combine blank replies together
## Strip out blanks
blanks <- mail[mail$persona == "",]
mail <- mail[mail$persona != "",]

## Remove craigslist id from tuples, merge back in
blanks$posting_id <- substr(sapply(blanks[,"posting_id"], function (x) str_split(x,",")[[1]][1]),2,12)
mail <- merge(mail,blanks[,c('posting_id','treat','time_received')],by = c('posting_id','treat'), all.x = T)
mail$time_received <- ifelse(!is.na(mail$time_received.y),mail$time_received.y, mail$time_received.x)
mail <- mail[,!(names(mail) %in% c('time_received.x','time_received.y'))]

## Clean up the time fields 
clean_time <- function(input_time) {
        if (input_time == "") {
                return(NA)
        }
        timeparts <- strsplit(input_time," ")[[1]]
        GMT_adjustment <- as.integer(substr(timeparts[6],1,3))
        GMT_adjustment_char <- paste0(ifelse(GMT_adjustment > 0, "+",""),as.character(GMT_adjustment))
        timezone <- paste0("Etc/GMT", GMT_adjustment_char)
        ymd_hms(paste(timeparts[4],timeparts[3],timeparts[2],timeparts[5]),tz = timezone)
}

mail$time_sent_clean <- sapply(mail$time_sent,clean_time)
mail$time_received_clean <- sapply(mail$time_received, clean_time)

## Record whether there was a reply
mail$received_response <- !is.na(mail$time_received_clean)

## Calculate whether treatment or control got the email first
## Merge treatment and control times by craigslist id and compare
treat_times <- subset(mail, treat == 1)[,c("posting_id","time_sent_clean")]
colnames(treat_times)[2] <- "time_sent_clean_t"
control_times <- subset(mail, treat == 0)[,c("posting_id","time_sent_clean")]
all_times <- merge(treat_times, control_times, by="posting_id")

id_treat_first <- data.frame(posting_id = all_times$posting_id,
                             treat_first = all_times$time_sent_clean_t < all_times$time_sent_clean)
mail2 <- merge(mail, id_treat_first, by="posting_id", all = T)

## Cleanup leftover files 
rm(all_times, control_times, id_treat_first, treat_times, clean_time, mail, blanks)

############################################################################################################
## Import and clean postings data
############################################################################################################

## Get sampled postings filenames and locations
filenames <- paste0("../",list.files("..")[grepl("_sample.csv",list.files(".."))])
filenames <- c(filenames, '../bham_postings_2015-04-13.csv')
is_sf <- ifelse(substr(filenames,4,6)=="sfb",1,0)

## Loop through and append results from each day
postings <- data.frame(posting_id=numeric(), is_sf=logical(), beds=numeric(), baths=numeric(), sqft=numeric())
for (i in 1:length(filenames)){
        file <- read.csv(filenames[i])[c("posting_id","date_posted","beds","baths","sqft","email","price","npics")]
        file$is_sf <- is_sf[i]
        postings <- rbind(postings, file)
}

## Deduplicate, merge with mail dataframe, and clean up
postings_deduped <- unique(postings)
mail3 <- merge(mail2, postings_deduped, by= "posting_id", all.x = T)
rm(filenames, is_sf, i, postings, file, postings_deduped, mail2)

## Format postings variables

## Awful string parsing to get emails
## Split on %40
## In the first part, cut off "mailto:"
## In the second part, find the period and cut off everything after
## Paste together with an @
splits <- strsplit(as.character(mail3$email),"%40")
mail3$email <- sapply(splits, function(x) paste0(
        substr(x[[1]],8,100),
        "@",
        strsplit(x[2],"\\.")[[1]][1]
        ))
rm(splits)

mail3$price <- substr(as.character(mail3$price),2,10)

## Days since posting
mail3$days_since_posting <- round(difftime(as.POSIXct(mail3$time_sent_clean, origin = origin), ymd(mail3$date_posted), units="days"),0)

## Duration between sending and receiving
mail3$duration <- (mail3$time_received_clean - mail3$time_sent_clean)/(60*1.0) 

############################################################################################################
## Import and clean availability data
############################################################################################################

avail <- read.csv("availability.csv")
avail$received_response = T

mail4 <- merge(mail3, avail, by=c('treat','email','received_response'), all.x = T)
mail4 <- arrange(mail4, email, received_response, available)

############################################################################################################
## Check that everything ties
############################################################################################################

# ## Check for NAs in the main body of this one
# leftover <- merge(mail3, avail, by=c('treat','email'), all.y = T)
# 
# ## Anything got a response but not available, or no response but is available?
# missing_responses <- mail4[mail4$received_response == T & is.na(mail4$available),]
# present_responses <- mail4[mail4$received_response == F & !is.na(mail4$available),]
# 
# ## Any one to many merging happening?
# table(mail4$email)[table(mail4$email) > 1]
# table(avail$email)[table(avail$email) > 1]
# 
# ## Check the number of availables before and after the merge
# sum(as.numeric(as.character(mail4$available)),na.rm = T)
# sum(as.numeric(as.character(avail$available)),na.rm = T)
# 
# ## See if any email addresses are missing
# a_email <- avail[avail$available ==1, 'email']
# m_email <- mail4[mail4$available == 1, 'email']
# m_email <- m_email[!is.na(m_email)]
# 
# a_email[!(a_email %in% m_email)]
# m_email[!(m_email %in% a_email)]

############################################################################################################
## Output file
############################################################################################################

mail4 <- arrange(mail4, posting_id, treat)
write.csv(mail4, "mail_data_clean_experiment.csv", row.names = F)