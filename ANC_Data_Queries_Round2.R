#RMySQL is necessary for accessing a MySQL database
require(RMySQL)

#establishing connection to the MYSQL server

mydb = dbConnect(MySQL(), user='database_user', password='database_password', dbname='clinical', host='hostname')

## call the stored procedure that displays the ANC/HIV Incidence summary summary

rs = dbSendQuery(mydb, "CALL Extract_Mesh_Round2_Data_Summary()")

anc_round2_dataset = fetch(rs, n=-1)

## the command below is instrumental in clearing a result set; should be done after every database fetch

dbClearResult(rs)



#flagging invalid blood pressure values

#blood pressure values are recorded in formats such as 123/90 or 116/81
#an errored blood pressure is where the numerator(diastolic value) is equal to or less than the denominator(systolic value)

#first extract the systolic and diastolic blood pressure components

systolic_pressure<-as.numeric(gsub("(.*)/.*","\\1",anc_round2_dataset$blood_pressure));

diastolic_pressure<-as.numeric(gsub(".*/(.*)","\\1",anc_round2_dataset$blood_pressure));


invalid_blood_pressure=
  anc_round2_dataset[diastolic_pressure>=systolic_pressure & anc_round2_dataset$ANC_study_round=="round 2" & !is.na(anc_round2_dataset$person_id) & anc_round2_dataset$blood_pressure !="999"
                     & !is.na(anc_round2_dataset$blood_pressure),
                     c("person_id","study_id","facility_mflcode","visit_date","blood_pressure")];


##invalid gestation values

#99 and 999 is the pre-defined way to acknowledge missing values so anything else violating the expectation is to be flagged

invalid_gestation=
  anc_round2_dataset[!is.na(anc_round2_dataset$gestation) & (as.numeric(anc_round2_dataset$gestation)==0 | as.numeric(anc_round2_dataset$gestation)>50) & 
                       anc_round2_dataset$gestation != "999.0" & anc_round2_dataset$ANC_study_round=="round 2"
                     & anc_round2_dataset$gestation != "99.0" & anc_round2_dataset$gestation != "0.0", 
                     c("person_id","study_id","facility_mflcode","visit_date","gestation")];

#everyone in the study/attending the ANC clinic should have gravidae one or more to imply one or more pregnancy

invalid_gravidae_values=
  anc_round2_dataset[!is.na(anc_round2_dataset$gravidae) & as.numeric(anc_round2_dataset$gravidae)<1 & anc_round2_dataset$gravidae != "999.0" & anc_round2_dataset$ANC_study_round=="round 2", 
                     c("person_id","study_id","facility_mflcode","visit_date","gravidae")];


##Find invalid weight values

invalid_weights=
  anc_round2_dataset[!is.na(anc_round2_dataset$weight) & as.numeric(anc_round2_dataset$weight)>200 & as.numeric(anc_round2_dataset$weight)<25 
                     & anc_round2_dataset$weight != "999.0" & anc_round2_dataset$ANC_study_round=="round 2", 
                     c("person_id","study_id","facility_mflcode","visit_date","weight")];


#with CCC ART start dates but missing regimen
ccc_ARTdates_Missing_regimen=
  anc_round2_dataset[!is.na(anc_round2_dataset$CCC_ART_start_date) & is.na(anc_round2_dataset$current_regimen)
                     & anc_round2_dataset$ANC_study_round=="round 2", 
                     c("person_id","study_id","facility_mflcode","visit_date","ANC_study_round","CCC_ART_start_date","current_regimen")];


##displaying suspicious ages

##install.packages("lubridate")


#predefining functions to calculate ages

calc_year_diff <- function(birthDate, refDate = Sys.Date()) {
  
  require(lubridate)
  
  period <- as.period(interval(birthDate, refDate),
                      unit = "year")
  
  period$year
  
}


client_age<-calc_year_diff(anc_round2_dataset$birthdate)

Suspicious_ages=
  anc_round2_dataset[client_age<13 | client_age>45 | is.na(anc_round2_dataset$birthdate) & anc_round2_dataset$person_id>2457 & anc_round2_dataset$ANC_study_round=="round 2", 
                     c("person_id","study_id","facility_mflcode","birthdate")];


##tring to determine invalid LMP values

lmp_diff_in_weeks = round(difftime(anc_round2_dataset$visit_date, anc_round2_dataset$lmp, units = "weeks"))

Invalid_LMP_Values=
  anc_round2_dataset[!is.na(anc_round2_dataset$lmp) & anc_round2_dataset$ANC_study_round=="round 2" &
                       (lmp_diff_in_weeks<0 | lmp_diff_in_weeks>52),
                     c("person_id","study_id","facility_mflcode", "ANC_study_round","visit_date","lmp")]; 





##invalid parity gravidae combo

##parity is recorded in formats such as 2+0 or 3+1; the first portion is the number of births and the second the number of deaths if any

#gravidae is the number of pregnancies a mother has ever had

#the gravidae value should always be greater than or equivalent to the first portion of parity

library(stringr);

absolute_parity=substr(anc_round2_dataset$parity,0,1)

##as.numeric(absolute_parity)

gravidae_parity_diff=as.numeric(anc_round2_dataset$gravidae)-as.numeric(absolute_parity)

Invalid_parity_gravidae_combo=
  anc_round2_dataset[!is.na(anc_round2_dataset$parity) & anc_round2_dataset$ANC_study_round=="round 2" &
                       !is.na(anc_round2_dataset$gravidae) & gravidae_parity_diff < 1,
                     c("person_id","study_id","facility_mflcode", "ANC_study_round","visit_date","parity","gravidae")]; 



##tagging invalid HDSS residency start dates
Invalid_residency_start_dates=
  anc_round2_dataset[!is.na(anc_round2_dataset$residency_start_date) & anc_round2_dataset$ANC_study_round=="round 2" &
                       anc_round2_dataset$residency_start_date>anc_round2_dataset$visit_date,
                     c("person_id","study_id","facility_mflcode", "ANC_study_round","visit_date","residency_start_date")]; 



##flag invalid viral load dates
Invalid_VL_dates=
  anc_round2_dataset[!is.na(anc_round2_dataset$most_recent_ViralLoad_datetime) & anc_round2_dataset$ANC_study_round=="round 2" &
                       anc_round2_dataset$most_recent_ViralLoad_datetime != "UNAVAIBLE-UNAVAIBLE-UNAVAIBLE" &
                       anc_round2_dataset$most_recent_ViralLoad_datetime>anc_round2_dataset$visit_date,
                     c("person_id","study_id","facility_mflcode", "ANC_study_round","visit_date","most_recent_ViralLoad_datetime")];



##invalid EDD dates

edd_diff_in_weeks = round(difftime(anc_round2_dataset$visit_date, anc_round2_dataset$edd, units = "weeks"))


Invalid_EDD_dates=
  anc_round2_dataset[!is.na(anc_round2_dataset$edd) & anc_round2_dataset$ANC_study_round=="round 2" &
                       (edd_diff_in_weeks>40),
                     c("person_id","study_id","facility_mflcode", "ANC_study_round","visit_date","edd")]; 


##flag sample draw questions with negative status

Negative_with_Sample_draw=
  anc_round2_dataset[!is.na(anc_round2_dataset$sample_actually_drawn) & anc_round2_dataset$ANC_study_round=="round 2" &
                       anc_round2_dataset$sample_actually_drawn=="YES" & anc_round2_dataset$first_ANC_hiv_test_result=="N",
 
                     c("person_id","study_id","facility_mflcode", "ANC_study_round","visit_date","sample_actually_drawn","first_ANC_hiv_test_result")];

#setting the current working directory
setwd("D:/ANC/R");



##get the current date to append to the file name of the intended Excel output

#currentDate <- Sys.Date();
currentDate <- format(Sys.Date(), "%d%b%Y");
ExcelFileName <- paste("data_cleaning_",currentDate,".xlsx",sep="") 



#install.packages("openxlsx")


## output the ANC issues/queries to an Excel file
require(openxlsx)
list_of_datasets <- list("Suspecious ages" = Suspicious_ages, "Wrong Parity Gravidae Combo" = Invalid_parity_gravidae_combo, "Invalid gravidae" = invalid_gravidae_values,
                         "Invalid Blood pressure" = invalid_blood_pressure,"Negative With Sample Draw" = Negative_with_Sample_draw,
                         "Suspicious weights" = invalid_weights, "Invalid gestation" = invalid_gestation,
                         "Invalid LMP Values" = Invalid_LMP_Values, "Invalid EDD" = Invalid_EDD_dates, "Invalid Residency Start Dates" = Invalid_residency_start_dates,
                         "ART Start Missing Regimen"=ccc_ARTdates_Missing_regimen,
                         "Invalid VL dates" = Invalid_VL_dates)

write.xlsx(list_of_datasets, file = ExcelFileName)

##the below portion is useful in forwading the generated workbook to given mail addresses

#sending mail messages requires mailR library

##it is noteworthy that the mailR requires jdk 8 or highre installed

#configuring R to point to the JRE path in your machine
Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jdk1.8.0_151\\jre")

require(mailR)

send.mail(from = "xyz@gmail.com",  
          to = c("destination1@gmail.com", "destination2@gmail.com"),
          subject = "ANC Data Quality Issues",
          body ="ANC Study Queries for response by field workers",
          smtp = list(host.name = "smtp.gmail.com", port = 587, user.name = "xyz@gmail.com", passwd = "password_for_the_smtp_mail", ssl = TRUE),              
          authenticate = TRUE,
          attach.files = c(ExcelFileName),
          file.names = c(ExcelFileName),
          send = TRUE)

#the above case use the free gmail SMTP server


























