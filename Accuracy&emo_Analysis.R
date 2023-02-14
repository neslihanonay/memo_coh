install.packages("rprime")
library("rprime")
library(splitstackshape)
library(tidyr)
install.packages("stringr")
library("stringr")
install.packages("tidyverse")
library(splitstackshape)
library(dplyr)
install.packages("dplyr")
library("writexl") 
install.packages("writexl")
library(dplyr)
install.packages("rlang")
remove.packages("rlang")
library(readxl)


path_raw <- "/Users/onay9/Desktop/holyrec_data/6_sec_childreb/memory/" #UPDATE WITH THE DIRECTORY IN WHICH YOUR RAW XLSX FILES IS.

files.lst_raw <- list.files(path_raw, pattern = "*txt")

matte <- function(myfile){
  aa<- read_eprime(paste0(path_raw, myfile)) #here we are reading e-prime files. There needs to be changing pathname. Once it is set, then for each participant changing the subject name is sufficient for code to proceed. For example, changing 01 to 02 for the second participant.
  bb <- FrameList(aa) 
  cc <- to_data_frame(bb) #changing e-prime file format to data frame 
  cc <- select(cc, c('Event', 'Recup.ACC')) #we select relevant variables for the dependency models
  cc <- na.omit(cc) #we deleted trials
  cc<- cSplit(cc, "Event", '_') #data cleaning
  cc <- cSplit(cc, "Event_4", '.jpg') #data cleaning
  dd <- select(cc, c('Event_2', 'Event_4_1','Recup.ACC')) #in order to create matrix, we choose the relevant variables for the model
  names(dd)[names(dd) == "Event_2"] <- "retrieval_trial" #update the variable name
  names(dd)[names(dd) == "Event_4_1"] <- "event_number" #update the variable name
  names(dd)[names(dd) == "Recup.ACC"] <- "accuracy" #update the variable name
  ee<- dd %>% spread(key = retrieval_trial, value = accuracy) #transform the matrix to MxN format
  sn <- aa[19]
  subj_number <- str_replace(sn, ": ", "_")
  subj_number <- str_remove(subj_number, "DataFile.Basename_") #FOR ADULT DATA
  subj_number <- as.data.frame(subj_number)
  write_xlsx(ee, paste0(subj_number, ".xlsx")) #write the data frame to computer. Here one needs to change the pathname to set his/her computer. Also, for each participant, there needs to be change in the file name. For example changing 101 to 102 for the second participant.
}

mattem <- lapply(files.lst_raw, matte)

#here we will calculate accuracy scores

path_matr <- "/Users/onay9/Desktop/holyrec_data/6_sec_childreb/matrices/"
files.lst_mat <- list.files(path_matr, pattern = "*xlsx")

acc_ass <- function(myfile){
  my_df_acc <- read_xlsx(paste0(path_matr, myfile))
  my_df_acc <- lapply(my_df_acc, as.numeric)
  my_df_acc <- as.data.frame(my_df_acc)
  accdf_neu <- my_df_acc[1:20,]
  accdf_neg <- my_df_acc[21:40,]
  acc_neu <- sum(accdf_neu[2:7])/120
  acc_neg <-sum(accdf_neg[2:7])/120
  acc_LocPer_neu <- sum(accdf_neu[,c(2, 4)])/40
  acc_LocObj_neu <- sum(accdf_neu[,c(3, 6)])/40
  acc_PerObj_neu <- sum(accdf_neu[,c(5, 7)])/40
  
  acc_LocPer_neg <- sum(accdf_neg[,c(2, 4)])/40
  acc_LocObj_neg <- sum(accdf_neg[,c(3, 6)])/40
  acc_PerObj_neg <- sum(accdf_neg[,c(5, 7)])/40
  
  subjNo <- str_remove(paste0(myfile), ".xlsx")
  all_acc <- cbind(subjNo, acc_neu, acc_neg, acc_LocPer_neu, acc_LocObj_neu, acc_PerObj_neu, acc_LocPer_neg, acc_LocObj_neg, acc_PerObj_neg)
  all_acc <- as.data.frame(all_acc)
  write_xlsx(all_acc, paste0(subjNo, "acc.xlsx"))
}

mattem_acc <- lapply(files.lst_mat, acc_ass)
  

#Now, we will bind them into one data frame.

path_acc_all <- "/Users/onay9/Desktop/holyrec_data/6_sec_childreb/accuracy_per/"

url_xlsx_acc <- list.files(path_acc_all, pattern = "*.xlsx", recursive = TRUE)


#here there is a listing of all files with specific extention (xlsx) that we want to merge. IMPORTANT:Pay attention if you have just your data files in the path.
read_xlsx_files_acc <- function(x){
  df <- read_xlsx(path = paste(path_acc_all, x, sep = "/"))
  subjnumber <- str_remove(paste0(x), ".xlsx")
  df <- cbind(df, subjnumber)
  return(df)
} 

df_acc <- lapply(url_xlsx_acc,read_xlsx_files_acc ) %>%
  bind_rows() #Files (one file-per subject) is merged into one data frame

write_xlsx(df_acc, "/Users/onay9/Desktop/holyrec_data/acc_6sec_kids.xlsx")
  
#---------------------------
#MATLAB
#---------------------------

path_dep <- "/Users/onay9/Desktop/holyrec_data/6_sec_childreb/dep_per"

url_xlsx_dep <- list.files(path_dep, pattern = "*.xlsx", recursive = TRUE)


#here there is a listing of all files with specific extention (xlsx) that we want to merge. IMPORTANT:Pay attention if you have just your data files in the path.
read_xlsx_files_dep <- function(x){
  df <- read_xlsx(path = paste(path_dep, x, sep = "/"))
  subjnumber2 <- str_remove(paste0(x), ".xlsx")
  df <- cbind(df, subjnumber2)
  return(df)
} 

df_dep <- lapply(url_xlsx_dep,read_xlsx_files_dep ) %>%
  bind_rows() #Files (one file-per subject) is merged into one data frame

event_type <- (x = c("all_data", "emotional", "neutral")) #To be able to do within-subjects analysis for event type, we stored all data. all_data represents the indices from the all the trials which a respondent answered. "emotional" represents only emotional events. "neutral" represents only neutral events.

df_dep <- data.frame(df_dep, event_type) #we created one data frame in which all subjects are here.
attach(df_dep)
new_all_df <- filter(df_dep,df_dep$event_type=="all_data")
new_emo_df <- filter(df_dep,df_dep$event_type=="emotional")
new_neutral_df <- filter(df_dep,df_dep$event_type=="neutral")
names(new_all_df)[names(new_all_df) == "data_dependency"] <- "all_data_DataDependency"
names(new_all_df)[names(new_all_df) == "independent_dependency"] <- "all_data_IndepModelDependency"
names(new_all_df)[names(new_all_df) == "dependent_dependency"] <- "all_data_DepModelDependency"
names(new_emo_df)[names(new_emo_df) == "data_dependency"] <- "emotional_DataDependency"
names(new_emo_df)[names(new_emo_df) == "independent_dependency"] <- "emotional_IndepModelDependency"
names(new_emo_df)[names(new_emo_df) == "dependent_dependency"] <- "emotional_DepModelDependency"
names(new_neutral_df)[names(new_neutral_df) == "data_dependency"] <- "neutral_data_DataDependency"
names(new_neutral_df)[names(new_neutral_df) == "independent_dependency"] <- "neutral_data_IndepModelDependency"
names(new_neutral_df)[names(new_neutral_df) == "dependent_dependency"] <- "neutral_data_DepModelDependency"
all <- cbind(new_all_df, new_emo_df, new_neutral_df)
all <- all[, !duplicated(colnames(all))]
all <- subset(all, select = -c(event_type))
all = all %>% select(subj_number, everything()) #we moved the variable subj_number to the first order.
all$dep2_all <- all$all_data_DataDependency-all$all_data_IndepModelDependency
all$dep2_negative <- all$emotional_DataDependency-all$emotional_IndepModelDependency
all$dep2_neutral <- all$neutral_data_DataDependency-all$neutral_data_IndepModelDependency
write_xlsx(all, "/Users/onay9/Desktop/holyrec_data/data_dep_6sec_kids.xlsx") #UPDATE IS NEEDED BEFORE RUNNING

#--------------------------
#NEGATIVITY RATING
#---------------------------

path_neg_rat <- "/Users/onay9/switchdrive/Neslihan/Andreias_Folder/Test/04-Data/v04_Sadness_Incong_12sec/Affect/Negativity/"

url_txt <- list.files(path_neg_rat, pattern = "*.txt", recursive = TRUE)

neg_aff <-  function(x){
  aa<- read_eprime(paste0(path_neg_rat, x))
  bb <- FrameList(aa) 
  cc <- to_data_frame(bb) #changing e-prime file format to data frame 
  cc <- select(cc, c('image', 'Slide1.RESP')) #we select relevant variables for the dependency models
  cc <- na.omit(cc) #we deleted trials
  names(cc)[names(cc) == "image"] <- "event" #update the variable name
  names(cc)[names(cc) == "Slide1.RESP"] <- "rate" #update the variable name
  cc$rate <- as.numeric(cc$rate)
  cc<- cSplit(cc, "event", '_')
  cc <- cSplit(cc, "event_2", '.jpg')
  names(cc)[names(cc) == "event_2_1"] <- "event_number"
  cc$event_number<- as.numeric(cc$event_number)
  cc$cond_emo[cc$event_number<=20] <- "neutral"
  cc$cond_emo[cc$event_number>20] <- "negative"
  neutral_mean <- cc[cond_emo=="neutral", mean(rate)]
  negative_mean <- cc[cond_emo=="negative", mean(rate)]
  subj_number <- str_remove(paste0(x), ".txt")
  subj_number <- str_remove(subj_number, "A_Post-")
  subj_number <- str_remove(subj_number, "-1")
  all_neg <- cbind(subj_number, neutral_mean, negative_mean)
  all_neg <- as.data.frame(all_neg)
  write_xlsx(all_neg, paste0(subj_number, "neg.xlsx"))
}

neg_rat <- lapply(url_txt, neg_aff)

#Now, we merge everything:

path_neg_rat_p <- "/Users/onay9/switchdrive/Neslihan/Andreias_Folder/Test/04-Data/v04_Sadness_Incong_6sec/Adults/subj1-33/02-02-Processed_negativity_per_subj/"

url_xlsx_p <- list.files(path_neg_rat_p, pattern = "*.xlsx", recursive = TRUE)

read_xlsx_files_neg <- function(x){
  df <- read_xlsx(path = paste(path_neg_rat_p, x, sep = "/"))
  subjnumber <- str_remove(paste0(x), ".xlsx")
  df <- cbind(df, subjnumber)
  return(df)
} 

df_neg<- lapply(url_xlsx_p,read_xlsx_files_neg ) %>%
  bind_rows() #Files (one file-per subject) is merged into one data frame

write_xlsx(df_neg, "/Users/onay9/switchdrive/Neslihan/Andreias_Folder/Test/04-Data/v04_Sadness_Incong_6sec/Adults/subj1-33/neg_all_adults.xlsx")


#--------------------------
#AROUSAL-VALENCE
#---------------------------

path_sam <- "/Users/onay9/Desktop/holyrec_data/Kids/November/vol02/Raw_xlsx_SAM/"

url_sam <- list.files(path_sam, pattern = "*.xlsx", recursive = TRUE)

sam_aff <- function(x){
  subj_sam <- read_xlsx(paste0(path_sam, x))
  subj_sam$stimuliCondition <- as.factor(subj_sam$stimuliCondition)
  subj_sam <- subj_sam[-c(41:48),]
  attach(subj_sam)
  
  val_neutr_mean <- mean(subj_sam$ValResp.response_raw[subj_sam$stimuliCondition == "Neutral"])
  arousal_neutr_mean <- mean(subj_sam$ArousalResp.response_raw[subj_sam$stimuliCondition == "Neutral"])
  val_neg_mean <- mean(subj_sam$ValResp.response_raw[subj_sam$stimuliCondition == "Negative"])
  arousal_neg_mean <- mean(subj_sam$ArousalResp.response_raw[subj_sam$stimuliCondition == "Negative"])
  subjNosam <- str_remove(paste0(x), ".xlsx")
  subjNosam<-substring(subjNosam,1, nchar(subjNosam)-45)
  affectives <- cbind(subjNosam, val_neutr_mean, arousal_neutr_mean, val_neg_mean, arousal_neg_mean)
  affectives <- as.data.frame(affectives)
  write_xlsx(affectives, paste0(subjNosam, "_sam.xlsx")) #UPDATE IS NEEDED BEFORE RUNNING
}

sam_rat <- lapply(url_sam, sam_aff)

#now we merge everything:


path_sam_p <- "/Users/onay9/Desktop/holyrec_data/Kids/November/vol02/SAM_per_subj"
url_sam_p <- list.files(path_sam_p, pattern = "*.xlsx", recursive = TRUE)

read_xlsx_files_sam <- function(x){
  df <- read_xlsx(path = paste(path_sam_p, x, sep = "/"))
  subjnumber <- str_remove(paste0(x), ".xlsx")
  df <- cbind(df, subjnumber)
  return(df)
} 

df_sam<- lapply(url_sam_p,read_xlsx_files_sam ) %>%
  bind_rows() #Files (one file-per subject) is merged into one data frame

write_xlsx(df_sam, "/Users/onay9/Desktop/holyrec_data/sam_all_11thNov.xlsx")

#After we have one data frame each dimension of an analysis, below we merge them into one.


#accuracy data frame, check with the subject numbers
df_acc$subjNo <- str_remove(df_acc$subjNo, "Adultes-")
df_acc$subjNo <- str_remove(df_acc$subjNo, "-1")
df_acc<-df_acc[,-c(10)]

#dependency data frame, check with the subject numbers
all$subjnumber2 <- str_remove(all$subjnumber2, "Adultes-")
all$subjnumber2 <- str_remove(all$subjnumber2, "-1_p")
all<- all[,-c(1,2,3,11)]

#negativity rating data frame, check with the subject numbers

df_neg$subjnumber <- str_remove(df_neg$subjnumber, "QA_1")
df_neg$subjnumber <- str_remove(df_neg$subjnumber, "neg")

df_neg[3,4]<- str_remove(df_neg[3,4], "-43")
df_neg <- df_neg[,-c(1)]
df_neg$Subj_Number <- as.integer(df_neg$Subj_Number)

#SAM rating data frame, check with the subject numbers


df_sam$subjnumber<- str_remove(df_sam$subjnumber, "_sam")
df_sam<-df_sam[,-c(6)]

#change in names
names(df_acc)[names(df_acc) == "subjNo"] <- "Subj_Number"
names(all)[names(all) == "subjnumber2"] <- "Subj_Number"
names(df_neg)[names(df_neg) == "subjnumber"] <- "Subj_Number"

names(df_sam)[names(df_sam) == "subjNosam"] <- "Subj_Number"

#now we merge files
df_all_varsv01 <-  merge(df_acc, all, by="Subj_Number")
df_all_varsv02 <- merge(df_neg, df_all_varsv01, by="Subj_Number")


write_xlsx(df_all_varsv02, "/Users/onay9/switchdrive/Neslihan/Andreias_Folder/Test/04-Data/v04_Sadness_Incong_6sec/Adults/subj1-33/binded_adults.xlsx" )

binded_df <-  rbind(kids_v01, kids_v02)
write_xlsx(binded_df, "/Users/onay9/switchdrive/Neslihan/Andreias_Folder/Test/04-Data/v04_Sadness_Incong_6sec/Adults/subj1-33/binded_adults.xlsx" )





























































































































  
  





