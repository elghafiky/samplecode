# Clear console and environment 
graphics.off(); rm(list=ls());cat("\14");

# Load packages
pacman::p_load(readstata13,stringr,tidyverse,expss)

# Set directory
user1 <- str_extract(getwd(),"u7083951")
user2 <- str_extract(getwd(),"elgha")
folder <- "/OneDrive - Australian National University/School/ANU/3b Applied Metrics Paper"

if (!is.na(user1)==TRUE)
{setwd(paste("C:/Users/",user1,folder, sep=""))}
if (!is.na(user2)==TRUE)
{setwd(paste("C:/Users/",user2,folder, sep=""))}

raw <-  "/2 Data/1 Raw"
pro <-	"/2 Data/2 Processed"
fin <-	"/2 Data/3 Final"
tb  <-  "/5 Tables"
gp  <-  "/6 Graph"

for (i in 20:22) {
  for (j in c("Mar","Sep")) {
    assign(paste("sus",i,j, sep=""),paste(getwd(),raw,"/Susenas 20",i," ",j,sep=""))
  }
}

# Load data
allsus <- list(read.dta13(paste(sus22Mar,"/KOR/kor22ind_subset.dta", sep="")),
               read.dta13(paste(sus21Mar,"/KOR/kor21ind_subset.dta", sep="")),
               read.dta13(paste(sus20Mar,"/KOR/kor20ind_subset.dta", sep="")))

# Rename variables
allsus <- lapply(allsus,rename,
                 prov=r101,
                 locres=r105,
                 id=r401,
                 sex=r405,
                 age=r407,
                 agefm=r409,
                 kip=r616,
                 pip=r617,
                 pw_working=r702_a,
                 pw_schooling=r702_b,
                 pw_domwork=r702_c,
                 pw_other=r702_d,
                 pw_nothing=r702_x,
                 wstat=r706,
                 dis_vision=r1002,  
                 dis_hearing=r1003,
                 dis_walking=r1004,  
                 dis_arm=r1005,  
                 dis_mind=r1006,  
                 dis_behavior=r1007,  
                 dis_talking=r1008,  
                 dis_pcare=r1009,
                 sickpm=r1102, 
                 sickdisturbact=r1103,
                 everpregnant=r1501a, 
                 agepreg=r1501b,      
                 birthsucceed=r1502a,      
                 birthage=r1502b,
                 birthloc=r1504a,      
                 birthhelp=r1504b,      
                 birthweight=r1504c,
                 kbstat=r1601, 
                 kbtype=r1602,
                 dob_d=r406a,  
                 dob_m=r406b,  
                 dob_y=r406c)

sus22mar <- data.frame(allsus[1])
sus22mar <- rename(sus22mar,
                   hhid=urut,
                   edu_lvl=r612,
                   edu_class=r613,
                   edu_cert=r614,
                   num_abuse=r908, 
                   num_assault=r912)      

sus2 <- list(data.frame(allsus[2]),data.frame(allsus[3]))
sus2 <- lapply(sus2,rename,
               hhid=renum,
               edu_lvl=r613,
               edu_class=r614,
               edu_cert=r615,
               num_abuse=r907, 
               num_assault=r911)
sus21mar <- data.frame(sus2[1])
sus20mar <- data.frame(sus2[2])
rm(allsus,sus2)

# Create year variable
sus22mar$year <- 3
sus21mar$year <- 2
sus20mar$year <- 1

# Append data
appenddata <- rbind(sus22mar,sus21mar,sus20mar)
appenddata <- subset(appenddata, select=-r1504alain)

# Create running variable
for (i in c("dob_d","dob_m")) {
  appenddata[,paste(i,"x",sep="")]=appenddata[,i]
  appenddata[,i] <- as.character(appenddata[,i])
  for (j in c(which(appenddata[,paste(i,"x",sep="")]<10))) {
    appenddata[j,i] <- paste("0",appenddata[j,i],sep="")
  }
  for (j in c(which(appenddata[,paste(i,"x",sep="")]==98))) {
    appenddata[j,i] <- NA
  }
}

appenddata$dob_YM <- paste(appenddata$dob_y,appenddata$dob_m,sep="")
appenddata$dob_YMD <- paste(appenddata$dob_y,dob_m,appenddata$dob_d,sep="")
dobvar <- c("dob_YM","dob_YMD")
appenddata[,dobvar] <- lapply(appenddata[,dobvar],as.numeric)
appenddata$dob19_YM <- appenddata$dob_YM+1900
appenddata$dob19_YMD <- appenddata$dob_YMD+190000
appenddata$runvar_dob_YM <- appenddata$dob19_YM - 201910
appenddata$runvar_dob_YMD <- appenddata$dob19_YMD - 20191015 

appenddata$abp_YM <- (as.numeric(difftime("2019-10-15",
                              as.Date(paste(appenddata$dob_y,appenddata$dob_m,"01",sep="-")))))/365
appenddata$abp_YMD <- (as.numeric(difftime("2019-10-15",
                               as.Date(paste(appenddata$dob_y,appenddata$dob_m,appenddata$dob_d,sep="-")))))/365
abpvar <- c("abp_YM","abp_YMD")
for (i in abpvar) {
   appenddata[,paste("runvar",i,sep="_")] <- (appenddata[,i]-19)*-1
}

for (i in c("dob","abp")) {
  for (j in c("YM","YMD")) {
    appenddata[,paste("itt",i,j,sep="_")]=appenddata[,paste("runvar",i,j,sep="_")]>=0 
  }
}

# Label variables
appenddata <- apply_labels(appenddata, 
                         hhid="household id",
                         id="individual id",
                         prov="province id",
                         locres="Type of residential location",
                         agefm="Age when first got married",
                         edu_lvl="Highest education ever attended",
                         edu_class="Highest class ever attended",
                         edu_cert="Highest education certificate owned",
                         kip="Received KIP in the past year",
                         pip="Received PIP in the past year",
                         pw_working="Working in the past week",
                         pw_schooling="Schooling in the past week",
                         pw_domwork="Engaged in domestic work in the past week",
                         pw_other="Doing other work in the past week",
                         pw_nothing="Did nothing in the past week",
                         wstat="Employment status",
                         num_abuse="Number of times been abused in the past year",
                         num_assault="Number of times been assaulted in the past year",
                         dis_vision="Vision impairment",
                         dis_hearing="Hearing impairment",
                         dis_walking="Walking impairment",
                         dis_arm="Impairment in moving arms/finger",
                         dis_mind="Memorizing/concentrating impairment",
                         dis_behavior="Impairment in behavior/emotion",
                         dis_talking="Impairment in communication",
                         dis_pcare="Impairment in self care",
                         sickpm="Ever fell ill in the past month",
                         sickdisturbact="Illness disturbed activities in the past month",
                         everpregnant="Ever pregnant",
                         agepreg="Age when first pregnant",
                         birthsucceed="Ever gave successful birth",
                         birthage="Age when first giving successful birth",
                         birthloc="Facility of last successful delivery",
                         birthhelp="Who helped with the last successful delivery",
                         birthweight="Baby birth on the last successful delivery",
                         kbstat="Ever or currently using family planning",
                         kbtype="Type of family planning",
                         year="Year after intervention")

# Place value label
val_lab(appenddata$locres)=num_lab("
                        1 Urban 
                        2 Rural
                        ")
eduvar <- c("edu_lvl","edu_cert")
val_lab(appenddata[,eduvar])=num_lab("
                                      1	Paket A
                                      2	SDLB
                                      3	SD
                                      4	MI
                                      5	Paket B
                                      6	SMPLB
                                      7	SMP
                                      8	MTs
                                      9	Paket C
                                      10	SMLB
                                      11	SMA
                                      12	MA
                                      13	SMK
                                      14	MAK
                                      15	D1/D2
                                      16	D3
                                      17	D4
                                      18	S1
                                      19	Profesi
                                      20	S2
                                      21	S3
                                      ")