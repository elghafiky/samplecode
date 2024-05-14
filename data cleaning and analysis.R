# Clear console and environment 
graphics.off(); rm(list=ls());cat("\14");

# Load packages
pacman::p_load(stringr,tidyverse,readxl,rnaturalearth,rnaturalearthdata,sf,scales)

# Set directory
user1 <- str_extract(getwd(),"u7083951")
user2 <- str_extract(getwd(),"elgha")
folder <- "/Dropbox/EB/School/ANU/3b Applied Metrics Paper"

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
assign("sup",paste(getwd(),raw,"/Supas 2015/2 - Data",sep=""))
assign("md",paste(getwd(),raw,"/Marriage dispensation",sep=""))

#===============#
# DATA CLEANING #
#===============#

###################
# INDIVIDUAL DATA #
###################

# Load data, filter relevant obs, and rename var
allkor.id <- list(fread(paste(getwd(),pro,"/kor20ind.csv", sep="")),
                  fread(paste(getwd(),pro,"/kor21ind.csv", sep="")),
                  fread(paste(getwd(),pro,"/kor22ind.csv", sep=""))) %>%
  lapply(filter,r407>18) %>%
  lapply(rename,
         prop=r101,
         kab=r102,
         locres=r105,
         id=r401,
         relhh=r403,
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

allkor.id[[3]] <- allkor.id[[3]] %>%
  rename(hhid=urut,
         edu_class=r613,
         num_abuse=r908,
         num_assault=r912,
         bir_prov=r601,
         bir_dist=r602,
         premig_prov=r603,
         premig_dist=r604) %>%
  mutate(edu_lvl=case_when(
    r612<6 ~ 1,
    between(r612,6,10) ~ 2,
    between(r612,11,17) ~ 3,
    r612>17 ~ 4,
    is.na(r612) ~ 0),
    edu_cert=case_when(
      r614<6 ~ 1,
      between(r614,6,10) ~ 2,
      between(r614,11,17) ~ 3,
      between(r614,18,24) ~ 4,
      r614==25 ~ 0,
      is.na(r614) ~ 0))

for (i in 1:2) {
  allkor.id[[i]] <- allkor.id[[i]] %>%
    rename(hhid=renum,
           edu_class=r614,
           num_abuse=r907, 
           num_assault=r911,
           bir_prov=r602,
           bir_dist=r603,
           premig_prov=r604,
           premig_dist=r605) %>%
    mutate(edu_lvl=case_when(
      r613<5 ~ 1,
      between(r613,5,8) ~ 2,
      between(r613,9,14) ~ 3,
      r613>14 ~ 4,
      is.na(r613) ~ 0),
      edu_cert=case_when(
        r615<5 ~ 1,
        between(r615,5,8) ~ 2,
        between(r615,9,14) ~ 3,
        between(r615,15,21) ~ 4,
        r615==22 ~ 0,
        is.na(r615) ~ 0))
}

for (i in 1:3) {
  allkor.id[[i]] <- mutate(allkor.id[[i]],year=i)
}

# Select variables, uniformize variable types, and append data
iddata <- lapply(allkor.id,select,year,hhid,
                 prop,kab,locres,id,relhh,sex,age,agefm,kip,pip,pw_working,pw_schooling,pw_domwork,
                 pw_other,pw_nothing,wstat,dis_vision,dis_hearing,dis_walking,dis_arm,dis_mind,
                 dis_behavior,dis_talking,dis_pcare,sickpm,sickdisturbact,everpregnant,agepreg,
                 birthsucceed,birthage,birthloc,birthhelp,birthweight,kbstat,kbtype,dob_d,dob_m,dob_y,
                 edu_lvl,edu_class,edu_cert,num_abuse,num_assault,bir_prov,bir_dist,premig_prov,premig_dist) %>%
  lapply(mutate,
         bir_prov=as.numeric(bir_prov),
         bir_dist=as.numeric(bir_dist),
         premig_prov=as.numeric(premig_prov),
         premig_dist=as.numeric(premig_dist)) %>% 
  bind_rows() %>%
  filter(dob_m!=98) %>%
  mutate(dob_d=case_when(
    dob_d==98 ~ NA,
    dob_d!=98 ~ dob_d)) %>%
  as.data.frame()

# Create running variable
for (i in c("dob_d","dob_m")) {
  iddata[,paste(i,"x",sep="")]=iddata[,i]
  iddata[,i] <- as.character(iddata[,i])
  iddata[which(iddata[,paste(i,"x",sep="")]<10),i] <- 
    paste("0",iddata[which(iddata[,paste(i,"x",sep="")]<10),i],sep="")
  iddata[c(which(iddata[,paste(i,"x",sep="")]==98)),i] <- NA
}

iddata$dob_YM <- paste(iddata$dob_y,iddata$dob_m,sep="")
iddata$dob_YMD <- paste(iddata$dob_y,iddata$dob_m,iddata$dob_d,sep="")
dobvar <- c("dob_YM","dob_YMD")
iddata[,dobvar] <- lapply(iddata[,dobvar],as.numeric)
iddata$dob19_YM <- iddata$dob_YM+1900
iddata$dob19_YMD <- iddata$dob_YMD+190000
iddata$runvar_dob_YM <- iddata$dob19_YM - 201910
iddata$runvar_dob_YMD <- iddata$dob19_YMD - 20191015 

iddata$abp_YM <- (as.numeric(difftime("2019-10-15",
                                      as.Date(paste(iddata$dob_y,iddata$dob_m,"01",sep="-")))))/365
iddata$abp_YMD <- (as.numeric(difftime("2019-10-15",
                                       as.Date(paste(iddata$dob_y,iddata$dob_m,iddata$dob_d,sep="-")))))/365
abpvar <- c("abp_YM","abp_YMD")
for (i in abpvar) {
  iddata[,paste("runvar",i,sep="_")] <- (iddata[,i]-19)*-1
}

for (i in c("dob","abp")) {
  for (j in c("YM","YMD")) {
    iddata[,paste("itt",i,j,sep="_")]=iddata[,paste("runvar",i,j,sep="_")]>=0 
  }
}

# New variables and recoding
for (i in 18:19) {
  iddata[,paste("mb",i,sep="")] <- iddata$agefm<i
  iddata[which(is.na(iddata$agefm)),paste("mb",i,sep="")] = FALSE
}

for (i in 1:max(iddata$year)) {
  iddata[,paste("year",i,sep="")] <- iddata$year==i
}

iddata <- iddata %>%
  mutate(rural=locres==2,
         java=between(prop,31,36),
         female=sex==2,
         atdshs=case_when(
           edu_lvl>2 ~ 1,
           edu_lvl<3 ~ 0,
           is.na(edu_lvl) ~ 0),
         gradshs=case_when(
           edu_cert>2 ~ 1,
           edu_cert<3 ~ 0,
           is.na(edu_cert) ~ 0),
         atdhed=case_when(
           edu_lvl>3 ~ 1,
           edu_lvl<4 ~ 0,
           is.na(edu_lvl) ~ 0),
         num_violence=num_abuse+num_assault,
         failpreg=everpregnant==1 & birthsucceed==5,
         sickdisturbact=case_when(
           is.na(sickdisturbact) ~ 0,
           TRUE ~ sickdisturbact),
         workingpw=pw_working=="A",
         schoolingpw=pw_schooling=="B")

for (i in names(select(iddata,starts_with("dis")))) {
  for (j in 5:8) {
    iddata[which(iddata[,i]==j),i] <- j-4
  }
  iddata[,paste("no",i,sep="")] <- iddata[,i]==4
}

for (i in c("sickdisturbact","birthsucceed")) {
  iddata[which(iddata[,i]==5),i] <- 0 
}

for (i in c("num_abuse","num_assault","num_violence")) {
  iddata[which(is.na(iddata[,i])),i] <- 0
  iddata[,substr(i,5,nchar(i))] <- iddata[,i]>0
}

# Variable labels
iddata <- apply_labels(iddata,
                       id="individual id",
                       relhh="Relationship with household head",
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
                       mb18="Married before 18",
                       mb19="Married before 19",
                       bir_prov="Birthplace province",
                       bir_dist="Birthplace district",
                       premig_prov="Province 5 years ago",
                       premig_dist="District 5 years ago")

# Place value label
val_lab(iddata$sex)=num_lab("1 Male 
                             2 Female")
val_lab(iddata$locres)=num_lab("1 Urban 
                                2 Rural")
val_lab(iddata$wstat)=num_lab("1 Self-employed (SE)
                               2 SE with casual labor/unpaid worker
                               3 SE with paid labor
                               4 Laborer/employee
                               5 Casual worker
                               6 Unpaid family worker")
  
##################
# HOUSEHOLD DATA #
##################

# Load data and uniformize variable name
allkor.rt <- list(fread(paste(getwd(),pro,"/kor20rt.csv", sep="")),
                  fread(paste(getwd(),pro,"/kor21rt.csv", sep="")),
                  fread(paste(getwd(),pro,"/kor22rt.csv", sep=""))) %>%
  lapply(rename,
         ownrefri=r2001b,
         ownmbike=r2001h,
         walltype=r1807) 

allkor.rt[3] <- lapply(allkor.rt[3],rename,hhid=urut) 
allkor.rt[1:2] <- lapply(allkor.rt[1:2],rename,hhid=renum)
for (i in 1:3) {
  allkor.rt[i] <- lapply(allkor.rt[i],mutate,year=i)
}

# Select variables and append data
rtdata <- lapply(allkor.rt,select,hhid,ownrefri,ownmbike,walltype,year) %>%
  bind_rows() %>%
  mutate(cwall=walltype==1) %>%
  data.frame()

for (i in c("ownrefri","ownmbike")) {
  rtdata[which(rtdata[,i]==5),i] <- 0 
}

# household head variables
hhead <- filter(iddata,relhh==1) %>%
  select(hhid,sex,edu_cert,wstat,age,year) %>%
  rename(hhead_sex=sex,
         hhead_edu_cert=edu_cert,
         hhead_wstat=wstat,
         hhead_age=age) %>%
  mutate(hhead_fem=hhead_sex==2,
         hhead_smp=case_when(
           hhead_edu_cert>2 ~ 1,
           hhead_edu_cert<3 ~ 0,
           is.na(hhead_edu_cert) ~ 0),
         hhead_formalw=case_when(
           between(hhead_wstat,3,4) ~ TRUE,
           !between(hhead_wstat,3,4) ~ FALSE,
           is.na(hhead_wstat) ~ FALSE,),
         hhead_agesq=hhead_age^2) %>%
  apply_labels(hhead_sex="Sex of household head",
               hhead_edu_cert="Education of household head",
               hhead_wstat="Employment status of household head")

# household size
byhh <- group_by(iddata,hhid,year) %>%
  summarize(hhsize=n())

###############
# CONSUMPTION #
###############

allkp <- list(fread(paste(getwd(),pro,"/kp20blok43.csv", sep="")),
           fread(paste(getwd(),pro,"/kp21blok43.csv", sep="")),
           fread(paste(getwd(),pro,"/kp22blok43.csv", sep=""))) 

allkp[3] <- lapply(allkp[3],rename,hhid=urut) 
allkp[1:2] <- lapply(allkp[1:2],rename,hhid=renum)
for (i in 1:3) {
  allkp[i] <- lapply(allkp[i],mutate,year=i)
}
kpdata <- lapply(allkp,select,hhid,kapita,year) %>%
  bind_rows()

#########
# SUPAS #
#########

supas <- fread(paste(getwd(),pro,"/supas15.csv",sep="")) %>%
  filter(between(r407,20,24)) %>%
  mutate(mb18=case_when(
    r409<18 ~ 1,
    r409>=18 ~ 0,
    is.na(r409) ~ 0)) %>%
  group_by(prop,kab) %>%
  summarize(cmi=weighted.mean(mb18,weight)) %>%
  mutate(cmh=cmi>median(cmi)) %>%
  apply_labels(cmi="Child marriage incidence among 20-24 yo",
               cmh="Region with high child marriage incidence")

#########################
# MARRIAGE DISPENSATION #
#########################

cmd <- fread(paste(md,"/relcourt.csv",sep="")) %>%
  mutate(court_fnm=paste(court_type,court_nm,sep=" ")) %>%
  full_join(fread(paste(md,"/marrdisp.csv",sep="")),
            by=c("court_fnm","prov_nm","prov_id"),
            relationship="many-to-one") %>%
  mutate(kab_nm=case_when(
    kab_nm=="HUMBANG HASUDUTAN" ~ "HUMBANG HASUNDUTAN",
    kab_nm=="GUNUNG SITOLI" ~ "GUNUNGSITOLI",
    kab_nm=="PADANG SIDEMPUAN" ~ "PADANGSIDIMPUAN",
    kab_nm=="SAWAHLUNTO" ~ "SAWAH LUNTO",
    kab_nm=="PANUKAL ABAB LEMATANG ILIR" ~ "PENUKAL ABAB LEMATANG ILIR",
    kab_nm=="LUBUK LINGGAU" ~ "LUBUKLINGGAU",
    kab_nm=="BANYUASIN" ~ "BANYU ASIN", 
    kab_nm=="TULANG BAWANG" ~ "TULANGBAWANG",
    kab_nm=="KOTA KEDIRI" ~ "KEDIRI",
    kab_nm=="PONTIANAK" & kab_type=="KAB" ~ "MEMPAWAH",
    kab_nm=="PALANGKARAYA" ~ "PALANGKA RAYA",
    kab_nm=="BANJARBARU" ~ "BANJAR BARU",
    kab_nm=="KUTAI KERTANEGARA" ~ "KUTAI KARTANEGARA",
    kab_nm=="MAHAKAM ULU" ~ "MAHAKAM HULU",
    kab_nm=="TANAH TIDUNG" ~ "TANA TIDUNG",
    kab_nm=="KEPULAUAN SITARO" ~ "SIAU TAGULANDANG BIARO",
    kab_nm=="TOJO UNA UNA" ~ "TOJO UNA-UNA",
    kab_nm=="BAU-BAU" ~ "BAUBAU",
    kab_nm=="PAHUWANTO" ~ "POHUWATO",
    kab_nm=="YAKUHIMO" ~ "YAHUKIMO",
    kab_nm=="DEIYEI" ~ "DEIYAI",
    kab_nm=="FAK-FAK" ~ "FAKFAK",
    kab_nm=="TAMIANG" ~ "ACEH TAMIANG",
    kab_nm=="TANA TORAJA UTARA" ~ "TORAJA UTARA",
    kab_nm=="PANGKAJENE KEPULAUAN" ~ "PANGKAJENE DAN KEPULAUAN",
    TRUE ~ kab_nm)) %>%
  mutate(kab_type=case_when(
    kab_nm=="SERDANG BEDAGAI" ~ "KAB",
    kab_nm=="PADANG LAWAS" ~ "KAB",
    kab_nm=="CIANJUR" ~ "KAB",
    kab_nm=="BONTANG" ~ "KOTA",
    TRUE ~ kab_type)) %>%
  mutate(kab_fnm=paste(kab_type,kab_nm,sep=" ")) %>%
  as.data.frame()

for (i in c("withdrawn","decided")) {
  cmd[which(cmd[,i]=="-"),i] <- "0"
  cmd[,i] <- gsub(",","",cmd[,i]) %>%
    as.numeric()
  cmd[,paste("mean",i,sep="_")] <- ave(cmd[,i],cmd$kab_fnm) 
}

cmdu <- distinct(cmd,kab_fnm,.keep_all = TRUE) %>%
  mutate(dtwr=case_when(
    mean_decided>0 & mean_withdrawn>0 ~ mean_decided/mean_withdrawn,
    mean_withdrawn==0 ~ mean_decided,
    mean_decided==0 ~ 0))

cwalk <- fread(paste(sus20Mar,"KOR/master_wilayah_survei_sosial_ekonomi_nasional_2020_maret__kor_.csv",sep="/")) %>%
  mutate(kab_type=case_when(
    kab>69 ~ "KOTA",
    kab<70 ~ "KAB")) %>%
  mutate(nama_kab=case_when(
    nama_kab=="B A T A M" ~ "BATAM",
    nama_kab=="D U M A I" ~ "DUMAI",
    nama_kab=="S I A K" ~ "SIAK",
    TRUE ~ nama_kab)) %>%
  mutate(kab_fnm=paste(kab_type,nama_kab,sep=" ")) %>%
  rename(prov_nm=nama_prov,
         prov_id=value_prov) %>%
  full_join(cmdu,by=c("kab_fnm","prov_nm","prov_id")) %>%
  mutate(prop=prov_id,
         hdtwr=dtwr>median(dtwr)) %>%
  select(-c(variable_kab,leftover_prev,leftover_next,new,total,withdrawn,decided,
            court_type,court_nm,court_class,court_fnm,high_court))

###########
# MERGING #
###########

# merging
mdata <- left_join(iddata,byhh,by=c("hhid","year"),relationship="many-to-one") %>%
  left_join(hhead,by=c("hhid","year"),relationship="many-to-one") %>%
  left_join(rtdata,by=c("hhid","year"),relationship="many-to-one") %>%
  left_join(kpdata,by=c("hhid","year"),relationship="many-to-one") %>%
  left_join(cwalk,by=c("prop","kab"),relationship="many-to-one") %>%
  apply_labels(year="Year after intervention",
               dtwr="Decide-to-withdraw")

# checking missing values
c(length(which(is.na(mdata$hhead_age))),
  length(which(is.na(mdata$hhead_fem))),
  length(which(is.na(mdata$hhead_formalw))),
  length(which(is.na(mdata$hhead_smp))),
  length(which(is.na(mdata$hhsize))),
  length(which(is.na(mdata$dtwr))))

# save data to csv and dta
fwrite(mdata,file=paste(getwd(),fin,"/analysis.csv",sep=""))

#======================#
# DESCRIPTIVE ANALYSIS #
#======================#

############################
# CHILD MARRIAGE INDONESIA #  
############################
ind <- read.csv(paste(getwd(),dsc,"/wb_gender-stat.csv",sep="")) %>%
  filter(Series.Code!="") %>%
  select(Series.Code,5:68) 
ind[,grep("YR",names(ind))] <- lapply(ind[,grep("YR",names(ind))],as.numeric) 
ind <- ind %>%
  pivot_longer(cols=2:ncol(ind),names_to="year") %>%
  pivot_wider(names_from=Series.Code,values_from=value) %>%
  rename(pctmb15=SP.M15.2024.FE.ZS,pctmb18=SP.M18.2024.FE.ZS) %>%
  select(year,pctmb15,pctmb18) %>%
  filter(!is.na(pctmb18))
for (i in 1960:2023) {
  ind[grep(i,ind$year),"year"] <- as.character(i) 
}
ind <- pivot_longer(ind,cols =2:3)

png(filename=paste(getwd(),gp,"/cmindo.png",sep=""),width=580,height=380)
ggplot(ind, aes(x=year, y=value, group=name)) +
  geom_line(aes(color=name)) +
  geom_point(aes(color=name)) + 
  theme_minimal() +
  theme(legend.position="bottom") +
  labs(y="",x="",
       title="Percentage of women age 20-24 who have married as a child",
       caption="Source: World Bank Gender Statistitcs") + 
  scale_colour_discrete(name="", labels = c("Before 15", "Before 18"))
dev.off()
  
############################
# INTERNATIONAL COMPARISON #  
############################
sdg <- read.csv(paste(getwd(),dsc,"/unsdg.csv",sep="")) %>%
  select(ref_area,ref_area_desc,time_period,obs_value,iso3) %>%
  rename(ccode=ref_area,cname=ref_area_desc,year=time_period,pctmb18=obs_value)
pop <- read.csv(paste(getwd(),dsc,"/WPP2022_Population1JanuaryByAge5GroupSex_Medium.csv",sep="")) %>%
  filter(AgeGrp=="20-24") %>%
  select(LocID,Location,Time,PopFemale) %>%
  rename(ccode=LocID,cname=Location,year=Time)
glbcm <- left_join(sdg,pop,by=c("ccode","year"),relationship="one-to-one") %>%
  mutate(cmbrd=(pctmb18/100)*PopFemale)
earth <- ne_countries(scale = "medium", returnclass = "sf") %>%
  select(name_long,geometry,iso_a3) %>%
  rename(iso3=iso_a3) 
wrldcm <- st_as_sf(full_join(glbcm,earth,by="iso3",relationship="one-to-one"))

png(filename=paste(getwd(),gp,"/cmglb.png",sep=""),width=580,height=380)
ggplot(data = wrldcm) +
  geom_sf(aes(fill =cmbrd)) +
  scale_fill_viridis_c(option = "turbo",name="",labels=label_comma()) +
  labs(title="Number of women age 20-24 who married before 18 years old",
       caption="Source: UN") +
  theme(legend.position="bottom") 
dev.off()
