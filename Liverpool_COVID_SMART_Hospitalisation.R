
rm(list=ls())

library(data.table)
library(ggplot2)
library(ggpmisc)
library(sjstats)
library(MASS)
library(panelr)
library(GGally)
library(psych)
library(geepack)
library(sf)
library(tmap)
library(tmaptools)
library(caret)
library(randomForest)
library(viridis)  
library(RColorBrewer)
library(regclass)
library(biostat3)
library(regclass)
library(haven)
library(readxl)
library(statar)
library(foreign)
library(janitor)
library(stringr)
library(fuzzyjoin)
library(FNN)
library(fingertipsR)
library(splitstackshape)
library(dplyr)
library(tsibble)
library(readODS)
library(ggspatial)
library(microsynth)
library(scales)
library(survey)
library(aweek)  # for the get_aweek function
library(epitools)
library(maptools)
library(cartogram)  # for creating cartograms https://www.r-graph-gallery.com/331-basic-cartogram.html
library(tidyverse)
library(broom) # for the tidy function, which takes the messy output of built-in functions in R, such as lm, nls, or t.test, and turns them into tidy tibbles.
library(mapproj) # for the coord_ma function
library(ggrepel) ## for creating non-overlapping annotation texts with the geom_text_repel function
library(MatchIt)
library(CausalImpact)



`%notin%` <- Negate(`%in%`)


################################### filepath ###################################
a<-"/Volumes/RDM02/ILRR_Store/Safeguarded_Data/PLDR_indicators/COVID19_Admi_MSOA"
#a <- "//rfs02/rdm02/ILRR_Store/Safeguarded_Data/PLDR_indicators/COVID19_Admi_MSOA"
b<-"/Users/benbarr/OneDrive - The University of Liverpool/Documents/Hip-R Projects/HIP_R_005_COVID_ineq"
#b<-"C:/Users/Xingna/OneDrive - The University of Liverpool/HIP_R_005_COVID_ineq"
c<-"/Volumes/Sharing Folder"
#c<-"//bh-fs01/dept01/Ctrcis/Multivariate Modelling Group/Vaccine Analysis/Sharing Folder"

########################### load msoa admissions data data ######################

#  this is so frustrating - if anyone knows how to convert between different date time format I would love to know. 
msoa_adm<-fread(normalizePath(file.path(a,"/U071_U072_Admi_isoweek_MSOA.csv")))

#I think the aweek package is useful here
msoa_adm[isoyear==2021 & isoweek==53, isoyear:=2020]
msoa_adm<-msoa_adm[, list(admi2_cnt=sum(admi2_cnt)), by=.(isoyear, isoweek, msoa11)]
msoa_adm[,char_week:=gsub("-", " ", as.character(trunc(get_aweek(isoweek,isoyear))))]
class(msoa_adm$char_week)
table(msoa_adm$char_week)

setnames(msoa_adm, "isoyear", "year")

summary(msoa_adm$admi2_cnt)

# msoa_adm[year==2021 & isoweek==53, year:=2020]
# msoa_adm<-msoa_adm[, list(admi2_cnt=sum(admi2_cnt)), by=.(year, isoweek, msoa11)]
# plot1<-msoa_adm[, list(admi2_cnt=sum(admi2_cnt)), by=.(year, isoweek)]
# ggplot(data=plot1, aes(y=admi2_cnt, x=isoweek))+geom_line()+facet_grid(year~.)
# msoa_adm[, week:=as.character(isoweek)]
# msoa_adm[nchar(week)==1, week:=paste0("0",week)]
# msoa_adm[, char_week:=paste0(as.character(year)," ","W",week)]

########################### load the updated cases_msoa data ###################

# load(normalizePath("raw_data/msoa/msoa_inf_smart.RData"))
# 

load(normalizePath(file.path(b,"raw_data/msoa/msoa_inf.RData")))

table(cases_msoa$date)


max(cases_msoa$date)
min(cases_msoa$date)

cases_msoa[, char_week:=as.character(week_number)]

table(cases_msoa$char_week)
class(cases_msoa$char_week)

cases_msoa<-merge(cases_msoa,msoa_adm[, .(msoa11,char_week,admi2_cnt)], 
                  by.x=c("msoa11_cd", "char_week"),by.y=c("msoa11", "char_week"), all.x=T)

cases_msoa[is.na(admi2_cnt)==T,admi2_cnt:=0 ]
summary(cases_msoa$admi2_cnt)


#  tiers introduced - "2020-12-04"
# time window to focus on 5 weeks before intervention start and 5 weeks after intervention finished 
tier_date<-as.Date("2020-12-07")
yearweek("2020-12-04")
get_date(13,2021)
#"2021-03-29"
get_date(13,2021,7)
#"2021-04-04"


test0<-cases_msoa[date>=as.Date("2020-10-01") & week_number<=yearweek("2021 W13")]
max(test0$date)
as.Date(yearweek("2021 W12"))
#  identify intervention group 
test0[, intervention:=as.numeric(ladnm=="Liverpool")]
test0[, after:=as.numeric(week_number>yearweek(as.Date("2020-11-13")))]
test0[, id:=as.numeric(as.factor(msoa11_cd))]

table(test0$intervention)


test0[, adm_rate:=admi2_cnt*100000/total_pop]

yearweek(tier_date)

#  identify those who entered tier3 on "2020-12-07"
test0[, tier3:=max(as.numeric(tier=="3" & week_number==yearweek(tier_date))), 
      by=.(msoa11_cd)]
with(test0, table(tier,week_number,  useNA = "ifany"))

with(test0[tier3==1 & week_number==yearweek(tier_date)], table(ladnm))
#  identify those who entered tier2.
test0[, tier2:=max(as.numeric(tier=="2" & week_number==yearweek(tier_date))), 
      by=.(msoa11_cd)]
table(test0$tier2, useNA = "ifany")


test0[, st_week:=as.numeric((week_number-yearweek(tier_date)))]
table(test0$st_week)

# exclude places that were never in tier two or three
test0<-test0[tier2==1|tier3==1,]


test0[, prop_sgtf:=mean(percent_sgtf_cases, na.rm = T), by=.(msoa11_cd)]
test0[, prop_sgtf:=mean(prop_sgtf, na.rm = T), by=.(msoa11_cd)]
summary(test0$prop_sgtf)


# synthetic cohort analysis
# need a balanced panel


table(test0$st_week, useNA = "ifany")



test0[, max_wave:=max(st_week), by=.(msoa11_cd)]
test0[, min_wave:=min(st_week), by=.(msoa11_cd)]
table(test0$max_wave, useNA = "ifany")
table(test0$min_wave, useNA = "ifany")


test0[, num_wave:=.N, by=.(msoa11_cd)]
table(test0$num_wave)
table(test0$st_week)


test0[, id:=as.numeric(as.factor(msoa11_cd))]
test0[, time:=as.numeric(as.factor(st_week))]

table(test0[st_week==0]$time)
table(test0$time)

test0[, regioncdn:=as.numeric(as.factor(regionname))]
table(test0$regionname)
table(test0$regioncdn)

# generate the counts of week within an intervention
class(test0$tier)
class(test0$tier3)
table(test0$tier3)
test0[st_week>=0, week_tier3:=sum(as.numeric(tier=="3")),by = .(msoa11_cd)]
test0[st_week>=0, week_tier4:=sum(as.numeric(tier=="4")),by = .(msoa11_cd)]
table(test0$week_tier3)
table(test0$week_tier4)

# this will be missing for st_week<0 unless you do this
test0[, week_tier3:=max(week_tier3, na.rm = T),by = .(msoa11_cd)]
test0[, week_tier4:=max(week_tier4, na.rm = T),by = .(msoa11_cd)]
class(test0$tier2)
table(test0$tier2)

# average test rate before intervention
test0[date<as.Date("2020-11-06"), av_test:=mean(test_rate), by = .(msoa11_cd)]
summary(test0$av_test)
test0[, av_test:=max(av_test, na.rm = T), by = .(msoa11_cd)]
summary(test0$av_test)
#  missing and had to be imputed
prop.table(table(test0$miss_msoa_flag))

test0[, case_rate:=cases_imp1*100000/total_pop]
summary(test0$case_rate)
# ggplot(data=test, aes(x=st_week, y=case_rate, group=msoa11_cd,  
#                                color=as.factor(tier))) +geom_line(size=1)


test0[, after_tier2:=week_number>=yearweek(as.Date("2020-12-07"))]
with(test0, table(time, after_tier2))
test0[, other_lcr:=(ladnm=="Halton"| ladnm=="Sefton"|ladnm=="Knowsley"|
                      ladnm=="St. Helens"|ladnm=="Wirral")]

test0[is.na(lfd_test_la)==T, lfd_test_la:=0]
test0[, lft_test_rate:=lfd_test_la*100/an_pop]
test0[date>as.Date("2020-11-05") & date<as.Date("2020-12-31"), 
      meanlftrate:=mean(lft_test_rate,na.rm=T), by=.(msoa11_cd)]
test0[, meanlftrate:=max(meanlftrate,na.rm=T), by=.(msoa11_cd)]

summary(test0$prop_sgtf)

####################################### sea2 ###################################


# cov.var2 <- c("total_pop", "prop_70plus", "prop_7_11", "prop_bame", "pop_dens", 
#               "prop_students", "ch_prop", "imd_score", "prop_sgtf","av_test")

cov.var2 <- c("total_pop", "prop_bame", "pop_dens","imd_score", "prop_70plus",
              "as_prop_vulner","prop_sgtf","prop_7_11","prop_students","av_test")
match.out2 <- c("admi2_cnt")


end.post=max(test0$time)
max(test0$date)
min(test0$date)

min(test0[week_number==yearweek(tier_date)]$time)

table(test0[time==1]$week_number)
get_date(40,2020,1)
get_date(40,2020,7)
# time from which would expect tiers to affect hospital admissions 
end.pre=12
table(test0[time==12]$date)
table(test0[time==12]$week_number)
get_date(51,2021,1)
get_date(51,2021,7)

table(test0[time==11]$week_number)
get_date(50,2021,1)
get_date(50,2021,7)
table(test0[time==10]$week_number)
get_date(49,2021,1)
get_date(49,2021,7)

table(test0[time==21]$date)
table(test0[time==1]$date)
table(test0[time==21]$week_number)
end.post=21
get_date(7,2021,1)
get_date(7,2021,7)

table(test0$time)
table(test0[intervention==0 & meanlftrate<1 & other_lcr==0]$ladnm)


cov.var2 <- c("total_pop", "prop_70plus", "prop_bame", "pop_dens", "as_prop_vulner", "imd_score","av_test","prop_sgtf")
set.seed(6768)
## by selecting intervention==0, ladnm=="Liverpool" MSOAs were excluded.
sea2 <- microsynth(as.data.frame(test0[intervention==0 & meanlftrate<1 & other_lcr==0]), 
                   idvar="id", timevar="time", intvar="tier3", 
                   start.pre=1, end.pre=end.pre, end.post=end.post, 
                   match.out=match.out2, match.covar=cov.var2, 
                   result.var=match.out2 , 
                   omnibus.var=match.out2,
                   test="twosided",
                   confidence = 0.95,
                   perm=250,
                   jack=F, 
                   n.cores = 1)

summary(sea2)
plot_microsynth(sea2)

#  add weights into main dataset. 
weights2<-cbind(ID=row.names(sea2$w$Weights),as.data.frame(sea2$w$Weights),
                row.names=NULL)
weights2<-cbind(weights2,interv2=as.data.frame(sea2$w$Intervention), 
                row.names=NULL)
colnames(weights2)[1]<-"id"
weights2$id<-as.numeric(as.character(weights2$id))
summary(weights2$Main)

test0$Main<-NULL
test0<-as.data.table(merge(test0,weights2[,1:2], by="id", 
                           all.x=T ))
table(test0$after_tier2, useNA = "ifany")
summary(test0$Main)


########################### Plotting out the tiers effect #######################

svyfull<-svydesign(ids=~msoa11_cd, weights=~Main, 
                   data=test0[is.na(Main)==F])

prop0<-cbind(svyby(~admi2_cnt, denominator=~total_pop, by=~tier3+date,
                   design=svyfull,FUN=svyratio),
             confint(svyby(~admi2_cnt, denominator=~total_pop, by=~tier3+date,
                           design=svyfull,FUN=svyratio))[,2],
             confint(svyby(~admi2_cnt, denominator=~total_pop, by=~tier3+date,
                           design=svyfull,FUN=svyratio))[,1])


names(prop0)<-c("tier3", "date","rate", "se_rate","lcl", "ucl")

text_plot <- data.frame(text = c("Tier 3 begins"), 
                        dates = as.Date(c("2020-12-04")), 
                        stringsAsFactors = FALSE)

ggplot(data=prop0, aes(x = as.Date(date), y = rate*100000, 
                       color=as.factor(tier3)))+ 
  geom_line(aes(lty=as.factor(tier3)), size=1)+
  geom_ribbon(aes(fill=as.factor(tier3), ymin=lcl*100000, ymax=ucl*100000),
              size=0, alpha=0.3)+
  # scale_colour_viridis_d(name="Group", 
  #                        labels=c( "Tier 2 Synthetic Control", "Tier 3"))+
  # scale_fill_viridis_d(guide="none")+
  scale_colour_manual(values = c("#440154FF","#FDE725FF"),labels=c( "Tier 2 Synthetic Control", "Tier 3")) +
  scale_linetype_manual(values = c("solid","dashed"),labels=c( "Tier 2 Synthetic Control", "Tier 3")) +
  scale_fill_manual(values = c("#440154FF","#FDE725FF"),labels=c( "Tier 2 Synthetic Control", "Tier 3")) +
  scale_x_date(date_breaks ="weeks" )+
  geom_vline(mapping = aes(xintercept = dates), data = text_plot, 
             show.legend = F, linetype = "dotted") +
  geom_text(aes(x=dates-10, label=text, y=40), data = text_plot, 
            colour="black", size=6, fontface = "bold.italic") +
  xlab("Week start date") + 
  ylab("Admissions per 100,000 population")+
  theme_classic()+
  theme(text = element_text(size=16),legend.position="bottom", 
        legend.box = "horizontal", legend.text = element_text(size=24),
        legend.title = element_blank(),legend.key.size = unit(1, "cm"), 
        axis.text.x = element_text(angle = 45,hjust=1))+ 
  theme(strip.background = element_blank())



ggsave("tier3_admission_synth.png",path=normalizePath(file.path(b,"papers/SMART_hospitalisation/BMJ_submission")),
       width = 16, height = 8, units="in",dpi=600)
ggsave("tier3_admission_synth.svg",path=normalizePath(file.path(b,"papers/SMART_hospitalisation/BMJ_submission")),
       width = 16, height = 8, units="in",dpi=600)
################# Calculate the coefficients for tier 3 effect #################

svyfull<-svydesign(ids=~msoa11_cd, weights=~Main, 
                   data=test0[time>end.pre & time <=end.post & is.na(Main)==F])

mod1 <- survey::svyglm(admi2_cnt ~ tier3+as.factor(time)+offset(log(total_pop)), 
                       design=svyfull, family = poisson(link="log"))
1-lincom(mod1, c("tier3"), eform=T)[1]$Estimate
summary(mod1)


inc_1=lincom(mod1, c("tier3"), eform=T)[1]$Estimate

inc_lcl=lincom(mod1, c("tier3"), eform=T)[2]$`2.5 %`

inc_ucl=lincom(mod1, c("tier3"), eform=T)[3]$`97.5 %`


mod2 <- survey::svyglm(admi2_cnt ~ tier3*as.factor(time)+offset(log(total_pop)), 
                       design=svyfull, family = poisson(link="log"))

summary(mod2)


coef_list<-list()
dt<-data.table(lincom(mod2, c("tier3"), eform=T)[1]$Estimate)
dt$i<-end.pre+1
coef_list[[1]]<-dt

j=1
for (i in  (end.pre+2):end.post){
  j=j+1
  dt<-data.table(lincom(mod2, c(paste0("tier3 + tier3:as.factor(time)", i)), eform=T)[1]$Estimate)
  dt$i<-i
  coef_list[[j]]<-dt
}

coefs<-rbindlist(coef_list)
coefs[, week_number:=min(test0[time==1]$week_number)+i-1]
coefs<-coefs[, .(week_number,inc_2=V1)]

#  lower cis 

lcl_list<-list()
dt<-data.table(lincom(mod1, c("tier3"), eform=T)[2]$`2.5 %`)
dt$i<-end.pre+1
lcl_list[[1]]<-dt

j=1
for (i in  (end.pre+2):end.post){
  j=j+1
  dt<-data.table(lincom(mod2, c(paste0("tier3 + tier3:as.factor(time)", i)), eform=T)[2]$`2.5 %`)
  dt$i<-i
  lcl_list[[j]]<-dt
}

lcls<-rbindlist(lcl_list)


lcls[, week_number:=min(test0[time==1]$week_number)+i-1]
lcls<-lcls[, .(week_number,inc_lcl2=V1)]


ucl_list<-list()
dt<-data.table(lincom(mod1, c("tier3"), eform=T)[3]$`97.5 %`)
dt$i<-end.pre+1
ucl_list[[1]]<-dt

j=1
for (i in  (end.pre+2):end.post){
  j=j+1
  dt<-data.table(lincom(mod2, c(paste0("tier3 + tier3:as.factor(time)", i)), eform=T)[3]$`97.5 %`)
  dt$i<-i
  ucl_list[[j]]<-dt
}

ucls<-rbindlist(ucl_list)

ucls[, week_number:=min(test0[time==1]$week_number)+i-1]
ucls<-ucls[, .(week_number,inc_ucl2=V1)]

coefs<-merge(coefs,lcls, by="week_number")
coefs<-merge(coefs,ucls, by="week_number")


############################ Subset by meanlftrate<1  ##########################
# synthetic control analysis SMART testing in Liverpool
test0[, st_week:=week_number-yearweek(as.Date("2020-11-20"))]

yearweek(as.Date("2020-11-20"))
as.Date(yearweek("2020 W48"))

test<-test0[date>=as.Date("2020-10-01") & week_number<=yearweek("2021 W20") & 
              (intervention==1|(meanlftrate<1 & other_lcr==0))]


nrow(unique(test[intervention==0, .(msoa11_cd)]))

nrow(unique(test0[meanlftrate<1, .(msoa11_cd)]))
nrow(unique(test0[, .(msoa11_cd)]))-nrow(unique(test0[(other_lcr==0|intervention==1), .(msoa11_cd)]))

nrow(unique(test0[, .(msoa11_cd)]))-nrow(unique(test0[meanlftrate<1 , .(msoa11_cd)]))

nrow(unique(test[, .(msoa11_cd)]))-nrow(unique(test0[meanlftrate<1 & (other_lcr==0|intervention==1), .(msoa11_cd)]))


prop.table(table(test$miss_msoa_flag))

with(test, table(intervention, time))
with(test, table(intervention, date))

test[, inc_2:=NULL]
test[, inc_lcl2:=NULL]
test[, inc_ucl2:=NULL]
test<-merge(test,coefs, by="week_number", all.x = T)


test[, admi2_cnt_adj2:=as.numeric(admi2_cnt)]
# test[tier2==0 & after_tier2==T & time<=20, admi2_cnt_adj2:=admi2_cnt/inc_1]

table(test$tier3, test$tier2, useNA = "ifany")
test[tier3==1 & is.na(inc_2)==F, admi2_cnt_adj2:=admi2_cnt_adj2/inc_2]

# check<-test[tier3==1 & is.na(inc_2)==F, .(tier3,admi2_cnt_adj2,admi2_cnt,week_number,inc_2)]


with(test, table(tier3, intervention))
test[, admi2_cnt_adj2_lcl:=as.numeric(admi2_cnt)]

test[tier3==1 &  is.na(inc_2)==F, admi2_cnt_adj2_lcl:=admi2_cnt_adj2_lcl/inc_lcl2]
test[, admi2_cnt_adj2_ucl:=as.numeric(admi2_cnt)]
test[tier3==1  & is.na(inc_2)==F, admi2_cnt_adj2_ucl:=admi2_cnt_adj2_ucl/inc_ucl2]

table(test$tier2,test$tier2 )
min(test[tier2==1 & after_tier2==T]$date)
max(test[tier2==1 & after_tier2==T]$date)

end.post=max(test$time)

max(as.Date(test[time==8]$week_number))

max(as.Date(test[time==end.post]$week_number))

# MSOA hospital admissions analysis  - no adjustment for tier 2
test[is.na(admi2_cnt)==T, admi2_cnt:=0]
table(test[is.na(admi2_cnt)==T]$time)
table(msoa_adm$char_week)

table(test[time==8]$date)
table(test[time==8]$week_number)
get_date(47,2020)
get_date(47,2020,7)


table(test[time==14]$date)
table(test[time==14]$week_number)
get_date(53,2020)
get_date(53,2020,7)

table(test[time==12]$date)
table(test[time==12]$week_number)
get_date(51,2020)
get_date(51,2020,7)

table(test[time==16]$date)
table(test[time==16]$week_number)
get_date(2,2021)
get_date(2,2021,7)


cov.var <- c("total_pop", "prop_70plus", "prop_bame", "pop_dens", "as_prop_vulner", "imd_score","av_test")
############################## Model 1 - sea1 ##################################
### Intervention Period;	Periods in the model
### 6/11/20 - 3/12/20;	19/11/20 - 16/12/20


table(test[time==7]$date)
table(test[time==7]$week_number)
get_date(46,2020)
get_date(46,2020,7)

table(test[time==12]$date)
table(test[time==12]$week_number)
get_date(51,2020)
get_date(51,2020,7)

set.seed(6768)
sea1 <- microsynth(as.data.frame(test),
                   idvar="id", timevar="time", intvar="intervention",
                   start.pre=1, end.pre=7, end.post=12,
                   match.out=c("admi2_cnt"), match.covar=cov.var,
                   result.var=c("admi2_cnt"), omnibus.var=F,
                   test="twosided",
                   check.feas=TRUE,
                   confidence=0.95,
                   perm=250,
                   jack=F,
                   n.cores = 1)

sea1
summary(sea1)
par(mar=c(1,1,1,1))
plot_microsynth(sea1)


######## Model 2 - assumes mean effect of tiered restrictions - sea1_long_adj2 ####
### Intervention Period;	Periods in the model
### 6/11/20 - 2/1/21;	19/11/20 - 15/1/21


table(test[time==16]$date)
table(test[time==16]$week_number)
get_date(2,2021)
get_date(2,2021,7)

set.seed(6768)
sea1_long_adj2 <- microsynth(as.data.frame(test),
                             idvar="id", timevar="time", intvar="intervention",
                             start.pre=1, end.pre=7, end.post=16,
                             match.out=c("admi2_cnt_adj2"), match.covar=cov.var,
                             result.var=c("admi2_cnt_adj2"), omnibus.var=F,
                             test="twosided",
                             check.feas=TRUE,
                             confidence=0.95,
                             perm=250,
                             jack=F,
                             # result="smart1",
                             n.cores = 1)

sea1_long_adj2 
summary(sea1_long_adj2 )
plot_microsynth(sea1_long_adj2)

######## Model 3 - assumes no effect of tiered restrictions - sea1_long ####

## Intervention Period;	Periods in the model; unadjusted model;
## 6/11/20 - 2/1/21;	19/11/20 - 15/1/21
table(test[time==7]$date)
table(test[time==7]$week_number)
get_date(46,2021)
get_date(46,2021,7)

table(test[time==16]$date)
table(test[time==16]$week_number)
get_date(2,2021)
get_date(2,2021,7)


set.seed(6768)
sea1_long <- microsynth(as.data.frame(test),
                        idvar="id", timevar="time", intvar="intervention",
                        start.pre=1, end.pre=7, end.post=16,
                        match.out=c("admi2_cnt"), match.covar=cov.var,
                        result.var=c("admi2_cnt"), omnibus.var=F,
                        test="twosided",
                        check.feas=TRUE,
                        confidence=0.95,
                        perm=250,
                        jack=F,
                        # result="smart1",
                        n.cores = 1)
sea1_long 
summary(sea1_long)
plot_microsynth(sea1_long)


# result
summary(sea1_long )
sea1_long$Results$`16`[,3]*100
sea1_long$Results$`16`[,8]*100
sea1_long$Results$`16`[,9]*100


#  add weights into main dataset - long
weights_est<-as.data.table(cbind(id=row.names(sea1$w$Weights),
                                 as.data.frame(sea1$w$Weights),
                                 row.names=NULL))
weights_est<-weights_est[, .(id=as.numeric(as.character(id)),wt_est=Main)]


test$Main<-NULL
test$wt_est<-NULL
test<-as.data.table(merge(test,weights_est, by="id", 
                          all.x=T ))

table(test[time==8 & wt_est>0]$intervention)


########################### Plotting out the lft rate #######################

# Figure 1 estimate of the LFT rate
plot_lft<-unique(test0[date<as.Date("2021-01-03"), 
                       .(ladcd, lad19_nm, date,lfd_test_la, an_pop, intervention)])


plot_lft[, lftrate:=lfd_test_la*100/an_pop, by=.(ladcd, date)]

plot_lft[date>as.Date("2020-11-05") & date<=as.Date("2021-01-02"), 
         meanlftrate:=max(lftrate, na.rm = T), by=.(ladcd)]

sum(plot_lft[date<as.Date("2021-01-03") & intervention==1, lfd_test_la])
sum(plot_lft[date>as.Date("2020-11-05") & date<=as.Date("2021-01-02") & intervention==1, lfd_test_la])

sum(plot_lft[date<as.Date("2021-01-03") & intervention==1, meanlftrate])
sum(plot_lft[date>as.Date("2020-11-05") & date<=as.Date("2021-01-02") & intervention==1, meanlftrate])

# plot_lft[date>as.Date("2020-11-05") & date<as.Date("2020-12-31"), meanlftrate:=sum(lfd_test_la)*100/an_pop, by=.(ladcd)]
# 

ggplot(plot_lft[date<=as.Date("2021-01-02")],aes(y=lftrate, x=date, 
                                                 group=ladcd, 
                                                 color=as.factor(intervention)))+
  geom_line()

ggplot(plot_lft[date<=as.Date("2021-01-02") & (meanlftrate<1|intervention==1)],
       aes(y=lftrate, x=date, group=ladcd, color=as.factor(intervention)))+
  geom_line()

check_dist<-unique(plot_lft[date>as.Date("2020-11-05") & date<=as.Date("2021-01-02")&is.na(meanlftrate)==F, 
                            .(meanlftrate,ladcd,intervention)])
table(check_dist$intervention)

check_dist[intervention==1]$meanlftrate

quantile(na.omit(check_dist$meanlftrate), probs = 0.84)
quantile(na.omit(check_dist$meanlftrate), probs = 0.8673)

ggplot(check_dist,aes(meanlftrate))+
  geom_histogram(binwidth = 0.1,fill = "white",colour="black") +
  geom_vline(aes(xintercept = 1), linetype = "longdash")+
  # geom_text(x=c(1.5,4), y=c(60,60), label=c("Included: n= 6290","Excluded: n= 142")) +
  theme_classic()+
  theme(axis.title=element_text(colour="black",size=16,face="bold"),
        axis.text=element_text(colour="black",size=14))+
  xlab("Mean LFT testing rates (%)") + 
  ylab("Numbers of Local Authorities") +
  ylim(0,70) +
  annotate(geom="text", x=c(0.2,2,4), y=c(65,65,65), 
           label=c("Synthetic control\nincluded:\nn = 6290",
                   "Synthetic control\nexcluded:\nn = 142",
                   "Intervention\nincluded:\nn = 61"),size=6)



ggsave("mean_lft_rate.png",path=normalizePath(file.path(b,"papers/SMART_hospitalisation/BMJ_submission")),
       width = 16, height = 8, units="in",dpi=600)
ggsave("mean_lft_rate.svg",path=normalizePath(file.path(b,"papers/SMART_hospitalisation/BMJ_submission")),
       width = 16, height = 8, units="in",dpi=600)


plot_lft<-plot_lft[, list(lft=sum(lfd_test_la),pop=sum(an_pop)), 
                   by=.(date,intervention)]

plot_lft<-plot_lft[, rate:=lft*100/pop,  by=.(date,intervention)]

plot_lft<-plot_lft[, ucl:=100*pois.exact(lft,pop)[5],  by=.(date,intervention)]

plot_lft<-plot_lft[, lcl:=100*pois.exact(lft,pop)[4],  by=.(date,intervention)]



ggplot(data=plot_lft[date<=as.Date("2021-01-13")], aes(x = as.Date(date), y = rate, 
                                                             color=as.factor(intervention)))+ 
  geom_line(aes(lty=as.factor(intervention)), size=1)+
  geom_ribbon(aes(fill=as.factor(intervention), ymin=lcl, ymax=ucl),size=0, alpha=0.3)+
  # scale_colour_viridis_d(name="Group", labels=c( "Rest of England", "Liverpool"))+
  # scale_fill_viridis_d(guide="none")+
  # scale_x_date(date_breaks ="weeks" )+
  scale_colour_manual(values = c("#440154FF","#FDE725FF"), labels=c("Rest of England", "Liverpool")) +
  scale_x_date(date_breaks ="weeks" )+
  scale_linetype_manual(values = c("solid","dashed"), labels=c("Rest of England", "Liverpool")) +
  scale_fill_manual(values = c("#440154FF","#FDE725FF"), labels=c("Rest of England", "Liverpool")) +
  scale_linetype(guide="none")+
  geom_vline(xintercept = as.Date("2020-11-06"), show.legend = FALSE, linetype = "dotted") +
  xlab("Week end date") + ylab("LFT tests per 100 population")+
  theme_classic()+
  theme(text = element_text(size=16),legend.position="bottom", legend.box = "horizontal", legend.text = element_text(size=24),
        legend.title = element_blank(),legend.key.size = unit(1, "cm"), 
        axis.text.x = element_text(angle = 45,hjust=1))+ theme(strip.background = element_blank())


ggsave("lft_trend.png",path=normalizePath(file.path(b,"papers/SMART_hospitalisation/BMJ_submission")),
       width = 16, height = 8, units="in",dpi=600)
ggsave("lft_trend.svg",path=normalizePath(file.path(b,"papers/SMART_hospitalisation/BMJ_submission")),
       width = 16, height = 8, units="in",dpi=600)

########################### Plotting out the main result #######################

svyfull_est<-svydesign(ids=~msoa11_cd, weights=~wt_est, data=test[is.na(wt_est)==F])


prop1<-cbind(svyby(~admi2_cnt_adj2, denominator=~total_pop, by=~intervention+date,
                   design=svyfull_est,FUN=svyratio),
             confint(svyby(~admi2_cnt_adj2, denominator=~total_pop, by=~intervention+date,
                           design=svyfull_est,FUN=svyratio))[,2],
             confint(svyby(~admi2_cnt_adj2, denominator=~total_pop, by=~intervention+date,
                           design=svyfull_est,FUN=svyratio))[,1])
# 
# check<-test[is.na(wt_est)==F, list(count=sum(admi2_cnt_adj2*wt_est)/sum(wt_est), pop=sum(total_pop*wt_est)/(sum(wt_est))), by=.(intervention,date)]
# 
# data=test[is.na(wt_est)==F & intervention==0 & date==as.Date("2021-01-24"), .(msoa11_cd,wt_est,admi2_cnt_adj2,total_pop) ]
# 
# check1<-svydesign(ids=~msoa11_cd, weights=~wt_est, data=data)
# 
# confint(svyratio(numerator=~admi2_cnt_adj2, denominator=~total_pop,  design=check1))
# 

names(prop1)<-c("intervention", "date","rate", "se_rate","lcl", "ucl")

text_plot <- data.frame(text = c("Liverpool\ntesting\npilot\nbegins\nin high\nlevel of\nrestrictions",
                                 "Liverpool\nin lower\nrestrictions\nwith lower\nintensity of\ntesting",
                                 "National roll-out of\ncommunity testing,\nfirst during lockdown"), 
                        dates = as.Date(c("2020-11-06","2020-12-03","2021-01-02")), 
                        stringsAsFactors = FALSE)

ggplot(data=prop1, aes(x = as.Date(date), y = rate*100000, 
                              color=as.factor(intervention)))+ 
  geom_line(aes(lty=as.factor(intervention)), size=1)+
  geom_ribbon(aes(fill=as.factor(intervention), ymin=lcl*100000, ymax=ucl*100000),size=0, alpha=0.3)+
  #scale_colour_viridis_d(name="Group", labels=c("Synthetic Control", "Liverpool"))+
  scale_colour_manual(values = c("#440154FF","#FDE725FF"), labels=c("Synthetic Control", "Liverpool")) +
  #scale_fill_viridis_d(guide="none")+
  scale_x_date(date_breaks ="weeks" )+
  scale_linetype_manual(values = c("solid","dashed"), labels=c("Synthetic Control", "Liverpool")) +
  scale_fill_manual(values = c("#440154FF","#FDE725FF"), labels=c("Synthetic Control", "Liverpool")) +
  geom_vline(mapping = aes(xintercept = dates), data = text_plot, show.legend = FALSE, linetype = "dotted") +
  geom_text(aes(x=c(dates[1]+2,dates[2]+4,dates[3]+2), 
                label=text, y=c(80,80,90)), data = text_plot, 
            colour="black", size=6, fontface = "bold.italic",hjust=c(0,0,0)) +
  ylim(c(-5,100)) +
  xlab("Week start date") + ylab("Admissions per 100,000 population")+
  theme_classic()+
  theme(text = element_text(size=16),legend.position="bottom", legend.box = "horizontal", legend.text = element_text(size=24),
        legend.title = element_blank(),legend.key.size = unit(1, "cm"), 
        axis.text.x = element_text(angle = 45,hjust=1))+ theme(strip.background = element_blank())

ggsave("fin_model1.png",path=normalizePath(file.path(b,"papers/SMART_hospitalisation/BMJ_submission")),
       width = 16, height = 8, units="in",dpi=600)
ggsave("fin_model1.svg",path=normalizePath(file.path(b,"papers/SMART_hospitalisation/BMJ_submission")),
       width = 16, height = 8, units="in",dpi=600)


text_plot <- data.frame(text = c("Liverpool testing\npilot begins in \nhigh level of\nrestrictions",
                                 "Liverpool in lower\nrestrictions with lower\nintensity of testing",
                                 "National roll-out of\ncommunity testing,\nfirst during lockdown"), 
                        dates = as.Date(c("2020-11-06","2020-12-03","2021-01-02")), 
                        stringsAsFactors = FALSE)

# ggplot(data=prop1[prop1$date<=as.Date("2021-01-17"),], 
#        aes(x = as.Date(date), y = rate*100000, color=as.factor(intervention)))+ 
#   geom_line(aes(lty=as.factor(intervention)), size=1)+
#   geom_ribbon(aes(fill=as.factor(intervention), ymin=lcl*100000, ymax=ucl*100000),size=0, alpha=0.3)+
#   scale_colour_viridis_d(name="Group", labels=c( "Synthetic Control", "Liverpool"))+
#   scale_fill_viridis_d(guide="none", labels=c( "Synthetic Control", "Liverpool"))+
#   scale_x_date(date_breaks ="weeks")+
#   scale_linetype(guide="none", labels=c( "Synthetic Control", "Liverpool"))+
#   geom_vline(mapping = aes(xintercept = dates), data = text_plot, show.legend = FALSE, linetype = "dotted") +
#   geom_text(aes(x=c(dates[1]+2,dates[2]+4,dates[3]+2), 
#                 label=text, y=c(60,60,60)), data = text_plot, 
#             colour="black", size=6, fontface = "bold.italic",hjust=c(0,0,0)) +
#   ylim(c(0,65)) +
#   xlab("Week start date") + ylab("Admissions per 100,000 population")+
#   theme_classic()+
#   theme(text = element_text(size=16),legend.position="bottom", legend.box = "horizontal", legend.text = element_text(size=24),
#         legend.title = element_blank(),legend.key.size = unit(1, "cm"), 
#         axis.text.x = element_text(angle = 45,hjust=1))+ 
#   theme(strip.background = element_blank())
ggplot(data=prop1[prop1$date<=as.Date("2021-01-17"),], 
       aes(x = as.Date(date), y = rate*100000, color=as.factor(intervention)))+ 
  geom_line(aes(lty=as.factor(intervention)), size=1)+
  geom_ribbon(aes(fill=as.factor(intervention), ymin=lcl*100000, ymax=ucl*100000),size=0, alpha=0.3)+
  #scale_colour_viridis_d(name="Group", labels=c("Synthetic Control", "Liverpool"))+
  scale_colour_manual(values = c("#440154FF","#FDE725FF"), labels=c("Synthetic Control", "Liverpool")) +
  #scale_fill_viridis_d(guide="none")+
  scale_x_date(date_breaks ="weeks" )+
  scale_linetype_manual(values = c("solid","dashed"), labels=c("Synthetic Control", "Liverpool")) +
  scale_fill_manual(values = c("#440154FF","#FDE725FF"), labels=c("Synthetic Control", "Liverpool")) +
  geom_vline(mapping = aes(xintercept = dates), data = text_plot, show.legend = FALSE, linetype = "dotted") +
  geom_text(aes(x=c(dates[1]+2,dates[2]+4,dates[3]+2),
                label=text, y=c(60,60,60)), data = text_plot,
            colour="black", size=6, fontface = "bold.italic",hjust=c(0,0,0)) +
  ylim(c(0,65)) +
  xlab("Week start date") + ylab("Admissions per 100,000 population")+
  theme_classic()+
  theme(text = element_text(size=16),legend.position="bottom", legend.box = "horizontal", 
        legend.text = element_text(size=24),
        legend.title = element_blank(),legend.key.size = unit(1, "cm"), 
        axis.text.x = element_text(angle = 45,hjust=1))+ theme(strip.background = element_blank())

# ggsave("model23.png",p1a,path=normalizePath(file.path(b,"papers/SMART_hospitalisation")),
#        width = 16, height = 8, units="in",dpi=600)
# ggsave("model23.svg",p1a,path=normalizePath(file.path(b,"papers/SMART_hospitalisation")),
#        width = 16, height = 8, units="in",dpi=600)

# ggsave("msoa_synth.svg",p1a,path=normalizePath("./SMART_evaluation/report/final_report"),width = 16, height = 12, units="in",dpi=600)
#Nina's plot
ggsave("fin_model1b.png",path=normalizePath(file.path(b,"papers/SMART_hospitalisation/BMJ_submission")),
       width = 16, height = 8, units="in",dpi=600)
ggsave("fin_model1b.svg",path=normalizePath(file.path(b,"papers/SMART_hospitalisation/BMJ_submission")),
       width = 16, height = 8, units="in",dpi=600)

############################ Plotting out the study areas ######################
# load LAD boundaries that are downloaded from https://geoportal.statistics.gov.uk/datasets/local-authority-districts-december-2017-boundaries-gb-bfc?geometry=-35.772%2C51.103%2C30.981%2C59.783
lad <- read_sf(dsn = normalizePath(file.path(b,"raw_data/lad_shp/Local_Authority_Districts__December_2017__Boundaries_GB_BFC.shp")))
lad <- lad[substr(lad$LAD17CD, 1,1)=="E",]
lad <- st_transform(lad, 4326)

city <- read_sf(dsn = normalizePath(file.path(b,"raw_data/Major_Towns_and_Cities_(December_2015)_Boundaries_V2/Major_Towns_and_Cities_(December_2015)_Boundaries_V2.shp")))
plot(city)
NROW(unique(test$utlaname))
table(test$utlaname)
city <- city[city$TCITY15NM %in% c("London","Liverpool","Manchester","Birmingham",
                                   "Newcastle upon Tyne","Bristol","Leeds","Nottingham"),]
city <- st_transform(city, 4326)


# load MSOA boundaries
sp_msoa <- st_read(normalizePath(file.path(b,"raw_data/msoa/msoa_geo.shp")))
sp_msoa <- sp_msoa[sp_msoa$msoa11cd %in% test$msoa11_cd,]
st_crs(sp_msoa) = 27700

table(test$intervention)
adj2_msoa <- merge(sp_msoa,test[st_week==0,
                                .(st_week,time,intervention,tier3,msoa11_cd,admi2_cnt_adj2,wt_est)],
                   by.x="msoa11cd", by.y="msoa11_cd", all.x=T)
table(adj2_msoa$tier3)
class(adj2_msoa$tier3)

table(adj2_msoa$intervention)
class(adj2_msoa$intervention)

summary(adj2_msoa$wt_est)

#plotting the study areas of the main result

ggplot() +
  geom_sf(data = adj2_msoa,aes(fill = as.factor(intervention)),
          colour = NA,
          lwd = 0) +
  scale_fill_viridis_d(alpha = 1,
                       breaks=c(0,1),
                       labels=c("Synthetic Control", "Intervention"),
                       na.value = "white") +
  geom_sf(data = lad,aes(fill = NA), na.rm = F,alpha=0,size = 0.1) +
  geom_sf(data = city,aes(fill = NA), na.rm = F,alpha=0,size = 0.1) +
  guides(fill = guide_legend(title = "")) +
  theme_void() +
  theme(legend.text=element_text(size=14))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

ggsave(normalizePath(file.path(b,"papers/SMART_hospitalisation/adj2_areas.png")), width = 16, height = 14,device="png",
       units = "in", dpi=700)
ggsave(normalizePath(file.path(b,"papers/SMART_hospitalisation/adj2_areas.svg")), width = 16, height = 14,device="svg",
       units = "in", dpi=700)


#plotting the weights of the main result
show_col(viridis_pal(option = "viridis")(14))
viridis(14)[5:12]

ggplot() +
  geom_sf(data = lad,aes(fill = NA), na.rm = F,alpha=0,size = 0.1) +
  geom_sf(data = adj2_msoa[adj2_msoa$wt_est!=0 & adj2_msoa$intervention!=1,],
          aes(geometry = geometry, fill = as.numeric(wt_est)),
          colour = NA,
          alpha = 1,
          lwd = 0) +
  # scale_fill_continuous(trans = "sqrt",
  #                       type = "viridis", 
  #                       direction = -1,
  #                       option = "C",
  #                       breaks = c(1,2,3), 
  #                       labels = c("1", "2", "3")) +
  scale_fill_viridis(
    trans = "sqrt", 
    direction = -1,
    option = "plasma",
    breaks = c(0,0.10,1,3,5,7,9), 
    labels = c("0.00","0.1","1.0", "3.0", "5.0","7.0","9.0"),
    na.value = "white",
    name="Synthetic control weights") +
  geom_sf(data = lad[lad$LAD17NM=="Liverpool",],
          aes(geometry = geometry), 
          fill= "black",
          colour = "black",
          alpha = 1,
          lwd = 0) +
  geom_sf(data = city,aes(fill = NA),
          color = viridis(14)[5:12],
          na.rm = F,alpha=0,size = 1, show.legend = FALSE) +
  geom_sf_text(data = city,aes(label =TCITY15NM),
               color = viridis(14)[5:12],
               size = 6,
               nudge_x = c(-2,-1,2,-0.8,2,-2,1.5,-3), 
               nudge_y = c(0,0,0.1,0,0,0.1,0,0),
               show.legend = FALSE,
               clip = "off") +
  #geom_sf_label(data = city,aes(label =TCITY15NM),size = 3.5) +
  #ggrepel::geom_label_repel(data = city,aes(label =TCITY15NM, geometry = geometry),size = 3.5) +
  #geom_text_repel(data = city,aes(label =TCITY15NM, geometry = geometry),size = 3.5) +
  #guides(fill = guide_legend(title = "Synthetic control weights")) +
  theme_void() +
  theme(legend.text=element_text(size=14),
        legend.title=element_text(size=16))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

ggsave(normalizePath(file.path(b,"papers/SMART_hospitalisation/BMJ_submission/adj2_areas_wts.png")), width = 16, height = 14,device="png",
       units = "in", dpi=700)
ggsave(normalizePath(file.path(b,"papers/SMART_hospitalisation/BMJ_submission/adj2_areas_wts.svg")), width = 16, height = 14,device="svg",
       units = "in", dpi=700)


## create a cartogram
# construct a cartogram using the synthetic weights
summary(adj2_msoa$wt_est)
NROW(adj2_msoa[is.na(adj2_msoa$wt_est),]$wt_est)
NROW(adj2_msoa[adj2_msoa$wt_est==0,]$wt_est)
NROW(adj2_msoa[adj2_msoa$wt_est!=0,]$wt_est)
NROW(adj2_msoa[adj2_msoa$wt_est!=0 & adj2_msoa$wt_est!=1,]$wt_est)
sum(adj2_msoa[adj2_msoa$wt_est!=0 & adj2_msoa$wt_est!=1,]$wt_est)
# adj2_cart <- cartogram_cont(adj2_msoa, 
#                             weight="wt_est")
# # , 
# # #itermax=3,
# # maxSizeError = 1.0001,
# # prepare = "adjust",
# # threshold = 0.05)
# plot(st_geometry(adj2_cart))


# needs to recode the zero weights; otherwise the algorithm couldn't cope
adj2_msoa$wt2 <- ifelse(adj2_msoa$wt_est==0,adj2_msoa$wt_est+0.0001,
                        adj2_msoa$wt_est)
summary(adj2_msoa)

# This is a new geospatial object. Do a simple visualisation on it
adj2_cart <- cartogram_cont(adj2_msoa, 
                            weight="wt2")
plot(st_geometry(adj2_cart))


tm_shape(adj2_cart) + tm_polygons("wt2", style = "jenks") +
  tm_layout(frame = FALSE, legend.position = c("left", "bottom"))

# # start with a basic ggplot2 chloropleth map:
# adj2_spdf <- tidy(as_Spatial(adj2_cart))
# adj2_spdf = adj2_spdf %>% left_join(. , as_Spatial(adj2_cart)@data, by=c("id"="ISO3")) 
# ggplot() +
#   geom_polygon(data = adj2_spdf, aes(fill = order, x = long, y = lat, group = group) , 
#                size=0, alpha=0.9) +
#   coord_map() +
#   theme_void()

ggplot() +
  geom_sf(data = lad,aes(fill = NA), na.rm = F,alpha=0,size = 0.1) +
  geom_sf(data = adj2_cart[adj2_cart$wt_est!=0 & adj2_cart$intervention!=1,],
          aes(geometry = geometry, fill = as.numeric(wt_est)),
          colour = NA,
          alpha = 1,
          lwd = 0) +
  scale_fill_viridis(
    trans = "sqrt", 
    direction = -1,
    option = "C",
    breaks = c(0,0.10,1,3,5,7,9), 
    labels = c("0.00","0.1","1.0", "3.0", "5.0","7.0","9.0"),
    na.value = "white",
    name="Synthetic control weights") +
  geom_sf(data = lad[lad$LAD17NM=="Liverpool",],
          aes(geometry = geometry), 
          fill= "black",
          colour = "black",
          alpha = 1,
          lwd = 0) +
  geom_sf(data = city,aes(fill = NA),
          color = viridis(14)[5:12],
          na.rm = F,alpha=0,size = 1, show.legend = FALSE) +
  geom_sf_text(data = city,aes(label =TCITY15NM),
               color = viridis(14)[5:12],
               size = 6,
               nudge_x = c(-2,-1,2,-0.8,2,-2,1.5,-3), 
               nudge_y = c(0,0,0.1,0,0,0.1,0,0),
               show.legend = FALSE,
               clip = "off") +
  theme_void() +
  theme(legend.text=element_text(size=14),
        legend.title=element_text(size=16))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

# ggsave(normalizePath(file.path(b,"papers/SMART_hospitalisation/adj2_areas_wts_cartogram.png")), width = 16, height = 14,device="png",
#        units = "in", dpi=700)
# ggsave(normalizePath(file.path(b,"papers/SMART_hospitalisation/adj2_areas_wts_cartogram.svg")), width = 16, height = 14,device="svg",
#        units = "in", dpi=700)
ggsave(normalizePath(file.path(b,"papers/SMART_hospitalisation/BMJ_submission/fin_wts_cartogram.png")), width = 16, height = 14,device="png",
       units = "in", dpi=700)
ggsave(normalizePath(file.path(b,"papers/SMART_hospitalisation/BMJ_submission/fin_wts_cartogram.svg")), width = 16, height = 14,device="svg",
       units = "in", dpi=700)


# adj2_msoa$wt3 <- ifelse(adj2_msoa$wt_est==0 | adj2_msoa$intervention==1,adj2_msoa$wt_est,
#                         adj2_msoa$wt_est*10)
# summary(adj2_msoa)
# 
# adj2_cart0 <- cartogram_cont(adj2_msoa[adj2_msoa$wt_est!=0& adj2_msoa$intervention!=1,], 
#                              weight="wt3")
# plot(st_geometry(adj2_cart0))
# 
# ggplot() +
#   geom_sf(data = lad,aes(fill = NA), na.rm = F,alpha=0,size = 0.1) +
#   geom_sf(data = adj2_cart0[adj2_cart0$wt_est!=0 & adj2_cart0$intervention!=1,],
#           aes(geometry = geometry, fill = as.numeric(wt_est)),
#           colour = NA,
#           alpha = 1,
#           lwd = 0) +
#   scale_fill_viridis(
#     trans = "sqrt", 
#     direction = -1,
#     option = "C",
#     breaks = c(0,0.10,1,3,5,7,9), 
#     labels = c("0.00","0.1","1.0", "3.0", "5.0","7.0","9.0"),
#     na.value = "white",
#     name="Synthetic control weights") +
#   geom_sf(data = lad[lad$LAD17NM=="Liverpool",],
#           aes(geometry = geometry), 
#           fill= "black",
#           colour = "black",
#           alpha = 1,
#           lwd = 0) +
#   theme_void() +
#   theme(legend.text=element_text(size=14),
#         legend.title=element_text(size=16))+
#   annotation_scale(location = "bl", width_hint = 0.5) +
#   annotation_north_arrow(location = "tr", which_north = "true",
#                          pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
#                          style = north_arrow_fancy_orienteering)


########################## Summary statistics table ############################
table(test$st_week)
test$case
test[, case_rate:=cases_imp1*100000/total_pop]
min(test$date)
# plot(test$prop_70plus,test$prop_bame)
table5a <- test[st_week<0,
                list(imd_score=weighted.mean(imd_score),
                     pop_dens=weighted.mean(pop_dens),
                     prop_70plus=weighted.mean(prop_70plus)*100,
                     prop_bame=weighted.mean(prop_bame)*100,
                     av_test=weighted.mean(av_test),
                     as_prop_vulner=weighted.mean(as_prop_vulner)*100,
                     adm_rate=weighted.mean(adm_rate),
                     case_rate= weighted.mean(case_rate),
                     total_pop=sum(total_pop),
                     num_msoa=.N),by=.(intervention)]

table5a[, total_pop:= total_pop/7]
table5a[, num_msoa:= num_msoa/7]
table5a<-as.data.table(t(table5a), keep.rownames=T)


cols <- c('V1','V2')
table5a[,(cols):=lapply(.SD, round),.SDcols=cols]


# write.csv(table5a,
#           file= normalizePath(file.path(b,"papers/SMART_hospitalisation/summary_tab.csv")))

write.csv(table5a,
          file= normalizePath(file.path(b,"papers/SMART_hospitalisation/BMJ_submission/fin_summary_tab.csv")))



########################## Results table ############################
table1 <- as.data.table(sea1$Results$`12`)
table1 <- rbind(table1, as.data.table(sea1_long$Results$`16`))
table1 <- rbind(table1, as.data.table(sea1_long_adj2$Results$`16`))

# write.csv(table1,
#           file= normalizePath(file.path(b,"papers/SMART_hospitalisation/result_tab.csv")))

write.csv(table1,
          file= normalizePath(file.path(b,"papers/SMART_hospitalisation/BMJ_submission/fin_result_tab.csv")))

########################### Table for the appendix #############################
#####  rural-urban classification
rural_urban <- fread(normalizePath(file.path(b,"raw_data/msoa/Rural_Urban_Classification_(2011)_of_Middle_Layer_Super_Output_Areas_in_England_and_Wales.csv")), 
                     sep = ",", header= TRUE) %>%
  clean_names(.,"lower_camel")

rural_urban <- rural_urban[msoa11Cd %in% test$msoa11_cd]
table(rural_urban$ruc11)
table(rural_urban$ruc11Cd)
rural_urban[,urban:=ifelse((ruc11Cd=="A1" | ruc11Cd=="B1"| ruc11Cd=="C1"| ruc11Cd=="C2"),1,0)]
table(rural_urban$urban)

##### adding the proportion of children
test<- merge(test,rural_urban[,.(msoa11Cd,ruc11Cd,ruc11,urban)], by.x="msoa11_cd", by.y="msoa11Cd",all.x=T)
test[,prop_0_11:= (prop_0_1+prop_2_6+prop_7_11)]

##### adding the proportion of women based on both 2019 and 2020 mid year estimates
pop<-as.data.table(read_excel(normalizePath(file.path(b,"raw_data/msoa/SAPE22DT4-mid-2019-msoa-syoa-estimates-unformatted.xlsx")), 
                              sheet=4,skip=4))
pop[,8]
pop[,25]
pop$age_0_17 <- rowSums(pop[ , c(8:25)])
pop[, prop_0_17:=age_0_17/`All Ages`]


pop<-pop[, (8:98):=NULL]
setnames(pop, old=c("MSOA Code","All Ages"), new = c("msoa11_cd","total_pop"))
msoa_pop<-pop


male<-as.data.table(read_excel(normalizePath(file.path(b,"raw_data/msoa/SAPE22DT4-mid-2019-msoa-syoa-estimates-unformatted.xlsx")), 
                               sheet=5,range="A5:G7206",skip=4))
setnames(male, old=c("MSOA Code","All Ages"), new = c("msoa11_cd","male"))
msoa_pop <- merge(msoa_pop,male[,.(msoa11_cd,male)],by="msoa11_cd",all.x=T)


female<-as.data.table(read_excel(normalizePath(file.path(b,"raw_data/msoa/SAPE22DT4-mid-2019-msoa-syoa-estimates-unformatted.xlsx")), 
                                 sheet=6,range="A5:G7206",skip=4))
setnames(female, old=c("MSOA Code","All Ages"), new = c("msoa11_cd","female"))
msoa_pop <- merge(msoa_pop,female[,.(msoa11_cd,female)],by="msoa11_cd",all.x=T)

msoa_pop[,prop_male:=male/total_pop]
msoa_pop[,prop_female:=female/total_pop]

test<- merge(test,msoa_pop[,.(msoa11_cd,prop_0_17,prop_male,prop_female)], 
             by="msoa11_cd", all.x=T)


### generate the extended summary box

table(test$st_week)
min(test$date)
table(test$time)

test[, prop_urban:=sum(urban)/.N*100, by=.(ladcd,time)]
summary(test$prop_urban)
test[, lad_pop:=sum(total_pop), by=.(ladcd,time)]
test[, prop_urban_pop:=sum(total_pop)/lad_pop*100, by=.(ladcd,time,urban)]
summary(test$prop_urban_pop)

table(test$tier)
test[, week_tier1:=NULL]
test[, week_tier2:=NULL]
test[, week_tier3:=NULL]
test[, week_tier4:=NULL]
test[st_week<0, week_tier1:=sum(as.numeric(tier=="1")),by = .(msoa11_cd)]
test[st_week<0, week_tier2:=sum(as.numeric(tier=="2")),by = .(msoa11_cd)]
test[st_week<0, week_tier3:=sum(as.numeric(tier=="3")),by = .(msoa11_cd)]
test[st_week<0, week_tier4:=sum(as.numeric(tier=="4")),by = .(msoa11_cd)]
test[, week_tier1:=max(week_tier1, na.rm = T),by = .(msoa11_cd)]
test[, week_tier2:=max(week_tier2, na.rm = T),by = .(msoa11_cd)]
test[, week_tier3:=max(week_tier3, na.rm = T),by = .(msoa11_cd)]
test[, week_tier4:=max(week_tier4, na.rm = T),by = .(msoa11_cd)]

table(test$week_tier1)
table(test$week_tier2)
table(test$week_tier3)
table(test$week_tier4)
table(test$week_tier1, test$intervention)
table(test$week_tier2, test$intervention)
table(test$week_tier3, test$intervention)
table(test$week_tier4, test$intervention)

summary(test$educ)

# plot(test$prop_70plus,test$prop_bame)
tab_appendix <- test[st_week<0,
                     list(num_msoa=.N,
                          all_pop=sum(total_pop),
                          total_pop=weighted.mean(total_pop),
                          imd_score=weighted.mean(imd_score),
                          pop_dens=weighted.mean(pop_dens),
                          prop_70plus=weighted.mean(prop_70plus)*100,
                          prop_bame=weighted.mean(prop_bame)*100,
                          as_prop_vulner=weighted.mean(as_prop_vulner)*100,
                          av_test=weighted.mean(av_test),
                          adm_rate=weighted.mean(adm_rate),
                          case_rate= weighted.mean(case_rate),
                          educ= weighted.mean(educ),
                          prop_0_17=weighted.mean(prop_0_17)*100,
                          prop_female=weighted.mean(prop_female)*100,
                          prop_urban_pop=weighted.mean(prop_urban_pop),
                          week_tier1=weighted.mean(week_tier1),
                          week_tier2=weighted.mean(week_tier2),
                          week_tier3=weighted.mean(week_tier3),
                          week_tier4=weighted.mean(week_tier4)),by=.(intervention)]

tab_appendix[, all_pop:= all_pop/7]
tab_appendix[, num_msoa:= num_msoa/7]
tab_appendix<-as.data.table(t(tab_appendix), keep.rownames=T)

min_appendix <- test[st_week<0,
                     list(total_pop=min(total_pop),
                          imd_score=min(imd_score),
                          pop_dens=min(pop_dens),
                          prop_70plus=min(prop_70plus)*100,
                          prop_bame=min(prop_bame)*100,
                          as_prop_vulner=min(as_prop_vulner)*100,
                          av_test=min(av_test),
                          adm_rate=min(adm_rate),
                          case_rate= min(case_rate),
                          educ= min(educ),
                          prop_0_17=min(prop_0_17)*100,
                          prop_female=min(prop_female)*100,
                          prop_urban_pop=min(prop_urban_pop),
                          week_tier1=min(week_tier1),
                          week_tier2=min(week_tier2),
                          week_tier3=min(week_tier3),
                          week_tier4=min(week_tier4)),by=.(intervention)]
min_appendix<-as.data.table(t(min_appendix), keep.rownames=T)


max_appendix <- test[st_week<0,
                     list(total_pop=max(total_pop),
                          imd_score=max(imd_score),
                          pop_dens=max(pop_dens),
                          prop_70plus=max(prop_70plus)*100,
                          prop_bame=max(prop_bame)*100,
                          as_prop_vulner=max(as_prop_vulner)*100,
                          av_test=max(av_test),
                          adm_rate=max(adm_rate),
                          case_rate= max(case_rate),
                          educ= max(educ),
                          prop_0_17=max(prop_0_17)*100,
                          prop_female=max(prop_female)*100,
                          prop_urban_pop=max(prop_urban_pop),
                          week_tier1=max(week_tier1),
                          week_tier2=max(week_tier2),
                          week_tier3=max(week_tier3),
                          week_tier4=max(week_tier4)),by=.(intervention)]
max_appendix<-as.data.table(t(max_appendix), keep.rownames=T)

colnames(min_appendix)[2:3] <- c("noninterv_min","interv_min")
colnames(max_appendix)[2:3] <- c("noninterv_max","interv_max")

tab_appendix[,row_num:=1:NROW(rn)]

tab_appendix<-merge(tab_appendix,min_appendix,by="rn",all.x=T)
tab_appendix<-merge(tab_appendix,max_appendix,by="rn",all.x=T)
tab_appendix<-tab_appendix[order(row_num), ]
setcolorder(tab_appendix, c(4,1,3,6,8,2,5,7))
tab_appendix<-format(tab_appendix, scientific=FALSE, nsmall=0)


write.csv(tab_appendix,
          file= normalizePath(file.path(b,"papers/SMART_hospitalisation/BMJ_submission/app_summary_tab.csv")))


#### applying weights

tab_appendix_wts <- test[st_week<0 & wt_est>0,
                         list(
                           # num_msoa=.N*wt_est,
                           #all_pop=sum(total_pop*wt_est),
                           total_pop=weighted.mean(total_pop,w=wt_est),
                           imd_score=weighted.mean(imd_score,w=wt_est),
                           pop_dens=weighted.mean(pop_dens,w=wt_est),
                           prop_70plus=weighted.mean(prop_70plus,w=wt_est)*100,
                           prop_bame=weighted.mean(prop_bame,w=wt_est)*100,
                           as_prop_vulner=weighted.mean(as_prop_vulner,w=wt_est)*100,
                           av_test=weighted.mean(av_test,w=wt_est),
                           adm_rate=weighted.mean(adm_rate,w=wt_est),
                           case_rate= weighted.mean(case_rate,w=wt_est),
                           educ= weighted.mean(educ,w=wt_est),
                           prop_0_17=weighted.mean(prop_0_17,w=wt_est)*100,
                           prop_female=weighted.mean(prop_female,w=wt_est)*100,
                           prop_urban_pop=weighted.mean(prop_urban_pop,w=wt_est),
                           week_tier1=weighted.mean(week_tier1,w=wt_est),
                           week_tier2=weighted.mean(week_tier2,w=wt_est),
                           week_tier3=weighted.mean(week_tier3,w=wt_est),
                           week_tier4=weighted.mean(week_tier4,w=wt_est)),by=.(intervention)]


tab_appendix_wts[, all_pop:= unique(test[st_week==0 & wt_est>0,
                                         list(num_msoa=sum(wt_est),
                                              all_pop=sum(total_pop*wt_est)),by=.(intervention)])[,all_pop]]
tab_appendix_wts[, num_msoa:= unique(test[st_week==0 & wt_est>0,
                                          list(num_msoa=sum(wt_est),
                                               all_pop=sum(total_pop*wt_est)),by=.(intervention)])[,num_msoa]]
tab_appendix_wts<-as.data.table(t(tab_appendix_wts), keep.rownames=T)

min_appendix_wts <- test[st_week<0 & wt_est>0,
                         list(total_pop=min(total_pop*wt_est),
                              imd_score=min(imd_score*wt_est),
                              pop_dens=min(pop_dens*wt_est),
                              prop_70plus=min(prop_70plus*wt_est)*100,
                              prop_bame=min(prop_bame*wt_est)*100,
                              as_prop_vulner=min(as_prop_vulner*wt_est)*100,
                              av_test=min(av_test*wt_est),
                              adm_rate=min(adm_rate*wt_est),
                              case_rate= min(case_rate*wt_est),
                              educ= min(educ*wt_est),
                              prop_0_17=min(prop_0_17*wt_est)*100,
                              prop_female=min(prop_female*wt_est)*100,
                              prop_urban_pop=min(prop_urban_pop*wt_est),
                              week_tier1=min(week_tier1*wt_est),
                              week_tier2=min(week_tier2*wt_est),
                              week_tier3=min(week_tier3*wt_est),
                              week_tier4=min(week_tier4*wt_est)),by=.(intervention)]
min_appendix_wts<-as.data.table(t(min_appendix_wts), keep.rownames=T)


max_appendix_wts <- test[st_week<0 & wt_est>0,
                         list(total_pop=max(total_pop*wt_est),
                              imd_score=max(imd_score*wt_est),
                              pop_dens=max(pop_dens*wt_est),
                              prop_70plus=max(prop_70plus*wt_est)*100,
                              prop_bame=max(prop_bame*wt_est)*100,
                              as_prop_vulner=max(as_prop_vulner*wt_est)*100,
                              av_test=max(av_test*wt_est),
                              adm_rate=max(adm_rate*wt_est),
                              case_rate= max(case_rate*wt_est),
                              educ= max(educ*wt_est),
                              prop_0_17=max(prop_0_17*wt_est)*100,
                              prop_female=max(prop_female*wt_est)*100,
                              prop_urban_pop=max(prop_urban_pop*wt_est),
                              week_tier1=max(week_tier1*wt_est),
                              week_tier2=max(week_tier2*wt_est),
                              week_tier3=max(week_tier3*wt_est),
                              week_tier4=max(week_tier4*wt_est)),by=.(intervention)]
max_appendix_wts<-as.data.table(t(max_appendix_wts), keep.rownames=T)

colnames(min_appendix_wts)[2:3] <- c("noninterv_min","interv_min")
colnames(max_appendix_wts)[2:3] <- c("noninterv_max","interv_max")

setorder(tab_appendix_wts[, .r := order(c(20,19,1:18))], .r)[, .r := NULL]
tab_appendix_wts[,row_num:=1:NROW(rn)]

tab_appendix_wts<-merge(tab_appendix_wts,min_appendix_wts,by="rn",all.x=T)
tab_appendix_wts<-merge(tab_appendix_wts,max_appendix_wts,by="rn",all.x=T)
tab_appendix_wts<-tab_appendix_wts[order(row_num), ]
setcolorder(tab_appendix_wts, c(4,1,3,6,8,2,5,7))
tab_appendix_wts<-format(tab_appendix_wts, scientific=FALSE, nsmall=0)


write.csv(tab_appendix_wts,
          file= normalizePath(file.path(b,"papers/SMART_hospitalisation/BMJ_submission/app_wts_summary_tab.csv")))


unique(test[st_week==0,
            list(num_msoa=sum(wt_est),
                 #  tot_admt=sum(adm_rate*total_pop*wt_est),
                 all_pop=sum(total_pop*wt_est)),by=.(intervention)])[,all_pop]
unique(test[st_week<=0,
            list(tot_admt=sum(adm_rate*total_pop*wt_est)),by=.(intervention)])
unique(test[st_week==-7,
            list(tot_admt=sum(adm_rate*total_pop*wt_est)),by=.(intervention)])
unique(test[st_week==-6,
            list(tot_admt=sum(adm_rate*total_pop*wt_est)),by=.(intervention)])
unique(test[st_week==-5,
            list(tot_admt=sum(adm_rate*total_pop*wt_est)),by=.(intervention)])
unique(test[st_week==-4,
            list(tot_admt=sum(adm_rate*total_pop*wt_est)),by=.(intervention)])
unique(test[st_week==-3,
            list(tot_admt=sum(adm_rate*total_pop*wt_est)),by=.(intervention)])
unique(test[st_week==-2,
            list(tot_admt=sum(adm_rate*total_pop*wt_est)),by=.(intervention)])
unique(test[st_week==-1,
            list(tot_admt=sum(adm_rate*total_pop*wt_est)),by=.(intervention)])
unique(test[st_week==0,
            list(tot_admt=sum(adm_rate*total_pop*wt_est)),by=.(intervention)])
adm_appendix_wts<- unique(test[st_week<0,
                               list(tot_admt_wts=sum(adm_rate*total_pop*wt_est),
                                    tot_admt=sum(adm_rate*total_pop)),by=.(intervention,st_week)])


table(test0[st_week<0]$week_number)
get_date(40,2020,1)
get_date(40,2020,7)


write.csv(adm_appendix_wts,
          file= normalizePath(file.path(b,"papers/SMART_hospitalisation/BMJ_submission/adm_wts_summary_tab.csv")))

############################# Look at the weights at LA level ###############################
synth_wts<-test[st_week==0&wt_est>0,.(msoa11_cd,msoa_name,ladcd,ladnm,intervention,wt_est, regionname)]
NROW(unique(synth_wts[intervention==0]$msoa11_cd))
NROW(unique(synth_wts[intervention==0]$ladcd))
as.data.table(table(synth_wts[intervention==0]$ladnm))
table(synth_wts[intervention==0]$ladnm,synth_wts[intervention==0]$regionname)


lad<-merge(lad,as.data.table(table(synth_wts[intervention==0]$ladnm)), by.x="LAD17NM",by.y="V1",all.x=T)

ggplot() +
  geom_sf(data = lad,aes(fill = N), na.rm = F,alpha=1,size = 0.1) +
  scale_fill_viridis(
    trans = "sqrt", 
    direction = -1,
    option = "C",
    breaks = c(0,1,2,3,4,5,6), 
    labels = c("0.00","1","2", "3", "4","5","6"),
    na.value = "white",
    name="Number of MSOAs\nin Synthetic control") +
  geom_sf(data = lad[lad$LAD17NM=="Liverpool",],
          aes(geometry = geometry), 
          fill= "black",
          colour = "black",
          alpha = 1,
          lwd = 0) +
  geom_sf(data = city,aes(fill = NA),
          color = viridis(14)[5:12],
          na.rm = F,alpha=0,size = 1, show.legend = FALSE) +
  geom_sf_text(data = city,aes(label =TCITY15NM),
               color = viridis(14)[5:12],
               size = 6,
               nudge_x = c(-2,-1,2,-0.8,2,-2,1.5,-3), 
               nudge_y = c(0,0,0.1,0,0,0.1,0,0),
               show.legend = FALSE,
               clip = "off") +
  theme_void() +
  theme(legend.text=element_text(size=14),
        legend.title=element_text(size=16))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

ggsave(normalizePath(file.path(b,"papers/SMART_hospitalisation/BMJ_submission/lad_wts.png")), width = 16, height = 14,device="png",
       units = "in", dpi=700)
ggsave(normalizePath(file.path(b,"papers/SMART_hospitalisation/BMJ_submission/lad_wts.svg")), width = 16, height = 14,device="svg",
       units = "in", dpi=700)


ggplot() +
  geom_sf(data = lad,aes(fill = N), na.rm = F,alpha=1,size = 0.1) +
  scale_fill_viridis(
    trans = "sqrt", 
    direction = -1,
    option = "C",
    breaks = c(0,1,2,3,4,5,6), 
    labels = c("0.00","1","2", "3", "4","5","6"),
    na.value = "white",
    name="Number of MSOAs\nin Synthetic control") +
  geom_sf_text(data = lad[lad$N>0,],aes(label = LAD17NM),
               show.legend = FALSE,
               clip = "off") +
  theme_void() +
  theme(legend.text=element_text(size=14),
        legend.title=element_text(size=16))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
#################### Sensitivity test - adding new covariates ##################

###### adding the proportion of women

cov.var1 <- c("total_pop", "prop_70plus", "prop_bame", "pop_dens", "as_prop_vulner", "imd_score","av_test","prop_female")

set.seed(6768)
sea1_sex <- microsynth(as.data.frame(test),
                       idvar="id", timevar="time", intvar="intervention",
                       start.pre=1, end.pre=7, end.post=12,
                       match.out=c("admi2_cnt"), match.covar=cov.var1,
                       result.var=c("admi2_cnt"), omnibus.var=F,
                       test="twosided",
                       check.feas=TRUE,
                       confidence=0.95,
                       perm=250,
                       jack=F,
                       n.cores = 1)

sea1_sex
summary(sea1_sex)
par(mar=c(1,1,1,1))
plot_microsynth(sea1_sex)


set.seed(6768)
sea1_sex_long_adj2 <- microsynth(as.data.frame(test),
                                 idvar="id", timevar="time", intvar="intervention",
                                 start.pre=1, end.pre=7, end.post=16,
                                 match.out=c("admi2_cnt_adj2"), match.covar=cov.var1,
                                 result.var=c("admi2_cnt_adj2"), omnibus.var=F,
                                 test="twosided",
                                 check.feas=TRUE,
                                 confidence=0.95,
                                 perm=250,
                                 jack=F,
                                 # result="smart1",
                                 n.cores = 1)

sea1_sex_long_adj2 
summary(sea1_sex_long_adj2)
plot_microsynth(sea1_sex_long_adj2)

set.seed(6768)
sea1_sex_long <- microsynth(as.data.frame(test),
                            idvar="id", timevar="time", intvar="intervention",
                            start.pre=1, end.pre=7, end.post=16,
                            match.out=c("admi2_cnt"), match.covar=cov.var1,
                            result.var=c("admi2_cnt"), omnibus.var=F,
                            test="twosided",
                            check.feas=TRUE,
                            confidence=0.95,
                            perm=250,
                            jack=F,
                            # result="smart1",
                            n.cores = 1)
sea1_sex_long 
summary(sea1_sex_long)
plot_microsynth(sea1_sex_long)




tab_sex <- as.data.table(sea1_sex$Results$`12`)
tab_sex <- rbind(tab_sex, as.data.table(sea1_sex_long$Results$`16`))
tab_sex <- rbind(tab_sex, as.data.table(sea1_sex_long_adj2$Results$`16`))



write.csv(tab_sex,
          file= normalizePath(file.path(b,"papers/SMART_hospitalisation/BMJ_submission/app_sex.csv")))

###### using the education domain score of IMD

cov.var2 <- c("total_pop", "prop_70plus", "prop_bame", "pop_dens", "as_prop_vulner", "educ","av_test")

set.seed(6768)
sea1_edu <- microsynth(as.data.frame(test),
                       idvar="id", timevar="time", intvar="intervention",
                       start.pre=1, end.pre=7, end.post=12,
                       match.out=c("admi2_cnt"), match.covar=cov.var2,
                       result.var=c("admi2_cnt"), omnibus.var=F,
                       test="twosided",
                       check.feas=TRUE,
                       confidence=0.95,
                       perm=250,
                       jack=F,
                       n.cores = 1)



set.seed(6768)
sea1_edu_long_adj2 <- microsynth(as.data.frame(test),
                                 idvar="id", timevar="time", intvar="intervention",
                                 start.pre=1, end.pre=7, end.post=16,
                                 match.out=c("admi2_cnt_adj2"), match.covar=cov.var2,
                                 result.var=c("admi2_cnt_adj2"), omnibus.var=F,
                                 test="twosided",
                                 check.feas=TRUE,
                                 confidence=0.95,
                                 perm=250,
                                 jack=F,
                                 # result="smart1",
                                 n.cores = 1)


set.seed(6768)
sea1_edu_long <- microsynth(as.data.frame(test),
                            idvar="id", timevar="time", intvar="intervention",
                            start.pre=1, end.pre=7, end.post=16,
                            match.out=c("admi2_cnt"), match.covar=cov.var2,
                            result.var=c("admi2_cnt"), omnibus.var=F,
                            test="twosided",
                            check.feas=TRUE,
                            confidence=0.95,
                            perm=250,
                            jack=F,
                            # result="smart1",
                            n.cores = 1)


tab_edu <- as.data.table(sea1_edu$Results$`12`)
tab_edu <- rbind(tab_edu, as.data.table(sea1_edu_long$Results$`16`))
tab_edu <- rbind(tab_edu, as.data.table(sea1_edu_long_adj2$Results$`16`))



write.csv(tab_edu,
          file= normalizePath(file.path(b,"papers/SMART_hospitalisation/BMJ_submission/app_edu.csv")))

###### adding the proportion of children

cov.var3 <- c("total_pop", "prop_bame", "pop_dens", "as_prop_vulner", "imd_score","av_test","prop_0_17")

cor.test(test$prop_0_17,test$prop_70plus) 

set.seed(6768)
sea1_kid <- microsynth(as.data.frame(test),
                       idvar="id", timevar="time", intvar="intervention",
                       start.pre=1, end.pre=7, end.post=12,
                       match.out=c("admi2_cnt"), match.covar=cov.var3,
                       result.var=c("admi2_cnt"), omnibus.var=F,
                       test="twosided",
                       check.feas=TRUE,
                       confidence=0.95,
                       perm=250,
                       jack=F,
                       n.cores = 1)


set.seed(6768)
sea1_kid_long_adj2 <- microsynth(as.data.frame(test),
                                 idvar="id", timevar="time", intvar="intervention",
                                 start.pre=1, end.pre=7, end.post=16,
                                 match.out=c("admi2_cnt_adj2"), match.covar=cov.var3,
                                 result.var=c("admi2_cnt_adj2"), omnibus.var=F,
                                 test="twosided",
                                 check.feas=TRUE,
                                 confidence=0.95,
                                 perm=250,
                                 jack=F,
                                 # result="smart1",
                                 n.cores = 1)


set.seed(6768)
sea1_kid_long <- microsynth(as.data.frame(test),
                            idvar="id", timevar="time", intvar="intervention",
                            start.pre=1, end.pre=7, end.post=16,
                            match.out=c("admi2_cnt"), match.covar=cov.var3,
                            result.var=c("admi2_cnt"), omnibus.var=F,
                            test="twosided",
                            check.feas=TRUE,
                            confidence=0.95,
                            perm=250,
                            jack=F,
                            # result="smart1",
                            n.cores = 1)




tab_kid <- as.data.table(sea1_kid$Results$`12`)
tab_kid <- rbind(tab_kid, as.data.table(sea1_kid_long$Results$`16`))
tab_kid <- rbind(tab_kid, as.data.table(sea1_kid_long_adj2$Results$`16`))



write.csv(tab_kid,
          file= normalizePath(file.path(b,"papers/SMART_hospitalisation/BMJ_submission/app_kid.csv")))


#################### Sensitivity test - upper/lower 95% CI adjustments #################
###### Model 2a - assumes lower bound effect of tiered restrictions - sea1_long_adj2 
### Intervention Period;	Periods in the model
### 6/11/20 - 2/1/21;	19/11/20 - 15/1/21

set.seed(6768)
sea1_long_adj2_lcl <- microsynth(as.data.frame(test), 
                                 idvar="id", timevar="time", intvar="intervention", 
                                 start.pre=1, end.pre=8, end.post=end.post, 
                                 match.out=c("admi2_cnt_adj2_lcl"), match.covar=cov.var, 
                                 result.var=c("admi2_cnt_adj2_lcl"), omnibus.var=F,
                                 test="twosided",
                                 check.feas=TRUE, 
                                 confidence=0.95,
                                 perm=250,
                                 jack=F,
                                 result="smart1",
                                 n.cores = 1)

sea1_long_adj2_lcl 
summary(sea1_long_adj2_lcl )
plot_microsynth(sea1_long_adj2_lcl)




######## Model 2b - assumes upper bound effect of tiered restrictions - sea1_long_adj2
set.seed(6768)
sea1_long_adj2_ucl <- microsynth(as.data.frame(test), 
                                 idvar="id", timevar="time", intvar="intervention", 
                                 start.pre=1, end.pre=8, end.post=end.post, 
                                 match.out=c("admi2_cnt_adj2_ucl"), match.covar=cov.var,
                                 result.var=c("admi2_cnt_adj2_ucl"), omnibus.var=F,
                                 test="twosided",
                                 check.feas=TRUE, 
                                 confidence=0.95,
                                 perm=250,
                                 jack=F,
                                 result="smart1",
                                 n.cores = 1)

sea1_long_adj2_ucl 
summary(sea1_long_adj2_ucl )
plot_microsynth(sea1_long_adj2_ucl)



tab2 <- rbind(tab2, as.data.table(sea1_long_adj2_lcl$Results$`16`))
tab2 <- rbind(tab2, as.data.table(sea1_long_adj2_ucl$Results$`16`))

# write.csv(table01,
#           file= normalizePath(file.path(b,"papers/SMART_hospitalisation/result01_tab_all_lft_rate.csv")))

write.csv(tab2,
          file= normalizePath(file.path(b,"papers/SMART_hospitalisation/BMJ_submission/app_0.95ci.csv")))

####################### Sensitivity test - matching on the LA level #################

#  make into local authority level dataset


la_data<-test[time<13, list(admi2_cnt=sum(admi2_cnt), total_pop=sum(total_pop), 
                            prop_70plus=weighted.mean(prop_70plus, w=total_pop),
                            prop_bame=weighted.mean(prop_bame, w=total_pop),
                            pop_dens=weighted.mean(pop_dens, w=total_pop),
                            as_prop_vulner=weighted.mean( as_prop_vulner, w=total_pop),
                            imd_score=weighted.mean(imd_score, w=total_pop)),
              by=.(av_test, ltlacode,ltlaname,date,time)]

date_lk<-unique(la_data[, .(date,time)])
la_data[, intervention:=as.numeric(ltlaname=="Liverpool"
)]
#  match Liverpool with comparison group

pre_data<-la_data[time<8, -c("date")]
pre_data<-dcast.data.table(pre_data, prop_70plus+prop_bame+pop_dens+
                             as_prop_vulner+imd_score+av_test+ltlacode+ltlaname+intervention~time, 
                           value.var = "admi2_cnt")


pre_data$`1`
set.seed(12345)
matchcohort <- matchit(intervention~imd_score+prop_bame+prop_70plus+pop_dens+
                         as_prop_vulner+`1`+`2`+`3`+`4`+`5`+`6`+`7`, 
                       data = na.omit(pre_data), method="nearest",
                       distance = "mahalanobis", ratio=10)


matchcohort2 <- as.data.table(match.data(matchcohort))

ls1<-matchcohort2[, .(ltlacode,include=1)]


# merge this back in to the main la data
anal_data<-merge(la_data,ls1, by="ltlacode", all.x = T)
anal_data[is.na(include)==T, include:=0]

anal_data[, rate:=admi2_cnt*100000/total_pop]


# 
dt1<-dcast(na.omit(anal_data[include==1, .(ltlaname,time,rate)]), time~ltlaname, value.var = c("rate"))
unique(anal_data[include==1]$ltlaname)

dt1<-na.omit(dt1)
dt1<-clean_names(dt1)

dt2<-as.data.frame(cbind(dt1[, c("liverpool")],dt1[, -c("liverpool" ,"time")]))
pre.period <- c(1, 7)
post.period <- c(8, 12)
# impact <- CausalImpact(d1, pre.period, post.period)
impact_est <- CausalImpact(dt2, pre.period, post.period,model.args = list(prior.level.sd=0.1))
dev.off()
plot(impact_est)
summary(impact_est)



results<-as.data.table(impact_est$summary)
results<-results[1, .(res="admissions", estimate=RelEffect,lcl=RelEffect.lower,ucl=RelEffect.upper, p)]

results[, lcl:=paste0("(", as.character(round(lcl*100,1)))]
results[, ucl:=paste0( ", ",as.character(round(ucl*100,1)),")")]
results[, estimate:= as.character(round(estimate*100,1))]
results[, p.value:= as.character(round(p,3))]
fwrite(results[, .(res,estimate,lcl, ucl, p.value)], 
       file = normalizePath(file.path(b,"papers/SMART_hospitalisation/BMJ_submission/la_level_results.csv")))




fig<-as.data.table(impact_est$series)
fig[, time:=1:.N]
fig<-merge(fig,date_lk, by="time")


fig<-fig[, .(estimate1=response, estimate2=point.pred, lcl2=point.pred.lower,
             ucl2=point.pred.upper,lcl1=numeric(), ucl1=numeric(), date)]

fig<-melt(fig,id.vars = "date", measure.vars = list(c("estimate1","estimate2"),
                                                    c("ucl1","ucl2"),c("lcl1","lcl2")))

names(fig)<-c("date","v1", "estimate", "lcl", "ucl")
fig$inter <- ifelse(as.numeric(fig$v1)==1,"Liverpool","Synthetic control")


show_col(viridis_pal(option = "viridis")(2))
viridis(2)


ggplot(data=fig)+
  geom_line(aes(x = date, y = estimate, color=as.factor(inter), linetype=as.factor(inter)), size=1) +
  geom_ribbon(aes(x=date, ymin=lcl, ymax=ucl, fill=as.factor(inter)), alpha=0.2)+
  scale_colour_manual(values = c("#FDE725FF","#440154FF")) +
  scale_linetype_manual( values = c("dashed","solid")) +
  scale_fill_manual(values = c("#FDE725FF","#440154FF")) +
  labs(x = "Week start date", y = "Admissions per 100,000 population", color = NULL, fill = NULL, linetype=NULL)+
  theme_classic() +
  scale_x_date(date_breaks ="weeks" )+
  geom_vline(linetype = "dotted", xintercept =as.Date(c("2020-11-19")), size=1.0)+
  theme(text = element_text(size=12),legend.position="bottom",  legend.text = element_text(size=12),
        legend.title = element_blank(), axis.text.x = element_text(angle = 90,vjust=0.5))+ 
  theme(strip.background = element_blank())

ggsave("la_level_chart1.png",path=normalizePath(file.path(b,"papers/SMART_hospitalisation/BMJ_submission")),
       width = 16, height = 8, units="in",dpi=600)
ggsave("la_level_chart1.svg",path=normalizePath(file.path(b,"papers/SMART_hospitalisation/BMJ_submission")),
       width = 16, height = 8, units="in",dpi=600)


fig<-as.data.table(impact_est$series)
fig[, time:=1:.N]
fig<-merge(fig,date_lk, by="time")


fig<-fig[, .(estimate=cum.effect,  lcl=cum.effect.lower,ucl=cum.effect.upper, date)]

ggplot(data=fig )+
  geom_line(aes(x = date, y = estimate), size=1, color="#FDE725FF",linetype = "dashed") +
  geom_ribbon(aes(x=date, ymin=lcl, ymax=ucl), alpha=0.2, fill="#FDE725FF")+
  labs(x = "Week start date", y = "Admissions per 100,000 population", color = NULL, 
       fill = NULL, linetype=NULL)+
  theme_classic() +
  scale_x_date(date_breaks ="weeks" )+
  geom_vline(linetype = "dotted", xintercept =as.Date(c("2020-11-19")), size=1.0)+
  geom_hline(linetype = "solid", yintercept =0, color = "#440154FF",size=1.0)+
  theme(text = element_text(size=12),legend.position="bottom",  legend.text = element_text(size=12),
        legend.title = element_blank(), axis.text.x = element_text(angle = 90,vjust=0.5))+
  theme(strip.background = element_blank())


ggsave("la_level_chart2.png",path=normalizePath(file.path(b,"papers/SMART_hospitalisation/BMJ_submission")),
       width = 16, height = 8, units="in",dpi=600)
ggsave("la_level_chart2.svg",path=normalizePath(file.path(b,"papers/SMART_hospitalisation/BMJ_submission")),
       width = 16, height = 8, units="in",dpi=600)

############## Sensitivity test - assumes lower/upper bound effect of tiered restrictions ###########
cov.var <- c("total_pop", "prop_70plus", "prop_bame", "pop_dens", "as_prop_vulner", "imd_score","av_test")

set.seed(6768)
sea1_long_adj2_lcl <- microsynth(as.data.frame(test), 
                                  idvar="id", timevar="time", intvar="intervention", 
                                  start.pre=1, end.pre=7, end.post=16, 
                                  match.out=c("admi2_cnt_adj2_lcl"), match.covar=cov.var, 
                                  result.var=c("admi2_cnt_adj2_lcl"), omnibus.var=F,
                                  test="twosided",
                                  check.feas=TRUE, 
                                  confidence=0.95,
                                  perm=250,
                                  jack=F,
                                 # result="smart1",
                                  n.cores = 1)

sea1_long_adj2_lcl 
summary(sea1_long_adj2_lcl )
plot_microsynth(sea1_long_adj2_lcl)


set.seed(6768)
sea1_long_adj2_ucl <- microsynth(as.data.frame(test), 
                                  idvar="id", timevar="time", intvar="intervention", 
                                  start.pre=1, end.pre=7, end.post=16, 
                                  match.out=c("admi2_cnt_adj2_ucl"), match.covar=cov.var,
                                  result.var=c("admi2_cnt_adj2_ucl"), omnibus.var=F,
                                  test="twosided",
                                  check.feas=TRUE, 
                                  confidence=0.95,
                                  perm=250,
                                  jack=F,
                                 # result="smart1",
                                  n.cores = 1)

sea1_long_adj2_ucl 
summary(sea1_long_adj2_ucl )
plot_microsynth(sea1_long_adj2_ucl)


app_tab_ci <- as.data.table(sea1_long_adj2_lcl$Results$`16`)
app_tab_ci <- rbind(app_tab_ci, as.data.table(sea1_long_adj2_ucl$Results$`16`))

write.csv(app_tab_ci,
          file= normalizePath(file.path(b,"papers/SMART_hospitalisation/BMJ_submission/app_tab_ci.csv")))




####################### Sensitivity test - not excluding places with 1 in 100,000 meanlftrate #################
#### sea02 ####
end.post=max(test0$time)

# time from which would expect tiers to affect hospital admissions 
end.pre=12
end.post=21

cov.var2 <- c("total_pop", "prop_70plus", "prop_bame", "pop_dens", "as_prop_vulner", "imd_score","av_test","prop_sgtf")
set.seed(6768)
match.out2 <- c("admi2_cnt")
## by selecting intervention==0, ladnm=="Liverpool" MSOAs were excluded.
sea02 <- microsynth(as.data.frame(test0[intervention==0 & other_lcr==0]), 
                    idvar="id", timevar="time", intvar="tier3", 
                    start.pre=1, end.pre=end.pre, end.post=end.post, 
                    match.out=match.out2, match.covar=cov.var2, 
                    result.var=match.out2 , 
                    omnibus.var=match.out2,
                    test="twosided",
                    confidence = 0.95,
                    perm=250,
                    jack=F, 
                    n.cores = 1)

summary(sea02)
plot_microsynth(sea02)

#  doesnt make any difference to the tier estimates. 



test01<-test0[date>=as.Date("2020-10-01") & week_number<=yearweek("2021 W20") & 
                (intervention==1|(other_lcr==0))]

prop.table(table(test01$miss_msoa_flag))

with(test01, table(intervention, time))
with(test01, table(intervention, date))

test01[, inc_2:=NULL]
test01[, inc_lcl2:=NULL]
test01[, inc_ucl2:=NULL]
test01<-merge(test01,coefs, by="week_number", all.x = T)


test01[, admi2_cnt_adj2:=as.numeric(admi2_cnt)]
# test[tier2==0 & after_tier2==T & time<=20, admi2_cnt_adj2:=admi2_cnt/inc_1]

table(test01$tier3, test01$tier2, useNA = "ifany")
test01[tier3==1 & is.na(inc_2)==F, admi2_cnt_adj2:=admi2_cnt_adj2/inc_2]

# check<-test[tier3==1 & is.na(inc_2)==F, .(tier3,admi2_cnt_adj2,admi2_cnt,week_number,inc_2)]


with(test01, table(tier3, intervention))
test01[, admi2_cnt_adj2_lcl:=as.numeric(admi2_cnt)]

test01[tier3==1 &  is.na(inc_2)==F, admi2_cnt_adj2_lcl:=admi2_cnt_adj2_lcl/inc_lcl2]
test01[, admi2_cnt_adj2_ucl:=as.numeric(admi2_cnt)]
test01[tier3==1  & is.na(inc_2)==F, admi2_cnt_adj2_ucl:=admi2_cnt_adj2_ucl/inc_ucl2]

table(test01$tier2,test01$tier2 )
min(test01[tier2==1 & after_tier2==T]$date)
max(test01[tier2==1 & after_tier2==T]$date)
cov.var <- c("total_pop", "prop_bame", "pop_dens","imd_score", "prop_70plus",
             "as_prop_vulner")
end.post=max(test01$time)

max(as.Date(test01[time==8]$week_number))

max(as.Date(test01[time==end.post]$week_number))

# MSOA hospital admissions analysis  - no adjustment for tier 2
test01[is.na(admi2_cnt)==T, admi2_cnt:=0]
table(test01[is.na(admi2_cnt)==T]$time)
table(msoa_adm$char_week)

############################## Model 01 - sea01 ##################################
### Intervention Period;	Periods in the model
### 6/11/20 - 3/12/20;	19/11/20 - 16/12/20


table(test01[time==7]$date)
table(test01[time==7]$week_number)
get_date(46,2020)
get_date(46,2020,7)

table(test01[time==12]$date)
table(test01[time==12]$week_number)
get_date(51,2020)
get_date(51,2020,7)

set.seed(6768)
sea01 <- microsynth(as.data.frame(test01),
                    idvar="id", timevar="time", intvar="intervention",
                    start.pre=1, end.pre=7, end.post=12,
                    match.out=c("admi2_cnt"), match.covar=cov.var,
                    result.var=c("admi2_cnt"), omnibus.var=F,
                    test="twosided",
                    check.feas=TRUE,
                    confidence=0.95,
                    perm=250,
                    jack=F,
                    n.cores = 1)

sea01
summary(sea01)
par(mar=c(1,1,1,1))
plot_microsynth(sea01)


######## Model 02 - assumes mean effect of tiered restrictions - sea01_long_adj2 ####
### Intervention Period;	Periods in the model
### 6/11/20 - 2/1/21;	19/11/20 - 15/1/21


table(test01[time==16]$date)
table(test01[time==16]$week_number)
get_date(2,2021)
get_date(2,2021,7)

set.seed(6768)
sea01_long_adj2 <- microsynth(as.data.frame(test01),
                              idvar="id", timevar="time", intvar="intervention",
                              start.pre=1, end.pre=7, end.post=16,
                              match.out=c("admi2_cnt_adj2"), match.covar=cov.var,
                              result.var=c("admi2_cnt_adj2"), omnibus.var=F,
                              test="twosided",
                              check.feas=TRUE,
                              confidence=0.95,
                              perm=250,
                              jack=F,
                              # result="smart1",
                              n.cores = 1)

sea01_long_adj2 
summary(sea01_long_adj2 )
plot_microsynth(sea01_long_adj2)

######## Model 03 - assumes no effect of tiered restrictions - sea01_long ####

## Intervention Period;	Periods in the model; unadjusted model;
## 6/11/20 - 2/1/21;	19/11/20 - 15/1/21
table(test01[time==7]$date)
table(test01[time==7]$week_number)
get_date(46,2021)
get_date(46,2021,7)

table(test01[time==16]$date)
table(test01[time==16]$week_number)
get_date(2,2021)
get_date(2,2021,7)

set.seed(6768)
sea01_long <- microsynth(as.data.frame(test01),
                         idvar="id", timevar="time", intvar="intervention",
                         start.pre=1, end.pre=7, end.post=16,
                         match.out=c("admi2_cnt"), match.covar=cov.var,
                         result.var=c("admi2_cnt"), omnibus.var=F,
                         test="twosided",
                         check.feas=TRUE,
                         confidence=0.95,
                         perm=250,
                         jack=F,
                         # result="smart1",
                         n.cores = 1)
sea01_long 
summary(sea01_long)
plot_microsynth(sea01_long)

###### Model 02a - assumes lower bound effect of tiered restrictions - sea01_long_adj2 ####
### Intervention Period;	Periods in the model
### 6/11/20 - 2/1/21;	19/11/20 - 15/1/21
table(test01[time==7]$date)
table(test01[time==7]$week_number)
get_date(46,2020)
get_date(46,2020,7)

table(test01[time==16]$date)
table(test01[time==16]$week_number)
get_date(2,2021)
get_date(2,2021,7)

set.seed(6768)
sea01_long_adj2_lcl <- microsynth(as.data.frame(test01), 
                                  idvar="id", timevar="time", intvar="intervention", 
                                  start.pre=1, end.pre=7, end.post=16, 
                                  match.out=c("admi2_cnt_adj2_lcl"), match.covar=cov.var, 
                                  result.var=c("admi2_cnt_adj2_lcl"), omnibus.var=F,
                                  test="twosided",
                                  check.feas=TRUE, 
                                  confidence=0.95,
                                  perm=250,
                                  jack=F,
                                  result="smart1",
                                  n.cores = 1)

sea01_long_adj2_lcl 
summary(sea01_long_adj2_lcl )
plot_microsynth(sea01_long_adj2_lcl)


######## Model 02b - assumes upper bound effect of tiered restrictions - sea1_long_adj2 ####
set.seed(6768)
sea01_long_adj2_ucl <- microsynth(as.data.frame(test01), 
                                  idvar="id", timevar="time", intvar="intervention", 
                                  start.pre=1, end.pre=7, end.post=16, 
                                  match.out=c("admi2_cnt_adj2_ucl"), match.covar=cov.var,
                                  result.var=c("admi2_cnt_adj2_ucl"), omnibus.var=F,
                                  test="twosided",
                                  check.feas=TRUE, 
                                  confidence=0.95,
                                  perm=250,
                                  jack=F,
                                  result="smart1",
                                  n.cores = 1)

sea01_long_adj2_ucl 
summary(sea01_long_adj2_ucl )
plot_microsynth(sea01_long_adj2_ucl)

########################## Results table ############################
table01 <- as.data.table(sea01$Results$`12`)
table01 <- rbind(table01, as.data.table(sea01_long$Results$`16`))
table01 <- rbind(table01, as.data.table(sea01_long_adj2$Results$`16`))
table01 <- rbind(table01, as.data.table(sea01_long_adj2_lcl$Results$`16`))
table01 <- rbind(table01, as.data.table(sea01_long_adj2_ucl$Results$`16`))

# write.csv(table01,
#           file= normalizePath(file.path(b,"papers/SMART_hospitalisation/result01_tab_all_lft_rate.csv")))

write.csv(table01,
          file= normalizePath(file.path(b,"papers/SMART_hospitalisation/BMJ_submission/fin_result01_tab_all_lft_rate.csv")))



