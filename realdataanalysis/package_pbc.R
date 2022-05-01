#-------------------------------------------------------------
#---------------------------SETTING---------------------------
#-------------------------------------------------------------
#rm(list=ls());gc();

memory.limit(16*2^20)

options(max.print=999999)
options(error=NULL)

# install.packages("devtools")
# install.packages("Rcpp")
# install.packages("RcppArmadillo")
# install.packages("survival")
# install.packages("ggplot2")
# install.packages("gridExtra")
# install.packages("aftgee")
# install_github("WoojungBae/afttest")

library(devtools)
library(Rcpp)
library(RcppArmadillo)
library(survival)
library(ggplot2)
library(gridExtra)
library(aftgee)
library(afttest)

#-------------------------------------------------------------
#---------------------------- DATA ---------------------------
#-------------------------------------------------------------
set.seed(1)

path=200

delete_na=which(is.na(pbc$protime)+is.na(pbc$trt)>0)

D_pbc = pbc$status[-delete_na]; D_pbc[which(D_pbc==1)]=0; D_pbc[which(D_pbc==2)]=1
X_pbc = pbc$time[-delete_na]

trt = pbc$trt[-delete_na]-1; range(trt);
edema = pbc$edema[-delete_na]; range(edema);
bili = pbc$bili[-delete_na]; range(bili); log_bili=log(bili)
protime = pbc$protime[-delete_na]; range(protime); log_protime=log(protime)
albumin = pbc$albumin[-delete_na]; range(albumin); log_albumin=log(albumin)
age = pbc$age[-delete_na]; range(age); log_age=log(age)

# ------------------------------------------------------------------------------
# ------------------------------------ "mns" -----------------------------------
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# --------------- Covariates: bili+protime+albumin+age+edema+trt ---------------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
result01_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~bili+protime+albumin+age+edema+trt,path=path,testtype="omni",eqType="mns")
result01_afttest_omni_mns$p_value
result01_afttest_omni_mns$p_std_value
# afttestplot(result01_afttest_omni_mns,std="unstd")
# afttestplot(result01_afttest_omni_mns,std="std")

# ------------------------------------ link ------------------------------------
result01_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~bili+protime+albumin+age+edema+trt,path=path,testtype="link",eqType="mns")
result01_afttest_link_mns$p_value
result01_afttest_link_mns$p_std_value
# afttestplot(result01_afttest_link_mns,std="unstd")
# afttestplot(result01_afttest_link_mns,std="std")

# ------------------------------------ form ------------------------------------
result01_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~bili+protime+albumin+age+edema+trt,path=path,testtype="form",eqType="mns",form="bili")
result01_afttest_form1_mns$p_value
result01_afttest_form1_mns$p_std_value
# afttestplot(result01_afttest_form1_mns,std="unstd")
# afttestplot(result01_afttest_form1_mns,std="std")

result01_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~bili+protime+albumin+age+edema+trt,path=path,testtype="form",eqType="mns",form="protime")
result01_afttest_form2_mns$p_value
result01_afttest_form2_mns$p_std_value
# afttestplot(result01_afttest_form2_mns,std="unstd")
# afttestplot(result01_afttest_form2_mns,std="std")

result01_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~bili+protime+albumin+age+edema+trt,path=path,testtype="form",eqType="mns",form="albumin")
result01_afttest_form3_mns$p_value
result01_afttest_form3_mns$p_std_value
# afttestplot(result01_afttest_form3_mns,std="unstd")
# afttestplot(result01_afttest_form3_mns,std="std")

result01_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~bili+protime+albumin+age+edema+trt,path=path,testtype="form",eqType="mns",form="age")
result01_afttest_form4_mns$p_value
result01_afttest_form4_mns$p_std_value
# afttestplot(result01_afttest_form4_mns,std="unstd")
# afttestplot(result01_afttest_form4_mns,std="std")

result01_afttest_form5_mns=afttest(Surv(X_pbc,D_pbc)~bili+protime+albumin+age+edema+trt,path=path,testtype="form",eqType="mns",form="edema")
result01_afttest_form5_mns$p_value
result01_afttest_form5_mns$p_std_value
# afttestplot(result01_afttest_form5_mns,std="unstd")
# afttestplot(result01_afttest_form5_mns,std="std")

# ------------------------------------------------------------------------------
# ------------- Covariates: log_bili+protime+albumin+age+edema+trt -------------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
result02_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+albumin+age+edema+trt,path=path,testtype="omni",eqType="mns")
result02_afttest_omni_mns$p_value
result02_afttest_omni_mns$p_std_value
# afttestplot(result02_afttest_omni_mns,std="unstd")
# afttestplot(result02_afttest_omni_mns,std="std")

# ------------------------------------ link ------------------------------------
result02_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+albumin+age+edema+trt,path=path,testtype="link",eqType="mns")
result02_afttest_link_mns$p_value
result02_afttest_link_mns$p_std_value
# afttestplot(result02_afttest_link_mns,std="unstd")
# afttestplot(result02_afttest_link_mns,std="std")

# ------------------------------------ form ------------------------------------
result02_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+albumin+age+edema+trt,path=path,testtype="form",eqType="mns",form="log_bili")
result02_afttest_form1_mns$p_value
result02_afttest_form1_mns$p_std_value
# afttestplot(result02_afttest_form1_mns,std="unstd")
# afttestplot(result02_afttest_form1_mns,std="std")

result02_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+albumin+age+edema+trt,path=path,testtype="form",eqType="mns",form="protime")
result02_afttest_form2_mns$p_value
result02_afttest_form2_mns$p_std_value
# afttestplot(result02_afttest_form2_mns,std="unstd")
# afttestplot(result02_afttest_form2_mns,std="std")

result02_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+albumin+age+edema+trt,path=path,testtype="form",eqType="mns",form="albumin")
result02_afttest_form3_mns$p_value
result02_afttest_form3_mns$p_std_value
# afttestplot(result02_afttest_form3_mns,std="unstd")
# afttestplot(result02_afttest_form3_mns,std="std")

result02_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+albumin+age+edema+trt,path=path,testtype="form",eqType="mns",form="age")
result02_afttest_form4_mns$p_value
result02_afttest_form4_mns$p_std_value
# afttestplot(result02_afttest_form4_mns,std="unstd")
# afttestplot(result02_afttest_form4_mns,std="std")

result02_afttest_form5_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+albumin+age+edema+trt,path=path,testtype="form",eqType="mns",form="edema")
result02_afttest_form5_mns$p_value
result02_afttest_form5_mns$p_std_value
# afttestplot(result02_afttest_form5_mns,std="unstd")
# afttestplot(result02_afttest_form5_mns,std="std")

# ------------------------------------------------------------------------------
# ------------- Covariates: bili+log_protime+albumin+age+edema+trt -------------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
result03_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+albumin+age+edema+trt,path=path,testtype="omni",eqType="mns")
result03_afttest_omni_mns$p_value
result03_afttest_omni_mns$p_std_value
# afttestplot(result03_afttest_omni_mns,std="unstd")
# afttestplot(result03_afttest_omni_mns,std="std")

# ------------------------------------ link ------------------------------------
result03_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+albumin+age+edema+trt,path=path,testtype="link",eqType="mns")
result03_afttest_link_mns$p_value
result03_afttest_link_mns$p_std_value
# afttestplot(result03_afttest_link_mns,std="unstd")
# afttestplot(result03_afttest_link_mns,std="std")

# ------------------------------------ form ------------------------------------
result03_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+albumin+age+edema+trt,path=path,testtype="form",eqType="mns",form="bili")
result03_afttest_form1_mns$p_value
result03_afttest_form1_mns$p_std_value
# afttestplot(result03_afttest_form1_mns,std="unstd")
# afttestplot(result03_afttest_form1_mns,std="std")

result03_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+albumin+age+edema+trt,path=path,testtype="form",eqType="mns",form="log_protime")
result03_afttest_form2_mns$p_value
result03_afttest_form2_mns$p_std_value
# afttestplot(result03_afttest_form2_mns,std="unstd")
# afttestplot(result03_afttest_form2_mns,std="std")

result03_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+albumin+age+edema+trt,path=path,testtype="form",eqType="mns",form="albumin")
result03_afttest_form3_mns$p_value
result03_afttest_form3_mns$p_std_value
# afttestplot(result03_afttest_form3_mns,std="unstd")
# afttestplot(result03_afttest_form3_mns,std="std")

result03_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+albumin+age+edema+trt,path=path,testtype="form",eqType="mns",form="age")
result03_afttest_form4_mns$p_value
result03_afttest_form4_mns$p_std_value
# afttestplot(result03_afttest_form4_mns,std="unstd")
# afttestplot(result03_afttest_form4_mns,std="std")

result03_afttest_form5_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+albumin+age+edema+trt,path=path,testtype="form",eqType="mns",form="edema")
result03_afttest_form5_mns$p_value
result03_afttest_form5_mns$p_std_value
# afttestplot(result03_afttest_form5_mns,std="unstd")
# afttestplot(result03_afttest_form5_mns,std="std")

# ------------------------------------------------------------------------------
# ------------- Covariates: bili+protime+log_albumin+age+edema+trt -------------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
result04_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~bili+protime+log_albumin+age+edema+trt,path=path,testtype="omni",eqType="mns")
result04_afttest_omni_mns$p_value
result04_afttest_omni_mns$p_std_value
# afttestplot(result04_afttest_omni_mns,std="unstd")
# afttestplot(result04_afttest_omni_mns,std="std")

# ------------------------------------ link ------------------------------------
result04_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~bili+protime+log_albumin+age+edema+trt,path=path,testtype="link",eqType="mns")
result04_afttest_link_mns$p_value
result04_afttest_link_mns$p_std_value
# afttestplot(result04_afttest_link_mns,std="unstd")
# afttestplot(result04_afttest_link_mns,std="std")

# ------------------------------------ form ------------------------------------
result04_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~bili+protime+log_albumin+age+edema+trt,path=path,testtype="form",eqType="mns",form="bili")
result04_afttest_form1_mns$p_value
result04_afttest_form1_mns$p_std_value
# afttestplot(result04_afttest_form1_mns,std="unstd")
# afttestplot(result04_afttest_form1_mns,std="std")

result04_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~bili+protime+log_albumin+age+edema+trt,path=path,testtype="form",eqType="mns",form="protime")
result04_afttest_form2_mns$p_value
result04_afttest_form2_mns$p_std_value
# afttestplot(result04_afttest_form2_mns,std="unstd")
# afttestplot(result04_afttest_form2_mns,std="std")

result04_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~bili+protime+log_albumin+age+edema+trt,path=path,testtype="form",eqType="mns",form="log_albumin")
result04_afttest_form3_mns$p_value
result04_afttest_form3_mns$p_std_value
# afttestplot(result04_afttest_form3_mns,std="unstd")
# afttestplot(result04_afttest_form3_mns,std="std")

result04_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~bili+protime+log_albumin+age+edema+trt,path=path,testtype="form",eqType="mns",form="age")
result04_afttest_form4_mns$p_value
result04_afttest_form4_mns$p_std_value
# afttestplot(result04_afttest_form4_mns,std="unstd")
# afttestplot(result04_afttest_form4_mns,std="std")

result04_afttest_form5_mns=afttest(Surv(X_pbc,D_pbc)~bili+protime+log_albumin+age+edema+trt,path=path,testtype="form",eqType="mns",form="edema")
result04_afttest_form5_mns$p_value
result04_afttest_form5_mns$p_std_value
# afttestplot(result04_afttest_form5_mns,std="unstd")
# afttestplot(result04_afttest_form5_mns,std="std")

# ------------------------------------------------------------------------------
# ------------- Covariates: bili+protime+albumin+log_age+edema+trt -------------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
result05_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~bili+protime+albumin+log_age+edema+trt,path=path,testtype="omni",eqType="mns")
result05_afttest_omni_mns$p_value
result05_afttest_omni_mns$p_std_value
# afttestplot(result05_afttest_omni_mns,std="unstd")
# afttestplot(result05_afttest_omni_mns,std="std")

# ------------------------------------ link ------------------------------------
result05_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~bili+protime+albumin+log_age+edema+trt,path=path,testtype="link",eqType="mns")
result05_afttest_link_mns$p_value
result05_afttest_link_mns$p_std_value
# afttestplot(result05_afttest_link_mns,std="unstd")
# afttestplot(result05_afttest_link_mns,std="std")

# ------------------------------------ form ------------------------------------
result05_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~bili+protime+albumin+log_age+edema+trt,path=path,testtype="form",eqType="mns",form="bili")
result05_afttest_form1_mns$p_value
result05_afttest_form1_mns$p_std_value
# afttestplot(result05_afttest_form1_mns,std="unstd")
# afttestplot(result05_afttest_form1_mns,std="std")

result05_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~bili+protime+albumin+log_age+edema+trt,path=path,testtype="form",eqType="mns",form="protime")
result05_afttest_form2_mns$p_value
result05_afttest_form2_mns$p_std_value
# afttestplot(result05_afttest_form2_mns,std="unstd")
# afttestplot(result05_afttest_form2_mns,std="std")

result05_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~bili+protime+albumin+log_age+edema+trt,path=path,testtype="form",eqType="mns",form="albumin")
result05_afttest_form3_mns$p_value
result05_afttest_form3_mns$p_std_value
# afttestplot(result05_afttest_form3_mns,std="unstd")
# afttestplot(result05_afttest_form3_mns,std="std")

result05_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~bili+protime+albumin+log_age+edema+trt,path=path,testtype="form",eqType="mns",form="log_age")
result05_afttest_form4_mns$p_value
result05_afttest_form4_mns$p_std_value
# afttestplot(result05_afttest_form4_mns,std="unstd")
# afttestplot(result05_afttest_form4_mns,std="std")

result05_afttest_form5_mns=afttest(Surv(X_pbc,D_pbc)~bili+protime+albumin+log_age+edema+trt,path=path,testtype="form",eqType="mns",form="edema")
result05_afttest_form5_mns$p_value
result05_afttest_form5_mns$p_std_value
# afttestplot(result05_afttest_form5_mns,std="unstd")
# afttestplot(result05_afttest_form5_mns,std="std")

# ------------------------------------------------------------------------------
# ----------- Covariates: log_bili+log_protime+albumin+age+edema+trt -----------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
result06_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+albumin+age+edema+trt,path=path,testtype="omni",eqType="mns")
result06_afttest_omni_mns$p_value
result06_afttest_omni_mns$p_std_value
# afttestplot(result06_afttest_omni_mns,std="unstd")
# afttestplot(result06_afttest_omni_mns,std="std")

# ------------------------------------ link ------------------------------------
result06_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+albumin+age+edema+trt,path=path,testtype="link",eqType="mns")
result06_afttest_link_mns$p_value
result06_afttest_link_mns$p_std_value
# afttestplot(result06_afttest_link_mns,std="unstd")
# afttestplot(result06_afttest_link_mns,std="std")

# ------------------------------------ form ------------------------------------
result06_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+albumin+age+edema+trt,path=path,testtype="form",eqType="mns",form="log_bili")
result06_afttest_form1_mns$p_value
result06_afttest_form1_mns$p_std_value
# afttestplot(result06_afttest_form1_mns,std="unstd")
# afttestplot(result06_afttest_form1_mns,std="std")

result06_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+albumin+age+edema+trt,path=path,testtype="form",eqType="mns",form="log_protime")
result06_afttest_form2_mns$p_value
result06_afttest_form2_mns$p_std_value
# afttestplot(result06_afttest_form2_mns,std="unstd")
# afttestplot(result06_afttest_form2_mns,std="std")

result06_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+albumin+age+edema+trt,path=path,testtype="form",eqType="mns",form="albumin")
result06_afttest_form3_mns$p_value
result06_afttest_form3_mns$p_std_value
# afttestplot(result06_afttest_form3_mns,std="unstd")
# afttestplot(result06_afttest_form3_mns,std="std")

result06_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+albumin+age+edema+trt,path=path,testtype="form",eqType="mns",form="age")
result06_afttest_form4_mns$p_value
result06_afttest_form4_mns$p_std_value
# afttestplot(result06_afttest_form4_mns,std="unstd")
# afttestplot(result06_afttest_form4_mns,std="std")

result06_afttest_form5_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+albumin+age+edema+trt,path=path,testtype="form",eqType="mns",form="edema")
result06_afttest_form5_mns$p_value
result06_afttest_form5_mns$p_std_value
# afttestplot(result06_afttest_form5_mns,std="unstd")
# afttestplot(result06_afttest_form5_mns,std="std")

# ------------------------------------------------------------------------------
# ----------- Covariates: log_bili+protime+log_albumin+age+edema+trt -----------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
result07_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+log_albumin+age+edema+trt,path=path,testtype="omni",eqType="mns")
result07_afttest_omni_mns$p_value
result07_afttest_omni_mns$p_std_value
# afttestplot(result07_afttest_omni_mns,std="unstd")
# afttestplot(result07_afttest_omni_mns,std="std")

# ------------------------------------ link ------------------------------------
result07_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+log_albumin+age+edema+trt,path=path,testtype="link",eqType="mns")
result07_afttest_link_mns$p_value
result07_afttest_link_mns$p_std_value
# afttestplot(result07_afttest_link_mns,std="unstd")
# afttestplot(result07_afttest_link_mns,std="std")

# ------------------------------------ form ------------------------------------
result07_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+log_albumin+age+edema+trt,path=path,testtype="form",eqType="mns",form="log_bili")
result07_afttest_form1_mns$p_value
result07_afttest_form1_mns$p_std_value
# afttestplot(result07_afttest_form1_mns,std="unstd")
# afttestplot(result07_afttest_form1_mns,std="std")

result07_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+log_albumin+age+edema+trt,path=path,testtype="form",eqType="mns",form="protime")
result07_afttest_form2_mns$p_value
result07_afttest_form2_mns$p_std_value
# afttestplot(result07_afttest_form2_mns,std="unstd")
# afttestplot(result07_afttest_form2_mns,std="std")

result07_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+log_albumin+age+edema+trt,path=path,testtype="form",eqType="mns",form="log_albumin")
result07_afttest_form3_mns$p_value
result07_afttest_form3_mns$p_std_value
# afttestplot(result07_afttest_form3_mns,std="unstd")
# afttestplot(result07_afttest_form3_mns,std="std")

result07_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+log_albumin+age+edema+trt,path=path,testtype="form",eqType="mns",form="age")
result07_afttest_form4_mns$p_value
result07_afttest_form4_mns$p_std_value
# afttestplot(result07_afttest_form4_mns,std="unstd")
# afttestplot(result07_afttest_form4_mns,std="std")

result07_afttest_form5_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+log_albumin+age+edema+trt,path=path,testtype="form",eqType="mns",form="edema")
result07_afttest_form5_mns$p_value
result07_afttest_form5_mns$p_std_value
# afttestplot(result07_afttest_form5_mns,std="unstd")
# afttestplot(result07_afttest_form5_mns,std="std")

# ------------------------------------------------------------------------------
# ----------- Covariates: log_bili+protime+albumin+log_age+edema+trt -----------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
result08_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+albumin+log_age+edema+trt,path=path,testtype="omni",eqType="mns")
result08_afttest_omni_mns$p_value
result08_afttest_omni_mns$p_std_value
# afttestplot(result08_afttest_omni_mns,std="unstd")
# afttestplot(result08_afttest_omni_mns,std="std")

# ------------------------------------ link ------------------------------------
result08_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+albumin+log_age+edema+trt,path=path,testtype="link",eqType="mns")
result08_afttest_link_mns$p_value
result08_afttest_link_mns$p_std_value
# afttestplot(result08_afttest_link_mns,std="unstd")
# afttestplot(result08_afttest_link_mns,std="std")

# ------------------------------------ form ------------------------------------
result08_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+albumin+log_age+edema+trt,path=path,testtype="form",eqType="mns",form="log_bili")
result08_afttest_form1_mns$p_value
result08_afttest_form1_mns$p_std_value
# afttestplot(result08_afttest_form1_mns,std="unstd")
# afttestplot(result08_afttest_form1_mns,std="std")

result08_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+albumin+log_age+edema+trt,path=path,testtype="form",eqType="mns",form="protime")
result08_afttest_form2_mns$p_value
result08_afttest_form2_mns$p_std_value
# afttestplot(result08_afttest_form2_mns,std="unstd")
# afttestplot(result08_afttest_form2_mns,std="std")

result08_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+albumin+log_age+edema+trt,path=path,testtype="form",eqType="mns",form="albumin")
result08_afttest_form3_mns$p_value
result08_afttest_form3_mns$p_std_value
# afttestplot(result08_afttest_form3_mns,std="unstd")
# afttestplot(result08_afttest_form3_mns,std="std")

result08_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+albumin+log_age+edema+trt,path=path,testtype="form",eqType="mns",form="log_age")
result08_afttest_form4_mns$p_value
result08_afttest_form4_mns$p_std_value
# afttestplot(result08_afttest_form4_mns,std="unstd")
# afttestplot(result08_afttest_form4_mns,std="std")

result08_afttest_form5_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+albumin+log_age+edema+trt,path=path,testtype="form",eqType="mns",form="edema")
result08_afttest_form5_mns$p_value
result08_afttest_form5_mns$p_std_value
# afttestplot(result08_afttest_form5_mns,std="unstd")
# afttestplot(result08_afttest_form5_mns,std="std")

# ------------------------------------------------------------------------------
# ----------- Covariates: bili+log_protime+log_albumin+age+edema+trt -----------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
result09_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+log_albumin+age+edema+trt,path=path,testtype="omni",eqType="mns")
result09_afttest_omni_mns$p_value
result09_afttest_omni_mns$p_std_value
# afttestplot(result09_afttest_omni_mns,std="unstd")
# afttestplot(result09_afttest_omni_mns,std="std")

# ------------------------------------ link ------------------------------------
result09_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+log_albumin+age+edema+trt,path=path,testtype="link",eqType="mns")
result09_afttest_link_mns$p_value
result09_afttest_link_mns$p_std_value
# afttestplot(result09_afttest_link_mns,std="unstd")
# afttestplot(result09_afttest_link_mns,std="std")

# ------------------------------------ form ------------------------------------
result09_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+log_albumin+age+edema+trt,path=path,testtype="form",eqType="mns",form="bili")
result09_afttest_form1_mns$p_value
result09_afttest_form1_mns$p_std_value
# afttestplot(result09_afttest_form1_mns,std="unstd")
# afttestplot(result09_afttest_form1_mns,std="std")

result09_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+log_albumin+age+edema+trt,path=path,testtype="form",eqType="mns",form="log_protime")
result09_afttest_form2_mns$p_value
result09_afttest_form2_mns$p_std_value
# afttestplot(result09_afttest_form2_mns,std="unstd")
# afttestplot(result09_afttest_form2_mns,std="std")

result09_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+log_albumin+age+edema+trt,path=path,testtype="form",eqType="mns",form="log_albumin")
result09_afttest_form3_mns$p_value
result09_afttest_form3_mns$p_std_value
# afttestplot(result09_afttest_form3_mns,std="unstd")
# afttestplot(result09_afttest_form3_mns,std="std")

result09_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+log_albumin+age+edema+trt,path=path,testtype="form",eqType="mns",form="age")
result09_afttest_form4_mns$p_value
result09_afttest_form4_mns$p_std_value
# afttestplot(result09_afttest_form4_mns,std="unstd")
# afttestplot(result09_afttest_form4_mns,std="std")

result09_afttest_form5_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+log_albumin+age+edema+trt,path=path,testtype="form",eqType="mns",form="edema")
result09_afttest_form5_mns$p_value
result09_afttest_form5_mns$p_std_value
# afttestplot(result09_afttest_form5_mns,std="unstd")
# afttestplot(result09_afttest_form5_mns,std="std")

# ------------------------------------------------------------------------------
# ----------- Covariates: bili+log_protime+albumin+log_age+edema+trt -----------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
result10_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+albumin+log_age+edema+trt,path=path,testtype="omni",eqType="mns")
result10_afttest_omni_mns$p_value
result10_afttest_omni_mns$p_std_value
# afttestplot(result10_afttest_omni_mns,std="unstd")
# afttestplot(result10_afttest_omni_mns,std="std")

# ------------------------------------ link ------------------------------------
result10_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+albumin+log_age+edema+trt,path=path,testtype="link",eqType="mns")
result10_afttest_link_mns$p_value
result10_afttest_link_mns$p_std_value
# afttestplot(result10_afttest_link_mns,std="unstd")
# afttestplot(result10_afttest_link_mns,std="std")

# ------------------------------------ form ------------------------------------
result10_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+albumin+log_age+edema+trt,path=path,testtype="form",eqType="mns",form="bili")
result10_afttest_form1_mns$p_value
result10_afttest_form1_mns$p_std_value
# afttestplot(result10_afttest_form1_mns,std="unstd")
# afttestplot(result10_afttest_form1_mns,std="std")

result10_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+albumin+log_age+edema+trt,path=path,testtype="form",eqType="mns",form="log_protime")
result10_afttest_form2_mns$p_value
result10_afttest_form2_mns$p_std_value
# afttestplot(result10_afttest_form2_mns,std="unstd")
# afttestplot(result10_afttest_form2_mns,std="std")

result10_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+albumin+log_age+edema+trt,path=path,testtype="form",eqType="mns",form="albumin")
result10_afttest_form3_mns$p_value
result10_afttest_form3_mns$p_std_value
# afttestplot(result10_afttest_form3_mns,std="unstd")
# afttestplot(result10_afttest_form3_mns,std="std")

result10_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+albumin+log_age+edema+trt,path=path,testtype="form",eqType="mns",form="log_age")
result10_afttest_form4_mns$p_value
result10_afttest_form4_mns$p_std_value
# afttestplot(result10_afttest_form4_mns,std="unstd")
# afttestplot(result10_afttest_form4_mns,std="std")

result10_afttest_form5_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+albumin+log_age+edema+trt,path=path,testtype="form",eqType="mns",form="edema")
result10_afttest_form5_mns$p_value
result10_afttest_form5_mns$p_std_value
# afttestplot(result10_afttest_form5_mns,std="unstd")
# afttestplot(result10_afttest_form5_mns,std="std")

# ------------------------------------------------------------------------------
# ----------- Covariates: bili+protime+log_albumin+log_age+edema+trt -----------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
result11_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~bili+protime+log_albumin+log_age+edema+trt,path=path,testtype="omni",eqType="mns")
result11_afttest_omni_mns$p_value
result11_afttest_omni_mns$p_std_value
# afttestplot(result11_afttest_omni_mns,std="unstd")
# afttestplot(result11_afttest_omni_mns,std="std")

# ------------------------------------ link ------------------------------------
result11_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~bili+protime+log_albumin+log_age+edema+trt,path=path,testtype="link",eqType="mns")
result11_afttest_link_mns$p_value
result11_afttest_link_mns$p_std_value
# afttestplot(result11_afttest_link_mns,std="unstd")
# afttestplot(result11_afttest_link_mns,std="std")

# ------------------------------------ form ------------------------------------
result11_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~bili+protime+log_albumin+log_age+edema+trt,path=path,testtype="form",eqType="mns",form="bili")
result11_afttest_form1_mns$p_value
result11_afttest_form1_mns$p_std_value
# afttestplot(result11_afttest_form1_mns,std="unstd")
# afttestplot(result11_afttest_form1_mns,std="std")

result11_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~bili+protime+log_albumin+log_age+edema+trt,path=path,testtype="form",eqType="mns",form="protime")
result11_afttest_form2_mns$p_value
result11_afttest_form2_mns$p_std_value
# afttestplot(result11_afttest_form2_mns,std="unstd")
# afttestplot(result11_afttest_form2_mns,std="std")

result11_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~bili+protime+log_albumin+log_age+edema+trt,path=path,testtype="form",eqType="mns",form="log_albumin")
result11_afttest_form3_mns$p_value
result11_afttest_form3_mns$p_std_value
# afttestplot(result11_afttest_form3_mns,std="unstd")
# afttestplot(result11_afttest_form3_mns,std="std")

result11_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~bili+protime+log_albumin+log_age+edema+trt,path=path,testtype="form",eqType="mns",form="log_age")
result11_afttest_form4_mns$p_value
result11_afttest_form4_mns$p_std_value
# afttestplot(result11_afttest_form4_mns,std="unstd")
# afttestplot(result11_afttest_form4_mns,std="std")

result11_afttest_form5_mns=afttest(Surv(X_pbc,D_pbc)~bili+protime+log_albumin+log_age+edema+trt,path=path,testtype="form",eqType="mns",form="edema")
result11_afttest_form5_mns$p_value
result11_afttest_form5_mns$p_std_value
# afttestplot(result11_afttest_form5_mns,std="unstd")
# afttestplot(result11_afttest_form5_mns,std="std")

# ------------------------------------------------------------------------------
# --------- Covariates: log_bili+log_protime+log_albumin+age+edema+trt ---------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
result12_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+log_albumin+age+edema+trt,path=path,testtype="omni",eqType="mns")
result12_afttest_omni_mns$p_value
result12_afttest_omni_mns$p_std_value
# afttestplot(result12_afttest_omni_mns,std="unstd")
# afttestplot(result12_afttest_omni_mns,std="std")

# ------------------------------------ link ------------------------------------
result12_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+log_albumin+age+edema+trt,path=path,testtype="link",eqType="mns")
result12_afttest_link_mns$p_value
result12_afttest_link_mns$p_std_value
# afttestplot(result12_afttest_link_mns,std="unstd")
# afttestplot(result12_afttest_link_mns,std="std")

# ------------------------------------ form ------------------------------------
result12_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+log_albumin+age+edema+trt,path=path,testtype="form",eqType="mns",form="log_bili")
result12_afttest_form1_mns$p_value
result12_afttest_form1_mns$p_std_value
# afttestplot(result12_afttest_form1_mns,std="unstd")
# afttestplot(result12_afttest_form1_mns,std="std")

result12_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+log_albumin+age+edema+trt,path=path,testtype="form",eqType="mns",form="log_protime")
result12_afttest_form2_mns$p_value
result12_afttest_form2_mns$p_std_value
# afttestplot(result12_afttest_form2_mns,std="unstd")
# afttestplot(result12_afttest_form2_mns,std="std")

result12_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+log_albumin+age+edema+trt,path=path,testtype="form",eqType="mns",form="log_albumin")
result12_afttest_form3_mns$p_value
result12_afttest_form3_mns$p_std_value
# afttestplot(result12_afttest_form3_mns,std="unstd")
# afttestplot(result12_afttest_form3_mns,std="std")

result12_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+log_albumin+age+edema+trt,path=path,testtype="form",eqType="mns",form="age")
result12_afttest_form4_mns$p_value
result12_afttest_form4_mns$p_std_value
# afttestplot(result12_afttest_form4_mns,std="unstd")
# afttestplot(result12_afttest_form4_mns,std="std")

result12_afttest_form5_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+log_albumin+age+edema+trt,path=path,testtype="form",eqType="mns",form="edema")
result12_afttest_form5_mns$p_value
result12_afttest_form5_mns$p_std_value
# afttestplot(result12_afttest_form5_mns,std="unstd")
# afttestplot(result12_afttest_form5_mns,std="std")

# ------------------------------------------------------------------------------
# --------- Covariates: log_bili+log_protime+albumin+log_age+edema+trt ---------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
result13_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+albumin+log_age+edema+trt,path=path,testtype="omni",eqType="mns")
result13_afttest_omni_mns$p_value
result13_afttest_omni_mns$p_std_value
# afttestplot(result13_afttest_omni_mns,std="unstd")
# afttestplot(result13_afttest_omni_mns,std="std")

# ------------------------------------ link ------------------------------------
result13_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+albumin+log_age+edema+trt,path=path,testtype="link",eqType="mns")
result13_afttest_link_mns$p_value
result13_afttest_link_mns$p_std_value
# afttestplot(result13_afttest_link_mns,std="unstd")
# afttestplot(result13_afttest_link_mns,std="std")

# ------------------------------------ form ------------------------------------
result13_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+albumin+log_age+edema+trt,path=path,testtype="form",eqType="mns",form="log_bili")
result13_afttest_form1_mns$p_value
result13_afttest_form1_mns$p_std_value
# afttestplot(result13_afttest_form1_mns,std="unstd")
# afttestplot(result13_afttest_form1_mns,std="std")

result13_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+albumin+log_age+edema+trt,path=path,testtype="form",eqType="mns",form="log_protime")
result13_afttest_form2_mns$p_value
result13_afttest_form2_mns$p_std_value
# afttestplot(result13_afttest_form2_mns,std="unstd")
# afttestplot(result13_afttest_form2_mns,std="std")

result13_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+albumin+log_age+edema+trt,path=path,testtype="form",eqType="mns",form="albumin")
result13_afttest_form3_mns$p_value
result13_afttest_form3_mns$p_std_value
# afttestplot(result13_afttest_form3_mns,std="unstd")
# afttestplot(result13_afttest_form3_mns,std="std")

result13_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+albumin+log_age+edema+trt,path=path,testtype="form",eqType="mns",form="log_age")
result13_afttest_form4_mns$p_value
result13_afttest_form4_mns$p_std_value
# afttestplot(result13_afttest_form4_mns,std="unstd")
# afttestplot(result13_afttest_form4_mns,std="std")

result13_afttest_form5_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+albumin+log_age+edema+trt,path=path,testtype="form",eqType="mns",form="edema")
result13_afttest_form5_mns$p_value
result13_afttest_form5_mns$p_std_value
# afttestplot(result13_afttest_form5_mns,std="unstd")
# afttestplot(result13_afttest_form5_mns,std="std")

# ------------------------------------------------------------------------------
# --------- Covariates: log_bili+protime+log_albumin+log_age+edema+trt ---------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
result14_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+log_albumin+log_age+edema+trt,path=path,testtype="omni",eqType="mns")
result14_afttest_omni_mns$p_value
result14_afttest_omni_mns$p_std_value
# afttestplot(result14_afttest_omni_mns,std="unstd")
# afttestplot(result14_afttest_omni_mns,std="std")

# ------------------------------------ link ------------------------------------
result14_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+log_albumin+log_age+edema+trt,path=path,testtype="link",eqType="mns")
result14_afttest_link_mns$p_value
result14_afttest_link_mns$p_std_value
# afttestplot(result14_afttest_link_mns,std="unstd")
# afttestplot(result14_afttest_link_mns,std="std")

# ------------------------------------ form ------------------------------------
result14_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+log_albumin+log_age+edema+trt,path=path,testtype="form",eqType="mns",form="log_bili")
result14_afttest_form1_mns$p_value
result14_afttest_form1_mns$p_std_value
# afttestplot(result14_afttest_form1_mns,std="unstd")
# afttestplot(result14_afttest_form1_mns,std="std")

result14_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+log_albumin+log_age+edema+trt,path=path,testtype="form",eqType="mns",form="protime")
result14_afttest_form2_mns$p_value
result14_afttest_form2_mns$p_std_value
# afttestplot(result14_afttest_form2_mns,std="unstd")
# afttestplot(result14_afttest_form2_mns,std="std")

result14_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+log_albumin+log_age+edema+trt,path=path,testtype="form",eqType="mns",form="log_albumin")
result14_afttest_form3_mns$p_value
result14_afttest_form3_mns$p_std_value
# afttestplot(result14_afttest_form3_mns,std="unstd")
# afttestplot(result14_afttest_form3_mns,std="std")

result14_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+log_albumin+log_age+edema+trt,path=path,testtype="form",eqType="mns",form="log_age")
result14_afttest_form4_mns$p_value
result14_afttest_form4_mns$p_std_value
# afttestplot(result14_afttest_form4_mns,std="unstd")
# afttestplot(result14_afttest_form4_mns,std="std")

result14_afttest_form5_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+log_albumin+log_age+edema+trt,path=path,testtype="form",eqType="mns",form="edema")
result14_afttest_form5_mns$p_value
result14_afttest_form5_mns$p_std_value
# afttestplot(result14_afttest_form5_mns,std="unstd")
# afttestplot(result14_afttest_form5_mns,std="std")

# ------------------------------------------------------------------------------
# --------- Covariates: bili+log_protime+log_albumin+log_age+edema+trt ---------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
result15_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+log_albumin+log_age+edema+trt,path=path,testtype="omni",eqType="mns")
result15_afttest_omni_mns$p_value
result15_afttest_omni_mns$p_std_value
# afttestplot(result15_afttest_omni_mns,std="unstd")
# afttestplot(result15_afttest_omni_mns,std="std")

# ------------------------------------ link ------------------------------------
result15_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+log_albumin+log_age+edema+trt,path=path,testtype="link",eqType="mns")
result15_afttest_link_mns$p_value
result15_afttest_link_mns$p_std_value
# afttestplot(result15_afttest_link_mns,std="unstd")
# afttestplot(result15_afttest_link_mns,std="std")

# ------------------------------------ form ------------------------------------
result15_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+log_albumin+log_age+edema+trt,path=path,testtype="form",eqType="mns",form="bili")
result15_afttest_form1_mns$p_value
result15_afttest_form1_mns$p_std_value
# afttestplot(result15_afttest_form1_mns,std="unstd")
# afttestplot(result15_afttest_form1_mns,std="std")

result15_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+log_albumin+log_age+edema+trt,path=path,testtype="form",eqType="mns",form="log_protime")
result15_afttest_form2_mns$p_value
result15_afttest_form2_mns$p_std_value
# afttestplot(result15_afttest_form2_mns,std="unstd")
# afttestplot(result15_afttest_form2_mns,std="std")

result15_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+log_albumin+log_age+edema+trt,path=path,testtype="form",eqType="mns",form="log_albumin")
result15_afttest_form3_mns$p_value
result15_afttest_form3_mns$p_std_value
# afttestplot(result15_afttest_form3_mns,std="unstd")
# afttestplot(result15_afttest_form3_mns,std="std")

result15_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+log_albumin+log_age+edema+trt,path=path,testtype="form",eqType="mns",form="log_age")
result15_afttest_form4_mns$p_value
result15_afttest_form4_mns$p_std_value
# afttestplot(result15_afttest_form4_mns,std="unstd")
# afttestplot(result15_afttest_form4_mns,std="std")

result15_afttest_form5_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+log_albumin+log_age+edema+trt,path=path,testtype="form",eqType="mns",form="edema")
result15_afttest_form5_mns$p_value
result15_afttest_form5_mns$p_std_value
# afttestplot(result15_afttest_form5_mns,std="unstd")
# afttestplot(result15_afttest_form5_mns,std="std")

# ------------------------------------------------------------------------------
# ------- Covariates: log_bili+log_protime+log_albumin+log_age+edema+trt -------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
result16_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+log_albumin+log_age+edema+trt,path=path,testtype="omni",eqType="mns")
result16_afttest_omni_mns$p_value
result16_afttest_omni_mns$p_std_value
# afttestplot(result16_afttest_omni_mns,std="unstd")
# afttestplot(result16_afttest_omni_mns,std="std")

# ------------------------------------ link ------------------------------------
result16_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+log_albumin+log_age+edema+trt,path=path,testtype="link",eqType="mns")
result16_afttest_link_mns$p_value
result16_afttest_link_mns$p_std_value
# afttestplot(result16_afttest_link_mns,std="unstd")
# afttestplot(result16_afttest_link_mns,std="std")

# ------------------------------------ form ------------------------------------
result16_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+log_albumin+log_age+edema+trt,path=path,testtype="form",eqType="mns",form="log_bili")
result16_afttest_form1_mns$p_value
result16_afttest_form1_mns$p_std_value
# afttestplot(result16_afttest_form1_mns,std="unstd")
# afttestplot(result16_afttest_form1_mns,std="std")

result16_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+log_albumin+log_age+edema+trt,path=path,testtype="form",eqType="mns",form="log_protime")
result16_afttest_form2_mns$p_value
result16_afttest_form2_mns$p_std_value
# afttestplot(result16_afttest_form2_mns,std="unstd")
# afttestplot(result16_afttest_form2_mns,std="std")

result16_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+log_albumin+log_age+edema+trt,path=path,testtype="form",eqType="mns",form="log_albumin")
result16_afttest_form3_mns$p_value
result16_afttest_form3_mns$p_std_value
# afttestplot(result16_afttest_form3_mns,std="unstd")
# afttestplot(result16_afttest_form3_mns,std="std")

result16_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+log_albumin+log_age+edema+trt,path=path,testtype="form",eqType="mns",form="log_age")
result16_afttest_form4_mns$p_value
result16_afttest_form4_mns$p_std_value
# afttestplot(result16_afttest_form4_mns,std="unstd")
# afttestplot(result16_afttest_form4_mns,std="std")

result16_afttest_form5_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+log_albumin+log_age+edema+trt,path=path,testtype="form",eqType="mns",form="edema")
result16_afttest_form5_mns$p_value
result16_afttest_form5_mns$p_std_value
# afttestplot(result16_afttest_form5_mns,std="unstd")
# afttestplot(result16_afttest_form5_mns,std="std")

beta11=aftsrr(Surv(X_pbc,D_pbc)~log_bili+protime+albumin+log_age+edema+trt,eqType="mns")
summary(beta11)

beta16=aftsrr(Surv(X_pbc,D_pbc)~log_bili+log_protime+albumin+log_age+edema+trt,eqType="mns")
summary(beta16)

beta19=aftsrr(Surv(X_pbc,D_pbc)~log_bili+log_protime+log_albumin+log_age+edema+trt,eqType="mns")
summary(beta19)

# ------------------------------------------------------------------------------
# ------------------------------------ "mis" -----------------------------------
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# --------------- Covariates: bili+protime+albumin+age+edema+trt ---------------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
result01_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~bili+protime+albumin+age+edema+trt,path=path,testtype="omni",eqType="mis")
result01_afttest_omni_mis$p_value
result01_afttest_omni_mis$p_std_value
# afttestplot(result01_afttest_omni_mis,std="unstd")
# afttestplot(result01_afttest_omni_mis_mis,std="std")

# ------------------------------------ link ------------------------------------
result01_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~bili+protime+albumin+age+edema+trt,path=path,testtype="link",eqType="mis")
result01_afttest_link_mis$p_value
result01_afttest_link_mis$p_std_value
# afttestplot(result01_afttest_link_mis,std="unstd")
# afttestplot(result01_afttest_link_mis,std="std")

# ------------------------------------ form ------------------------------------
result01_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~bili+protime+albumin+age+edema+trt,path=path,testtype="form",eqType="mis",form="bili")
result01_afttest_form1_mis$p_value
result01_afttest_form1_mis$p_std_value
# afttestplot(result01_afttest_form1_mis,std="unstd")
# afttestplot(result01_afttest_form1_mis,std="std")

result01_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~bili+protime+albumin+age+edema+trt,path=path,testtype="form",eqType="mis",form="protime")
result01_afttest_form2_mis$p_value
result01_afttest_form2_mis$p_std_value
# afttestplot(result01_afttest_form2_mis,std="unstd")
# afttestplot(result01_afttest_form2_mis,std="std")

result01_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~bili+protime+albumin+age+edema+trt,path=path,testtype="form",eqType="mis",form="albumin")
result01_afttest_form3_mis$p_value
result01_afttest_form3_mis$p_std_value
# afttestplot(result01_afttest_form3_mis,std="unstd")
# afttestplot(result01_afttest_form3_mis,std="std")

result01_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~bili+protime+albumin+age+edema+trt,path=path,testtype="form",eqType="mis",form="age")
result01_afttest_form4_mis$p_value
result01_afttest_form4_mis$p_std_value
# afttestplot(result01_afttest_form4_mis,std="unstd")
# afttestplot(result01_afttest_form4_mis,std="std")

result01_afttest_form5_mis=afttest(Surv(X_pbc,D_pbc)~bili+protime+albumin+age+edema+trt,path=path,testtype="form",eqType="mis",form="edema")
result01_afttest_form5_mis$p_value
result01_afttest_form5_mis$p_std_value
# afttestplot(result01_afttest_form5_mis,std="unstd")
# afttestplot(result01_afttest_form5_mis,std="std")

# ------------------------------------------------------------------------------
# ------------- Covariates: log_bili+protime+albumin+age+edema+trt -------------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
result02_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+albumin+age+edema+trt,path=path,testtype="omni",eqType="mis")
result02_afttest_omni_mis$p_value
result02_afttest_omni_mis$p_std_value
# afttestplot(result02_afttest_omni_mis,std="unstd")
# afttestplot(result02_afttest_omni_mis,std="std")

# ------------------------------------ link ------------------------------------
result02_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+albumin+age+edema+trt,path=path,testtype="link",eqType="mis")
result02_afttest_link_mis$p_value
result02_afttest_link_mis$p_std_value
# afttestplot(result02_afttest_link_mis,std="unstd")
# afttestplot(result02_afttest_link_mis,std="std")

# ------------------------------------ form ------------------------------------
result02_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+albumin+age+edema+trt,path=path,testtype="form",eqType="mis",form="log_bili")
result02_afttest_form1_mis$p_value
result02_afttest_form1_mis$p_std_value
# afttestplot(result02_afttest_form1_mis,std="unstd")
# afttestplot(result02_afttest_form1_mis,std="std")

result02_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+albumin+age+edema+trt,path=path,testtype="form",eqType="mis",form="protime")
result02_afttest_form2_mis$p_value
result02_afttest_form2_mis$p_std_value
# afttestplot(result02_afttest_form2_mis,std="unstd")
# afttestplot(result02_afttest_form2_mis,std="std")

result02_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+albumin+age+edema+trt,path=path,testtype="form",eqType="mis",form="albumin")
result02_afttest_form3_mis$p_value
result02_afttest_form3_mis$p_std_value
# afttestplot(result02_afttest_form3_mis,std="unstd")
# afttestplot(result02_afttest_form3_mis,std="std")

result02_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+albumin+age+edema+trt,path=path,testtype="form",eqType="mis",form="age")
result02_afttest_form4_mis$p_value
result02_afttest_form4_mis$p_std_value
# afttestplot(result02_afttest_form4_mis,std="unstd")
# afttestplot(result02_afttest_form4_mis,std="std")

result02_afttest_form5_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+albumin+age+edema+trt,path=path,testtype="form",eqType="mis",form="edema")
result02_afttest_form5_mis$p_value
result02_afttest_form5_mis$p_std_value
# afttestplot(result02_afttest_form5_mis,std="unstd")
# afttestplot(result02_afttest_form5_mis,std="std")

# ------------------------------------------------------------------------------
# ------------- Covariates: bili+log_protime+albumin+age+edema+trt -------------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
result03_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+albumin+age+edema+trt,path=path,testtype="omni",eqType="mis")
result03_afttest_omni_mis$p_value
result03_afttest_omni_mis$p_std_value
# afttestplot(result03_afttest_omni_mis,std="unstd")
# afttestplot(result03_afttest_omni_mis,std="std")

# ------------------------------------ link ------------------------------------
result03_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+albumin+age+edema+trt,path=path,testtype="link",eqType="mis")
result03_afttest_link_mis$p_value
result03_afttest_link_mis$p_std_value
# afttestplot(result03_afttest_link_mis,std="unstd")
# afttestplot(result03_afttest_link_mis,std="std")

# ------------------------------------ form ------------------------------------
result03_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+albumin+age+edema+trt,path=path,testtype="form",eqType="mis",form="bili")
result03_afttest_form1_mis$p_value
result03_afttest_form1_mis$p_std_value
# afttestplot(result03_afttest_form1_mis,std="unstd")
# afttestplot(result03_afttest_form1_mis,std="std")

result03_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+albumin+age+edema+trt,path=path,testtype="form",eqType="mis",form="log_protime")
result03_afttest_form2_mis$p_value
result03_afttest_form2_mis$p_std_value
# afttestplot(result03_afttest_form2_mis,std="unstd")
# afttestplot(result03_afttest_form2_mis,std="std")

result03_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+albumin+age+edema+trt,path=path,testtype="form",eqType="mis",form="albumin")
result03_afttest_form3_mis$p_value
result03_afttest_form3_mis$p_std_value
# afttestplot(result03_afttest_form3_mis,std="unstd")
# afttestplot(result03_afttest_form3_mis,std="std")

result03_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+albumin+age+edema+trt,path=path,testtype="form",eqType="mis",form="age")
result03_afttest_form4_mis$p_value
result03_afttest_form4_mis$p_std_value
# afttestplot(result03_afttest_form4_mis,std="unstd")
# afttestplot(result03_afttest_form4_mis,std="std")

result03_afttest_form5_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+albumin+age+edema+trt,path=path,testtype="form",eqType="mis",form="edema")
result03_afttest_form5_mis$p_value
result03_afttest_form5_mis$p_std_value
# afttestplot(result03_afttest_form5_mis,std="unstd")
# afttestplot(result03_afttest_form5_mis,std="std")

# ------------------------------------------------------------------------------
# ------------- Covariates: bili+protime+log_albumin+age+edema+trt -------------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
result04_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~bili+protime+log_albumin+age+edema+trt,path=path,testtype="omni",eqType="mis")
result04_afttest_omni_mis$p_value
result04_afttest_omni_mis$p_std_value
# afttestplot(result04_afttest_omni_mis,std="unstd")
# afttestplot(result04_afttest_omni_mis,std="std")

# ------------------------------------ link ------------------------------------
result04_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~bili+protime+log_albumin+age+edema+trt,path=path,testtype="link",eqType="mis")
result04_afttest_link_mis$p_value
result04_afttest_link_mis$p_std_value
# afttestplot(result04_afttest_link_mis,std="unstd")
# afttestplot(result04_afttest_link_mis,std="std")

# ------------------------------------ form ------------------------------------
result04_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~bili+protime+log_albumin+age+edema+trt,path=path,testtype="form",eqType="mis",form="bili")
result04_afttest_form1_mis$p_value
result04_afttest_form1_mis$p_std_value
# afttestplot(result04_afttest_form1_mis,std="unstd")
# afttestplot(result04_afttest_form1_mis,std="std")

result04_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~bili+protime+log_albumin+age+edema+trt,path=path,testtype="form",eqType="mis",form="protime")
result04_afttest_form2_mis$p_value
result04_afttest_form2_mis$p_std_value
# afttestplot(result04_afttest_form2_mis,std="unstd")
# afttestplot(result04_afttest_form2_mis,std="std")

result04_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~bili+protime+log_albumin+age+edema+trt,path=path,testtype="form",eqType="mis",form="log_albumin")
result04_afttest_form3_mis$p_value
result04_afttest_form3_mis$p_std_value
# afttestplot(result04_afttest_form3_mis,std="unstd")
# afttestplot(result04_afttest_form3_mis,std="std")

result04_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~bili+protime+log_albumin+age+edema+trt,path=path,testtype="form",eqType="mis",form="age")
result04_afttest_form4_mis$p_value
result04_afttest_form4_mis$p_std_value
# afttestplot(result04_afttest_form4_mis,std="unstd")
# afttestplot(result04_afttest_form4_mis,std="std")

result04_afttest_form5_mis=afttest(Surv(X_pbc,D_pbc)~bili+protime+log_albumin+age+edema+trt,path=path,testtype="form",eqType="mis",form="edema")
result04_afttest_form5_mis$p_value
result04_afttest_form5_mis$p_std_value
# afttestplot(result04_afttest_form5_mis,std="unstd")
# afttestplot(result04_afttest_form5_mis,std="std")

# ------------------------------------------------------------------------------
# ------------- Covariates: bili+protime+albumin+log_age+edema+trt -------------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
result05_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~bili+protime+albumin+log_age+edema+trt,path=path,testtype="omni",eqType="mis")
result05_afttest_omni_mis$p_value
result05_afttest_omni_mis$p_std_value
# afttestplot(result05_afttest_omni_mis,std="unstd")
# afttestplot(result05_afttest_omni_mis,std="std")

# ------------------------------------ link ------------------------------------
result05_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~bili+protime+albumin+log_age+edema+trt,path=path,testtype="link",eqType="mis")
result05_afttest_link_mis$p_value
result05_afttest_link_mis$p_std_value
# afttestplot(result05_afttest_link_mis,std="unstd")
# afttestplot(result05_afttest_link_mis,std="std")

# ------------------------------------ form ------------------------------------
result05_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~bili+protime+albumin+log_age+edema+trt,path=path,testtype="form",eqType="mis",form="bili")
result05_afttest_form1_mis$p_value
result05_afttest_form1_mis$p_std_value
# afttestplot(result05_afttest_form1_mis,std="unstd")
# afttestplot(result05_afttest_form1_mis,std="std")

result05_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~bili+protime+albumin+log_age+edema+trt,path=path,testtype="form",eqType="mis",form="protime")
result05_afttest_form2_mis$p_value
result05_afttest_form2_mis$p_std_value
# afttestplot(result05_afttest_form2_mis,std="unstd")
# afttestplot(result05_afttest_form2_mis,std="std")

result05_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~bili+protime+albumin+log_age+edema+trt,path=path,testtype="form",eqType="mis",form="albumin")
result05_afttest_form3_mis$p_value
result05_afttest_form3_mis$p_std_value
# afttestplot(result05_afttest_form3_mis,std="unstd")
# afttestplot(result05_afttest_form3_mis,std="std")

result05_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~bili+protime+albumin+log_age+edema+trt,path=path,testtype="form",eqType="mis",form="log_age")
result05_afttest_form4_mis$p_value
result05_afttest_form4_mis$p_std_value
# afttestplot(result05_afttest_form4_mis,std="unstd")
# afttestplot(result05_afttest_form4_mis,std="std")

result05_afttest_form5_mis=afttest(Surv(X_pbc,D_pbc)~bili+protime+albumin+log_age+edema+trt,path=path,testtype="form",eqType="mis",form="edema")
result05_afttest_form5_mis$p_value
result05_afttest_form5_mis$p_std_value
# afttestplot(result05_afttest_form5_mis,std="unstd")
# afttestplot(result05_afttest_form5_mis,std="std")

# ------------------------------------------------------------------------------
# ----------- Covariates: log_bili+log_protime+albumin+age+edema+trt -----------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
result06_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+albumin+age+edema+trt,path=path,testtype="omni",eqType="mis")
result06_afttest_omni_mis$p_value
result06_afttest_omni_mis$p_std_value
# afttestplot(result06_afttest_omni_mis,std="unstd")
# afttestplot(result06_afttest_omni_mis,std="std")

# ------------------------------------ link ------------------------------------
result06_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+albumin+age+edema+trt,path=path,testtype="link",eqType="mis")
result06_afttest_link_mis$p_value
result06_afttest_link_mis$p_std_value
# afttestplot(result06_afttest_link_mis,std="unstd")
# afttestplot(result06_afttest_link_mis,std="std")

# ------------------------------------ form ------------------------------------
result06_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+albumin+age+edema+trt,path=path,testtype="form",eqType="mis",form="log_bili")
result06_afttest_form1_mis$p_value
result06_afttest_form1_mis$p_std_value
# afttestplot(result06_afttest_form1_mis,std="unstd")
# afttestplot(result06_afttest_form1_mis,std="std")

result06_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+albumin+age+edema+trt,path=path,testtype="form",eqType="mis",form="log_protime")
result06_afttest_form2_mis$p_value
result06_afttest_form2_mis$p_std_value
# afttestplot(result06_afttest_form2_mis,std="unstd")
# afttestplot(result06_afttest_form2_mis,std="std")

result06_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+albumin+age+edema+trt,path=path,testtype="form",eqType="mis",form="albumin")
result06_afttest_form3_mis$p_value
result06_afttest_form3_mis$p_std_value
# afttestplot(result06_afttest_form3_mis,std="unstd")
# afttestplot(result06_afttest_form3_mis,std="std")

result06_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+albumin+age+edema+trt,path=path,testtype="form",eqType="mis",form="age")
result06_afttest_form4_mis$p_value
result06_afttest_form4_mis$p_std_value
# afttestplot(result06_afttest_form4_mis,std="unstd")
# afttestplot(result06_afttest_form4_mis,std="std")

result06_afttest_form5_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+albumin+age+edema+trt,path=path,testtype="form",eqType="mis",form="edema")
result06_afttest_form5_mis$p_value
result06_afttest_form5_mis$p_std_value
# afttestplot(result06_afttest_form5_mis,std="unstd")
# afttestplot(result06_afttest_form5_mis,std="std")

# ------------------------------------------------------------------------------
# ----------- Covariates: log_bili+protime+log_albumin+age+edema+trt -----------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
result07_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+log_albumin+age+edema+trt,path=path,testtype="omni",eqType="mis")
result07_afttest_omni_mis$p_value
result07_afttest_omni_mis$p_std_value
# afttestplot(result07_afttest_omni_mis,std="unstd")
# afttestplot(result07_afttest_omni_mis,std="std")

# ------------------------------------ link ------------------------------------
result07_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+log_albumin+age+edema+trt,path=path,testtype="link",eqType="mis")
result07_afttest_link_mis$p_value
result07_afttest_link_mis$p_std_value
# afttestplot(result07_afttest_link_mis,std="unstd")
# afttestplot(result07_afttest_link_mis,std="std")

# ------------------------------------ form ------------------------------------
result07_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+log_albumin+age+edema+trt,path=path,testtype="form",eqType="mis",form="log_bili")
result07_afttest_form1_mis$p_value
result07_afttest_form1_mis$p_std_value
# afttestplot(result07_afttest_form1_mis,std="unstd")
# afttestplot(result07_afttest_form1_mis,std="std")

result07_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+log_albumin+age+edema+trt,path=path,testtype="form",eqType="mis",form="protime")
result07_afttest_form2_mis$p_value
result07_afttest_form2_mis$p_std_value
# afttestplot(result07_afttest_form2_mis,std="unstd")
# afttestplot(result07_afttest_form2_mis,std="std")

result07_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+log_albumin+age+edema+trt,path=path,testtype="form",eqType="mis",form="log_albumin")
result07_afttest_form3_mis$p_value
result07_afttest_form3_mis$p_std_value
# afttestplot(result07_afttest_form3_mis,std="unstd")
# afttestplot(result07_afttest_form3_mis,std="std")

result07_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+log_albumin+age+edema+trt,path=path,testtype="form",eqType="mis",form="age")
result07_afttest_form4_mis$p_value
result07_afttest_form4_mis$p_std_value
# afttestplot(result07_afttest_form4_mis,std="unstd")
# afttestplot(result07_afttest_form4_mis,std="std")

result07_afttest_form5_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+log_albumin+age+edema+trt,path=path,testtype="form",eqType="mis",form="edema")
result07_afttest_form5_mis$p_value
result07_afttest_form5_mis$p_std_value
# afttestplot(result07_afttest_form5_mis,std="unstd")
# afttestplot(result07_afttest_form5_mis,std="std")

# ------------------------------------------------------------------------------
# ----------- Covariates: log_bili+protime+albumin+log_age+edema+trt -----------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
result08_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+albumin+log_age+edema+trt,path=path,testtype="omni",eqType="mis")
result08_afttest_omni_mis$p_value
result08_afttest_omni_mis$p_std_value
# afttestplot(result08_afttest_omni_mis,std="unstd")
# afttestplot(result08_afttest_omni_mis,std="std")

# ------------------------------------ link ------------------------------------
result08_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+albumin+log_age+edema+trt,path=path,testtype="link",eqType="mis")
result08_afttest_link_mis$p_value
result08_afttest_link_mis$p_std_value
# afttestplot(result08_afttest_link_mis,std="unstd")
# afttestplot(result08_afttest_link_mis,std="std")

# ------------------------------------ form ------------------------------------
result08_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+albumin+log_age+edema+trt,path=path,testtype="form",eqType="mis",form="log_bili")
result08_afttest_form1_mis$p_value
result08_afttest_form1_mis$p_std_value
# afttestplot(result08_afttest_form1_mis,std="unstd")
# afttestplot(result08_afttest_form1_mis,std="std")

result08_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+albumin+log_age+edema+trt,path=path,testtype="form",eqType="mis",form="protime")
result08_afttest_form2_mis$p_value
result08_afttest_form2_mis$p_std_value
# afttestplot(result08_afttest_form2_mis,std="unstd")
# afttestplot(result08_afttest_form2_mis,std="std")

result08_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+albumin+log_age+edema+trt,path=path,testtype="form",eqType="mis",form="albumin")
result08_afttest_form3_mis$p_value
result08_afttest_form3_mis$p_std_value
# afttestplot(result08_afttest_form3_mis,std="unstd")
# afttestplot(result08_afttest_form3_mis,std="std")

result08_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+albumin+log_age+edema+trt,path=path,testtype="form",eqType="mis",form="log_age")
result08_afttest_form4_mis$p_value
result08_afttest_form4_mis$p_std_value
# afttestplot(result08_afttest_form4_mis,std="unstd")
# afttestplot(result08_afttest_form4_mis,std="std")

result08_afttest_form5_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+albumin+log_age+edema+trt,path=path,testtype="form",eqType="mis",form="edema")
result08_afttest_form5_mis$p_value
result08_afttest_form5_mis$p_std_value
# afttestplot(result08_afttest_form5_mis,std="unstd")
# afttestplot(result08_afttest_form5_mis,std="std")

# ------------------------------------------------------------------------------
# ----------- Covariates: bili+log_protime+log_albumin+age+edema+trt -----------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
result09_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+log_albumin+age+edema+trt,path=path,testtype="omni",eqType="mis")
result09_afttest_omni_mis$p_value
result09_afttest_omni_mis$p_std_value
# afttestplot(result09_afttest_omni_mis,std="unstd")
# afttestplot(result09_afttest_omni_mis,std="std")

# ------------------------------------ link ------------------------------------
result09_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+log_albumin+age+edema+trt,path=path,testtype="link",eqType="mis")
result09_afttest_link_mis$p_value
result09_afttest_link_mis$p_std_value
# afttestplot(result09_afttest_link_mis,std="unstd")
# afttestplot(result09_afttest_link_mis,std="std")

# ------------------------------------ form ------------------------------------
result09_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+log_albumin+age+edema+trt,path=path,testtype="form",eqType="mis",form="bili")
result09_afttest_form1_mis$p_value
result09_afttest_form1_mis$p_std_value
# afttestplot(result09_afttest_form1_mis,std="unstd")
# afttestplot(result09_afttest_form1_mis,std="std")

result09_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+log_albumin+age+edema+trt,path=path,testtype="form",eqType="mis",form="log_protime")
result09_afttest_form2_mis$p_value
result09_afttest_form2_mis$p_std_value
# afttestplot(result09_afttest_form2_mis,std="unstd")
# afttestplot(result09_afttest_form2_mis,std="std")

result09_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+log_albumin+age+edema+trt,path=path,testtype="form",eqType="mis",form="log_albumin")
result09_afttest_form3_mis$p_value
result09_afttest_form3_mis$p_std_value
# afttestplot(result09_afttest_form3_mis,std="unstd")
# afttestplot(result09_afttest_form3_mis,std="std")

result09_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+log_albumin+age+edema+trt,path=path,testtype="form",eqType="mis",form="age")
result09_afttest_form4_mis$p_value
result09_afttest_form4_mis$p_std_value
# afttestplot(result09_afttest_form4_mis,std="unstd")
# afttestplot(result09_afttest_form4_mis,std="std")

result09_afttest_form5_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+log_albumin+age+edema+trt,path=path,testtype="form",eqType="mis",form="edema")
result09_afttest_form5_mis$p_value
result09_afttest_form5_mis$p_std_value
# afttestplot(result09_afttest_form5_mis,std="unstd")
# afttestplot(result09_afttest_form5_mis,std="std")

# ------------------------------------------------------------------------------
# ----------- Covariates: bili+log_protime+albumin+log_age+edema+trt -----------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
result10_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+albumin+log_age+edema+trt,path=path,testtype="omni",eqType="mis")
result10_afttest_omni_mis$p_value
result10_afttest_omni_mis$p_std_value
# afttestplot(result10_afttest_omni_mis,std="unstd")
# afttestplot(result10_afttest_omni_mis,std="std")

# ------------------------------------ link ------------------------------------
result10_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+albumin+log_age+edema+trt,path=path,testtype="link",eqType="mis")
result10_afttest_link_mis$p_value
result10_afttest_link_mis$p_std_value
# afttestplot(result10_afttest_link_mis,std="unstd")
# afttestplot(result10_afttest_link_mis,std="std")

# ------------------------------------ form ------------------------------------
result10_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+albumin+log_age+edema+trt,path=path,testtype="form",eqType="mis",form="bili")
result10_afttest_form1_mis$p_value
result10_afttest_form1_mis$p_std_value
# afttestplot(result10_afttest_form1_mis,std="unstd")
# afttestplot(result10_afttest_form1_mis,std="std")

result10_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+albumin+log_age+edema+trt,path=path,testtype="form",eqType="mis",form="log_protime")
result10_afttest_form2_mis$p_value
result10_afttest_form2_mis$p_std_value
# afttestplot(result10_afttest_form2_mis,std="unstd")
# afttestplot(result10_afttest_form2_mis,std="std")

result10_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+albumin+log_age+edema+trt,path=path,testtype="form",eqType="mis",form="albumin")
result10_afttest_form3_mis$p_value
result10_afttest_form3_mis$p_std_value
# afttestplot(result10_afttest_form3_mis,std="unstd")
# afttestplot(result10_afttest_form3_mis,std="std")

result10_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+albumin+log_age+edema+trt,path=path,testtype="form",eqType="mis",form="log_age")
result10_afttest_form4_mis$p_value
result10_afttest_form4_mis$p_std_value
# afttestplot(result10_afttest_form4_mis,std="unstd")
# afttestplot(result10_afttest_form4_mis,std="std")

result10_afttest_form5_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+albumin+log_age+edema+trt,path=path,testtype="form",eqType="mis",form="edema")
result10_afttest_form5_mis$p_value
result10_afttest_form5_mis$p_std_value
# afttestplot(result10_afttest_form5_mis,std="unstd")
# afttestplot(result10_afttest_form5_mis,std="std")

# ------------------------------------------------------------------------------
# ----------- Covariates: bili+protime+log_albumin+log_age+edema+trt -----------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
result11_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~bili+protime+log_albumin+log_age+edema+trt,path=path,testtype="omni",eqType="mis")
result11_afttest_omni_mis$p_value
result11_afttest_omni_mis$p_std_value
# afttestplot(result11_afttest_omni_mis,std="unstd")
# afttestplot(result11_afttest_omni_mis,std="std")

# ------------------------------------ link ------------------------------------
result11_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~bili+protime+log_albumin+log_age+edema+trt,path=path,testtype="link",eqType="mis")
result11_afttest_link_mis$p_value
result11_afttest_link_mis$p_std_value
# afttestplot(result11_afttest_link_mis,std="unstd")
# afttestplot(result11_afttest_link_mis,std="std")

# ------------------------------------ form ------------------------------------
result11_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~bili+protime+log_albumin+log_age+edema+trt,path=path,testtype="form",eqType="mis",form="bili")
result11_afttest_form1_mis$p_value
result11_afttest_form1_mis$p_std_value
# afttestplot(result11_afttest_form1_mis,std="unstd")
# afttestplot(result11_afttest_form1_mis,std="std")

result11_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~bili+protime+log_albumin+log_age+edema+trt,path=path,testtype="form",eqType="mis",form="protime")
result11_afttest_form2_mis$p_value
result11_afttest_form2_mis$p_std_value
# afttestplot(result11_afttest_form2_mis,std="unstd")
# afttestplot(result11_afttest_form2_mis,std="std")

result11_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~bili+protime+log_albumin+log_age+edema+trt,path=path,testtype="form",eqType="mis",form="log_albumin")
result11_afttest_form3_mis$p_value
result11_afttest_form3_mis$p_std_value
# afttestplot(result11_afttest_form3_mis,std="unstd")
# afttestplot(result11_afttest_form3_mis,std="std")

result11_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~bili+protime+log_albumin+log_age+edema+trt,path=path,testtype="form",eqType="mis",form="log_age")
result11_afttest_form4_mis$p_value
result11_afttest_form4_mis$p_std_value
# afttestplot(result11_afttest_form4_mis,std="unstd")
# afttestplot(result11_afttest_form4_mis,std="std")

result11_afttest_form5_mis=afttest(Surv(X_pbc,D_pbc)~bili+protime+log_albumin+log_age+edema+trt,path=path,testtype="form",eqType="mis",form="edema")
result11_afttest_form5_mis$p_value
result11_afttest_form5_mis$p_std_value
# afttestplot(result11_afttest_form5_mis,std="unstd")
# afttestplot(result11_afttest_form5_mis,std="std")

# ------------------------------------------------------------------------------
# --------- Covariates: log_bili+log_protime+log_albumin+age+edema+trt ---------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
result12_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+log_albumin+age+edema+trt,path=path,testtype="omni",eqType="mis")
result12_afttest_omni_mis$p_value
result12_afttest_omni_mis$p_std_value
# afttestplot(result12_afttest_omni_mis,std="unstd")
# afttestplot(result12_afttest_omni_mis,std="std")

# ------------------------------------ link ------------------------------------
result12_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+log_albumin+age+edema+trt,path=path,testtype="link",eqType="mis")
result12_afttest_link_mis$p_value
result12_afttest_link_mis$p_std_value
# afttestplot(result12_afttest_link_mis,std="unstd")
# afttestplot(result12_afttest_link_mis,std="std")

# ------------------------------------ form ------------------------------------
result12_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+log_albumin+age+edema+trt,path=path,testtype="form",eqType="mis",form="log_bili")
result12_afttest_form1_mis$p_value
result12_afttest_form1_mis$p_std_value
# afttestplot(result12_afttest_form1_mis,std="unstd")
# afttestplot(result12_afttest_form1_mis,std="std")

result12_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+log_albumin+age+edema+trt,path=path,testtype="form",eqType="mis",form="log_protime")
result12_afttest_form2_mis$p_value
result12_afttest_form2_mis$p_std_value
# afttestplot(result12_afttest_form2_mis,std="unstd")
# afttestplot(result12_afttest_form2_mis,std="std")

result12_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+log_albumin+age+edema+trt,path=path,testtype="form",eqType="mis",form="log_albumin")
result12_afttest_form3_mis$p_value
result12_afttest_form3_mis$p_std_value
# afttestplot(result12_afttest_form3_mis,std="unstd")
# afttestplot(result12_afttest_form3_mis,std="std")

result12_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+log_albumin+age+edema+trt,path=path,testtype="form",eqType="mis",form="age")
result12_afttest_form4_mis$p_value
result12_afttest_form4_mis$p_std_value
# afttestplot(result12_afttest_form4_mis,std="unstd")
# afttestplot(result12_afttest_form4_mis,std="std")

result12_afttest_form5_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+log_albumin+age+edema+trt,path=path,testtype="form",eqType="mis",form="edema")
result12_afttest_form5_mis$p_value
result12_afttest_form5_mis$p_std_value
# afttestplot(result12_afttest_form5_mis,std="unstd")
# afttestplot(result12_afttest_form5_mis,std="std")

# ------------------------------------------------------------------------------
# --------- Covariates: log_bili+log_protime+albumin+log_age+edema+trt ---------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
result13_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+albumin+log_age+edema+trt,path=path,testtype="omni",eqType="mis")
result13_afttest_omni_mis$p_value
result13_afttest_omni_mis$p_std_value
# afttestplot(result13_afttest_omni_mis,std="unstd")
# afttestplot(result13_afttest_omni_mis,std="std")

# ------------------------------------ link ------------------------------------
result13_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+albumin+log_age+edema+trt,path=path,testtype="link",eqType="mis")
result13_afttest_link_mis$p_value
result13_afttest_link_mis$p_std_value
# afttestplot(result13_afttest_link_mis,std="unstd")
# afttestplot(result13_afttest_link_mis,std="std")

# ------------------------------------ form ------------------------------------
result13_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+albumin+log_age+edema+trt,path=path,testtype="form",eqType="mis",form="log_bili")
result13_afttest_form1_mis$p_value
result13_afttest_form1_mis$p_std_value
# afttestplot(result13_afttest_form1_mis,std="unstd")
# afttestplot(result13_afttest_form1_mis,std="std")

result13_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+albumin+log_age+edema+trt,path=path,testtype="form",eqType="mis",form="log_protime")
result13_afttest_form2_mis$p_value
result13_afttest_form2_mis$p_std_value
# afttestplot(result13_afttest_form2_mis,std="unstd")
# afttestplot(result13_afttest_form2_mis,std="std")

result13_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+albumin+log_age+edema+trt,path=path,testtype="form",eqType="mis",form="albumin")
result13_afttest_form3_mis$p_value
result13_afttest_form3_mis$p_std_value
# afttestplot(result13_afttest_form3_mis,std="unstd")
# afttestplot(result13_afttest_form3_mis,std="std")

result13_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+albumin+log_age+edema+trt,path=path,testtype="form",eqType="mis",form="log_age")
result13_afttest_form4_mis$p_value
result13_afttest_form4_mis$p_std_value
# afttestplot(result13_afttest_form4_mis,std="unstd")
# afttestplot(result13_afttest_form4_mis,std="std")

result13_afttest_form5_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+albumin+log_age+edema+trt,path=path,testtype="form",eqType="mis",form="edema")
result13_afttest_form5_mis$p_value
result13_afttest_form5_mis$p_std_value
# afttestplot(result13_afttest_form5_mis,std="unstd")
# afttestplot(result13_afttest_form5_mis,std="std")

# ------------------------------------------------------------------------------
# --------- Covariates: log_bili+protime+log_albumin+log_age+edema+trt ---------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
result14_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+log_albumin+log_age+edema+trt,path=path,testtype="omni",eqType="mis")
result14_afttest_omni_mis$p_value
result14_afttest_omni_mis$p_std_value
# afttestplot(result14_afttest_omni_mis,std="unstd")
# afttestplot(result14_afttest_omni_mis,std="std")

# ------------------------------------ link ------------------------------------
result14_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+log_albumin+log_age+edema+trt,path=path,testtype="link",eqType="mis")
result14_afttest_link_mis$p_value
result14_afttest_link_mis$p_std_value
# afttestplot(result14_afttest_link_mis,std="unstd")
# afttestplot(result14_afttest_link_mis,std="std")

# ------------------------------------ form ------------------------------------
result14_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+log_albumin+log_age+edema+trt,path=path,testtype="form",eqType="mis",form="log_bili")
result14_afttest_form1_mis$p_value
result14_afttest_form1_mis$p_std_value
# afttestplot(result14_afttest_form1_mis,std="unstd")
# afttestplot(result14_afttest_form1_mis,std="std")

result14_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+log_albumin+log_age+edema+trt,path=path,testtype="form",eqType="mis",form="protime")
result14_afttest_form2_mis$p_value
result14_afttest_form2_mis$p_std_value
# afttestplot(result14_afttest_form2_mis,std="unstd")
# afttestplot(result14_afttest_form2_mis,std="std")

result14_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+log_albumin+log_age+edema+trt,path=path,testtype="form",eqType="mis",form="log_albumin")
result14_afttest_form3_mis$p_value
result14_afttest_form3_mis$p_std_value
# afttestplot(result14_afttest_form3_mis,std="unstd")
# afttestplot(result14_afttest_form3_mis,std="std")

result14_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+log_albumin+log_age+edema+trt,path=path,testtype="form",eqType="mis",form="log_age")
result14_afttest_form4_mis$p_value
result14_afttest_form4_mis$p_std_value
# afttestplot(result14_afttest_form4_mis,std="unstd")
# afttestplot(result14_afttest_form4_mis,std="std")

result14_afttest_form5_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+protime+log_albumin+log_age+edema+trt,path=path,testtype="form",eqType="mis",form="edema")
result14_afttest_form5_mis$p_value
result14_afttest_form5_mis$p_std_value
# afttestplot(result14_afttest_form5_mis,std="unstd")
# afttestplot(result14_afttest_form5_mis,std="std")

# ------------------------------------------------------------------------------
# --------------- Covariates: bili+protime+albumin+age+edema+trt ---------------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
result15_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+log_albumin+log_age+edema+trt,path=path,testtype="omni",eqType="mis")
result15_afttest_omni_mis$p_value
result15_afttest_omni_mis$p_std_value
# afttestplot(result15_afttest_omni_mis,std="unstd")
# afttestplot(result15_afttest_omni_mis,std="std")

# ------------------------------------ link ------------------------------------
result15_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+log_albumin+log_age+edema+trt,path=path,testtype="link",eqType="mis")
result15_afttest_link_mis$p_value
result15_afttest_link_mis$p_std_value
# afttestplot(result15_afttest_link_mis,std="unstd")
# afttestplot(result15_afttest_link_mis,std="std")

# ------------------------------------ form ------------------------------------
result15_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+log_albumin+log_age+edema+trt,path=path,testtype="form",eqType="mis",form="bili")
result15_afttest_form1_mis$p_value
result15_afttest_form1_mis$p_std_value
# afttestplot(result15_afttest_form1_mis,std="unstd")
# afttestplot(result15_afttest_form1_mis,std="std")

result15_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+log_albumin+log_age+edema+trt,path=path,testtype="form",eqType="mis",form="log_protime")
result15_afttest_form2_mis$p_value
result15_afttest_form2_mis$p_std_value
# afttestplot(result15_afttest_form2_mis,std="unstd")
# afttestplot(result15_afttest_form2_mis,std="std")

result15_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+log_albumin+log_age+edema+trt,path=path,testtype="form",eqType="mis",form="log_albumin")
result15_afttest_form3_mis$p_value
result15_afttest_form3_mis$p_std_value
# afttestplot(result15_afttest_form3_mis,std="unstd")
# afttestplot(result15_afttest_form3_mis,std="std")

result15_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+log_albumin+log_age+edema+trt,path=path,testtype="form",eqType="mis",form="log_age")
result15_afttest_form4_mis$p_value
result15_afttest_form4_mis$p_std_value
# afttestplot(result15_afttest_form4_mis,std="unstd")
# afttestplot(result15_afttest_form4_mis,std="std")

result15_afttest_form5_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_protime+log_albumin+log_age+edema+trt,path=path,testtype="form",eqType="mis",form="edema")
result15_afttest_form5_mis$p_value
result15_afttest_form5_mis$p_std_value
# afttestplot(result15_afttest_form5_mis,std="unstd")
# afttestplot(result15_afttest_form5_mis,std="std")

# ------------------------------------------------------------------------------
# ------- Covariates: log_bili+log_protime+log_albumin+log_age+edema+trt -------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
result16_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+log_albumin+log_age+edema+trt,path=path,testtype="omni",eqType="mis")
result16_afttest_omni_mis$p_value
result16_afttest_omni_mis$p_std_value
# afttestplot(result16_afttest_omni_mis,std="unstd")
# afttestplot(result16_afttest_omni_mis,std="std")

# ------------------------------------ link ------------------------------------
result16_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+log_albumin+log_age+edema+trt,path=path,testtype="link",eqType="mis")
result16_afttest_link_mis$p_value
result16_afttest_link_mis$p_std_value
# afttestplot(result16_afttest_link_mis,std="unstd")
# afttestplot(result16_afttest_link_mis,std="std")

# ------------------------------------ form ------------------------------------
result16_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+log_albumin+log_age+edema+trt,path=path,testtype="form",eqType="mis",form="log_bili")
result16_afttest_form1_mis$p_value
result16_afttest_form1_mis$p_std_value
# afttestplot(result16_afttest_form1_mis,std="unstd")
# afttestplot(result16_afttest_form1_mis,std="std")

result16_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+log_albumin+log_age+edema+trt,path=path,testtype="form",eqType="mis",form="log_protime")
result16_afttest_form2_mis$p_value
result16_afttest_form2_mis$p_std_value
# afttestplot(result16_afttest_form2_mis,std="unstd")
# afttestplot(result16_afttest_form2_mis,std="std")

result16_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+log_albumin+log_age+edema+trt,path=path,testtype="form",eqType="mis",form="log_albumin")
result16_afttest_form3_mis$p_value
result16_afttest_form3_mis$p_std_value
# afttestplot(result16_afttest_form3_mis,std="unstd")
# afttestplot(result16_afttest_form3_mis,std="std")

result16_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+log_albumin+log_age+edema+trt,path=path,testtype="form",eqType="mis",form="log_age")
result16_afttest_form4_mis$p_value
result16_afttest_form4_mis$p_std_value
# afttestplot(result16_afttest_form4_mis,std="unstd")
# afttestplot(result16_afttest_form4_mis,std="std")

result16_afttest_form5_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_protime+log_albumin+log_age+edema+trt,path=path,testtype="form",eqType="mis",form="edema")
result16_afttest_form5_mis$p_value
result16_afttest_form5_mis$p_std_value
# afttestplot(result16_afttest_form5_mis,std="unstd")
# afttestplot(result16_afttest_form5_mis,std="std")

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
beta11=aftsrr(Surv(X_pbc,D_pbc)~log_bili+protime+albumin+log_age+edema+trt,eqType="mis")
summary(beta11)

beta16=aftsrr(Surv(X_pbc,D_pbc)~log_bili+log_protime+albumin+log_age+edema+trt,eqType="mis")
summary(beta16)

beta19=aftsrr(Surv(X_pbc,D_pbc)~log_bili+log_protime+log_albumin+log_age+edema+trt,eqType="mis")
summary(beta19)