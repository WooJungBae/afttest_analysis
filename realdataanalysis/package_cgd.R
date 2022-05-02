#-------------------------------------------------------------
#---------------------------SETTING---------------------------
#-------------------------------------------------------------
#rm(list=ls());gc();

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

cgd_data = subset(cgd,enum==1,c(tstop,status,treat,age,steroids))

D_cgd = cgd_data$status
X_cgd = cgd_data$tstop

trt = ifelse(cgd_data$treat=="placebo",0,1)
age = cgd_data$age ; log_age = log(age)
str = cgd_data$steroids

# ------------------------------------------------------------------------------
# ------------------------------------ "mns" -----------------------------------
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# --------------------------- Covariates: trt+str+age --------------------------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
result01_afttest_omni_mns=afttest(Surv(X_cgd,D_cgd)~trt+str+age,path=path,testtype="omni",eqType="mns")
result01_afttest_omni_mns$p_value
result01_afttest_omni_mns$p_std_value
# afttestplot(result01_afttest_omni_mns,std="unstd")
# afttestplot(result01_afttest_omni_mns,std="std")

# ------------------------------------ link ------------------------------------
result01_afttest_link_mns=afttest(Surv(X_cgd,D_cgd)~trt+str+age,path=path,testtype="link",eqType="mns")
result01_afttest_link_mns$p_value
result01_afttest_link_mns$p_std_value
# afttestplot(result01_afttest_link_mns,std="unstd")
# afttestplot(result01_afttest_link_mns,std="std")

# ------------------------------------ form ------------------------------------
result01_afttest_link_mns=afttest(Surv(X_cgd,D_cgd)~trt+str+age,path=path,testtype="link",eqType="mns",form="age")
result01_afttest_link_mns$p_value
result01_afttest_link_mns$p_std_value
# afttestplot(result01_afttest_link_mns,std="unstd")
# afttestplot(result01_afttest_link_mns,std="std")

# ------------------------------------------------------------------------------
# ------------------------- Covariates: trt+str+log_age ------------------------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
result02_afttest_omni_mns=afttest(Surv(X_cgd,D_cgd)~trt+str+log_age,path=path,testtype="omni",eqType="mns")
result02_afttest_omni_mns$p_value
result02_afttest_omni_mns$p_std_value
# afttestplot(result01_afttest_omni_mns,std="unstd")
# afttestplot(result01_afttest_omni_mns,std="std")

# ------------------------------------ link ------------------------------------
result02_afttest_link_mns=afttest(Surv(X_cgd,D_cgd)~trt+str+log_age,path=path,testtype="link",eqType="mns")
result02_afttest_link_mns$p_value
result02_afttest_link_mns$p_std_value
# afttestplot(result01_afttest_link_mns,std="unstd")
# afttestplot(result01_afttest_link_mns,std="std")

# ------------------------------------ form ------------------------------------
result02_afttest_link_mns=afttest(Surv(X_cgd,D_cgd)~trt+str+log_age,path=path,testtype="link",eqType="mns",form="age")
result02_afttest_link_mns$p_value
result02_afttest_link_mns$p_std_value
# afttestplot(result01_afttest_link_mns,std="unstd")
# afttestplot(result01_afttest_link_mns,std="std")

# ------------------------------------------------------------------------------
# ------------------------------------ "mis" -----------------------------------
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# --------------------------- Covariates: trt+str+age --------------------------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
result01_afttest_omni_mns=afttest(Surv(X_cgd,D_cgd)~trt+str+age,path=path,testtype="omni",eqType="mis")
result01_afttest_omni_mns$p_value
result01_afttest_omni_mns$p_std_value
# afttestplot(result01_afttest_omni_mns,std="unstd")
# afttestplot(result01_afttest_omni_mns,std="std")

# ------------------------------------ link ------------------------------------
result01_afttest_link_mns=afttest(Surv(X_cgd,D_cgd)~trt+str+age,path=path,testtype="link",eqType="mis")
result01_afttest_link_mns$p_value
result01_afttest_link_mns$p_std_value
# afttestplot(result01_afttest_link_mns,std="unstd")
# afttestplot(result01_afttest_link_mns,std="std")

# ------------------------------------ form ------------------------------------
result01_afttest_link_mns=afttest(Surv(X_cgd,D_cgd)~trt+str+age,path=path,testtype="link",eqType="mis",form="age")
result01_afttest_link_mns$p_value
result01_afttest_link_mns$p_std_value
# afttestplot(result01_afttest_link_mns,std="unstd")
# afttestplot(result01_afttest_link_mns,std="std")

# ------------------------------------------------------------------------------
# ------------------------- Covariates: trt+str+log_age ------------------------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
result02_afttest_omni_mns=afttest(Surv(X_cgd,D_cgd)~trt+str+log_age,path=path,testtype="omni",eqType="mis")
result02_afttest_omni_mns$p_value
result02_afttest_omni_mns$p_std_value
# afttestplot(result01_afttest_omni_mns,std="unstd")
# afttestplot(result01_afttest_omni_mns,std="std")

# ------------------------------------ link ------------------------------------
result02_afttest_link_mns=afttest(Surv(X_cgd,D_cgd)~trt+str+log_age,path=path,testtype="link",eqType="mis")
result02_afttest_link_mns$p_value
result02_afttest_link_mns$p_std_value
# afttestplot(result01_afttest_link_mns,std="unstd")
# afttestplot(result01_afttest_link_mns,std="std")

# ------------------------------------ form ------------------------------------
result02_afttest_link_mns=afttest(Surv(X_cgd,D_cgd)~trt+str+log_age,path=path,testtype="link",eqType="mis",form="age")
result02_afttest_link_mns$p_value
result02_afttest_link_mns$p_std_value
# afttestplot(result01_afttest_link_mns,std="unstd")
# afttestplot(result01_afttest_link_mns,std="std")



