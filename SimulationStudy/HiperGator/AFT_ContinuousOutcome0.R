# Load R packages
library(Rcpp)
library(RcppArmadillo)
library(survival)
library(ggplot2)
library(gridExtra)
library(aftgee)
library(afttest)

# Load R code
source("afttest_source_r.R")

Scenario = 11

{
  # The numer of simulation run per file
  sim_per_file = 4
  
  # ------ Define of constants (adjust to fit the data generating scenario) ------
  
  # Define number of observations for each dataset
  # N = 500
  # N = 300
  N = 100
  
  # The number of the approximated paths for each simulation is 200
  path = 500
  
  # Type 2 error check
  gamma_0 = 0
  # gamma_0 = 0.1
  # gamma_0 = 0.2
  # gamma_0 = 0.3
  # gamma_0 = 0.4
  # gamma_0 = 0.5
  
  # Extract ID for simulated dataset (specific to LSF computing cluster)
  # Note: The LSB_JOBINDEX is specified in the bsub command using the -J
  # option
  run_ID = as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
  
  txt.title = paste0("Results/afttest","Scn",Scenario,"N",N,"gamma",gamma_0*10,"_result.txt")
  if (run_ID == 1) {
    df = data.frame(matrix(ncol = 13, nrow = 0))
    df_col_names = c("run_ID", 
                     "omni_mns_pvalue", "omni_mns_stdpvalue",
                     "omni_mis_pvalue", "omni_mis_stdpvalue",
                     "link_mns_pvalue", "link_mns_stdpvalue",
                     "link_mis_pvalue", "link_mis_stdpvalue",
                     "form_mns_pvalue", "form_mns_stdpvalue",
                     "form_mis_pvalue", "form_mis_stdpvalue")
    colnames(df) = df_col_names
    write.table(df, file = txt.title, sep = "\t", row.names = FALSE, col.names = TRUE)
  }
}

cond1 = (sum(Scenario == c(11,12))>0)
cond2 = (sum(Scenario == c(21,22))>0)
cond3 = (sum(Scenario == c(31,32))>0)
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
if (cond1){
  # ------------------------------------------------------------------------------
  for (sim in 1:sim_per_file) {
    # Specify the seed so can repeat simulation later if necessary
    run = sim + (run_ID-1)*(sim_per_file)
    set.seed(run)
    
    # ------------------------------------------------------------------------------
    temp_data = generate_data(N,gamma_0,Scenario)
    X = temp_data$X
    D = temp_data$D
    Z = temp_data$Z
    
    # ------------------------------------ omni ------------------------------------
    result_omni_mns = afttest(Surv(X, D) ~ Z, path=path,testtype='omni', eqType='mns')
    result_omni_mis = afttest(Surv(X, D) ~ Z, path=path,testtype='omni', eqType='mis')
    
    # ------------------------------------ link ------------------------------------
    result_link_mns = afttest(Surv(X, D) ~ Z, path=path,testtype='link', eqType='mns')
    result_link_mis = afttest(Surv(X, D) ~ Z, path=path,testtype='link', eqType='mis')
    
    # ------------------------------------ form ------------------------------------
    result_form_mns = afttest(Surv(X, D) ~ Z, path=path,testtype='form', eqType='mns', form = 1)
    result_form_mis = afttest(Surv(X, D) ~ Z, path=path,testtype='form', eqType='mis', form = 1)
    
    # ------------------------------------------------------------------------------
    allinfo = data.frame(run,
                         "omni_mns_pvalue" = result_omni_mns$p_value, 
                         "omni_mns_stdpvalue" = result_omni_mns$p_std_value,
                         "omni_mis_pvalue" = result_omni_mis$p_value, 
                         "omni_mis_stdpvalue" = result_omni_mis$p_std_value,
                         "link_mns_pvalue" = result_form_mns$p_value, 
                         "link_mns_stdpvalue" = result_link_mns$p_std_value,
                         "link_mis_pvalue" = result_link_mis$p_value, 
                         "link_mis_stdpvalue" = result_link_mis$p_std_value,
                         "form_mns_pvalue" = result_form_mns$p_value, 
                         "form_mns_stdpvalue" = result_form_mns$p_std_value,
                         "form_mis_pvalue" = result_form_mis$p_value, 
                         "form_mis_stdpvalue" = result_form_mis$p_std_value)
    write.table(allinfo, file = txt.title, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE)
    rm(list="result_omni_mns")
    rm(list="result_omni_mis")
    rm(list="result_link_mns")
    rm(list="result_link_mis")
    rm(list="result_form_mns")
    rm(list="result_form_mis")
    gc()
  }
} else if(cond2) {
  # ------------------------------------------------------------------------------
  for (sim in 1:sim_per_file) {
    # Specify the seed so can repeat simulation later if necessary
    run = sim + (run_ID-1)*(sim_per_file)
    set.seed(run)
    
    # ------------------------------------------------------------------------------
    temp_data = generate_data(N,gamma_0,Scenario)
    X = temp_data$X
    D = temp_data$D
    Z = temp_data$Z
    Z1 = Z[,1]
    Z2 = Z[,2]
    
    # ------------------------------------ omni ------------------------------------
    result_omni_mns = afttest(Surv(X, D) ~ Z1 + Z2, path=path,testtype='omni', eqType='mns')
    result_omni_mis = afttest(Surv(X, D) ~ Z1 + Z2, path=path,testtype='omni', eqType='mis')
    
    # ------------------------------------ link ------------------------------------
    result_link_mns = afttest(Surv(X, D) ~ Z1 + Z2, path=path,testtype='link', eqType='mns')
    result_link_mis = afttest(Surv(X, D) ~ Z1 + Z2, path=path,testtype='link', eqType='mis')
    
    # ------------------------------------ form ------------------------------------
    result_form_mns = afttest(Surv(X, D) ~ Z1 + Z2, path=path,testtype='form', eqType='mns', form = 2)
    result_form_mis = afttest(Surv(X, D) ~ Z1 + Z2, path=path,testtype='form', eqType='mis', form = 2)
    
    # ------------------------------------------------------------------------------
    allinfo = data.frame(run,
                         "omni_mns_pvalue" = result_omni_mns$p_value, 
                         "omni_mns_stdpvalue" = result_omni_mns$p_std_value,
                         "omni_mis_pvalue" = result_omni_mis$p_value, 
                         "omni_mis_stdpvalue" = result_omni_mis$p_std_value,
                         "link_mns_pvalue" = result_form_mns$p_value, 
                         "link_mns_stdpvalue" = result_link_mns$p_std_value,
                         "link_mis_pvalue" = result_link_mis$p_value, 
                         "link_mis_stdpvalue" = result_link_mis$p_std_value,
                         "form_mns_pvalue" = result_form_mns$p_value, 
                         "form_mns_stdpvalue" = result_form_mns$p_std_value,
                         "form_mis_pvalue" = result_form_mis$p_value, 
                         "form_mis_stdpvalue" = result_form_mis$p_std_value)
    write.table(allinfo, file = txt.title, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE)
    rm(list="result_omni_mns")
    rm(list="result_omni_mis")
    rm(list="result_link_mns")
    rm(list="result_link_mis")
    rm(list="result_form_mns")
    rm(list="result_form_mis")
    gc()
  }
} else if(cond3) {
  # ------------------------------------------------------------------------------
  for (sim in 1:sim_per_file) {
    # Specify the seed so can repeat simulation later if necessary
    run = sim + (run_ID-1)*(sim_per_file)
    set.seed(run)
    
    # ------------------------------------------------------------------------------
    temp_data = generate_data(N,gamma_0,Scenario)
    X = temp_data$X
    D = temp_data$D
    Z = temp_data$Z
    Z1 = Z[,1]
    Z2 = Z[,2]
    
    # ------------------------------------ omni ------------------------------------
    result_omni_mns = afttest(Surv(X, D) ~ Z1 + Z2, path=path,testtype='omni', eqType='mns')
    result_omni_mis = afttest(Surv(X, D) ~ Z1 + Z2, path=path,testtype='omni', eqType='mis')
    
    # ------------------------------------ link ------------------------------------
    result_link_mns = afttest(Surv(X, D) ~ Z1 + Z2, path=path,testtype='link', eqType='mns')
    result_link_mis = afttest(Surv(X, D) ~ Z1 + Z2, path=path,testtype='link', eqType='mis')
    
    # ------------------------------------ form ------------------------------------
    result_form_mns = afttest(Surv(X, D) ~ Z1 + Z2, path=path,testtype='form', eqType='mns', form = 2)
    result_form_mis = afttest(Surv(X, D) ~ Z1 + Z2, path=path,testtype='form', eqType='mis', form = 2)
    
    # ------------------------------------------------------------------------------
    allinfo = data.frame(run,
                         "omni_mns_pvalue" = result_omni_mns$p_value, 
                         "omni_mns_stdpvalue" = result_omni_mns$p_std_value,
                         "omni_mis_pvalue" = result_omni_mis$p_value, 
                         "omni_mis_stdpvalue" = result_omni_mis$p_std_value,
                         "link_mns_pvalue" = result_form_mns$p_value, 
                         "link_mns_stdpvalue" = result_link_mns$p_std_value,
                         "link_mis_pvalue" = result_link_mis$p_value, 
                         "link_mis_stdpvalue" = result_link_mis$p_std_value,
                         "form_mns_pvalue" = result_form_mns$p_value, 
                         "form_mns_stdpvalue" = result_form_mns$p_std_value,
                         "form_mis_pvalue" = result_form_mis$p_value, 
                         "form_mis_stdpvalue" = result_form_mis$p_std_value)
    write.table(allinfo, file = txt.title, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE)
    rm(list="result_omni_mns")
    rm(list="result_omni_mis")
    rm(list="result_link_mns")
    rm(list="result_link_mis")
    rm(list="result_form_mns")
    rm(list="result_form_mis")
    gc()
  }
}