# Define functions -----------------------------------------------------------------

# Define function to generate datasets
generate_data = function(N,gamma_0,Scenario) {
  
  if (Scenario==11){
    # --------------------------------- Scenario 1 ---------------------------------
    # True Coefficient: beta_1 = 1
    beta_1 = 1
    
    # ------------------------------------------------------------------------------
    # Generate covariate Z
    Z = matrix(rnorm(N,3,1),nrow=N)
    
    # ------------------------------------------------------------------------------
    # censoring rate 20% 
    # T: rnorm(N,4,1)
    # C: rnorm(N,5.2,1)
    
    # T_data: true event time
    # C_data: true censoring time
    # X_data: observed time
    # D_data: observed indicator
    # Z_data: covariates fitted
    T_data = as.vector(exp(-beta_1*Z-gamma_0*(Z^2)+rnorm(N,4,1)))
    C_data = as.vector(exp(-beta_1*Z-gamma_0*(Z^2)+rnorm(N,5.2,1)))
    X_data = C_data*(T_data>C_data)+T_data*(T_data<=C_data)
    D_data = 0*(T_data>C_data)+1*(T_data<=C_data)
    Z_data = Z
    
  } else if (Scenario==12){
    # --------------------------------- Scenario 1 ---------------------------------
    # True Coefficient: beta_1 = 1
    beta_1 = 1
    
    # ------------------------------------------------------------------------------
    # Generate covariate Z
    Z = matrix(rnorm(N,3,1),nrow=N)
    
    # ------------------------------------------------------------------------------
    # censoring rate 40% 
    # T: rnorm(N,4,1)
    # C: rnorm(N,4.35,1)
    
    # T_data: true event time
    # C_data: true censoring time
    # X_data: observed time
    # D_data: observed indicator
    # Z_data: covariates fitted
    T_data = as.vector(exp(-beta_1*Z-gamma_0*(Z^2)+rnorm(N,4,1)))
    C_data = as.vector(exp(-beta_1*Z-gamma_0*(Z^2)+rnorm(N,4.35,1)))
    X_data = C_data*(T_data>C_data)+T_data*(T_data<=C_data)
    D_data = 0*(T_data>C_data)+1*(T_data<=C_data)
    Z_data = Z
    
  } else if (Scenario==21){
    # --------------------------------- Scenario 2 ---------------------------------
    # True Coefficient: beta_1 = 1 and beta_2 = 1
    beta_1 = 1
    beta_2 = 1
    
    # ------------------------------------------------------------------------------
    # Generate covariate Z
    Z1 = rbinom(N,1,0.5)
    Z2 = rnorm(N,3,1)
    
    # ------------------------------------------------------------------------------
    # censoring rate 20% 
    # T: rexp(N,1)
    # C: rexp(N,1/4)
    
    # T_data: true event time
    # C_data: true censoring time
    # X_data: observed time
    # D_data: observed indicator
    # Z_data: covariates fitted
    T_data = exp(- beta_1 * Z1 - beta_2 * Z2 - gamma_0 * (Z2^{2}) + rexp(N,1))
    C_data = exp(- beta_1 * Z1 - beta_2 * Z2 - gamma_0 * (Z2^{2}) + rexp(N,1/4))
    X_data = C_data * (T_data > C_data) + T_data * (T_data <= C_data)
    D_data = 0 * (T_data > C_data) + 1 * (T_data <= C_data)
    Z_data = cbind(Z1,Z2)
    
  } else if (Scenario==22){
    # --------------------------------- Scenario 2 ---------------------------------
    # True Coefficient: beta_1 = 1 and beta_2 = 1
    beta_1 = 1
    beta_2 = 1
    
    # ------------------------------------------------------------------------------
    # Generate covariate Z
    Z1 = rbinom(N,1,0.5)
    Z2 = rnorm(N,3,1)
    
    # ------------------------------------------------------------------------------
    # censoring rate 40% 
    # T: rexp(N,1)
    # C: rexp(N,1/1.5)
    
    # T_data: true event time
    # C_data: true censoring time
    # X_data: observed time
    # D_data: observed indicator
    # Z_data: covariates fitted
    T_data = exp(- beta_1 * Z1 - beta_2 * Z2 - gamma_0 * (Z2^{2}) + rexp(N,1))
    C_data = exp(- beta_1 * Z1 - beta_2 * Z2 - gamma_0 * (Z2^{2}) + rexp(N,1/1.5))
    X_data = C_data * (T_data > C_data) + T_data * (T_data <= C_data)
    D_data = 0 * (T_data > C_data) + 1 * (T_data <= C_data)
    Z_data = cbind(Z1,Z2)
    
  }  else if (Scenario==31){
    # --------------------------------- Scenario 3 ---------------------------------
    # True Coefficient: beta_1 = 1 and beta_2 = 1
    beta_1 = 1
    beta_2 = 1
    
    # ------------------------------------------------------------------------------
    # Generate covariate Z
    Z1 = rbinom(N,1,0.5) + 1
    Z2 = rnorm(N,3,1)
    
    # ------------------------------------------------------------------------------
    # censoring rate 20% 
    # T: rexp(N,1)
    # C: rexp(N,1/4)
    
    # T_data: true event time
    # C_data: true censoring time
    # X_data: observed time
    # D_data: observed indicator
    # Z_data: covariates fitted
    T_data = exp(- beta_1 * Z1 - beta_2 * Z2 - gamma_0 * lgamma(1+0.5*Z2^{2}) + rexp(N,1))
    C_data = exp(- beta_1 * Z1 - beta_2 * Z2 - gamma_0 * lgamma(1+0.5*Z2^{2}) + rexp(N,1/4))
    X_data = C_data * (T_data > C_data) + T_data * (T_data <= C_data)
    D_data = 0 * (T_data > C_data) + 1 * (T_data <= C_data)
    Z_data = cbind(Z1,Z2)
    
  } else if (Scenario==32){
    # --------------------------------- Scenario 3 ---------------------------------
    # True Coefficient: beta_1 = 1 and beta_2 = 1
    beta_1 = 1
    beta_2 = 1
    
    # ------------------------------------------------------------------------------
    # Generate covariate Z
    Z1 = rbinom(N,1,0.5)
    Z2 = rnorm(N,3,1)
    
    # ------------------------------------------------------------------------------
    # censoring rate 40% 
    # T: rexp(N,1)
    # C: rexp(N,1/1.5)
    
    # T_data: true event time
    # C_data: true censoring time
    # X_data: observed time
    # D_data: observed indicator
    # Z_data: covariates fitted
    T_data = exp(- beta_1 * Z1 - beta_2 * Z2 - gamma_0 * log(1 + Z2^{2}) + rexp(N,1))
    C_data = exp(- beta_1 * Z1 - beta_2 * Z2 - gamma_0 * log(1 + Z2^{2}) + rexp(N,1/1.5))
    X_data = C_data * (T_data > C_data) + T_data * (T_data <= C_data)
    D_data = 0 * (T_data > C_data) + 1 * (T_data <= C_data)
    Z_data = cbind(Z1,Z2)
  } else if (Scenario==41){
    # --------------------------------- Scenario 4 ---------------------------------
    # True Coefficient: beta_1 = 1 and beta_2 = 1
    beta_1 = 1
    beta_2 = 1
    
    # ------------------------------------------------------------------------------
    # Generate covariate Z
    Z1 = rbinom(N,1,0.5) + 1
    Z2 = rnorm(N,3,1)
    
    # ------------------------------------------------------------------------------
    # censoring rate 20% 
    # T: rexp(N,1)
    # C: rexp(N,1/4)
    
    # T_data: true event time
    # C_data: true censoring time
    # X_data: observed time
    # D_data: observed indicator
    # Z_data: covariates fitted
    T_data = exp(- beta_1 * Z1 - beta_2 * Z2 - gamma_0 * Z1 * Z2 + rexp(N,1))
    C_data = exp(- beta_1 * Z1 - beta_2 * Z2 - gamma_0 * Z1 * Z2 + rexp(N,1/4))
    X_data = C_data * (T_data > C_data) + T_data * (T_data <= C_data)
    D_data = 0 * (T_data > C_data) + 1 * (T_data <= C_data)
    Z_data = cbind(Z1,Z2)
    
  } else if (Scenario==42){
    # --------------------------------- Scenario 4 ---------------------------------
    # True Coefficient: beta_1 = 1 and beta_2 = 1
    beta_1 = 1
    beta_2 = 1
    
    # ------------------------------------------------------------------------------
    # Generate covariate Z
    Z1 = rbinom(N,1,0.5)
    Z2 = rnorm(N,3,1)
    
    # ------------------------------------------------------------------------------
    # censoring rate 40% 
    # T: rexp(N,1)
    # C: rexp(N,1/1.5)
    
    # T_data: true event time
    # C_data: true censoring time
    # X_data: observed time
    # D_data: observed indicator
    # Z_data: covariates fitted
    T_data = exp(- beta_1 * Z1 - beta_2 * Z2 - gamma_0 * Z1 * Z2 + rexp(N,1))
    C_data = exp(- beta_1 * Z1 - beta_2 * Z2 - gamma_0 * Z1 * Z2 + rexp(N,1/1.5))
    X_data = C_data * (T_data > C_data) + T_data * (T_data <= C_data)
    D_data = 0 * (T_data > C_data) + 1 * (T_data <= C_data)
    Z_data = cbind(Z1,Z2)
  }
  
  return(list(X=X_data, D=D_data, Z=Z_data))
}

rejectionratio = function(Scenario) {
  # Type 1 error control
  alpha = 0.05
  
  # ------------------------------------------------------------------------------
  # ------------------------------------------------------------------------------
  S1 = as.numeric(paste0(Scenario,1))
  {
    N = 100
    {
      # ------------------------------------------------------------------------------
      # ------------------------------------------------------------------------------
      gamma_0 = 0
      txt.title = paste0("afttest","Scn",S1,"N",N,"gamma",gamma_0*10,"_result.txt")
      
      gamma0_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma0_sim = nrow(gamma0_result)
      gamma0_rejectionratio = matrix(apply(gamma0_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma0_sim
      gamma0_rejectionratio = round(gamma0_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma_0 = 0.1
      txt.title = paste0("afttest","Scn",S1,"N",N,"gamma",gamma_0*10,"_result.txt")
      
      gamma1_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma1_sim = nrow(gamma1_result)
      gamma1_rejectionratio = matrix(apply(gamma1_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma1_sim
      gamma1_rejectionratio = round(gamma1_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma_0 = 0.2
      txt.title = paste0("afttest","Scn",S1,"N",N,"gamma",gamma_0*10,"_result.txt")
      
      gamma2_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma2_sim = nrow(gamma2_result)
      gamma2_rejectionratio = matrix(apply(gamma2_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma2_sim
      gamma2_rejectionratio = round(gamma2_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma_0 = 0.3
      txt.title = paste0("afttest","Scn",S1,"N",N,"gamma",gamma_0*10,"_result.txt")
      
      gamma3_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma3_sim = nrow(gamma3_result)
      gamma3_rejectionratio = matrix(apply(gamma3_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma3_sim
      gamma3_rejectionratio = round(gamma3_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma_0 = 0.4
      txt.title = paste0("afttest","Scn",S1,"N",N,"gamma",gamma_0*10,"_result.txt")
      
      gamma4_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma4_sim = nrow(gamma4_result)
      gamma4_rejectionratio = matrix(apply(gamma4_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma4_sim
      gamma4_rejectionratio = round(gamma4_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma_0 = 0.5
      txt.title = paste0("afttest","Scn",S1,"N",N,"gamma",gamma_0*10,"_result.txt")
      
      gamma5_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma5_sim = nrow(gamma5_result)
      gamma5_rejectionratio = matrix(apply(gamma5_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma5_sim
      gamma5_rejectionratio = round(gamma5_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      # ------------------------------------------------------------------------------
      Scn1n100 = cbind(gamma0_rejectionratio,gamma1_rejectionratio,gamma2_rejectionratio,
                       gamma3_rejectionratio,gamma4_rejectionratio,gamma5_rejectionratio)
    }
    
    N = 300
    {
      # ------------------------------------------------------------------------------
      # ------------------------------------------------------------------------------
      gamma_0 = 0
      txt.title = paste0("afttest","Scn",S1,"N",N,"gamma",gamma_0*10,"_result.txt")
      
      gamma0_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma0_sim = nrow(gamma0_result)
      gamma0_rejectionratio = matrix(apply(gamma0_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma0_sim
      gamma0_rejectionratio = round(gamma0_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma_0 = 0.1
      txt.title = paste0("afttest","Scn",S1,"N",N,"gamma",gamma_0*10,"_result.txt")
      
      gamma1_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma1_sim = nrow(gamma1_result)
      gamma1_rejectionratio = matrix(apply(gamma1_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma1_sim
      gamma1_rejectionratio = round(gamma1_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma_0 = 0.2
      txt.title = paste0("afttest","Scn",S1,"N",N,"gamma",gamma_0*10,"_result.txt")
      
      gamma2_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma2_sim = nrow(gamma2_result)
      gamma2_rejectionratio = matrix(apply(gamma2_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma2_sim
      gamma2_rejectionratio = round(gamma2_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma_0 = 0.3
      txt.title = paste0("afttest","Scn",S1,"N",N,"gamma",gamma_0*10,"_result.txt")
      
      gamma3_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma3_sim = nrow(gamma3_result)
      gamma3_rejectionratio = matrix(apply(gamma3_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma3_sim
      gamma3_rejectionratio = round(gamma3_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma_0 = 0.4
      txt.title = paste0("afttest","Scn",S1,"N",N,"gamma",gamma_0*10,"_result.txt")
      
      gamma4_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma4_sim = nrow(gamma4_result)
      gamma4_rejectionratio = matrix(apply(gamma4_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma4_sim
      gamma4_rejectionratio = round(gamma4_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma_0 = 0.5
      txt.title = paste0("afttest","Scn",S1,"N",N,"gamma",gamma_0*10,"_result.txt")
      
      gamma5_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma5_sim = nrow(gamma5_result)
      gamma5_rejectionratio = matrix(apply(gamma5_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma5_sim
      gamma5_rejectionratio = round(gamma5_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      # ------------------------------------------------------------------------------
      Scn1n300 = cbind(gamma0_rejectionratio,gamma1_rejectionratio,gamma2_rejectionratio,
                       gamma3_rejectionratio,gamma4_rejectionratio,gamma5_rejectionratio)
    }
    
    N = 500
    {
      # ------------------------------------------------------------------------------
      # ------------------------------------------------------------------------------
      gamma_0 = 0
      txt.title = paste0("afttest","Scn",S1,"N",N,"gamma",gamma_0*10,"_result.txt")
      
      gamma0_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma0_sim = nrow(gamma0_result)
      gamma0_rejectionratio = matrix(apply(gamma0_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma0_sim
      gamma0_rejectionratio = round(gamma0_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma_0 = 0.1
      txt.title = paste0("afttest","Scn",S1,"N",N,"gamma",gamma_0*10,"_result.txt")
      
      gamma1_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma1_sim = nrow(gamma1_result)
      gamma1_rejectionratio = matrix(apply(gamma1_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma1_sim
      gamma1_rejectionratio = round(gamma1_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma_0 = 0.2
      txt.title = paste0("afttest","Scn",S1,"N",N,"gamma",gamma_0*10,"_result.txt")
      
      gamma2_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma2_sim = nrow(gamma2_result)
      gamma2_rejectionratio = matrix(apply(gamma2_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma2_sim
      gamma2_rejectionratio = round(gamma2_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma_0 = 0.3
      txt.title = paste0("afttest","Scn",S1,"N",N,"gamma",gamma_0*10,"_result.txt")
      
      gamma3_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma3_sim = nrow(gamma3_result)
      gamma3_rejectionratio = matrix(apply(gamma3_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma3_sim
      gamma3_rejectionratio = round(gamma3_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma_0 = 0.4
      txt.title = paste0("afttest","Scn",S1,"N",N,"gamma",gamma_0*10,"_result.txt")
      
      gamma4_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma4_sim = nrow(gamma4_result)
      gamma4_rejectionratio = matrix(apply(gamma4_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma4_sim
      gamma4_rejectionratio = round(gamma4_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma_0 = 0.5
      txt.title = paste0("afttest","Scn",S1,"N",N,"gamma",gamma_0*10,"_result.txt")
      
      gamma5_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma5_sim = nrow(gamma5_result)
      gamma5_rejectionratio = matrix(apply(gamma5_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma5_sim
      gamma5_rejectionratio = round(gamma5_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      # ------------------------------------------------------------------------------
      Scn1n500 = cbind(gamma0_rejectionratio,gamma1_rejectionratio,gamma2_rejectionratio,
                       gamma3_rejectionratio,gamma4_rejectionratio,gamma5_rejectionratio)
    }
  }
  
  # ------------------------------------------------------------------------------
  # ------------------------------------------------------------------------------
  S2 = as.numeric(paste0(Scenario,2))
  {
    N = 100
    {
      # ------------------------------------------------------------------------------
      # ------------------------------------------------------------------------------
      gamma_0 = 0
      txt.title = paste0("afttest","Scn",S2,"N",N,"gamma",gamma_0*10,"_result.txt")
      
      gamma0_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma0_sim = nrow(gamma0_result)
      gamma0_rejectionratio = matrix(apply(gamma0_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma0_sim
      gamma0_rejectionratio = round(gamma0_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma_0 = 0.1
      txt.title = paste0("afttest","Scn",S2,"N",N,"gamma",gamma_0*10,"_result.txt")
      
      gamma1_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma1_sim = nrow(gamma1_result)
      gamma1_rejectionratio = matrix(apply(gamma1_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma1_sim
      gamma1_rejectionratio = round(gamma1_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma_0 = 0.2
      txt.title = paste0("afttest","Scn",S2,"N",N,"gamma",gamma_0*10,"_result.txt")
      
      gamma2_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma2_sim = nrow(gamma2_result)
      gamma2_rejectionratio = matrix(apply(gamma2_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma2_sim
      gamma2_rejectionratio = round(gamma2_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma_0 = 0.3
      txt.title = paste0("afttest","Scn",S2,"N",N,"gamma",gamma_0*10,"_result.txt")
      
      gamma3_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma3_sim = nrow(gamma3_result)
      gamma3_rejectionratio = matrix(apply(gamma3_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma3_sim
      gamma3_rejectionratio = round(gamma3_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma_0 = 0.4
      txt.title = paste0("afttest","Scn",S2,"N",N,"gamma",gamma_0*10,"_result.txt")
      
      gamma4_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma4_sim = nrow(gamma4_result)
      gamma4_rejectionratio = matrix(apply(gamma4_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma4_sim
      gamma4_rejectionratio = round(gamma4_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma_0 = 0.5
      txt.title = paste0("afttest","Scn",S2,"N",N,"gamma",gamma_0*10,"_result.txt")
      
      gamma5_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma5_sim = nrow(gamma5_result)
      gamma5_rejectionratio = matrix(apply(gamma5_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma5_sim
      gamma5_rejectionratio = round(gamma5_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      # ------------------------------------------------------------------------------
      Scn2n100 = cbind(gamma0_rejectionratio,gamma1_rejectionratio,gamma2_rejectionratio,
                       gamma3_rejectionratio,gamma4_rejectionratio,gamma5_rejectionratio)
    }
    
    N = 300
    {
      # ------------------------------------------------------------------------------
      # ------------------------------------------------------------------------------
      gamma_0 = 0
      txt.title = paste0("afttest","Scn",S2,"N",N,"gamma",gamma_0*10,"_result.txt")
      
      gamma0_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma0_sim = nrow(gamma0_result)
      gamma0_rejectionratio = matrix(apply(gamma0_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma0_sim
      gamma0_rejectionratio = round(gamma0_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma_0 = 0.1
      txt.title = paste0("afttest","Scn",S2,"N",N,"gamma",gamma_0*10,"_result.txt")
      
      gamma1_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma1_sim = nrow(gamma1_result)
      gamma1_rejectionratio = matrix(apply(gamma1_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma1_sim
      gamma1_rejectionratio = round(gamma1_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma_0 = 0.2
      txt.title = paste0("afttest","Scn",S2,"N",N,"gamma",gamma_0*10,"_result.txt")
      
      gamma2_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma2_sim = nrow(gamma2_result)
      gamma2_rejectionratio = matrix(apply(gamma2_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma2_sim
      gamma2_rejectionratio = round(gamma2_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma_0 = 0.3
      txt.title = paste0("afttest","Scn",S2,"N",N,"gamma",gamma_0*10,"_result.txt")
      
      gamma3_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma3_sim = nrow(gamma3_result)
      gamma3_rejectionratio = matrix(apply(gamma3_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma3_sim
      gamma3_rejectionratio = round(gamma3_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma_0 = 0.4
      txt.title = paste0("afttest","Scn",S2,"N",N,"gamma",gamma_0*10,"_result.txt")
      
      gamma4_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma4_sim = nrow(gamma4_result)
      gamma4_rejectionratio = matrix(apply(gamma4_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma4_sim
      gamma4_rejectionratio = round(gamma4_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma_0 = 0.5
      txt.title = paste0("afttest","Scn",S2,"N",N,"gamma",gamma_0*10,"_result.txt")
      
      gamma5_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma5_sim = nrow(gamma5_result)
      gamma5_rejectionratio = matrix(apply(gamma5_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma5_sim
      gamma5_rejectionratio = round(gamma5_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      # ------------------------------------------------------------------------------
      Scn2n300 = cbind(gamma0_rejectionratio,gamma1_rejectionratio,gamma2_rejectionratio,
                       gamma3_rejectionratio,gamma4_rejectionratio,gamma5_rejectionratio)
    }
    
    N = 500
    {
      # ------------------------------------------------------------------------------
      # ------------------------------------------------------------------------------
      gamma_0 = 0
      txt.title = paste0("afttest","Scn",S2,"N",N,"gamma",gamma_0*10,"_result.txt")
      
      gamma0_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma0_sim = nrow(gamma0_result)
      gamma0_rejectionratio = matrix(apply(gamma0_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma0_sim
      gamma0_rejectionratio = round(gamma0_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma_0 = 0.1
      txt.title = paste0("afttest","Scn",S2,"N",N,"gamma",gamma_0*10,"_result.txt")
      
      gamma1_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma1_sim = nrow(gamma1_result)
      gamma1_rejectionratio = matrix(apply(gamma1_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma1_sim
      gamma1_rejectionratio = round(gamma1_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma_0 = 0.2
      txt.title = paste0("afttest","Scn",S2,"N",N,"gamma",gamma_0*10,"_result.txt")
      
      gamma2_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma2_sim = nrow(gamma2_result)
      gamma2_rejectionratio = matrix(apply(gamma2_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma2_sim
      gamma2_rejectionratio = round(gamma2_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma_0 = 0.3
      txt.title = paste0("afttest","Scn",S2,"N",N,"gamma",gamma_0*10,"_result.txt")
      
      gamma3_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma3_sim = nrow(gamma3_result)
      gamma3_rejectionratio = matrix(apply(gamma3_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma3_sim
      gamma3_rejectionratio = round(gamma3_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma_0 = 0.4
      txt.title = paste0("afttest","Scn",S2,"N",N,"gamma",gamma_0*10,"_result.txt")
      
      gamma4_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma4_sim = nrow(gamma4_result)
      gamma4_rejectionratio = matrix(apply(gamma4_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma4_sim
      gamma4_rejectionratio = round(gamma4_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma_0 = 0.5
      txt.title = paste0("afttest","Scn",S2,"N",N,"gamma",gamma_0*10,"_result.txt")
      
      gamma5_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma5_sim = nrow(gamma5_result)
      gamma5_rejectionratio = matrix(apply(gamma5_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma5_sim
      gamma5_rejectionratio = round(gamma5_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      # ------------------------------------------------------------------------------
      Scn2n500 = cbind(gamma0_rejectionratio,gamma1_rejectionratio,gamma2_rejectionratio,
                       gamma3_rejectionratio,gamma4_rejectionratio,gamma5_rejectionratio)
    }
  }
  
  # ------------------------------------------------------------------------------
  # ------------------------------------------------------------------------------
  result = t(rbind(Scn1n100,Scn1n300,Scn1n500,Scn2n100,Scn2n300,Scn2n500))
  
  row2 = 1:18*2; row1 = row2 - 1
  resultdataframe = matrix(nrow=36, ncol=12)
  for (cc in 1:12) {
    ccc = cc*2
    cccc = ccc - 1
    resultdataframe[row1,cc] = result[,ccc]  # odd: standardized test 
    resultdataframe[row2,cc] = result[,cccc] # even: unstandardized test
  }
  colnames(resultdataframe) = rep(c("mns","mis"),6)
  rownames(resultdataframe) = rep(c("omni", "link", "form"),6, each = 2)
  
  resultdataframe = data.frame(gamma = rep(paste0("gamma",0:5), each = 6),
                               rep(c("omni", "link", "form"),6, each = 2),
                               resultdataframe)
  colnames(resultdataframe) = NULL
  rownames(resultdataframe) = NULL
  
  return(resultdataframe)
}
# End function definitions ---------------------------------------------------------