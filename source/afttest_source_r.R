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
    
  } else if (Scenario==31){
    # --------------------------------- Scenario 3 ---------------------------------
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
    T_data = exp(- beta_1 * Z1 - beta_2 * Z2 - gamma_0 * Z1 * Z2 + rexp(N,1))
    C_data = exp(- beta_1 * Z1 - beta_2 * Z2 - gamma_0 * Z1 * Z2 + rexp(N,1/4))
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
    T_data = exp(- beta_1 * Z1 - beta_2 * Z2 - gamma_0 * Z1 * Z2 + rexp(N,1))
    C_data = exp(- beta_1 * Z1 - beta_2 * Z2 - gamma_0 * Z1 * Z2 + rexp(N,1/1.5))
    X_data = C_data * (T_data > C_data) + T_data * (T_data <= C_data)
    D_data = 0 * (T_data > C_data) + 1 * (T_data <= C_data)
    Z_data = cbind(Z1,Z2)
  }
  
  return(list(X=X_data, D=D_data, Z=Z_data))
}

# End function definitions ---------------------------------------------------------