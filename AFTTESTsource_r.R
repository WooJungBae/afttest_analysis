# Define functions -----------------------------------------------------------------

# Define function to generate datasets
generate_data = function(N,gamma_0,Scenario) {
  
  if (Scenario==1){
    # --------------------------------- Scenario 1 ---------------------------------
    # True Coefficient: beta_0 = 1
    beta_0 = 1
    
    # ------------------------------------------------------------------------------
    # Generate covariate Z
    Z = matrix(rnorm(N,3,1),nrow=N)
    
    # ------------------------------------------------------------------------------
    # T_data: true event time
    # C_data: true censoring time
    # X_data: observed time
    # D_data: observed indicator
    # Z_data: covariates fitted
    T_data = as.vector(exp(-beta_0*Z-gamma_0*(Z^2)+rnorm(N,5,1)))
    C_data = as.vector(exp(-beta_0*Z-gamma_0*(Z^2)+rnorm(N,6.5,1)))
    X_data = C_data*(T_data>C_data)+T_data*(T_data<=C_data)
    D_data = 0*(T_data>C_data)+1*(T_data<=C_data)
    Z_data = Z
    
    # ------------------------------------------------------------------------------
  } else if (Scenario==2){
    # --------------------------------- Scenario 2 ---------------------------------
    # True Coefficient: beta_0 = 1
    beta_00 = 1
    beta_01 = 1
    
    # ------------------------------------------------------------------------------
    # Generate covariate Z
    Z1 = rbinom(N,1,0.5)
    Z2 = rnorm(N,3,1)
    
    # ------------------------------------------------------------------------------
    # T_data: true event time
    # C_data: true censoring time
    # X_data: observed time
    # D_data: observed indicator
    # Z_data: covariates fitted
    T_data = exp(- beta_00 * Z1 - beta_01 * Z2 - gamma_0 * (Z2^{2}) + rexp(N,3))
    C_data = exp(- beta_00 * Z1 - beta_01 * Z2 - gamma_0 * (Z2^{2}) + rexp(N,4))
    X_data = C_data * (T_data > C_data) + T_data * (T_data <= C_data)
    D_data = 0 * (T_data > C_data) + 1 * (T_data <= C_data)
    Z_data = cbind(Z1,Z2)
  } 
  
  return(list(X=X_data, D=D_data, Z=Z_data))
}

# End function definitions ---------------------------------------------------------