# setwd("C:/Users/WooJung/Desktop")
# setwd("C:/Users/WooJung/Documents/Rproject/EDPmediation")
setwd("C:/Users/WooJung/Documents/Rproject/EDPmediation/SimulationStudy")

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# True values of E_Yz1m1, E_Yz1m0 and E_Yz0m0
E_Yz1m1_D7_true = 5.935422
E_Yz1m0_D7_true = 5.319422
E_Yz0m0_D7_true = 4.019422

D7_result = read.table("D7_result.txt", header = TRUE, sep = "", dec = ".")

D7_sim = nrow(D7_result)

D7_median = apply(D7_result,2,median);D7_median
D7_mean = apply(D7_result,2,mean);D7_mean
D7_sd = apply(D7_result,2,sd);D7_sd

D7_E_Yz0m0_est = D7_median[8];D7_E_Yz0m0_est
D7_E_Yz1m0_est = D7_median[9];D7_E_Yz1m0_est
D7_E_Yz1m1_est = D7_median[10];D7_E_Yz1m1_est

D7_E_Yz0m0_bias = D7_E_Yz0m0_est - E_Yz0m0_D7_true;D7_E_Yz0m0_bias
D7_E_Yz1m0_bias = D7_E_Yz1m0_est - E_Yz1m0_D7_true;D7_E_Yz1m0_bias
D7_E_Yz1m1_bias = D7_E_Yz1m1_est - E_Yz1m1_D7_true;D7_E_Yz1m1_bias

# D7_E_Yz1m1_mse = (sum((D7_result$E_Yz1m1-D7_E_Yz1m1_est)^{2}))/(D7_sim-1) + D7_E_Yz1m1_bias^{2};D7_E_Yz1m1_mse
D7_E_Yz0m0_mse = (D7_sd[8]^{2}) + D7_E_Yz0m0_bias^{2};D7_E_Yz0m0_mse
D7_E_Yz1m0_mse = (D7_sd[9]^{2}) + D7_E_Yz1m0_bias^{2};D7_E_Yz1m0_mse
D7_E_Yz1m1_mse = (D7_sd[10]^{2}) + D7_E_Yz1m1_bias^{2};D7_E_Yz1m1_mse

D7_result_table = rbind(cbind(E_Yz0m0_D7_true,E_Yz1m0_D7_true,E_Yz1m1_D7_true),
                        cbind(D7_median[2],D7_median[3],D7_median[4]),
                        cbind(D7_median[5],D7_median[6],D7_median[7]),
                        cbind(D7_E_Yz0m0_est,D7_E_Yz1m0_est,D7_E_Yz1m1_est),
                        cbind(D7_E_Yz0m0_bias,D7_E_Yz1m0_bias,D7_E_Yz1m1_bias),
                        cbind(D7_E_Yz0m0_mse,D7_E_Yz1m0_mse,D7_E_Yz1m1_mse))
D7_result_table = round(D7_result_table,3)
rownames(D7_result_table) = c("TRUE","Data","EDP","G-comp","Bias","MSE")
colnames(D7_result_table) = c("E_Yz0m0","E_Yz1m0","E_Yz1m1")

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# True values of E_Yz1m1, E_Yz1m0 and E_Yz0m0
E_Yz1m1_D8_true = 5.132737
E_Yz1m0_D8_true = 4.146737
E_Yz0m0_D8_true = 3.843422

D8_result = read.table("D8_result.txt", header = TRUE, sep = "", dec = ".")

D8_sim = nrow(D8_result)

D8_median = apply(D8_result,2,median);D8_median
D8_mean = apply(D8_result,2,mean);D8_mean
D8_sd = apply(D8_result,2,sd);D8_sd

D8_E_Yz0m0_est = D8_median[8];D8_E_Yz0m0_est
D8_E_Yz1m0_est = D8_median[9];D8_E_Yz1m0_est
D8_E_Yz1m1_est = D8_median[10];D8_E_Yz1m1_est

D8_E_Yz0m0_bias = D8_E_Yz0m0_est - E_Yz0m0_D8_true;D8_E_Yz0m0_bias
D8_E_Yz1m0_bias = D8_E_Yz1m0_est - E_Yz1m0_D8_true;D8_E_Yz1m0_bias
D8_E_Yz1m1_bias = D8_E_Yz1m1_est - E_Yz1m1_D8_true;D8_E_Yz1m1_bias

# D8_E_Yz1m1_mse = (sum((D8_result$E_Yz1m1-D8_E_Yz1m1_est)^{2}))/(D8_sim-1) + D8_E_Yz1m1_bias^{2};D8_E_Yz1m1_mse
D8_E_Yz0m0_mse = (D8_sd[8]^{2}) + D8_E_Yz0m0_bias^{2};D8_E_Yz0m0_mse
D8_E_Yz1m0_mse = (D8_sd[9]^{2}) + D8_E_Yz1m0_bias^{2};D8_E_Yz1m0_mse
D8_E_Yz1m1_mse = (D8_sd[10]^{2}) + D8_E_Yz1m1_bias^{2};D8_E_Yz1m1_mse

D8_result_table = rbind(cbind(E_Yz0m0_D8_true,E_Yz1m0_D8_true,E_Yz1m1_D8_true),
                        cbind(D8_median[2],D8_median[3],D8_median[4]),
                        cbind(D8_median[5],D8_median[6],D8_median[7]),
                        cbind(D8_E_Yz0m0_est,D8_E_Yz1m0_est,D8_E_Yz1m1_est),
                        cbind(D8_E_Yz0m0_bias,D8_E_Yz1m0_bias,D8_E_Yz1m1_bias),
                        cbind(D8_E_Yz0m0_mse,D8_E_Yz1m0_mse,D8_E_Yz1m1_mse))
D8_result_table = round(D8_result_table,3)
rownames(D8_result_table) = c("TRUE","Data","EDP","G-comp","Bias","MSE")
colnames(D8_result_table) = c("E_Yz0m0","E_Yz1m0","E_Yz1m1")

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# True values of E_Yz1m1, E_Yz1m0 and E_Yz0m0
E_Yz1m1_D9_true = 9.481157
E_Yz1m0_D9_true = 9.060581
E_Yz0m0_D9_true = 6.560580

D9_result = read.table("D9_result.txt", header = TRUE, sep = "", dec = ".")

D9_sim = nrow(D9_result)

D9_median = apply(D9_result,2,median);D9_median
D9_mean = apply(D9_result,2,mean);D9_mean
D9_sd = apply(D9_result,2,sd);D9_sd

D9_E_Yz0m0_est = D9_median[8];D9_E_Yz0m0_est
D9_E_Yz1m0_est = D9_median[9];D9_E_Yz1m0_est
D9_E_Yz1m1_est = D9_median[10];D9_E_Yz1m1_est

D9_E_Yz0m0_bias = D9_E_Yz0m0_est - E_Yz0m0_D9_true;D9_E_Yz0m0_bias
D9_E_Yz1m0_bias = D9_E_Yz1m0_est - E_Yz1m0_D9_true;D9_E_Yz1m0_bias
D9_E_Yz1m1_bias = D9_E_Yz1m1_est - E_Yz1m1_D9_true;D9_E_Yz1m1_bias

# D9_E_Yz1m1_mse = (sum((D9_result$E_Yz1m1-D9_E_Yz1m1_est)^{2}))/(D9_sim-1) + D9_E_Yz1m1_bias^{2};D9_E_Yz1m1_mse
D9_E_Yz0m0_mse = (D9_sd[8]^{2}) + D9_E_Yz0m0_bias^{2};D9_E_Yz0m0_mse
D9_E_Yz1m0_mse = (D9_sd[9]^{2}) + D9_E_Yz1m0_bias^{2};D9_E_Yz1m0_mse
D9_E_Yz1m1_mse = (D9_sd[10]^{2}) + D9_E_Yz1m1_bias^{2};D9_E_Yz1m1_mse

D9_result_table = rbind(cbind(E_Yz0m0_D9_true,E_Yz1m0_D9_true,E_Yz1m1_D9_true),
                        cbind(D9_median[2],D9_median[3],D9_median[4]),
                        cbind(D9_median[5],D9_median[6],D9_median[7]),
                        cbind(D9_E_Yz0m0_est,D9_E_Yz1m0_est,D9_E_Yz1m1_est),
                        cbind(D9_E_Yz0m0_bias,D9_E_Yz1m0_bias,D9_E_Yz1m1_bias),
                        cbind(D9_E_Yz0m0_mse,D9_E_Yz1m0_mse,D9_E_Yz1m1_mse))
D9_result_table = round(D9_result_table,3)
rownames(D9_result_table) = c("TRUE","Data","EDP","G-comp","Bias","MSE")
colnames(D9_result_table) = c("E_Yz0m0","E_Yz1m0","E_Yz1m1")

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
D7_result_table
D8_result_table
D9_result_table