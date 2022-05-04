# setwd("C:/Users/WooJung/Desktop")
# setwd("C:/Users/WooJung/Documents/Rproject/afttest_analysis")
setwd("C:/Users/WooJung/Documents/Rproject/afttest_analysis/SimulationStudy")

# Type 1 error control
alpha = 0.05

# 
Scenario = 11
N = 100

# ------------------------------------------------------------------------------
gamma_0 = 0
txt.title = paste0("afttest","Scn",Scenario,"N",N,"gamma",gamma_0*10,"_result.txt")

gamma0_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
gamma0_sim = nrow(gamma0_result)-sum(apply(gamma0_result, 1, function(l) (sum(is.na(l))>0)))
gamma0_rejectionratio = matrix(apply(gamma0_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma0_sim
gamma0_rejectionratio = round(gamma0_rejectionratio,4)
rownames(gamma0_rejectionratio) = c("mns","mnsstd","mis","misstd")
colnames(gamma0_rejectionratio) = c("omni","link","form")

# ------------------------------------------------------------------------------
gamma_0 = 0.1
txt.title = paste0("afttest","Scn",Scenario,"N",N,"gamma",gamma_0*10,"_result.txt")

gamma1_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
gamma1_sim = nrow(gamma1_result)-sum(apply(gamma1_result, 1, function(l) (sum(is.na(l))>0)))
gamma1_rejectionratio = matrix(apply(gamma1_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma1_sim
gamma1_rejectionratio = round(gamma1_rejectionratio,4)
rownames(gamma1_rejectionratio) = c("mns","mnsstd","mis","misstd")
colnames(gamma1_rejectionratio) = c("omni","link","form")

# ------------------------------------------------------------------------------
gamma_0 = 0.2
txt.title = paste0("afttest","Scn",Scenario,"N",N,"gamma",gamma_0*10,"_result.txt")

gamma2_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
gamma2_sim = nrow(gamma2_result)-sum(apply(gamma2_result, 1, function(l) (sum(is.na(l))>0)))
gamma2_rejectionratio = matrix(apply(gamma2_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma2_sim
gamma2_rejectionratio = round(gamma2_rejectionratio,4)
rownames(gamma2_rejectionratio) = c("mns","mnsstd","mis","misstd")
colnames(gamma2_rejectionratio) = c("omni","link","form")

# ------------------------------------------------------------------------------
gamma_0 = 0.3
txt.title = paste0("afttest","Scn",Scenario,"N",N,"gamma",gamma_0*10,"_result.txt")

gamma3_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
gamma3_sim = nrow(gamma3_result)-sum(apply(gamma3_result, 1, function(l) (sum(is.na(l))>0)))
gamma3_rejectionratio = matrix(apply(gamma3_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma3_sim
gamma3_rejectionratio = round(gamma3_rejectionratio,4)
rownames(gamma3_rejectionratio) = c("mns","mnsstd","mis","misstd")
colnames(gamma3_rejectionratio) = c("omni","link","form")

# ------------------------------------------------------------------------------
gamma_0 = 0.4
txt.title = paste0("afttest","Scn",Scenario,"N",N,"gamma",gamma_0*10,"_result.txt")

gamma4_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
gamma4_sim = nrow(gamma4_result)-sum(apply(gamma4_result, 1, function(l) (sum(is.na(l))>0)))
gamma4_rejectionratio = matrix(apply(gamma4_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma4_sim
gamma4_rejectionratio = round(gamma4_rejectionratio,4)
rownames(gamma4_rejectionratio) = c("mns","mnsstd","mis","misstd")
colnames(gamma4_rejectionratio) = c("omni","link","form")

# ------------------------------------------------------------------------------
gamma_0 = 0.5
txt.title = paste0("afttest","Scn",Scenario,"N",N,"gamma",gamma_0*10,"_result.txt")

gamma5_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
gamma5_sim = nrow(gamma5_result)-sum(apply(gamma5_result, 1, function(l) (sum(is.na(l))>0)))
gamma5_rejectionratio = matrix(apply(gamma5_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma5_sim
gamma5_rejectionratio = round(gamma5_rejectionratio,4)
rownames(gamma5_rejectionratio) = c("mns","mnsstd","mis","misstd")
colnames(gamma5_rejectionratio) = c("omni","link","form")

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
gamma0_rejectionratio
gamma1_rejectionratio
gamma2_rejectionratio
gamma3_rejectionratio
gamma4_rejectionratio
gamma5_rejectionratio
