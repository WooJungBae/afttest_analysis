library(knitr)
library(kableExtra)

setwd("C:/Users/WooJung/Documents/Rproject/afttest_analysis/source")
source("afttest_source_r.R")

# setwd("C:/Users/WooJung/Desktop")
# setwd("C:/Users/WooJung/Documents/Rproject/afttest_analysis")
setwd("C:/Users/WooJung/Documents/Rproject/afttest_analysis/SimulationStudy")

Scenario = 1;result_Scn1 = rejectionratio(Scenario)
kable(result_Scn1, digits = 3, "latex", booktabs = T, escape = F,
      col.names = c("gamma", "test", rep(c("mns","mis"),6)),
      caption = "Simulation results scenario 1",
      label = "sim1:result", align = "c") %>%
  add_header_above(c("n"=2,"100"=2,"300"=2,"500"=2,"100"=2,"300"=2,"500"=2)) %>%
  add_header_above(c("censoring"=2,"20 \\%"=6,"40 \\%"=6)) %>%
  kable_styling() %>%
  collapse_rows(columns = 1:2, latex_hline = "major", valign = "middle") %>%
  row_spec(row = (1:18*2 - 1), bold = TRUE)

Scenario = 2;result_Scn2 = rejectionratio(Scenario)
kable(result_Scn2, digits = 3, "latex", booktabs = T, escape = F,
      col.names = c("gamma", "test", rep(c("mns","mis"),6)),
      caption = "Simulation results scenario 2",
      label = "sim2:result", align = "c") %>%
  add_header_above(c("n"=2,"100"=2,"300"=2,"500"=2,"100"=2,"300"=2,"500"=2)) %>%
  add_header_above(c("censoring"=2,"20 \\%"=6,"40 \\%"=6)) %>%
  kable_styling() %>%
  collapse_rows(columns = 1:2, latex_hline = "major", valign = "middle") %>%
  row_spec(row = (1:18*2 - 1), bold = TRUE)

Scenario = 3;result_Scn3 = rejectionratio(Scenario)
kable(result_Scn3, digits = 3, "latex", booktabs = T, escape = F,
      col.names = c("gamma", "test", rep(c("mns","mis"),6)),
      caption = "Simulation results scenario 3",
      label = "sim3:result", align = "c") %>%
  add_header_above(c("n"=2,"100"=2,"300"=2,"500"=2,"100"=2,"300"=2,"500"=2)) %>%
  add_header_above(c("censoring"=2,"20 \\%"=6,"40 \\%"=6)) %>%
  kable_styling() %>%
  collapse_rows(columns = 1:2, latex_hline = "major", valign = "middle") %>%
  row_spec(row = (1:18*2 - 1), bold = TRUE)

Scenario = 4;result_Scn4 = rejectionratio(Scenario)
kable(result_Scn4, digits = 3, "latex", booktabs = T, escape = F,
      col.names = c("gamma", "test", rep(c("mns","mis"),6)),
      caption = "Simulation results scenario 4",
      label = "sim4:result", align = "c") %>%
  add_header_above(c("n"=2,"100"=2,"300"=2,"500"=2,"100"=2,"300"=2,"500"=2)) %>%
  add_header_above(c("censoring"=2,"20 \\%"=6,"40 \\%"=6)) %>%
  kable_styling() %>%
  collapse_rows(columns = 1:2, latex_hline = "major", valign = "middle") %>%
  row_spec(row = (1:18*2 - 1), bold = TRUE)





