

# Outcome: Thermal comfort
# DN Borg
# January 2025

# Set working directory
here::here()

# Packages
library(tidyverse)
library(dmetar)
library(meta)
library(janitor)
library(readxl)
library(naniar)

# Load data
d <- read_csv("data/data_comfort.csv") %>%
  clean_names()

head(d,10)

df <- d

# Check missing data
vis_miss(df)

# Meta analysis
meta_fit <- metacont(n.c = con_n,
                     mean.c = con_x,
                     sd.c = con_sd,
                     n.e = int_n,
                     mean.e = int_x,
                     sd.e = int_sd, 
                     data = df,
                     studlab = paste(study_id),
                     common = F,
                     random = T,
                     prediction = T,
                     sm = "SMD",
                     method.random.ci = "HK",
                     method.smd = "Hedges",
                     method.tau = "REML",
                     method.tau.ci = "QP")
summary(meta_fit)


#Save forest plot
png(file = 'figures/comfort.png',
    width = 10,
    height = 7,
    res = 600,
    units = "in")
forest(meta_fit, 
       sortvar = TE,
       #xlim = c(-2,2),
       xlab ="Favours L-Menthol     SMD         Favours Control",
       rightlabs = c("g","95% CI","Weight"),
       leftlabs = c("Author (year)", "N","Mean","SD","N","Mean","SD"),
       label.e = "Treatment",
       label.c = "Control",
       pooled.totals = T,
       smlab = "",
       text.random = "Overall effect",
       print.tau2 = T,
       print.tau2.ci = T,
       col.diamond = "gray",
       col.diamond.lines = "black",
       col.predict = "black",
       print.I2 = T,
       print.I2.ci = T,
       digits.tau2 = 3,
       digits.sd = 1,
       digits.mean = 1,
       mlab = "",
       ilab.xpos = 8,
       ilab.pos = 8,
       showweights = T)
dev.off()



# Sensitivity analysis
loo_results <- data.frame()
n_studies <- length(unique(df$study_id))

for (i in 1:n_studies) {
  d_loo <- df[-i, ]
  
  fits <- metacont(n.c = con_n,
                   mean.c = con_x,
                   sd.c = con_sd,
                   n.e = int_n,
                   mean.e = int_x,
                   sd.e = int_sd, 
                   data = d_loo,
                   studlab = paste(study_id),
                   common = F,
                   random = T,
                   prediction = T,
                   sm = "SMD",
                   method.random.ci = "HK",
                   method.smd = "Hedges",
                   method.tau = "REML",
                   method.tau.ci = "QP")
  
  result <- data.frame(
    study_left_out = df$study_id[i],
    smd = fits$TE.random,
    lower_ci = fits$lower.random,
    upper_ci = fits$upper.random,
    i2 = fits$I2,
    tau2 = fits$tau^2,
    lower_pi = fits$lower.predict,
    upper_pi = fits$upper.predict
  )     
  
  loo_results <- rbind(loo_results, result)
}

# Save results
loo_results %>%
  mutate(across(c(smd,
                  lower_ci,
                  upper_ci,
                  lower_pi,
                  upper_pi,
                  i2,
                  tau2),
                ~ round(.x, 2))) %>%
  arrange(smd) %>%
  gt() %>%
  cols_align(align = "left") -> res_table; res_table

gtsave(res_table, "results/sensitivity_comfort.docx")


# Check fixed effect model
meta_fit_fixed <-
  metacont(n.c = con_n,
                     mean.c = con_x,
                     sd.c = con_sd,
                     n.e = int_n,
                     mean.e = int_x,
                     sd.e = int_sd, 
                     data = df,
                     studlab = paste(study_id),
                     common = T,
                     random = F,
                     prediction = F,
                     sm = "SMD",
                     method.random.ci = "HK",
                     method.smd = "Hedges",
                     method.tau = "REML",
                     method.tau.ci = "QP")
summary(meta_fit_fixed)



#### End

