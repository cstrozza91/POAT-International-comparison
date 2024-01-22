### DATA PREPPING ###

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(HMDHFDplus)
library(readxl)
library(unpivotr)

source(file = "functions.R")

# Data download: life tables ----------------------------------------------

##total population
ita <- 
  readHMDweb(CNTRY = "ITA", 
                  item = "bltper_1x1", #total
                  username = "", 
                  password = "") %>% 
  filter(Year > 1949) %>% 
  mutate(Country = "ita") %>% 
  group_by(Year) %>% 
  mutate(e_dag1 = rev(cumsum(rev(dx/100000*ex))),
         ex_ave = ifelse(Age == 110, ex,
                         (ax*lead(ex) + (1-ax)*ex)),
         e_dag2 = rev(cumsum(rev(dx/100000*ex_ave))),
         entropy = e_dag2/ex,
         diff_e0 = ((Age+ax)-first(ex))^2,
         e0_var = rev(cumsum(rev(dx/100000*diff_e0))),
         e0_sd = sqrt(e0_var),
         e0_cv = e0_sd/ex,
         diff_e65 = ((Age+ax-65)-nth(ex, 66))^2,
         e65_var = rev(cumsum(rev(dx/100000*diff_e65))),
         e65_sd = sqrt(e65_var),
         e65_cv = e65_sd/ex)

fra <- 
  readHMDweb(CNTRY = "FRATNP", 
             item = "bltper_1x1", #total
             username = "", 
             password = "") %>% 
  filter(Year > 1949) %>% 
  mutate(Country = "fra") %>% 
  group_by(Year) %>% 
  mutate(e_dag1 = rev(cumsum(rev(dx/100000*ex))),
         ex_ave = ifelse(Age == 110, ex,
                         (ax*lead(ex) + (1-ax)*ex)),
         e_dag2 = rev(cumsum(rev(dx/100000*ex_ave))),
         entropy = e_dag2/ex,
         diff_e0 = ((Age+ax)-first(ex))^2,
         e0_var = rev(cumsum(rev(dx/100000*diff_e0))),
         e0_sd = sqrt(e0_var),
         e0_cv = e0_sd/ex,
         diff_e65 = ((Age+ax-65)-nth(ex, 66))^2,
         e65_var = rev(cumsum(rev(dx/100000*diff_e65))),
         e65_sd = sqrt(e65_var),
         e65_cv = e65_sd/ex)

dnk <- 
  readHMDweb(CNTRY = "DNK", 
             item = "bltper_1x1", #total
             username = "", 
             password = "") %>% 
  filter(Year > 1949) %>% 
  mutate(Country = "dnk") %>% 
  group_by(Year) %>% 
  mutate(e_dag1 = rev(cumsum(rev(dx/100000*ex))),
         ex_ave = ifelse(Age == 110, ex,
                         (ax*lead(ex) + (1-ax)*ex)),
         e_dag2 = rev(cumsum(rev(dx/100000*ex_ave))),
         entropy = e_dag2/ex,
         diff_e0 = ((Age+ax)-first(ex))^2,
         e0_var = rev(cumsum(rev(dx/100000*diff_e0))),
         e0_sd = sqrt(e0_var),
         e0_cv = e0_sd/ex,
         diff_e65 = ((Age+ax-65)-nth(ex, 66))^2,
         e65_var = rev(cumsum(rev(dx/100000*diff_e65))),
         e65_sd = sqrt(e65_var),
         e65_cv = e65_sd/ex)

swe <- 
  readHMDweb(CNTRY = "SWE", 
             item = "bltper_1x1", #total
             username = "", 
             password = "") %>% 
  filter(Year > 1949) %>% 
  mutate(Country = "swe") %>% 
  group_by(Year) %>% 
  mutate(e_dag1 = rev(cumsum(rev(dx/100000*ex))),
         ex_ave = ifelse(Age == 110, ex,
                         (ax*lead(ex) + (1-ax)*ex)),
         e_dag2 = rev(cumsum(rev(dx/100000*ex_ave))),
         entropy = e_dag2/ex,
         diff_e0 = ((Age+ax)-first(ex))^2,
         e0_var = rev(cumsum(rev(dx/100000*diff_e0))),
         e0_sd = sqrt(e0_var),
         e0_cv = e0_sd/ex,
         diff_e65 = ((Age+ax-65)-nth(ex, 66))^2,
         e65_var = rev(cumsum(rev(dx/100000*diff_e65))),
         e65_sd = sqrt(e65_var),
         e65_cv = e65_sd/ex)

uk <- 
  readHMDweb(CNTRY = "GBR_NP", 
             item = "bltper_1x1", #total
             username = "", 
             password = "") %>% 
  filter(Year > 1949) %>% 
  mutate(Country = "uk") %>% 
  group_by(Year) %>% 
  mutate(e_dag1 = rev(cumsum(rev(dx/100000*ex))),
         ex_ave = ifelse(Age == 110, ex,
                         (ax*lead(ex) + (1-ax)*ex)),
         e_dag2 = rev(cumsum(rev(dx/100000*ex_ave))),
         entropy = e_dag2/ex,
         diff_e0 = ((Age+ax)-first(ex))^2,
         e0_var = rev(cumsum(rev(dx/100000*diff_e0))),
         e0_sd = sqrt(e0_var),
         e0_cv = e0_sd/ex,
         diff_e65 = ((Age+ax-65)-nth(ex, 66))^2,
         e65_var = rev(cumsum(rev(dx/100000*diff_e65))),
         e65_sd = sqrt(e65_var),
         e65_cv = e65_sd/ex)

usa <- 
  readHMDweb(CNTRY = "USA", 
             item = "bltper_1x1", #total
             username = "", 
             password = "") %>% 
  filter(Year > 1949) %>% 
  mutate(Country = "usa") %>% 
  group_by(Year) %>% 
  mutate(e_dag1 = rev(cumsum(rev(dx/100000*ex))),
         ex_ave = ifelse(Age == 110, ex,
                         (ax*lead(ex) + (1-ax)*ex)),
         e_dag2 = rev(cumsum(rev(dx/100000*ex_ave))),
         entropy = e_dag2/ex,
         diff_e0 = ((Age+ax)-first(ex))^2,
         e0_var = rev(cumsum(rev(dx/100000*diff_e0))),
         e0_sd = sqrt(e0_var),
         e0_cv = e0_sd/ex,
         diff_e65 = ((Age+ax-65)-nth(ex, 66))^2,
         e65_var = rev(cumsum(rev(dx/100000*diff_e65))),
         e65_sd = sqrt(e65_var),
         e65_cv = e65_sd/ex)


##female
ita_f <- 
  readHMDweb(CNTRY = "ITA", 
             item = "fltper_1x1", #total
             username = "", 
             password = "") %>% 
  filter(Year > 1949) %>% 
  mutate(Country = "ita") %>% 
  group_by(Year) %>% 
  mutate(e_dag1 = rev(cumsum(rev(dx/100000*ex))),
         ex_ave = ifelse(Age == 110, ex,
                         (ax*lead(ex) + (1-ax)*ex)),
         e_dag2 = rev(cumsum(rev(dx/100000*ex_ave))),
         entropy = e_dag2/ex,
         diff_e0 = ((Age+ax)-first(ex))^2,
         e0_var = rev(cumsum(rev(dx/100000*diff_e0))),
         e0_sd = sqrt(e0_var),
         e0_cv = e0_sd/ex,
         diff_e65 = ((Age+ax-65)-nth(ex, 66))^2,
         e65_var = rev(cumsum(rev(dx/100000*diff_e65))),
         e65_sd = sqrt(e65_var),
         e65_cv = e65_sd/ex)

fra_f <- 
  readHMDweb(CNTRY = "FRATNP", 
             item = "fltper_1x1", #total
             username = "", 
             password = "") %>% 
  filter(Year > 1949) %>% 
  mutate(Country = "fra") %>% 
  group_by(Year) %>% 
  mutate(e_dag1 = rev(cumsum(rev(dx/100000*ex))),
         ex_ave = ifelse(Age == 110, ex,
                         (ax*lead(ex) + (1-ax)*ex)),
         e_dag2 = rev(cumsum(rev(dx/100000*ex_ave))),
         entropy = e_dag2/ex,
         diff_e0 = ((Age+ax)-first(ex))^2,
         e0_var = rev(cumsum(rev(dx/100000*diff_e0))),
         e0_sd = sqrt(e0_var),
         e0_cv = e0_sd/ex,
         diff_e65 = ((Age+ax-65)-nth(ex, 66))^2,
         e65_var = rev(cumsum(rev(dx/100000*diff_e65))),
         e65_sd = sqrt(e65_var),
         e65_cv = e65_sd/ex)

dnk_f <- 
  readHMDweb(CNTRY = "DNK", 
             item = "fltper_1x1", #total
             username = "", 
             password = "") %>% 
  filter(Year > 1949) %>% 
  mutate(Country = "dnk") %>% 
  group_by(Year) %>% 
  mutate(e_dag1 = rev(cumsum(rev(dx/100000*ex))),
         ex_ave = ifelse(Age == 110, ex,
                         (ax*lead(ex) + (1-ax)*ex)),
         e_dag2 = rev(cumsum(rev(dx/100000*ex_ave))),
         entropy = e_dag2/ex,
         diff_e0 = ((Age+ax)-first(ex))^2,
         e0_var = rev(cumsum(rev(dx/100000*diff_e0))),
         e0_sd = sqrt(e0_var),
         e0_cv = e0_sd/ex,
         diff_e65 = ((Age+ax-65)-nth(ex, 66))^2,
         e65_var = rev(cumsum(rev(dx/100000*diff_e65))),
         e65_sd = sqrt(e65_var),
         e65_cv = e65_sd/ex)

swe_f <- 
  readHMDweb(CNTRY = "SWE", 
             item = "fltper_1x1", #total
             username = "", 
             password = "") %>% 
  filter(Year > 1949) %>% 
  mutate(Country = "swe") %>% 
  group_by(Year) %>% 
  mutate(e_dag1 = rev(cumsum(rev(dx/100000*ex))),
         ex_ave = ifelse(Age == 110, ex,
                         (ax*lead(ex) + (1-ax)*ex)),
         e_dag2 = rev(cumsum(rev(dx/100000*ex_ave))),
         entropy = e_dag2/ex,
         diff_e0 = ((Age+ax)-first(ex))^2,
         e0_var = rev(cumsum(rev(dx/100000*diff_e0))),
         e0_sd = sqrt(e0_var),
         e0_cv = e0_sd/ex,
         diff_e65 = ((Age+ax-65)-nth(ex, 66))^2,
         e65_var = rev(cumsum(rev(dx/100000*diff_e65))),
         e65_sd = sqrt(e65_var),
         e65_cv = e65_sd/ex)

uk_f <- 
  readHMDweb(CNTRY = "GBR_NP", 
             item = "fltper_1x1", #total
             username = "", 
             password = "") %>% 
  filter(Year > 1949) %>% 
  mutate(Country = "uk") %>% 
  group_by(Year) %>% 
  mutate(e_dag1 = rev(cumsum(rev(dx/100000*ex))),
         ex_ave = ifelse(Age == 110, ex,
                         (ax*lead(ex) + (1-ax)*ex)),
         e_dag2 = rev(cumsum(rev(dx/100000*ex_ave))),
         entropy = e_dag2/ex,
         diff_e0 = ((Age+ax)-first(ex))^2,
         e0_var = rev(cumsum(rev(dx/100000*diff_e0))),
         e0_sd = sqrt(e0_var),
         e0_cv = e0_sd/ex,
         diff_e65 = ((Age+ax-65)-nth(ex, 66))^2,
         e65_var = rev(cumsum(rev(dx/100000*diff_e65))),
         e65_sd = sqrt(e65_var),
         e65_cv = e65_sd/ex)

usa_f <- 
  readHMDweb(CNTRY = "USA", 
             item = "fltper_1x1", #total
             username = "", 
             password = "") %>% 
  filter(Year > 1949) %>% 
  mutate(Country = "usa") %>% 
  group_by(Year) %>% 
  mutate(e_dag1 = rev(cumsum(rev(dx/100000*ex))),
         ex_ave = ifelse(Age == 110, ex,
                         (ax*lead(ex) + (1-ax)*ex)),
         e_dag2 = rev(cumsum(rev(dx/100000*ex_ave))),
         entropy = e_dag2/ex,
         diff_e0 = ((Age+ax)-first(ex))^2,
         e0_var = rev(cumsum(rev(dx/100000*diff_e0))),
         e0_sd = sqrt(e0_var),
         e0_cv = e0_sd/ex,
         diff_e65 = ((Age+ax-65)-nth(ex, 66))^2,
         e65_var = rev(cumsum(rev(dx/100000*diff_e65))),
         e65_sd = sqrt(e65_var),
         e65_cv = e65_sd/ex)


##male
ita_m <- 
  readHMDweb(CNTRY = "ITA", 
             item = "mltper_1x1", #total
             username = "", 
             password = "") %>% 
  filter(Year > 1949) %>% 
  mutate(Country = "ita") %>% 
  group_by(Year) %>% 
  mutate(e_dag1 = rev(cumsum(rev(dx/100000*ex))),
         ex_ave = ifelse(Age == 110, ex,
                         (ax*lead(ex) + (1-ax)*ex)),
         e_dag2 = rev(cumsum(rev(dx/100000*ex_ave))),
         entropy = e_dag2/ex,
         diff_e0 = ((Age+ax)-first(ex))^2,
         e0_var = rev(cumsum(rev(dx/100000*diff_e0))),
         e0_sd = sqrt(e0_var),
         e0_cv = e0_sd/ex,
         diff_e65 = ((Age+ax-65)-nth(ex, 66))^2,
         e65_var = rev(cumsum(rev(dx/100000*diff_e65))),
         e65_sd = sqrt(e65_var),
         e65_cv = e65_sd/ex)

fra_m <- 
  readHMDweb(CNTRY = "FRATNP", 
             item = "mltper_1x1", #total
             username = "", 
             password = "") %>% 
  filter(Year > 1949) %>% 
  mutate(Country = "fra") %>% 
  group_by(Year) %>% 
  mutate(e_dag1 = rev(cumsum(rev(dx/100000*ex))),
         ex_ave = ifelse(Age == 110, ex,
                         (ax*lead(ex) + (1-ax)*ex)),
         e_dag2 = rev(cumsum(rev(dx/100000*ex_ave))),
         entropy = e_dag2/ex,
         diff_e0 = ((Age+ax)-first(ex))^2,
         e0_var = rev(cumsum(rev(dx/100000*diff_e0))),
         e0_sd = sqrt(e0_var),
         e0_cv = e0_sd/ex,
         diff_e65 = ((Age+ax-65)-nth(ex, 66))^2,
         e65_var = rev(cumsum(rev(dx/100000*diff_e65))),
         e65_sd = sqrt(e65_var),
         e65_cv = e65_sd/ex)

dnk_m <- 
  readHMDweb(CNTRY = "DNK", 
             item = "mltper_1x1", #total
             username = "", 
             password = "") %>% 
  filter(Year > 1949) %>% 
  mutate(Country = "dnk") %>% 
  group_by(Year) %>% 
  mutate(e_dag1 = rev(cumsum(rev(dx/100000*ex))),
         ex_ave = ifelse(Age == 110, ex,
                         (ax*lead(ex) + (1-ax)*ex)),
         e_dag2 = rev(cumsum(rev(dx/100000*ex_ave))),
         entropy = e_dag2/ex,
         diff_e0 = ((Age+ax)-first(ex))^2,
         e0_var = rev(cumsum(rev(dx/100000*diff_e0))),
         e0_sd = sqrt(e0_var),
         e0_cv = e0_sd/ex,
         diff_e65 = ((Age+ax-65)-nth(ex, 66))^2,
         e65_var = rev(cumsum(rev(dx/100000*diff_e65))),
         e65_sd = sqrt(e65_var),
         e65_cv = e65_sd/ex)

swe_m <- 
  readHMDweb(CNTRY = "SWE", 
             item = "mltper_1x1", #total
             username = "", 
             password = "") %>% 
  filter(Year > 1949) %>% 
  mutate(Country = "swe") %>% 
  group_by(Year) %>% 
  mutate(e_dag1 = rev(cumsum(rev(dx/100000*ex))),
         ex_ave = ifelse(Age == 110, ex,
                         (ax*lead(ex) + (1-ax)*ex)),
         e_dag2 = rev(cumsum(rev(dx/100000*ex_ave))),
         entropy = e_dag2/ex,
         diff_e0 = ((Age+ax)-first(ex))^2,
         e0_var = rev(cumsum(rev(dx/100000*diff_e0))),
         e0_sd = sqrt(e0_var),
         e0_cv = e0_sd/ex,
         diff_e65 = ((Age+ax-65)-nth(ex, 66))^2,
         e65_var = rev(cumsum(rev(dx/100000*diff_e65))),
         e65_sd = sqrt(e65_var),
         e65_cv = e65_sd/ex)

uk_m <- 
  readHMDweb(CNTRY = "GBR_NP", 
             item = "mltper_1x1", #total
             username = "", 
             password = "") %>% 
  filter(Year > 1949) %>% 
  mutate(Country = "uk") %>% 
  group_by(Year) %>% 
  mutate(e_dag1 = rev(cumsum(rev(dx/100000*ex))),
         ex_ave = ifelse(Age == 110, ex,
                         (ax*lead(ex) + (1-ax)*ex)),
         e_dag2 = rev(cumsum(rev(dx/100000*ex_ave))),
         entropy = e_dag2/ex,
         diff_e0 = ((Age+ax)-first(ex))^2,
         e0_var = rev(cumsum(rev(dx/100000*diff_e0))),
         e0_sd = sqrt(e0_var),
         e0_cv = e0_sd/ex,
         diff_e65 = ((Age+ax-65)-nth(ex, 66))^2,
         e65_var = rev(cumsum(rev(dx/100000*diff_e65))),
         e65_sd = sqrt(e65_var),
         e65_cv = e65_sd/ex)

usa_m <- 
  readHMDweb(CNTRY = "USA", 
             item = "mltper_1x1", #total
             username = "", 
             password = "") %>% 
  filter(Year > 1949) %>% 
  mutate(Country = "usa") %>% 
  group_by(Year) %>% 
  mutate(e_dag1 = rev(cumsum(rev(dx/100000*ex))),
         ex_ave = ifelse(Age == 110, ex,
                         (ax*lead(ex) + (1-ax)*ex)),
         e_dag2 = rev(cumsum(rev(dx/100000*ex_ave))),
         entropy = e_dag2/ex,
         diff_e0 = ((Age+ax)-first(ex))^2,
         e0_var = rev(cumsum(rev(dx/100000*diff_e0))),
         e0_sd = sqrt(e0_var),
         e0_cv = e0_sd/ex,
         diff_e65 = ((Age+ax-65)-nth(ex, 66))^2,
         e65_var = rev(cumsum(rev(dx/100000*diff_e65))),
         e65_sd = sqrt(e65_var),
         e65_cv = e65_sd/ex)

# Italy 2020 LT to be added to HMD data -----------------------------------

#load ISTAT data: manually removed " anni" from "Età.e.classi.di.età" column
ita2020_full <- 
  read.csv(file = "Data/ita_lifetable_2020.csv", sep = ",")

#select desired columns
ita2020_red <- 
  ita2020_full %>% 
  select(Funzioni.biometriche, Sesso, Età.e.classi.di.età, Value) %>% 
  pivot_wider(c(Sesso, Età.e.classi.di.età), 
              names_from = Funzioni.biometriche, 
              values_from = Value)

#replicate HMD format
ita2020 <- 
  ita2020_red %>% 
  filter(Sesso == "totale") %>%
  arrange(Età.e.classi.di.età) %>% 
  mutate(Year = 2020,
         Age = Età.e.classi.di.età,
         mx = NA,
         qx = `probabilità di morte (per 1.000) - qx`/1000,
         ax = ifelse(Age == 0, 0.14, 0.5),
         lx = `sopravviventi - lx`,
         dx = `decessi - dx`,
         Lx = `anni vissuti - Lx`,
         Tx = rev(cumsum(rev(Lx))),
         ex = `speranza di vita - ex`,
         OpenInterval = F,
         Country = "ita") %>% 
  select(Year, Age, mx, qx, ax, lx, dx, Lx, Tx, ex, OpenInterval, Country) %>% 
  filter(Age < 111) %>% #remove to close LT below
  group_by(Year) %>% 
  mutate(e_dag1 = rev(cumsum(rev(dx/100000*ex))),
         ex_ave = ifelse(Age == 110, ex,
                         (ax*lead(ex) + (1-ax)*ex)),
         e_dag2 = rev(cumsum(rev(dx/100000*ex_ave))),
         entropy = e_dag2/ex,
         diff_e0 = ((Age+ax)-first(ex))^2,
         e0_var = rev(cumsum(rev(dx/100000*diff_e0))),
         e0_sd = sqrt(e0_var),
         e0_cv = e0_sd/ex,
         diff_e65 = ((Age+ax-65)-nth(ex, 66))^2,
         e65_var = rev(cumsum(rev(dx/100000*diff_e65))),
         e65_sd = sqrt(e65_var),
         e65_cv = e65_sd/ex)

ita2020_f <- 
  ita2020_red %>% 
  filter(Sesso == "femmine") %>%
  arrange(Età.e.classi.di.età) %>% 
  mutate(Year = 2020,
         Age = Età.e.classi.di.età,
         mx = NA,
         qx = `probabilità di morte (per 1.000) - qx`/1000,
         ax = ifelse(Age == 0, 0.14, 0.5),
         lx = `sopravviventi - lx`,
         dx = `decessi - dx`,
         Lx = `anni vissuti - Lx`,
         Tx = rev(cumsum(rev(Lx))),
         ex = `speranza di vita - ex`,
         OpenInterval = F,
         Country = "ita") %>% 
  select(Year, Age, mx, qx, ax, lx, dx, Lx, Tx, ex, OpenInterval, Country) %>% 
  filter(Age < 111) %>% #remove to close LT below
  group_by(Year) %>% 
  mutate(e_dag1 = rev(cumsum(rev(dx/100000*ex))),
         ex_ave = ifelse(Age == 110, ex,
                         (ax*lead(ex) + (1-ax)*ex)),
         e_dag2 = rev(cumsum(rev(dx/100000*ex_ave))),
         entropy = e_dag2/ex,
         diff_e0 = ((Age+ax)-first(ex))^2,
         e0_var = rev(cumsum(rev(dx/100000*diff_e0))),
         e0_sd = sqrt(e0_var),
         e0_cv = e0_sd/ex,
         diff_e65 = ((Age+ax-65)-nth(ex, 66))^2,
         e65_var = rev(cumsum(rev(dx/100000*diff_e65))),
         e65_sd = sqrt(e65_var),
         e65_cv = e65_sd/ex)

ita2020_m <- 
  ita2020_red %>% 
  filter(Sesso == "maschi") %>%
  arrange(Età.e.classi.di.età) %>% 
  mutate(Year = 2020,
         Age = Età.e.classi.di.età,
         mx = NA,
         qx = `probabilità di morte (per 1.000) - qx`/1000,
         ax = ifelse(Age == 0, 0.14, 0.5),
         lx = `sopravviventi - lx`,
         dx = `decessi - dx`,
         Lx = `anni vissuti - Lx`,
         Tx = rev(cumsum(rev(Lx))),
         ex = `speranza di vita - ex`,
         OpenInterval = F,
         Country = "ita") %>% 
  select(Year, Age, mx, qx, ax, lx, dx, Lx, Tx, ex, OpenInterval, Country) %>% 
  filter(Age < 111) %>% #remove to close LT below
  group_by(Year) %>% 
  mutate(e_dag1 = rev(cumsum(rev(dx/100000*ex))),
         ex_ave = ifelse(Age == 110, ex,
                         (ax*lead(ex) + (1-ax)*ex)),
         e_dag2 = rev(cumsum(rev(dx/100000*ex_ave))),
         entropy = e_dag2/ex,
         diff_e0 = ((Age+ax)-first(ex))^2,
         e0_var = rev(cumsum(rev(dx/100000*diff_e0))),
         e0_sd = sqrt(e0_var),
         e0_cv = e0_sd/ex,
         diff_e65 = ((Age+ax-65)-nth(ex, 66))^2,
         e65_var = rev(cumsum(rev(dx/100000*diff_e65))),
         e65_sd = sqrt(e65_var),
         e65_cv = e65_sd/ex)

#closing LT at age 110, necessary?
# ita2020 <- 
#   ita2020 %>% 
#   mutate(qx = ifelse(Age == 110, 1, qx),
#          #ax = ifelse(Age == 110, 1.3, ax),
#          lx = ifelse(Age == 110, lag(lx) - lag(dx), lx),
#          dx = ifelse(Age == 110, lx, dx),
#          Lx = ifelse(Age == 110, (lx - dx) + ax*dx, Lx),
#          Tx = ifelse(Age == 110, Lx, Tx),
#          ex = ifelse(Age == 110, Tx/lx, ex),
#          OpenInterval = ifelse(Age == 110, T, F)) %>% 
#   filter(Age < 111)

#append 2020 to HMD ita
ita <- 
  rbind(ita, ita2020)

ita_f <- 
  rbind(ita_f, ita2020_f)

ita_m <- 
  rbind(ita_m, ita2020_m)


# Data download: populations ----------------------------------------------

##populations, male and female
pop_ita <- 
  readHMDweb(CNTRY = "ITA", 
             item = "Population", #total
             username = "", 
             password = "") %>% 
  filter(Year > 1949) %>% 
  mutate(Country = "Italy")

pop_fra <- 
  readHMDweb(CNTRY = "FRATNP", 
             item = "Population", #total
             username = "", 
             password = "") %>% 
  filter(Year > 1949) %>% 
  mutate(Country = "France")

pop_dnk <- 
  readHMDweb(CNTRY = "DNK", 
             item = "Population", #total
             username = "", 
             password = "") %>% 
  filter(Year > 1949) %>% 
  mutate(Country = "Denmark")

pop_swe <- 
  readHMDweb(CNTRY = "SWE", 
             item = "Population", #total
             username = "", 
             password = "") %>% 
  filter(Year > 1949) %>% 
  mutate(Country = "Sweden")

pop_uk <- 
  readHMDweb(CNTRY = "GBR_NP", 
             item = "Population", #total
             username = "", 
             password = "") %>% 
  filter(Year > 1949) %>% 
  mutate(Country = "UK")

pop_usa <- 
  readHMDweb(CNTRY = "USA", 
             item = "Population", #total
             username = "", 
             password = "") %>% 
  filter(Year > 1949) %>% 
  mutate(Country = "USA")


# Italy 2020 population to be added to HMD --------------------------------

#load ISTAT data: manually removed " anni" from "Età.e.classi.di.età" column
ita2020_population <- 
  read.csv(file = "Data/ita_population_2020.csv", sep = ",")

#select desired columns
ita2020_population_red <- 
  ita2020_population %>% 
  select(Sesso, Età, Value) %>%
  pivot_wider(c(Età), 
              names_from = Sesso, 
              values_from = Value) %>% 
  mutate(Year = 2020,
         Female2 = NA,
         Male2 = NA,
         Total2 = NA,
         OpenInterval = ifelse(Età %in% 1:99, FALSE, TRUE),
         Country = "Italy") %>% 
  rename(Age = Età,
         Female1 = femmine,
         Male1 = maschi,
         Total1 = totale)

pop_ita <- 
  rbind(pop_ita, ita2020_population_red)

