### DATA ANALYSIS ###

# Libraries ---------------------------------------------------------------

source("functions.R")
library(tidyverse)
library(viridis)
library(scales)

# Analysis: international comparison between countries male ---------------

#new values of age for smoothing 
newAge <- seq(0, 110, by = 0.001)

#reference value: same for all
ref.uk_m <- 
  uk_m %>% 
  filter(Year == 1950,
         Age == 65) %>% 
  summarise(ex = ex) %>% 
  as.numeric()


## ITA
#df to fill with prospective ages
prosp_age_ita_m <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function prosp.age
for(i in min(ita_m$Year):max(ita_m$Year)){
  
  df <- ita_m %>% filter(Year == i)
  age <- prosp.age(df$Age, newAge, df$ex, ref.uk_m)
  prosp_age_ita_m <- 
    rbind(prosp_age_ita_m,
          t(c(i,age)))
}

#rename columns
colnames(prosp_age_ita_m) <- c("year", "prosp_age")


## FRA
#df to fill with prospective ages
prosp_age_fra_m <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function prosp.age
for(i in min(fra_m$Year):max(fra_m$Year)){
  
  df <- fra_m %>% filter(Year == i)
  age <- prosp.age(df$Age, newAge, df$ex, ref.uk_m)
  prosp_age_fra_m <- 
    rbind(prosp_age_fra_m,
          t(c(i,age)))
}

#rename columns
colnames(prosp_age_fra_m) <- c("year", "prosp_age")


## DNK
#df to fill with prospective ages
prosp_age_dnk_m <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function prosp.age
for(i in min(dnk_m$Year):max(dnk_m$Year)){
  
  df <- dnk_m %>% filter(Year == i)
  age <- prosp.age(df$Age, newAge, df$ex, ref.uk_m)
  prosp_age_dnk_m <- 
    rbind(prosp_age_dnk_m,
          t(c(i,age)))
}

#rename columns
colnames(prosp_age_dnk_m) <- c("year", "prosp_age")


## SWE
#df to fill with prospective ages
prosp_age_swe_m <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function prosp.age
for(i in min(swe_m$Year):max(swe_m$Year)){
  
  df <- swe_m %>% filter(Year == i)
  age <- prosp.age(df$Age, newAge, df$ex, ref.uk_m)
  prosp_age_swe_m <- 
    rbind(prosp_age_swe_m,
          t(c(i,age)))
}

#rename columns
colnames(prosp_age_swe_m) <- c("year", "prosp_age")


## UK
#df to fill with prospective ages
prosp_age_uk_m <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function prosp.age
for(i in min(uk_m$Year):max(uk_m$Year)){
  
  df <- uk_m %>% filter(Year == i)
  age <- prosp.age(df$Age, newAge, df$ex, ref.uk_m)
  prosp_age_uk_m <- 
    rbind(prosp_age_uk_m,
          t(c(i,age)))
}

#rename columns
colnames(prosp_age_uk_m) <- c("year", "prosp_age")

## USA
#df to fill with prospective ages
prosp_age_usa_m <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function prosp.age
for(i in min(usa_m$Year):max(usa_m$Year)){
  
  df <- usa_m %>% filter(Year == i)
  age <- prosp.age(df$Age, newAge, df$ex, ref.uk_m)
  prosp_age_usa_m <- 
    rbind(prosp_age_usa_m,
          t(c(i,age)))
}

#rename columns
colnames(prosp_age_usa_m) <- c("year", "prosp_age")

## MERGE ALL
prosp_age_m <- 
  rbind(prosp_age_ita_m %>% mutate(country = "Italy"),
        prosp_age_fra_m %>% mutate(country = "France"),
        prosp_age_dnk_m %>% mutate(country = "Denmark"),
        prosp_age_swe_m %>% mutate(country = "Sweden"),
        prosp_age_uk_m %>% mutate(country = "UK"),
        prosp_age_usa_m %>% mutate(country = "USA")) %>% 
  filter(year < 2021) %>% 
  mutate(sex = "Male")

# Analysis: international comparison between countries female -------------

#new values of age for smoothing 
newAge <- seq(0, 110, by = 0.001)

#reference value: same for all
ref.uk_m <- 
  uk_m %>% 
  filter(Year == 1950,
         Age == 65) %>% 
  summarise(ex = ex) %>% 
  as.numeric()


## ITA
#df to fill with prospective ages
prosp_age_ita_f <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function prosp.age
for(i in min(ita_f$Year):max(ita_f$Year)){
  
  df <- ita_f %>% filter(Year == i)
  age <- prosp.age(df$Age, newAge, df$ex, ref.uk_m)
  prosp_age_ita_f <- 
    rbind(prosp_age_ita_f,
          t(c(i,age)))
}

#rename columns
colnames(prosp_age_ita_f) <- c("year", "prosp_age")


## FRA
#df to fill with prospective ages
prosp_age_fra_f <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function prosp.age
for(i in min(fra_f$Year):max(fra_f$Year)){
  
  df <- fra_f %>% filter(Year == i)
  age <- prosp.age(df$Age, newAge, df$ex, ref.uk_m)
  prosp_age_fra_f <- 
    rbind(prosp_age_fra_f,
          t(c(i,age)))
}

#rename columns
colnames(prosp_age_fra_f) <- c("year", "prosp_age")


## DNK
#df to fill with prospective ages
prosp_age_dnk_f <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function prosp.age
for(i in min(dnk_f$Year):max(dnk_f$Year)){
  
  df <- dnk_f %>% filter(Year == i)
  age <- prosp.age(df$Age, newAge, df$ex, ref.uk_m)
  prosp_age_dnk_f <- 
    rbind(prosp_age_dnk_f,
          t(c(i,age)))
}

#rename columns
colnames(prosp_age_dnk_f) <- c("year", "prosp_age")


## SWE
#df to fill with prospective ages
prosp_age_swe_f <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function prosp.age
for(i in min(swe_f$Year):max(swe_f$Year)){
  
  df <- swe_f %>% filter(Year == i)
  age <- prosp.age(df$Age, newAge, df$ex, ref.uk_m)
  prosp_age_swe_f <- 
    rbind(prosp_age_swe_f,
          t(c(i,age)))
}

#rename columns
colnames(prosp_age_swe_f) <- c("year", "prosp_age")


## UK
#df to fill with prospective ages
prosp_age_uk_f <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function prosp.age
for(i in min(uk_f$Year):max(uk_f$Year)){
  
  df <- uk_f %>% filter(Year == i)
  age <- prosp.age(df$Age, newAge, df$ex, ref.uk_m)
  prosp_age_uk_f <- 
    rbind(prosp_age_uk_f,
          t(c(i,age)))
}

#rename columns
colnames(prosp_age_uk_f) <- c("year", "prosp_age")

## USA
#df to fill with prospective ages
prosp_age_usa_f <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function prosp.age
for(i in min(usa_f$Year):max(usa_f$Year)){
  
  df <- usa_f %>% filter(Year == i)
  age <- prosp.age(df$Age, newAge, df$ex, ref.uk_m)
  prosp_age_usa_f <- 
    rbind(prosp_age_usa_f,
          t(c(i,age)))
}

#rename columns
colnames(prosp_age_usa_f) <- c("year", "prosp_age")

## MERGE ALL
prosp_age_f <- 
  rbind(prosp_age_ita_f %>% mutate(country = "Italy"),
        prosp_age_fra_f %>% mutate(country = "France"),
        prosp_age_dnk_f %>% mutate(country = "Denmark"),
        prosp_age_swe_f %>% mutate(country = "Sweden"),
        prosp_age_uk_f %>% mutate(country = "UK"),
        prosp_age_usa_f %>% mutate(country = "USA")) %>% 
  filter(year < 2021) %>% 
  mutate(sex = "Female")

# Results prosp.age: plots ------------------------------------------------

prosp_age_m_f <- 
  rbind(prosp_age_m,
        prosp_age_f)

#chosing colors
show_col(viridis_pal(option="B")(30))
vB<-viridis(option="B", 30) #1:tot; 

#raw plot
prosp_age_m_f %>% 
  filter(year %in% c(seq(1950,2015,5),2019,2020)) %>% #manual smoothing
  ggplot(aes(x = year, y = prosp_age, color = country)) +
  geom_line(size = 1, alpha = .8) +
  geom_hline(yintercept = 65) +
  theme_minimal() +
  labs(x = "Year",
       y = "POAT",
#       title = "Prospective age computed for selected countries, both sexes. Years 1950-2020.\nReference: remaining life expectancy at age 65 for UK males in 1950",
#  caption = "Source: Authors' calculations on HMD data"
       color = "Country") +
  facet_wrap(~ fct_rev(sex)) +
  scale_color_manual(values = vB[c(1, 6, 11, 16, 21, 26)]) +
  scale_y_continuous(limits = c(63.5, 80))


#smoothed plot
prosp_age_m_f %>%
  ggplot(aes(x = year, y = prosp_age, color = country)) +
  #geom_point() +
  geom_line(stat = "smooth", size = 1, alpha = .8) +
  geom_hline(yintercept = 65) +
  theme_minimal(base_size = 12) +
  labs(x = "Year",
       y = "Prospective age",
#       title = "Prospective age computed for selected countries, both sexes. Years 1950-2020.\nReference: remaining life expectancy at age 65 for UK male in 1950",
#       caption = "Source: Authors' calculations on HMD data",
       color = "Country") +
  facet_wrap(~ fct_rev(sex)) +
  scale_color_viridis_d(option = "B", end = .8)+
  scale_y_continuous(limits = c(63.5, 80))

ggsave(filename = "Output/prosp_age_m_f_raw.pdf", width = 8, height = 4, dpi = 300)
write.csv(prosp_age_m_f, file = "Output/prosp_age_m_f.csv")

# Results: population aging -----------------------------------------------

## ITA
pop_aging_ita <- 
  pop_ita %>% 
  select(Year, Age, Female1, Male1, Total1, Country) %>% 
  pivot_longer(-c("Year", "Age", "Country"),
               names_to = "Sex",
               values_to = "Population") %>% 
  filter(Sex != "Total1") %>% 
  mutate(Sex = ifelse(Sex == "Female1", "Female", "Male")) %>% 
  left_join(prosp_age_m_f, by = c("Year" = "year", 
                                  "Sex" = "sex", 
                                  "Country" = "country")) %>% 
  group_by(Year) %>% 
  summarise(prospective_elderly_m = 
              (Population[Age == floor(prosp_age) &
                            Sex == "Male"]*
                 (abs(floor(unique(prosp_age[Sex == "Male"])) + 1 - 
                        unique(prosp_age[Sex == "Male"]))) +
                 sum(Population[Age>floor(prosp_age) &
                                  Sex == "Male"]))/
              sum(Population[Sex == "Male"]),
            elderly_m = sum(Population[Sex == "Male" & Age>=65])/
              sum(Population[Sex == "Male"]),
            prosp_age_m = unique(prosp_age[Sex == "Male"]),
            prospective_elderly_f = 
              (Population[Age == floor(prosp_age) &
                            Sex == "Female"]*
                 (abs(floor(unique(prosp_age[Sex == "Female"])) + 1 - 
                        unique(prosp_age[Sex == "Female"]))) +
                 sum(Population[Age>floor(prosp_age) &
                                  Sex == "Female"]))/
              sum(Population[Sex == "Female"]),
            elderly_f = sum(Population[Sex == "Female" & Age>=65])/
              sum(Population[Sex == "Female"]),
            prosp_age_f = unique(prosp_age[Sex == "Female"])

              ) %>% 
  mutate(Country = "Italy")

## FRA
pop_aging_fra <- 
  pop_fra %>% 
  select(Year, Age, Female1, Male1, Total1, Country) %>% 
  pivot_longer(-c("Year", "Age", "Country"),
               names_to = "Sex",
               values_to = "Population") %>% 
  filter(Sex != "Total1") %>% 
  mutate(Sex = ifelse(Sex == "Female1", "Female", "Male")) %>% 
  left_join(prosp_age_m_f, by = c("Year" = "year", 
                                  "Sex" = "sex", 
                                  "Country" = "country")) %>% 
  group_by(Year) %>% 
  summarise(prospective_elderly_m = 
              (Population[Age == floor(prosp_age) &
                            Sex == "Male"]*
                 (abs(floor(unique(prosp_age[Sex == "Male"])) + 1 - 
                        unique(prosp_age[Sex == "Male"]))) +
                 sum(Population[Age>floor(prosp_age) &
                                  Sex == "Male"]))/
              sum(Population[Sex == "Male"]),
            elderly_m = sum(Population[Sex == "Male" & Age>=65])/
              sum(Population[Sex == "Male"]),
            prosp_age_m = unique(prosp_age[Sex == "Male"]),
            prospective_elderly_f = 
              (Population[Age == floor(prosp_age) &
                            Sex == "Female"]*
                 (abs(floor(unique(prosp_age[Sex == "Female"])) + 1 - 
                        unique(prosp_age[Sex == "Female"]))) +
                 sum(Population[Age>floor(prosp_age) &
                                  Sex == "Female"]))/
              sum(Population[Sex == "Female"]),
            elderly_f = sum(Population[Sex == "Female" & Age>=65])/
              sum(Population[Sex == "Female"]),
            prosp_age_f = unique(prosp_age[Sex == "Female"])
  ) %>% 
  mutate(Country = "France")

## DNK
pop_aging_dnk <- 
  pop_dnk %>% 
  select(Year, Age, Female1, Male1, Total1, Country) %>% 
  pivot_longer(-c("Year", "Age", "Country"),
               names_to = "Sex",
               values_to = "Population") %>% 
  filter(Sex != "Total1") %>% 
  mutate(Sex = ifelse(Sex == "Female1", "Female", "Male")) %>% 
  left_join(prosp_age_m_f, by = c("Year" = "year", 
                                  "Sex" = "sex", 
                                  "Country" = "country")) %>% 
  group_by(Year) %>% 
  summarise(prospective_elderly_m = 
              (Population[Age == floor(prosp_age) &
                            Sex == "Male"]*
                 (abs(floor(unique(prosp_age[Sex == "Male"])) + 1 - 
                        unique(prosp_age[Sex == "Male"]))) +
                 sum(Population[Age>floor(prosp_age) &
                                  Sex == "Male"]))/
              sum(Population[Sex == "Male"]),
            elderly_m = sum(Population[Sex == "Male" & Age>=65])/
              sum(Population[Sex == "Male"]),
            prosp_age_m = unique(prosp_age[Sex == "Male"]),
            prospective_elderly_f = 
              (Population[Age == floor(prosp_age) &
                            Sex == "Female"]*
                 (abs(floor(unique(prosp_age[Sex == "Female"])) + 1 - 
                        unique(prosp_age[Sex == "Female"]))) +
                 sum(Population[Age>floor(prosp_age) &
                                  Sex == "Female"]))/
              sum(Population[Sex == "Female"]),
            elderly_f = sum(Population[Sex == "Female" & Age>=65])/
              sum(Population[Sex == "Female"]),
            prosp_age_f = unique(prosp_age[Sex == "Female"])
  ) %>% 
  mutate(Country = "Denmark")

## SWE
pop_aging_swe <- 
  pop_swe %>% 
  select(Year, Age, Female1, Male1, Total1, Country) %>% 
  pivot_longer(-c("Year", "Age", "Country"),
               names_to = "Sex",
               values_to = "Population") %>% 
  filter(Sex != "Total1") %>% 
  mutate(Sex = ifelse(Sex == "Female1", "Female", "Male")) %>% 
  left_join(prosp_age_m_f, by = c("Year" = "year", 
                                  "Sex" = "sex", 
                                  "Country" = "country")) %>% 
  group_by(Year) %>% 
  summarise(prospective_elderly_m = 
              (Population[Age == floor(prosp_age) &
                            Sex == "Male"]*
                 (abs(floor(unique(prosp_age[Sex == "Male"])) + 1 - 
                        unique(prosp_age[Sex == "Male"]))) +
                 sum(Population[Age>floor(prosp_age) &
                                  Sex == "Male"]))/
              sum(Population[Sex == "Male"]),
            elderly_m = sum(Population[Sex == "Male" & Age>=65])/
              sum(Population[Sex == "Male"]),
            prosp_age_m = unique(prosp_age[Sex == "Male"]),
            prospective_elderly_f = 
              (Population[Age == floor(prosp_age) &
                            Sex == "Female"]*
                 (abs(floor(unique(prosp_age[Sex == "Female"])) + 1 - 
                        unique(prosp_age[Sex == "Female"]))) +
                 sum(Population[Age>floor(prosp_age) &
                                  Sex == "Female"]))/
              sum(Population[Sex == "Female"]),
            elderly_f = sum(Population[Sex == "Female" & Age>=65])/
              sum(Population[Sex == "Female"]),
            prosp_age_f = unique(prosp_age[Sex == "Female"])
  ) %>% 
  mutate(Country = "Sweden")

## UK
pop_aging_uk <- 
  pop_uk %>% 
  select(Year, Age, Female1, Male1, Total1, Country) %>% 
  pivot_longer(-c("Year", "Age", "Country"),
               names_to = "Sex",
               values_to = "Population") %>% 
  filter(Sex != "Total1") %>% 
  mutate(Sex = ifelse(Sex == "Female1", "Female", "Male")) %>% 
  left_join(prosp_age_m_f, by = c("Year" = "year", 
                                  "Sex" = "sex", 
                                  "Country" = "country")) %>% 
  group_by(Year) %>% 
  summarise(prospective_elderly_m = 
              (Population[Age == floor(prosp_age) &
                            Sex == "Male"]*
                 (abs(floor(unique(prosp_age[Sex == "Male"])) + 1 - 
                        unique(prosp_age[Sex == "Male"]))) +
                 sum(Population[Age>floor(prosp_age) &
                                  Sex == "Male"]))/
              sum(Population[Sex == "Male"]),
            elderly_m = sum(Population[Sex == "Male" & Age>=65])/
              sum(Population[Sex == "Male"]),
            prosp_age_m = unique(prosp_age[Sex == "Male"]),
            prospective_elderly_f = 
              (Population[Age == floor(prosp_age) &
                            Sex == "Female"]*
                 (abs(floor(unique(prosp_age[Sex == "Female"])) + 1 - 
                        unique(prosp_age[Sex == "Female"]))) +
                 sum(Population[Age>floor(prosp_age) &
                                  Sex == "Female"]))/
              sum(Population[Sex == "Female"]),
            elderly_f = sum(Population[Sex == "Female" & Age>=65])/
              sum(Population[Sex == "Female"]),
            prosp_age_f = unique(prosp_age[Sex == "Female"])
  ) %>% 
  mutate(Country = "UK")

## USA
pop_aging_usa <- 
  pop_usa %>% 
  select(Year, Age, Female1, Male1, Total1, Country) %>% 
  pivot_longer(-c("Year", "Age", "Country"),
               names_to = "Sex",
               values_to = "Population") %>% 
  filter(Sex != "Total1") %>% 
  mutate(Sex = ifelse(Sex == "Female1", "Female", "Male")) %>% 
  left_join(prosp_age_m_f, by = c("Year" = "year", 
                                  "Sex" = "sex", 
                                  "Country" = "country")) %>% 
  group_by(Year) %>% 
  summarise(prospective_elderly_m = 
              (Population[Age == floor(prosp_age) &
                            Sex == "Male"]*
                 (abs(floor(unique(prosp_age[Sex == "Male"])) + 1 - 
                        unique(prosp_age[Sex == "Male"]))) +
                 sum(Population[Age>floor(prosp_age) &
                                  Sex == "Male"]))/
              sum(Population[Sex == "Male"]),
            elderly_m = sum(Population[Sex == "Male" & Age>=65])/
              sum(Population[Sex == "Male"]),
            prosp_age_m = unique(prosp_age[Sex == "Male"]),
            prospective_elderly_f = 
              (Population[Age == floor(prosp_age) &
                            Sex == "Female"]*
                 (abs(floor(unique(prosp_age[Sex == "Female"])) + 1 - 
                        unique(prosp_age[Sex == "Female"]))) +
                 sum(Population[Age>floor(prosp_age) &
                                  Sex == "Female"]))/
              sum(Population[Sex == "Female"]),
            elderly_f = sum(Population[Sex == "Female" & Age>=65])/
              sum(Population[Sex == "Female"]),
            prosp_age_f = unique(prosp_age[Sex == "Female"])
  ) %>% 
  mutate(Country = "USA")

pop_aging <- 
  rbind(pop_aging_ita,
        pop_aging_fra,
        pop_aging_dnk,
        pop_aging_swe,
        pop_aging_uk,
        pop_aging_usa)

pop_aging %>% 
  filter(Year %in% c(1950, 2019, 2020)) %>% 
  arrange(Country) %>% view()

# write.csv(x = pop_aging %>% filter(Year %in% c(1950, 2019, 2020)), file = "Output/Invecchiamento_internazionale_R_upd.csv")

# Results entropy: plots --------------------------------------------------

dati <- 
  rbind(ita_m %>% mutate(sex = "Male",
                         country = "Italy"), 
        ita_f %>% mutate(sex = "Female",
                         country = "Italy"),
        fra_m %>% mutate(sex = "Male",
                         country = "France"),
        fra_f %>% mutate(sex = "Female",
                         country = "France"),
        dnk_m %>% mutate(sex = "Male",
                         country = "Denmark"),
        dnk_f %>% mutate(sex = "Female",
                         country = "Denmark"),
        swe_m %>% mutate(sex = "Male",
                         country = "Sweden"),
        swe_f %>% mutate(sex = "Female",
                         country = "Sweden"),
        usa_m %>% mutate(sex = "Male",
                         country = "USA"),
        usa_f %>% mutate(sex = "Female",
                         country = "USA"),
        uk_m %>% mutate(sex = "Male",
                        country = "UK"),
        uk_f %>% mutate(sex = "Female",
                        country = "UK"))

#raw plot
dati %>% 
  filter(Year %in% c(seq(1950,2015,5),2019,2020)) %>% #manual smoothing
  filter(Age == 65,
         Year < 2021) %>% 
  ggplot(aes(x = Year, y = entropy*100, color = country)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(x = "Year",
       y = "Lifespan inequality (life table entropy in %)",
       # title = "Relative variation in length of life at age 65 computed for selected countries, both sexes.\nYears 1950-2020",
       # caption = "Source: Authors' calculations on HMD data",
       color = "Country") +
  facet_wrap(~ fct_rev(sex)) +
  scale_color_viridis_d(option = "B", end = .8)

#smoothed plot
dati %>% 
  filter(Age == 65,
         Year < 2021) %>% 
  ggplot(aes(x = Year, y = entropy, color = Country)) +
  geom_line(stat = "smooth", size = 1, alpha = 0.8) +
  theme_minimal() +
  labs(x = "Year",
       y = "Lifespan inequality (entropy)",
       title = "Relative variation in length of life at age 65 computed for selected countries, both sexes.\nYears 1950-2020",
       color = "Country",
       caption = "Source: Authors' calculations on HMD data") +
  facet_wrap(~ fct_rev(sex)) +
  scale_color_viridis_d(option = "B", end = .8)

ggsave(filename = "Output/entropy_m_f_raw.pdf", width = 8, height = 4, dpi = 300)
write.csv(dati, file = "Output/dati.csv")  

# Analysis: entropy vs prosp.age vs LE ------------------------------------

#entropy vs ex age = 65
cor_entropy_ex <- 
  rbind(
    cor.test(ita$entropy[ita$Age == 65], ita$ex[ita$Age == 65])[[4]],
    cor.test(ita_m$entropy[ita_m$Age == 65], ita_m$ex[ita_m$Age == 65])[[4]],
    cor.test(ita_f$entropy[ita_f$Age == 65], ita_f$ex[ita_f$Age == 65])[[4]],
    
    cor.test(fra$entropy[fra$Age == 65], fra$ex[fra$Age == 65])[[4]],
    cor.test(fra_m$entropy[fra_m$Age == 65], fra_m$ex[fra_m$Age == 65])[[4]],
    cor.test(fra_f$entropy[fra_f$Age == 65], fra_f$ex[fra_f$Age == 65])[[4]],
    
    cor.test(dnk$entropy[dnk$Age == 65], dnk$ex[dnk$Age == 65])[[4]],
    cor.test(dnk_m$entropy[dnk_m$Age == 65], dnk_m$ex[dnk_m$Age == 65])[[4]],
    cor.test(dnk_f$entropy[dnk_f$Age == 65], dnk_f$ex[dnk_f$Age == 65])[[4]],
    
    cor.test(swe$entropy[swe$Age == 65], swe$ex[swe$Age == 65])[[4]],
    cor.test(swe_m$entropy[swe_m$Age == 65], swe_m$ex[swe_m$Age == 65])[[4]],
    cor.test(swe_f$entropy[swe_f$Age == 65], swe_f$ex[swe_f$Age == 65])[[4]],
    
    cor.test(usa$entropy[usa$Age == 65], usa$ex[usa$Age == 65])[[4]],
    cor.test(usa_m$entropy[usa_m$Age == 65], usa_m$ex[usa_m$Age == 65])[[4]],
    cor.test(usa_f$entropy[usa_f$Age == 65], usa_f$ex[usa_f$Age == 65])[[4]],
    
    cor.test(uk$entropy[uk$Age == 65], uk$ex[uk$Age == 65])[[4]],
    cor.test(uk_m$entropy[uk_m$Age == 65], uk_m$ex[uk_m$Age == 65])[[4]],
    cor.test(uk_f$entropy[uk_f$Age == 65], uk_f$ex[uk_f$Age == 65])[[4]]
  )

country <- 
  c(rep("Italy",3),
    rep("France",3),
    rep("Denmark",3),
    rep("Sweden",3),
    rep("USA",3),
    rep("UK",3))

sex <- 
  c(rep(c("Total","Male","Female"),6))

cor_entropy_ex <- 
  cbind(cor_entropy_ex, country, sex)

#raw plot
dati %>% 
  filter(Year %in% c(seq(1950,2015,5),2019,2020)) %>% #manual smoothing
  filter(Age == 65,
         Year < 2021) %>% 
  ggplot(aes(x = Year, y = ex, color = country)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(x = "Year",
       y = "Life expectancy at age 65",
       # title = "Relative variation in length of life at age 65 computed for selected countries, both sexes.\nYears 1950-2020",
       # caption = "Source: Authors' calculations on HMD data",
       color = "Country") +
  facet_wrap(~ fct_rev(sex)) +
  scale_color_viridis_d(option = "B", end = .8)

ggsave(filename = "Output/LE_m_f_raw.pdf", width = 8, height = 4, dpi = 300)

dati %>% 
  filter(Age == 65,
         Year < 2021) %>% 
  ggplot(aes(x = ex, y = entropy, color = country)) +
  geom_point(size = 1, alpha = .8) +
  theme_minimal() +
  labs(x = "Life expectancy at age 65",
       y = "Lifespan inequality (life tablæe entropy)",
       # title = "Correlation between life expectancy and entropy at age 65 in selected countries, both sexes.\nYears 1950-2020",
       # caption = "Source: Authors' calculations on HMD data",
       color = "Country") +
  facet_wrap(~ fct_rev(sex)) +
  scale_color_viridis_d(option = "B", end = .8)

ggsave(filename = "Output/LEvsEntropy.pdf")

#entropy vs prosp.age
cor_entropy_prosp_age <- 
  rbind(
    cor.test(dnk_m$entropy[dnk_m$Age == 65], prosp_age_dnk_m$prosp_age)[[4]],
    cor.test(dnk_f$entropy[dnk_f$Age == 65], prosp_age_dnk_f$prosp_age)[[4]],
    
    cor.test(ita_m$entropy[ita_m$Age == 65], prosp_age_ita_m$prosp_age)[[4]],
    cor.test(ita_f$entropy[ita_f$Age == 65], prosp_age_ita_f$prosp_age)[[4]],
    
    cor.test(swe_m$entropy[swe_m$Age == 65], prosp_age_swe_m$prosp_age)[[4]],
    cor.test(swe_f$entropy[swe_f$Age == 65], prosp_age_swe_f$prosp_age)[[4]],
    
    cor.test(fra_m$entropy[fra_m$Age == 65], prosp_age_fra_m$prosp_age)[[4]],
    cor.test(fra_f$entropy[fra_f$Age == 65], prosp_age_fra_f$prosp_age)[[4]],
    
    cor.test(usa_m$entropy[usa_m$Age == 65], prosp_age_usa_m$prosp_age)[[4]],
    cor.test(usa_f$entropy[usa_f$Age == 65], prosp_age_usa_f$prosp_age)[[4]],
    
    cor.test(uk_m$entropy[uk_m$Age == 65], prosp_age_uk_m$prosp_age)[[4]],
    cor.test(uk_f$entropy[uk_f$Age == 65], prosp_age_uk_f$prosp_age)[[4]]
  )

country <- 
  c(rep("Italy",2),
    rep("France",2),
    rep("Denmark",2),
    rep("Sweden",2),
    rep("USA",2),
    rep("UK",2))

sex <- 
  c(rep(c("Male","Female"),6))

cor_entropy_prosp_age <- 
  cbind(cor_entropy_prosp_age, country, sex)

left_join(prosp_age_m_f,
          dati %>% 
            filter(Age == 65) %>% 
            select(Year, country, sex, entropy) %>% 
            rename(year = Year),
          by = c("year", "country", "sex")) %>% 
  ggplot(aes(x = prosp_age, y = entropy*100, color = fct_rev(sex))) +
  geom_point(size = 1, alpha = .8) +
  theme_minimal() +
  labs(x = "POAT",
       y = "Lifespan inequality (life table entropy in %)",
       # title = "Correlation between entropy at age 65 and prospective age in selected countries, both sexes.\nYears 1950-2020. Reference: remaining life expectancy at age 65 for UK males in 1950",
       # caption = "Source: Authors' calculations on HMD data",
       color = "Sex") +
  facet_wrap(~ country) +
  scale_color_manual(values = c("#0d47a1","#e65100"))

ggsave(filename = "Output/EntropyvsProsp_Age.pdf", width = 8, height = 4, dpi = 300)

#ex vs prosp.age
cor_ex_prosp_age <- 
  rbind(
    cor.test(dnk_m$ex[dnk_m$Age == 65], prosp_age_dnk_m$prosp_age)[[4]],
    cor.test(dnk_f$ex[dnk_f$Age == 65], prosp_age_dnk_f$prosp_age)[[4]],
    
    cor.test(ita_m$ex[ita_m$Age == 65], prosp_age_ita_m$prosp_age)[[4]],
    cor.test(ita_f$ex[ita_f$Age == 65], prosp_age_ita_f$prosp_age)[[4]],
    
    cor.test(swe_m$ex[swe_m$Age == 65], prosp_age_swe_m$prosp_age)[[4]],
    cor.test(swe_f$ex[swe_f$Age == 65], prosp_age_swe_f$prosp_age)[[4]],
    
    cor.test(fra_m$ex[fra_m$Age == 65], prosp_age_fra_m$prosp_age)[[4]],
    cor.test(fra_f$ex[fra_f$Age == 65], prosp_age_fra_f$prosp_age)[[4]],
    
    cor.test(usa_m$ex[usa_m$Age == 65], prosp_age_usa_m$prosp_age)[[4]],
    cor.test(usa_f$ex[usa_f$Age == 65], prosp_age_usa_f$prosp_age)[[4]],
    
    cor.test(uk_m$ex[uk_m$Age == 65], prosp_age_uk_m$prosp_age)[[4]],
    cor.test(uk_f$ex[uk_f$Age == 65], prosp_age_uk_f$prosp_age)[[4]]
  )

cor_ex_prosp_age <- 
  cbind(cor_ex_prosp_age, country, sex)    

left_join(prosp_age_m_f,
          dati %>% 
            filter(Age == 65) %>% 
            select(Year, country, sex, ex) %>% 
            rename(year = Year),
          by = c("year", "country", "sex")) %>% 
  ggplot(aes(x = ex, y = prosp_age, color = country)) +
  geom_point(size = 1, alpha = .8) +
  theme_minimal() +
  labs(x = "Life expectancy",
       y = "Prospective age",
       title = "Correlation between life expectancy at age 65 and prospective age in selected countries, both sexes.\nYears 1950-2020. Reference: remaining life expectancy at age 65 for UK males in 1950",
       color = "Country",
       caption = "Source: Authors' calculations on HMD data") +
  facet_wrap(~ sex) +
  scale_color_viridis_d(option = "B", end = .8)

ggsave(filename = "Output/LEvsProsp_Age.pdf")

#Export correlations
write.csv(cor_entropy_ex, file = "Output/cor_entropy_ex.csv")
write.csv(cor_entropy_prosp_age, file = "Output/cor_entropy_prosp_age.csv")
write.csv(cor_ex_prosp_age, file = "Output/cor_ex_prosp_age.csv")


# Regression analysis -----------------------------------------------------

dati_reg_m <- 
  prosp_age_m %>%
  left_join(dati %>% 
              filter(Age == 65,
                     sex == "Males") %>% 
              rename(year = Year) %>% 
              select(country, year, sex, ex, e65_sd, e65_cv, e_dag2, entropy), by = c("year", "sex", "country"))

dati_reg_f <- 
  prosp_age_f %>%
  left_join(dati %>% 
              filter(Age == 65,
                     sex == "Females") %>% 
              rename(year = Year) %>% 
              select(country, year, sex, ex, e65_sd, e65_cv, e_dag2, entropy), by = c("year", "sex", "country"))

reg_m <- 
  lm(formula = prosp_age ~ ex + entropy,
     data = dati_reg_m)
summary(reg_m)

reg_m_int <- 
  lm(formula = prosp_age ~ ex + entropy + ex*entropy,
     data = dati_reg_m)
summary(reg_m_int)

anova(reg_m, reg_m_int)

reg_m_dnk <- 
  lm(formula = prosp_age ~ ex + entropy,
     data = dati_reg_m %>% filter(country == "Denmark"))
summary(reg_m_dnk)

reg_m_dnk_int <- 
  lm(formula = prosp_age ~ ex + entropy + ex*entropy,
     data = dati_reg_m %>% filter(country == "Denmark"))
summary(reg_m_dnk_int)

anova(reg_m_dnk, reg_m_dnk_int)

reg_m_ita <- 
  lm(formula = prosp_age ~ ex + entropy,
     data = dati_reg_m %>% filter(country == "Italy"))
summary(reg_m_ita)

reg_m_ita_int <- 
  lm(formula = prosp_age ~ ex + entropy + ex*entropy,
     data = dati_reg_m %>% filter(country == "Italy"))
summary(reg_m_ita_int)

anova(reg_m_ita, reg_m_ita_int)

reg_m_usa <- 
  lm(formula = prosp_age ~ ex + entropy,
     data = dati_reg_m %>% filter(country == "USA"))
summary(reg_m_usa)

reg_m_usa_int <- 
  lm(formula = prosp_age ~ ex + entropy + ex*entropy,
     data = dati_reg_m %>% filter(country == "USA"))
summary(reg_m_usa_int)

anova(reg_m_usa, reg_m_usa_int)

reg_f <- 
  lm(formula = prosp_age ~ ex + e_dag2 + ex*e_dag2,
     data = dati_reg_f)
summary(reg_f)

# -------------------------------------------------------------------------
### EXTRA ###
# -------------------------------------------------------------------------
# Results e-dag: plots ----------------------------------------------------

dati <- 
  rbind(ita_m %>% mutate(sex = "Male",
                         country = "Italy"), 
        ita_f %>% mutate(sex = "Female",
                         country = "Italy"),
        fra_m %>% mutate(sex = "Male",
                         country = "France"),
        fra_f %>% mutate(sex = "Female",
                         country = "France"),
        dnk_m %>% mutate(sex = "Male",
                         country = "Denmark"),
        dnk_f %>% mutate(sex = "Female",
                         country = "Denmark"),
        swe_m %>% mutate(sex = "Male",
                         country = "Sweden"),
        swe_f %>% mutate(sex = "Female",
                         country = "Sweden"),
        usa_m %>% mutate(sex = "Male",
                         country = "USA"),
        usa_f %>% mutate(sex = "Female",
                         country = "USA"),
        uk_m %>% mutate(sex = "Male",
                        country = "UK"),
        uk_f %>% mutate(sex = "Female",
                        country = "UK"))

#raw plot
dati %>% 
  filter(Age == 65,
         Year < 2021) %>% 
  ggplot(aes(x = Year, y = e_dag2, color = Country)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(x = "Year",
       y = "Lifespan inequality (e-dagger)",
       title = "Average number of years of life lost due to death at age 65 computed for selected countries,\n both sexes. Years 1950-2020",
       color = "Country",
       caption = "Source: Authors' calculations on HMD data") +
  facet_wrap(~ sex)

#smoothed plot
dati %>% 
  filter(Age == 65,
         Year < 2021) %>% 
  ggplot(aes(x = Year, y = e_dag2, color = Country)) +
  geom_line(stat = "smooth", size = 1, alpha = 0.8) +
  theme_minimal() +
  labs(x = "Year",
       y = "Lifespan inequality (e-dagger)",
       title = "Average number of years of life lost due to death at age 65 computed for selected countries,\nboth sexes. Years 1950-2020",
       color = "Country",
       caption = "Source: Authors' calculations on HMD data") +
  facet_wrap(~ sex) +
  scale_color_viridis_d(option = "B", end = .8)

ggsave(filename = "Output/e-dag_m_f_smooth.pdf") 

# Results sd_e65: plots --------------------------------------------------

dati <- 
  rbind(ita_m %>% mutate(sex = "Males",
                         country = "Italy"), 
        ita_f %>% mutate(sex = "Females",
                         country = "Italy"),
        fra_m %>% mutate(sex = "Males",
                         country = "France"),
        fra_f %>% mutate(sex = "Females",
                         country = "France"),
        dnk_m %>% mutate(sex = "Males",
                         country = "Denmark"),
        dnk_f %>% mutate(sex = "Females",
                         country = "Denmark"),
        swe_m %>% mutate(sex = "Males",
                         country = "Sweden"),
        swe_f %>% mutate(sex = "Females",
                         country = "Sweden"),
        usa_m %>% mutate(sex = "Males",
                         country = "USA"),
        usa_f %>% mutate(sex = "Females",
                         country = "USA"),
        uk_m %>% mutate(sex = "Males",
                        country = "UK"),
        uk_f %>% mutate(sex = "Females",
                        country = "UK"))

#raw plot
dati %>% 
  filter(Age == 65,
         Year < 2021) %>% 
  ggplot(aes(x = Year, y = e65_sd, color = country)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(x = "Year",
       y = "Lifespan inequality (standard deviation)",
       title = "Lifespan inequality at age 65 computed for selected countries,\n both sexes. Years 1950-2020",
       color = "Country",
       caption = "Source: Authors' calculations on HMD data") +
  facet_wrap(~ sex)

#smoothed plot
dati %>% 
  filter(Age == 65,
         Year < 2021) %>% 
  ggplot(aes(x = Year, y = e65_sd, color = country)) +
  geom_line(stat = "smooth", size = 1, alpha = 0.8) +
  theme_minimal() +
  labs(x = "Year",
       y = "Lifespan inequality (standard deviation)",
       title = "Lifespan inequality at age 65 computed for selected countries,\nboth sexes. Years 1950-2020",
       color = "Country",
       caption = "Source: Authors' calculations on HMD data") +
  facet_wrap(~ sex) +
  scale_color_viridis_d(option = "B", end = .8)

ggsave(filename = "Output/e65_sd_m_f_smooth.pdf")
write.csv(dati, file = "dati.csv")

# Results cv_e65: plots --------------------------------------------------

dati <- 
  rbind(ita_m %>% mutate(sex = "Males",
                         country = "Italy"), 
        ita_f %>% mutate(sex = "Females",
                         country = "Italy"),
        fra_m %>% mutate(sex = "Males",
                         country = "France"),
        fra_f %>% mutate(sex = "Females",
                         country = "France"),
        dnk_m %>% mutate(sex = "Males",
                         country = "Denmark"),
        dnk_f %>% mutate(sex = "Females",
                         country = "Denmark"),
        swe_m %>% mutate(sex = "Males",
                         country = "Sweden"),
        swe_f %>% mutate(sex = "Females",
                         country = "Sweden"),
        usa_m %>% mutate(sex = "Males",
                         country = "USA"),
        usa_f %>% mutate(sex = "Females",
                         country = "USA"),
        uk_m %>% mutate(sex = "Males",
                        country = "UK"),
        uk_f %>% mutate(sex = "Females",
                        country = "UK"))

#raw plot
dati %>% 
  filter(Age == 65,
         Year < 2021) %>% 
  ggplot(aes(x = Year, y = e65_cv, color = country)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(x = "Year",
       y = "Lifespan inequality (coefficent of variation)",
       title = "Lifespan inequality at age 65 computed for selected countries,\n both sexes. Years 1950-2020",
       color = "Country",
       caption = "Source: Authors' calculations on HMD data") +
  facet_wrap(~ sex)

#smoothed plot
dati %>% 
  filter(Age == 65,
         Year < 2021) %>% 
  ggplot(aes(x = Year, y = e65_cv, color = country)) +
  geom_line(stat = "smooth", size = 1, alpha = 0.8) +
  theme_minimal() +
  labs(x = "Year",
       y = "Lifespan inequality (coefficent of variation)",
       title = "Lifespan inequality at age 65 computed for selected countries,\nboth sexes. Years 1950-2020",
       color = "Country",
       caption = "Source: Authors' calculations on HMD data") +
  facet_wrap(~ sex) +
  scale_color_viridis_d(option = "B", end = .8)

ggsave(filename = "Output/e65_cv_m_f_smooth.pdf")
write.csv(dati, file = "dati.csv")

# Analysis: modal age at death male ---------------------------------------

#new values of age for smoothing 
newAge <- seq(0, 110, by = 0.001)

## ITA
#df to fill with modal age at death
mode_ita_m <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function prosp.age
for(i in min(ita_m$Year):max(ita_m$Year)){
  
  df <- ita_m %>% filter(Year == i, Age > 0)
  age <- mode.spline(df$Age, newAge, df$dx)
  mode_ita_m <- 
    rbind(mode_ita_m,
          t(c(i,age)))
}

#rename columns
colnames(mode_ita_m) <- c("year", "mode_spline")

## FRA
#df to fill with modal age at death
mode_fra_m <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function prosp.age
for(i in min(fra_m$Year):max(fra_m$Year)){
  
  df <- fra_m %>% filter(Year == i, Age > 0)
  age <- mode.spline(df$Age, newAge, df$dx)
  mode_fra_m <- 
    rbind(mode_fra_m,
          t(c(i,age)))
}

#rename columns
colnames(mode_fra_m) <- c("year", "mode_spline")

## DNK
#df to fill with modal age at death
mode_dnk_m <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function prosp.age
for(i in min(dnk_m$Year):max(dnk_m$Year)){
  
  df <- dnk_m %>% filter(Year == i, Age > 0)
  age <- mode.spline(df$Age, newAge, df$dx)
  mode_dnk_m <- 
    rbind(mode_dnk_m,
          t(c(i,age)))
}

#rename columns
colnames(mode_dnk_m) <- c("year", "mode_spline")

## SWE
#df to fill with modal age at death
mode_swe_m <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function prosp.age
for(i in min(swe_m$Year):max(swe_m$Year)){
  
  df <- swe_m %>% filter(Year == i, Age > 0)
  age <- mode.spline(df$Age, newAge, df$dx)
  mode_swe_m <- 
    rbind(mode_swe_m,
          t(c(i,age)))
}

#rename columns
colnames(mode_swe_m) <- c("year", "mode_spline")

## USA
#df to fill with modal age at death
mode_usa_m <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function prosp.age
for(i in min(usa_m$Year):max(usa_m$Year)){
  
  df <- usa_m %>% filter(Year == i, Age > 0)
  age <- mode.spline(df$Age, newAge, df$dx)
  mode_usa_m <- 
    rbind(mode_usa_m,
          t(c(i,age)))
}

#rename columns
colnames(mode_usa_m) <- c("year", "mode_spline")

## UK
#df to fill with modal age at death
mode_uk_m <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function prosp.age
for(i in min(uk_m$Year):max(uk_m$Year)){
  
  df <- uk_m %>% filter(Year == i, Age > 0)
  age <- mode.spline(df$Age, newAge, df$dx)
  mode_uk_m <- 
    rbind(mode_uk_m,
          t(c(i,age)))
}

#rename columns
colnames(mode_uk_m) <- c("year", "mode_spline")

## MERGE ALL
mode_m <- 
  rbind(mode_ita_m %>% mutate(country = "Italy"),
        mode_fra_m %>% mutate(country = "France"),
        mode_dnk_m %>% mutate(country = "Denmark"),
        mode_swe_m %>% mutate(country = "Sweden"),
        mode_uk_m %>% mutate(country = "UK"),
        mode_usa_m %>% mutate(country = "USA")) %>% 
  filter(year < 2021) %>% 
  mutate(sex = "Males")

# Analysis: modal age at death female -------------------------------------

## ITA
#df to fill with modal age at death
mode_ita_f <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function prosp.age
for(i in min(ita_f$Year):max(ita_f$Year)){
  
  df <- ita_f %>% filter(Year == i, Age > 0)
  age <- mode.spline(df$Age, newAge, df$dx)
  mode_ita_f <- 
    rbind(mode_ita_f,
          t(c(i,age)))
}

#rename columns
colnames(mode_ita_f) <- c("year", "mode_spline")

## FRA
#df to fill with modal age at death
mode_fra_f <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function prosp.age
for(i in min(fra_f$Year):max(fra_f$Year)){
  
  df <- fra_f %>% filter(Year == i, Age > 0)
  age <- mode.spline(df$Age, newAge, df$dx)
  mode_fra_f <- 
    rbind(mode_fra_f,
          t(c(i,age)))
}

#rename columns
colnames(mode_fra_f) <- c("year", "mode_spline")

## DNK
#df to fill with modal age at death
mode_dnk_f <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function prosp.age
for(i in min(dnk_f$Year):max(dnk_f$Year)){
  
  df <- dnk_f %>% filter(Year == i, Age > 0)
  age <- mode.spline(df$Age, newAge, df$dx)
  mode_dnk_f <- 
    rbind(mode_dnk_f,
          t(c(i,age)))
}

#rename columns
colnames(mode_dnk_f) <- c("year", "mode_spline")

## SWE
#df to fill with modal age at death
mode_swe_f <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function prosp.age
for(i in min(swe_f$Year):max(swe_f$Year)){
  
  df <- swe_f %>% filter(Year == i, Age > 0)
  age <- mode.spline(df$Age, newAge, df$dx)
  mode_swe_f <- 
    rbind(mode_swe_f,
          t(c(i,age)))
}

#rename columns
colnames(mode_swe_f) <- c("year", "mode_spline")

## USA
#df to fill with modal age at death
mode_usa_f <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function prosp.age
for(i in min(usa_f$Year):max(usa_f$Year)){
  
  df <- usa_f %>% filter(Year == i, Age > 0)
  age <- mode.spline(df$Age, newAge, df$dx)
  mode_usa_f <- 
    rbind(mode_usa_f,
          t(c(i,age)))
}

#rename columns
colnames(mode_usa_f) <- c("year", "mode_spline")

## UK
#df to fill with modal age at death
mode_uk_f <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function prosp.age
for(i in min(uk_f$Year):max(uk_f$Year)){
  
  df <- uk_f %>% filter(Year == i, Age > 0)
  age <- mode.spline(df$Age, newAge, df$dx)
  mode_uk_f <- 
    rbind(mode_uk_f,
          t(c(i,age)))
}

#rename columns
colnames(mode_uk_f) <- c("year", "mode_spline")

## MERGE ALL
mode_f <- 
  rbind(mode_ita_f %>% mutate(country = "Italy"),
        mode_fra_f %>% mutate(country = "France"),
        mode_dnk_f %>% mutate(country = "Denmark"),
        mode_swe_f %>% mutate(country = "Sweden"),
        mode_uk_f %>% mutate(country = "UK"),
        mode_usa_f %>% mutate(country = "USA")) %>% 
  filter(year < 2021) %>% 
  mutate(sex = "Females")

# Results mode.spline -----------------------------------------------------

mode_m_f <- 
  rbind(mode_m,
        mode_f)

#raw plot
mode_m_f %>% 
  ggplot(aes(x = year, y = mode_spline, color = country)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(x = "Year",
       y = "Modal age at death",
       title = "Modal age at death computed for selected countries, both sexes. Years 1950-2020",
       color = "Country",
       caption = "Source: Authors' calculations on HMD data") +
  facet_wrap(~ sex)

#smoothed plot
mode_m_f %>% 
  ggplot(aes(x = year, y = mode_spline, color = country)) +
  geom_line(stat = "smooth", size = 1, alpha = .8) +
  theme_minimal() +
  labs(x = "Year",
       y = "Modal age at death",
       title = "Modal age at death computed for selected countries, both sexes. Years 1950-2020",
       color = "Country",
       caption = "Source: Authors' calculations on HMD data") +
  facet_wrap(~ sex) +
  scale_color_viridis_d(option = "B", end = .8)

ggsave(filename = "Output/mode_m_f_smooth.pdf")
write.csv(mode_m_f, file = "Output/mode_m_f.csv")

# Analysis: e-dag vs prosp.age vs LE --------------------------------------

#e-dag vs ex age = 65
cor_e_dag_ex <- 
  rbind(
    cor.test(ita$e_dag2[ita$Age == 65], ita$ex[ita$Age == 65])[[4]],
    cor.test(ita_m$e_dag2[ita_m$Age == 65], ita_m$ex[ita_m$Age == 65])[[4]],
    cor.test(ita_f$e_dag2[ita_f$Age == 65], ita_f$ex[ita_f$Age == 65])[[4]],
    
    cor.test(fra$e_dag2[fra$Age == 65], fra$ex[fra$Age == 65])[[4]],
    cor.test(fra_m$e_dag2[fra_m$Age == 65], fra_m$ex[fra_m$Age == 65])[[4]],
    cor.test(fra_f$e_dag2[fra_f$Age == 65], fra_f$ex[fra_f$Age == 65])[[4]],
    
    cor.test(dnk$e_dag2[dnk$Age == 65], dnk$ex[dnk$Age == 65])[[4]],
    cor.test(dnk_m$e_dag2[dnk_m$Age == 65], dnk_m$ex[dnk_m$Age == 65])[[4]],
    cor.test(dnk_f$e_dag2[dnk_f$Age == 65], dnk_f$ex[dnk_f$Age == 65])[[4]],
    
    cor.test(swe$e_dag2[swe$Age == 65], swe$ex[swe$Age == 65])[[4]],
    cor.test(swe_m$e_dag2[swe_m$Age == 65], swe_m$ex[swe_m$Age == 65])[[4]],
    cor.test(swe_f$e_dag2[swe_f$Age == 65], swe_f$ex[swe_f$Age == 65])[[4]],
    
    cor.test(usa$e_dag2[usa$Age == 65], usa$ex[usa$Age == 65])[[4]],
    cor.test(usa_m$e_dag2[usa_m$Age == 65], usa_m$ex[usa_m$Age == 65])[[4]],
    cor.test(usa_f$e_dag2[usa_f$Age == 65], usa_f$ex[usa_f$Age == 65])[[4]],
    
    cor.test(uk$e_dag2[uk$Age == 65], uk$ex[uk$Age == 65])[[4]],
    cor.test(uk_m$e_dag2[uk_m$Age == 65], uk_m$ex[uk_m$Age == 65])[[4]],
    cor.test(uk_f$e_dag2[uk_f$Age == 65], uk_f$ex[uk_f$Age == 65])[[4]]
  )

country <- 
  c(rep("Italy",3),
    rep("France",3),
    rep("Denmark",3),
    rep("Sweden",3),
    rep("USA",3),
    rep("UK",3))

sex <- 
  c(rep(c("Total","Male","Female"),6))

cor_e_dag_ex <- 
  cbind(cor_e_dag_ex, country, sex)
    
dati %>% 
  filter(Age == 65,
         Year < 2021) %>% 
  ggplot(aes(x = ex, y = e_dag2, color = country)) +
  geom_point(size = 1, alpha = .8) +
  theme_minimal() +
  labs(x = "Life expectancy",
       y = "Lifespan inequality (e-dagger)",
       title = "Correlation between life expectancy and lifespan inequality at age 65 in selected countries, both sexes.\nYears 1950-2020",
       color = "Country",
       caption = "Source: Authors' calculations on HMD data") +
  facet_wrap(~ sex) +
  scale_color_viridis_d(option = "B", end = .8)

ggsave(filename = "Output/LEvsLI.pdf")
write.csv(dati, file = "Output/dati.csv")

#e-dag vs prosp.age
cor_e_dag_prosp_age <- 
  rbind(
    cor.test(dnk_m$e_dag2[dnk_m$Age == 65], prosp_age_dnk_m$prosp_age)[[4]],
    cor.test(dnk_f$e_dag2[dnk_f$Age == 65], prosp_age_dnk_f$prosp_age)[[4]],
    
    cor.test(ita_m$e_dag2[ita_m$Age == 65], prosp_age_ita_m$prosp_age)[[4]],
    cor.test(ita_f$e_dag2[ita_f$Age == 65], prosp_age_ita_f$prosp_age)[[4]],
    
    cor.test(swe_m$e_dag2[swe_m$Age == 65], prosp_age_swe_m$prosp_age)[[4]],
    cor.test(swe_f$e_dag2[swe_f$Age == 65], prosp_age_swe_f$prosp_age)[[4]],
    
    cor.test(fra_m$e_dag2[fra_m$Age == 65], prosp_age_fra_m$prosp_age)[[4]],
    cor.test(fra_f$e_dag2[fra_f$Age == 65], prosp_age_fra_f$prosp_age)[[4]],
    
    cor.test(usa_m$e_dag2[usa_m$Age == 65], prosp_age_usa_m$prosp_age)[[4]],
    cor.test(usa_f$e_dag2[usa_f$Age == 65], prosp_age_usa_f$prosp_age)[[4]],
    
    cor.test(uk_m$e_dag2[uk_m$Age == 65], prosp_age_uk_m$prosp_age)[[4]],
    cor.test(uk_f$e_dag2[uk_f$Age == 65], prosp_age_uk_f$prosp_age)[[4]]
  )

country <- 
  c(rep("Italy",2),
    rep("France",2),
    rep("Denmark",2),
    rep("Sweden",2),
    rep("USA",2),
    rep("UK",2))

sex <- 
  c(rep(c("Male","Female"),6))
  
cor_e_dag_prosp_age <- 
  cbind(cor_e_dag_prosp_age, country, sex)
  
left_join(prosp_age_m_f,
          dati %>% 
            filter(Age == 65) %>% 
            select(Year, country, sex, e_dag2) %>% 
            rename(year = Year),
          by = c("year", "country", "sex")) %>% 
ggplot(aes(x = e_dag2, y = prosp_age, color = country)) +
  geom_point(size = 1, alpha = .8) +
  theme_minimal() +
  labs(x = "Lifespan inequality (e-dagger)",
       y = "Prospective age",
       title = "Correlation between lifespan inequality at age 65 and prospective age in selected countries, both sexes.\nYears 1950-2020. Reference: remaining life expectancy at age 65 for UK males in 1950",
       color = "Country",
       caption = "Source: Authors' calculations on HMD data") +
  facet_wrap(~ sex) +
  scale_color_viridis_d(option = "B", end = .8)

ggsave(filename = "Output/LIvsProsp_Age.pdf")

#ex vs prosp.age
cor_ex_prosp_age <- 
  rbind(
    cor.test(dnk_m$ex[dnk_m$Age == 65], prosp_age_dnk_m$prosp_age)[[4]],
    cor.test(dnk_f$ex[dnk_f$Age == 65], prosp_age_dnk_f$prosp_age)[[4]],
    
    cor.test(ita_m$ex[ita_m$Age == 65], prosp_age_ita_m$prosp_age)[[4]],
    cor.test(ita_f$ex[ita_f$Age == 65], prosp_age_ita_f$prosp_age)[[4]],
    
    cor.test(swe_m$ex[swe_m$Age == 65], prosp_age_swe_m$prosp_age)[[4]],
    cor.test(swe_f$ex[swe_f$Age == 65], prosp_age_swe_f$prosp_age)[[4]],
    
    cor.test(fra_m$ex[fra_m$Age == 65], prosp_age_fra_m$prosp_age)[[4]],
    cor.test(fra_f$ex[fra_f$Age == 65], prosp_age_fra_f$prosp_age)[[4]],
    
    cor.test(usa_m$ex[usa_m$Age == 65], prosp_age_usa_m$prosp_age)[[4]],
    cor.test(usa_f$ex[usa_f$Age == 65], prosp_age_usa_f$prosp_age)[[4]],
    
    cor.test(uk_m$ex[uk_m$Age == 65], prosp_age_uk_m$prosp_age)[[4]],
    cor.test(uk_f$ex[uk_f$Age == 65], prosp_age_uk_f$prosp_age)[[4]]
  )

cor_ex_prosp_age <- 
  cbind(cor_ex_prosp_age, country, sex)    
    
left_join(prosp_age_m_f,
          dati %>% 
            filter(Age == 65) %>% 
            select(Year, country, sex, ex) %>% 
            rename(year = Year),
          by = c("year", "country", "sex")) %>% 
  ggplot(aes(x = ex, y = prosp_age, color = country)) +
  geom_point(size = 1, alpha = .8) +
  theme_minimal() +
  labs(x = "Life expectancy",
       y = "Prospective age",
       title = "Correlation between life expectancy at age 65 and prospective age in selected countries, both sexes.\nYears 1950-2020. Reference: remaining life expectancy at age 65 for UK males in 1950",
       color = "Country",
       caption = "Source: Authors' calculations on HMD data") +
  facet_wrap(~ sex) +
  scale_color_viridis_d(option = "B", end = .8)

ggsave(filename = "Output/LEvsProsp_Age.pdf")


# Analysis: e65_sd vs prosp.age vs LE -----------------------------------

#e-dag vs ex age = 65
cor_e65_sd_ex <- 
  rbind(
    cor.test(ita$e65_sd[ita$Age == 65], ita$ex[ita$Age == 65])[[4]],
    cor.test(ita_m$e65_sd[ita_m$Age == 65], ita_m$ex[ita_m$Age == 65])[[4]],
    cor.test(ita_f$e65_sd[ita_f$Age == 65], ita_f$ex[ita_f$Age == 65])[[4]],
    
    cor.test(fra$e65_sd[fra$Age == 65], fra$ex[fra$Age == 65])[[4]],
    cor.test(fra_m$e65_sd[fra_m$Age == 65], fra_m$ex[fra_m$Age == 65])[[4]],
    cor.test(fra_f$e65_sd[fra_f$Age == 65], fra_f$ex[fra_f$Age == 65])[[4]],
    
    cor.test(dnk$e65_sd[dnk$Age == 65], dnk$ex[dnk$Age == 65])[[4]],
    cor.test(dnk_m$e65_sd[dnk_m$Age == 65], dnk_m$ex[dnk_m$Age == 65])[[4]],
    cor.test(dnk_f$e65_sd[dnk_f$Age == 65], dnk_f$ex[dnk_f$Age == 65])[[4]],
    
    cor.test(swe$e65_sd[swe$Age == 65], swe$ex[swe$Age == 65])[[4]],
    cor.test(swe_m$e65_sd[swe_m$Age == 65], swe_m$ex[swe_m$Age == 65])[[4]],
    cor.test(swe_f$e65_sd[swe_f$Age == 65], swe_f$ex[swe_f$Age == 65])[[4]],
    
    cor.test(usa$e65_sd[usa$Age == 65], usa$ex[usa$Age == 65])[[4]],
    cor.test(usa_m$e65_sd[usa_m$Age == 65], usa_m$ex[usa_m$Age == 65])[[4]],
    cor.test(usa_f$e65_sd[usa_f$Age == 65], usa_f$ex[usa_f$Age == 65])[[4]],
    
    cor.test(uk$e65_sd[uk$Age == 65], uk$ex[uk$Age == 65])[[4]],
    cor.test(uk_m$e65_sd[uk_m$Age == 65], uk_m$ex[uk_m$Age == 65])[[4]],
    cor.test(uk_f$e65_sd[uk_f$Age == 65], uk_f$ex[uk_f$Age == 65])[[4]]
  )

country <- 
  c(rep("Italy",3),
    rep("France",3),
    rep("Denmark",3),
    rep("Sweden",3),
    rep("USA",3),
    rep("UK",3))

sex <- 
  c(rep(c("Total","Male","Female"),6))

cor_e65_sd_ex <- 
  cbind(cor_e65_sd_ex, country, sex)

dati %>% 
  filter(Age == 65,
         Year < 2021) %>% 
  ggplot(aes(x = ex, y = e65_sd, color = country)) +
  geom_point(size = 1, alpha = .8) +
  theme_minimal() +
  labs(x = "Life expectancy",
       y = "Lifespan inequality (standard deviation)",
       title = "Correlation between life expectancy and lifespan inequality at age 65 in selected countries, both sexes.\nYears 1950-2020",
       color = "Country",
       caption = "Source: Authors' calculations on HMD data") +
  facet_wrap(~ sex) +
  scale_color_viridis_d(option = "B", end = .8)

ggsave(filename = "Output/LEvsSD.pdf")
write.csv(dati, file = "Output/dati.csv")

#e-dag vs prosp.age
cor_e65_sd_prosp_age <- 
  rbind(
    cor.test(dnk_m$e65_sd[dnk_m$Age == 65], prosp_age_dnk_m$prosp_age)[[4]],
    cor.test(dnk_f$e65_sd[dnk_f$Age == 65], prosp_age_dnk_f$prosp_age)[[4]],
    
    cor.test(ita_m$e65_sd[ita_m$Age == 65], prosp_age_ita_m$prosp_age)[[4]],
    cor.test(ita_f$e65_sd[ita_f$Age == 65], prosp_age_ita_f$prosp_age)[[4]],
    
    cor.test(swe_m$e65_sd[swe_m$Age == 65], prosp_age_swe_m$prosp_age)[[4]],
    cor.test(swe_f$e65_sd[swe_f$Age == 65], prosp_age_swe_f$prosp_age)[[4]],
    
    cor.test(fra_m$e65_sd[fra_m$Age == 65], prosp_age_fra_m$prosp_age)[[4]],
    cor.test(fra_f$e65_sd[fra_f$Age == 65], prosp_age_fra_f$prosp_age)[[4]],
    
    cor.test(usa_m$e65_sd[usa_m$Age == 65], prosp_age_usa_m$prosp_age)[[4]],
    cor.test(usa_f$e65_sd[usa_f$Age == 65], prosp_age_usa_f$prosp_age)[[4]],
    
    cor.test(uk_m$e65_sd[uk_m$Age == 65], prosp_age_uk_m$prosp_age)[[4]],
    cor.test(uk_f$e65_sd[uk_f$Age == 65], prosp_age_uk_f$prosp_age)[[4]]
  )

country <- 
  c(rep("Italy",2),
    rep("France",2),
    rep("Denmark",2),
    rep("Sweden",2),
    rep("USA",2),
    rep("UK",2))

sex <- 
  c(rep(c("Male","Female"),6))

cor_e65_sd_prosp_age <- 
  cbind(cor_e65_sd_prosp_age, country, sex)

left_join(prosp_age_m_f,
          dati %>% 
            filter(Age == 65) %>% 
            select(Year, country, sex, e65_sd) %>% 
            rename(year = Year),
          by = c("year", "country", "sex")) %>% 
  ggplot(aes(x = e65_sd, y = prosp_age, color = country)) +
  geom_point(size = 1, alpha = .8) +
  theme_minimal() +
  labs(x = "Lifespan inequality (standard deviation)",
       y = "Prospective age",
       title = "Correlation between lifespan inequality at age 65 and prospective age in selected countries, both sexes.\nYears 1950-2020. Reference: remaining life expectancy at age 65 for UK males in 1950",
       color = "Country",
       caption = "Source: Authors' calculations on HMD data") +
  facet_wrap(~ sex) +
  scale_color_viridis_d(option = "B", end = .8)

ggsave(filename = "Output/SDvsProsp_Age.pdf")

#ex vs prosp.age
cor_ex_prosp_age <- 
  rbind(
    cor.test(dnk_m$ex[dnk_m$Age == 65], prosp_age_dnk_m$prosp_age)[[4]],
    cor.test(dnk_f$ex[dnk_f$Age == 65], prosp_age_dnk_f$prosp_age)[[4]],
    
    cor.test(ita_m$ex[ita_m$Age == 65], prosp_age_ita_m$prosp_age)[[4]],
    cor.test(ita_f$ex[ita_f$Age == 65], prosp_age_ita_f$prosp_age)[[4]],
    
    cor.test(swe_m$ex[swe_m$Age == 65], prosp_age_swe_m$prosp_age)[[4]],
    cor.test(swe_f$ex[swe_f$Age == 65], prosp_age_swe_f$prosp_age)[[4]],
    
    cor.test(fra_m$ex[fra_m$Age == 65], prosp_age_fra_m$prosp_age)[[4]],
    cor.test(fra_f$ex[fra_f$Age == 65], prosp_age_fra_f$prosp_age)[[4]],
    
    cor.test(usa_m$ex[usa_m$Age == 65], prosp_age_usa_m$prosp_age)[[4]],
    cor.test(usa_f$ex[usa_f$Age == 65], prosp_age_usa_f$prosp_age)[[4]],
    
    cor.test(uk_m$ex[uk_m$Age == 65], prosp_age_uk_m$prosp_age)[[4]],
    cor.test(uk_f$ex[uk_f$Age == 65], prosp_age_uk_f$prosp_age)[[4]]
  )

cor_ex_prosp_age <- 
  cbind(cor_ex_prosp_age, country, sex)    

left_join(prosp_age_m_f,
          dati %>% 
            filter(Age == 65) %>% 
            select(Year, country, sex, ex) %>% 
            rename(year = Year),
          by = c("year", "country", "sex")) %>% 
  ggplot(aes(x = ex, y = prosp_age, color = country)) +
  geom_point(size = 1, alpha = .8) +
  theme_minimal() +
  labs(x = "Life expectancy",
       y = "Prospective age",
       title = "Correlation between life expectancy at age 65 and prospective age in selected countries, both sexes.\nYears 1950-2020. Reference: remaining life expectancy at age 65 for UK males in 1950",
       color = "Country",
       caption = "Source: Authors' calculations on HMD data") +
  facet_wrap(~ sex) +
  scale_color_viridis_d(option = "B", end = .8)

ggsave(filename = "Output/LEvsProsp_Age.pdf")

# Analysis: e65_cv vs prosp.age vs LE --------------------------------------

#e-dag vs ex age = 65
cor_e65_cv_ex <- 
  rbind(
    cor.test(ita$e65_cv[ita$Age == 65], ita$ex[ita$Age == 65])[[4]],
    cor.test(ita_m$e65_cv[ita_m$Age == 65], ita_m$ex[ita_m$Age == 65])[[4]],
    cor.test(ita_f$e65_cv[ita_f$Age == 65], ita_f$ex[ita_f$Age == 65])[[4]],
    
    cor.test(fra$e65_cv[fra$Age == 65], fra$ex[fra$Age == 65])[[4]],
    cor.test(fra_m$e65_cv[fra_m$Age == 65], fra_m$ex[fra_m$Age == 65])[[4]],
    cor.test(fra_f$e65_cv[fra_f$Age == 65], fra_f$ex[fra_f$Age == 65])[[4]],
    
    cor.test(dnk$e65_cv[dnk$Age == 65], dnk$ex[dnk$Age == 65])[[4]],
    cor.test(dnk_m$e65_cv[dnk_m$Age == 65], dnk_m$ex[dnk_m$Age == 65])[[4]],
    cor.test(dnk_f$e65_cv[dnk_f$Age == 65], dnk_f$ex[dnk_f$Age == 65])[[4]],
    
    cor.test(swe$e65_cv[swe$Age == 65], swe$ex[swe$Age == 65])[[4]],
    cor.test(swe_m$e65_cv[swe_m$Age == 65], swe_m$ex[swe_m$Age == 65])[[4]],
    cor.test(swe_f$e65_cv[swe_f$Age == 65], swe_f$ex[swe_f$Age == 65])[[4]],
    
    cor.test(usa$e65_cv[usa$Age == 65], usa$ex[usa$Age == 65])[[4]],
    cor.test(usa_m$e65_cv[usa_m$Age == 65], usa_m$ex[usa_m$Age == 65])[[4]],
    cor.test(usa_f$e65_cv[usa_f$Age == 65], usa_f$ex[usa_f$Age == 65])[[4]],
    
    cor.test(uk$e65_cv[uk$Age == 65], uk$ex[uk$Age == 65])[[4]],
    cor.test(uk_m$e65_cv[uk_m$Age == 65], uk_m$ex[uk_m$Age == 65])[[4]],
    cor.test(uk_f$e65_cv[uk_f$Age == 65], uk_f$ex[uk_f$Age == 65])[[4]]
  )

country <- 
  c(rep("Italy",3),
    rep("France",3),
    rep("Denmark",3),
    rep("Sweden",3),
    rep("USA",3),
    rep("UK",3))

sex <- 
  c(rep(c("Total","Male","Female"),6))

cor_e65_cv_ex <- 
  cbind(cor_e65_cv_ex, country, sex)

dati %>% 
  filter(Age == 65,
         Year < 2021) %>% 
  ggplot(aes(x = ex, y = e65_cv, color = country)) +
  geom_point(size = 1, alpha = .8) +
  theme_minimal() +
  labs(x = "Life expectancy",
       y = "Lifespan inequality (coefficent of variation)",
       title = "Correlation between life expectancy and lifespan inequality at age 65 in selected countries, both sexes.\nYears 1950-2020",
       color = "Country",
       caption = "Source: Authors' calculations on HMD data") +
  facet_wrap(~ sex) +
  scale_color_viridis_d(option = "B", end = .8)

ggsave(filename = "Output/LEvsCV.pdf")
write.csv(dati, file = "Output/dati.csv")

#cv vs prosp.age
cor_e65_cv_prosp_age <- 
  rbind(
    cor.test(dnk_m$e65_cv[dnk_m$Age == 65], prosp_age_dnk_m$prosp_age)[[4]],
    cor.test(dnk_f$e65_cv[dnk_f$Age == 65], prosp_age_dnk_f$prosp_age)[[4]],
    
    cor.test(ita_m$e65_cv[ita_m$Age == 65], prosp_age_ita_m$prosp_age)[[4]],
    cor.test(ita_f$e65_cv[ita_f$Age == 65], prosp_age_ita_f$prosp_age)[[4]],
    
    cor.test(swe_m$e65_cv[swe_m$Age == 65], prosp_age_swe_m$prosp_age)[[4]],
    cor.test(swe_f$e65_cv[swe_f$Age == 65], prosp_age_swe_f$prosp_age)[[4]],
    
    cor.test(fra_m$e65_cv[fra_m$Age == 65], prosp_age_fra_m$prosp_age)[[4]],
    cor.test(fra_f$e65_cv[fra_f$Age == 65], prosp_age_fra_f$prosp_age)[[4]],
    
    cor.test(usa_m$e65_cv[usa_m$Age == 65], prosp_age_usa_m$prosp_age)[[4]],
    cor.test(usa_f$e65_cv[usa_f$Age == 65], prosp_age_usa_f$prosp_age)[[4]],
    
    cor.test(uk_m$e65_cv[uk_m$Age == 65], prosp_age_uk_m$prosp_age)[[4]],
    cor.test(uk_f$e65_cv[uk_f$Age == 65], prosp_age_uk_f$prosp_age)[[4]]
  )

country <- 
  c(rep("Italy",2),
    rep("France",2),
    rep("Denmark",2),
    rep("Sweden",2),
    rep("USA",2),
    rep("UK",2))

sex <- 
  c(rep(c("Male","Female"),6))

cor_e65_cv_prosp_age <- 
  cbind(cor_e65_cv_prosp_age, country, sex)

left_join(prosp_age_m_f,
          dati %>% 
            filter(Age == 65) %>% 
            select(Year, country, sex, e65_cv) %>% 
            rename(year = Year),
          by = c("year", "country", "sex")) %>% 
  ggplot(aes(x = e65_cv, y = prosp_age, color = country)) +
  geom_point(size = 1, alpha = .8) +
  theme_minimal() +
  labs(x = "Lifespan inequality (coefficent of variation)",
       y = "Prospective age",
       title = "Correlation between lifespan inequality at age 65 and prospective age in selected countries, both sexes.\nYears 1950-2020. Reference: remaining life expectancy at age 65 for UK males in 1950",
       color = "Country",
       caption = "Source: Authors' calculations on HMD data") +
  facet_wrap(~ sex) +
  scale_color_viridis_d(option = "B", end = .8)

ggsave(filename = "Output/CVvsProsp_Age.pdf")

#ex vs prosp.age
cor_ex_prosp_age <- 
  rbind(
    cor.test(dnk_m$ex[dnk_m$Age == 65], prosp_age_dnk_m$prosp_age)[[4]],
    cor.test(dnk_f$ex[dnk_f$Age == 65], prosp_age_dnk_f$prosp_age)[[4]],
    
    cor.test(ita_m$ex[ita_m$Age == 65], prosp_age_ita_m$prosp_age)[[4]],
    cor.test(ita_f$ex[ita_f$Age == 65], prosp_age_ita_f$prosp_age)[[4]],
    
    cor.test(swe_m$ex[swe_m$Age == 65], prosp_age_swe_m$prosp_age)[[4]],
    cor.test(swe_f$ex[swe_f$Age == 65], prosp_age_swe_f$prosp_age)[[4]],
    
    cor.test(fra_m$ex[fra_m$Age == 65], prosp_age_fra_m$prosp_age)[[4]],
    cor.test(fra_f$ex[fra_f$Age == 65], prosp_age_fra_f$prosp_age)[[4]],
    
    cor.test(usa_m$ex[usa_m$Age == 65], prosp_age_usa_m$prosp_age)[[4]],
    cor.test(usa_f$ex[usa_f$Age == 65], prosp_age_usa_f$prosp_age)[[4]],
    
    cor.test(uk_m$ex[uk_m$Age == 65], prosp_age_uk_m$prosp_age)[[4]],
    cor.test(uk_f$ex[uk_f$Age == 65], prosp_age_uk_f$prosp_age)[[4]]
  )

cor_ex_prosp_age <- 
  cbind(cor_ex_prosp_age, country, sex)    

left_join(prosp_age_m_f,
          dati %>% 
            filter(Age == 65) %>% 
            select(Year, country, sex, ex) %>% 
            rename(year = Year),
          by = c("year", "country", "sex")) %>% 
  ggplot(aes(x = ex, y = prosp_age, color = country)) +
  geom_point(size = 1, alpha = .8) +
  theme_minimal() +
  labs(x = "Life expectancy",
       y = "Prospective age",
       title = "Correlation between life expectancy at age 65 and prospective age in selected countries, both sexes.\nYears 1950-2020. Reference: remaining life expectancy at age 65 for UK males in 1950",
       color = "Country",
       caption = "Source: Authors' calculations on HMD data") +
  facet_wrap(~ sex) +
  scale_color_viridis_d(option = "B", end = .8)

ggsave(filename = "Output/LEvsProsp_Age.pdf")



# Analysis: mode vs prosp.age vs LE ---------------------------------------

#mode vs ex age = 65
cor_mode_ex <- 
  rbind(
    cor.test(mode_dnk_m$mode_spline, dnk_m$ex[dnk_m$Age == 65])[[4]],
    cor.test(mode_dnk_f$mode_spline, dnk_f$ex[dnk_f$Age == 65])[[4]],
    
    cor.test(mode_ita_m$mode_spline, ita_m$ex[ita_m$Age == 65])[[4]],
    cor.test(mode_ita_f$mode_spline, ita_f$ex[ita_f$Age == 65])[[4]],
    
    cor.test(mode_fra_m$mode_spline, fra_m$ex[fra_m$Age == 65])[[4]],
    cor.test(mode_fra_f$mode_spline, fra_f$ex[fra_f$Age == 65])[[4]],
    
    cor.test(mode_swe_m$mode_spline, swe_m$ex[swe_m$Age == 65])[[4]],
    cor.test(mode_swe_f$mode_spline, swe_f$ex[swe_f$Age == 65])[[4]],
    
    cor.test(mode_usa_m$mode_spline, usa_m$ex[usa_m$Age == 65])[[4]],
    cor.test(mode_usa_f$mode_spline, usa_f$ex[usa_f$Age == 65])[[4]],
    
    cor.test(mode_uk_m$mode_spline, uk_m$ex[uk_m$Age == 65])[[4]],
    cor.test(mode_uk_f$mode_spline, uk_f$ex[uk_f$Age == 65])[[4]]
  )
    
cor_mode_ex <- 
  cbind(cor_mode_ex, country, sex)

left_join(mode_m_f,
          dati %>% 
            filter(Age == 65) %>% 
            select(Year, country, sex, ex) %>% 
            rename(year = Year),
          by = c("year", "country", "sex")) %>% 
  ggplot(aes(x = ex, y = mode_spline, color = country)) +
  geom_point(size = 1, alpha = .8) +
  theme_minimal() +
  labs(x = "Life expectancy",
       y = "Modal age at death",
       title = "Correlation between life expectancy  at age 65 and modal age at death in selected countries, both sexes.\nYears 1950-2020",
       color = "Country",
       caption = "Source: Authors' calculations on HMD data") +
  facet_wrap(~ sex) +
  scale_color_viridis_d(option = "B", end = .8)

ggsave(filename = "Output/LEvsMode.pdf")


#mode vs prosp.age
cor_mode_prosp_age <- 
  rbind(
    cor.test(mode_dnk_m$mode_spline, prosp_age_dnk_m$prosp_age)[[4]],
    cor.test(mode_dnk_f$mode_spline, prosp_age_dnk_f$prosp_age)[[4]],
    
    cor.test(mode_ita_m$mode_spline, prosp_age_ita_m$prosp_age)[[4]],
    cor.test(mode_ita_f$mode_spline, prosp_age_ita_f$prosp_age)[[4]],
    
    cor.test(mode_fra_m$mode_spline, prosp_age_fra_m$prosp_age)[[4]],
    cor.test(mode_fra_f$mode_spline, prosp_age_fra_f$prosp_age)[[4]],
    
    cor.test(mode_swe_m$mode_spline, prosp_age_swe_m$prosp_age)[[4]],
    cor.test(mode_swe_f$mode_spline, prosp_age_swe_f$prosp_age)[[4]],
    
    cor.test(mode_usa_m$mode_spline, prosp_age_usa_m$prosp_age)[[4]],
    cor.test(mode_usa_f$mode_spline, prosp_age_usa_f$prosp_age)[[4]],
    
    cor.test(mode_uk_m$mode_spline, prosp_age_uk_m$prosp_age)[[4]],
    cor.test(mode_uk_f$mode_spline, prosp_age_uk_f$prosp_age)[[4]]
  )
    
cor_mode_prosp_age <- 
  cbind(cor_mode_prosp_age, country, sex)

left_join(mode_m_f,
          prosp_age_m_f,
          by = c("year", "country", "sex")) %>% 
  ggplot(aes(x = mode_spline, y = prosp_age, color = country)) +
  geom_point(size = 1, alpha = .8) +
  theme_minimal() +
  labs(x = "Modal age at death",
       y = "Prospective age",
       title = "Correlation between modal age at death and prospective age in selected countries, both sexes.\nYears 1950-2020. Reference: remaining life expectancy at age 65 for UK males in 1950",
       color = "Country",
       caption = "Source: Authors' calculations on HMD data") +
  facet_wrap(~ sex) +
  scale_color_viridis_d(option = "B", end = .8)

ggsave(filename = "Output/ModevsProsp_Age.pdf")

#mode vs e_dag age = 65
cor_mode_e_dag <- 
  rbind(
    cor.test(mode_dnk_m$mode_spline, dnk_m$e_dag2[dnk_m$Age == 65])[[4]],
    cor.test(mode_dnk_f$mode_spline, dnk_f$e_dag2[dnk_f$Age == 65])[[4]],
    
    cor.test(mode_ita_m$mode_spline, ita_m$e_dag2[ita_m$Age == 65])[[4]],
    cor.test(mode_ita_f$mode_spline, ita_f$e_dag2[ita_f$Age == 65])[[4]],
    
    cor.test(mode_fra_m$mode_spline, fra_m$e_dag2[fra_m$Age == 65])[[4]],
    cor.test(mode_fra_f$mode_spline, fra_f$e_dag2[fra_f$Age == 65])[[4]],
    
    cor.test(mode_swe_m$mode_spline, swe_m$e_dag2[swe_m$Age == 65])[[4]],
    cor.test(mode_swe_f$mode_spline, swe_f$e_dag2[swe_f$Age == 65])[[4]],
    
    cor.test(mode_usa_m$mode_spline, usa_m$e_dag2[usa_m$Age == 65])[[4]],
    cor.test(mode_usa_f$mode_spline, usa_f$e_dag2[usa_f$Age == 65])[[4]],
    
    cor.test(mode_uk_m$mode_spline, uk_m$e_dag2[uk_m$Age == 65])[[4]],
    cor.test(mode_uk_f$mode_spline, uk_f$e_dag2[uk_f$Age == 65])[[4]]
  )
    
cor_mode_e_dag <- 
  cbind(cor_mode_e_dag, country, sex)

left_join(mode_m_f,
          dati %>% 
            filter(Age == 65) %>% 
            select(Year, country, sex, e_dag) %>% 
            rename(year = Year),
          by = c("year", "country", "sex")) %>% 
  ggplot(aes(x = e_dag, y = mode_spline, color = country)) +
  geom_point(size = 1, alpha = .8) +
  theme_minimal() +
  labs(x = "Lifespan inequality (e-dagger)",
       y = "Modal age at death",
       title = "Correlation between lifespan inequality at age 65 and modal age at death in selected countries, both sexes.\nYears 1950-2020",
       color = "Country",
       caption = "Source: Authors' calculations on HMD data") +
  facet_wrap(~ sex) +
  scale_color_viridis_d(option = "B", end = .8)

ggsave(filename = "Output/LIvsMode.pdf")
# Export correlations -----------------------------------------------------

write.csv(cor_e_dag_ex, file = "Output/cor_e_dag_ex.csv")
write.csv(cor_e_dag_prosp_age, file = "Output/cor_e_dag_prosp_age.csv")
write.csv(cor_ex_prosp_age, file = "Output/cor_ex_prosp_age.csv")

write.csv(cor_e65_sd_ex, file = "Output/cor_e65_sd_ex.csv")
write.csv(cor_e65_sd_prosp_age, file = "Output/cor_e65_sd_prosp_age.csv")

write.csv(cor_e65_cv_ex, file = "Output/cor_e65_cv_ex.csv")
write.csv(cor_e65_cv_prosp_age, file = "Output/cor_e65_cv_prosp_age.csv")

write.csv(cor_mode_ex, file = "Output/cor_mode_ex.csv")
write.csv(cor_mode_e_dag, file = "Output/cor_mode_e_dag.csv")
write.csv(cor_mode_prosp_age, file = "Output/cor_mode_prosp_age.csv")

# -------------------------------------------------------------------------
### REVISION ###
# -------------------------------------------------------------------------
# Equivalent age: international comparison between countries male ---------------

#new values of age for smoothing 
newAge <- seq(0, 110, by = 0.001)

#reference value: same for all
ref.uk_m <- 
  uk_m %>% 
  filter(Year == 1950,
         Age == 65) %>% 
  summarise(qx = qx) %>% 
  as.numeric()


## ITA
#df to fill with prospective ages
equi_age_ita_m <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function equi.age
for(i in min(ita_m$Year):max(ita_m$Year)){
  
  df <- ita_m %>% filter(Year == i)
  age <- equi.age(df$Age, newAge, df$qx, ref.uk_m)
  equi_age_ita_m <- 
    rbind(equi_age_ita_m,
          t(c(i,age)))
}

#rename columns
colnames(equi_age_ita_m) <- c("year", "equi_age")


## FRA
#df to fill with prospective ages
equi_age_fra_m <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function equi.age
for(i in min(fra_m$Year):max(fra_m$Year)){
  
  df <- fra_m %>% filter(Year == i)
  age <- equi.age(df$Age, newAge, df$qx, ref.uk_m)
  equi_age_fra_m <- 
    rbind(equi_age_fra_m,
          t(c(i,age)))
}

#rename columns
colnames(equi_age_fra_m) <- c("year", "equi_age")


## DNK
#df to fill with prospective ages
equi_age_dnk_m <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function equi.age
for(i in min(dnk_m$Year):max(dnk_m$Year)){
  
  df <- dnk_m %>% filter(Year == i)
  age <- equi.age(df$Age, newAge, df$qx, ref.uk_m)
  equi_age_dnk_m <- 
    rbind(equi_age_dnk_m,
          t(c(i,age)))
}

#rename columns
colnames(equi_age_dnk_m) <- c("year", "equi_age")


## SWE
#df to fill with prospective ages
equi_age_swe_m <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function equi.age
for(i in min(swe_m$Year):max(swe_m$Year)){
  
  df <- swe_m %>% filter(Year == i)
  age <- equi.age(df$Age, newAge, df$qx, ref.uk_m)
  equi_age_swe_m <- 
    rbind(equi_age_swe_m,
          t(c(i,age)))
}

#rename columns
colnames(equi_age_swe_m) <- c("year", "equi_age")


## UK
#df to fill with prospective ages
equi_age_uk_m <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function equi.age
for(i in min(uk_m$Year):max(uk_m$Year)){
  
  df <- uk_m %>% filter(Year == i)
  age <- equi.age(df$Age, newAge, df$qx, ref.uk_m)
  equi_age_uk_m <- 
    rbind(equi_age_uk_m,
          t(c(i,age)))
}

#rename columns
colnames(equi_age_uk_m) <- c("year", "equi_age")

## USA
#df to fill with prospective ages
equi_age_usa_m <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function equi.age
for(i in min(usa_m$Year):max(usa_m$Year)){
  
  df <- usa_m %>% filter(Year == i)
  age <- equi.age(df$Age, newAge, df$qx, ref.uk_m)
  equi_age_usa_m <- 
    rbind(equi_age_usa_m,
          t(c(i,age)))
}

#rename columns
colnames(equi_age_usa_m) <- c("year", "equi_age")

## MERGE ALL
equi_age_m <- 
  rbind(equi_age_ita_m %>% mutate(country = "Italy"),
        equi_age_fra_m %>% mutate(country = "France"),
        equi_age_dnk_m %>% mutate(country = "Denmark"),
        equi_age_swe_m %>% mutate(country = "Sweden"),
        equi_age_uk_m %>% mutate(country = "UK"),
        equi_age_usa_m %>% mutate(country = "USA")) %>% 
  filter(year < 2021) %>% 
  mutate(sex = "Male")

# Equivalent age: international comparison between countries female -------------

#new values of age for smoothing 
newAge <- seq(0, 110, by = 0.001)

#reference value: same for all
ref.uk_m <- 
  uk_m %>% 
  filter(Year == 1950,
         Age == 65) %>% 
  summarise(qx = qx) %>% 
  as.numeric()


## ITA
#df to fill with prospective ages
equi_age_ita_f <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function equi.age
for(i in min(ita_f$Year):max(ita_f$Year)){
  
  df <- ita_f %>% filter(Year == i, Age > 1) #correction for a year resulting age 0
  age <- equi.age(df$Age, newAge, df$qx, ref.uk_m)
  equi_age_ita_f <- 
    rbind(equi_age_ita_f,
          t(c(i,age)))
}

#rename columns
colnames(equi_age_ita_f) <- c("year", "equi_age")


## FRA
#df to fill with prospective ages
equi_age_fra_f <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function equi.age
for(i in min(fra_f$Year):max(fra_f$Year)){
  
  df <- fra_f %>% filter(Year == i)
  age <- equi.age(df$Age, newAge, df$qx, ref.uk_m)
  equi_age_fra_f <- 
    rbind(equi_age_fra_f,
          t(c(i,age)))
}

#rename columns
colnames(equi_age_fra_f) <- c("year", "equi_age")


## DNK
#df to fill with prospective ages
equi_age_dnk_f <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function equi.age
for(i in min(dnk_f$Year):max(dnk_f$Year)){
  
  df <- dnk_f %>% filter(Year == i)
  age <- equi.age(df$Age, newAge, df$qx, ref.uk_m)
  equi_age_dnk_f <- 
    rbind(equi_age_dnk_f,
          t(c(i,age)))
}

#rename columns
colnames(equi_age_dnk_f) <- c("year", "equi_age")


## SWE
#df to fill with prospective ages
equi_age_swe_f <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function equi.age
for(i in min(swe_f$Year):max(swe_f$Year)){
  
  df <- swe_f %>% filter(Year == i)
  age <- equi.age(df$Age, newAge, df$qx, ref.uk_m)
  equi_age_swe_f <- 
    rbind(equi_age_swe_f,
          t(c(i,age)))
}

#rename columns
colnames(equi_age_swe_f) <- c("year", "equi_age")


## UK
#df to fill with prospective ages
equi_age_uk_f <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function equi.age
for(i in min(uk_f$Year):max(uk_f$Year)){
  
  df <- uk_f %>% filter(Year == i)
  age <- equi.age(df$Age, newAge, df$qx, ref.uk_m)
  equi_age_uk_f <- 
    rbind(equi_age_uk_f,
          t(c(i,age)))
}

#rename columns
colnames(equi_age_uk_f) <- c("year", "equi_age")

## USA
#df to fill with prospective ages
equi_age_usa_f <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function equi.age
for(i in min(usa_f$Year):max(usa_f$Year)){
  
  df <- usa_f %>% filter(Year == i)
  age <- equi.age(df$Age, newAge, df$qx, ref.uk_m)
  equi_age_usa_f <- 
    rbind(equi_age_usa_f,
          t(c(i,age)))
}

#rename columns
colnames(equi_age_usa_f) <- c("year", "equi_age")

## MERGE ALL
equi_age_f <- 
  rbind(equi_age_ita_f %>% mutate(country = "Italy"),
        equi_age_fra_f %>% mutate(country = "France"),
        equi_age_dnk_f %>% mutate(country = "Denmark"),
        equi_age_swe_f %>% mutate(country = "Sweden"),
        equi_age_uk_f %>% mutate(country = "UK"),
        equi_age_usa_f %>% mutate(country = "USA")) %>% 
  filter(year < 2021) %>% 
  mutate(sex = "Female")

# Results equivalent age: plots ------------------------------------------------

equi_age_m_f <- 
  rbind(equi_age_m,
        equi_age_f)

#chosing colors
show_col(viridis_pal(option="B")(30))
vB<-viridis(option="B", 30) #1:tot; 

#raw plot
equi_age_m_f %>% 
  filter(year %in% c(seq(1950,2015,5),2019,2020)) %>% #manual smoothing
  ggplot(aes(x = year, y = equi_age, color = country)) +
  geom_line(size = 1, alpha = .8) +
  geom_hline(yintercept = 65) +
  theme_minimal() +
  labs(x = "Year",
       y = "Equivalent age",
       #       title = "Prospective age computed for selected countries, both sexes. Years 1950-2020.\nReference: remaining life expectancy at age 65 for UK males in 1950",
       #  caption = "Source: Authors' calculations on HMD data"
       color = "Country") +
  facet_wrap(~ fct_rev(sex)) +
  scale_color_manual(values = vB[c(1, 6, 11, 16, 21, 26)]) +
  scale_y_continuous(limits = c(62, 83))


#smoothed plot
equi_age_m_f %>%
  ggplot(aes(x = year, y = equi_age, color = country)) +
  #geom_point() +
  geom_line(stat = "smooth", size = 1, alpha = .8) +
  geom_hline(yintercept = 65) +
  theme_minimal(base_size = 12) +
  labs(x = "Year",
       y = "Equivalent age",
       #       title = "Prospective age computed for selected countries, both sexes. Years 1950-2020.\nReference: remaining life expectancy at age 65 for UK male in 1950",
       #       caption = "Source: Authors' calculations on HMD data",
       color = "Country") +
  facet_wrap(~ fct_rev(sex)) +
  scale_color_viridis_d(option = "B", end = .8)+
  scale_y_continuous(limits = c(63.5, 80))

ggsave(filename = "Output/equi_age_m_f_raw.pdf", width = 8, height = 4, dpi = 300)
write.csv(equi_age_m_f, file = "Output/equi_age_m_f.csv")

#variability equivalent and prospective age
equi_age_m_f %>% 
  group_by(country) %>% 
  summarise(sd(equi_age))

prosp_age_m_f %>% 
  group_by(country) %>% 
  summarise(sd(prosp_age))


# Analysis: equivalent.age vs prosp.age -----------------------------------

cor_equi_prosp_age <- 
  rbind(
    cor.test(equi_age_dnk_m$equi_age, prosp_age_dnk_m$prosp_age)[[4]],
    cor.test(equi_age_dnk_f$equi_age, prosp_age_dnk_f$prosp_age)[[4]],
    
    cor.test(equi_age_ita_m$equi_age, prosp_age_ita_m$prosp_age)[[4]],
    cor.test(equi_age_ita_f$equi_age, prosp_age_ita_f$prosp_age)[[4]],
    
    cor.test(equi_age_swe_m$equi_age, prosp_age_swe_m$prosp_age)[[4]],
    cor.test(equi_age_swe_f$equi_age, prosp_age_swe_f$prosp_age)[[4]],
    
    cor.test(equi_age_fra_m$equi_age, prosp_age_fra_m$prosp_age)[[4]],
    cor.test(equi_age_fra_f$equi_age, prosp_age_fra_f$prosp_age)[[4]],
    
    cor.test(equi_age_usa_m$equi_age, prosp_age_usa_m$prosp_age)[[4]],
    cor.test(equi_age_usa_f$equi_age, prosp_age_usa_f$prosp_age)[[4]],
    
    cor.test(equi_age_uk_m$equi_age, prosp_age_uk_m$prosp_age)[[4]],
    cor.test(equi_age_uk_f$equi_age, prosp_age_uk_f$prosp_age)[[4]]
  )

country <- 
  c(rep("Italy",2),
    rep("France",2),
    rep("Denmark",2),
    rep("Sweden",2),
    rep("USA",2),
    rep("UK",2))

sex <- 
  c(rep(c("Male","Female"),6))

cor_equi_prosp_age <- 
  cbind(cor_equi_prosp_age, country, sex)

write.csv(cor_equi_prosp_age, file = "Output/cor_equi_prosp_age.csv")

left_join(prosp_age_m_f,
          equi_age_m_f,
          by = c("year", "country", "sex")) %>% 
  ggplot(aes(x = prosp_age, y = equi_age, color = fct_rev(sex))) +
  geom_point(size = 1, alpha = .8) +
  theme_minimal() +
  labs(x = "POAT",
       y = "Equivalent age",
       # title = "Correlation between entropy at age 65 and prospective age in selected countries, both sexes.\nYears 1950-2020. Reference: remaining life expectancy at age 65 for UK males in 1950",
       # caption = "Source: Authors' calculations on HMD data",
       color = "Sex") +
  facet_wrap(~ country) +
  scale_color_manual(values = c("#0d47a1","#e65100"))

ggsave(filename = "Output/EquivsProsp_Age.pdf", width = 8, height = 4, dpi = 300)

# Prosp age: different reference male -------------------------------------

#new values of age for smoothing 
newAge <- seq(0, 110, by = 0.001)

#reference value: same for all
ref.fr_m <- 
  fra_m %>% 
  filter(Year == 1970,
         Age == 65) %>% 
  summarise(ex = ex) %>% 
  as.numeric()


## ITA
#df to fill with prospective ages
prosp_age_ita_m <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function prosp.age
for(i in min(ita_m$Year):max(ita_m$Year)){
  
  df <- ita_m %>% filter(Year == i)
  age <- prosp.age(df$Age, newAge, df$ex, ref.fr_m)
  prosp_age_ita_m <- 
    rbind(prosp_age_ita_m,
          t(c(i,age)))
}

#rename columns
colnames(prosp_age_ita_m) <- c("year", "prosp_age")


## FRA
#df to fill with prospective ages
prosp_age_fra_m <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function prosp.age
for(i in min(fra_m$Year):max(fra_m$Year)){
  
  df <- fra_m %>% filter(Year == i)
  age <- prosp.age(df$Age, newAge, df$ex, ref.fr_m)
  prosp_age_fra_m <- 
    rbind(prosp_age_fra_m,
          t(c(i,age)))
}

#rename columns
colnames(prosp_age_fra_m) <- c("year", "prosp_age")


## DNK
#df to fill with prospective ages
prosp_age_dnk_m <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function prosp.age
for(i in min(dnk_m$Year):max(dnk_m$Year)){
  
  df <- dnk_m %>% filter(Year == i)
  age <- prosp.age(df$Age, newAge, df$ex, ref.fr_m)
  prosp_age_dnk_m <- 
    rbind(prosp_age_dnk_m,
          t(c(i,age)))
}

#rename columns
colnames(prosp_age_dnk_m) <- c("year", "prosp_age")


## SWE
#df to fill with prospective ages
prosp_age_swe_m <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function prosp.age
for(i in min(swe_m$Year):max(swe_m$Year)){
  
  df <- swe_m %>% filter(Year == i)
  age <- prosp.age(df$Age, newAge, df$ex, ref.fr_m)
  prosp_age_swe_m <- 
    rbind(prosp_age_swe_m,
          t(c(i,age)))
}

#rename columns
colnames(prosp_age_swe_m) <- c("year", "prosp_age")


## UK
#df to fill with prospective ages
prosp_age_uk_m <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function prosp.age
for(i in min(uk_m$Year):max(uk_m$Year)){
  
  df <- uk_m %>% filter(Year == i)
  age <- prosp.age(df$Age, newAge, df$ex, ref.fr_m)
  prosp_age_uk_m <- 
    rbind(prosp_age_uk_m,
          t(c(i,age)))
}

#rename columns
colnames(prosp_age_uk_m) <- c("year", "prosp_age")

## USA
#df to fill with prospective ages
prosp_age_usa_m <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function prosp.age
for(i in min(usa_m$Year):max(usa_m$Year)){
  
  df <- usa_m %>% filter(Year == i)
  age <- prosp.age(df$Age, newAge, df$ex, ref.fr_m)
  prosp_age_usa_m <- 
    rbind(prosp_age_usa_m,
          t(c(i,age)))
}

#rename columns
colnames(prosp_age_usa_m) <- c("year", "prosp_age")

## MERGE ALL
prosp_age_m <- 
  rbind(prosp_age_ita_m %>% mutate(country = "Italy"),
        prosp_age_fra_m %>% mutate(country = "France"),
        prosp_age_dnk_m %>% mutate(country = "Denmark"),
        prosp_age_swe_m %>% mutate(country = "Sweden"),
        prosp_age_uk_m %>% mutate(country = "UK"),
        prosp_age_usa_m %>% mutate(country = "USA")) %>% 
  filter(year < 2021) %>% 
  mutate(sex = "Male")

# Prosp age: different reference female -----------------------------------

#new values of age for smoothing 
newAge <- seq(0, 110, by = 0.001)

#reference value: same for all
ref.fr_m <- 
  fra_m %>% 
  filter(Year == 1970,
         Age == 65) %>% 
  summarise(ex = ex) %>% 
  as.numeric()


## ITA
#df to fill with prospective ages
prosp_age_ita_f <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function prosp.age
for(i in min(ita_f$Year):max(ita_f$Year)){
  
  df <- ita_f %>% filter(Year == i)
  age <- prosp.age(df$Age, newAge, df$ex, ref.fr_m)
  prosp_age_ita_f <- 
    rbind(prosp_age_ita_f,
          t(c(i,age)))
}

#rename columns
colnames(prosp_age_ita_f) <- c("year", "prosp_age")


## FRA
#df to fill with prospective ages
prosp_age_fra_f <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function prosp.age
for(i in min(fra_f$Year):max(fra_f$Year)){
  
  df <- fra_f %>% filter(Year == i)
  age <- prosp.age(df$Age, newAge, df$ex, ref.fr_m)
  prosp_age_fra_f <- 
    rbind(prosp_age_fra_f,
          t(c(i,age)))
}

#rename columns
colnames(prosp_age_fra_f) <- c("year", "prosp_age")


## DNK
#df to fill with prospective ages
prosp_age_dnk_f <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function prosp.age
for(i in min(dnk_f$Year):max(dnk_f$Year)){
  
  df <- dnk_f %>% filter(Year == i)
  age <- prosp.age(df$Age, newAge, df$ex, ref.fr_m)
  prosp_age_dnk_f <- 
    rbind(prosp_age_dnk_f,
          t(c(i,age)))
}

#rename columns
colnames(prosp_age_dnk_f) <- c("year", "prosp_age")


## SWE
#df to fill with prospective ages
prosp_age_swe_f <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function prosp.age
for(i in min(swe_f$Year):max(swe_f$Year)){
  
  df <- swe_f %>% filter(Year == i)
  age <- prosp.age(df$Age, newAge, df$ex, ref.fr_m)
  prosp_age_swe_f <- 
    rbind(prosp_age_swe_f,
          t(c(i,age)))
}

#rename columns
colnames(prosp_age_swe_f) <- c("year", "prosp_age")


## UK
#df to fill with prospective ages
prosp_age_uk_f <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function prosp.age
for(i in min(uk_f$Year):max(uk_f$Year)){
  
  df <- uk_f %>% filter(Year == i)
  age <- prosp.age(df$Age, newAge, df$ex, ref.fr_m)
  prosp_age_uk_f <- 
    rbind(prosp_age_uk_f,
          t(c(i,age)))
}

#rename columns
colnames(prosp_age_uk_f) <- c("year", "prosp_age")

## USA
#df to fill with prospective ages
prosp_age_usa_f <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function prosp.age
for(i in min(usa_f$Year):max(usa_f$Year)){
  
  df <- usa_f %>% filter(Year == i)
  age <- prosp.age(df$Age, newAge, df$ex, ref.fr_m)
  prosp_age_usa_f <- 
    rbind(prosp_age_usa_f,
          t(c(i,age)))
}

#rename columns
colnames(prosp_age_usa_f) <- c("year", "prosp_age")

## MERGE ALL
prosp_age_f <- 
  rbind(prosp_age_ita_f %>% mutate(country = "Italy"),
        prosp_age_fra_f %>% mutate(country = "France"),
        prosp_age_dnk_f %>% mutate(country = "Denmark"),
        prosp_age_swe_f %>% mutate(country = "Sweden"),
        prosp_age_uk_f %>% mutate(country = "UK"),
        prosp_age_usa_f %>% mutate(country = "USA")) %>% 
  filter(year < 2021) %>% 
  mutate(sex = "Female")

# Results prosp.age: plots ------------------------------------------------

prosp_age_m_f <- 
  rbind(prosp_age_m,
        prosp_age_f)

#chosing colors
show_col(viridis_pal(option="B")(30))
vB<-viridis(option="B", 30) #1:tot; 

#raw plot
prosp_age_m_f %>% 
  filter(year %in% c(seq(1950,2015,5),2019,2020)) %>% #manual smoothing
  ggplot(aes(x = year, y = prosp_age, color = country)) +
  geom_line(size = 1, alpha = .8) +
  geom_hline(yintercept = 65) +
  theme_minimal() +
  labs(x = "Year",
       y = "POAT",
       #       title = "Prospective age computed for selected countries, both sexes. Years 1950-2020.\nReference: remaining life expectancy at age 65 for UK males in 1950",
       #  caption = "Source: Authors' calculations on HMD data"
       color = "Country") +
  facet_wrap(~ fct_rev(sex)) +
  scale_color_manual(values = vB[c(1, 6, 11, 16, 21, 26)]) +
  scale_y_continuous(limits = c(62.5, 80))

#smoothed plot
# prosp_age_m_f %>%
#   ggplot(aes(x = year, y = prosp_age, color = country)) +
#   #geom_point() +
#   geom_line(stat = "smooth", size = 1, alpha = .8) +
#   geom_hline(yintercept = 65) +
#   theme_minimal(base_size = 12) +
#   labs(x = "Year",
#        y = "Prospective age",
#        #       title = "Prospective age computed for selected countries, both sexes. Years 1950-2020.\nReference: remaining life expectancy at age 65 for UK male in 1950",
#        #       caption = "Source: Authors' calculations on HMD data",
#        color = "Country") +
#   facet_wrap(~ fct_rev(sex)) +
#   scale_color_viridis_d(option = "B", end = .8)+
#   scale_y_continuous(limits = c(63.5, 80))

ggsave(filename = "Output/prosp_age_m_f_raw_referenceFR.pdf", width = 8, height = 4, dpi = 300)
# write.csv(prosp_age_m_f, file = "Output/prosp_age_m_f_referenceFR.csv")

# s-age: international comparison between countries male ---------------

#new values of age for smoothing 
newAge <- seq(0, 110, by = 0.001)

#reference value: same for all
ref.uk_m <- 
  uk_m %>% 
  filter(Year == 1950,
         Age == 65) %>% 
  summarise(lx = lx/100000) %>% 
  as.numeric()


## ITA
#df to fill with prospective ages
s_age_ita_m <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function s.age
for(i in min(ita_m$Year):max(ita_m$Year)){
  
  df <- ita_m %>% filter(Year == i)
  age <- s.age(df$Age, newAge, df$lx/100000, ref.uk_m)
  s_age_ita_m <- 
    rbind(s_age_ita_m,
          t(c(i,age)))
}

#rename columns
colnames(s_age_ita_m) <- c("year", "s_age")


## FRA
#df to fill with prospective ages
s_age_fra_m <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function s.age
for(i in min(fra_m$Year):max(fra_m$Year)){
  
  df <- fra_m %>% filter(Year == i)
  age <- s.age(df$Age, newAge, df$lx/100000, ref.uk_m)
  s_age_fra_m <- 
    rbind(s_age_fra_m,
          t(c(i,age)))
}

#rename columns
colnames(s_age_fra_m) <- c("year", "s_age")


## DNK
#df to fill with prospective ages
s_age_dnk_m <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function s.age
for(i in min(dnk_m$Year):max(dnk_m$Year)){
  
  df <- dnk_m %>% filter(Year == i)
  age <- s.age(df$Age, newAge, df$lx/100000, ref.uk_m)
  s_age_dnk_m <- 
    rbind(s_age_dnk_m,
          t(c(i,age)))
}

#rename columns
colnames(s_age_dnk_m) <- c("year", "s_age")


## SWE
#df to fill with prospective ages
s_age_swe_m <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function s.age
for(i in min(swe_m$Year):max(swe_m$Year)){
  
  df <- swe_m %>% filter(Year == i)
  age <- s.age(df$Age, newAge, df$lx/100000, ref.uk_m)
  s_age_swe_m <- 
    rbind(s_age_swe_m,
          t(c(i,age)))
}

#rename columns
colnames(s_age_swe_m) <- c("year", "s_age")


## UK
#df to fill with prospective ages
s_age_uk_m <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function s.age
for(i in min(uk_m$Year):max(uk_m$Year)){
  
  df <- uk_m %>% filter(Year == i)
  age <- s.age(df$Age, newAge, df$lx/100000, ref.uk_m)
  s_age_uk_m <- 
    rbind(s_age_uk_m,
          t(c(i,age)))
}

#rename columns
colnames(s_age_uk_m) <- c("year", "s_age")

## USA
#df to fill with prospective ages
s_age_usa_m <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function s.age
for(i in min(usa_m$Year):max(usa_m$Year)){
  
  df <- usa_m %>% filter(Year == i)
  age <- s.age(df$Age, newAge, df$lx/100000, ref.uk_m)
  s_age_usa_m <- 
    rbind(s_age_usa_m,
          t(c(i,age)))
}

#rename columns
colnames(s_age_usa_m) <- c("year", "s_age")

## MERGE ALL
s_age_m <- 
  rbind(s_age_ita_m %>% mutate(country = "Italy"),
        s_age_fra_m %>% mutate(country = "France"),
        s_age_dnk_m %>% mutate(country = "Denmark"),
        s_age_swe_m %>% mutate(country = "Sweden"),
        s_age_uk_m %>% mutate(country = "UK"),
        s_age_usa_m %>% mutate(country = "USA")) %>% 
  filter(year < 2021) %>% 
  mutate(sex = "Male")

# s-age: international comparison between countries female -------------

#new values of age for smoothing 
newAge <- seq(0, 110, by = 0.001)

#reference value: same for all
ref.uk_m <- 
  uk_m %>% 
  filter(Year == 1950,
         Age == 65) %>% 
  summarise(lx = lx/100000) %>% 
  as.numeric()


## ITA
#df to fill with prospective ages
s_age_ita_f <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function s.age
for(i in min(ita_f$Year):max(ita_f$Year)){
  
  df <- ita_f %>% filter(Year == i)
  age <- s.age(df$Age, newAge, df$lx/100000, ref.uk_m)
  s_age_ita_f <- 
    rbind(s_age_ita_f,
          t(c(i,age)))
}

#rename columns
colnames(s_age_ita_f) <- c("year", "s_age")


## FRA
#df to fill with prospective ages
s_age_fra_f <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function s.age
for(i in min(fra_f$Year):max(fra_f$Year)){
  
  df <- fra_f %>% filter(Year == i)
  age <- s.age(df$Age, newAge, df$lx/100000, ref.uk_m)
  s_age_fra_f <- 
    rbind(s_age_fra_f,
          t(c(i,age)))
}

#rename columns
colnames(s_age_fra_f) <- c("year", "s_age")


## DNK
#df to fill with prospective ages
s_age_dnk_f <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function s.age
for(i in min(dnk_f$Year):max(dnk_f$Year)){
  
  df <- dnk_f %>% filter(Year == i)
  age <- s.age(df$Age, newAge, df$lx/100000, ref.uk_m)
  s_age_dnk_f <- 
    rbind(s_age_dnk_f,
          t(c(i,age)))
}

#rename columns
colnames(s_age_dnk_f) <- c("year", "s_age")


## SWE
#df to fill with prospective ages
s_age_swe_f <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function s.age
for(i in min(swe_f$Year):max(swe_f$Year)){
  
  df <- swe_f %>% filter(Year == i)
  age <- s.age(df$Age, newAge, df$lx/100000, ref.uk_m)
  s_age_swe_f <- 
    rbind(s_age_swe_f,
          t(c(i,age)))
}

#rename columns
colnames(s_age_swe_f) <- c("year", "s_age")


## UK
#df to fill with prospective ages
s_age_uk_f <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function s.age
for(i in min(uk_f$Year):max(uk_f$Year)){
  
  df <- uk_f %>% filter(Year == i)
  age <- s.age(df$Age, newAge, df$lx/100000, ref.uk_m)
  s_age_uk_f <- 
    rbind(s_age_uk_f,
          t(c(i,age)))
}

#rename columns
colnames(s_age_uk_f) <- c("year", "s_age")

## USA
#df to fill with prospective ages
s_age_usa_f <- data.frame(matrix(ncol = 2, nrow = 0))

#fill in df with function s.age
for(i in min(usa_f$Year):max(usa_f$Year)){
  
  df <- usa_f %>% filter(Year == i)
  age <- s.age(df$Age, newAge, df$lx/100000, ref.uk_m)
  s_age_usa_f <- 
    rbind(s_age_usa_f,
          t(c(i,age)))
}

#rename columns
colnames(s_age_usa_f) <- c("year", "s_age")

## MERGE ALL
s_age_f <- 
  rbind(s_age_ita_f %>% mutate(country = "Italy"),
        s_age_fra_f %>% mutate(country = "France"),
        s_age_dnk_f %>% mutate(country = "Denmark"),
        s_age_swe_f %>% mutate(country = "Sweden"),
        s_age_uk_f %>% mutate(country = "UK"),
        s_age_usa_f %>% mutate(country = "USA")) %>% 
  filter(year < 2021) %>% 
  mutate(sex = "Female")

# Results s-age: plots ------------------------------------------------

s_age_m_f <- 
  rbind(s_age_m,
        s_age_f)

#chosing colors
show_col(viridis_pal(option="B")(30))
vB<-viridis(option="B", 30) #1:tot; 

#raw plot
s_age_m_f %>% 
  filter(year %in% c(seq(1950,2015,5),2019,2020)) %>% #manual smoothing
  ggplot(aes(x = year, y = s_age, color = country)) +
  geom_line(size = 1, alpha = .8) +
  geom_hline(yintercept = 65) +
  theme_minimal() +
  labs(x = "Year",
       y = "s-age",
       #       title = "Prospective age computed for selected countries, both sexes. Years 1950-2020.\nReference: remaining life expectancy at age 65 for UK males in 1950",
       #  caption = "Source: Authors' calculations on HMD data"
       color = "Country") +
  facet_wrap(~ fct_rev(sex)) +
  scale_color_manual(values = vB[c(1, 6, 11, 16, 21, 26)]) +
  scale_y_continuous(limits = c(62, 85))


#smoothed plot
s_age_m_f %>%
  ggplot(aes(x = year, y = s_age, color = country)) +
  #geom_point() +
  geom_line(stat = "smooth", size = 1, alpha = .8) +
  geom_hline(yintercept = 65) +
  theme_minimal(base_size = 12) +
  labs(x = "Year",
       y = "Prospective age",
       #       title = "Prospective age computed for selected countries, both sexes. Years 1950-2020.\nReference: remaining life expectancy at age 65 for UK male in 1950",
       #       caption = "Source: Authors' calculations on HMD data",
       color = "Country") +
  facet_wrap(~ fct_rev(sex)) +
  scale_color_viridis_d(option = "B", end = .8)+
  scale_y_continuous(limits = c(63.5, 80))

ggsave(filename = "Output/s_age_m_f_raw.pdf", width = 8, height = 4, dpi = 300)
write.csv(s_age_m_f, file = "Output/s_age_m_f.csv")


