## total population from the UN data
d_pop_bc <- d_pop %>% 
  filter(Age %in% 40:49,
         Year %in% 1970:2009) %>% 
  mutate(bc = case_when(Year %in% 1970:1979 ~ "1930s",
                        Year %in% 1980:1989 ~ "1940s",
                        Year %in% 1990:1999 ~ "1950s",
                        Year %in% 2000:2009 ~ "1960s"),
         Country = case_when(Country == "United States of America" ~ "The US",
                             Country == "Russian Federation" ~ "Russia",
                             Country == "United Kingdom" ~ "The UK",
                             T ~ Country)) %>% 
  group_by(Country, bc) %>% 
  summarise(N = sum(PopFemale)) %>% 
  mutate(N = N * 1000)

## Harmonized Histories
ggs_f <- ggs %>% 
  filter(SEX == "Female")

d_ggs_f <- ggs_f %>% 
  mutate(SAge = YEAR_S - BORN_Y) %>% 
  filter(SAge >= 40,
         NATIVE == "Born in country of interview") %>% 
  mutate(BC10 = case_when(BORN_Y <= 1919  ~ "-1919",
                          BORN_Y >= 1920 & BORN_Y <= 1929  ~ "1920s",
                          BORN_Y >= 1930 & BORN_Y <= 1939  ~ "1930s",
                          BORN_Y >= 1940 & BORN_Y <= 1949  ~ "1940s",
                          BORN_Y >= 1950 & BORN_Y <= 1959  ~ "1950s",
                          BORN_Y >= 1960 & BORN_Y <= 1969  ~ "1960s",
                          BORN_Y >= 1970 & BORN_Y <= 1979  ~ "1970s",
                          BORN_Y >= 1980 & BORN_Y <= 1989  ~ "1980s",
                          BORN_Y >= 1990 ~ "1990s"),
         KID_1 = ifelse(KID_1 == "No child of order 1", 0, 1),
         KID_2 = ifelse(KID_2 == "No child of order 2", 0, 1),
         KID_3 = ifelse(KID_3 == "No child of order 3", 0, 1),
         KID_4 = ifelse(KID_4 == "No child of order 4", 0, 1),
         KID_1 = as.numeric(as.character(KID_1)),
         KID_2 = as.numeric(as.character(KID_2)),
         KID_3 = as.numeric(as.character(KID_3)),
         KID_4 = as.numeric(as.character(KID_4)),
         Parity = KID_1 + KID_2 + KID_3 + KID_4,
         Parity = case_when(Parity == 0 ~ "P0",
                            Parity == 1 ~ "P1",
                            Parity == 2 ~ "P2",
                            Parity == 3 ~ "P3",
                            Parity == 4 ~ "P4+"),
         child1 = KID_Y1 - BORN_Y,
         child2 = KID_Y2 - BORN_Y,
         child3 = KID_Y3 - BORN_Y,
         child4 = KID_Y4 - BORN_Y,
         Country = case_when(COUNTRY == "Bulgaria GGS wave1" ~ "Bulgaria",
                             COUNTRY == "France GGS wave1" ~ "France",
                             COUNTRY %in% c("Germany GGS wave1", "Germany Pairfam") ~ "Germany",
                             COUNTRY == "Hungary GGS wave1" ~ "Hungary",
                             COUNTRY == "Italy GGS wave1" ~ "Italy",
                             COUNTRY %in% c("Netherlands FFS", "Netherlands OG 2013") ~ "Netherlands",
                             COUNTRY == "Romania GGS wave1" ~ "Romania",
                             COUNTRY == "Russia GGS wave1" ~ "Russia",
                             COUNTRY == "UK BHPS" ~ "The UK",
                             COUNTRY %in% c("USA NSFG 1995", "USA NSFG 2007") ~ "The US",
                             COUNTRY == "Austria GGS wave1" ~ "Austria",
                             COUNTRY == "Norway GGS wave1" ~ "Norway",
                             COUNTRY == "Poland GGS wave1" ~ "Poland",
                             COUNTRY %in% c("Spain SFS 2006", "Spain SFS 2018") ~ "Spain",
                             COUNTRY == "Estonia GGS wave1" ~ "Estonia",
                             COUNTRY == "Belgium GGS wave1" ~ "Belgium",
                             COUNTRY == "Lithuania GGS wave1" ~ "Lithuania",
                             COUNTRY == "Georgia GGS wave1" ~ "Georgia",
                             COUNTRY == "Czech Republic GGS wave 1" ~ "Czechia",
                             COUNTRY == "Belarus GGS wave 1" ~ "Belarus",
                             COUNTRY == "Canada GSS 2006" ~ "Canada",
                             COUNTRY == "Sweden GGS wave 1" ~ "Sweden")) %>% 
  select(RESPID, BC10, Parity, child1, child2, child3, child4, Country) %>% 
  filter(!is.na(Country))

d_ggs_f_sel <- d_ggs_f %>% 
  filter(BC10 %in% c("1930s", "1940s", "1950s", "1960s"))

# check the number of missing cases
miss1 <- d_ggs_f_sel %>% 
  filter(!is.na(Parity))
nrow(d_ggs_f_sel) - nrow(miss1) # 153 cases

miss2 <- miss1 %>% 
  mutate(par0 = ifelse(Parity %in% c("P1", "P2", "P3", "P4+") & is.na(child1), 1, 0),
         tooearly = case_when(Parity %in% c("P1", "P2", "P3", "P4+") & child1 < 15 ~ 1,
                              Parity %in% c("P1", "P2", "P3", "P4+") & child2 < 15 ~ 1,
                              Parity %in% c("P1", "P2", "P3", "P4+") & child3 < 15 ~ 1,
                              Parity %in% c("P1", "P2", "P3", "P4+") & child4 < 15 ~ 1,
                              T ~ 0)) %>% 
  filter(par0 != 1, tooearly != 1)
nrow(miss1) - nrow(miss2) # 548 cases

D <- miss2 %>% 
  gather(key = Type, value = Age, -c(RESPID, BC10, Parity, Country)) %>%
  select(RESPID, BC10, Parity, Type, Age, Country) %>% 
  mutate(Parity = recode(Parity, "P0" = "0",
                         "P1" = "1", "P2" = "2", 
                         "P3" = "3", "P4+" = "4+"),
         Parity = factor(Parity, levels = c("0", "1", "2", "3", "4+")),
         Type = factor(Type, levels = c("child1", "child2", "child3", "child4")))

## weight
n_ggs <- table(D$BC10, D$Country)
d_weight <- n_ggs %>% 
  as.data.frame() %>% 
  rename(bc = Var1, Country = Var2, ggs = Freq) %>% 
  left_join(d_pop_bc, by = c("Country", "bc"))

d_weight_total <- d_weight %>% 
  group_by(bc) %>% 
  summarise(n_ggs = sum(ggs),
            n_pop = sum(N))

weight <- d_weight %>% 
  left_join(d_weight_total, by = "bc") %>% 
  mutate(num_prop = N / n_pop,
         denom_prop = ggs / n_ggs,
         weight = num_prop / denom_prop) %>% 
  rename(BC10 = bc)

# Average
MeanSD_country <- D %>%
  filter(BC10 %out% c("-1919", "1920-1929"), !is.na(Type),
         Country %out% c("Austria", "The US")) %>%
  group_by(BC10, Country, Parity, Type) %>% 
  summarise(Mean_age = mean(Age, na.rm = T)) %>% 
  ungroup(BC10, Country, Parity, Type)

MeanSD_all <- MeanSD_country %>% 
  filter(BC10 %out% c("-1919", "1920-1929"), !is.na(Type)) %>% 
  left_join(weight, by = c("BC10", "Country")) %>% 
  filter(ggs!= 0, Parity != 0, Mean_age > 0) %>% 
  group_by(BC10, Parity, Type) %>% 
  summarise(Mean_age2 = weighted.mean(Mean_age, weight, na.rm = T),
            SD = radiant.data::weighted.sd(Mean_age, weight, na.rm = T),
            q25 = Hmisc::wtd.quantile(Mean_age, weight, 0.25, na.rm = T),
            q75 = Hmisc::wtd.quantile(Mean_age, weight, 0.75, na.rm = T)) %>% 
  ungroup(BC10, Parity, Type) %>% 
  mutate(Country = "All") %>% 
  rename(Mean_age = Mean_age2)
write.csv(MeanSD_all, "out/Tab_MeanSD_all.csv")
