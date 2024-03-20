MeanSD <- read.csv("out/Tab_MeanSD_all.csv")

# all countries combined: mean age of each country's mean age
fig_main <- MeanSD %>% 
  mutate(BC10 = factor(BC10, levels = c("1960s", "1950s", "1940s", "1930s")),
         Parity = paste("Parity", Parity),
         type_cntry = paste(Country, Type, sep = "-"),
         strange = case_when(Parity == "Parity 2" & Type %in% c("child3", "child4") ~ 1,
                             Parity == "Parity 3" & Type == "child4" ~ 1,
                             T ~ 0)) %>% 
  filter(Parity != "Parity 0", BC10 %in% c("1930s", "1940s", "1950s", "1960s"),
         strange == 0) %>% 
  mutate(Parity = case_when(Parity == "Parity 1" ~ "Total number of children at age of 40+: 1",
                            Parity == "Parity 2" ~ "Total number of children at age of 40+: 2",
                            Parity == "Parity 3" ~ "Total number of children at age of 40+: 3",
                            Parity == "Parity 4+" ~ "Total number of children at age of 40+: 4+"))
write.csv(fig_main, "out/data_fig_main.csv", row.names = F)

# for sns english
fig_main %>% 
  tibble %>% 
  ggplot(aes(x = Mean_age, y = BC10, colour = Type)) +
  facet_wrap(~ Parity, ncol = 1) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(xmin = q25, xmax = q75), width = 0.2, size = 1) +
  scale_colour_manual(labels = c("1st Childbirth", "2nd Childbirth", 
                                 "3rd Childbirth", "4th Childbirth"),
                      values = c(mycol[1], mycol[3], mycol[2], mycol[4]),
                      name = "", guide = "legend") +
  xlim(20, 35) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(x = "Age", y = "Birth cohort") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.text = element_text(size = 15),
        strip.text = element_text(size = 15),
        axis.text = element_text(size = 13, face = "bold"),
        axis.title = element_text(size = 19))
ggsave("out/AvgLifeCourse-allcountries-q2575_sns.png", width = 8, height = 9, bg = "white")

# for sns japanese
library(showtext)
font_add(family = "HiraKakuPro-W3", regular = "src/HiraKakuPro-W3.otf")
showtext_auto()

fig_main %>% 
  mutate(Parity = case_when(Parity == "Total number of children at age of 40+: 1" ~ "40歳時の子ども数：1人",
                            Parity == "Total number of children at age of 40+: 2" ~ "40歳時の子ども数：2人",
                            Parity == "Total number of children at age of 40+: 3" ~ "40歳時の子ども数：3人",
                            Parity == "Total number of children at age of 40+: 4+" ~ "40歳時の子ども数：4人以上")) %>% 
  tibble %>% 
  ggplot(aes(x = Mean_age, y = BC10, colour = Type)) +
  facet_wrap(~ Parity, ncol = 1) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(xmin = q25, xmax = q75), width = 0.2, size = 1) +
  scale_colour_manual(labels = c("第一子を産んだ年齢", "第二子を産んだ年齢", 
                                 "第三子を産んだ年齢", "第四子を産んだ年齢"),
                      values = c(mycol[1], mycol[3], mycol[2], mycol[4]),
                      name = "", guide = "legend") +
  xlim(20, 35) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(x = "年齢", y = "生まれた年代") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.text = element_text(size = 35),
        strip.text = element_text(size = 45),
        axis.text = element_text(size = 45, face = "bold"),
        axis.title = element_text(size = 45))
ggsave("out/AvgLifeCourse-allcountries-q2575_snsjpn.png", width = 8, height = 9, bg = "white")
