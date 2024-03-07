# Please download GGS Harmonized histories
# https://www.ggp-i.org/data/harmonized-histories/
ggs <- read.dta13("DATA.dta", nonint.factors = T, convert.factors = T)

# Please download Population on 01 July, by single age from 1950-2021.
# https://population.un.org/wpp/Download/Standard/CSV/
d_pop <- read.csv("DATA.csv")
