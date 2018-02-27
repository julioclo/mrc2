# 1. install packages:
devtools::install_github("jvcasillas/untidydata")
devtools::install_github('yihui/xaringan')

# download and install xfun from packages if it does not work.

library(untidydata)
library(xaringan)
library(plot3D)
library(tidyverse)

# 2. Load language_diversity dataset
# 3. Explore variables, tidy (long to wide)
str(language_diversity)
head(language_diversity)
unique(language_diversity$Measurement)

ld <- language_diversity %>%
  filter(., Continent == 'Africa') %>%
  spread(., key = Measurement, value = Value) %>%
  select(., country = Country, pop = Population, area = Area, lang = Langs) %>%
  mutate(., logArea = log(area), logPop = log(pop))

# 4. Check normality, transform, plot

hist(log(ld$area))
hist(log(ld$pop))

ld %>%
  ggplot(., aes(x = logArea, y = lang)) + 
    geom_point()

ld %>%
  ggplot(., aes(x = logPop, y = lang)) + 
  geom_point()

# Cool text plot
ld %>%
  ggplot(., aes(x = logPop, y = lang, label = country)) + 
  geom_text()

ld %>%
  ggplot(., aes(x = logPop, y = lang, color = logArea)) +
  geom_point()

# 5. Fit model (MRC, 3 params)

my_mod <- lm(lang ~ logPop + logArea, data = ld)
summary(my_mod)

my_int <- lm(lang ~ logPop + logArea, + logPop:logArea, data = ld)
summary(my_int)

# lm(lang ~ logPop + logArea, + logPop:logArea, data = ld) ES IGUAL A
# lm(lang ~ logPop * logArea, data = ld)
# * means all of the main effects and all of the interactions

# 6. Write up results

# 7. Convert ot an html presentation
