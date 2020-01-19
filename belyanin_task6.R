library(dplyr)
library(ggplot2)
library(gapminder)

# presintation example
ggplot(gapminder, aes(x = gdpPercap,
                      y = lifeExp,
                      size = pop,
                      color = continent)
       ) + 
  facet_wrap(.~year) +
  geom_point() + 
  scale_x_log10()

# my transformations
ggplot(gapminder, aes(x = gdpPercap,
                      y = lifeExp,
                      size = pop,
                      color = continent)
       ) +
  facet_wrap(.~year) +
  geom_point() +
  scale_x_log10() +
  ggtitle("Title something") +
  xlab("per-capita GDP") +
  ylab("life expectancy at birth") +
  theme_linedraw()

# :)
gapminder %>% filter(country == 'Pakistan' | country == 'Israel') %>% 
ggplot(aes(x = year,
           y = pop,
           size = lifeExp,
           color = country)
       ) +
  geom_line() + 
  scale_x_log10() +
  ggtitle("Title something") +
  xlab("year") +
  ylab("population") +
  theme_linedraw()
