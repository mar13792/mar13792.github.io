---
title: "Data Wrangling Project"
date: 2021-02-19
tags: [gapminder, data science, global health]
header:
  image: "/images/perceptron/percept.jpg"
excerpt: "Data Wrangling, Data Science, Messy Data"
mathjax: "true"
---

---
title: 'Gapminder World Dataset: Exploratory Analysis with R Markdown'
author: "Meg Robertson"
output:
  html_document:
    df_print: paged
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
library(pander)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(gapminder)
library(knitr)
library(plotly)
library(arsenal)
library(kableExtra)
library(lemon)
```

<!-- README file: https://cran.r-project.org/web/packages/gapminder/README.html -->
`r Sys.Date()`

## Dataset Overview
The Gapminder dataset contains country-level data from `r min(gapminder$year)` to `r max(gapminder$year)` (in five-year increments) and inlcudes the following variables:

- Country: the country with which the row is associated
- Continent: the geographic area in which the country is located (Africa, Americas, Asia, Europe, and Oceania)
- Year: the year each measure was recorded 
- Life Expectancy: average life expecatancy at birth
- Population: population of the country
- GDP per Capita: GDP per capita for the country in US dollars

This following exploratory analysis highlights trends over time and comparisons among continents and countries. The analysis was completed as part of an introductory R course.

## Summary of Gapminder Data
```{r, results='asis', echo=FALSE}
median_gdp <- median(gapminder$gdpPercap)
gapminder %>%
select(-country) %>%
mutate(gdpPercap = ifelse(gdpPercap > median_gdp, "High", "Low")) %>%
mutate(gdpPercap = factor(gdpPercap)) %>%
mutate(pop = pop/1000000) -> gapminder2


gapminder2 <- lapply(gapminder2, function(x) x[sample(c(TRUE, NA),
                                                prob = c(0.9, 0.1),
                                                 size = length(x),
                                                 replace = TRUE
)])

summary_table_labels <- list(lifeExp = "Life Expectancy", pop = "Population (in millions)", gdpPercap = "GDP Per Capita", year = "Year")

summary_table<- tableby(continent ~ ., data = gapminder2)
summary_table2<-summary(summary_table, text = TRUE, labelTranslations = summary_table_labels, title = "Gapminder Dataset")

summary_table2 %>%
  kable(caption = "Gapminder Summary Statistics") %>%
  kable_styling(font_size = 10)
```
## Trends in Life Expectancy, Population, and GDP 
The Gapminder dataset include three variables that can be used to assess the well-being of countries: life expectancy, population, and gross domestic product (GDP) per capita.

### Life Expectancy
```{r, echo=FALSE, include=FALSE}
coef(lm(lifeExp ~ year, data = gapminder))
```

```{r, echo=FALSE}
gapminder%>%
  ggplot(aes(x = year, y = lifeExp))+
  geom_point(colour="#000099")+ 
  scale_x_continuous(breaks = seq(1950, 2010, 5))+
  scale_y_continuous(breaks = seq(10, 100, 10))+
  geom_abline(intercept = -585.6521874, slope = 0.3259038)+
  ggtitle("Country Life Expectancy\n1950-2017")+
  xlab("Year")+
  ylab("Life Expectancy")+
  theme(plot.title = element_text(hjust = 0.5))
```

#### Identifying and Exploring Outliers in Life Expectancy
The preceding graph highlights two interesting outliers in life expectancy: Cambodian life expectancy in 1977 and Rwandan life expectancy in 1992.

```{r, echo=FALSE}
gapminder %>% 
  filter(year>1975 & year<1995 & lifeExp<35) %>% 
  kable() %>% 
  kable_styling(font_size = 10)

```

```{r, echo=FALSE}
Rwanda <- gapminder %>% filter(country == 'Rwanda')
rwanda_plot<-ggplot(data = Rwanda, aes(x = year, y = lifeExp)) +
  geom_line(color = "deeppink4", size=1) +
  xlab("Year") + 
  ylab("Life Expectancy") +
  ggtitle("Life Expectancy in Rwanda") +
  theme(plot.title = element_text(hjust = 0.5))

Cambodia <- gapminder %>% filter(country == 'Cambodia')
cambodia_plot<-ggplot(data = Cambodia, aes(x = year, y = lifeExp)) +
  geom_line(color = "deeppink4", size=1) +
  xlab("Year") + 
  ylab("Life Expectancy") +
  ggtitle("Life Expectancy in Cambodia") +
  theme(plot.title = element_text(hjust = 0.5))
```

```{r, figures-side, fig.show="hold", out.width="50%"}
cambodia_plot
rwanda_plot
```

The genocide committed under the Khmer Rouge regime (1951-1999) in Cambodia and the impact of political violence and the HIV/AIDS epidemic in Rwanda both had devastating impacts on their repsective ocuntires' life expectancies.

Finally, life expectancy can be compared over time by continent. 

```{r, echo=FALSE, include=FALSE}
gapminder %>% 
  ggplot(aes(year, lifeExp, colour = continent)) + 
  geom_smooth() + 
  ggtitle("Life Expectancy over Time by Continent\n1950-2017")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year") + 
  ylab("Life Expectancy")+
  labs(color='Continent')
```

### Population
Population growth over time varies greatly by both continent and country. 

```{r, echo=FALSE, include=FALSE}
gapminder %>% 
  ggplot(aes(year, pop/1000000, color = continent))+ 
  geom_smooth()+ 
  ggtitle("Population by Continent\n1950-2017")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(color='Continent')+
  xlab("Year")+ 
  ylab("Population in Millions")
```

```{r, echo=FALSE}
ggplot(data = gapminder, 
       aes(x = year, y = pop/1000000, 
           group = country, 
           color = continent))+
  geom_line()+
  scale_x_continuous(breaks = seq(1950, 2010, 15))+
  xlab("Year")+ 
  ylab("Population in Millions")+
  ggtitle("Country Population Trends by Continent")+
  theme_gray(base_size = 10)+
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(.94, .03),
    legend.justification = c("right", "bottom"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6))+
  labs(color='Continent')+
  facet_rep_wrap(~continent, repeat.tick.labels=TRUE, scales="fixed")+
  theme(panel.spacing.x = unit(1, "lines"))+
  theme(panel.spacing.y = unit(1, "lines"))
```


### GDP
Gross domestic product is the most common (albeit a crude) measure of a country's economic well-being 

```{r, echo=FALSE}
gapminder %>% 
  ggplot(aes(year,gdpPercap, colour = continent))+ 
  geom_smooth()+ 
  ggtitle("GDP per Capita by Continent (US$)")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(color='Continent')+
  xlab("Year")+ 
  ylab("Mean GDP per Capita")
```

```{r}
life_exp_gdp <- gapminder %>% 
  mutate(gdpPercap_ordinal = 
    case_when(
      gdpPercap <  800 ~ "Low (<800)",
      gdpPercap >= 800 & gdpPercap < 1000 ~ "Moderate (800-1000)",
      TRUE ~ "High (>1000)" )) %>%
  ggplot(aes(x = year, y = lifeExp, col=gdpPercap_ordinal))+
  geom_point()+ 
  ggtitle("Country Life Expectancy\n1950-2017")+
  xlab("Year")+
  ylab("Life Expectancy")+
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(color='GDP per Capita (US$)')+
  scale_color_manual(values=c("darkgreen", "red", "yellow"))

fig <- ggplotly(life_exp_gdp)
life_exp_gdp
```

There is a strong correlation between a country's GDP per capita and life expetancy. Countries with higher average GDP's per capita also usually have higher life expectancy rates.

```{r}
plotly_a <- ggplot(gapminder, aes(
  x = gdpPercap, y = lifeExp,
  color = continent, size = pop, ids = country
)) +
  geom_point(alpha = 0.5) +
  ggtitle("Life Expectancy versus GDP, 2007") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("GDP per Capita") +
  ylab("Life Expectancy") +
  scale_color_discrete(name = "Continent") +
  scale_size_continuous(name = " ")
ggplotly(plotly_a)
```

The interactive graph above plots each country's life expectancy and GDP per capita for the most recently-available data in the dataset (2007).
