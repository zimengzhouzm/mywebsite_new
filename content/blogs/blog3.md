---
categories:
- ""
- ""
date: "2017-10-31T22:26:13-05:00"
description: Nullam et orci eu lorem consequat tincidunt vivamus et sagittis magna sed nunc rhoncus condimentum sem. In efficitur ligula tate urna. Maecenas massa sed magna lacinia magna pellentesque lorem ipsum dolor. Nullam et orci eu lorem consequat tincidunt. Vivamus et sagittis tempus.
draft: false
image: pic08.jpg
keywords: ""
slug: tempus
title: Tempus
---

```{r, setup, include=FALSE}
knitr::opts_chunk$set(
  message = FALSE, 
  warning = FALSE, 
  tidy=FALSE,     # display code as typed
  size="small")   # slightly smaller font for code
options(digits = 3)

# default figure size
knitr::opts_chunk$set(
  fig.width=6.75, 
  fig.height=6.75,
  fig.align = "center"
)
```


```{r load-libraries, include=FALSE}
library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(mosaic)
library(ggthemes)
library(lubridate)
library(here)
library(skimr)
library(janitor)
library(httr)
library(readxl)
library(vroom)
```

# Trump's Approval Margins

As we saw in class, fivethirtyeight.com has detailed data on [all polls that track the president's approval ](https://projects.fivethirtyeight.com/trump-approval-ratings)

```{r, cache=TRUE,warnings= FALSE, message=FALSE}
# Import approval polls data
approval_polllist <- read_csv(here::here('data', 'approval_polllist.csv'))

# or directly off fivethirtyeight website
# approval_polllist <- read_csv('https://projects.fivethirtyeight.com/trump-approval-data/approval_polllist.csv') 

glimpse(approval_polllist)

approval_polllist<-approval_polllist %>% 
  mutate(end_date1 = mdy(enddate))

# Use `lubridate` to fix dates, as they are given as characters.
```

## Create a plot

What I would like you to do is to calculate the average net approval rate (approve- disapprove) for each week since he got into office. I want you plot the net approval, along with its 95% confidence interval. There are various dates given for each poll, please use `enddate`, i.e., the date the poll ended.


You can facet by year, and add an orange line at zero. Your plot should look like this:

```{r trump_margins, echo=FALSE, out.width="100%",warnings= FALSE, message=FALSE}
knitr::include_graphics(here::here("images", "trump_approval_margin.png"), error = FALSE)
```
```{r}
library(sjstats)
approval_polllist<-approval_polllist %>% 
  #filter(subgroup == "Voters") %>% 
  mutate(net_approve=approve-disapprove,#used adjusted data
         year=year(end_date1),
         enddate_week=week(end_date1))


CI_approval<-approval_polllist %>%
  group_by(year,enddate_week) %>%
  filter (!is.na(net_approve)) %>% 
  filter (enddate_week!="3") %>% 
  summarise(mean_net_approve = mean(net_approve, na.rm = TRUE),
  sd_net_approve = sd(net_approve, na.rm = TRUE),
  count=n(),
  se_net_approve = parameters::standard_error(net_approve),
  net_approve_low = mean_net_approve - se_net_approve*qt(0.95, count-1),
  net_approve_high = mean_net_approve + se_net_approve*qt(0.95, count-1))

cbp1 <- c("2017"="#FF6699","2018" ="#99CC66","2019"= "#66CCCC","2020"= "violet")
#cutoff <- data.frame( x = c(-Inf, Inf), y = 0, cutoff = factor(0))

ggplot(CI_approval,aes(x=enddate_week, y = mean_net_approve,color=as.factor(year)))+
  geom_point(size=1)+
  geom_line()+
  geom_hline(aes(yintercept=0), color="orange")+
    #annotate("text", min(CI_approval$enddate_week), 0, vjust = -1, label = "Cutoff")+
 # geom_line(aes( x, y, linetype = cutoff ), cutoff)+
  labs (
    title = "Estimating Net Approval (approve-disapprove) for Donald Trump",
    subtitle="weekly average of all polls",
    x= 'Week of the year', y = "Average Net Approval(%)")+
  scale_x_continuous(breaks = c(13,26,39,52))+
  scale_y_continuous(breaks = c(-20,-17.5,-15,-12.5,-10,-7.5,-5,-2.5,0,2.5,5,7.5))+
  facet_wrap(~year)+
  scale_fill_manual(values = cbp1,guide = FALSE)+
  scale_color_manual(values = cbp1,guide = FALSE)+
  geom_ribbon(aes(ymax = net_approve_high, ymin = net_approve_low,fill=factor(year)),alpha=0.3)+
  coord_fixed(ratio = 0.9)+
  theme(axis.text.x = element_text(size = 7, margin = margin(0,15,0,0)))+
  theme(axis.text.y = element_text(size = 7, margin = margin(15,0,0,0)))+
  NULL
```


## Compare Confidence Intervals

Compare the confidence intervals for `week 15` (6-12 April 2020) and `week 34` (17-23 August 2020). Can you explain what's going on? One paragraph would be enough.
```{r Compare Confidence Intervals, warnings= FALSE, message=FALSE}
CI_week15<-approval_polllist %>%
  filter(year=="2020") %>%
  filter(enddate_week=="15") %>%
  summarise(mean_net_approve = mean(net_approve),
  sd_net_approve = sd(net_approve),
  count=n(),
  se_net_approve = parameters::standard_error(net_approve),
  net_approve_low = mean_net_approve - se_net_approve*qt(0.95, count-1),
  net_approve_high = mean_net_approve + se_net_approve*qt(0.95, count-1))
CI_week34<-approval_polllist %>%
  filter(year=="2020") %>%
  filter(enddate_week=="34") %>%
  summarise(mean_net_approve = mean(net_approve),
  sd_net_approve = sd(net_approve),
  count=n(),
  se_net_approve = parameters::standard_error(net_approve),
  net_approve_low = mean_net_approve - se_net_approve*qt(0.95, count-1),
  net_approve_high = mean_net_approve + se_net_approve*qt(0.95, count-1))
CI_week15
CI_week34
```

In the Week 34(17-23 August 2020), both the lower bound and higher bound of 95% Confidence Interval of the net approval of Trump declined from Week 15 (6-12 April 2020). This suggested that the approval rate of Trump declined over the 4 months. Also, it is noticeable that the width of the Confidence Interval increased over the period, which suggested that the estimated net approval rates was more difficult to estimate. This is consistent with the increase in standard deviation, which also demonstrated that the net approval rate in the sample is more divided. So generally speaking, as the election date coming closer, the approval rates from different pollsters became more different,and it is harder to estimate the result.
