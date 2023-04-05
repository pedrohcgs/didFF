#---------------------------------------------------
#      Empirical Exercise
#      Effect of Minimum Wage on Teen Employments
#      Codes based on Callaway and Sant'Anna (2021)
#---------------------------------------------------
#-----------------------------------------------------------------------------
# Load packages
#-----------------------------------------------------------------------------
# Libraries
# Load libraries
library(ggplot2)
library(here)
library(ggthemes)
library(patchwork)
library(ggtext)
library(foreign)
library(tidyverse)
library(dplyr)
## Load external packages
library(foreign)
library(remotes)
#install_github("bcallaway11/BMisc")
library(BMisc)
library(did)
library(gridExtra)
library(knitr)
library(ggpubr)


library(bacondecomp) 
library(TwoWayFEWeights)
library(fixest)
library(glue)
library(plm)
#---------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------
# Load data
min_wage <- readRDS((here("data",'min_wage_CS.rds')))

# If year of adoption is missing, set it to Infinity ( zero or any large number  would also work with the did package)
min_wage$first.treat[(min_wage$first.treat)==0] <- Inf
# Create treatment dummy - 1 if treated by that year, 0 otherwise
min_wage$treated <- as.numeric(min_wage$year >= min_wage$first.treat)
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
# ggplot2 theme
# Set ggplot theme
theme_set(
  #theme_clean() + 
  theme_classic() +
    theme(
      panel.background = element_rect(fill = "transparent"), # bg of the panel
      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
      legend.background = element_rect(color = "white"),
      legend.box.background = element_rect(fill = "transparent"), # get rid of legend panel bg
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      panel.spacing = unit(10, "lines"))
)
#--------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
# Start the analysis
#---------------------------------------------------------------------------------------
# Get TWFE coefficient
twfe_unc <- fixest::feols(lemp ~ treated| first.treat + year, 
                      data = min_wage,
                      #weights = ~W, 
                      cluster = ~countyreal)

summary(twfe_unc)
#---------------------------------------------------------------------------------------
# Get TWFE coefficient with covariates
twfe_cond<- fixest::feols(lemp ~ treated +
                            region + (medinc + pop ) + I(pop^2) + I(medinc^2)  + white + hs  + pov| first.treat + year, 
                          data = min_wage,
                          #weights = ~W, 
                          cluster = ~countyreal)

summary(twfe_cond)
#---------------------------------------------------------------------------------------
# Get regressions with region by year FE
min_wage$regionyear <- as.factor(paste(min_wage$region, min_wage$year, sep="-"))

reg_regionyear <- plm(lemp ~ as.factor(regionyear) + treated, 
              data=min_wage,
              index=c("countyreal"), 
              model="within")
reg_regionyear_coeff <- tail(coef(reg_regionyear),1)
reg_regionyear_se <- tail(sqrt(diag(vcovHC(reg_regionyear))),1)
#---------------------------------------------------------------------------------------
# Get Bacon decomposition (without weights bc not supported in the R package)
# df_bacon <- bacon(lemp ~ treated,
#                   data = min_wage,
#                   id_var = "countyreal",
#                   time_var = "year")
#---------------------------------------------------------------------------------------
# # Get de Chaisemartin and D'Haultfoeuille Decomposition
# dCDH_decomp <- twowayfeweights(
#   df = min_wage, 
#   Y = "lemp", 
#   G = "first.treat",
#   T = "year", 
#   D ="treated",
#   cmd_type =  "feTR"
# )
# 
# # Weakly Positive weights
# dCDH_positive <- sum(dCDH_decomp$weight[dCDH_decomp$weight>=0])
# 
# # Negative weights
# dCDH_negative <- sum(dCDH_decomp$weight[dCDH_decomp$weight<0])
#---------------------------------------------------------------------------------------
# Get TWFE event study coefficients

# create event times (rel_year)
min_wage <- min_wage %>% 
  mutate(
    rel_year = year - first.treat
  )

# Create min_year to omit it in the TWFE regression
min_year <- min(min_wage$rel_year)
# Formula we will use
formula_twfe_es_unc <- as.formula(glue::glue("lemp ~ i(rel_year, ref=c(-1, {min_year})) | first.treat + year"))


# estimate the TWFE coefficients
twfe_es_unc <- fixest::feols(formula_twfe_es_unc, data = min_wage, cluster = ~countyreal)
summary(twfe_es_unc)
# Put the TWFE coefficients in a tibble that is easy to plot
twfe_es_unc <- broom::tidy(twfe_es_unc) %>%
  filter(str_detect(term, "rel_year::")) %>% 
  mutate(
    rel_year = as.numeric(str_remove(term, "rel_year::")),
  ) %>%
  filter(rel_year <= 5 & rel_year >= -5) %>%
  select(event.time = rel_year, 
         estimate,
         std.error) %>%
  add_row(event.time = -1, 
          estimate = 0,
          std.error =0)  %>%
  mutate( point.conf.low  = estimate - stats::qnorm(1 - .05/2) * std.error,
          point.conf.high = estimate + stats::qnorm(1 - .05/2) * std.error)


twfe_es_unc
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
# TWFE with covariates

# Formula we will use
formula_twfe_es_cond <- as.formula(glue::glue("lemp ~ i(rel_year, ref=c(-1, {min_year})) +
                                              + region + (medinc + pop ) + I(pop^2) + I(medinc^2)  + 
                                              white + hs  + pov| first.treat + year"))


# estimate the TWFE coefficients
twfe_es_cond <- fixest::feols(formula_twfe_es_cond, data = min_wage, cluster = ~countyreal)
summary(twfe_es_cond)
# Put the TWFE coefficients in a tibble that is easy to plot
twfe_es_cond <- broom::tidy(twfe_es_cond) %>%
  filter(str_detect(term, "rel_year::")) %>% 
  mutate(
    rel_year = as.numeric(str_remove(term, "rel_year::")),
  ) %>%
  filter(rel_year <= 5 & rel_year >= -5) %>%
  select(event.time = rel_year, 
         estimate,
         std.error) %>%
  add_row(event.time = -1, 
          estimate = 0,
          std.error =0)  %>%
  mutate( point.conf.low  = estimate - stats::qnorm(1 - .05/2) * std.error,
          point.conf.high = estimate + stats::qnorm(1 - .05/2) * std.error)


twfe_es_cond



#---------------------------------------------------------------------------------------
# Region-by-year Event study

# Formula we will use
formula_reg_es_regionyear <- as.formula(glue::glue("lemp ~ i(rel_year, ref=c(-1, {min_year})) +  as.factor(regionyear)"))

reg_es_regionyear <- plm(as.formula(glue::glue("lemp ~ i(rel_year, ref=c(-1, {min_year})) + 
                                                (regionyear)")), 
                      data=min_wage,
                      index=c("countyreal"), 
                      model="within")
summary(reg_es_regionyear)
# Put the TWFE coefficients in a tibble that is easy to plot
reg_es_regionyear <- broom::tidy(reg_es_regionyear)[1:9,] %>%
  mutate(
    rel_year = c(-6:-2, 0:3),
  ) %>%
  filter(rel_year <= 5 & rel_year >= -5) %>%
  select(event.time = rel_year, 
         estimate,
         std.error) %>%
  add_row(event.time = -1, 
          estimate = 0,
          std.error =0)  %>%
  mutate( point.conf.low  = estimate - stats::qnorm(1 - .05/2) * std.error,
          point.conf.high = estimate + stats::qnorm(1 - .05/2) * std.error)


reg_es_regionyear

#--------------------------------------------------------------------------------------------
# Callaway and Sant'Anna (2021) procedure
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
# Formula for covarites
xformla <- ~ region + (medinc + pop ) + I(pop^2) + I(medinc^2)  + white + hs  + pov
#--------------------------------------------------------------------------------------------
# Using not-yet treated
CS_ny_unc <- did::att_gt(yname="lemp",
                   tname="year",
                   idname="countyreal",
                   gname="first.treat",
                   xformla=~1,
                   #xformla = xformla,
                   control_group="notyettreated",
                   data=min_wage,
                   #anticipation = 1,
                   base_period = "universal",
                   panel=TRUE,
                   bstrap=TRUE,
                   cband=TRUE)

summary(CS_ny_unc)
ggdid(CS_ny_unc, ncol = 3, title = "DiD based on unconditional PTA and using not-yet-treated as comparison group ")
CS_es_ny_unc <- aggte(CS_ny_unc, type = "dynamic", min_e = -5, max_e = 5)
summary(CS_es_ny_unc)
ggdid(CS_es_ny_unc,  title = "Event-study aggregation \n DiD based on unconditional PTA and using not-yet-treated as comparison group ")


# Conditional on covaraites and DR
CS_ny_cond <- did::att_gt(yname="lemp",
                        tname="year",
                        idname="countyreal",
                        gname="first.treat",
                        #xformla=~1,
                        xformla = xformla,
                        control_group="notyettreated",
                        est_method = "dr",
                        data=min_wage,
                        #anticipation = 1,
                        base_period = "universal",
                        panel=TRUE,
                        bstrap=TRUE,
                        cband=TRUE)
summary(CS_ny_cond)
ggdid(CS_ny_cond, ncol = 3, title = "DiD based on conditional PTA and using not-yet-treated as comparison group ")

CS_es_ny_cond <- aggte(CS_ny_cond, type = "dynamic", min_e = -5, max_e = 5)
summary(CS_es_ny_cond)
ggdid(CS_es_ny_cond,  title = "Event-study aggregation \n DiD based on conditional PTA and using not-yet-treated as comparison group ")

#--------------------------------------------------------------------------------------------
# Using never treated
CS_never_unc <- did::att_gt(yname="lemp",
                         tname="year",
                         idname="countyreal",
                         gname="first.treat",
                         xformla=~1,
                         #anticipation = 1,
                         #base_period = "universal",
                         #xformla = xformla,
                         control_group="nevertreated",
                         data=min_wage,
                         #anticipation = 1,
                         base_period = "universal",
                         panel=TRUE,
                         bstrap=TRUE,
                         cband=TRUE)
summary(CS_never_unc)
ggdid(CS_never_unc, ncol = 3, title = "DiD based on unconditional PTA and using never-treated as comparison group ")
CS_es_never_unc <- aggte(CS_never_unc, type = "dynamic", min_e = -5, max_e = 5)
summary(CS_es_never_unc)
ggdid(CS_es_never_unc,  title = "Event-study aggregation \n DiD based on unconditional PTA and using never-treated as comparison group ")



# Using covariates and DR
CS_never_cond <- did::att_gt(yname="lemp",
                          tname="year",
                          idname="countyreal",
                          gname="first.treat",
                          #xformla=~1,
                          xformla = xformla,
                          control_group="nevertreated",
                          data = min_wage,
                          #anticipation = 1,
                          base_period = "universal",
                          panel = TRUE,
                          bstrap = TRUE,
                          cband = TRUE)
summary(CS_never_cond)
ggdid(CS_never_cond, ncol = 3, title = "DiD based on conditional PTA and using never-treated as comparison group ")
CS_es_never_cond <- aggte(CS_never_cond, type = "dynamic", min_e = -5, max_e = 5)
summary(CS_es_never_cond)
ggdid(CS_es_never_cond,
      title = "Event-study aggregation \n DiD based on conditional PTA and using never-treated as comparison group ")

#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
# Optional customization
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
# Select not yet-treated subgroups

CS_ny_unc1 <- tidy(CS_ny_unc) %>%
  filter( group == 2004)

CS_ny_cond1 <- tidy(CS_ny_cond) %>%
  filter( group == 2004)


CS_ny_unc2 <- tidy(CS_ny_unc) %>%
  filter( group == 2006)

CS_ny_cond2 <- tidy(CS_ny_cond) %>%
  filter( group == 2006)


CS_ny_unc3 <- tidy(CS_ny_unc) %>%
  filter( group == 2007)

CS_ny_cond3 <- tidy(CS_ny_cond) %>%
  filter( group == 2007)

CS_never_cond_es <- tidy(CS_es_never_cond)
CS_ny_cond_es <- tidy(CS_es_ny_cond)
#--------------------------------------------------------------------------------------------
# Customize graphs
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
# Group = 2004
CS_ny_unc1<-  ggplot(data = CS_ny_unc1,
                     mapping = aes(x = time, y = estimate)) +
  geom_line(size = 0.5, alpha = 2, colour = "black") +
  geom_vline(xintercept = 2004-0.1, color = 'grey', size = 1.2, linetype = "dotted") + 
  geom_hline(yintercept = 0, colour="black",  linetype = "dotted")+
  
  
  
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), show.legend = FALSE, linetype= 1, size = 1.1,
                  color = "red")+
  geom_pointrange(aes(ymin = point.conf.low, ymax = point.conf.high), show.legend = FALSE, size = 1.1)+
  xlab("Year") +
  ylab("ATT(g,t) Estimate") +
  ylim(-.22,.1)+
  theme(axis.text.y = element_text(size = 9))+
  theme(axis.text.x = element_text(size = 9)) +
  theme(axis.title = element_text(color="black",  size = 9))+
  theme(plot.title=ggtext::element_markdown(size=9,
                                            #face = "bold",
                                            color="black",
                                            hjust=0,
                                            lineheight=1.2)
  )+
  ggtitle("Group 2004: DiD based on unconditional PTA")


CS_ny_unc1

CS_ny_cond1<-  ggplot(data = CS_ny_cond1,
                      mapping = aes(x = time, y = estimate)) +
  geom_line(size = 0.5, alpha = 2, colour = "black") +
  geom_vline(xintercept = 2004-0.1, color = 'grey', size = 1.2, linetype = "dotted") + 
  geom_hline(yintercept = 0, colour="black",  linetype = "dotted")+
  
  
  
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), show.legend = FALSE, linetype= 1, size = 1.1,
                  color = "red")+
  geom_pointrange(aes(ymin = point.conf.low, ymax = point.conf.high), show.legend = FALSE, size = 1.1)+
  xlab("Year") +
  ylab("ATT(g,t) Estimate") +
  ylim(-.22,.1)+
  theme(axis.text.y = element_text(size = 9))+
  theme(axis.text.x = element_text(size = 9)) +
  theme(axis.title = element_text(color="black",  size = 9))+
  theme(plot.title=ggtext::element_markdown(size=9,
                                            #face = "bold",
                                            color="black",
                                            hjust=0,
                                            lineheight=1.2)
  )+
  ggtitle("Group 2004: DiD based on conditional PTA")



CS_ny_cond1

#--------------------------------------------------------------------------------------------
# Group = 2006
CS_ny_unc2<-  ggplot(data = CS_ny_unc2,
                     mapping = aes(x = time, y = estimate)) +
  geom_line(size = 0.5, alpha = 2, colour = "black") +
  geom_vline(xintercept = 2006-0.1, color = 'grey', size = 1.2, linetype = "dotted") + 
  geom_hline(yintercept = 0, colour="black",  linetype = "dotted")+
  
  
  
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), show.legend = FALSE, linetype= 1, size = 1.1,
                  color = "red")+
  geom_pointrange(aes(ymin = point.conf.low, ymax = point.conf.high), show.legend = FALSE, size = 1.1)+
  xlab("Year") +
  ylab("ATT(g,t) Estimate") +
  ylim(-.22,.1)+
  theme(axis.text.y = element_text(size = 9))+
  theme(axis.text.x = element_text(size = 9)) +
  theme(axis.title = element_text(color="black",  size = 9))+
  theme(plot.title=ggtext::element_markdown(size=9,
                                            #face = "bold",
                                            color="black",
                                            hjust=0,
                                            lineheight=1.2)
  )+
  ggtitle("Group 2006: DiD based on unconditional PTA")


CS_ny_unc2

CS_ny_cond2<-  ggplot(data = CS_ny_cond2,
                      mapping = aes(x = time, y = estimate)) +
  geom_line(size = 0.5, alpha = 2, colour = "black") +
  geom_vline(xintercept = 2006-0.1, color = 'grey', size = 1.2, linetype = "dotted") + 
  geom_hline(yintercept = 0, colour="black",  linetype = "dotted")+
  
  
  
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), show.legend = FALSE, linetype= 1, size = 1.1,
                  color = "red")+
  geom_pointrange(aes(ymin = point.conf.low, ymax = point.conf.high), show.legend = FALSE, size = 1.1)+
  xlab("Year") +
  ylab("ATT(g,t) Estimate") +
  ylim(-.22,.1)+
  theme(axis.text.y = element_text(size = 9))+
  theme(axis.text.x = element_text(size = 9)) +
  theme(axis.title = element_text(color="black",  size = 9))+
  theme(plot.title=ggtext::element_markdown(size=9,
                                            #face = "bold",
                                            color="black",
                                            hjust=0,
                                            lineheight=1.2)
  )+
  ggtitle("Group 2006: DiD based on conditional PTA")



CS_ny_cond2

#--------------------------------------------------------------------------------------------
# Group = 2007
CS_ny_unc3<-  ggplot(data = CS_ny_unc3,
                     mapping = aes(x = time, y = estimate)) +
  geom_line(size = 0.5, alpha = 2, colour = "black") +
  geom_vline(xintercept = 2007-0.1, color = 'grey', size = 1.2, linetype = "dotted") + 
  geom_hline(yintercept = 0, colour="black",  linetype = "dotted")+
  
  
  
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), show.legend = FALSE, linetype= 1, size = 1.1,
                  color = "red")+
  geom_pointrange(aes(ymin = point.conf.low, ymax = point.conf.high), show.legend = FALSE, size = 1.1)+
  xlab("Year") +
  ylab("ATT(g,t) Estimate") +
  ylim(-.22,.1)+
  theme(axis.text.y = element_text(size = 9))+
  theme(axis.text.x = element_text(size = 9)) +
  theme(axis.title = element_text(color="black",  size = 9))+
  theme(plot.title=ggtext::element_markdown(size=9,
                                            #face = "bold",
                                            color="black",
                                            hjust=0,
                                            lineheight=1.2)
  )+
  ggtitle("Group 2007: DiD based on unconditional PTA")


CS_ny_unc3

CS_ny_cond3<-  ggplot(data = CS_ny_cond3,
                      mapping = aes(x = time, y = estimate)) +
  geom_line(size = 0.5, alpha = 2, colour = "black") +
  geom_vline(xintercept = 2007-0.1, color = 'grey', size = 1.2, linetype = "dotted") + 
  geom_hline(yintercept = 0, colour="black",  linetype = "dotted")+
  
  
  
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), show.legend = FALSE, linetype= 1, size = 1.1,
                  color = "red")+
  geom_pointrange(aes(ymin = point.conf.low, ymax = point.conf.high), show.legend = FALSE, size = 1.1)+
  xlab("Year") +
  ylab("ATT(g,t) Estimate") +
  ylim(-.22,.1)+
  theme(axis.text.y = element_text(size = 9))+
  theme(axis.text.x = element_text(size = 9)) +
  theme(axis.title = element_text(color="black",  size = 9))+
  theme(plot.title=ggtext::element_markdown(size=9,
                                            #face = "bold",
                                            color="black",
                                            hjust=0,
                                            lineheight=1.2)
  )+
  ggtitle("Group 2007: DiD based on conditional PTA")



CS_ny_cond3

#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
# Combine these groups
fig_CS <- (CS_ny_unc1 + CS_ny_unc2 + CS_ny_unc3)/ (CS_ny_cond1 + CS_ny_cond2 + CS_ny_cond3)

# Save plots
ggsave(here("plots","CS_attgt_ny.pdf"),
       fig_CS,  
       dpi = 500,
       width = 14, 
       height = 7)
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
# Event-study plots
CS_ny_cond_es<-  ggplot(data = CS_ny_cond_es,
                      mapping = aes(x = event.time, y = estimate)) +
  geom_line(size = 0.5, alpha = 2, colour = "black") +
  geom_vline(xintercept = 0-0.1, color = 'grey', size = 1.2, linetype = "dotted") + 
  geom_hline(yintercept = 0, colour="black",  linetype = "dotted")+

  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), show.legend = FALSE, linetype= 1, size = 1.1,
                  color = "red")+
  geom_pointrange(aes(ymin = point.conf.low, ymax = point.conf.high), show.legend = FALSE, size = 1.1)+
  xlab("Year") +
  ylab("ATT(g,t) Estimate") +
  #ylim(-.22,.1)+
  theme(axis.text.y = element_text(size = 9))+
  theme(axis.text.x = element_text(size = 9)) +
  theme(axis.title = element_text(color="black",  size = 9))+
  theme(plot.title=ggtext::element_markdown(size=9,
                                            #face = "bold",
                                            color="black",
                                            hjust=0,
                                            lineheight=1.2)
  )+
  ggtitle("Event-study plot:\n DiD based on conditional PTA and using not-yet-treated as comparison group")


CS_ny_cond_es 

# Save plots
ggsave(here("plots","CS_es_ny.pdf"),
       CS_ny_cond_es,  
       dpi = 500,
       width = 14, 
       height = 7)

# Event-study plots
CS_never_cond_es<-  ggplot(data = CS_never_cond_es,
                        mapping = aes(x = event.time, y = estimate)) +
  geom_line(size = 0.5, alpha = 2, colour = "black") +
  geom_vline(xintercept = 0-0.1, color = 'grey', size = 1.2, linetype = "dotted") + 
  geom_hline(yintercept = 0, colour="black",  linetype = "dotted")+
  
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), show.legend = FALSE, linetype= 1, size = 1.1,
                  color = "red")+
  geom_pointrange(aes(ymin = point.conf.low, ymax = point.conf.high), show.legend = FALSE, size = 1.1)+
  xlab("Year") +
  ylab("ATT(g,t) Estimate") +
  #ylim(-.22,.1)+
  theme(axis.text.y = element_text(size = 9))+
  theme(axis.text.x = element_text(size = 9)) +
  theme(axis.title = element_text(color="black",  size = 9))+
  theme(plot.title=ggtext::element_markdown(size=9,
                                            #face = "bold",
                                            color="black",
                                            hjust=0,
                                            lineheight=1.2)
  )+
  ggtitle("Event-study plot:\n DiD based on conditional PTA and using nevert-treated as comparison group")


CS_never_cond_es 

# Save plots
ggsave(here("plots","CS_es_never.pdf"),
       CS_never_cond_es,  
       dpi = 500,
       width = 14, 
       height = 7)
