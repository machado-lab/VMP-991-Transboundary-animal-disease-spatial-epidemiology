library(tidyverse);
library(INLAOutputs)
library(sf); library(ggsn); library(gridExtra)
# install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)
library(stringr); library(spdep)
library(INLA)
library(viridis)

rm(list = ls())
cat("\014")
## Data preparation ------------------------------------------------------------

load("Q:/My Drive/GitHub/VMP_991_Transboundary-animal-disease-spatial-epidemiology/data/DR_comb.RData")

## Data for RR 
ASF <- Po_comb %>%
  group_by(year) %>%
  mutate(grid = str_to_title(id),
         expected_cases = numcontrols+numcase * sum(numcase) / sum(numcontrols+numcase),
         incidence_cases = numcase / numcontrols+numcase * 1000,
         rr_cases = numcontrols+numcase / expected_cases,
         cases2 = numcase + 1) %>%
  ungroup() %>%
  mutate(id_cell = as.numeric(as.factor(id)),
         id_cell2 = as.numeric(as.factor(id)),
         id_cell3 = as.numeric(as.factor(id)),
         id_year = year - min(year) + 1,
         id_year2 = year - min(year) + 1,
         id_year3 = year - min(year) + 1,
         id_cell_year = 1:n())


ASF<-mutate_if(ASF, is.numeric, ~replace(., is.na(.), 0))

dr_y <- Po_comb %>%
  group_by(year) %>%
  summarise(cases = sum(numcase),
            population = sum(numcontrols+numcase),
            incidence_cases = cases / population * 1000)


## Spatial data
br_nbp <- poly2nb(as(Po_comb, "Spatial"))
nb2INLA("./data/br_adjp", br_nbp)
br_adjp <- "./data/br_adjp"

## Model components

# Priors
prec <- list(prec = list(prior = "pc.prec", param = c(.3/.31, .01)))
prec_bym2 <- list(phi = list(prior = "pc", param = c(.5, 2/3)),
                  prec = list(prior = "pc.prec", param = c(.3/.31, .01)))

# Linear combination for spatio temporal interactions
lcs <- inla.make.lincombs(id_year = diag(length(unique(ASF$id_year))),
                          id_year2 = diag(length(unique(ASF$id_year2))))


## Exploratory analysis --------------------------------------------------------

## Average RR
ASF$id <- as.factor(ASF$id)
rr_cell <- aggregate(ASF$rr_cases, list(ASF$id), mean)
names(rr_cell) <- c("id", "rr_cases")

Po_comb$id<-as.factor(Po_comb$id)

rr_states <- Po_comb %>%
  select(id) %>%
  left_join(rr_cell, by = "id")


rr_states$rr_cases <- cut(rr_states$rr_cases, 
                breaks=c(0,0.05,0.1,0.25,10,20,30,78), 
                labels=c("0-0.05","0.05-0.1","0.1-0.25","0.25-10","10-20","20-30","30"),
                include.lowest=TRUE)

## cathegorical RR plot
rr_states%>%
  filter(!rr_cases=="0-0.05")%>%
  ggplot(aes(fill = rr_cases)) +
  geom_sf(size = .05,color=NA) +
  #geom_sf(data=basin, color="grey", fill=NA)+
  #xlim(c(18, 50)) + ylim(c(35, 54))+
  #facet_wrap(~ tm_s_ID) +
  #transition_manual(Date) +
  labs(title = "ASF RR")  +
  scale_fill_viridis(option = "viridis",
                     alpha = 0.85, 
                     discrete = T, 
                     direction = -1, 
                     guide = guide_legend(
                       direction = "top",
                       title.position = "bottom",
                       title.hjust =0.5),name="RR") +
  theme(axis.text = element_blank()
        ,axis.title = element_blank()
        ,axis.ticks = element_blank()
        ,axis.line=element_blank()
        ,legend.title = element_blank()
        ,legend.text = element_text(size = 14)
        ,legend.key.width = unit(0.35,"cm")
        ,legend.key.height = unit(0.5,"cm")
        ,plot.title = element_text(size= 22)
        ,plot.subtitle=element_text(size=18)
        ,legend.position = "right"
        ,plot.caption = element_text()
        ,legend.background = element_blank()
        ,panel.background = element_blank()
        ,legend.spacing.x = unit(0.25, 'cm'))

# ggsave("./fig/rr_total_poland_rr_Class.tiff", plot = last_plot(),
#        dpi = 300, width = 320, height = 300, units = "mm")

# continues
ASF$id <- as.factor(ASF$id)
rr_cell <- aggregate(ASF$rr_cases, list(ASF$id), mean)
names(rr_cell) <- c("id", "rr_cases")

Po_comb$id<-as.factor(Po_comb$id)

rr_states <- Po_comb %>%
  select(id) %>%
  left_join(rr_cell, by = "id")

## Regular RR plot
ggplot(rr_states, aes(fill = rr_cases)) +
  geom_sf(size = .0005) +
  scale_fill_continuous(name = "Average RR",
  low = "#fff7ec", high = "#7F0000",
  breaks = round(seq(floor(min(rr_states$rr_cases)),
  ceiling(max(rr_states$rr_cases)), l = 5)),
  limits = c(floor(min(rr_states$rr_cases)),
  ceiling(max(rr_states$rr_cases)))) +
  blank() +
  theme(text = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(face = "bold",size = 14),
        axis.text.y = element_text(face = "bold",size = 14))

# ggsave("./fig/rr_total_poland.tiff", plot = last_plot(),
#      dpi = 300, width = 320, height = 300, units = "mm")

### counts total
dr_y2 <- Po_comb %>%
  group_by(id) %>%
  summarise(cases = sum(numcase))

ggplot(dr_y2, aes(fill = cases)) +
  geom_sf(size = .0005) +
  scale_fill_continuous(name = "Average cases",
                        low = "#fff7ec", high = "#7F0000",
                        breaks = round(seq(floor(min(dr_y2$rr_cases)),
                                           ceiling(max(dr_y2$rr_cases)), l = 5)),
                        limits = c(floor(min(dr_y2$rr_cases)),
                                   ceiling(max(dr_y2$rr_cases)))) +
  blank() +
  theme(text = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(face = "bold",size = 14),
        axis.text.y = element_text(face = "bold",size = 14))

ggsave("./fig/rr_total_poland_counts.tiff", plot = last_plot(),
       dpi = 300, width = 320, height = 300, units = "mm")

# Average per year
ASF$year <- as.factor(ASF$year)

rr_years <- aggregate(ASF$rr_cases, list(ASF$year), mean)
names(rr_years) <- c("year", "rr_cases")

ggplot(data = rr_years, aes(as.factor(year), rr_cases)) +
  geom_point(size = .5) +
  geom_line(size = .3, group = 1) +
  expand_limits(y = c(0, 3)) +
  scale_x_discrete(breaks = 2021:2022) +
  labs(x = "Year", y = "Average RR") +
  theme(text = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(face = "bold",size = 14),
        axis.text.y = element_text(face = "bold",size = 14))

ggsave("./fig/rr_year_total_poland.tiff", plot = last_plot(),
       dpi = 300, width = 320, height = 300, units = "mm")

save.image("./data/asf_po.data.RData")

## Models ----------------------------------------------------------------------

RR01p <- inla(numcase ~ 1 +
               f(id_cell,
                 model = "iid",
                 hyper = prec,
                 constr = TRUE) +
               f(id_year,
                 model = "iid",
                 hyper = prec,
                 constr = TRUE),
             family = "poisson",
             data = ASF,
             E = expected_cases,
             verbose = TRUE,
             control.predictor = list(link = 1, compute = TRUE),
             control.compute = list(dic = TRUE))
summary(RR01p)
save(RR01p, file="./data/RR01p.RData")

RR001p <- inla(numcase ~ 1 +
               f(id_cell,
                 model = "bym2",
                 hyper = prec_bym2,
                 graph = br_adjp,
                 constr = TRUE,
                 scale.model = TRUE) +
               f(id_year,
                 model = "iid",
                 hyper = prec,
                 constr = TRUE),
             family = "poisson",
             data = ASF,
             E = expected_cases,
             verbose = TRUE,
             control.predictor = list(link = 1, compute = TRUE),
             control.compute = list(dic = TRUE))
summary(RR001p)

RR02 <- inla(numcase ~ 1  +
               f(id_cell,
                 model = "bym2",
                 hyper = prec_bym2,
                 graph = br_adjp,
                 constr = TRUE,
                 scale.model = TRUE) +
               f(id_year,
                 model = "iid",
                 hyper = prec,
                 constr = TRUE) +
               f(id_year2,
                 model = "rw1",
                 hyper = prec,
                 constr = TRUE),
             family = "poisson",
             data = ASF,
             E = expected_cases,
             control.predictor = list(link = 1, compute = TRUE),
             verbose=T,
             control.compute = list(dic = TRUE))
summary(RR02)

## Intercation 1: nu_i x phi_t
RR1 <- inla(numcase ~ 1 +
              f(id_cell,
                model = "bym2",
                hyper = prec_bym2,
                graph = br_adj,
                scale.model = TRUE,
                constr = TRUE) +
              f(id_year,
                model = "iid",
                hyper = prec,
                constr = TRUE) +
              f(id_year2,
                model = "rw1",
                hyper = prec,
                constr = TRUE,
                scale.model = TRUE) +
              f(id_cell_year,
                model = "iid",
                hyper = prec,
                constr = TRUE),
            family = "poisson",
            data = ASF,
            E = expected_cases,
            lincomb = lcs, 
            control.predictor = list(link = 1, compute = TRUE),
            verbose=T,
            control.compute = list(dic = TRUE))
save(RR1, file = "./data/RR1.RData")

# ## Interaction 2: nu_i x gamma_t
RR2 <- inla(numcase ~ 1 +
              f(id_cell,
                model = "bym2",
                hyper = prec_bym2,
                graph = br_adj,
                scale.model = TRUE,
                constr = TRUE) +
              f(id_year,
                model = "iid",
                hyper = prec,
                constr = TRUE) +
              f(id_year2,
                model = "rw1",
                hyper = prec,
                constr = TRUE,
                scale.model = TRUE) +
              f(id_cell2,
                model = "iid",
                hyper = prec,
                constr = TRUE,
                group = id_year3,
                control.group = list(model = "rw1")),
            family = "poisson",
            data = ASF,
            E = expected_cases,
            lincomb = lcs,
            control.predictor = list(compute = TRUE),
            control.compute = list(dic = TRUE))

## Interaction 3: phi_t x mu_i
RR3 <- inla(numcase ~ 1 +
              f(id_cell,
                model = "bym2",
                hyper = prec_bym2,
                graph = br_adj,
                scale.model = TRUE,
                constr = TRUE) +
              f(id_year,
                model = "iid",
                hyper = prec,
                constr = TRUE) +
              f(id_year2,
                model = "rw1",
                hyper = prec,
                constr = TRUE,
                scale.model = TRUE) +
              f(id_year3,
                model = "iid",
                hyper = prec,
                constr = TRUE,
                group = id_cell2,
                control.group = list(model = "besag", graph = br_adj)),
            family = "poisson",
            data = ASF,
            E = expected_cases,
            lincomb = lcs,
            control.predictor = list(compute = TRUE),
            control.compute = list(dic = TRUE))

## Interaction 4: mu_i x gamma_t
RR4 <- inla(numcase ~ 1 +
              f(id_cell,
                model = "bym2",
                hyper = prec_bym2,
                graph = br_adj,
                scale.model = TRUE,
                constr = TRUE) +
              f(id_year,
                model = "iid",
                hyper = prec,
                constr = TRUE) +
              f(id_year2,
                model = "rw1",
                hyper = prec,
                constr = TRUE,
                scale.model = TRUE) +
              f(id_cell2,
                model = "besag",
                hyper = prec,
                graph = br_adj,
                group = id_year3,
                constr = TRUE,
                scale.model = TRUE,
                control.group = list(model = "rw1")),
            family = "poisson",
            data = ASF,
            E = expected_cases,
            lincomb = lcs,
            control.predictor = list(compute = TRUE),
            control.compute = list(dic = TRUE))

save(RR01, RR02, RR1, RR2, RR3, RR4, file="./data/models.RData")

## Model checking --------------------------------------------------------------

DIC(RR01, RR02, RR1, RR2, RR3)
pred_p <- PredPValue(RR01, RR02, RR1, RR2, RR3, RR4)
pred_p$p_tails * 100

## Explained variance
ExplainedVariance(RR01)

## Priority Index within years
pr_idx <- PriorityIndex(RR01, effect = "fitted", cutoff = 1, rescale_by = "year")
ASF <- ASF %>%
  mutate(pr_index = pr_idx)

ASF %>%
  select(id, year, pr_index) %>%
  print(n = 27)

## Graphic results -------------------------------------------------------------

## Probability of SMR > 1 (excess risk).
exc_risk <- FittedExcess(RR01, cutoff = 1)

spatio_temporal_exc_risk <- ASF %>%
  select(id, year) 


ggplot(spatio_temporal_exc_risk, aes(fill = ER)) +
  geom_sf(size = .0005) +
  theme_minimal() +
  facet_wrap(~ year, nrow = 5) +
  theme(strip.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.key.size = unit(12,"points")) +
  scale_fill_continuous(name = "Pr(RR > 1)",
                        low = "#fff7ec", high = "#7F0000",
                        limits = c(0, 1)) +
  blank()

ggsave("./fig/estimated_ER.tiff", plot = last_plot(),
       dpi = 300, width = 320, height = 300, units = "mm")

# Temporal trend
# re <- RandomEffects(RR01)
# temporal2 <- tibble(Year = 2021:2022,
#                     IID = re$id_year,
#                     RW1 = re$id_year2,
#                     variable = "RR")
# temporal2 <- temporal2 %>%
#   gather(RE, value, -Year, -variable)

# save(temporal2, file = "temporal2.RData")

spatio_temporal_exc_risk$year<-as.factor(spatio_temporal_exc_risk$year)
ASF$year<-as.factor(ASF$year)

# Priority index
ASF3 <- spatio_temporal_exc_risk %>%
  left_join(
    {as.data.frame(ASF) %>%
        mutate(year = as.integer(as.character(year))) %>%
        select(id, year, pr_index)},
    by = c("id", "year"))


ggplot(lepto3, aes(fill = pr_index)) +
  geom_sf(size = .02) +
  theme_minimal() +
  facet_wrap(~ year, nrow = 5) +
  theme(strip.text = element_text(size = 6),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 6),
        legend.key.size = unit(10, "points")) +
  scale_fill_continuous(name = "Priority indiex",
                        low = "#fff7ec", high = "#7F0000") +
  blank()

ggsave("./fig/rr_index.tiff", plot = last_plot(),
       dpi = 300, width = 320, height = 300, units = "mm")


## Prior sensitivity analysis --------------------------------------------------

## PC prior for tau_i: U = 1, alpha = 0.01
## PC prior for phi: U = 0.1, alpha = 0.9
prec2 <- list(prec = list(prior = "pc.prec", param = c(.1/.31, .01)))
prec2_bym2 <- list(phi = list(prior = "pc", param = c(.1, .9)),
                   prec = list(prior = "pc.prec", param = c(.1/.31, .01)))

mod_prec2 <- inla(cases ~ 1  +
                    f(id_state,
                      model = "bym2",
                      hyper = prec2_bym2,
                      graph = br_adj,
                      scale.model = TRUE,
                      constr = TRUE) +
                    f(id_year,
                      model = "iid",
                      hyper = prec2,
                      constr = TRUE) +
                    f(id_year2,
                      model = "rw1",
                      hyper = prec2,
                      constr = TRUE,
                      scale.model = TRUE) +
                    f(id_state_year,
                      model = "iid",
                      hyper = prec2,
                      constr = TRUE),
                  family = "poisson",
                  data = lepto,
                  E = expected_cases,
                  lincomb = lcs,
                  control.predictor = list(link = 1, compute = TRUE),
                  control.compute = list(dic = TRUE, waic = TRUE))
summary(mod_prec2)

# Exponentiated effects 
sapply(RandomEffects(mod_prec2), summary)
FixedEffects(mod_prec2, rnd = 5)

## Priori for tau_i: Gamma: shape = 1, rate = 0.5)
## Prior for phi: U = 0.1, alpha = 0.9
prec3 <- list(theta = list(prior = 'loggamma', param = c(1, .5)))
prec3_bym2 <-list(phi = list(prior = "pc", param = c(.5, .5)),
                  prec = list(prior = "loggamma", param = c(1, .5)))

mod_prec3 <- inla(cases ~ 1 +
                    f(id_state,
                      model = "bym2",
                      hyper = prec3_bym2,
                      graph = br_adj,
                      scale.model = TRUE,
                      constr = TRUE) +
                    f(id_year,
                      model = "iid",
                      hyper = prec3,
                      constr = TRUE) +
                    f(id_year2,
                      model = "rw1",
                      hyper = prec3,
                      constr = TRUE,
                      scale.model = TRUE) +
                    f(id_state_year,
                      model = "iid",
                      hyper = prec3,
                      constr = TRUE),
                  family = "poisson",
                  data = lepto,
                  E = expected_cases,
                  lincomb = lcs,
                  control.predictor = list(link = 1, compute = TRUE),
                  control.compute = list(dic = TRUE))

# Exponentiated effects 
sapply(RandomEffects(mod_prec3), summary)
FixedEffects(mod_prec3, rnd = 5)

## Priori for tau_i: Gamma: shape = 1, rate = 0.001)
## Prior for phi: U = 0.1, alpha = 0.9
prec4 <- list(theta = list(prior = 'loggamma', param = c(1, .001)))
prec4_bym2 <- list(phi = list(prior = "pc", param = c(.5, .5)),
                   prec = list(prior = "loggamma", param = c(1, .001)))

mod_prec4 <- inla(cases ~ 1  +
                    f(id_state,
                      model = "bym2",
                      hyper = prec4_bym2,
                      graph = br_adj,
                      scale.model = TRUE,
                      constr = TRUE) +
                    f(id_year,
                      model = "iid",
                      hyper = prec4,
                      constr = TRUE) +
                    f(id_year2,
                      model = "rw1",
                      hyper = prec4,
                      constr = TRUE,
                      scale.model = TRUE) +
                    f(id_state_year,
                      model = "iid",
                      hyper = prec4,
                      constr = TRUE),
                  family = "poisson",
                  data = lepto,
                  E = expected_cases,
                  lincomb = lcs,
                  control.predictor = list(link = 1, compute = TRUE),
                  control.compute = list(dic = TRUE))

# Exponentiated effects 
sapply(RandomEffects(mod_prec4), summary)
FixedEffects(mod_prec4, rnd = 5)

## Comparisons

# Fitted values
summary(RR1$summary.fitted.values[, c("mean", "0.025quant", "0.975quant")])
summary(mod_prec2$summary.fitted.values[, c("mean", "0.025quant", "0.975quant")])
summary(mod_prec3$summary.fitted.values[, c("mean", "0.025quant", "0.975quant")])
summary(mod_prec4$summary.fitted.values[, c("mean", "0.025quant", "0.975quant")])

combined <- rbind(
  RR1$summary.fitted.values[, c("mean", "0.025quant", "0.975quant")],
  mod_prec2$summary.fitted.values[, c("mean", "0.025quant", "0.975quant")],
  mod_prec3$summary.fitted.values[, c("mean", "0.025quant", "0.975quant")],
  mod_prec4$summary.fitted.values[, c("mean", "0.025quant", "0.975quant")])

combined$prior <- rep(c("pc.prec", "pc.prec2", "loggamma .5", "loggamma .001"),
                      each = 1)
combined$prior <- rep(c("pc.prec", "pc.prec2", "loggamma .5", "loggamma .001"),
                      each = nrow(RR1$summary.fitted.values))
combined$variable <- rep(rownames(RR1$summary.fixed), 1)
colnames(combined) <- c("Mean", "Lo", "Up", "prior")
combined$prior <- factor(combined$prior,
                         levels = c("pc.prec", "pc.prec2",
                                    "loggamma .5", "loggamma .001"))
combined$x <- 1:nrow(RR1$summary.fitted.values)


ggplot() +
  geom_point(data = combined,
             size = .3,
             aes(x = x, 
                 y = Mean,
                 color = prior),
             position = position_dodge(width = 2)) +
  geom_errorbar(data = combined,
                size = .3,
                aes(x = x, 
                    ymax = Up, 
                    ymin = Lo,
                    color = prior), 
                #width = 0.2,
                position = position_dodge(width = 2)) +
  xlab("") +
  ylab("Posterior mean values and 95% CI") +
  theme_classic() +
  theme(text = element_text(size = 6, face = "bold"),
        axis.text.x = element_text(size = 6))

ggsave("./fig/sensitivity_analysis.tiff", plot = last_plot(),
       dpi = 300, width = 320, height = 300, units = "mm")

# Random effects
sapply(RandomEffects(RR1), summary)
sapply(RandomEffects(mod_prec2), summary)
sapply(RandomEffects(mod_prec3), summary)
sapply(RandomEffects(mod_prec4), summary)

combined_re <- rbind(
  RR1$summary.random$id_state[1:27, c("mean", "0.025quant", "0.975quant")],
  mod_prec2$summary.random$id_state[1:27, c("mean", "0.025quant", "0.975quant")],
  mod_prec3$summary.random$id_state[1:27, c("mean", "0.025quant", "0.975quant")],
  mod_prec4$summary.random$id_state[1:27, c("mean", "0.025quant", "0.975quant")])

combined_re$Prior <- rep(c("pc.prec", "pc.prec2", "loggamma .5", "loggamma .001"),
                         each = 27)
colnames(combined_re) <- c("Mean", "Lo", "Up", "Prior")
combined_re$Prior <- factor(combined_re$Prior,
                            levels = c("pc.prec", "pc.prec2",
                                       "loggamma .5", "loggamma .001"))
combined_re$State <- unique(lepto$state)

ggplot() +
  geom_point(data = combined_re,
             size = .3,
             aes(x = State, 
                 y = Mean,
                 color = Prior),
             position = position_dodge(width = 0.4)) +
  geom_errorbar(data = combined_re,
                size = .3,
                aes(x = State, 
                    ymax = Up, 
                    ymin = Lo,
                    color = Prior), 
                width = 0.2,
                position = position_dodge(width = 0.4)) +
  xlab("State") +
  ylab("Posterior mean values and 95% CI") +
  theme_classic() +
  theme(text = element_text(size = 6, face = "bold"),
        axis.text.x = element_text(size = 6, angle = 90))


ggsave("./fig/sensitivity_analysis2.tiff", plot = last_plot(),
       dpi = 300, width = 320, height = 300, units = "mm")

# Fixed Effects
FixedEffects(RR1)
FixedEffects(mod_prec2)
FixedEffects(mod_prec3)
FixedEffects(mod_prec4)

combined_fe <- rbind.data.frame(FixedEffects(RR1),
                                FixedEffects(mod_prec2),
                                FixedEffects(mod_prec3),
                                FixedEffects(mod_prec4))
combined_fe$prior <- rep(c("pc.prec", "pc.prec2", "loggamma .5", "loggamma .001"),
                         each = 1)
colnames(combined_fe) <- c("Mean", "Lo", "Up", "prior")


ggplot() + 
  geom_point(data = combined_fe,
             size = .3,
             aes(x = prior, 
                 y = Mean)) +
  geom_errorbar(data = combined_fe,
                size = .3,
                aes(x = prior, 
                    ymax = Up, 
                    ymin = Lo), 
                width = .2) +
  xlab("Parameters") + ylab("Values") +
  theme_classic() +
  theme(text = element_text(size = 6, face = "bold"),
        axis.text.x = element_text(size = 6)) +
  theme(legend.position="none")

ggsave("./fig/fixed_effect.tiff", plot = last_plot(),
       dpi = 300, width = 320, height = 300, units = "mm")

## Comparison for DIC and p-value
DIC(RR1,mod_prec2, mod_prec3, mod_prec4)
pred_p <- PredPValue(RR1,mod_prec2, mod_prec3, mod_prec4)
pred_p$p_tails * 100

