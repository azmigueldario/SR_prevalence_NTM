# ---------------------------- Meta regression

metareg <- 
  rma.glmm(xi = ntm_point_infection,
           ni = sample_size_cf,
           slab = id,
           data = metareg_data,
           method = "ML",
           measure = "PLO",
           # random effects model
           model = "UM.RS",
           mods= ~ design_or_registry + sample_size_cat + region1,
           control = list(optimizer = 'bobyqa', optCtrl=list(iter.max=10000, rel.tol=1e-12)))

###### ---------------------------- table metaregression

summary(metareg) %>%
  coef() %>%
  mutate(Coefs = temp$coef.names, .before = 1,
         zval = NULL) %>%
  # specify names of columns
  flextable_wrapper(names_col = c("Coefficients",  "LOGIT-estimate", "Std. error",
                                  "p.value", "CI-lower", "CI-upper"),
                    digits = 4) %>% 
  bold(i= ~ p.value <0.05 & Coefficients!="Intercept", part = "body") %>% 
  print(preview="docx")


# ---------------------------- sample size ~ estimate scatter plot

clean_data %>%
  filter(design_or_registry != "Registry report/used registry") %>%
  ggplot(aes(y = sample_size_cf, 
             x = first_year_data)) +
  geom_point(size = 2) +
  labs (subtitle = "Observational studies, non-registry data",
        x = "First year of data collection",
        y = "CF sample size") +
  scale_y_continuous(trans = "log10") +
  theme(plot.subtitle = element_text(color = "red4", size = 13, hjust=0.5),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12)) 

# ---------------------------- exploring tendencies inside registries

clean_data %>% 
  # select point_infection or annual_period studies
  filter(point_infection_ind == "yes" & is_registry==1) %>%
  # percentages and calculated prevalence
  mutate(estimate = case_when(is.na(point_inf_perc) ~ 
                                (ntm_point_infection / sample_size_cf ) * 100,
                              TRUE ~ point_inf_perc)) %>% 
  ggplot(aes(x = first_year_data,
             y = estimate,
             group = used_registry)) +
  geom_point() +
  facet_wrap(~ used_registry) +
  scale_x_continuous(breaks = seq(2010, 2019, 2))
  

# ---------------------------- alternative categories for sample size

temp$untidy_data<- 
  temp$untidy_data %>%
  mutate(study_design=case_when(study_design=="Cross-sectional study" ~ "Cross-sectional study",
                                study_design=="Registry report" ~ "Registry report",
                                TRUE ~ "Cohort study"),
         design_or_registry= case_when(study_design=="Registry report" | 
                                         used_registry!="no" ~ "Registry report/used registry",
                                       TRUE ~ study_design),
         is_registry=case_when(study_design=="Registry report" ~ 1,
                               TRUE ~ 0),
         size_cat = case_when(sample_size_cf <= 1000 ~ "small",
                              sample_size_cf > 1000 ~ "large"),
         size_cat = factor(size_cat, levels = c("small", "large")))


# ------------------------ sensitivity: reported culture method

inf_point_data %>% 
  mutate(culture_reported = case_when(!is.na(culture_method) ~ "yes",
                                      TRUE ~ "no")) %>% 
  metaprop_formatted() %>% 
  update.meta(.,
              subgroup =  culture_reported, 
              tau.common = F) %>% 
  forest_wrapper()

# ----------------------- sensitivity: excluding subjective sampling

# seddon & preece screened (even partially) by symptoms
inf_point_data %>% 
  filter(!str_detect(id, "seddon|preece")) %>% 
  metaprop_formatted() %>% 
  forest_wrapper()

# ----------------------- plot of year of data vs estimate prevalence

clean_data %>% 
  filter(point_infection_ind=="yes" | annual_period_ind =="yes") %>% 
  # percentages and calculated prevalence
  mutate(estimate = case_when(is.na(point_inf_perc) ~ 
                                (ntm_point_infection / sample_size_cf ) * 100,
                              TRUE ~ point_inf_perc)) %>% 
  ggplot(aes(x = first_year_data,
             y = estimate, 
             shape = study_design,
             color = study_design)) +
  geom_jitter(size = 2)

# ---------------------- How many cohorts in meta-analyses

clean_data %>% 
  filter(study_design=="Cohort study") %>% 
  select(point_infection_ind, period_infection_ind, period_disease_ind, incidence_ind)

  
  

