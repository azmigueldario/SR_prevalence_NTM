########################################################################
###                   Risk of Bias plots
#########################################################################

# Script must be run over SR prevalence analysis script
# Requires robvis, cairo, meta, metafor and dplyr packages 
# Starting point is risk of bias data and meta-analysis results

#########################################################################


# Clean the names of domains and remove domain 5
rob_data2 <- select(rob_data, -matches("Q5"))
colnames(rob_data2) <- str_replace(string = colnames(rob_data2), 
                                   pattern = "Q([0-9])_", 
                                   replacement = "\\1) ")

# Cairo can be used to produce high quality images in svg format
library(Cairo)

### Registry reports only
{tiff(filename = "Fig2a.tiff",
      width = 370,
      height = 150,
      units = "mm",
      pointsize = 10, # size of text
      res = 300, # desired dpi
      type = "cairo", # specify use of Cairo
      compression = "lzw" # to reduce size
      )
rob_data2 %>%
  filter(study_design=="Registry report") %>% 
  rob.summary(data=.,
              name.high="High",
              name.unclear="Unclear",
              name.low="Low",
              name.missing ="Missing",
              studies=.$study) 
dev.off()}

### Non registry reports only
{tiff(filename = "Fig2b.tiff",
      width = 370,
      height = 150,
      units = "mm",
      pointsize = 10, # size of text
      res = 300, # desired dpi
      type = "cairo", # specify use of Cairo
      compression = "lzw" # to reduce size
)
  
  rob_data2 %>%
    filter(study_design!="Registry report") %>% 
    rob.summary(data = .,
                name.high = "High",
                name.unclear = "Unclear",
                name.low = "Low",
                name.missing = "Missing",
                studies = .$study)
  dev.off()
}


#########################################################################
###                   Forest plot of main meta-analysis
#########################################################################

# We first create a working dataset of the studies in meta-analysis
pub_data <-  
  clean_data %>% 
  # select point_infection or annual_period studies
  filter(point_infection_ind == "yes" | annual_period_ind =="yes") %>% 
  # drop_na
  drop_na(ntm_point_infection) %>% 
  # drop studies that used registry data (duplicate the data)
  filter(!(is_registry!="Registry" & used_registry !="no")) %>% 
  # include only last registry report per country
  filter(last.registry_analysis=="yes") %>%
  ## we clean the names of registries
  mutate(study=str_replace(study, "Usa", "USA CF registry"),
         study=str_replace(study, "Ecfs", "European CF registry"),
         study=str_replace(study, "Brazil", "Brazilian CF registry"),
         study=str_replace(study, "Canada", "Canadian CF registry"),
         study=str_replace(study, "Australia", "Australian CF registry"),
         study=str_replace(study, "2003b", "2003")
  )

  

pub_meta<- metaprop_formatted(pub_data)
pub_meta$sm

# Cairo can be used to produce plots in .svg format or in high quality
{tiff(filename = "Fig3.tiff",
      width = 220,
      height = 150,
      units = "mm",
      pointsize = 10, # size of text
      res = 300, # desired dpi
      type = "cairo", # specify use of Cairo
      compression = "lzw" # to reduce size
)
forest.meta(x = pub_meta,
            sortvar = meta.full$TE,
            prediction = T,
            comb.fixed = F,
            layout = "meta",
            print.tau2 = F,
            print.pval.Q = F,
            print.I2 = T,
            print.I2.ci = T,
            colgap.left = "0cm",
            colgap.forest = "0.5cm",
            spacing=1.1,
            leftcols = c("studlab", "effect", "n", "ci"),
            leftlabs = c("Study", "Proportion", "Sample \nsize", "95% CI"),
            smlab= "Cases per 100 observations",
            just= "center",
            rightcols = FALSE,
            study.results = T,
            pscale=100,
            addrow = T,
            addrow.overall = T,
            col.by = "grey2",
            xlim = c(0, 60),
            addrows.below.overall = 1,
            col.square = "darkblue",
            col.diamond = "lightblue",
            col.random = "darkblue",
            digits=1,
            fontsize = 14,
            fs.heading = 16,
            fs.random = 16)
dev.off()}
  
#########################################################################
###                  Figure 4 - Meta-analyses for MAC and MABs
#########################################################################

#-------------------- Meta-analysis for MABs infection
{tiff(filename = "Fig4a.tiff",
      width = 230,
      height = 110,
      units = "mm",
      pointsize = 10, # size of text
      res = 300, # desired dpi
      type = "cairo", # specify use of Cairo
      compression = "lzw" # to reduce size
)
pub_data %>% 
  filter(!is.na(mabs_infection)) %>% 
  metaprop_formatted(.,
                     event = mabs_infection) %>%
  forest.meta(x = .,
              prediction = T,
              comb.fixed = F,
              layout = "meta",
              print.tau2 = F,
              print.pval.Q = F,
              print.I2 = T,
              print.I2.ci = T,
              colgap.left = "0cm",
              colgap.forest = "0.5cm",
              spacing = 1.1,
              leftcols = c("studlab", "effect", "n", "ci"),
              leftlabs = c("Study", "Proportion", "Sample \nsize", "95% CI"),
              smlab= "Cases per 100 observations",
              just = "center",
              rightcols = FALSE,
              study.results = T,
              pscale = 100,
              addrow = T,
              addrow.overall = T,
              col.by = "grey2",
              xlim = c(0, 35),
              addrows.below.overall = 1,
              col.square = "darkblue",
              col.diamond = "lightblue",
              col.random = "darkblue",
              digits = 1,
              fontsize = 14,
              fs.heading = 16,
              fs.random = 16)
dev.off()}

#-------------------- Meta-analysis for MAC infection
{tiff(filename = "Fig4b.tiff",
      width = 230,
      height = 110,
      units = "mm",
      pointsize = 10, # size of text
      res = 300, # desired dpi
      type = "cairo", # specify use of Cairo
      compression = "lzw" # to reduce size
)
pub_data %>% 
  filter(!is.na(avium_infection)) %>% 
  metaprop_formatted(.,
                     event = avium_infection) %>%
  forest.meta(x = .,
              prediction = T,
              comb.fixed = F,
              layout = "meta",
              print.tau2 = F,
              print.pval.Q = F,
              print.I2 = T,
              print.I2.ci = T,
              colgap.left = "0cm",
              colgap.forest = "0.5cm",
              spacing = 1.1,
              leftcols = c("studlab", "effect", "n", "ci"),
              leftlabs = c("Study", "Proportion", "Sample \nsize", "95% CI"),
              smlab= "Cases per 100 observations",
              just = "center",
              rightcols = FALSE,
              study.results = T,
              pscale = 100,
              addrow = T,
              addrow.overall = T,
              col.by = "grey2",
              xlim = c(0, 25),
              addrows.below.overall = 1,
              col.square = "darkblue",
              col.diamond = "lightblue",
              col.random = "darkblue",
              digits = 1,
              fontsize = 14,
              fs.heading = 16,
              fs.random = 16)
dev.off()}

#########################################################################
###                   Supplementary figures
#########################################################################

### ------------ Supplementary figure 1 - Traffic plots

# incidence plot
{tiff(filename = "FigS1a.tiff",
      width = 150,
      height = 90,
      units = "mm",
      pointsize = 10, # size of text
      res = 300, # desired dpi
      type = "cairo", # specify use of Cairo
      compression = "lzw" # to reduce size
)
plot<-   rob_data2 %>% 
    filter(incidence_ind=="yes") %>% 
    select(-ROB_overall) %>% 
    rob.summary(table = T,
                studies=.$study) %>% 
    delete_layers(idx = 1:3) +
    geom_tile(color = "black", fill = "white", size = 0.7) +
    geom_point(aes(color=as.factor(measurement)),
               size=13, 
               shape=15) +
  scale_y_discrete(position = "right", ) +
  geom_text(aes(label = measurement),
            size = 6) +
  theme(axis.text.x = element_text(size=11,
                                     angle = 45,
                                     face = "bold"),
          axis.text.y = element_text(size=13))
print(plot)  
dev.off()}

# ntm disease plot
{tiff(filename = "FigS1b.tiff",
      width = 150,
      height = 180,
      units = "mm",
      pointsize = 10, # size of text
      res = 300, # desired dpi
      type = "cairo", # specify use of Cairo
      compression = "lzw" # to reduce size
)
  plot<-   rob_data2 %>%
    filter(period_disease_ind =="yes" | point_disease_ind=="yes") %>% 
    # reverse the order of study levels so when flipped it looks okay
    mutate(study=reorder(study, desc(study))) %>% 
    select(-ROB_overall) %>% 
    rob.summary(table = T,
                studies=.$study) %>% 
    delete_layers(idx = 1:3) +
    geom_tile(color = "black",
              fill = "white", 
              size = 0.7) +
    geom_point(aes(color=as.factor(measurement)),
               size=12,
               shape=15) +
    geom_text(aes(label = measurement),
              size = 7) +
    scale_y_discrete(position = "right", ) +
    theme(axis.text.x = element_text(size=11, 
                                     angle = 45,
                                     face = "bold"),
          axis.text.y = element_text(size=13))
  print(plot)  
  dev.off()}

### ------------ Supplementary figure 2 - Subgroup analyses

## Subgroup: study design (registry or not) 
{tiff(filename = "FigS2a.tiff",
      width = 240,
      height = 90,
      units = "mm",
      pointsize = 10, # size of text
      res = 300, # desired dpi
      type = "cairo", # specify use of Cairo
      compression = "lzw" # to reduce size
)
  update.meta(meta.full,
              subgroup = is_registry,
              subgroup.name = "registry",
              tau.common = F) %>% 
    forest_wrapper()
  dev.off()}

## Subgroup: year of data collection
{tiff(filename = "FigS2b.tiff",
      width = 240,
      height = 100,
      units = "mm",
      pointsize = 10, # size of text
      res = 300, # desired dpi
      type = "cairo", # specify use of Cairo
      compression = "lzw" # to reduce size
)
  update.meta(meta.full,
              subgroup = before_year,
              tau.common = F) %>%
    forest_wrapper(data = .,
                   pred_int = T,
                   xlim = c(0, 0.5),
                   subgroup.name = "First year of data collection")
  
  dev.off()}

## Subgroup: regions (continents)
{tiff(filename = "FigS2c.tiff",
      width = 240,
      height = 100,
      units = "mm",
      pointsize = 10, # size of text
      res = 300, # desired dpi
      type = "cairo", # specify use of Cairo
      compression = "lzw" # to reduce size
)
  update.meta(meta.full,
              subgroup =  region1, 
              tau.common = F) %>% 
    forest_wrapper(data = .,
                   subgroup.name = "Region",
                   xlim = c(0, 0.5)) 
  dev.off()}

### ------------ Supplementary figure 3

### Subgroup analysis by region of MABs meta-analysis

{tiff(filename = "FigS3a.tiff",
      width = 240,
      height = 80,
      units = "mm",
      pointsize = 10, # size of text
      res = 300, # desired dpi
      type = "cairo", # specify use of Cairo
      compression = "lzw" # to reduce size
)
  inf_point_data %>% 
    filter(!is.na(mabs_infection)) %>% 
    metaprop_formatted(.,
                       event = mabs_infection) %>%
    update.meta(.,
                subgroup = region1,
                tau.common = F) %>% 
    forest_wrapper(xlim = c(0, 0.2))
  dev.off()}

### Subgroup analysis by region of MAC meta-analysis

{tiff(filename = "FigS3b.tiff",
      width = 240,
      height = 80,
      units = "mm",
      pointsize = 10, # size of text
      res = 300, # desired dpi
      type = "cairo", # specify use of Cairo
      compression = "lzw" # to reduce size
)
  inf_point_data %>% 
    filter(!is.na(avium_infection)) %>% 
    metaprop_formatted(.,
                       event = avium_infection) %>%
    update.meta(.,
                subgroup = region1,
                tau.common = F) %>% 
    forest_wrapper(stu.res = F, xlim = c(0, 0.2))
  dev.off()}

### Sensitivity analysis of main meta-analysis without Preece 2016

{tiff(filename = "FigS3c.tiff",
      width = 240,
      height = 40,
      units = "mm",
      pointsize = 10, # size of text
      res = 300, # desired dpi
      type = "cairo", # specify use of Cairo
      compression = "lzw" # to reduce size
)
  # sensitivity analysis without preece2016
  inf_point_data %>% 
    filter(!str_detect(id, "preece")) %>% 
    metaprop_formatted() %>% 
    forest_wrapper()
  dev.off()}

### ------------ Supplementary figure 4 - Funnel plot 

{tiff(filename = "FigS4.tiff",
      width = 140,
      height = 110,
      units = "mm",
      pointsize = 10, # size of text
      res = 300, # desired dpi
      type = "cairo", # specify use of Cairo
      compression = "lzw" # to reduce size
)
  funnel(x = metafor.full,
         yaxis = "ni",
         xlab="Logit transformed proportions",
         level = 95,
         atransf = transf.ilogit,
         col = "darkblue")
  dev.off()}

### ------------ Supplementary figure 5a - Only registry sensitivity analysis


{tiff(filename = "FigS7a.tiff",
      width = 220,
      height = 80,
      units = "mm",
      pointsize = 10, # size of text
      res = 300, # desired dpi
      type = "cairo", # specify use of Cairo
      compression = "lzw" # to reduce size
)
  pub_data %>% 
    # select point_infection or annual_period studies
    filter(point_infection_ind == "yes" | annual_period_ind =="yes") %>% 
    # drop_na
    drop_na(ntm_point_infection) %>% 
    # drop studies that used registry data (duplicate the data)
    filter(is_registry=="Registry" & used_registry !="no") %>% 
    # include only last registry report per country
    filter(last.registry_analysis=="yes") %>% 
    metaprop_formatted() %>% 
    forest.meta(x = .,
                prediction = T,
                comb.fixed = F,
                layout = "meta",
                print.tau2 = F,
                print.pval.Q = F,
                print.I2 = T,
                print.I2.ci = T,
                colgap.left = "0cm",
                colgap.forest = "0.5cm",
                spacing = 1.1,
                leftcols = c("studlab", "effect", "n", "ci"),
                leftlabs = c("Study", "Proportion", "Sample \nsize", "95% CI"),
                smlab= "Cases per 100 observations",
                just = "center",
                rightcols = FALSE,
                study.results = T,
                pscale = 100,
                addrow = T,
                addrow.overall = T,
                col.by = "grey2",
                xlim = c(0, 80),
                addrows.below.overall = 1,
                col.square = "darkblue",
                col.diamond = "lightblue",
                col.random = "darkblue",
                digits = 1,
                fontsize = 14,
                fs.heading = 16,
                fs.random = 16)
  dev.off()}

### ------------ Supplementary figure 5b - Sensitivity without RGM/BCSA culture media

{tiff(filename = "FigS7b.tiff",
      width = 220,
      height = 130,
      units = "mm",
      pointsize = 10, # size of text
      res = 300, # desired dpi
      type = "cairo", # specify use of Cairo
      compression = "lzw" # to reduce size
)
  
  inf_point_data %>% 
    filter(!str_detect(id, "preece|scohy|raidt|plongla")) %>% 
    metaprop_formatted() %>% 
    forest.meta(x = .,
                prediction = T,
                comb.fixed = F,
                layout = "meta",
                print.tau2 = F,
                print.pval.Q = F,
                print.I2 = T,
                print.I2.ci = T,
                colgap.left = "0cm",
                colgap.forest = "0.5cm",
                spacing = 1.1,
                leftcols = c("studlab", "effect", "n", "ci"),
                leftlabs = c("Study", "Proportion", "Sample \nsize", "95% CI"),
                smlab= "Cases per 100 observations",
                just = "center",
                rightcols = FALSE,
                study.results = T,
                pscale = 100,
                addrow = T,
                addrow.overall = T,
                col.by = "grey2",
                xlim = c(0, 70),
                addrows.below.overall = 1,
                col.square = "darkblue",
                col.diamond = "lightblue",
                col.random = "darkblue",
                digits = 1,
                fontsize = 14,
                fs.heading = 16,
                fs.random = 16)
  dev.off()}



### ------------ Supplementary Figure 6 - Boxplots of period prevalence

tiff(filename = "FigS5.tiff",
      width = 140,
      height = 100,
      units = "mm",
      pointsize = 10, # size of text
      res = 300, # desired dpi
      type = "cairo", # specify use of Cairo
      compression = "lzw" # to reduce size
      )
temp$period_infection %>% 
  ggplot(data = .,
         aes(y = estimate,
             x = study_length_cat,
             color = study_length_cat)) +
    # three categories of length looks better
    geom_boxplot(outlier.shape = NA, width = 0.5) +
    geom_jitter(aes(color = study_length_cat),
                width = 0.1) +
    theme_classic() +
    theme(legend.position = "none",
          axis.title = element_text(size = 12)) +
    scale_y_continuous(breaks = seq(0, 40, 5)) +
    labs(x = "\nStudy length in years ",
         y = "Prevalence estimate (%)\n")
dev.off()

### ------------ Supplementary figure 7 - Tendencies in registry

tiff(filename = "FigS6.tiff",
     width = 160,
     height = 120,
     units = "mm",
     pointsize = 10, # size of text
     res = 300, # desired dpi
     type = "cairo", # specify use of Cairo
     compression = "lzw" # to reduce size
)
clean_data %>% 
  # select registries
  filter(point_infection_ind == "yes" & is_registry=="Registry") %>%
  # percentages and calculated prevalence
  mutate(estimate = case_when(is.na(point_inf_perc) ~ 
                                (ntm_point_infection / sample_size_cf ) * 100,
                              TRUE ~ point_inf_perc)) %>% 
  ggplot(aes(x = first_year_data,
             y = estimate,
             group = used_registry,
             color = used_registry)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(2010, 2019, 2)) +
  labs (y = "Annual prevalence estimate (%)",
        x = "Year",
        color = "") +
  theme_classic() +
  theme(panel.grid.major.x = element_line(color = "gray70",size = 0.25),
        panel.grid.major.y = element_line(color = "gray70",size = 0.25))
dev.off()




#########################################################################
####       Meta-regression table
#########################################################################

summary(metareg) %>%
  coef() %>%
  mutate(Coefs = temp$coef.names, .before = 1,
         zval = NULL) %>%
  # specify names of columns
  flextable_wrapper(names_col = c("Coefficients",  "LOGIT-estimate", "Std. error",
                                  "p.value", "CI-lower", "CI-upper"),
                    digits = 3) %>% 
  bold(i= ~ p.value <0.05 & Coefficients!="Intercept", part = "body") %>% 
  print(., preview = "docx")


#########################################################################
####       Concatenate pictures using the command line
#########################################################################


# brew install imagemagick

# ------------- concatenate horizontally
      convert -compress lzw +append img1 img2 new_image

# ------------- concatenate vertically
      convert -compress lzw -append img1 img2 new_image

# ------------- add white space for labels on the left
      convert img.tiff -compress lzw  -gravity east \
              -extent 4600x3542 img_edited.tiff
# extent uses pixels of image so check before adding a number




