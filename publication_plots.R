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
# Modify the Robvis rob.summary function to improve colors and sizes
rob.summary2 = function(data,
                        name.high="High",
                        name.unclear="Unclear",
                        name.low="Low",
                        studies,
                        name.missing,
                        table = FALSE){
  
  # Class Checks
  if (class(data) != "data.frame"){
    stop("'data' must be of class 'data.frame'.")
  }
  
  
  if (missing(name.missing)){
    
    # Only select columns with RoB data
    
    colnames.rob = character()
    
    for (i in 1:ncol(data)){
      
      vect = as.character(data[,i])
      
      for (j in 1:length(data[,i])){
        
        if (vect[j] %in% c(name.high, name.unclear, name.low)){
          
          colnames.rob[i] = TRUE
          
        } else {
          
          colnames.rob[i] = FALSE
          message(cat("Column '", colnames(data)[i],
                      "' removed from plot because it did not contain the specified RoB ratings (only). \n",
                      sep=""))
          break
          
        }
      }
    }
    
    # Use mask: rob data
    rob = data[ , as.logical(colnames.rob)]
    
    # Relevel for plot
    for (i in 1:ncol(rob)){
      
      rob[,i] = as.character(rob[,i])
      rob[rob[,i]==name.high,i] = "High"
      rob[rob[,i]==name.unclear,i] = "Unclear"
      rob[rob[,i]==name.low,i] = "Low"
      
    }
    
    # Make table
    if (table == TRUE){
      
      if (missing(studies)){
        stop("'studies' has to be specified when 'table = TRUE'.")
      }
      
      if (length(as.vector(studies)) != nrow(data)){
        stop("'studies' vector is not of equal length as the data.")
      }
      
      if (length(unique(studies)) != length(studies)){
        stop("'studies' cannot contain duplicate study labels.")
      }
      
      robby = rob
      robby = data.frame(study = studies,
                         condition = rep(colnames(robby), each = length(studies)),
                         measurement = unlist(robby))
      rownames(robby) = NULL
      robby$condition = gsub("_"," ", robby$condition)
      robby$condition = gsub("-"," ", robby$condition)
      robby$condition = gsub("\\."," ", robby$condition)
      robby[robby$measurement=="Low", "measurement"] = "+"
      robby[robby$measurement=="Unclear", "measurement"] = "?"
      robby[robby$measurement=="High", "measurement"] = "-"
      
      # Order factor
      robby$study = factor(robby$study,
                           levels = unique(studies)[rev(order(unique(robby$study)))])
      
      
      rob.table = ggplot(data = robby, aes(y = study, x = condition)) +
        geom_tile(color="black", fill="white", size = 0.8) +
        geom_point(aes(color=as.factor(measurement)), size=20) +
        geom_text(aes(label = measurement), size = 12) +
        scale_x_discrete(position = "top") +
        scale_color_manual(values = c("?" = "#E2DF07",
                                      "-" = "#BF0000",
                                      "+" = "#02C100")) +
        theme_minimal() +
        coord_equal() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.y = element_text(size = 15, color = "black"),
              axis.text.x = element_text(size = 13, color = "black", angle = 90, hjust=0),
              legend.position = "none",
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank())
      
    }
    
    # Make long format, clean the factors
    rob.long = data.frame(condition = rep(colnames(rob), each = nrow(rob)),
                          measurement = unlist(rob))
    rownames(rob.long) = NULL
    rob.long$condition = gsub("_"," ",rob.long$condition)
    rob.long$condition = gsub("-"," ",rob.long$condition)
    rob.long$condition = gsub("\\."," ",rob.long$condition)
    rob.long$measurement = as.factor(rob.long$measurement)
    rob.long$measurement = factor(rob.long$measurement, levels(rob.long$measurement)[c(1, 3, 2)])
    
    # Make plot
    rob.plot = ggplot(data = rob.long) +
      geom_bar(mapping = aes(x = condition, fill = measurement), width = 0.7,
               position = "fill", color = "black") +
      coord_flip(ylim = c(0, 1)) +
      guides(fill = guide_legend(reverse = TRUE)) +
      scale_fill_manual("Risk of Bias",
                        labels = c("    Unclear risk of bias       ",
                                   "    High risk of bias          ",
                                   "    Low risk of bias  "),
                        values = c(Unclear = "#E2DF07", High = "#BF0000", Low = "#02C100")) +
      scale_y_continuous(labels = scales::percent) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_text(size = 54, color = "black"),
            axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
            legend.position = "bottom",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            legend.background = element_rect(linetype = "solid", colour = "black"),
            legend.title = element_blank(),
            legend.key.size = unit(0.75, "cm"),
            legend.text = element_text(size = 14))
    
    plot(rob.plot)
    
    if (table == TRUE){
      plot(rob.table)
    }
    
  } else {
    
    # Only select columns with RoB data
    data = as.data.frame(data)
    
    colnames.rob = character()
    
    for (i in 1:ncol(data)){
      
      vect = as.character(data[,i])
      
      for (j in 1:length(data[,i])){
        
        if (vect[j] %in% c(name.high, name.unclear, name.low, name.missing)){
          
          colnames.rob[i] = TRUE
          
        } else {
          
          colnames.rob[i] = FALSE
          message(cat("Column '", colnames(data)[i],
                      "' removed from plot because it did not contain the specified RoB ratings (only). \n",
                      sep=""))
          break
          
        }
      }
    }
    
    # Use mask: rob data
    rob = data[ , as.logical(colnames.rob)]
    
    # Relevel for plot
    for (i in 1:ncol(rob)){
      
      rob[,i] = as.character(rob[,i])
      rob[rob[,i]==name.high,i] = "High"
      rob[rob[,i]==name.unclear,i] = "Unclear"
      rob[rob[,i]==name.low,i] = "Low"
      rob[rob[,i]==name.missing,i] = "Missing"
      
    }
    
    # Make Table
    
    if (table == TRUE){
      
      if (missing(studies)){
        stop("'studies' has to be specified when 'table = TRUE'.")
      }
      
      if (length(as.vector(studies)) != nrow(data)){
        stop("'studies' vector is not of equal length as the data.")
      }
      
      robby = rob
      robby = data.frame(study = as.factor(studies),
                         condition = rep(colnames(robby), each = length(studies)),
                         measurement = unlist(robby))
      rownames(robby) = NULL
      robby$condition = gsub("_"," ", robby$condition)
      robby$condition = gsub("-"," ", robby$condition)
      robby$condition = gsub("\\."," ", robby$condition)
      robby[robby$measurement=="Low", "measurement"] = "+"
      robby[robby$measurement=="Unclear", "measurement"] = "?"
      robby[robby$measurement=="High", "measurement"] = "-"
      robby[robby$measurement=="Missing", "measurement"] = " "
      
      # Order factor
      robby$study = factor(robby$study,
                           levels = unique(studies)[rev(order(unique(robby$study)))])
      
      rob.table = ggplot(data = robby, aes(y = study, x = condition)) +
        geom_tile(color="black", fill="white", size = 0.8) +
        geom_point(aes(color=as.factor(measurement)), size=20) +
        geom_text(aes(label = measurement), size = 8) +
        scale_x_discrete(position = "top") +
        scale_color_manual(values = c("?" = "#E2DF07",
                                      "-" = "#BF0000",
                                      "+" = "#02C100",
                                      " " = "white")) +
        theme_minimal() +
        coord_equal() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.y = element_text(size = 15, color = "black"),
              axis.text.x = element_text(size = 13, color = "black", angle = 90, hjust=0),
              legend.position = "none",
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank())
      
    }
    
    
    # Make long format, clean the factors
    rob.long = data.frame(condition = rep(colnames(rob), each = nrow(rob)),
                          measurement = unlist(rob))
    rownames(rob.long) = NULL
    rob.long$condition = gsub("_"," ",rob.long$condition)
    rob.long$condition = gsub("-"," ",rob.long$condition)
    rob.long$condition = gsub("\\."," ",rob.long$condition)
    rob.long$measurement = as.factor(rob.long$measurement)
    rob.long$measurement = factor(rob.long$measurement, levels(rob.long$measurement)[c(3,1,4,2)])
    
    rob.plot = ggplot(data = rob.long) +
      geom_bar(mapping = aes(x = condition, fill = measurement), width = 0.7,
               position = "fill", color = "black") +
      coord_flip(ylim = c(0, 1)) +
      guides(fill = guide_legend(reverse = TRUE)) +
      scale_fill_manual("Risk of Bias",
                        labels = c("  Unclear risk of bias  ",
                                   "  High risk of bias   ",
                                   "  Low risk of bias  ",
                                   "  Missing information  "),
                        values = c(Unclear = "#D9C032",
                                   High = "#A61B40",
                                   Low = "#1589b0",
                                   Missing = "white")) +
      scale_y_continuous(labels = scales::percent) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_text(size = 28, color = "black"),
            axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
            legend.position = "bottom",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            legend.background = element_rect(linetype = "solid", colour = "black"),
            legend.title = element_blank(),
            legend.key.size = unit(0.75, "cm"),
            legend.text = element_text(size = 16))
    
    plot(rob.plot)
    
    if (table == TRUE){
      plot(rob.table)
    }
    
  }
  
}


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
         study=str_replace(study, "Australia", "Australian CF registry")
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
            leftlabs = c("Study", "Proportion", "n", "95% CI"),
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
###                   Meta-analyses for MAC and MABs
#########################################################################

#-------------------- Meta-analysis for MABs infection
{tiff(filename = "Fig4a.tiff",
      width = 220,
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
              leftlabs = c("Study", "Proportion", "n", "95% CI"),
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
      width = 220,
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
              leftlabs = c("Study", "Proportion", "n", "95% CI"),
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


### ------------ Funnel plot at 300 dpi using Cairo

{tiff(filename = "FigS4.tiff",
     width = 120,
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

### ------------ Boxplots plot at 300 dpi using Cairo

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

### ------------ Plot of tendency estimates for each registry 

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
  # produce percentage estimate for all registry reports
  mutate(estimate = case_when(is.na(point_inf_perc) ~ 
                                (ntm_point_infection / sample_size_cf ) * 100,
                              TRUE ~ point_inf_perc)) %>%
  ## clean the names of registries for graph 
  mutate(used_registry=str_replace(used_registry, "US", "USA CF registry"),
         used_registry=str_replace(used_registry, "ECFS", "European CF registry"),
         used_registry=str_replace(used_registry, "Brazil", "Brazilian CF registry"),
         used_registry=str_replace(used_registry, "Canada", "Canadian CF registry"),
         used_registry=str_replace(used_registry, "Australia", "Australian CF registry")
         ) %>% 
  ggplot(aes(x = first_year_data,
             y = estimate,
             group = used_registry,
             color = used_registry)) +
  geom_point() +
  facet_wrap(~ used_registry) +
  scale_x_continuous(breaks = seq(2010, 2019, 2)) +
  labs (y = "Annual prevalence estimate (%)",
        x = "Year") +
  theme_bw() +
  theme(legend.position = "none",
        strip.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 13))
dev.off()


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

#########################################################################
####       Concatenate pictures using the command line
#########################################################################


# brew install imagemagick

# ------------- concatenate horizontally
# convert +append img1 img2

# ------------- concatenate vertically
# convert -append img1 img2

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
