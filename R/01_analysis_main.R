# -----------------------------------------------------------------------------
# 01_analysis_main.R
# Purpose: Load and clean the systematic-review table (review_table.csv),
#          standardise variables, recode 'Doubt' values to 'Yes' in the
#          how/where/why/when columns, build the canonical analysis tibble
#          papers_final, and run the NMDS ordination used in exploratory
#          plots. This script must be sourced before scripts 03 and 04.
# Inputs:  data/review_table.csv
# Outputs: output/processed/papers_reduced.csv,
#          output/processed/unique_values.csv,
#          output/processed/df_plot.csv,
#          output/processed/tabla_freq_country.csv,
#          papers_final (in the R session, used by downstream scripts)
# Author:  Arrondo E., Fandos G. and co-authors (Arrondo et al. 2026)
# Date:    2026
# R:       4.5.3. See ../sessionInfo.txt for the exact package versions
#          used to generate the published figures.
# -----------------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(fastDummies)
library(vegan)
library(vegan3d)
library(StatMatch)
library(ggplot2)

rm(list = ls())
########
papers = read_csv("data/review_table.csv",
                  col_types = cols(Suitability = col_factor(levels = c("Yes","No"))))

papers_na <- papers[is.na(papers$Ecosystem),]

table(papers$reviewer)
table(papers$`Year of publication`)
plot(table(as.numeric(papers$`Year of publication`)), log="x", type="h", xlab = "year_pub", ylab = "count")
table(papers$Area)
table(papers$`Number of species`)
plot(table(as.numeric(papers$`Number of species`)), log="x", type="h", xlab = "n_species", ylab = "count")
table(papers$`Wallace bioregion`)
table(papers$Taxon)
table(papers$Energy)
table(papers$How)
table(papers$Where)
table(papers$Why)
table(papers$When)
table(papers$`Geographical scale`)
table(papers$`Ecological scale`)
table(papers$`Tracking method`)
table(papers$Ecosystem)

names(papers)

# Transform the columns scale in short, medium, large or multiscale
papers <- papers %>%
  mutate(Spatial_scale = case_when(
    Short == "Yes" & Medium == "No" & Large == "No"  ~ "Short_scale", 
    Short == "No" & Medium == "Yes" & Large == "No"  ~ "Medium_scale",
    Short == "No" & Medium == "No" & Large == "Yes"  ~ "Large_scale",
    TRUE ~ "Multi_scale"
  ))

#  Standardize variables
get_unique_values = function(x){
  x_str = paste(x, collapse = ",")
  x_split = tolower(trimws(unlist(strsplit(x_str, split = ",(?![^(]*\\))", perl = T))))
  x_unique = unique(x_split)
  return(sort(x_unique))
}

get_unique_values(papers$`Wallace bioregion`)
get_unique_values(papers$Energy)
get_unique_values(papers$`Geographical scale`)
get_unique_values(papers$Ecosystem)
get_unique_values(papers$`Tracking method`)
get_unique_values(papers$`Ecological scale`)

papers_reduced <- papers %>% 
  dplyr::select(`ID paper`, `Year of publication`, Country, Area, `Wallace bioregion`,`Number of species`, Energy, 
                How, Where, When, Why,  `Geographical scale`, `Ecological scale`, `Tracking method`, Ecosystem,
                Spatial_scale) 
papers_reduced_na <- papers_reduced %>% 
  drop_na(Energy)

# Change the 10-13 to 13 in number of species
papers_reduced <- papers_reduced %>%
  mutate(across(`Number of species`, str_replace, '10-13', '13')) %>% 
  mutate(across(`Tracking method`, str_replace, 'Radar', 'radar')) 


unique_values = apply(dplyr::select(papers_reduced, -`ID paper`), 2, FUN = function(x){get_unique_values(x)})

unique_values_df = t(plyr::ldply(unique_values, rbind, .id = NULL)) %>% 
  as_tibble() %>% 
  `colnames<-`(colnames(papers_reduced)[-1])



write_csv(unique_values_df, file = "output/processed/unique_values.csv", col_names = T)

write_csv2(papers_reduced, file = "output/processed/papers_reduced.csv", col_names = T)

#---------------------

names(papers_reduced)

# Change doubt in how, where and hy by Yes; but check

papers_reduced <- papers_reduced %>%
  mutate(across(c(`How`, "Where", "Why", "When"), str_replace, 'Doubt', 'Yes'))

papers_final = papers_reduced %>% 
  dplyr::select(id_paper = `ID paper`,
                year_publication = "Year of publication", 
                country = "Country",
                area = "Area",
                wallace_bioregion = "Wallace bioregion",
                number_sp = "Number of species",
                energy = "Energy",
                how = "How",
                where = "Where",
                when = "When",
                why = "Why",
                geographical_scale = "Geographical scale",
                organizational_lvl = "Ecological scale",
                tracking_method = "Tracking method",
                ecosystem = "Ecosystem",
                spatial_scale = "Spatial_scale") %>% 
  mutate(spatial_ext = as.numeric(factor(geographical_scale, levels = c("local","regional","continental","global"))), 
         number_sp= as.numeric(number_sp), 
         year_publication= as.numeric(year_publication), 
         organizational_lvl_fact= organizational_lvl,
         organizational_lvl = as.numeric(factor(organizational_lvl, levels = c("individuals", "populations", "species", "communities", "ecosystems"))))

str(papers_final)

get_unique_values(papers_final$wallace_bioregion)
get_unique_values(papers_final$energy)
get_unique_values(papers_final$geographical_scale)
get_unique_values(papers_final$organizational_lvl)
get_unique_values(papers_final$spatial_scale)
get_unique_values(papers_final$how)
get_unique_values(papers_final$tracking_method)





##############################################################################
# 1. NMDS ordination of models

helperFunction <- function(x){
  ifelse(x=="yes", 1,0)
}



df_ord = papers_final %>% 
  #dplyr::select(id_paper, year_publication, number_sp, organizational_lvl, spatial_ext) %>% 
  dplyr::select(id_paper, year_publication, number_sp, organizational_lvl, spatial_ext, how, where, when, why) %>% 
  # dplyr::select(doi, dynamic:process_interactions, species_count, organizational_lvl, spatial_ext, spatial_res) %>% 
  mutate(across(c("how", "where", "why", "when"), helperFunction)) %>% 
  mutate(sum_components = rowSums(across(where(is.numeric)))) %>% 
  mutate(across(starts_with(c("how", "where", "why", "when")), as.logical)) %>% 
  mutate(species_count = log10(as.numeric(number_sp))) %>% 
  drop_na() %>% # Check NA in 20 papers
  select(-number_sp) %>% 
  column_to_rownames("id_paper") %>% 
  as_tibble(rownames = NA)

papers_dist = cluster::daisy(df_ord, metric="gower")

NMDS_papers = metaMDS(papers_dist, k = 2, trymax = 1000)
stressplot(NMDS_papers)
plot(NMDS_papers, display = "sites")

df_plot = NMDS_papers$points %>% 
  as_tibble(rownames = NA) %>% 
  rownames_to_column("id_paper") %>% 
  mutate(id_paper= as.numeric(id_paper)) %>% 
  left_join(papers_final, by = "id_paper")


df_plot$tracking_method <- as.factor(df_plot$tracking_method)
unique(df_plot$energy)
df_plot$energy = factor(df_plot$energy, levels = c("Eolic terrestrial", "Hydroelectric" ,
                                                   "Offshore eolic", "Solar and termosolar",
                                                   "Tidal power", "Other"))
df_plot <- df_plot %>% mutate(how=recode(how, 
                                         `1`="Yes",
                                         `0`="No")) 
df_env = df_plot %>% 
  dplyr::select(energy) %>% 
  dummy_cols(split = ",", remove_selected_columns = T)



env_coords = envfit(NMDS_papers, df_env) 
env_coords_df = as.data.frame(env_coords$vectors$arrows * sqrt(env_coords$vectors$r))

df <- tibble::rownames_to_column(env_coords_df, "Energy")
df <- df %>%
  mutate(Energy = str_remove_all(Energy, "energy_"))

rownames(df) <- df[,1]
env_coords_df <- df


###########################################################

#### Tracking method and energy ############

# Change color
library(RColorBrewer)

# View a single RColorBrewer palette by specifying its name
display.brewer.pal(n = 6, name = 'Dark2')
# Hexadecimal color specification 
brewer.pal(n = 6, name = "Dark2")



env_nmds <- ggplot(df_plot, aes(x = MDS1, y = MDS2)) +
  geom_point(aes(MDS1, MDS2, colour = factor(df_plot$tracking_method)), size = 2.2, alpha= 0.8) + #adds site points to plot, shape determined by Landuse, colour determined by Management 
  scale_color_manual(values=c('#1B9E77', '#D95F02', '#7570B3', "#E7298A",'grey30', '#66A61E', '#E6AB02')) +
  scale_fill_manual(values=c('#1B9E77', '#D95F02', '#7570B3', "#E7298A",'grey30', '#66A61E', '#E6AB02')) +
  coord_fixed()+
  theme_classic()+ 
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))+
  labs(colour = "Tracking method")+ # add legend labels for Management and Landuse
  theme(legend.position = "right", legend.text = element_text(size = 12), legend.title = element_text(size = 12), axis.text = element_text(size = 10)) # add legend at right of plot

env_nmds+
  geom_segment(data = env_coords_df, aes(x=0, xend= NMDS1, y=0, yend= NMDS2), inherit.aes = F, 
               arrow = arrow(length = unit(0.5, "cm")), colour="#180B2E", lwd=0.3) +
  ggrepel::geom_text_repel(data = env_coords_df, aes(x=NMDS1, y=NMDS2, label = rownames(env_coords_df)), cex = 4, direction = "both", segment.size = 0.25) #add labels for species, use ggrepel::geom_text_repel so that labels do not overlap

###################################################



df_plot$tracking_method <- as.factor(df_plot$tracking_method)
unique(df_plot$energy)
df_plot$energy = factor(df_plot$energy, levels = c("Eolic terrestrial", "Hydroelectric" ,
                                                   "Offshore eolic", "Solar and termosolar",
                                                   "Tidal power", "Other"))
df_plot$tracking_method = factor(df_plot$tracking_method, levels = c("Bioacustic", "Biologgers",
                                                                     "Isotopes", "Marks",
                                                   "No tracking", "Other", "radar"))
df_plot <- df_plot %>% mutate(how=recode(how, 
                                         `1`="Yes",
                                         `0`="No")) 

df_env = df_plot %>% 
  dplyr::select(tracking_method) %>% 
  dummy_cols(split = ",", remove_selected_columns = T)



env_coords = envfit(NMDS_papers, df_env) 
env_coords_df = as.data.frame(env_coords$vectors$arrows * sqrt(env_coords$vectors$r))
env_coords_df <- cbind(env_coords_df, pval = env_coords$vectors$pvals) #add pvalues to dataframe so you can select species which are significant
df <- tibble::rownames_to_column(env_coords_df, "Tracking method")
df <- df %>%
  mutate(`Tracking method` = str_remove_all(`Tracking method` , "tracking_method_"),
         significative= case_when(
           pval < 0.05 ~ "signficative",
           pval >= 0.05 ~ "no signficative",
          TRUE ~ NA
         ))

rownames(df) <- df[,1]
env_coords_df <- df
env_coords_df$significative <- as.factor(env_coords_df$significative)

# View a single RColorBrewer palette by specifying its name
display.brewer.pal(n = 5, name = 'Dark2')
# Hexadecimal color specification 
brewer.pal(n = 5, name = "Dark2")
library(ggnewscale)

env_nmds <- ggplot(df_plot, aes(x = MDS1, y = MDS2)) +
  geom_point(aes(MDS1, MDS2, colour = factor(df_plot$energy)), size = 2.2, alpha= 0.8) + #adds site points to plot, shape determined by Landuse, colour determined by Management 
  scale_color_manual(values=c('#1B9E77', '#D95F02', '#7570B3', "#E7298A",'#66A61E', 'grey30', '#E6AB02')) +
  scale_fill_manual(values=c('#1B9E77', '#D95F02', '#7570B3', "#E7298A",'#66A61E', 'grey30', '#E6AB02')) +
  coord_fixed()+
  theme_classic()+ 
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))+
  labs(colour = "Energy")+ # add legend labels for Management and Landuse
  theme(legend.position = "right", legend.text = element_text(size = 12), legend.title = element_text(size = 12), axis.text = element_text(size = 10)) # add legend at right of plot

env_nmds+
  # start a new scale
  new_scale_colour() +
  geom_segment(data = env_coords_df, aes(x=0, xend= NMDS1, y=0, yend= NMDS2, colour = significative), inherit.aes = F, 
               arrow = arrow(length = unit(0.5, "cm")), lwd=0.8, show.legend = FALSE) +
  
  scale_color_manual(values=c('grey30', 'blue')) +
  ggrepel::geom_text_repel(data = env_coords_df, aes(x=NMDS1, y=NMDS2, label = rownames(env_coords_df)), cex = 4, direction = "both", segment.size = 0.25) #add labels for species, use ggrepel::geom_text_repel so that labels do not overlap


#### Ecosystem and energy #######

df_env = df_plot %>% 
  dplyr::select(energy) %>% 
  dummy_cols(split = ",", remove_selected_columns = T)



env_coords = envfit(NMDS_papers, df_env) 
env_coords_df = as.data.frame(env_coords$vectors$arrows * sqrt(env_coords$vectors$r))

df <- tibble::rownames_to_column(env_coords_df, "Energy")
df <- df %>%
  mutate(Energy = str_remove_all(Energy, "energy_"))

rownames(df) <- df[,1]
env_coords_df <- df


# View a single RColorBrewer palette by specifying its name
display.brewer.pal(n = 8, name = 'Dark2')
# Hexadecimal color specification 
brewer.pal(n = 8, name = "Dark2")


levels(df_plot$ecosystem)

df_plot$Ecosystem = factor(df_plot$ecosystem, levels = c("farmlands", "forests",
                                                                     "grasslands/shrublands/Savannahs", "mountains",
                                                                     "oceans/coasts", "other", "freshwaters", "peatlands"))

env_nmds <- ggplot(df_plot, aes(x = MDS1, y = MDS2)) +
  geom_point(aes(MDS1, MDS2, colour = factor(df_plot$Ecosystem)), size = 2.2, alpha= 0.8) + #adds site points to plot, shape determined by Landuse, colour determined by Management 
  scale_color_manual(values=c("#1B9E77", "#D95F02", "#7570B3", "#E7298A" ,"#66A61E", "#E6AB02", "#A6761D", "#666666")) +
  scale_fill_manual(values=c("#1B9E77", "#D95F02", "#7570B3", "#E7298A" ,"#66A61E", "#E6AB02", "#A6761D", "#666666")) +
  coord_fixed()+
  theme_classic()+ 
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))+
  labs(colour = "Ecosystem")+ # add legend labels for Management and Landuse
  theme(legend.position = "right", legend.text = element_text(size = 12), legend.title = element_text(size = 12), axis.text = element_text(size = 10)) # add legend at right of plot

env_nmds+
  geom_segment(data = env_coords_df, aes(x=0, xend= NMDS1, y=0, yend= NMDS2), inherit.aes = F, 
               arrow = arrow(length = unit(0.5, "cm")), colour="#180B2E", lwd=0.3) +
  ggrepel::geom_text_repel(data = env_coords_df, aes(x=NMDS1, y=NMDS2, label = rownames(env_coords_df)), cex = 4, direction = "both", segment.size = 0.25) #add labels for species, use ggrepel::geom_text_repel so that labels do not overlap

#### Ecological level

df_plot$ecological_scale <- cut(df_plot$organizational_lvl,4, labels=c("individuals", "populations", "species", "communities"))


levels(df_plot$ecological_scale)


env_nmds <- ggplot(df_plot, aes(x = MDS1, y = MDS2)) +
  geom_point(aes(MDS1, MDS2, colour = factor(df_plot$ecological_scale)), size = 2.2, alpha= 0.8) + #adds site points to plot, shape determined by Landuse, colour determined by Management 
  scale_color_manual(values=c("#1B9E77", "#D95F02", "#7570B3", "#E7298A" ,"#66A61E", "#E6AB02", "#A6761D", "#666666")) +
  scale_fill_manual(values=c("#1B9E77", "#D95F02", "#7570B3", "#E7298A" ,"#66A61E", "#E6AB02", "#A6761D", "#666666")) +
  coord_fixed()+
  theme_classic()+ 
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))+
  labs(colour = "Ecological scale")+ # add legend labels for Management and Landuse
  theme(legend.position = "right", legend.text = element_text(size = 12), legend.title = element_text(size = 12), axis.text = element_text(size = 10)) # add legend at right of plot

env_nmds+
  geom_segment(data = env_coords_df, aes(x=0, xend= NMDS1, y=0, yend= NMDS2), inherit.aes = F, 
               arrow = arrow(length = unit(0.5, "cm")), colour="#180B2E", lwd=0.3) +
  ggrepel::geom_text_repel(data = env_coords_df, aes(x=NMDS1, y=NMDS2, label = rownames(env_coords_df)), cex = 4, direction = "both", segment.size = 0.25) #add labels for species, use ggrepel::geom_text_repel so that labels do not overlap



###### HOW, WHERE, WHEN and WHY as shapes #######

df_plot <- df_plot %>% mutate(where=recode(where, 
                                         `1`="Yes",
                                         `0`="No")) 

df_plot <- df_plot %>% mutate(how=recode(how, 
                                         `1`="Yes",
                                         `0`="No")) 
df_plot <- df_plot %>% mutate(when=recode(when, 
                                         `1`="Yes",
                                         `0`="No")) 
df_plot <- df_plot %>% mutate(why=recode(why, 
                                         `1`="Yes",
                                         `0`="No")) 


env_nmds <- ggplot(df_plot, aes(x = MDS1, y = MDS2)) +
  geom_point(aes(MDS1, MDS2, colour = factor(df_plot$tracking_method), shape = factor(df_plot$how)), size = 2.2, alpha= 0.8) + #adds site points to plot, shape determined by Landuse, colour determined by Management 
  scale_color_manual(values=c('#1B9E77', '#D95F02', '#7570B3', "#E7298A",'grey30', '#66A61E', '#E6AB02')) +
  scale_fill_manual(values=c('#1B9E77', '#D95F02', '#7570B3', "#E7298A",'grey30', '#66A61E', '#E6AB02')) +
  coord_fixed()+
  theme_classic()+ 
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))+
  labs(colour = "Tracking method", shape= "How")+ # add legend labels for Management and Landuse
  theme(legend.position = "right", legend.text = element_text(size = 12), legend.title = element_text(size = 12), axis.text = element_text(size = 10)) # add legend at right of plot

env_nmds+
  geom_segment(data = env_coords_df, aes(x=0, xend= NMDS1, y=0, yend= NMDS2), inherit.aes = F, 
               arrow = arrow(length = unit(0.5, "cm")), colour="#180B2E", lwd=0.3) +
  ggrepel::geom_text_repel(data = env_coords_df, aes(x=NMDS1, y=NMDS2, label = rownames(env_coords_df)), cex = 4, direction = "both", segment.size = 0.25) #add labels for species, use ggrepel::geom_text_repel so that labels do not overlap


env_nmds <- ggplot(df_plot, aes(x = MDS1, y = MDS2)) +
  geom_point(aes(MDS1, MDS2, colour = factor(df_plot$tracking_method), shape = factor(df_plot$where)), size = 2.2, alpha= 0.8) + #adds site points to plot, shape determined by Landuse, colour determined by Management 
  scale_color_manual(values=c('#1B9E77', '#D95F02', '#7570B3', "#E7298A",'grey30', '#66A61E', '#E6AB02')) +
  scale_fill_manual(values=c('#1B9E77', '#D95F02', '#7570B3', "#E7298A",'grey30', '#66A61E', '#E6AB02')) +
  coord_fixed()+
  theme_classic()+ 
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))+
  labs(colour = "Tracking method", shape= "Where")+ # add legend labels for Management and Landuse
  theme(legend.position = "right", legend.text = element_text(size = 12), legend.title = element_text(size = 12), axis.text = element_text(size = 10)) # add legend at right of plot

env_nmds+
  geom_segment(data = env_coords_df, aes(x=0, xend= NMDS1, y=0, yend= NMDS2), inherit.aes = F, 
               arrow = arrow(length = unit(0.5, "cm")), colour="#180B2E", lwd=0.3) +
  ggrepel::geom_text_repel(data = env_coords_df, aes(x=NMDS1, y=NMDS2, label = rownames(env_coords_df)), cex = 4, direction = "both", segment.size = 0.25) #add labels for species, use ggrepel::geom_text_repel so that labels do not overlap


env_nmds <- ggplot(df_plot, aes(x = MDS1, y = MDS2)) +
  geom_point(aes(MDS1, MDS2, colour = factor(df_plot$tracking_method), shape = factor(df_plot$why)), size = 2.2, alpha= 0.8) + #adds site points to plot, shape determined by Landuse, colour determined by Management 
  scale_color_manual(values=c('#1B9E77', '#D95F02', '#7570B3', "#E7298A",'grey30', '#66A61E', '#E6AB02')) +
  scale_fill_manual(values=c('#1B9E77', '#D95F02', '#7570B3', "#E7298A",'grey30', '#66A61E', '#E6AB02')) +
  coord_fixed()+
  theme_classic()+ 
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))+
  labs(colour = "Tracking method", shape= "Why")+ # add legend labels for Management and Landuse
  theme(legend.position = "right", legend.text = element_text(size = 12), legend.title = element_text(size = 12), axis.text = element_text(size = 10)) # add legend at right of plot

env_nmds+
  geom_segment(data = env_coords_df, aes(x=0, xend= NMDS1, y=0, yend= NMDS2), inherit.aes = F, 
               arrow = arrow(length = unit(0.5, "cm")), colour="#180B2E", lwd=0.3) +
  ggrepel::geom_text_repel(data = env_coords_df, aes(x=NMDS1, y=NMDS2, label = rownames(env_coords_df)), cex = 4, direction = "both", segment.size = 0.25) #add labels for species, use ggrepel::geom_text_repel so that labels do not overlap


env_nmds <- ggplot(df_plot, aes(x = MDS1, y = MDS2)) +
  geom_point(aes(MDS1, MDS2, colour = factor(df_plot$tracking_method), shape = factor(df_plot$when)), size = 2.2, alpha= 0.8) + #adds site points to plot, shape determined by Landuse, colour determined by Management 
  scale_color_manual(values=c('#1B9E77', '#D95F02', '#7570B3', "#E7298A",'grey30', '#66A61E', '#E6AB02')) +
  scale_fill_manual(values=c('#1B9E77', '#D95F02', '#7570B3', "#E7298A",'grey30', '#66A61E', '#E6AB02')) +
  coord_fixed()+
  theme_classic()+ 
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))+
  labs(colour = "Tracking method", shape= "When")+ # add legend labels for Management and Landuse
  theme(legend.position = "right", legend.text = element_text(size = 12), legend.title = element_text(size = 12), axis.text = element_text(size = 10)) # add legend at right of plot

env_nmds+
  geom_segment(data = env_coords_df, aes(x=0, xend= NMDS1, y=0, yend= NMDS2), inherit.aes = F, 
               arrow = arrow(length = unit(0.5, "cm")), colour="#180B2E", lwd=0.3) +
  ggrepel::geom_text_repel(data = env_coords_df, aes(x=NMDS1, y=NMDS2, label = rownames(env_coords_df)), cex = 4, direction = "both", segment.size = 0.25) #add labels for species, use ggrepel::geom_text_repel so that labels do not overlap


write_csv2(df_plot, "output/processed/df_plot.csv")



##################################
library(treemapify)
names(df_plot)
head(df_plot)

df_plot$organizational_lvl

#convert points column from numeric to factor with four levels

df_plot$ecological_scale <- cut(df_plot$organizational_lvl,4, labels=c("individuals", "populations", "species", "communities"))


head(G20)
str(G20)


table_freq <- df_plot %>%
  dplyr::select(wallace_bioregion, area, energy, ecological_scale, Ecosystem, country, year_publication) %>%
  mutate_if(is.character,as.factor) %>% 
  group_by(wallace_bioregion, area, energy, ecological_scale, Ecosystem, country, year_publication) %>%
  count() %>% 
  drop_na() 
table_freq <- as_tibble(table_freq)
str(table_freq$area)
  
ggplot(table_freq %>% filter(! energy== "Other"), aes(area = n, fill = wallace_bioregion, label = area, subgroup = wallace_bioregion)) +
  geom_treemap() +
  geom_treemap_text(grow = T, reflow = T, colour = "black") +
  facet_wrap( ~ energy) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "bottom") +
  labs(
    title = "The G-20 major economies by hemisphere",
    caption = "The area of each tile represents the country's GDP as a
      proportion of all countries in that hemisphere",
    fill = "Region"
  )

table_freq <- df_plot %>%
  dplyr::select(wallace_bioregion, country, energy) %>%
  mutate_if(is.character,as.factor) %>% 
  group_by(wallace_bioregion, country, energy) %>%
  count() %>% 
  drop_na() 
table_freq <- as_tibble(table_freq)
str(table_freq$area)

write_csv2(table_freq, "output/processed/tabla_freq_country.csv")

ggplot(table_freq %>% filter(! energy== "Other"), aes(area = n, fill = wallace_bioregion, label = country, subgroup = wallace_bioregion)) +
  geom_treemap() +
  geom_treemap_text(grow = T, reflow = T, colour = "black") +
  facet_wrap( ~ energy) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "bottom") +
  labs(
    #title = "The G-20 major economies by hemisphere",
    caption = "The area of each tile represents the frequency of studies as a
      proportion of all bioregions in that energy",
    fill = "Wallace Bioregion"
  )

# The exploratory block below depends on tabla_freq_country_corr.csv, which is
# produced by 02_country_corrections.R. Skip silently on first run; re-source
# 01 after running 02 to also reproduce the exploratory treemaps and the
# animated_treemap.gif. None of the downstream pipeline figures depend on it.
if (!file.exists("output/processed/tabla_freq_country_corr.csv")) {
  message("Skipping exploratory treemap block: run 02_country_corrections.R first, then re-source this script.")
} else {

table_freq_cor <- read_csv2("output/processed/tabla_freq_country_corr.csv")
table_freq_cor <- table_freq_cor %>%
  dplyr::select(wallace_bioregion, country, energy) %>%
  mutate_if(is.character,as.factor) %>% 
  group_by(wallace_bioregion, country, energy) %>%
  count() %>% 
  drop_na() 


ggplot(table_freq_cor %>% filter(! energy== "Other"), aes(area = n, fill = wallace_bioregion, label = country, subgroup = wallace_bioregion)) +
  geom_treemap() +
  geom_treemap_text(grow = T, reflow = T, colour = "black") +
  facet_wrap( ~ energy) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "bottom") +
  labs(
    #title = "The G-20 major economies by hemisphere",
    caption = "The area of each tile represents the frequency of studies as a
      proportion of all countries in that energy",
    fill = "Wallace Bioregion"
  )

ggplot(table_freq_cor %>% filter(! energy== "Other"), aes(area = n, fill = wallace_bioregion, label =country, subgroup = wallace_bioregion)) +
  geom_treemap() +
  geom_treemap_text(grow = F, reflow = T, colour = "black") +
  facet_wrap( ~ energy) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "bottom") +
  labs(
    #title = "The G-20 major economies by hemisphere",
    caption = "The area of each tile represents the frequency of studies as a
      proportion of all countries in that energy",
    fill = "Wallace Bioregion"
  )


table_freq <- df_plot %>%
  dplyr::select(energy, ecological_scale) %>%
  mutate_if(is.character,as.factor) %>% 
  group_by(energy, ecological_scale) %>%
  count() %>% 
  drop_na() 
table_freq <- as_tibble(table_freq)


ggplot(table_freq %>% filter(! energy== "Other"), aes(area = n, fill = ecological_scale, label =ecological_scale)) +
  geom_treemap() +
  geom_treemap_text(grow = F, reflow = T, colour = "black") +
  facet_wrap( ~ energy) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "bottom") +
  labs(
    #title = "The G-20 major economies by hemisphere",
    caption = "The area of each tile represents the frequency of studies as a
      proportion of all ecological scales in that energy",
    fill = "Ecological scale"
  )


table_freq <- df_plot %>%
  dplyr::select(energy, tracking_method) %>%
  mutate_if(is.character,as.factor) %>% 
  group_by(energy, tracking_method) %>%
  count() %>% 
  drop_na() 
table_freq <- as_tibble(table_freq) %>% 
  filter(! energy== "Other") %>% 
  filter(! tracking_method== "Other" )


ggplot(table_freq, aes(area = n, fill = tracking_method, label =tracking_method)) +
  geom_treemap() +
  geom_treemap_text(grow = F, reflow = T, colour = "black") +
  facet_wrap( ~ energy) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "bottom") +
  labs(
    #title = "The G-20 major economies by hemisphere",
    caption = "The area of each tile represents the frequency of studies as a
      proportion of all tracking methods used to study movement in that energy",
    fill = "Tracking method"
  )

#### Bar graph for the years
library(viridis)
library(hrbrthemes)
table_freq <- df_plot %>%
  dplyr::select(energy, year_publication) %>%
  mutate_if(is.character,as.factor) %>% 
  group_by(energy, year_publication) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

ggplot(data = df_plot, aes(x = year_publication, fill = energy)) +
  geom_bar(position = "fill") + ylab("proportion") +
  stat_count(geom = "text", 
             aes(label = stat(count)),
             position=position_fill(vjust=0.5), colour="black")+
  scale_fill_brewer(palette = "Set1") +
  #scale_fill_viridis(discrete = T) +
  theme_ipsum() +
  xlab("Publication year")

# Plot again
unique(table_freq$energy)
table(table_freq$energy)
table(table_freq$energy, table_freq$year_publication )

ggplot(table_freq %>% filter(! energy== "Other"), aes(x=year_publication, y=n, fill=energy)) + 
  geom_area(alpha=0.6 , size=.5, colour="white") +
  scale_fill_viridis(discrete = T) +
  theme_ipsum() +
  xlab("Publication year") +
  ylab("Number of studies") +
  labs(
    #title = "The G-20 major economies by hemisphere",
    caption = "Solar and thermosolar are excluded from the graph; only 1 study in 2022",
    fill = "Renewable energy"
  )
  
library(sjlabelled)

pp <- as.data.frame(xtabs(n~year_publication+energy, table_freq))
pp$year_publication <- as_numeric(pp$year_publication)
ggplot(pp %>% filter(! energy== "Other"), aes(x=year_publication, y=Freq, fill=energy)) + 
  geom_area(alpha=0.6 , size=.5, colour="white") +
  scale_fill_viridis(discrete = T) +
  theme_ipsum() +
  xlab("Publication year") +
  ylab("Number of studies") +
  labs(
    #title = "The G-20 major economies by hemisphere",
    #caption = "",
    fill = "Renewable energy"
  )

# Combining both eolic


table_freq_comb <-  table_freq %>% 
  mutate(
    energy_comb = fct_recode(as.factor(energy), "Eolic" = "Eolic terrestrial","Eolic" = "Offshore eolic"),
    proportion= n/nrow(df_plot))
  
pp_comb <- as.data.frame(xtabs(n~year_publication+energy_comb, table_freq_comb))
pp_comb$year_publication <- as_numeric(pp_comb$year_publication)
ggplot(pp_comb %>% filter(! energy_comb== "Other"), aes(x=year_publication, y=Freq, fill=energy_comb)) + 
  geom_area(alpha=0.6 , size=.5, colour="white") +
  scale_fill_viridis(discrete = T) +
  theme_ipsum() +
  xlab("Publication year") +
  ylab("Number of studies") +
  labs(
    #title = "The G-20 major economies by hemisphere",
    #caption = "",
    fill = "Renewable energy"
  )





# Plot
ggplot(table_freq, aes(x=year_publication, y=freq, fill=energy)) + 
  geom_area(alpha=0.6 , linewidth=1, colour="black")


table_freq <- df_plot %>%
  dplyr::select(energy, tracking_method) %>%
  mutate_if(is.character,as.factor) %>% 
  group_by(energy, tracking_method) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))
table_freq <- as_tibble(table_freq) %>% 
  filter(! energy== "Other") %>% 
  filter(! tracking_method== "Other" )

ggplot(table_freq, aes(area = freq, fill = tracking_method, label =tracking_method)) +
  geom_treemap() +
  geom_treemap_text(grow = F, reflow = T, colour = "black") +
  facet_wrap( ~ energy) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "bottom") +
  labs(
    #title = "The G-20 major economies by hemisphere",
    caption = "The area of each tile represents the frequency of studies as a
      proportion of all tracking methods used to study movement in that energy",
    fill = "Tracking method"
  )





############ Animate graph #######

library(gganimate)
library(gapminder)

gapminder

p <- ggplot(gapminder, aes(
  label = country,
  area = pop,
  subgroup = continent,
  fill = lifeExp
)) +
  geom_treemap(layout = "fixed") +
  geom_treemap_text(layout = "fixed", place = "centre", grow = TRUE, colour = "white") +
  geom_treemap_subgroup_text(layout = "fixed", place = "centre") +
  geom_treemap_subgroup_border(layout = "fixed") +
  transition_time(year) +
  ease_aes('linear') +
  labs(title = "Year: {frame_time}")

anim_save("output/figures/animated_treemap.gif", p, nframes = 48)



table_freq <- df_plot %>%
  dplyr::select(energy, year_publication) %>%
  mutate_if(is.character,as.factor) %>% 
  group_by(energy, year_publication) %>%
  count() %>% 
  drop_na() 
table_freq <- as_tibble(table_freq)
str(table_freq)
table_freq$year_publication <- as.integer(table_freq$year_publication )

p <- ggplot(table_freq, aes(
  label = energy,
  area = n,
  fill = energy
)) +
  geom_treemap(layout = "fixed") +
  geom_treemap_text(layout = "fixed", place = "centre", grow = TRUE, colour = "white") +
  transition_time(year_publication) +
  ease_aes('linear') +
  labs(title = "Year: {frame_time}")

anim_save("output/figures/animated_treemap.gif", p, nframes = 48)




table_freq_year <- table_freq %>% 
  mutate(year.cumsum=cumsum(n))

p <- ggplot(table_freq_year, aes(
  label = energy,
  area = year.cumsum,
  fill = energy
)) +
  geom_treemap(layout = "fixed") +
  geom_treemap_text(layout = "fixed", place = "centre", grow = TRUE, colour = "white") +
  transition_time(year_publication) +
  ease_aes('linear') +
  labs(title = "Year: {frame_time}")

anim_save("output/figures/animated_treemap.gif", p, nframes = 20)

}  # end of conditional exploratory block (depends on 02_country_corrections.R)
