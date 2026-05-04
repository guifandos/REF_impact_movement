df_ord = papers_final %>% 
  #dplyr::select(id_paper, year_publication, number_sp, organizational_lvl, spatial_ext) %>% 
  dplyr::select(id_paper, year_publication, number_sp, organizational_lvl, spatial_ext, how, where, when, why) %>% 
  # dplyr::select(doi, dynamic:process_interactions, species_count, organizational_lvl, spatial_ext, spatial_res) %>% 
  mutate(across(c("how", "where", "why", "when"), helperFunction)) %>% 
  mutate(across(starts_with(c("how", "where", "why", "when")), as.logical)) %>% 
  mutate(species_count = log10(as.numeric(number_sp))) %>% 
  drop_na() %>% # Check NA in 20 papers
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


df_plot$tracking_method <- as.factor(df_plot$tracking_method)

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



dune.envfit <- envfit(NMDS_papers, df_env, permutations = 999) # this fits environmental vectors
dune.spp.fit <- envfit(NMDS_papers, df_ord, permutations = 999) # this fits species vectors

site.scrs <- as.data.frame(scores(NMDS_papers, display = "sites")) #save NMDS results into dataframe
site.scrs <- cbind(site.scrs, energy = df_plot$energy) #add grouping variable "Management" to dataframe
site.scrs <- cbind(site.scrs, tracking = df_plot$tracking_method) #add grouping variable of cluster grouping to dataframe
#site.scrs <- cbind(site.scrs, Site = rownames(site.scrs)) #add site names as variable if you want to display on plot

head(site.scrs)

spp.scrs <- as.data.frame(scores(dune.spp.fit, display = "vectors")) #save species intrinsic values into dataframe
spp.scrs <- cbind(spp.scrs, id_paper = rownames(spp.scrs)) #add species names to dataframe
spp.scrs <- cbind(spp.scrs, pval = dune.spp.fit$vectors$pvals) #add pvalues to dataframe so you can select species which are significant
#spp.scrs<- cbind(spp.scrs, abrev = abbreviate(spp.scrs$Species, minlength = 6)) #abbreviate species names
sig.spp.scrs <- subset(spp.scrs, pval<=0.05) #subset data to show species significant at 0.05

head(spp.scrs)

env.scores.dune <- as.data.frame(scores(dune.envfit, display = "vectors")) #extracts relevant scores from envifit
env.scores.dune <- cbind(env.scores.dune, env.variables = rownames(env.scores.dune)) #and then gives them their names

env.scores.dune <- cbind(env.scores.dune, pval = dune.envfit$vectors$pvals) # add pvalues to dataframe
sig.env.scrs <- subset(env.scores.dune, pval<=0.05) #subset data to show variables significant at 0.05

head(env.scores.dune)

spp.scrs.fact <- as.data.frame(scores(dune.spp.fit, display = "factors")) #save species intrinsic values into dataframe
spp.scrs.fact <- cbind(spp.scrs.fact, id_paper = rownames(spp.scrs.fact)) #add species names to dataframe
spp.scrs.fact <- cbind(spp.scrs.fact, pval = dune.spp.fit$factors$pvals) #add pvalues to dataframe so you can select species which are significant


nmds.plot.dune <- ggplot(site.scrs, aes(x=NMDS1, y=NMDS2))+ #sets up the plot
  geom_point(aes(NMDS1, NMDS2, colour = factor(site.scrs$energy), shape = factor(site.scrs$tracking)), size = 2)+ #adds site points to plot, shape determined by Landuse, colour determined by Management
  coord_fixed()+
  theme_classic()+ 
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))+
  labs(colour = "energy", shape = "tracking")+ # add legend labels for Management and Landuse
  theme(legend.position = "right", legend.text = element_text(size = 12), legend.title = element_text(size = 12), axis.text = element_text(size = 10)) # add legend at right of plot

nmds.plot.dune + labs(title = "Basic ordination plot") #displays plot


nmds.plot.dune+
  geom_segment(data = sig.spp.scrs, aes(x = 0, xend=NMDS1, y=0, yend=NMDS2), arrow = arrow(length = unit(0.25, "cm")), colour = "grey10", lwd=0.3) + #add vector arrows of significant species
  ggrepel::geom_text_repel(data = sig.spp.scrs, aes(x=NMDS1, y=NMDS2, label = id_paper), cex = 3, direction = "both", segment.size = 0.25)+ #add labels for species, use ggrepel::geom_text_repel so that labels do not overlap
  labs(title = "Ordination with species vectors")


nmds.plot.dune+
  geom_segment(data = env.scores.dune, aes(x = 0, xend=NMDS1, y=0, yend=NMDS2), arrow = arrow(length = unit(0.25, "cm")), colour = "grey10", lwd=0.3) + #add vector arrows of significant env variables
  ggrepel::geom_text_repel(data = env.scores.dune, aes(x=NMDS1, y=NMDS2, label = env.variables), cex = 4, direction = "both", segment.size = 0.25)+ #add labels for env variables
  labs(title="Ordination with environmental vectors")

#################

country_data <-  read_csv2("output/processed/df_plot_corr.csv")
names(country_data)
country_data_freq <- country_data %>%
  dplyr::select(country_cor) %>%
  mutate_if(is.character,as.factor) %>% 
  group_by(country_cor) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>% 
  arrange(-freq)

year_data_freq <- df_ord %>%
  dplyr::select(year_publication) %>%
  mutate_if(is.character,as.factor) %>% 
  group_by(year_publication) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>% 
  arrange(-freq)

energy_data_freq <- papers_final %>%
  dplyr::select(energy) %>%
  mutate_if(is.character,as.factor) %>% 
  group_by(energy) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>% 
  arrange(-freq)


tracking_method_freq <- papers_final %>%
  dplyr::select(tracking_method) %>%
  mutate_if(is.character,as.factor) %>% 
  group_by(tracking_method) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>% 
  arrange(-freq)


how_freq <- papers_final %>%
  dplyr::select(how) %>%
  mutate_if(is.character,as.factor) %>% 
  group_by(how) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>% 
  arrange(-freq)

when_freq <- papers_final %>%
  dplyr::select(when) %>%
  mutate_if(is.character,as.factor) %>% 
  group_by(when) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>% 
  arrange(-freq)

why_freq <- papers_final %>%
  dplyr::select(why) %>%
  mutate_if(is.character,as.factor) %>% 
  group_by(why) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>% 
  arrange(-freq)

where_freq <- papers_final %>%
  dplyr::select(where) %>%
  mutate_if(is.character,as.factor) %>% 
  group_by(where) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>% 
  arrange(-freq)

helperFunction <- function(x){
  ifelse(x=="Yes", 1,0)
}

unique(papers_final$how)

type_freq <- papers_final %>% 
  dplyr::select(where, why, how, when) %>%
  mutate_all(helperFunction) %>% 
  mutate(sum = rowSums(across(where(is.numeric)))) %>% 
  mutate_if(is.numeric, as.factor) %>% 
  group_by(sum) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>% 
  arrange(-freq)
  


organizational_freq <- papers_final %>%
  dplyr::select(organizational_lvl_fact) %>%
  mutate_if(is.character,as.factor) %>% 
  group_by(organizational_lvl_fact) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>% 
  arrange(-freq)

ecosystem_freq <- papers_final %>%
  dplyr::select(ecosystem) %>%
  mutate_if(is.character,as.factor) %>% 
  group_by(ecosystem) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>% 
  arrange(-freq)

  
