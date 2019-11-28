# Rich's Script

library(tidyverse)
library(janitor)
library(directlabels)
library(DT)
library(kableExtra)
library(ggbeeswarm)
library(car)

mack_creek <- read_csv("mack_creek_vertebrates.csv") %>% 
  clean_names() %>% 
  filter(species == "DITE")

# --------------------
# Results A: Create data frame for salamander count graph
# --------------------

mack_creek_count <- mack_creek %>% 
  select(year, section) %>% 
  filter(section %in% c("OG", "CC")) %>% 
  group_by(year, section) %>% 
  summarize(count = n())

# ---------------------
# Create a GGplot of the count data
# ---------------------

ggplot(data = mack_creek_count, aes(x = year, y = count, group = section))+
  geom_line(aes(color = section), show.legend = FALSE)+
  geom_point(color = "gray50")+
  geom_dl(aes(label = section, color = section), method = list(dl.combine("last.points"), cex = 0.85))+
  scale_x_continuous(expand = c(0, 0),
                   limits = c(1993, 2019.5),
                   breaks = seq(1993, 2018, by = 5))+
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 800),
                     breaks = seq(0, 800, by = 200))+
  labs(x = "Year",
       y = "Count",
       title = "Salamander Counts by Section: 1993 - 2017")

# -----------------------------------------------------------------------------
# Results B: Create data frame for table (channel classification & forest type)
# -----------------------------------------------------------------------------

mack_c_table <- mack_creek %>% 
  mutate(section_name = if_else(section == "CC", "Clear Cut", "Old Growth")) %>% 
  select(year, section_name, unittype) %>%
  filter(year == 2017,
         unittype %in% c("C", "P", "SC"),
         section_name %in% c("Clear Cut", "Old Growth")) %>% 
  group_by(section_name, unittype) %>% 
  summarize(count = n()) %>% 
  pivot_wider(names_from = unittype, values_from = count) %>% 
  rename(Cascades = "C",
         Pool = "P",
         "Side Channel" = "SC",
         Section = "section_name")

mack_c_props <- mack_c_table %>% 
  adorn_percentages(denominator = "row") %>% 
  adorn_pct_formatting(digits = 1) %>% 
  adorn_ns(position = "front")

# --------------------
# Create contingency table using kable
# --------------------

kable(mack_c_props) %>% 
  kable_styling()

# -------------------------------------------------
# Results C: Use chi square to determine independence
# -------------------------------------------------

chi_mack <- mack_c_table %>%
  ungroup() %>% 
  select(-Section)

chi_mack_test <- chisq.test(chi_mack)

chi_mack_test

# -------------------------------------------------
# Results D: Compare weights of Pacific giant salamanders in clear cut and old growth forest sections of the creek in 2017
# -------------------------------------------------

# This data frame may be useful for a means comparison

mack_c_weights <- mack_creek %>% 
  select("year", "section", "weight") %>% 
  filter(section %in% c("OG", "CC"),
         year == 2017) %>% 
  group_by(year, section) %>% 
  summarize(mean_weight = round(mean(weight, na.rm = TRUE), 2)) %>% 
  ungroup() %>% 
  select(-year)

# This data frame will be used for box_plot & jitter_plot

mack_c_weights_graph <- mack_creek %>% 
  select("year", "section", "weight") %>% 
  filter(section %in% c("OG", "CC"),
         year == 2017)

# Create box_plot & jitter_plot (Remember - box plots compare medians, not means!)

ggplot(mack_c_weights_graph, aes(x = section, y = weight)) +
  geom_jitter(aes(color = section), show.legend = FALSE)+
  geom_boxplot(aes(color = section), alpha = 0.5, show.legend = FALSE)+
  scale_x_discrete(labels = c("Clear Cut", "Old Growth"))+
  labs(x = "Section",
       y = "Weight",
       title = "2017 Means Comparison: Old Growth vs Clear Cut")

# Conduct a means comparison of weights usins a 2-sample t.test


 mack_weights_cc <- mack_c_weights_graph %>% 
   filter(section == "CC") %>% 
   pull(weight)

 mack_weights_og <- mack_c_weights_graph %>% 
   filter(section == "OG") %>% 
   pull(weight)

 mack_means_comparison <- t.test(mack_weights_og, mack_weights_cc)
 mack_means_comparison
 
 # -----------------------------------------------------
 # Compare weights of Pacific giant salamanders in pools, cascades and side-channels of Mack Creek in 2017
 # -----------------------------------------------------
 
 mack_pcs_weights <- mack_creek %>% 
   select("year", "unittype", "weight", "section") %>% 
   filter(unittype %in% c("C", "P", "SC"),
          year == 2017,
          section %in% c("CC", "OG"))
 
 mack_pcs_summary <- mack_pcs_weights %>% 
   group_by(unittype) %>% 
   summarize(
     mean_weight = mean(weight, na.rm = TRUE),
     sd_weight = sd(weight, na.rm = TRUE),
     sample_size = n(),
     se_weight = sd(weight, na.rm = TRUE) / sqrt(n()),
     var_weight = var(weight, na.rm = TRUE))
 
 # Make a QQ plot

 # Make a gg_beeswarm plot to compare means
 
 ggplot()+
   geom_beeswarm(data = mack_pcs_weights,
                 aes(x = unittype, y = weight),
                 size = 1,
                 alpha = 0.6,
                 color = "dodgerblue") +
   geom_point(data = mack_pcs_summary,
              aes(x = unittype, y = mean_weight),
              color = "red",
              size = 2)+
   geom_errorbar(data = mack_pcs_summary,
                 aes(x = unittype,
                     ymin = mean_weight - sd_weight,
                     ymax = mean_weight + sd_weight),
                 width = 0.1,
                 color = "red") +
   scale_x_discrete(labels = c("Cascade", "Pool", "Side Channel"))
 
 # Maybe a qq plot?
 
 ggplot(data = mack_pcs_weights, aes(sample = weight))+
   geom_qq()+
   facet_wrap(~unittype)
 
 # Conduct a Laveene's test to check for equal variance
 
 leveneTest(weight ~ unittype, data = mack_pcs_weights)
 
 # If this is correct, retain the NULL hypothesis that variances are equal
 
mack_aov <- aov(weight ~ unittype, data = mack_pcs_weights)
summary(mack_aov)

TukeyHSD(mack_aov)