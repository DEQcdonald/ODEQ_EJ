library(tidyverse)
library(lubridate)
library(corrplot)
library(ggcorrplot)
library(ggplot2)


# read in the risk file that has the data for PM2.5, air toxics cancer risk, HAB advisories,
# pesticide load, ECSI sites, and LUST
ris <- read.csv("agu_data/merged_6env_with_deciles_v4.csv", stringsAsFactors = FALSE)

## read in the LUST csv with the LUST scores by GEOID
## I am expecting a file that one row for each tank, and for each tak it has
## the GEOID for the census block group, and score
lus_sc <- read.csv("agu_data/LUST_IDs_details2020_11_13_with_geoid.csv", stringsAsFactors = FALSE)

## create a score for each census block group
## the score can be the mean or the max, let's create a column for each
## replace lust_score with the actual name of the column that has he LUST scores
lus_cbg <- lus_sc %>% 
              group_by(GEOID) %>% 
              summarize(lus_av = mean(Score, na.rm = TRUE), lus_max = max(Score, na.rm = TRUE), lus_num = sum(!is.na(Score)))

## merge the lust scores with the other variables based on CBG GEOID
ris <- merge(ris, lus_cbg, by = "GEOID", all = TRUE)


## save the merged file as a csv as a convenience
write.csv(ris, "agu_data/merged_env_with_deciles_v5.csv", row.names = FALSE)



## function to create the boxplots
## it takes in three arguments - the merged data with deciles, the environmental variable
## you want the boxplot for, and the label for the y-axis
## it returns the boxplot
ee_dem_box <- function(ris, dem_var, ylabel = "") {
  ## box plot for cancer risk from air toxics
  minor <- ris %>% filter(min_quin == 1 | min_quin == 10) %>% select(!!dem_var, value = MINORPCT, decile = min_quin) %>% mutate( cat = "MINORPCT")
  lowinc <- ris %>% filter(inc_quin == 1 | inc_quin == 10) %>% select(!!dem_var, value = LOWINCPCT, decile = inc_quin) %>% mutate( cat = "LOWINCPCT")
  lhs <- ris %>% filter(lhs_quin == 1 | lhs_quin == 10) %>% select(!!dem_var, value = LESSHSPCT, decile = lhs_quin) %>% mutate( cat = "LESSHSPCT")
  un5 <- ris %>% filter(un5_quin == 1 | un5_quin == 10) %>% select(!!dem_var, value = UNDER5PCT, decile = un5_quin) %>% mutate( cat = "UNDER5PCT")
  o64 <- ris %>% filter(o64_quin == 1 | o64_quin == 10) %>% select(!!dem_var, value = OVER64PCT, decile = o64_quin) %>% mutate( cat = "OVER64PCT")
  lin  <- ris %>% filter(lin_quin == 1 | lin_quin == 10) %>% select(!!dem_var, value = LINGISOPCT, decile = lin_quin) %>% mutate( cat = "LINGISOPCT")
  df <- rbind( minor, lowinc, lhs, lin, un5, o64) 
  df$decile <- factor(df$decile,
                      levels = c(1, 10),
                      labels = c("10th %", "90th %"))
  df$cat_ordered <- factor(df$cat, 
                           levels = c("MINORPCT","LESSHSPCT", "LOWINCPCT", "LINGISOPCT", "UNDER5PCT", "OVER64PCT"),
                           labels = c("% Minority", "% Less than HS education", "% Low income", "% Linguistic isolation", "% Under 5 years", "% Over 64 years"))
  
  g <- ggplot(df) + 
    theme_bw() + 
    geom_boxplot(aes(x = as.factor(decile), y = !!dem_var)) + 
    ylab(ylabel) +
    xlab("Deciles") + 
    facet_grid(~cat_ordered, labeller = label_wrap_gen(width = 10, multi_line = TRUE))
  return(g)
  
}

## call the function to create the boxplot for LUST scores
lus_g <- ee_dem_box(ris, quo(lus_av), "LUST scores")
## save the graph as a png
ggsave("agu_data/lus_av_box.png", plot = lus_g, width = 9, height = 4)
## display the graph
lus_g


lus_g <- ee_dem_box(ris, quo(lus_max), "LUST scores MAX")
## save the graph as a png
ggsave("agu_data/lus_max_box.png", plot = lus_g, width = 9, height = 4)
## display the graph
lus_g




min_tbl <- var_summ(ris, quo(min_dec))
un5_tbl <- var_summ(ris, quo(un5_dec))
o64_tbl <- var_summ(ris, quo(o64_dec))
lin_tbl <- var_summ(ris, quo(lin_dec))
lhs_tbl <- var_summ(ris, quo(lhs_dec))
inc_tbl <- var_summ(ris, quo(inc_dec))


## create the correlation plot

## use cor to create the correlation matrix
## I have selected the columns by column number
## add the column number for lus_av
colnames(ris) ## find column numbers for columns to select for cor
mat <- cor(ris[,c(3:8,15, 2, 9, 10, 11)], method = "spearman", use = "pairwise.complete.obs")
## give the rows and columns of the correlation matrix reasonable names
colnames(mat) <-  c("% non-white", "% low income", "% less than HS", 
                    "% linguistic isolation", "% under 5yrs", "% over 64 yrs",
                    "PM2.5", "air toxics cancer risk", "open clean-up sites", 
                    "leaking underground tanks", "HAB advisories")
row.names(mat) <- c("% non-white", "% low income", "% less than HS", 
                    "% linguistic isolation", "% under 5yrs", "% over 64 yrs",
                    "PM2.5", "air toxics cancer risk", "open clean-up sites", 
                    "leaking underground tanks", "HAB advisories")

## create the correlation plot
cor_g <- ggcorrplot(mat, type = "lower",
                    outline.col = "white",
                    lab = TRUE,
                    pch = NA,
                    colors = c("#f57f32", "white", "#b1ca54"),
                    p.mat = mat)

## save the plot
ggsave("agu_data/corr_plot.png", plot = cor_g, width = 9, height = 9)

## display it
cor_g
