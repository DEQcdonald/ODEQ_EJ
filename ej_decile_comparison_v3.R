## Colin's maps: \\deqhq1\WQ-Share\EJ AGU\Maps

library(tidyverse)
library(lubridate)
library(corrplot)
library(ggcorrplot)
library(ggplot2)

## color scheme for ggcorrplot
DEQ_cbbPalette <- c("#000000", "#248f79", "#23769a", "#3aafbd", "#b1ca54", "#f57f32",
                    "#9fb9a2", "#81aec5", "#b1d6d4", "#dde398", "#fab38b")

## read in the data
cbg_ris <- read.csv("cbg_ris_test.csv", stringsAsFactors = FALSE)
eclu <- read.csv("agu_data/EJScreen_ECSI_LUST_2_TableToExcel.csv", stringsAsFactors = FALSE)
habs <- read.csv("agu_data/habs_min.csv", stringsAsFactors = FALSE)
pest <- read.csv("agu_data/pests_by_geoid2.csv", stringsAsFactors = FALSE)
pm25 <- read.csv("agu_data/PM25_CBG.csv", stringsAsFactors = FALSE)
pm25$PM25_EPA <- as.numeric(pm25$PM25_EPA)
pm25$PM25_NWAQ_MAX <- as.numeric(pm25$PM25_NWAQ_MAX)
pm25$PM25_Mean_NWAQ <- as.numeric(pm25$PM25_Mean_NWAQ)

## merge into single file, based on CBG GEOID
ris <- merge(cbg_ris[, c("GEOID", "tot_can")], eclu[, c(2:8,22,27)], by.x = "GEOID", by.y = "GeoID", all = TRUE)
ris <- merge(ris, habs, by = "GEOID", all = TRUE )
ris <- merge(ris, pest[, c("GEOID", "all_cat")], by = "GEOID", all = TRUE)
ris <- merge(ris, pm25, by = "GEOID", all = TRUE)
ris <- ris %>% select(-OBJECTID, -Name)


## create deciles for demographic variable
## note the "unique" for LINGISOPCT. This is because 50% of the CBGs have 0 LINGISOPCT
ris$min_dec <- cut(ris$MINORPCT, breaks = quantile(ris$MINORPCT, seq(0, 1, by = 0.1), na.rm = TRUE, include.lowest = TRUE))
ris$min_quin <- cut(ris$MINORPCT, breaks = quantile(ris$MINORPCT, seq(0, 1, by = 0.1),  na.rm = TRUE, include.lowest = TRUE), labels = FALSE)

ris$un5_dec <- cut(ris$UNDER5PCT, breaks = quantile(ris$UNDER5PCT, seq(0, 1, by = 0.1), na.rm = TRUE, include.lowest = TRUE))
ris$un5_quin <- cut(ris$UNDER5PCT, breaks = quantile(ris$UNDER5PCT, seq(0, 1, by = 0.1),  na.rm = TRUE, include.lowest = TRUE), labels = FALSE)

ris$o64_dec <- cut(ris$OVER64PCT, breaks = quantile(ris$OVER64PCT, seq(0, 1, by = 0.1), na.rm = TRUE, include.lowest = TRUE))
ris$o64_quin <- cut(ris$OVER64PCT, breaks = quantile(ris$OVER64PCT, seq(0, 1, by = 0.1),  na.rm = TRUE, include.lowest = TRUE), labels = FALSE)

ris$lhs_dec <- cut(ris$LESSHSPCT, breaks = quantile(ris$LESSHSPCT, seq(0, 1, by = 0.1), na.rm = TRUE, include.lowest = TRUE))
ris$lhs_quin <- cut(ris$LESSHSPCT, breaks = quantile(ris$LESSHSPCT, seq(0, 1, by = 0.1),  na.rm = TRUE, include.lowest = TRUE), labels = FALSE)

ris$inc_dec <- cut(ris$LOWINCPCT, breaks = quantile(ris$LOWINCPCT, seq(0, 1, by = 0.1), na.rm = TRUE, include.lowest = TRUE))
ris$inc_quin <- cut(ris$LOWINCPCT, breaks = quantile(ris$LOWINCPCT, seq(0, 1, by = 0.1),  na.rm = TRUE, include.lowest = TRUE), labels = FALSE)


## add the deciles for LINGISOPCT "by hand" as the first five deciles are zero
quantile(ris$LINGISOPCT, seq(0, 1, by = 0.1), na.rm = TRUE, include.lowest = TRUE)
ris$lin_dec <- NA
ris$lin_dec[!is.na(ris$LINGISOPCT) &  ris$LINGISOPCT == 0] <- "(0, 0]"
ris$lin_dec[!is.na(ris$LINGISOPCT) &  ris$LINGISOPCT > 0 & ris$LINGISOPCT <= 0.00790189] <- "(0, 0.07901]"
ris$lin_dec[!is.na(ris$LINGISOPCT) &  ris$LINGISOPCT > 0.00790189 & ris$LINGISOPCT <= 0.02187133] <- "(0.07901, 0.0218]"
ris$lin_dec[!is.na(ris$LINGISOPCT) &  ris$LINGISOPCT > 0.02187133 & ris$LINGISOPCT <= 0.04045338] <- "(0.0218,0.0404]"
ris$lin_dec[!is.na(ris$LINGISOPCT) &  ris$LINGISOPCT > 0.04045338 & ris$LINGISOPCT <= 0.07485050] <- "(0.0404, 0.0748]"
ris$lin_dec[!is.na(ris$LINGISOPCT) &  ris$LINGISOPCT > 0.07485050 & ris$LINGISOPCT <= 0.5] <- "(0.0748, 0.5]"
ris$lin_dec <- as.factor(ris$lin_dec)
ris$lin_dec <- ordered(ris$lin_dec, levels = c("(0, 0]", "(0, 0.07901]","(0.07901, 0.0218]", "(0.0218,0.0404]", "(0.0404, 0.0748]", "(0.0748, 0.5]"))
ris$lin_quin <- as.numeric(ris$lin_dec)
ris$lin_quin[!is.na(ris$lin_quin) & ris$lin_quin > 1] <- ris$lin_quin[!is.na(ris$lin_quin) & ris$lin_quin > 1] + 4


## write out the file, for convenience
write.csv(ris, "agu_data/merged_6env_with_deciles_v4.csv", row.names = FALSE)


# read in the risk file, if you don't want to go through the above steps
#ris <- read.csv("agu_data/merged_5env_with_deciles_v3.csv", stringsAsFactors = FALSE)


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
                            labels = c("% Minority", "% Less than HS education", "% Low incomce", "% Linguistic isolation", "% Under 5 years", "% Over 64 years"))

   g <- ggplot(df) + 
           theme_bw() + 
           geom_boxplot(aes(x = as.factor(decile), y = !!dem_var)) + 
           ylab(ylabel) +
           xlab("Deciles") + 
           facet_grid(~cat_ordered, labeller = label_wrap_gen(width = 10, multi_line = TRUE))
   return(g)

}
at_g <- ee_dem_box(ris, quo(tot_can), "Cancer risk from air toxics")
ggsave("agu_data/at_cancer_box.png", plot = at_g, width = 9, height = 4)

pm25_nwmean_g <- ee_dem_box(ris, quo(PM25_Mean_NWAQ), "PM2.5 concentrations")
ggsave("agu_data/pm25_conc_box.png", plot = pm25_nwmean_g, width = 9, height = 4)

hab_g <- ee_dem_box(ris, quo(countAdv), "# of HABs advisories (2004 - 2018)")
ggsave("agu_data/hab_adv_box.png", plot = hab_g, width = 9, height = 4)

ecsi_g <- ee_dem_box(ris, quo(SUM_ECSI_open), "Total open ECSI sites")
ggsave("agu_data/ecsi_sum_box.png", plot = ecsi_g, width = 9, height = 4)

## to plot without outliers
ecsi_less25_g <- ee_dem_box(ris %>% filter(SUM_ECSI_open < 25), quo(SUM_ECSI_open))
ecsi_less25_g

lust_g <- ee_dem_box(ris, quo(SUM_LUST_open), "Total open LUST sites")
ggsave("agu_data/lust_sum_box.png", plot = lust_g, width = 9, height = 4)

## bar graphs for persticide load 
## we could get fancy, and include this in the ee_dem_var function
## if the variable was a factor, do a bar graph; if numeric, do a boxplot; return error otherwise
pest_min <- ris %>% filter(min_quin == 1 | min_quin == 10) %>% select(all_cat, value = MINORPCT, decile = min_quin) %>% mutate( cat = "MINORPCT")
pest_inc <- ris %>% filter(inc_quin == 1 | inc_quin == 10) %>% select(all_cat, value = LOWINCPCT, decile = inc_quin) %>% mutate( cat = "LOWINCPCT")
pest_lhs <- ris %>% filter(lhs_quin == 1 | lhs_quin == 10) %>% select(all_cat, value = LESSHSPCT, decile = lhs_quin) %>% mutate( cat = "LESSHSPCT")
pest_un5 <- ris %>% filter(un5_quin == 1 | un5_quin == 10) %>% select(all_cat, value = UNDER5PCT, decile = un5_quin) %>% mutate( cat = "UNDER5PCT")
pest_o64 <- ris %>% filter(o64_quin == 1 | o64_quin == 10) %>% select(all_cat, value = OVER64PCT, decile = o64_quin) %>% mutate( cat = "OVER64PCT")
pest_lin  <- ris %>% filter(lin_quin == 1 | lin_quin == 10) %>% select(all_cat, value = LINGISOPCT, decile = lin_quin) %>% mutate( cat = "LINGISOPCT")
pest <- rbind( pest_min, pest_inc, pest_lhs, pest_lin, pest_un5, pest_o64) 
pest_summ <- pest %>% group_by(decile, cat) %>% summarize(pest_hi = sum(all_cat == "High"))

pest_summ$decile <- factor(pest_summ$decile,
                    levels = c(1, 10),
                    labels = c("10th %", "90th %"))
pest_summ$cat_ordered <- factor(pest_summ$cat, 
                         levels = c("MINORPCT","LESSHSPCT", "LOWINCPCT", "LINGISOPCT", "UNDER5PCT", "OVER64PCT"),
                         labels = c("% Minority", "% Less than HS education", "% Low incomce", "% Linguistic isolation", "% Under 5 years", "% Over 64 years"))


pest_g <- ggplot(pest_summ) + 
             theme_bw() +
            geom_bar(aes(x = as.factor(decile), y = pest_hi), width = 0.75, fill = "lightgray", position = "dodge", stat = "identity") + 
            ylab("# samples high pesticide load") +
            xlab("Deciles") + 
            facet_grid(~cat_ordered, labeller = label_wrap_gen(width = 10, multi_line = TRUE))

ggsave("agu_data/pest_hi_bar.png", plot = pest_g, width = 9, height = 4)


## function to create summary tables of environmental exposure by decile for the demographic variable passed in
var_summ <- function(ris, var) {
  tbl <- ris %>% drop_na(!!var) %>% 
    group_by(!!var) %>%
    summarize(can = mean(tot_can, na.rm = TRUE),
              open_sites = sum(SUM_Site_Open, na.rm = TRUE),
              habs_adv = sum(countAdv, na.rm = TRUE),
              hi_med_pollut = sum(!is.na(med_pollut_cat) & med_pollut_cat == "High"))
  return(tbl)
}

min_tbl <- var_summ(ris, quo(min_dec))
un5_tbl <- var_summ(ris, quo(un5_dec))
o64_tbl <- var_summ(ris, quo(o64_dec))
lin_tbl <- var_summ(ris, quo(lin_dec))
lhs_tbl <- var_summ(ris, quo(lhs_dec))
inc_tbl <- var_summ(ris, quo(inc_dec))



## graph
## function that takes one of the tables above and returns a bar graph comparing highest and lowest deciles

gra_tbl <- function(tbl, var_name) {
  n <- nrow(tbl)    
  tbl_gr <- data.frame(t(tbl[c(1, n),]))  # select the first & last rows of the table
  colnames(tbl_gr) <- c(paste0(var_name, "_lo10"), paste0( var_name, "_hi10"))   # colnames are X1 and X2, so give more meaningful colnames 
  tbl_gr <- tbl_gr[-1,]                   # drop the first row, which has the decile range
  tbl_gr$env_ex <- rownames(tbl_gr)       # since min_gr is a matrix, we need to create a column with rownames. The rownames are the environmental exposures
  to_gr <- gather(tbl_gr, key = "decile", value = "value", -env_ex)
  to_gr$env_ex <- as.factor(to_gr$env_ex)
  to_gr$decile <- as.factor(to_gr$decile)
  to_gr$value <- as.numeric(to_gr$value)
  
  # after all these manipulations, we are finally ready to plot
  g <- ggplot() + geom_bar(data = to_gr, 
                      aes( x = env_ex, y = value, fill = decile), 
                      stat = "identity",
                      position = "dodge")
  return(g)
  
}
# select the 1st and ast rows - highest and lowest decile, and transpose

min_gr <- gra_tbl(min_tbl, "min")
lin_gr <- gra_tbl(lin_tbl, "lin")
un5_gr <- gra_tbl(un5_tbl, "un5")
o64_gr <- gra_tbl(o64_tbl, "o64")
lhs_gr <- gra_tbl(lhs_tbl, "lhs")
inc_gr <- gra_tbl(inc_tbl, "inc")

## for my own reference - the correlation plot code
# correlation plot

#mydata <- select(OR_TL_inequality_TV, -GEOID, -TotalHH)

mydata <- select(wrk, -GEOID)
M <- cor(mydata, use = "pairwise.complete.obs" )
corrplot(M, method = "circle")


p.mat <- cor_pmat(mydata)
ggcorrplot(M, type = "lower",
           outline.col = "white",
           lab = TRUE,
           p.mat = p.mat)
