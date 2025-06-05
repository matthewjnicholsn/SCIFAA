#this command clears the environment
rm(list = ls(all = TRUE))

#first we read in all of our packages
library(haven) 
library(sf)
library(raster) 
library(dplyr) 
library(ggplot2) 
library(viridis)
library(tidyr) 
library(openxlsx)
library(writexl)
library(naniar)#for replace_with_na fx
library(sjlabelled) #for fx set_label
library(DHS.rates) #for functions like chmort
library(data.table) #for tables from mort calculations
library(beepr)

# in this script, we will calculate childhood mortality, GINI coefficient, 
# and mean wealth index by state. We will then create some helpful plots to visualize
# the inter-related nature of wealth, inequality, and health, as well as the impact 
# of environments on health. This code will be available to Science for all audiences 
# for usage, please see the license in the git repository

#read in data
PR <- read_dta('/Users/matthewnicholson/SCIFAA/2016 ethiopia data DHS 7/ETPR71DT/ETPR71FL.DTA') #load in household member microdata
BR <- read_dta("/Users/matthewnicholson/SCIFAA/2016 ethiopia data DHS 7/ETBR71DT/ETBR71FL.DTA") #load in birth microdata
KR <- read_dta('/Users/matthewnicholson/SCIFAA/2016 ethiopia data DHS 7/ETKR71DT/ETKR71FL.DTA') #load in child microdata
SF <- read_sf("/Users/matthewnicholson/SCIFAA/ethiopia shapefile regions/Eth_Region_2013.shp") #load in country shapefile with state-level data
CV <- read_sf('/Users/matthewnicholson/SCIFAA/ethiopia health facilities/hotosm_eth_health_facilities_points_shp.shp') #load in shapefile of desired spatial covariate

####################################################################################################################################################
# first we calculate all of the stats we will be using in the maps
# for these maps I want child mortality, mean wealth index (weighted) and inequality (GINI coefficient)

# prep data for mortality calculations
# The source code for the mortality and GINI calculations can be found at: https://github.com/DHSProgram/DHS-Indicators-R
# working with DHS data requires approval from the DHS program, which is unfortunately under review
# due to the ongoing shutdown of USAID. Other open data sources can be used, but major changes to the scripts
# would be required to acommodate your data structure.

BRdata <- BR %>%
  mutate(child_sex = b4) %>%
  mutate(child_sex = set_label(child_sex, label = "Sex of child"))  %>%
  mutate(months_age = b3-v011) %>%
  mutate(mo_age_at_birth =
           case_when(
             months_age < 20*12   ~ 1 ,
             months_age >= 20*12 & months_age < 30*12 ~ 2,
             months_age >= 30*12 & months_age < 40*12 ~ 3,
             months_age >= 40*12 & months_age < 50*12 ~ 4)) %>%
  mutate(mo_age_at_birth = factor(mo_age_at_birth, levels = c(1,2,3,4), labels = c("Mother's age at birth < 20", "Mother's age at birth 20-29", "Mother's age at birth 30-39","Mother's age at birth 40-49"))) %>%
  mutate(mo_age_at_birth = set_label(mo_age_at_birth, label = "Mother's age at birth")) %>%
  mutate(birth_order =
           case_when(
             bord == 1  ~ 1,
             bord >= 2 & bord <= 3 ~ 2,
             bord >= 4 & bord <= 6 ~ 3,
             bord >= 7  ~ 4,
             bord == NA ~ 99)) %>%
  replace_with_na(replace = list(birth_order = c(99))) %>%
  mutate(birth_order = factor(birth_order, levels = c(1,2,3,4), labels = c("Birth order:1", "Birth order:2-3", "Birth order:4-6","Birth order:7+"))) %>%
  mutate(birth_order = set_label(birth_order, label = "Birth order"))  %>%
  mutate(prev_bint =
           case_when(
             b11 <= 23 ~ 1,
             b11 >= 24 & b11 <= 35 ~ 2,
             b11 >= 36 & b11 <= 47 ~ 3,
             b11 >= 48 ~ 4)) %>%
  mutate(prev_bint = set_label(prev_bint, label = "Preceding birth interval"))  %>%
  mutate(birth_size =
           case_when(
             m18 >= 4 & m18 <= 5 ~ 1,
             m18 <= 3 ~ 2,
             m18 > 5 ~ 99)) %>%
  mutate(birth_size = set_label(birth_size, label = "Birth size")) 

BRdata[["prev_bint"]] <- ifelse(is.na(BRdata[["prev_bint"]]), 999, BRdata[["prev_bint"]])
BRdata[["birth_size"]] <- ifelse(is.na(BRdata[["birth_size"]]), 999, BRdata[["birth_size"]])

BRdata <- BRdata %>%
  mutate(prev_bint = factor(prev_bint, levels = c(1,2,3,4,999), labels = c("Previous birth interval <2 years", "Previous birth interval 2 years", "Previous birth interval 3 years","Previous birth interval 4+ years", "missing"))) %>%
  mutate(birth_size = factor(birth_size, levels = c(1,2,99,999), labels = c("Birth size: Small/very small","Birth size: Average or larger", "Birth size: Don't know/missing", "missing" )))

BRdata_CMORT <- (BRdata[, c("v021", "v022","v024", "v025", "v005", "v008","v011", 
                            "b3", "b7", "v106", "v190", "child_sex", "mo_age_at_birth", 
                            "birth_order", "prev_bint","birth_size")])

# NNMR, PNNMR, IMR, CMR & U5MR (neonatal mort, perinatal mort, infant mort, childhood mort, under 5 mort)
# TABLES 8.1, 8.2 and 8.3

# Different datasets for period ends: 5-9 and 10-14
BRdata_CMORT1 <- BRdata_CMORT
BRdata_CMORT2 <- BRdata_CMORT
BRdata_CMORT1$v008 <- BRdata_CMORT$v008 - 12 * (5)
BRdata_CMORT2$v008 <- BRdata_CMORT$v008 - 12 * (10)

resn1 <- as.data.frame(chmort(BRdata_CMORT))
resn1$period <- "0-4"

resn2 <- as.data.frame(chmort(BRdata_CMORT1))
resn2$period <- "5-9"
resn3 <- as.data.frame(chmort(BRdata_CMORT2))
resn3$period <- "10-14"

resc <- as.data.frame(rbind(chmort(BRdata_CMORT, Class="v024",Period = 120),
                            chmort(BRdata_CMORT, Class="v025",Period = 120),
                            chmort(BRdata_CMORT, Class="v106",Period = 120),
                            chmort(BRdata_CMORT, Class="v190",Period = 120),
                            chmort(BRdata_CMORT, Class="child_sex",Period = 120),
                            chmort(BRdata_CMORT, Class="mo_age_at_birth",Period = 120),
                            chmort(BRdata_CMORT, Class="birth_order",Period = 120),
                            chmort(BRdata_CMORT, Class="prev_bint",Period = 120),
                            chmort(BRdata_CMORT, Class="birth_size",Period = 120)))

resc$period <- "0-9"

CHMORT <- vector("list", 4)
CHMORT <- rbindlist(list(resn1,resn2,resn3,resc), fill = TRUE)

CHMORT[["Class"]] <- ifelse(is.na(CHMORT[["Class"]]), "National", CHMORT[["Class"]])
CHMORT <- CHMORT[CHMORT[["Class"]]!="missing",] 

# adjustment for row names
num_rows <- nrow(CHMORT)
num_repeats <- ceiling(num_rows / length(row.names(resn1)))  # Calculate how many times to repeat
rownames(CHMORT) <- paste(seq(1:num_rows), rep(row.names(resn1), num_repeats)[1:num_rows])

write.xlsx(CHMORT, "Tables_child_mort_ethiopia_2016.xlsx", asTable = TRUE, append = TRUE, overwrite = FALSE)
##calculate childhood mortality by state
# Check state is factor
BRdata_CMORT_state <- BRdata %>% 
  select(v021, v022, v024, v025, v005, v008, v011, # Subset
         b3, b7, v106, v190, child_sex, mo_age_at_birth, 
         birth_order, prev_bint, birth_size) %>%
  filter(!is.na(v021) & !is.na(v022) & !is.na(v024) & !is.na(v025)) # Remove NA

res_state <- as.data.frame(chmort(BRdata_CMORT_state, Class = "v024", Period = 120)) # Create chmort table by region

write.xlsx(res_state, "Tables_child_mort_by_region_ethiopia_2016.xlsx", asTable = TRUE, append = TRUE, overwrite = FALSE) # Write to Excel

####################################################################################################################################################
# Now that we have calculated some key mortality indicators by state, we will 
# calculate the weighted mean of wealth index by state and the gini coefficient
PRdata_gini <- select(PR,hv005, hv012, hv024, hv025, hv101, hv270, hv271)

calc_gini <- function(PRdata_gini, Class = NULL){
  
  if (is.null(Class)){
    
    Dat <- PRdata_gini
    # Keep one case per household (e.g. hvidx=1 or hv101=1 or hvidx=hv003) in households with at least one de jure member
    Dat <- Dat %>% filter(hv101==1 & hv012>0) %>%
      mutate(cases=hv012*hv005/1000000)   # Each household has hv012 de jure members
    
    Dat$category=1+as.integer(99*(Dat$hv271 - min(Dat$hv271))/(max(Dat$hv271)-min(Dat$hv271)))
    MIN <- min(Dat$hv271)
    
    # IMPORTANT! The assets for the household are weighted by hv005 but not multiplied by the number of household members
    CASES <- Dat %>% group_by(category) %>% summarise(cases = sum(cases, na.rm=TRUE)) 
    
    Dat$assets <- (Dat$hv271-MIN)*Dat$hv005/1000000
    
    ASSETS <- Dat %>% 
      group_by(category) %>% 
      summarise(assets = sum(assets, na.rm=TRUE)) 
    
    Dat_cat <- merge(ASSETS, CASES, by = c("category"), all.x = TRUE, all.y = TRUE)
    
    # Note: some categories may be empty; best to renumber them; must be certain they are in sequence
    Dat_cat <- transform(Dat_cat, category = as.numeric(factor(category)))
    
    Dat_cat$cases_prop= Dat_cat$cases/sum(Dat_cat$cases)
    Dat_cat$assets_prop= Dat_cat$assets/sum(Dat_cat$assets)
    
    # Calculate cumulative proportions for cases and assets. 
    Dat_cat[["cases_cumprop"]] <- ifelse(Dat_cat[["category"]]==1, Dat_cat[["cases_prop"]], 0)
    Dat_cat[["assets_cumprop"]] <- ifelse(Dat_cat[["category"]]==1, Dat_cat[["assets_prop"]], 0)
    Dat_cat[["term"]] <- ifelse(Dat_cat[["category"]]==1, 
                                Dat_cat[["cases_prop"]]*Dat_cat[["assets_cumprop"]], 0)
    
    ncats = (2:max(Dat_cat$category))
    for (i in (ncats)) {
      
      Dat_cat[["cases_cumprop"]][i] <- Dat_cat[["cases_cumprop"]][i-1] + Dat_cat[["cases_prop"]][i]
      Dat_cat[["assets_cumprop"]][i] <- Dat_cat[["assets_cumprop"]][i-1] + Dat_cat[["assets_prop"]][i]
      Dat_cat[["term"]][i] <- Dat_cat[["cases_prop"]][i]*(Dat_cat[["assets_cumprop"]][i-1]+Dat_cat[["assets_cumprop"]][i])
      
    }
    
    Gini=abs(1-sum(Dat_cat[["term"]]))
    
    RESULTS <- matrix(0, nrow = 1, ncol = 2)
    dimnames(RESULTS) <- list(NULL, c("Class", "Gini"))
    RESULTS <- as.data.frame(RESULTS)
    RESULTS[1, ] <- c("National", round(Gini, 3))
    
    list(RESULTS)[[1]]
    
  } else {
    
    PRdata_gini[[Class]] <- haven::as_factor(PRdata_gini[[Class]])
    PRdata_gini$DomID  <- c(as.numeric(PRdata_gini[[Class]]))
    
    RESULTS <- matrix(0, nrow = max(as.numeric(PRdata_gini$DomID)), ncol = 2)
    dimnames(RESULTS) <- list(NULL, c("Class", "Gini"))
    RESULTS <- as.data.frame(RESULTS)
    
    for (j in 1:(max(as.numeric(PRdata_gini$DomID)))) {
      
      Dat = PRdata_gini[PRdata_gini$DomID == j, ]
      
      Dat <- Dat %>% filter(hv101==1 & hv012>0) %>%
        mutate(cases=hv012*hv005/1000000)   # Each household has hv012 de jure members
      
      Dat$category=1+as.integer(99*(Dat$hv271 - min(Dat$hv271))/(max(Dat$hv271)-min(Dat$hv271)))
      MIN <- min(Dat$hv271)
      
      # IMPORTANT! The assets for the household are weighted by hv005 but not multiplied by the number of household members
      CASES <- Dat %>% group_by(category) %>% summarise(cases = sum(cases, na.rm=TRUE)) 
      
      Dat$assets <- (Dat$hv271-MIN)*Dat$hv005/1000000
      
      ASSETS <- Dat %>% 
        group_by(category) %>% 
        summarise(assets = sum(assets, na.rm=TRUE)) 
      
      Dat_cat <- merge(ASSETS, CASES, by = c("category"), all.x = TRUE, all.y = TRUE)
      
      # Note: some categories may be empty; best to renumber them; must be certain they are in sequence
      Dat_cat <- transform(Dat_cat, category = as.numeric(factor(category)))
      
      Dat_cat$cases_prop= Dat_cat$cases/sum(Dat_cat$cases)
      Dat_cat$assets_prop= Dat_cat$assets/sum(Dat_cat$assets)
      
      # Calculate cumulative proportions for cases and assets. 
      Dat_cat[["cases_cumprop"]] <- ifelse(Dat_cat[["category"]]==1, Dat_cat[["cases_prop"]], 0)
      Dat_cat[["assets_cumprop"]] <- ifelse(Dat_cat[["category"]]==1, Dat_cat[["assets_prop"]], 0)
      Dat_cat[["term"]] <- ifelse(Dat_cat[["category"]]==1, 
                                  Dat_cat[["cases_prop"]]*Dat_cat[["assets_cumprop"]], 0)
      
      ncats = (2:max(Dat_cat$category))
      for (i in (ncats)) {
        
        Dat_cat[["cases_cumprop"]][i] <- Dat_cat[["cases_cumprop"]][i-1] + Dat_cat[["cases_prop"]][i]
        Dat_cat[["assets_cumprop"]][i] <- Dat_cat[["assets_cumprop"]][i-1] + Dat_cat[["assets_prop"]][i]
        Dat_cat[["term"]][i] <- Dat_cat[["cases_prop"]][i]*(Dat_cat[["assets_cumprop"]][i-1]+Dat_cat[["assets_cumprop"]][i])
        
      }
      
      Gini=abs(1-sum(Dat_cat[["term"]]))
      
      RESULTS[j, ] <- c(attributes(Dat[[Class]])$levels[[j]], round(Gini, 3))
      
    }
    list(RESULTS)[[1]]
    
  }
  
}

ph_gini_results <- as.data.frame(rbind(calc_gini(PRdata_gini),
                                       calc_gini(PRdata_gini,Class="hv024")))

write.xlsx(ph_gini_results, "GINI_by_region_Ethiopia_2016.xlsx", append=TRUE)

# Calculate mean wealth index by region

weighted_mean_by_region <- PR %>%
  group_by(hv024) %>%
  summarise(weighted_mean_wealth = weighted.mean(hv270, w = hv005, na.rm = TRUE)) %>%
  ungroup()

write.xlsx(weighted_mean_by_region, "Wealth_by_region_Ethiopia_2016.xlsx", append = TRUE)
#tells us the code is done
beep()
####################################################################################################################################################
# Now that we have all of our statistics calculated and our shapefiles are loaded in,
# we are going to make some maps! For this project, I want a map of wealth by region, a 
# map of inequality by region, and a map of mortality by region. This will be helpful in understanding
# how health and wealth are inter-related spatially!

#read in our gini by region file
gini_by_region <- read.xlsx("/Users/matthewnicholson/SCIFAA/GINI_by_region_Ethiopia_2016.xlsx")

# first, lets edit our shapefile a bit as it contains unwanted information
# it is always best to only work with what you need, which, in this case, is 
# region and geometry 
# another thing to pay attention to is any difference in labelling between your shape
# file and your own table
# run str(SF$region) to check the labels and check the excel table for consistency
SF1 <- SF %>% 
  select(-c("REG_P_CODE","REG_Pcode", "HRname", "HRpcode", "HRparent")) %>% 
  rename(region = REGIONNAME) %>% 
  left_join(gini_by_region, by = "region") %>% 
  mutate(Gini = as.numeric(Gini)) %>% 
  mutate(centroid = st_centroid(geometry))
# get centroids for labelling



# create the plot 
p1 <- ggplot(data = SF1) +
  geom_sf(aes(fill = Gini), color = "white") +
  geom_sf_text(aes(label = region, geometry = centroid, color = "black"),
               size = 3,
               check_overlap = TRUE) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom") +
  scale_fill_viridis_c(option = "viridis", direction = -1, 
                       limits = c(0, 1), 
                       breaks = seq(0, 1, by = 0.2)) +
  scale_color_identity() +
  labs(title = "Wealth inequality by region in Ethiopia (2016)",
       fill = "GINI coefficient")

# save the plot
ggsave(filename = "Wealth_inequality_by_region_in_Ethiopia_2016.png", plot = p1)

# now we will plot mean wealth by region

#load mean wealth index by region
wealth_by_region <- read.xlsx("/Users/matthewnicholson/SCIFAA/Wealth_by_region_Ethiopia_2016.xlsx")

#modify shapefile
SF2 <- SF %>% 
  select(-c("REG_P_CODE","REG_Pcode", "HRname", "HRpcode", "HRparent")) %>% 
  rename(region = REGIONNAME) %>% 
  left_join(wealth_by_region, by = "region") %>% 
  mutate(weighted_mean_wealth = as.numeric(weighted_mean_wealth)) %>% 
  mutate(centroid = st_centroid(geometry))

#create the plot
p2 <- ggplot(data = SF2) +
  geom_sf(aes(fill = weighted_mean_wealth), color = "white") +
  geom_sf_text(aes(label = region, geometry = centroid, color = "black"),
               size = 3,
               check_overlap = TRUE) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom") +
  scale_fill_viridis_c(option = "viridis", direction = -1, 
                       limits = c(1, 5), 
                       breaks = seq(1, 5, by = 1)) +
  scale_color_identity() +
  labs(title = "Wealth distribution by region in Ethiopia (2016)",
       fill = "Mean wealth index (1-5)")

# save the plot
ggsave(filename = "Wealth_distribution_by_region_in_Ethiopia_2016.png", plot = p2)

# now we will plot childhood mortality by region
# will resolve the issue of actually using the excel table created later

cmort_by_region <- data.frame(
  region = c("Tigray", "Afar", "Amhara", "Oromia", "Somali", 
             "Beneshangul Gumu", "SNNPR", "Gambela", "Hareri", 
             "Addis Ababa", "Dire Dawa"),
  u5mort = c(58.63, 124.98, 85.16, 78.66, 94.1, 
             97.73, 88.31, 88.02, 72.39, 
             38.57, 92.68)
)

#modify shapefile
SF3 <- SF %>% 
  select(-c("REG_P_CODE","REG_Pcode", "HRname", "HRpcode", "HRparent")) %>% 
  rename(region = REGIONNAME) %>% 
  left_join(cmort_by_region, by = "region") %>% 
  mutate(u5mort = as.numeric(u5mort)) %>% 
  mutate(centroid = st_centroid(geometry))

#create the plot
p3 <- ggplot(data = SF3) +
  geom_sf(aes(fill = u5mort), color = "white") +
  geom_sf_text(aes(label = region, geometry = centroid, color = "black"),
               size = 3,
               check_overlap = TRUE) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom") +
  scale_fill_viridis_c(option = "viridis", direction =  -1) +
  scale_color_identity() +
  labs(title = "Under 5 Mortality in Ethiopia (2016)",
       fill = "Under 5 mortality per 1000")

# save the plot
ggsave(filename = "Under_5_Mortality_by_region_Ethiopia_2016.png", plot = p3)

# now want to try to create a map of health outposts and filter them by region
#check that the coordinate systems are the same
CV <- st_transform(CV, st_crs(SF))
count_outposts <- CV %>%
  st_join(SF, join = st_within) %>%
  group_by(REGIONNAME) %>%
  summarise(count = n(), .groups = 'drop')


p4 <- ggplot() +
  geom_sf(data = SF, fill = "lightgrey", color = "black") +  # Outline of Ethiopia with regions
  geom_sf(data = CV, color = "red", size = 0.25) +  # Health outposts
  theme_minimal() +
  labs(title = "Health Outposts in Ethiopia",
       subtitle = "Overlay of Health Outposts on Regional Map",
       x = "Longitude",
       y = "Latitude")

#calculate how many amenities are in each region and plot this too

p5 <- ggplot(data = count_outposts) +
  geom_sf(aes(fill = count), color = "white") +
  geom_sf_text(aes(label = region, geometry = centroid, color = "black"),
               size = 3,
               check_overlap = TRUE) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom") +
  scale_fill_viridis_c(option = "viridis", direction =  -1) +
  scale_color_identity() +
  labs(title = "Health Outposts by Region in Ethiopia (2016)",
       fill = "Number of Health Outposts")
  



  
