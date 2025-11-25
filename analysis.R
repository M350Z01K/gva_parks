library(sf)
library(ggplot2)


# declare function
calc_mean_min_max = function(var){
  
  paste0(round(mean(var), 2), " (", round(min(var), 2), "-", round(max(var), 2), ")")
  
  
}


# import municipalities shapefile and extract Geneva to use as mask
municipalities_shp = st_read("data/swissboundaries3d_2025-04_2056_5728.shp/swissBOUNDARIES3D_1_5_TLM_HOHEITSGEBIET.shp") # this will be used as mask to filter the data
gva_shp = municipalities_shp[municipalities_shp$NAME == "Genève",]
gva_shp = st_set_crs(gva_shp, st_crs("EPSG:2056"))

# import campagne masset shapefile to use as mask
cmasset = st_read("data/campagne_masset/campagne_masset.shp")
cmasset = st_set_crs(cmasset, st_crs("EPSG:2056"))
cmasset_250 = st_buffer(cmasset, 250)
cmasset_500 = st_buffer(cmasset, 500)


# import extracted data
sp = readRDS("data/final_extraction.rds")

# keep only the vars of interest
vars_of_interest = c("BBTOT",
                     "prop_below_20",
                     "prop_20_to_65",
                     "prop_above_65",
                     "prop_foreigners",
                     "gs_area_within_15min_walk",
                     "cmasset_area_within_15min_walk",
                     "gs_area_per_capita",
                     "gs_area_per_capita_s2")

sp = sp[, vars_of_interest]

# define area of soccer field to make surfaces easier to understand
soccer_field_area = 7886.7

area_vars = c("gs_area_within_15min_walk",
              "cmasset_area_within_15min_walk",
              "gs_area_per_capita",
              "gs_area_per_capita_s2")


sf_areas = st_drop_geometry(sp[, area_vars]/ soccer_field_area)
colnames(sf_areas) = paste0("sf_", area_vars)

sp = cbind(sp, sf_areas)

#write.csv(cbind(st_drop_geometry(sp), st_coordinates(sp)), "final_extraction.csv")
#write.csv(cbind(st_drop_geometry(sp_mun_ge), st_coordinates(sp_mun_ge)), "final_extraction_mun_ge.csv")

# filter extracted data to municipality of Geneva -----
sp_mun_ge = st_filter(sp, gva_shp)


# show data and stats
calc_mean_min_max(sp_mun_ge$sf_gs_area_within_15min_walk)


#write.csv(cbind(st_drop_geometry(sp_mun_ge), st_coordinates(sp_mun_ge)), "soccerfield_areas.csv")

# filter extracted data to campagne masset perimeter -----
sp_masset_250 = st_filter(sp, cmasset_250)
sp_masset_500 = st_filter(sp, cmasset_500)

# before purchasing the park
calc_mean_min_max(sp_masset_250$sf_gs_area_within_15min_walk)
calc_mean_min_max(sp_masset_500$sf_gs_area_within_15min_walk)


# after purchasing the park
sp_masset_250[, "sf_after_purchase"] =  sp_masset_250$sf_gs_area_within_15min_walk + sp_masset_250$sf_cmasset_area_within_15min_walk
sp_masset_500[, "sf_after_purchase"] =  sp_masset_500$sf_gs_area_within_15min_walk + sp_masset_500$sf_cmasset_area_within_15min_walk


calc_mean_min_max(sp_masset_250$sf_after_purchase)
calc_mean_min_max(sp_masset_500$sf_after_purchase)


# make plot for visualizing the means
means_abs = c(
  mean(sp_mun_ge$sf_gs_area_within_15min_walk),
  mean(sp_masset_250$sf_gs_area_within_15min_walk),
  mean(sp_masset_500$sf_gs_area_within_15min_walk),
  mean(sp_masset_250$sf_after_purchase),
  mean(sp_masset_500$sf_after_purchase)
  )

labs = c("Genève ville", "Masset 250m avant", "Masset 500m avant", "Masset 250m après", "Masset 500m après")


# 
data <- data.frame(
  Category = labs,
  Value = means_abs
)
reversed_labs <- rev(labs)
data$Category <- factor(data$Category, levels = reversed_labs)
# Define a generous left margin (e.g., 2 cm)
margin_cm <- 2

ggplot(data, aes(x = Category, y = Value)) +
  
  # 1. Add the bars
  geom_bar(stat = "identity", fill = "#1F77B4", color = "black") +
  
  # 2. Flip the coordinates to make it horizontal
  coord_flip() +
  
  # 3. Add Labels and Title
  labs(
    title = "Moyenne de surface de parcs atteignables en 15 min de marche",
    x = NULL, # Remove the x-axis label, as it's often redundant after coord_flip
    y = "Surface en nombre de terrains de foot"
  ) +
  
  # 4. Apply a Clean Theme and Adjust Margins
  theme_minimal() +
  theme(
    # Set the overall plot margin (top, right, bottom, left)
    # The left margin needs to be large for the category labels
    plot.margin = unit(c(0.5, 0.5, 0.5, margin_cm), "cm"),
    
    # Optional: Improve axis text appearance
    axis.text.y = element_text(hjust = 1, size = 10),
    axis.title.x = element_text(margin = margin(t = 10))
  )


# comparison per capita -----
calc_mean_min_max(sp_mun_ge$sf_gs_area_per_capita)
calc_mean_min_max(sp_masset_250$sf_gs_area_per_capita)
calc_mean_min_max(sp_masset_500$sf_gs_area_per_capita)

# per capita after purchasing the park

calc_mean_min_max(sp_masset_250$sf_gs_area_per_capita_s2)
calc_mean_min_max(sp_masset_500$sf_gs_area_per_capita_s2)

# make plot for visualizing the means
means_capita = c(
  mean(sp_mun_ge$sf_gs_area_per_capita),
  mean(sp_masset_250$sf_gs_area_per_capita),
  mean(sp_masset_500$sf_gs_area_per_capita),
  mean(sp_masset_250$sf_gs_area_per_capita_s2),
  mean(sp_masset_500$sf_gs_area_per_capita_s2) 
  )

data <- data.frame(
  Category = labs,
  Value = means_capita
)
data$Category <- factor(data$Category, levels = reversed_labs)
# Define a generous left margin (e.g., 2 cm)
margin_cm <- 2

ggplot(data, aes(x = Category, y = Value)) +
  
  # 1. Add the bars
  geom_bar(stat = "identity", fill = "#1F77B4", color = "black") +
  
  # 2. Flip the coordinates to make it horizontal
  coord_flip() +
  
  # 3. Add Labels and Title
  labs(
    title = "Moyenne de Surface de parcs atteignables en 15 min de marche par habitant",
    x = NULL, # Remove the x-axis label, as it's often redundant after coord_flip
    y = "Surface en nombre de terrains de foot"
  ) +
  
  # 4. Apply a Clean Theme and Adjust Margins
  theme_minimal() +
  theme(
    # Set the overall plot margin (top, right, bottom, left)
    # The left margin needs to be large for the category labels
    plot.margin = unit(c(0.5, 0.5, 0.5, margin_cm), "cm"),
    
    # Optional: Improve axis text appearance
    axis.text.y = element_text(hjust = 1, size = 10),
    axis.title.x = element_text(margin = margin(t = 10))
  )

