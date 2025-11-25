##### import libs
library(sf)
library(osrm)
library(pbapply)
library(future)
library(future.apply)

##### Import data ----

sp = read.csv("data/ag-b-00.03-vz2024statpop/STATPOP2024.csv", sep = ";")
municipalities_shp = st_read("data/swissboundaries3d_2025-04_2056_5728.shp/swissBOUNDARIES3D_1_5_TLM_HOHEITSGEBIET.shp") # this will be used as mask to filter the data
parks = st_read("data/OBS_EQUIPEMENTS_ESPACES_PUB-SHP/OBS_EQUIPEMENTS_ESPACES_PUB.shp")
forest = st_read("data/FFP_CADASTRE_FORET-SHP/FFP_CADASTRE_FORET.shp")
cmasset = st_read("data/campagne_masset/campagne_masset.shp")


# filter cantons to keep geneva only
gva_shp = municipalities_shp[municipalities_shp$NAME == "Genève",]
gva_shp = st_set_crs(gva_shp, st_crs("EPSG:2056"))
gva_shp_500 = st_buffer(gva_shp, 500)

# prepare campagne masset for analysis ----
cmasset = st_set_crs(cmasset, st_crs("EPSG:2056"))

# create buffer around campagne masset
cmasset_250 = st_buffer(cmasset, 250)

cmasset_500 = st_buffer(cmasset, 500)

##### Make sp object compliant with the analysis ----

#1 Trim to variables of interest ----
vars_of_interest = c(
  "E_KOORD",
  "N_KOORD",
  "BBTOT", # Population résidante permanente, Total
  "BB12",  # Etranger/étrangère, Total
  "BBM01", # Homme 0 à 4 ans
  "BBM02", # Homme 5 à 9 ans
  "BBM03", # Homme 10 à 14 ans
  "BBM04", # Homme 15 à 19 ans
  "BBM05", # Homme 20 à 24 ans
  "BBM06", # Homme 25 à 29 ans
  "BBM07", # Homme 30 à 34 ans
  "BBM08", # Homme 35 à 39 ans
  "BBM09", # Homme 40 à 44 ans
  "BBM10", # Homme 45 à 49 ans
  "BBM11", # Homme 50 à 54 ans
  "BBM12", # Homme 55 à 59 ans
  "BBM13", # Homme 60 à 64 ans
  "BBM14", # Homme 65 à 69 ans
  "BBM15", # Homme 70 à 74 ans
  "BBM16", # Homme 75 à 79 ans
  "BBM17", # Homme 80 à 84 ans
  "BBM18", # Homme 85 à 89 ans
  "BBM19",  # Homme 90 ans ou plus
  "BBW01", # Femme 0 à 4 ans
  "BBW02", # Femme 5 à 9 ans
  "BBW03", # Femme 10 à 14 ans
  "BBW04", # Femme 15 à 19 ans
  "BBW05", # Femme 20 à 24 ans
  "BBW06", # Femme 25 à 29 ans
  "BBW07", # Femme 30 à 34 ans
  "BBW08", # Femme 35 à 39 ans
  "BBW09", # Femme 40 à 44 ans
  "BBW10", # Femme 45 à 49 ans
  "BBW11", # Femme 50 à 54 ans
  "BBW12", # Femme 55 à 59 ans
  "BBW13", # Femme 60 à 64 ans
  "BBW14", # Femme 65 à 69 ans
  "BBW15", # Femme 70 à 74 ans
  "BBW16", # Femme 75 à 79 ans
  "BBW17", # Femme 80 à 84 ans
  "BBW18", # Femme 85 à 89 ans
  "BBW19"  # Femme 90 ans ou plus
)

sp = sp[, vars_of_interest]

#2 create new aggregated variables

sp[, "below_20"] = apply(sp[, c("BBM01", 
                              "BBM02", 
                              "BBM03", 
                              "BBM04",
                              "BBW01", 
                              "BBW02", 
                              "BBW03", 
                              "BBW04")],
                         1,
                         sum)

sp[, "20_to_65"] = apply(sp[, c("BBM05", 
                              "BBM06", 
                              "BBM07", 
                              "BBM08", 
                              "BBM09", 
                              "BBM10", 
                              "BBM11", 
                              "BBM12", 
                              "BBM13",
                              "BBW05", 
                              "BBW06", 
                              "BBW07", 
                              "BBW08", 
                              "BBW09", 
                              "BBW10", 
                              "BBW11", 
                              "BBW12", 
                              "BBW13")], 
                         1, 
                         sum) 

sp[, "above_65"] = apply(sp[, c("BBM14", 
                              "BBM15", 
                              "BBM16", 
                              "BBM17", 
                              "BBM18", 
                              "BBM19",
                              "BBW14", 
                              "BBW15", 
                              "BBW16", 
                              "BBW17", 
                              "BBW18", 
                              "BBW19")],
                         1,
                         sum) 

#3 calculate proportion of foreigners and age class for each point
sp[, "prop_below_20"] = sp[, "below_20"]/ sp[, "BBTOT"]
sp[, "prop_20_to_65"] = sp[, "20_to_65"]/ sp[, "BBTOT"]
sp[, "prop_above_65"] = sp[, "above_65"]/ sp[, "BBTOT"]
sp[, "prop_foreigners"] = sp[, "BB12"]/ sp[, "BBTOT"]


vars_to_keep = c("E_KOORD",
                 "N_KOORD",
                 "BBTOT", # Population résidante permanente, Total
                 "prop_below_20",
                 "prop_20_to_65",
                 "prop_above_65",
                 "prop_foreigners"
                 )
sp = sp[, vars_to_keep]
#3 convert to spatial object ----

sp = st_as_sf(x=sp, coords = c("E_KOORD", "N_KOORD"))
sp = st_set_crs(sp, st_crs("EPSG:2056"))

#5 trim to the GVA area ----
sp = st_filter(sp, gva_shp_500)


# import the calculated isochrones
final_isochrones = readRDS("data/isochrones_raw.rds")
final_isochrones_lv03 = st_transform(final_isochrones, crs = 2056)
#st_write(final_isochrones_lv03, "isochrones.shp") # export for visualization on qGIS

# create a new polygon for cropping green spaces and forests
crop_poly = st_convex_hull(st_union(final_isochrones_lv03))

# Prepare parks object for analysis ----

cats = c("Parc", "Espace vert", "Plage")
parks = parks[parks$CATEGORIE %in% cats,]
parks = st_set_crs(parks, st_crs("EPSG:2056"))
parks = st_filter(parks, crop_poly)

# prepare forest object for analysis -----
forest = st_set_crs(forest, st_crs("EPSG:2056"))
forest = st_filter(forest, crop_poly)

#6 for each point calculate distance to closest park
dist_to_parks = st_distance(sp, parks)
dist_to_closest_park = apply(dist_to_parks, 1, min)


#7 for each point calculate the area of green space and forests within 250m and 500m
buff_250 = st_buffer(sp, 250)
buff_500 = st_buffer(sp, 500)


# green space area within 250m
gs_area_within_250 = sapply(1:nrow(buff_250), function(i) {
  sum(st_area(st_intersection(buff_250$geometry[i], parks)))
})

# green space area within 500m
gs_area_within_500 = sapply(1:nrow(buff_500), function(i) {
  sum(st_area(st_intersection(buff_500$geometry[i], parks)))
})

# green space area within 15min walk
gs_area_within_15min_walk = sapply(1:nrow(final_isochrones), function(i) {
  sum(st_area(st_intersection(final_isochrones_lv03$geometry[i], parks)))
})

# forest area within 250m
forest_area_within_250 = sapply(1:nrow(buff_250), function(i) {
  sum(st_area(st_intersection(buff_250$geometry[i], forest)))
})

# forest area within 500m
forest_area_within_500 = sapply(1:nrow(buff_500), function(i) {
  sum(st_area(st_intersection(buff_500$geometry[i], forest)))
})

# forest space area within 15min walk
forest_area_within_15min_walk = sapply(1:nrow(final_isochrones), function(i) {
  sum(st_area(st_intersection(final_isochrones_lv03$geometry[i], forest)))
})

# campagne masset area within 250m
cmasset_area_within_250 = sapply(1:nrow(buff_250), function(i) {
  sum(st_area(st_intersection(buff_250$geometry[i], cmasset)))
})

# campagne masset area within 500m
cmasset_area_within_500 = sapply(1:nrow(buff_500), function(i) {
  sum(st_area(st_intersection(buff_500$geometry[i], cmasset)))
})

# campagne masset area within 15min walk
cmasset_area_within_15min_walk = sapply(1:nrow(final_isochrones), function(i) {
  sum(st_area(st_intersection(final_isochrones_lv03$geometry[i], cmasset)))
})


# package the results in a dataframe
sp = cbind(sp,
           gs_area_within_250, gs_area_within_500, gs_area_within_15min_walk, 
           forest_area_within_250, forest_area_within_500, forest_area_within_15min_walk,
           cmasset_area_within_250, cmasset_area_within_500, cmasset_area_within_15min_walk
           )


#8 for each point calculate the percentage of green space per capita -----


# scenario 1: current situation
sp[, "gs_area_per_capita"] = sp[["gs_area_within_15min_walk"]]/sp[[ "BBTOT"]]
sp[, "fgs_area_per_capita"] = (sp[["gs_area_within_15min_walk"]]+sp[["forest_area_within_15min_walk"]])/sp[["BBTOT"]]

# scenario 2: after purchase of the new park
sp[, "gs_area_per_capita_s2"] = (sp[["gs_area_within_15min_walk"]]+sp[["cmasset_area_within_15min_walk"]])/sp[[ "BBTOT"]]
sp[, "fgs_area_per_capita_s2"] = (sp[["gs_area_within_15min_walk"]]+sp[["forest_area_within_15min_walk"]]+sp[["cmasset_area_within_15min_walk"]])/sp[["BBTOT"]]


# remove points where population is equal to 3
sp = sp[sp$BBTOT>3,]

# remove points where population is above 1000
sp = sp[sp$BBTOT<1000,]

saveRDS(sp, "data/final_extraction.rds")
#sp = readRDS("final_extraction.rds")


#write.csv(cbind(st_drop_geometry(sp), st_coordinates(sp)), "final_extraction.csv") # export for visualization in qGIS


