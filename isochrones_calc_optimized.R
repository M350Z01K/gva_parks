# Instructions to set up local OpenStreetMap Server on Docker -> Run on Ubuntu Linux

# # 1. Download the Switzerland PBF file from Geofabrik.
# wget https://download.geofabrik.de/europe/switzerland-latest.osm.pbf
# 
# # 2. Rename the downloaded file to a simpler name for use in the Docker commands.
# mv switzerland-latest.osm.pbf switzerland.osm.pbf
# 
# A. Extract (Creates the initial graph using the walking profile)
#docker run -t -v "${PWD}:/data" ghcr.io/project-osrm/osrm-backend osrm-extract -p /opt/foot.lua /data/switzerland.osm.pbf
#
#B. Partition (Optimizes the graph structure)
#docker run -t -v "${PWD}:/data" ghcr.io/project-osrm/osrm-backend osrm-partition /data/switzerland.osrm
#
#C. Customize (Final optimization step)
#docker run -t -v "${PWD}:/data" ghcr.io/project-osrm/osrm-backend osrm-customize /data/switzerland.osrm
#
#2. Start the OSRM Server
#docker run --name osrm_server -d -p 5000:5000 -v "${PWD}:/data" ghcr.io/project-osrm/osrm-backend osrm-routed --algorithm mld /data/switzerland.osrm
#
# stop and clean up
#docker stop osrm_server
#docker rm osrm_server

library(osrm)
library(sf)
library(pbapply)
library(future)
library(future.apply)


# import the data
sp = read.csv("data/ag-b-00.03-vz2024statpop/STATPOP2024.csv", sep = ";")


municipalities_shp = st_read("data/swissboundaries3d_2025-04_2056_5728.shp/swissBOUNDARIES3D_1_5_TLM_HOHEITSGEBIET.shp") # this will be used as mask to filter the data
gva_shp = municipalities_shp[municipalities_shp$NAME == "Genève",]
gva_shp = st_set_crs(gva_shp, st_crs("EPSG:2056"))
gva_shp_500 = st_buffer(gva_shp, 500)

#5 trim to the GVA area ----
sp = st_as_sf(x=sp, coords = c("E_KOORD", "N_KOORD"))
sp = st_set_crs(sp, st_crs("EPSG:2056"))
sp = st_filter(sp, gva_shp_500)
sp_wgs84 = st_transform(sp, crs = 4326)

# 2. Set the R options for your local OSRM server (must be run in the main session)
options(osrm.server = "http://127.0.0.1:5000/")
options(osrm.profile = "foot")


# 1. Define batch size and breaks
batch_size <- 200
point_batches <- split(1:nrow(sp_wgs84), ceiling(1:nrow(sp_wgs84) / batch_size))
all_results <- list()

# 2. Set parallel plan for the workers *within* each batch
plan(multisession, workers = 4) # Use fewer workers for stability

# 3. Process batches sequentially
for (j in seq_along(point_batches)) {
  message(paste("Processing Batch", j, "of", length(point_batches), "at", Sys.time()))
  
  # Run the parallel pblapply on the current batch of indices
  current_indices <- point_batches[[j]]
  
  batch_results <- pblapply(
    X = current_indices,
    FUN = function(i) {
      # ... (Your optimized function body goes here) ...
      library(osrm)
      library(sf)
      options(osrm.server = "http://127.0.0.1:5000/")
      options(osrm.profile = "foot")
      
      
      single_point <- sp_wgs84[i, ]
      iso_polygon <- osrmIsochrone(loc = single_point, breaks = 15, res = 30)
      return(st_geometry(iso_polygon))
    },
    cl = "future"
  )
  
  all_results[[j]] <- do.call(c, batch_results)
  
  # CRITICAL: Pause the script to allow the server to reset its internal state
  Sys.sleep(10) # Pause for 10 seconds before starting the next batch
}

plan(sequential) 

# 4. Final combination
final_geometries <- do.call(c, all_results)
final_isochrones <- st_sf(id = 1:nrow(sp_wgs84), geometry = final_geometries)


# save intermediary file
saveRDS(final_isochrones, "data/isochrones_raw.rds")



