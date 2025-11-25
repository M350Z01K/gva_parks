# GVA Parks

An analysis of the park accessibility in the city of Geneva


## Get the data 

Download the following data, unzip and store them in the folder _data_

- [STATPOP](https://www.bfs.admin.ch/bfs/en/home/statistics/catalogues-databases.assetdetail.36171301.html)
- [Shapefiles of the Swiss municipalities](https://data.geo.admin.ch/ch.swisstopo.swissboundaries3d/swissboundaries3d_2025-04/swissboundaries3d_2025-04_2056_5728.gdb.zip)
- [Parks of Geneva](https://sitg.ge.ch/donnees/obs-equipements-espaces-pub)
- [Forests of Geneva](https://sitg.ge.ch/donnees/ffp-cadastre-foret) (if you are interested in the surface of forests)


## Run the scripts

- first extract the isochrones _isochrones_calc_optimized.R_
- then perform data extraction _extraction.R_
- finally perform the data analysis and generate the charts _analysis.R_ (if you want to run the analysis directly, just make sure you have downloaded the shapefiles of the municipalities of Switzerland)