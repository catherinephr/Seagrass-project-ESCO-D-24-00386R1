# Influence of Environmental Factors on pH Buffering, Productivity, and Blue Carbon Storage in Contrasting Thalassia testudinum Seagrass Meadows in La Parguera, Puerto Rico

Catherine Hernández Rodríguez1*, Johann Collazo Reyes1, Denise Ivette Perez2, Juan Gonzalez-Corredor3, Johannes R. Krause3, 
Justin E. Campbell3, James W. Fourqurean3, Juan Jose Cruz Motta1, Travis A Courtney1
1 Department of Marine Sciences, University of Puerto Rico Mayagüez, Mayagüez, Puerto Rico
2 Caribbean Division, The Nature Conservancy, San Juan, Puerto Rico
3 Institute of Environment, Coastlines and Oceans Division, and Department of Biological Sciences, Florida International University, Miami, Florida, USA

---

## Project Overview

Seagrass meadows provide essential ecosystem services such as **pH buffering**, **primary production**, and **blue carbon storage**, 
contributing to the mitigation of climate change and coastal acidification. Despite their ecological importance, few studies have simultaneously quantified 
the **biogeochemical, hydrodynamic, and biological drivers** that regulate these processes in tropical seagrass ecosystems.

This repository contains the data analysis and visualization script associated with the manuscript


## Study Summary

Two contrasting *Thalassia testudinum* meadows were studied in **La Parguera Nature Reserve (Puerto Rico)**:

- **Isla Magueyes (Inshore site):** low-flow, mangrove-adjacent environment.  
- **Cayo Laurel (Offshore site):** moderate-flow, reef-adjacent environment.

By combining **in situ environmental sensors** (CTD, MiniDOT, PAR, current meters) with **spatial seawater chemistry surveys** (TA, DIC, pH) and 
**biological measurements** (cover, productivity, biomass, sediment carbon cores), the study quantified:
- Temporal variability in **temperature, dissolved oxygen, light (PAR), and current velocity**.  
- Spatial gradients in **pH buffering and carbon chemistry**.  
- Differences in **productivity, biomass, and organic carbon stocks**.

## Repository Contents


This repository includes an R script that reproduces all analyses and figures developed for the manuscript. The script performs three main tasks:
1. Time–Series Analysis Using Autonomous Sensors
The script processes and visualizes high-frequency environmental data collected using autonomous sensors deployed at two contrasting Thalassia testudinum seagrass meadows (Laurel and Magueyes).
Time-series plots were generated for the following parameters:
-Temperature (°C)
-Depth (m)
-Dissolved Oxygen (DO, mg/L)
-Photosynthetically Active Radiation (PAR, µmol m⁻² s⁻¹)
-Current Speed (cm/s)

These plots show synchronized time windows across both sites to facilitate comparison of diel patterns and environmental variability.

2. Spatial Contour Maps (Contour Plots)
The script also generates spatial contour maps using discrete seawater chemistry samples collected during four surveys:
(a) Laurel early morning
(b) Laurel early afternoon
(c) Magueyes early morning
(d) Magueyes early afternoon

Contour plots were developed for:

-Dissolved Inorganic Carbon (nDIC)
-Total Alkalinity (nTA)
-Dissolved Oxygen (DO)
-pH (pHₜ)

A green arrow overlaid on each map represents current direction and relative speed during each survey period, based on Tilt Current Meter measurements.

3. Type II Linear Regression (nTA vs nDIC)
The script performs a Model II linear regression (lmodel2) to examine the relationship between seawater total alkalinity (nTA) and dissolved inorganic carbon (nDIC) from the spatial chemistry surveys.
This plot allows comparison of carbonate-system dynamics and organic carbon cycling between the two seagrass meadows.
