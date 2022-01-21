### Library Investment Grants - Neighborhood Poverty
#     2021 Local Library Investment Grants program requires CSL to prioritize
#       libraries in higher-poverty areas. Goal of this project is to
#       use US Census American Community Survey poverty data at the tract
#       level along with an adjustment for cost of living using the
#       California Poverty Measure (PPIC 2021)
#       (https://www.ppic.org/wp-content/uploads/JTF_PovertyJTF.pdf)
#       to calculate the level of poverty in each library service area
#
#     By Patrick Rogers, California Research Bureau
#       Sept 2021
#
#     Uses the following Packages
#       {here}
#       {sf}
#
#     Uses the following data
#       Admin_Boundaries.shp - CA Public Library Jurisdictions
#       CA_Library_Branches.shp - CA Public library locations
#       tl_2019_06_tabblock10.shp - US Census blocks in CA
#       tl_2019_06_tract.shp - US Census tracts in CA
#       tl_2019_06_puma10.shp - US Census PUMAs in CA
#       ACS Tables: Poverty B17001 (https://api.census.gov/data/2019/acs/acs5?get=NAME,group(B17001)&for=tract:*&in=state:06)
#       California Poverty Measure_PUMA.csv - California Poverty Measure estimates at the CA PUMA level


# Pseudo-Code
# 1. Load shapefiles
#   1a. blocks -- used to match with branches and build branch service areas
#   1b. tracts -- used to match FPL data with geographic layers
#   1c. pumas -- used to match tracts with CPM inflation factor
#   1d. fpl -- source for federal poverty data
#   1e. cpm -- source for CPM inflation factor
#   1f. districts -- used to make sure branch matching only pulls in blocks within corresponding library district
#   1g. branches -- used to match with blocks and build service areas
# 
# 2. Build branch-level service areas
#   2a. match blocks to jurisdiction
#   2b. match blocks to branches
#   2c. merge blocks into branch-level service areas
# 
# 3. Branch-level poverty
#   3a. Intersect tracts with pumas to get inflation factor at sub-tracts level
#   3b. Intersect sub-tracts with branch-level service areas
#   3c. Take percentage of in-poverty count based on sub-tract portion sizes
#   3d. Inflate to match CPM
#   3e. Sum up

# Clear the workspace
remove(list=ls(all=TRUE))

# Inits
# None Used

# Load Packages
library(here)
library(sf)

# Load Sourcefiles
# None Used

# User Parameters
# None Used

# Read in Data ####
# Read in block data
blockShape <- st_read(dsn=here("Data", "tl_2019_06_tabblock10.shp"))
blockShapeLabel <- c("GEOID10")
blockShape <- st_transform(blockShape, st_crs("EPSG:3310"))

# Read in tract data
tractShape <- st_read(dsn=here("Data", "tl_2019_06_tract.shp"))
colnames(tractShape)
tractShapeLabel <- c("GEOID")
tractShape <- st_transform(tractShape, st_crs("EPSG:3310"))

# Read in PUMA data
puma <- st_read(dsn = here("Data", "tl_2019_06_puma10.shp"))
colnames(puma)
pumaLabel <- c("GEOID10")
puma <- st_transform(puma, st_crs("EPSG:3310")) # NAD83 / California Albers

temp <- st_drop_geometry(puma[,"NAMELSAD10"])[,1]
temp <- gsub(pattern = "--", " - ", temp)
temp <- gsub(pattern = " PUMA", "", temp)
puma$NAMELSAD10 <- temp

remove(list = "temp")

# Read in jurisdictions data
districtShape <- st_read(dsn=here("Data", "Admin_Boundaries.shp"))
colnames(districtShape)
districtShapeLabel <- c("FSCS_KEY")
districtShape <- st_transform(districtShape, st_crs("EPSG:3310")) # NAD83 / California Albers

districtShape <- st_make_valid(districtShape)

# Read in branches data
branchShape <- st_read(dsn = here("Data", "CA_Library_Branches.shp"))
branchShapeLabel <- c("FSCS_KEY", "FSCS_SEQ")
branchShape <- st_transform(branchShape, st_crs("EPSG:3310"))

# Poverty Data at Tract level
fpl <- read.csv(file = here("Data", "Poverty_ACS_B17001_JSON.csv"))
fpl <- fpl[,c("GEO_ID","B17001_001E","B17001_002E")]
colnames(fpl) <- c("GEOID", "TOTALPOP", "BELOWPOV")
fpl$GEOID <- gsub("1400000US", "", fpl$GEOID)

# CPM Data at PUMA level
cpm <- read.csv(file = here("Data", "California Poverty Measure_PUMA.csv"),
                stringsAsFactors = FALSE)
cpm <- cpm[-1,c(1,2,6)]
colnames(cpm) <- c("PUMA", "CPMPOV", "FPLPOV")
cpm$CPMPOV <- as.numeric(gsub(pattern = "%", "", cpm$CPMPOV))
cpm$FPLPOV <- as.numeric(gsub(pattern = "%", "", cpm$FPLPOV))
cpm$INFLATE <- cpm$CPMPOV/cpm$FPLPOV
cpm <- cpm[!is.na(cpm$INFLATE),]

cpm$PUMA[203] <- puma$NAMELSAD10[93] # Add spacing in label to match PUMA shapefile
cpm$PUMA[244] <- puma$NAMELSAD10[62] # Add " - Redding City" to label to match PUMA shapefile

# CLEAN UP SHAPEFILES ####

# Loomis is coded weirdly in branchShape
branchShape[which(branchShape$FSCS_KEY=="CA270"),"FSCS_KEY"] <- "CA0270"

# Bear Valley Library Station has wrong lat/long
branchShape[(branchShape$FSCS_KEY=="CA0004" & branchShape$FSCS_SEQ==3),c("LONGITUDE","LATITUDE","x","y")] <- c(-120.038826, 38.465813, -120.038826, 38.465813)
st_geometry(branchShape[(branchShape$FSCS_KEY=="CA0004" & branchShape$FSCS_SEQ==3),]) <- st_geometry(st_transform(st_sfc(st_point(c(-120.038826,38.465813)), crs = 4326), st_crs("EPSG:3310")))

# Palos Verdes is missing and Palo Verde Valley is duplicated in districtShape
if(st_drop_geometry(districtShape[163, "Name"]) == "Palo Verde Valley Library District\r\n"){
  districtShape[163,c("Name", "FSCS_KEY")] <- c("Palos Verdes Library District", "CA0092")
} else {
  warning("Rows seem to have shifted, Palo Verde Valley may be duplicated still")
}

# Names in districtShape seem to have carriage return formatting still?
districtShape$Name <- gsub("\r\n", "", districtShape$Name)

# Custom Functions ####
# None Used

# Build Branch-Level Service Areas ####
# Create empty layer for the service area polygons to go into
branchServiceShape <- st_sfc(NA, crs = 3310)

for(i in 1:nrow(districtShape)){
  tictoc::tic()
  # Limit processing to blocks and branches inside the current districtShape polygon
  tempBlocks <- st_intersection(blockShape, districtShape[i,])
  tempBranches <- branchShape[branchShape$FSCS_KEY == districtShape$FSCS_KEY[i],]
  # find the nearest branch to each tempBlock and aggregate those blocks into a service area for each branch
  j <- st_nearest_feature(tempBlocks, tempBranches)
  temp <- aggregate(tempBlocks, by = list(j), mean)
  temp[,branchShapeLabel] <- NA
  temp <- temp[,branchShapeLabel]
  
  if(nrow(temp) != nrow(tempBranches)){
    warning("WARNING!! Missing branches from service area map!")
  } else {
    temp[,branchShapeLabel] <- st_drop_geometry(tempBranches[,branchShapeLabel])
  }
  
  # Add the new service area polygons into the main branchServiceShape layer
  branchServiceShape <- rbind(branchServiceShape, temp)
  
  # Do some logging for error checking
  pdf(file = here("Plots", paste0(gsub("/", " ", districtShape$Name[i]), ".pdf")))
  plot(st_geometry(temp),
       main = districtShape$Name[i],
       axes = TRUE)
  plot(st_geometry(tempBranches), pch = 16, add = TRUE)
  dev.off()
  
  print(paste0(i, ": ", districtShape$Name[i]))
  tictoc::toc()
}

branchServiceShape <- st_make_valid(branchServiceShape)

write_sf(st_transform(branchServiceShape, st_crs(4326)), 
         dsn = here("Data", "Library Branch Service Areas.shp"),
         delete_layer = TRUE)

branchServiceShape <- st_read(dsn = here("Data", "Library Branch Service Areas.shp"))
branchServiceShape <- st_transform(branchServiceShape, st_crs("EPSG:3310"))

# 3. Branch-level poverty
#   3d. Inflate to match CPM
#   3e. Sum up

# Get Branch-Level Poverty Estimates ####
# Merge FPL into tracts
temp <- merge(tractShape, fpl, by = c("GEOID"))
if (nrow(temp) != nrow(tractShape)){
  warning("Some tractShape polygons were lost in merge")
} else {
  tractShape <- temp
  rm(list = "temp")
}

# Merge CPM into PUMAs
temp <- merge(puma, cpm, by.x = "NAMELSAD10", by.y = "PUMA")
if (nrow(temp) != nrow(puma)){
  warning("Some puma polygons were lost in merge")
} else {
  puma <- temp
  rm(list = "temp")
}

# Intersect tracts with pumas to get inflation factor at sub-tract levels
tictoc::tic()
tractShape$TOTALAREA <- st_area(tractShape)
temp <- st_intersection(tractShape, puma)
temp$AREAPUMA <- st_area(temp)
temp <- temp[as.numeric(temp$AREAPUMA) != 0,]
temp$PERCPUMA <- as.numeric(temp$AREAPUMA/temp$TOTALAREA)
print(paste0(nrow(temp)-nrow(tractShape), " additional subdivisions made."))
tractShape <- temp
rm(list = "temp")
tictoc::toc()

# Adjust FPL data for PUMA intersections
tractShape$TOTALPOP <- tractShape$TOTALPOP * tractShape$PERCPUMA
tractShape$BELOWPOV <- tractShape$BELOWPOV * tractShape$PERCPUMA

# Intersect tracts with branch service areas
tictoc::tic()
tractShape$TOTALAREA <- st_area(tractShape)
temp <- st_intersection(tractShape, branchServiceShape)
temp$AREABRANCH <- st_area(temp)
temp <- temp[as.numeric(temp$AREABRANCH) != 0,]
print(paste0(nrow(temp)-nrow(tractShape), " additional subdivisions made."))
temp$PERCBRANCH <- temp$AREABRANCH / temp$TOTALAREA
tractShape <- temp
rm(list = "temp")
tictoc::toc

# Adjust FPL data for branch service area intersections
tractShape$TOTALPOP <- as.numeric(tractShape$TOTALPOP * tractShape$PERCBRANCH)
tractShape$BELOWPOV <- as.numeric(tractShape$BELOWPOV * tractShape$PERCBRANCH)

# Inflate FPL data to match CPM
tractShape$FINALPOV <- tractShape$BELOWPOV * tractShape$INFLATE

# Sum everything up
povertyData <- aggregate(st_drop_geometry(tractShape[,c("TOTALPOP", "FINALPOV")]),
                         by = list(tractShape$FSCS_KEY, tractShape$FSCS_SEQ),
                         FUN = sum)
colnames(povertyData) <- c("FSCS_KEY", "FSCS_SEQ", "TOTALPOP", "CPMADJ")
povertyData$CPMRATE <- (povertyData$CPMADJ / povertyData$TOTALPOP) * 100

povertyData$TOTALPOP <- round(povertyData$TOTALPOP)
povertyData$CPMADJ <- round(povertyData$CPMADJ)

temp <- merge(st_drop_geometry(branchShape[,c("FSCS_KEY", "FSCS_SEQ", "DISTRICT", "BRANCH_NAM")]),
              povertyData,
              by = c("FSCS_KEY", "FSCS_SEQ"))

temp <- merge(branchServiceShape, temp, by = c("FSCS_KEY", "FSCS_SEQ"))

if (nrow(temp) != nrow(povertyData)){
  warning("Some branches were lost in merge")
} else {
  povertyData <- temp
  rm(list = "temp")
}

st_write(povertyData,
         dsn = here("Output", "Poverty Data 20210915.shp"),
         delete_layer = TRUE)

write.csv(st_drop_geometry(povertyData), file = here("Output", "Poverty Data 20210915.csv"),
          row.names = FALSE)
