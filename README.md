# BREP
In progress code to develop an SST indicator of loggerhead presence within the California Bight. Will inform the timing of the DGN / loggerhead closure.

# Script 1
NC_batch_SST_SSTANOM.R
  reads in all netcdf files and converts into data frame (pixel ID x time), write out as monthly .rds files (pixel ID x time)

# Script 2
corellation_DF.r
  reads in monthly .rds files, integrates sightings data, writes out as monthly .rds files, runs pixel x sightings correlations, writes out pngs and grid files
  
# Script 3
priority_areas.R
  explores priority areas for SST box, writes out binary reclassified grid files
  
# Script 4
create_rule.R
  given an SST box, explores relationships between monthly spatial averages and sightings data
  
# Script 5
ENSO.R
  correlated sighting data w ENSO index for comparison w SST box
  
# Script 6
test_rules.R
  developes moderate and conservative rules for best (most correlated) indicator in each month