Landweb_CASFRI_GIDs dataset description

Pierre Racine (pierre.racine@sbf.ulaval.ca), 29 nov. 2016

The "Landweb_CASFRI_GIDs.tif" raster contains an index to CASFRI v. 4 stand geometries. Each row in the attribute table represents a distinct CASFRI stand with a selection of its attributes.

The "Landweb_CASFRI_GIDs.tif" raster covers all the study area covered by the raster provided two weeks ago where there are CASFRI attributes. All pixels outside the study area or inside the study area but not covered by CASFRI were assigned a NODATA value equal to zero (0).

All other pixels were assigned the unique identifier of the corresponding CASFRI stand. It is a 32 bits unsigned integer. There can be many pixels with the same identifier since the same stand may cover many pixels. This identifier match a line in the "Landweb_GIDs_attributes.csv" file. This is a comma separated value file, meaning comma is the effective column separator.

Missing values, in this file, occurs when no attributes about the related stand could be found in one or many of the CASFRI tables (DST, LYR, NFL). Other missing value (undocumented) codes like -9999, -8888 or -1111 are found for missing or erroneous attributes when other meaningfull attributes were found in the same CASFRI table.

The CASFRI attributes included in the CSV table are:

GID                     The unique geometry identifier of the CASFRI stand. (This is not the CAS_ID which is alphanumeric, and hence can not be assigned to a pixel, but there is one and only one GID per CAS_ID).

CAS_ID                  The Common Attribute Schema unique stand identifier.

SPECIES_1		Leading or only species.

SPECIES_PER_1		Percent species composition in 10% increments of the leading species.
       
SPECIES_2		Second leading species.

SPECIES_PER_2		Percent species composition in 10% increments of the second leading species.

SPECIES_3		Third leading species.

SPECIES_PER_3		Percent species composition in 10% increments of the third leading species.

SPECIES_4		Fourth leading species.

SPECIES_PER_4		Percent species composition in 10% increments of the fourth leading species.

SPECIES_5		Fifth leading species.

SPECIES_PER_5		Percent species composition in 10% increments of the fifth leading species.

LYR_CROWN_CLOSURE_LOWER Estimate of percentage of ground area covered by vertical projection of crowns. Lower bound of crown closure class.

LYR_CROWN_CLOSURE_UPPER Estimate of percentage of ground area covered by vertical projection of crowns. Upper bound of crown closure class.

LYR_HEIGHT_LOWER	Average stand height by layer of the dominant and codominant trees (leading species) or other vegetation. Lower bound of height class.

LYR_HEIGHT_UPPER	Average stand height by layer of the dominant and codominant trees (leading species) or other vegetation. Upper bound of height class.

WETLAND_TYPE            Wetland type. Bog, Fen, Swamp, Marsh, Shallow, Open Water, Tidal Flats, Estuary, Wetland, Not Wetland

SOIL_MOIST_REG          Available moisture supply for plant growth. Dry, Mesic (Fresh), Moist, Wet, and Aquatic.

AGE			The stand age is computed like this: photo_year - (origin_upper + origin_lower)/2. Missing value codes (-9999, -8888, -1111) were provided when one of photo_year, origin_upper or origin_lower were missing.

NAT_NON_VEG		Naturally non-vegetated land. Alpine, lake, River, Ocean, Rock, Sand, Snow, Slide, Exposed Land, Beach, Water Sediments, Flood, Island, Tidal Flats

NON_FOR_ANTH		Non-vegetated anthropogenic land. Industrial, Facility/Infrastructure, Cultivated, Settlement, Lagoon, Borrow Pit, Other

NON_FOR_VEG		Non-forested vegetated land. Tall Shrub, Low Shrub, Forbs, Herbs, Graminoids, Bryoid, Open Muskeg, Tundra

HAS_DST			Flag indicating if some attributes were found in the DST CASFRI table.

HAS_NFL			Flag indicating if some attributes were found in the NFL CASFRI table.

HAS_LYR			Flag indicating if some attributes were found in the LYR CASFRI table.
       
Details about the meaning of the codes and values found in the attribute table can be found in the "CASSummary_Cosco_2011.pdf" document accompanying this dataset.

The raster is georefenced in the EPSG:3978 Lambert Conformal Conic coordinate system having the following parameters:

Projection: Lambert_Conformal_Conic
false_easting: 0.0
false_northing: 0.0
central_meridian: -95.0
standard_parallel_1: 49.0
standard_parallel_2: 77.0
latitude_of_origin: 49.0
Linear Unit: Meter (1.0)

Geographic Coordinate System: GCS_North_American_1983
Angular Unit: Degree (0.0174532925199433)
Prime Meridian: Greenwich (0.0)
Datum: D_North_American_1983
  Spheroid: GRS_1980
    Semimajor Axis: 6378137.0
    Semiminor Axis: 6356752.314140356
    Inverse Flattening: 298.257222101
