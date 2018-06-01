loadCASFRI <- function(CASFRIRas, attrFile, headerFile) {
  CASFRIattr <- fread(attrFile)

  ## WORKAROUND: maunally extracted the column names because the text file is
  ## very inconsistent (blank lines and uses mix of tabs and spaces as delimiters)
  CASFRIheader <- c("GID", "CAS_ID", "SPECIES_1", "SPECIES_PER_1", "SPECIES_2",
                    "SPECIES_PER_2", "SPECIES_3", "SPECIES_PER_3", "SPECIES_4",
                    "SPECIES_PER_4", "SPECIES_5", "SPECIES_PER_5",
                    "LYR_CROWN_CLOSURE_LOWER", "LYR_CROWN_CLOSURE_UPPER",
                    "LYR_HEIGHT_LOWER", "LYR_HEIGHT_UPPER", "WETLAND_TYPE",
                    "SOIL_MOIST_REG", "AGE", "NAT_NON_VEG", "NON_FOR_ANTH",
                    "NON_FOR_VEG", "HAS_DST", "HAS_NFL", "HAS_LYR")
  #CASFRIheader <- fread(headerFile, skip = 14, nrows = 50, header = FALSE,
  #                      sep = "\t") ## doesn't work

  setnames(CASFRIattr, CASFRIheader)
  set(CASFRIattr, , grep(CASFRIheader, pattern = "^SPECIES|^GID|^AGE", invert = TRUE), NULL)
  #setnames(CASFRIattr, CASFRIheader$V1)
  #set(CASFRIattr, , grep(CASFRIheader$V1, pattern = "^SPECIES|^GID|^AGE", invert = TRUE), NULL)
  setkey(CASFRIattr, "GID")
  NAVals <- c("XXXX MISS", "UNDEF", "XXXX ERRC")
  for (i in 1:5) {
    set(CASFRIattr, which(CASFRIattr[[paste0("SPECIES_", i)]] %in% NAVals),
        paste0("SPECIES_", i), NA_character_)
    set(CASFRIattr, which(CASFRIattr[[paste0("SPECIES_PER_", i)]] %in% NAVals),
        paste0("SPECIES_", i), NA_character_)
  }
  for (i in 1:1) {
    CASFRIattr <- CASFRIattr[which(CASFRIattr[[paste0("SPECIES_PER_", i)]] > 15), ]
  }
  for (i in 2:5) {
    set(CASFRIattr, which(CASFRIattr[[paste0("SPECIES_PER_", i)]] <= 15),
        paste0("SPECIES_", i), NA_character_)
  }

  keepSpecies <- whSpecies(CASFRIattr, topN = 16) # 16 most abundant species
  CASFRIattrLong <- melt(CASFRIattr, id.vars = c("GID"),
                         measure.vars = paste0("SPECIES_", 1:5))
  CA2 <- melt(CASFRIattr, id.vars = c("GID"),
              measure.vars = c(paste0("SPECIES_PER_", 1:5)))
  CASFRIattrLong[, pct := CA2$value]
  rm(CA2)
  CASFRIattrLong <- na.omit(CASFRIattrLong)

  CASFRIdt <- CASFRIRas[] %>% data.table(GID = ., rastInd = 1:ncell(CASFRIRas))
  CASFRIdt <- CASFRIdt[, isNA := is.na(GID)]
  CASFRIdt <- CASFRIdt[isNA == FALSE]
  setkey(CASFRIdt, GID)
  set(CASFRIdt, , "isNA", NULL)

  return(list(keepSpecies = keepSpecies, CASFRIattrLong = CASFRIattrLong,
              CASFRIdt = CASFRIdt))
}

whSpecies <- function(CASFRIattr, topN = 16) {
  spAbund <- CASFRIattr[, .N, by = "SPECIES_1"] %>% setkeyv("N") #%>% print()
  spAbund2 <- CASFRIattr[, .N, by = "SPECIES_2"] %>% setkeyv("N") #%>% print()
  setorder(spAbund, -N)
  setorder(spAbund2, N)
  keepSpecies <- data.table(keepSpecies = spAbund$SPECIES_1[1:topN])
  set(keepSpecies, , "spGroup", keepSpecies$keepSpecies)
  setkey(keepSpecies, keepSpecies)
  keepSpecies <- keepSpecies[!"Pseu menz"]
  keepSpecies[c("Pice glau", "Pice enge", "Pice hybr", "Pice spp."), spGroup := "Pice_gla"]
  keepSpecies["Pice mari", spGroup := "Pice_mar"]
  keepSpecies["Betu papy", spGroup := "Betu_pap"]
  keepSpecies[c("Abie bals", "Abie lasi"), spGroup := "Abie_sp"]
  keepSpecies[c("Lari lari"), spGroup := "Lari_lar"]
  keepSpecies[c("Pinu cont", "Pinu conl"), spGroup := "Pinu_sp"]
  keepSpecies[c("Pinu bank", "Pinu spp."), spGroup := "Pinu_sp"]
  keepSpecies[c("Popu trem", "Popu balb"), spGroup := "Popu_tre"]
  keepSpecies
}

makePaulStack <- function(paths, PaulRaster, uniqueKeepSp, destinationPath) {
  PaulRaster[] <- PaulRaster[]
  PaulRaster[PaulRaster[] %in% c(230, 220, 255)] <- NA_integer_ # water, non veg
  #Paulvals <- sort(unique(PaulRaster[]))
  PaulStack <- list()
  #uniqueKeepSp <- unique(loadedCASFRI$keepSpecies$spGroup)

  rasterOptions(maxmemory = 1e9)
  NA_Sp <- c("Abie_sp", "Betu_pap", "Lari_lar")
  if (!(all(NA_Sp %in% uniqueKeepSp)))
    stop("Codes in loadedCASFRI have changed: expecting ", NA_Sp[!(NA_Sp %in% uniqueKeepSp)])

  for (N in lapply(NA_Sp, grep, uniqueKeepSp, value = TRUE)) {
    message("  running ", N, ", assigning NA because not enough data")
    PaulStack[[N]] <- raster(PaulRaster) %>% setValues(NA_integer_)
    PaulStack[[N]] <- Cache(writeRaster, PaulStack[[N]],
                            filename = asPath(file.path(destinationPath, paste0("Paul", N, ".tif"))),
                            overwrite = TRUE, datatype = "INT2U")
  }

  N <- "Pice_gla"
  message("  converting Paul's codes to pct cover raster, for ", N)
  PaulStack[[N]] <- raster(PaulRaster) %>% setValues(NA_integer_)
  PaulStack[[N]][PaulRaster[] %in% c(41, 42, 43)] <- 60
  PaulStack[[N]][PaulRaster[] %in% c(44)] <- 80
  PaulStack[[N]][PaulRaster[] %in% c(14, 34)] <- 40
  PaulStack[[N]] <- Cache(writeRaster, PaulStack[[N]] ,
                          filename = asPath(file.path(destinationPath, paste0("Paul", N, ".tif"))),
                          overwrite = TRUE, datatype = "INT2U")

  # 5
  N <- "Pice_mar"
  message("  converting Paul's codes to pct cover raster, for ", N)
  PaulStack[[N]] <- raster(PaulRaster) %>% setValues(NA_integer_)
  PaulStack[[N]][PaulRaster[] %in% c(23, 26)] <- 60
  PaulStack[[N]][PaulRaster[] %in% c(22)] <- 80
  PaulStack[[N]][PaulRaster[] %in% c(32, 42)] <- 40
  PaulStack[[N]] <- Cache(writeRaster, PaulStack[[N]],
                          filename = asPath(file.path(destinationPath, paste0("Paul", N, ".tif"))),
                          overwrite = TRUE, datatype = "INT2U")

  # 6
  N <- "Pinu_sp"
  message("  converting Paul's codes to pct cover raster, for ", N)
  PaulStack[[N]] <- raster(PaulRaster) %>% setValues(NA_integer_)
  PaulStack[[N]][PaulRaster[] %in% c(31, 32, 34)] <- 60
  PaulStack[[N]][PaulRaster[] %in% c(33)] <- 80
  PaulStack[[N]][PaulRaster[] %in% c(23, 43)] <- 40
  PaulStack[[N]] <- Cache(writeRaster, PaulStack[[N]],
                          filename = asPath(file.path(destinationPath, paste0("Paul", N, ".tif"))),
                          overwrite = TRUE, datatype = "INT2U")


  # 7
  N <- "Popu_tre"
  message("  converting Paul's codes to pct cover raster, for ", N)
  PaulStack[[N]] <- raster(PaulRaster) %>% setValues(NA_integer_)
  PaulStack[[N]][PaulRaster[] %in% c(14)] <- 60
  PaulStack[[N]][PaulRaster[] %in% c(11)] <- 80
  PaulStack[[N]][PaulRaster[] %in% c(31, 41)] <- 40
  PaulStack[[N]] <- Cache(writeRaster, PaulStack[[N]],
                          filename = asPath(file.path(destinationPath, paste0("Paul", N, ".tif"))),
                          overwrite = TRUE, datatype = "INT2U")

  stack(PaulStack)
}

CASFRItoSpRasts <- function(CASFRIRas, loadedCASFRI, destinationPath) {
  spRasts <- list()
  spRas <- raster(CASFRIRas) %>% setValues(., NA_integer_)
  for (sp in unique(loadedCASFRI$keepSpecies$spGroup)) {
    spRasts[[sp]] <- spRas
    message("starting ", sp)
    aa2 <- loadedCASFRI$CASFRIattrLong[
      value %in% loadedCASFRI$keepSpecies[spGroup == sp, keepSpecies]][
        , min(100L, sum(pct)), by = GID]
    setkey(aa2, GID)
    cc <- aa2[loadedCASFRI$CASFRIdt] %>% na.omit()
    rm(aa2)
    spRasts[[sp]][cc$rastInd] <- cc$V1
    message("  ", sp, " writing to disk")
    rastTmp <- writeRaster(
      spRasts[[sp]],
      filename = asPath(file.path(destinationPath, paste0("CASFRI",sp,".tif"))),
      datatype = "INT2U", overwrite = TRUE
    )
    if (is(rastTmp, "Raster")) { # Rasters need to have their disk-backed value assigned, but not shapefiles
      # This is a bug in writeRaster was spotted with crs of rastTmp became
      # +proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs
      # should have stayed at
      # +proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0
      if (!identical(crs(rastTmp), crs(spRasts[[sp]])))
        crs(rastTmp) <- crs(spRasts[[sp]])

      spRasts[[sp]] <- rastTmp
    }
    message("  ", sp, " done")
  }

  stack(spRasts)
}

overlayStacks <- function(highQualityStack, lowQualityStack, outputFilenameSuffix = "overlay",
                          destinationPath) {
  for (sp in layerNames(highQualityStack)) {
    hqLarger <- ncell(lowQualityStack) * prod(res(lowQualityStack)) <
      ncell(highQualityStack) * prod(res(highQualityStack))

    if (sp %in% layerNames(lowQualityStack)) {
      if (!(all(
        isTRUE(all.equal(extent(lowQualityStack), extent(highQualityStack))),
        isTRUE(all.equal(crs(lowQualityStack), crs(highQualityStack))),
        isTRUE(all.equal(res(lowQualityStack), res(highQualityStack)))))) {
        message("  ", sp, " extents, or resolution, or projection did not match; ",
                "using gdalwarp to make them overlap")
        LQRastName <- basename(tempfile(fileext = ".tif"))
        if (!nzchar(filename(lowQualityStack[[sp]]))) {
          LQCurName <- basename(tempfile(fileext = ".tif"))
          lowQualityStack[[sp]][] <- as.integer(lowQualityStack[[sp]][])
          lowQualityStack[[sp]] <- writeRaster(lowQualityStack[[sp]], filename = LQCurName,
                                               datatype = "INT2U")
        }

        LQRastInHQcrs <- projectExtent(lowQualityStack, crs = crs(highQualityStack))
        # project LQ raster into HQ dimensions
        gdalwarp(overwrite = TRUE,
                 dstalpha = TRUE,
                 s_srs = as.character(crs(lowQualityStack[[sp]])),
                 t_srs = as.character(crs(highQualityStack[[sp]])),
                 multi = TRUE, of = "GTiff",
                 tr = res(highQualityStack),
                 te = c(xmin(LQRastInHQcrs), ymin(LQRastInHQcrs),
                        xmax(LQRastInHQcrs), ymax(LQRastInHQcrs)),
                 filename(lowQualityStack[[sp]]), ot = "Byte",
                 LQRastName)

        LQRast <- raster(LQRastName)
        LQRast[] <- LQRast[]
        unlink(LQRastName)

        try(unlink(LQCurName), silent = TRUE)

        if (hqLarger) {
          tmpHQName <- basename(tempfile(fileext = ".tif"))

          gdalwarp(overwrite = TRUE,
                   dstalpha = TRUE,
                   s_srs = as.character(crs(highQualityStack[[sp]])),
                   t_srs = as.character(crs(highQualityStack[[sp]])),
                   multi = TRUE, of = "GTiff",
                   tr = res(highQualityStack),
                   te = c(xmin(LQRastInHQcrs), ymin(LQRastInHQcrs),
                          xmax(LQRastInHQcrs), ymax(LQRastInHQcrs)),
                   filename(highQualityStack[[sp]]), ot = "Byte", tmpHQName)
          HQRast <- raster(tmpHQName)
          HQRast[] <- HQRast[]
          HQRast[HQRast[] == 255] <- NA_integer_
          unlink(tmpHQName)
        } else {
          HQRast <- highQualityStack[[sp]]
        }
      } else {
        LQRast <- lowQualityStack[[sp]]
        HQRast <- highQualityStack[[sp]]
      }
      message("  Writing new, overlaid ", sp, " raster to disk.")
      if (!compareRaster(LQRast, HQRast))
        stop("Stacks not identical, something is wrong with overlayStacks function.")

      nas <- is.na(HQRast[])
      HQRast[nas] <- LQRast[][nas]
      HQRast <- writeRaster(HQRast, datatype = "INT2U",
                            filename = file.path(destinationPath,
                                                 paste0(sp, "_", outputFilenameSuffix, ".tif")),
                            overwrite = TRUE)
      names(HQRast) <- sp
      if (!exists("HQStack")) HQStack <- stack(HQRast) else HQStack[[sp]] <- HQRast
    }
  }
  HQStack
}

gdalwarp2 <- function(rasterWithDiskBacked, dstfilename, ...) {
  dstfilenameTmp <- .suffix(dstfilename, "_tmp")
  gdalwarp(srcfile = basename(filename(rasterWithDiskBacked)),
           dstfile = basename(dstfilenameTmp), ...)

  rr <- raster(dstfilenameTmp)
  rr[] <- rr[]
  if (is.integer(rr[])) {
    dt <- if (maxValue(rr) > 65534) {
      "INT4U"
    } else {
      "INT2U"
    }
  }
  rr[rr[] == 255] <- NA_integer_
  rr <- writeRaster(rr, filename = dstfilename,
                    datatype = dt,  overwrite = TRUE)
  unlink(dstfilenameTmp)
  rr
}


