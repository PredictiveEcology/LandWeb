recalcLargePatches <- function(input, output, session, rctLrgPatches, rctLrgPatchesCC,
                               rctChosenPolyName, rctPolygonList, largePatchesFn,
                               tsfFile, vtmFile, ageClasses, ageClassCutOffs,
                               useParallelCluster, .largePatchesCalcFn, authStatus) {
  lrgPtchs <- reactive({
    #browser()

    if (is.null(rctLrgPatches()[[rctChosenPolyName()]])) {
      crayon::magenta(message("Recalculating large patches for uploaded polygon..."))
      newPoly <- rctPolygonList()[[rctChosenPolyName()]]$crsSR
      lrgPatchesUser <- Cache(
        largePatchesFn,
        byPoly = newPoly,
        tsfFile = tsfFile,
        vtmFile = vtmFile,
        ageClasses = ageClasses,
        ageClassCutOffs = ageClassCutOffs,
        labelColumn = sim2$labelColumn, ## shinyLabel
        useParallelCluster = useParallelCluster,
        .largePatchesCalc = .largePatchesCalcFn # need to Cache the internals
      )
      crayon::magenta(message("  done."))
      lrgPatchesUser
    } else {
      rctLrgPatches()[[rctChosenPolyName()]]
    }
  })

  lrgPtchsCC <- reactive({
    if (authStatus() && is.null(rctLrgPatchesCC()[[rctChosenPolyName()]])) {
      crayon::magenta(message("Recalculating large patches CC for uploaded polygon..."))
      newPoly <- rctPolygonList()[[rctChosenPolyName()]]$crsSR
      lrgPatchesUserCC <- Cache(
        largePatchesFn,
        byPoly = newPoly,
        tsfFile = tsfFile,
        vtmFile = vtmFile,
        ageClasses = ageClasses,
        ageClassCutOffs = ageClassCutOffs,
        labelColumn = sim2$labelColumn, ## shinyLabel
        useParallelCluster = useParallelCluster,
        .largePatchesCalc = .largePatchesCalcFn # need to Cache the internals
      )
      crayon::magenta("  done.")
      lrgPatchesUserCC
    } else {
      rctLrgPatchesCC()[[rctChosenPolyName()]]
    }
  })

  return(list(largePatches = lrgPtchs, largePatchesCC = lrgPtchsCC))
}

recalcLeading <- function(input, output, session, rctLeadingDTlist, rctLeadingDTlistCC,
                          rctChosenPolyName, rctPolygonList, leadingByStageFn,
                          tsf, vtm, ageClasses, ageClassCutOffs, authStatus) {
  ldng <- reactive({
    if (is.null(rctLeadingDTlist()[[rctChosenPolyName()]])) {
      crayon::magenta(message("Recalculating leading veg type for uploaded polygon..."))
      newPoly <- rctPolygonList()[[rctChosenPolyName()]]$crsSR
      leadingDTlistUser <- Cache(leadingByStageFn, tsf = tsf, vtm = vtm,
                                 polygonToSummarizeBy = newPoly,
                                 ageClassCutOffs = ageClassCutOffs,
                                 ageClasses = ageClasses)
      crayon::magenta("  done.")
      leadingDTlistUser
    } else {
      rctLeadingDTlist()[[rctChosenPolyName()]]
    }
  })

  ldngCC <- reactive({
    if (authStatus() && is.null(rctLeadingDTlistCC()[[rctChosenPolyName()]])) {
      crayon::magenta(message("Recalculating leading veg type CC for uploaded polygon..."))
      newPoly <- rctPolygonList()[[rctChosenPolyName()]]$crsSR
      leadingDTlistUserCC <- Cache(leadingByStageFn, tsf = tsf, vtm = vtm,
                                   polygonToSummarizeBy = newPoly,
                                   ageClassCutOffs = ageClassCutOffs,
                                   ageClasses = ageClasses)
      crayon::magenta("  done.")
      leadingDTlistUserCC
    } else {
      rctLeadingDTlistCC()[[rctChosenPolyName()]]
    }
  })

  return(list(leading = ldng, leadingCC = ldngCC))
}
