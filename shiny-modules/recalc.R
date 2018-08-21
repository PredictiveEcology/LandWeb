recalcLargePatches <- function(input, output, session, rctLrgPatches, rctLrgPatchesCC,
                               rctChosenPolyName, rctPolygonList, largePatchesFn,
                               tsfFile, vtmFile, ageClasses, ageClassCutOffs,
                               useParallelCluster, .largePatchesCalcFn, authStatus) {
  lrgPtchs <- reactive({
    if (is.null(rctLrgPatches()[[rctChosenPolyName()]])) {
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

      updateList(rctLrgPatches(), lrgPatchesUser)
    } else {
      rctLrgPatches()
    }
  })

  lrgPtchsCC <- reactive({
    if (authStatus() && is.null(rctLrgPatchesCC()[[rctChosenPolyName()]])) {
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

      updateList(rctLrgPatchesCC(), lrgPatchesUser) ## TODO: test this
    } else {
      rctLrgPatchesCC()
    }
  })

  return(list(largePatches = lrgPtchs, largePatchesCC = lrgPtchsCC))
}

recalcLeading <- function(input, output, session, rctLeadingDTlist, rctLeadingDTlistCC,
                          rctChosenPolyName, rctPolygonList, leadingByStageFn,
                          tsf, vtm, ageClasses, ageClassCutOffs, authStatus) {
  ldng <- reactive({
    if (is.null(rctLeadingDTlist()[[rctChosenPolyName()]])) {
      newPoly <- rctPolygonList()[[rctChosenPolyName()]]$crsSR
      leadingDTlistUser <- Cache(leadingByStageFn, tsf = tsf, vtm = vtm,
                                 polygonToSummarizeBy = newPoly,
                                 ageClassCutOffs = ageClassCutOffs,
                                 ageClasses = ageClasses)

      updateList(rctLeadingDTlist(), leadingDTlistUser) ## TODO: test this
    } else {
      rctLeadingDTlist()
    }
  })

  ldngCC <- reactive({
    if (authStatus() && is.null(rctLeadingDTlistCC()[[rctChosenPolyName()]])) {
      newPoly <- rctPolygonList()[[rctChosenPolyName()]]$crsSR
      leadingDTlistUserCC <- Cache(leadingByStageFn, tsf = tsf, vtm = vtm,
                                 polygonToSummarizeBy = newPoly,
                                 ageClassCutOffs = ageClassCutOffs,
                                 ageClasses = ageClasses)

      updateList(rctLeadingDTlistCC(), leadingDTlistUserCC) ## TODO: test this
    } else {
      rctLeadingDTlist()
    }
  })

  return(list(leading = ldng, leadingCC = ldngCC))
}
