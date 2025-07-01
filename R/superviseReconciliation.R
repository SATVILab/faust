.superviseReconciliation <- function(projectPath,debugFlag)
{
    # parentNode is whatever is stored in the sanitizedCellPopStr.rds file - often "root"
    parentNode <- readRDS(file.path(normalizePath(projectPath),
                                    "faustData",
                                    "metaData",
                                    "sanitizedCellPopStr.rds"))
    # selectionList is a list of the items from the user-specified "supervisedList" which have actionType == "PostSelection"
    selectionList <- readRDS(file.path(normalizePath(projectPath),
                                       "faustData",
                                       "metaData",
                                       "selectionList.rds"))

    if (length(selectionList) > 0){
        if (debugFlag) print("Selection specific reconciled annotation boundaries.")
        # resListPrep contains the annotation boundaries for all the markers, 
        # post accounting for forcing and preference, but not yet post-selection.
        resListPrep <- readRDS(file.path(normalizePath(projectPath),
                                         "faustData",
                                         "gateData",
                                         paste0(parentNode,"_resListPrep.rds")))
        outList <- resListPrep
        # all of the selected channels 
        selectedChannels <- names(resListPrep)
        # all of the channels the user wants to shrink
        supervisedChannels <- names(selectionList)
        if (length(setdiff(supervisedChannels,selectedChannels))) {
            print("The following unselected channels (by depth score) are detected.")
            print(setdiff(supervisedChannels,selectedChannels))
            print("Proceding as if these are controlled values.")
        }
        for (channel in supervisedChannels) {
            # use the selection list to set the standard
            # current annotation boundaries
            tmpList <- outList[[channel]] 
            # user-specified quantiles which extreme annotation values should be shrunk towards
            supervision <- selectionList[[channel]] 
            # tmpList[[1]] contains gates for channel for all samples
            boundaries = quantile(as.numeric(resListPrep[["Ki67"]]), c(0.1, 0.9))
            # all gates lower than boundaries[1] are set to boundaries[1]
            for(gate_num in 1:length(tmpList)){
                if(tmpList[[gate_num]] < boundaries[[1]]){tmpList[[gate_num]][1] <- boundaries[1]}
                else if(tmpList[[gate_num]] > boundaries[2]){tmpList[[gate_num]][1] <- boundaries[2]}
            } # could make more efficient if we sort tmpList first
            outList[[channel]] <- tmpList
        }
        # save the updated annotation boundaries
        saveRDS(outList,
                file.path(normalizePath(projectPath),
                          "faustData",
                          "gateData",
                          paste0(parentNode,"_resList.rds")))
    }
    # If selectionList empty _resListPrep.rds is copied to _resList.rds
    else {
        file.copy(
            from = file.path(normalizePath(projectPath),
                             "faustData",
                             "gateData",
                             paste0(parentNode,"_resListPrep.rds")),
            to = file.path(normalizePath(projectPath),
                           "faustData",
                           "gateData",
                           paste0(parentNode,"_resList.rds")),
            overwrite = TRUE
        )
    }
    return()
}




