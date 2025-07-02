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
            
            # for each annotation boundary for a given sample and marker
            for(i in 1:length(tmpList[[1]])){
                vals = sapply(tmpList, function(x) x[i]) # get all of the ith annotation boundaries
                boundaries = quantile(vals, supervision[[1]]) # 1 -> i if allow dif quantiles to be specified for dif gates

                # for each gate for the ith annotation boundary
                for(gate_num in 1:length(tmpList)){
                    # all gates lower than boundaries[1] are set to boundaries[1]
                    if(tmpList[[gate_num]][i]  < boundaries[1]){
                        tmpList[[gate_num]][i] <- boundaries[1]
                    } else if(tmpList[[gate_num]][i]  > boundaries[2]){
                        tmpList[[gate_num]][i] <- boundaries[2]
                    }
                } # could make more efficient if we sort tmpList first
            }
            outList[[channel]] <- tmpList
        }
        # save the updated annotation boundaries
        saveRDS(outList,
                file.path(normalizePath(projectPath),
                          "faustData",
                          "gateData",
                          paste0(parentNode,"_resList.rds")))
    } else {
        # If selectionList empty _resListPrep.rds is copied to _resList.rds
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