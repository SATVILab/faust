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
            # current annotation boundaries:
            tmpList <- outList[[channel]] 

            # user-specified quantiles which extreme annotation values should be shrunk towards
            supervision <- selectionList[[channel]] 

            # Inform user if they specified the incorrect number of quantiles
            dif_lengths <- FALSE
            if(length(supervision) != length(tmpList[[1]])){
                print(paste0(length(supervision)," quantile set(s) was/were provided for channel ", channel, " but ", length(tmpList[[1]])," gate(s) was/were identified." ))
                print("Using the first quantile set for all gates.")
                dif_lengths <- TRUE
            }
            
            # for each annotation boundary for a given sample and marker
            for(gateNum in 1:length(tmpList[[1]])){
                # calculate desired upper and lower bounds
                vals <- sapply(tmpList, function(x) x[gateNum]) 
                boundaries <- quantile(vals, supervision[[ifelse(dif_lengths,1,gateNum)]]) 

                # for each gate for the ith annotation boundary
                for(sampleNum  in 1:length(tmpList)){
                    # all gates lower than boundaries[1] are set to boundaries[1]
                    if(tmpList[[sampleNum ]][gateNum]  < boundaries[1]){
                        tmpList[[sampleNum ]][gateNum] <- boundaries[1]
                    } else if(tmpList[[sampleNum ]][gateNum]  > boundaries[2]){
                        tmpList[[sampleNum ]][gateNum] <- boundaries[2]
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
            from <- file.path(normalizePath(projectPath),
                             "faustData",
                             "gateData",
                             paste0(parentNode,"_resListPrep.rds")),
            to <- file.path(normalizePath(projectPath),
                           "faustData",
                           "gateData",
                           paste0(parentNode,"_resList.rds")),
            overwrite <- TRUE
        )
    }
    return()
}