.superviseReconciliation <- function(projectPath, debugFlag)
{
    # parentNode is whatever is stored in the sanitizedCellPopStr.rds file - often "root"
    parentNode <- readRDS(file.path(normalizePath(projectPath),
                                    "faustData",
                                    "metaData",
                                    "sanitizedCellPopStr.rds"))

    # selectionList is a list of the items from the user-specified "supervisedList" which have actionType == "PostSelection"
    selectionListUser <- readRDS(file.path(normalizePath(projectPath),
                                       "faustData",
                                       "metaData",
                                       "selectionList.rds"))
    userSpecifiedChannels <- names(selectionListUser)
    
    if (debugFlag) print("Selection specific reconciled annotation boundaries.")

    # resListPrep contains the annotation boundaries for all the markers, 
    # post accounting for forcing and preference, but not yet post-selection.
    resListPrep <- readRDS(file.path(normalizePath(projectPath),
                                        "faustData",
                                        "gateData",
                                        paste0(parentNode,"_resListPrep.rds")))
    outList <- resListPrep
    # all channel names
    selectedChannels <- names(resListPrep)
    # create default list: c(0.1, 0.9) for each gate on each marker
    selectionList         <- list()
    length(selectionList) <- length(selectedChannels)
    names(selectionList)  <- selectedChannels
    # for each gate in each channel, set the quantiles
    for(channel in selectedChannels){
        # If the user has specified quantiles for this channel, use them.
        if(channel %in% userSpecifiedChannels){
            specifiedQuantiles <- selectionListUser[[channel]]
        }else{ 
            # Otherwise, use the default quantiles of c(0.1, 0.9) for each gate.
            specifiedQuantiles <- list()
            for(gate in 1:length(outList[[channel]][[1]])){
                specifiedQuantiles <- append(specifiedQuantiles, list(c(0.1, 0.9))) # default
            }
        }
        selectionList[[channel]] <- specifiedQuantiles
    }

    for (channel in selectedChannels) {
        # current annotation boundaries:
        tmpList <- outList[[channel]] 

        # user-specified quantiles which extreme annotation values should be shrunk towards
        supervision <- selectionList[[channel]] 

        # Inform user if they specified the incorrect number of quantiles
        difLen <- FALSE
        if(length(supervision) != length(tmpList[[1]])){
            print(paste0(length(supervision)," quantile set(s) was/were provided for channel ", channel, " but ", length(tmpList[[1]])," gate(s) was/were identified." ))
            print("Using the first quantile set for all gates.")
            difLen <- TRUE
        }
        
        # for each annotation boundary for a given sample and marker
        for(gateNum in 1:length(tmpList[[1]])){
            # calculate desired upper and lower bounds
            vals <- sapply(tmpList, function(x) x[gateNum]) 
            boundaries <- quantile(vals, supervision[[ifelse(difLen,1,gateNum)]]) 

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
    return()
}