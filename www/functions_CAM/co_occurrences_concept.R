# ==============================================================================
# R-Code - CAM
# date of creation: April 2023
# authors: Julius Fenn
# function: 
# ==============================================================================

############################################################################
#
#
############################################################################
### create (and return) data frame of concepts per CAM (column) from CAM-data
listConcepts <- function(datCAM = CAMfiles[[1]], useSummarized = TRUE, removeSuffix = TRUE){

  if(removeSuffix) {
    datCAM$text_summarized <-
      str_remove(string = datCAM$text_summarized , pattern = "_positive$|_negative$|_neutral$|_ambivalent$")

  }

  conceptList <- list()
  for(i in unique(datCAM$CAM)) {
    if (useSummarized == TRUE) {
      concepts <- unlist(datCAM[datCAM$CAM == i, "text_summarized"])
    } else {
      concepts <- unlist(datCAM[datCAM$CAM == i, "text"])
    }
    conceptList[[i]] <- concepts
  }
  conceptsDF <- data.frame(lapply(conceptList, "length<-", max(lengths(conceptList))))
  rownames(conceptsDF) <- NULL
  return(conceptsDF)
}



### Takes a data frame the concepts mentioned in the CAMs (one CAM one column)
### Returns a data frame with all concepts mentioned in more than one CAM (columns "concept") and the number of CAMS where it occurs (column "numCAMoccurences")
countDuplicates <- function(concepts = conceptsDF, orderFrequency = FALSE){
  allCons <- c()

  for(i in 1:ncol(concepts)) {       # for-loop over columns
    CAMcons <- unlist(concepts[ , i], use.names=FALSE)
    allCons <- append(allCons,unique(CAMcons))
  }

  duplicateCons <- names(table(allCons))[table(allCons) >=2]


  duplicateDF <- matrix(nrow = length(duplicateCons), ncol = 2)
  for(i in 1:length(duplicateCons)) {
    duplicateDF[i,] = c(duplicateCons[i], sum(allCons==duplicateCons[i], na.rm=TRUE))
  }
  colnames(duplicateDF) = c("concept","numCAMoccurences")
  duplicateDF <- as.data.frame(duplicateDF)
  duplicateDF$numCAMoccurences <- as.numeric(duplicateDF$numCAMoccurences)


  if( orderFrequency ) {
    duplicateDF <- duplicateDF[order(duplicateDF$numCAMoccurences, decreasing = TRUE),]
  }

  return(duplicateDF)
}


## Creating a Table with binary encoding for duplicate concepts. I.e., each concept mentioned more than once is a column, each CAM is a row.
# If the concept is mentioned in the respective CAM, cell is 1, if not cell is 0
binaryEncoding <- function(conDF = conceptsDF, duplDF = duplicateDF) {
  binaryDF <- data.frame(matrix(nrow = ncol(conDF), ncol = length(duplDF$concept))) # Create an empty dataframe where each column is going to represent a concept and each row a CAM
  colnames(binaryDF) <- duplDF$concept #Name the columns of the dataframe according to the list of concepts occurring more than once
  binaryDF[is.na(binaryDF)] <- 0

  rownames(binaryDF) <- colnames(conDF)

  for(r in 1:nrow(binaryDF)){
    binaryDF[rownames(binaryDF)[r],] <- as.numeric(duplDF$concept %in% conDF[, rownames(binaryDF)[r]])
  }

  return(binaryDF)
}


## Creating a Table with metric encoding for duplicate concepts. I.e., each concept mentioned more than once is a column, each CAM is a row.
# If the concept is mentioned in the respective CAM, cell is X, whereby X is the number of occurrence of concepts in the respective CAM if not cell is 0
# conDF = conceptsDF
# duplDF = duplicateDF
metricEncoding <- function(conDF = conceptsDF, duplDF = duplicateDF) {
  metricDF <- data.frame(matrix(nrow = 0, ncol = length(duplDF$concept))) # Create an empty dataframe where each column is going to represent a concept and each row a CAM
  colnames(metricDF) <- duplDF$concept #Name the columns of the dataframe according to the list of concepts occuring more than once
  #rownames(binaryDF) <- duplDF$concept
  CAMs <- colnames(conDF)
  for(CAM in CAMs) {
    metricVec <- c() #Create an empty vector that is going to be filled with the number of occurrence of concepts in the respective CAM
    for(concept in duplDF$concept) { #For each concept count how often it is mentioned in the respective CAM
      metricVec <- append(metricVec, length(which(conDF[[CAM]] == concept)))
    }
    metricDF[nrow(metricDF) + 1,] = metricVec #Append the vector as a row to binaryDF
    rownames(metricDF)[nrow(metricDF)] <- CAM #Name the respective row by the ID of the CAM it represents
  }

  return(metricDF)
}


## Calculates Phi coefficient and p-value (chi-square) and returns both in a vector
phiCoefficient <- function(var1, var2) {
  crosstab <- table(factor(var1, levels=c(0,1)), factor(var2, levels=c(0,1)))
  chisqTest <- stats::chisq.test(crosstab, correct = FALSE)
  sig <- chisqTest$p.value
  a <- crosstab[1, 1]
  b <- crosstab[1, 2]
  c <- crosstab[2, 1]
  d <- crosstab[2, 2]
  ad <- a * d
  bc <- b * c
  phi <- (ad - bc) / sqrt((a + b) * (c + d) * (a + c) * (b + d))

  ## return earlier is phi is nan (divide by 0)
  if(is.nan(x = phi)){
    # print(phi)
    # print(crosstab)
    return(c(NA, NA))
  }

  if(round(x = phi, digits = 5) != psych::phi(t = crosstab, digits = 5)){
    print("possible ERROR in phi computation")
  }
  return(c(phi, sig))
}



## Calculates pearson coefficient and p-value and returns both in a vector
pearsonCoefficient <- function(var1, var2) {
  correl <- cor.test(var1, var2)
  sig <- correl$p.value
  #print(typeof(correl$estimate))
  r <- correl$estimate
  return(c(r, sig))
}


## Calculates Cohens Kappa
cohensKappa <- function(var1, var2) {

  outCohen <- irr::kappa2(cbind(var1, var2))

  sig <- outCohen$p.value
  r <- outCohen$value

  return(c(r, sig))
}


## Builds a list that contains two Dataframes: one that contains the correlation coefficients for the pairwise co-occurence of all concepts and one the respective p-values
# args
# conList = duplicateDF
# inputTable =  binaryDf
# CorrFUNC = cohensKappa # pearsonCoefficient, phiCoefficient
# considerValence = TRUE
# datNodes = CAMfiles[[1]]

correlationTable <- function(conList = duplicateDF,
                             inputTable = binaryDf,
                             CorrFUNC,
                             considerValence = TRUE,
                             datNodes = NULL,
                             useSummarized = TRUE) {

  if(considerValence){
    ## update inputTable df by valence ratings
    if(useSummarized){
      datNodes$text_summarized <-
        str_remove(string = datNodes$text_summarized, pattern = "_positive$|_negative$|_neutral$|_ambivalent$")
    } else{
      datNodes$text <-
        str_remove(string = datNodes$text, pattern = "_positive$|_negative$|_neutral$|_ambivalent$")  # Just to make sure
    }
    datNodes$value <- ifelse(test = datNodes$value == 10, yes = 0, no = datNodes$value)

    for(r in 1:nrow(inputTable)){
      tmp_CAMid <- str_remove(string = rownames(inputTable)[r], pattern = "^X")
      tmp_CAMid <- str_replace_all(string = tmp_CAMid, pattern = "\\.", replacement = "-")

      tmp_datNodes <- datNodes[datNodes$CAM == tmp_CAMid,]

      for(c in 1:ncol(inputTable)){
        if(inputTable[r,c] == 1){
          if(useSummarized){
            tmp_value <- tmp_datNodes$value[tmp_datNodes$text_summarized %in% colnames(inputTable)[c]]
          } else{
            tmp_value <- tmp_datNodes$value[tmp_datNodes$text %in% colnames(inputTable)[c]]
          }
          
          if (mean(tmp_value) > 0){
            inputTable[r,c] <- "positive"
          }else if (mean(tmp_value) < 0){
            inputTable[r,c] <- "negative"
          } else {
            inputTable[r,c] <- "neutral"
          }
        }
      }
    }

    # error message if no overlapping words
    # for(c in 1:ncol(inputTable)){
    #   inputTable[,c] <- factor(inputTable[,c], levels=c("positive","negative","neutral"))
    # }
  }



  ## set up data frames
  mat_est <- matrix(data = NA, nrow = length(conList$concept), ncol = length(conList$concept))
  mat_p <- matrix(data = NA, nrow = length(conList$concept), ncol = length(conList$concept))

  for(r in 1:length(conList$concept)) {
    # print(r)
    for(c in 1:length(conList$concept)) {
      # print(c)

      if(r == c){
        mat_est[r, c] <- 1  # add estimate (main diagonal always 1)
        mat_p[r, c] <- 0 # add p-value
      }else if(r < c){
        newCell <- CorrFUNC(var1 = inputTable[, r],
                            var2 = inputTable[, c])
        # add estimate (phi or corr)
        mat_est[r, c] <- newCell[1]
        mat_est[c, r] <- newCell[1]
        # add p-value
        mat_p[r, c] <- newCell[2]
        mat_p[c, r] <- newCell[2]
      }
    }
  }

  rownames(mat_est) <- conList$concept
  colnames(mat_est) <- conList$concept

  rownames(mat_p) <- conList$concept
  colnames(mat_p) <- conList$concept


  return(list(mat_est, mat_p))
}
