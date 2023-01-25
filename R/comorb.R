#' Convert diagnosis codes to AHRQ comorbidities
#'
#' Convert ICD-10 diagnosis codes to a table of indicators for each of the 39
#' comorbidities using the 2021.1 definitions of the comorbidities.
#'
#' The input table should have columns for diagnosis code and POA flag.  Optionally
#' there can be an ID column, the value of which indicates which diagnoses belong
#' to the same aggregation unit (e.g., claim, or patient).  If the ID column is
#' omitted, the entire table is assumed to belong to a single aggregation unit, as
#' might be the case if, for example, a larger table were split on an ID column
#' before being passed in.
#'
#' The return value will be a table that includes the ID column, if any, and
#' columns for the 39 comorbidities, with a 1 indicating that the comorbidity is
#' present, and a 0 indicating that it is absent.  The clinically similar conditions
#' are accounted for, so if multiple levels of severity are indicated by the diagnosis
#' codes, only the most severe will be recorded.
#'
#' Note that we leave the two subcategories of cerebrovascular disease (CBVD
#' and CBVD_SQLA) separate because they are separated in the reference table
#' supplied by AHRQ.
#'
#' @param tbl Table of input data
#' @param diagcol Column containing the diagnosis codes.  Default is 'diagnosis'.
#' @param poacol Column containing the present-on-admission flags.  Default is 'poa'.
#' @param idcol Column containing the id variable (see details).  Default is 'id'.
#' Passing \code{NULL} will treat each row as a separate case.
#' @param dotless If \code{TRUE}, assume the diagnosis codes are already in dotless
#' format. Otherwise, the codes will be converted to dotless. Setting this flag
#' can save a little time if you are certain your codes are already dotless.
#' @export
comorb <- function(tbl, diagcol = 'diagnosis', poacol = 'poa', idcol=NULL, dotless = FALSE)
{
  comorb_description <- AHRQComorb::comorb_description

  if(is.null(idcol)) {
    tbl <- dplyr::bind_cols(id=1, tbl[,c(diagcol, poacol)])
    idcol <- 'id'
    rtnid <- FALSE
  }
  else {
    tbl <- tbl[, c(idcol, diagcol, poacol)]
    rtnid <- TRUE
  }
  #names(tbl) <- c('id','diagnosis', 'poa')

  if(!dotless) {
    tbl[[diagcol]] <- stringr::str_replace(tbl[[diagcol]], '[.]', '')
  }

  codes <- icd10_comorb[-1]  ## drop the column with the diagnosis description
  joinby <- 'diagnosis'
  names(joinby) <- diagcol
  rslt <- dplyr::inner_join(tbl, codes, by=joinby)

  if(nrow(rslt) == 0) {
    ## No diagnosis codes matched any comorbidities; record 0 for all comorbidities
    ## for each id grouping
    rslt <- tibble::tibble(id=unique(tbl[[idcol]]))
    for(cm in comorb_description[['comorbidity']]) {
      rslt[[cm]] <- 0
    }
    if(rtnid) {
      return(rslt)
    }
    else {
      return(rslt[comorb_description[['comorbidity']]])   # Return just the comorbidity columns
    }
  }

  ## Check POA for each condition, if necessary.
  poacond <- comorb_description$comorbidity[comorb_description$poa]
  for (cond in poacond) {
    rslt[[cond]] <- rslt[[cond]] * rslt[[poacol]]
  }

  ## Aggregate the results by id.
  rsplt <- split(rslt, rslt[[idcol]], drop=TRUE)
  rslt <- do.call(rbind,
                  lapply(rsplt, function(d) {
                    id <- d[1,1]
                    ## First three columns are id, diagnosis, and poa; the rest are comorbidities
                    comorbs <- t(apply(as.matrix(d[-c(1,2,3)]),2, intor))
                    r <- cbind(id=id, as.data.frame(comorbs))
                    colnames(r)[1] <- idcol
                    r
                  })
  )

  ## Correct clinically similar comorbidities
  rslt[,'diab_uncx'] <- pintand(rslt[,'diab_uncx'], !rslt[,'diab_cx'])
  rslt[,'htn_uncx'] <- pintand(rslt[,'htn_uncx'], !rslt[,'htn_cx'])
  rslt[,'liver_mld'] <- pintand(rslt[,'liver_mld'], !rslt[,'liver_sev'])
  rslt[,'renlfl_mod'] <- pintand(rslt[,'renlfl_mod'], !rslt[,'renlfl_sev'])
  rslt[,'cancer_nsitu'] <- pintand(rslt[,'cancer_nsitu'], !(rslt[,'cancer_solid'] | rslt[,'cancer_mets']))
  rslt[,'cancer_solid'] <- pintand(rslt[,'cancer_solid'], !rslt[,'cancer_mets'])

  if(rtnid) {
    rslt
  }
  else {
    rslt[comorb_description[['comorbidity']]]
  }
}
