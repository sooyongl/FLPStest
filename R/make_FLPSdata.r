#' data setting depending on latent variable models
#'
setData <- function(info, ...) {
  UseMethod("setData", info)
}

setData.default <- function(info) {

}

setData.flpsIRT <- function(info) {

}

#' get information for data generation ready
#'
infoSetting <- function(...) {

  info <- list(...)
  lv_type <- info$lv_type
  info$lv_type <- NULL

  structure(info, class = lv_type)
}

#' Convert a matrix to a FLPS data
#'
#' @param inp_data A matrix or a data frame
#' @param custom_data A list. should be provided with custome_stan
#' @param custom_stan A string. should be provided with custome_data
#' @param outcome A character indicating the name of an outcome variable
#' @param group A character indicating the name of a treatment/control group variable
#' @param covariate A character indicating the names of covariates variables
#' @param lv_model A description of the latent variable model, which is similar to the lavaan model syntax.
#' @param lv_type  A character indicating the type of latent variable models
#' @param ... Additional arguments for latent variable models information (e.g., nclass = 2).
#'
#' @returns a flpsData class.
#'
# #' @export
makeFLPSdata <- function(inp_data, outcome, group, covariate, lv_model, lv_type, custom = F, ...) {
  # flps_data <- dataSetting() ; S3 class
  dotdotdot <- list(...)

  if(custom) {

    ## S3
    out <- S3class("flpsData")

    out$outcome <- outcome
    out$group <- group
    out$covariate <- covariate
    out$lv_type <- class(inp_data)
    out$lv_model <- lv_model
    out$stan_data <- inp_data


  } else {

    inp_data <- data.frame(inp_data)

    outcome.data <- unname(unlist(inp_data[outcome]))
    group.data <- unname(unlist(inp_data[group]))
    covariate.data <- inp_data[covariate]

    lv_model1 <- unlist(strsplit(lv_model, "\n"))
    lv_model2 <- do.call("rbind",strsplit(lv_model1, "=~"))
    lv_model3 <- unlist(strsplit(lv_model2[, 2], "\\+"))
    lv_model4 <- unlist(strsplit(lv_model3, " "))

    obs.v.name <- lv_model4[lv_model4 != ""]
    obs.v.matrix <- inp_data[obs.v.name]

    obs.v.partial <- obs.v.matrix[group.data == 1, ]

    nsec <- ncol(obs.v.partial)
    nstu <- nrow(obs.v.matrix)

    obs.v.idx <- which(!is.na(obs.v.partial), arr.ind = T)

    obs.v.vector <- sapply(1:nrow(obs.v.idx),
                           function(n) obs.v.matrix[obs.v.idx[n,1], obs.v.idx[n,2]])

    a_idx <- gen_a_idx(nsec, nfac = 1) # temporary 1
    fi_idx <- detect_firstitem(a_idx)

    flps_data <- list(
      nsecWorked = length(obs.v.idx[,2]),
      nstud = nstu,
      nsec = nsec,

      studentM = unname(obs.v.idx[,1]),
      section = unname(obs.v.idx[,2]),

      grad = obs.v.vector,
      X = covariate.data,
      ncov = ncol(covariate.data),

      Z = group.data,
      Y = outcome.data,

      firstitem = fi_idx,
      factoridx = a_idx,
      nfac = 1
    )

    if(TRUE) {
      obtain_prior <- match.fun("obv_lambda")
    } else {
      obtain_prior <- match.fun("latent_lambda")
    }

    lv_type <- toupper(lv_type)
    if(lv_type %in% c("IRT","RASCH","2PL","3PL")) {
      flps_data$lambda_prior <- obtain_prior(obs.v.partial, a_idx)
      flps_data$min_k <- min(obs.v.vector)
      flps_data$max_k <- max(obs.v.vector)
      ## S3
      out <- S3class("flpsIRT")

      ## S4
      # out <- new("flpsIRT")
    }

    if(lv_type %in% c("GPCM","PCM","RSM")) {
      flps_data$lambda_prior <- obtain_prior(obs.v.partial, a_idx)
      flps_data$min_k <- min(obs.v.vector)
      flps_data$max_k <- max(obs.v.vector)

      ## S3
      out <- S3class("flpsGPCM")

      ## S4
      # out <- new("flpsGPCM")
    }

    if(lv_type %in% c("SEM","CFA")) {
      flps_data$lambda_prior <- obtain_prior(obs.v.partial, a_idx)

      ## S3
      out <- S3class("flpsSEM")

      ## S4
      # out <- new("flpsSEM")
    }

    if(lv_type %in% c("LGM")) {
      flps_data$time_loading <- dotdotdot$time_loading

      ## S3
      out <- S3class("flpsLGM")

      ## S4
      # out <- new("flpsLGM")
    }

    if(lv_type %in% c("LPA","LCA","MIXTURE","GMM")) {
      flps_data$nclass <- dotdotdot$nclass

      ## S3
      out <- S3class("flpsMixture")
      out$nclass <- dotdotdot$nclass

      ## S4
      # out <- new("flpsMixture")
      # out@nclass <- dotdotdot$nclass

    }

    ## S3
    out$outcome <- outcome
    out$group <- group
    out$covariate <- covariate
    out$lv_type <- lv_type
    out$lv_model <- lv_model
    out$lv_data <- obs.v.partial
    out$stan_data <- flps_data

  }

  return(out)
}

