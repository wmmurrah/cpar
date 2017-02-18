##########################################################################
#
# mplus_load - loads the file and lists all available plots
#
# arguments:
#    file - the quoted name of an existing GH5 file
#
# eg. mplus.load('ex.gh5')
#
#' mplus_load
#' @param file file name
#' @author Thuy Nguyen, Muthen & Muthen
#' @export
mplus.load <- function(file) {

  if (!(file.exists(file))) {
    cstr <- paste("- file does not exist:",file,"\n")
    cat(cstr)
  }

  gh5 <- h5dump(file, load=TRUE)

  cat(c("\nPlot functions:\n"))

  if ("individual_data" %in% names(gh5)) {
    if (exists("mplus.plot.histogram",mode="function")) {
      cat(c(" - mplus.plot.histogram('"),file,"',variable,bins)\n",sep="")
    }
    if (exists("mplus.plot.scatterplot",mode="function")) {
      cat(c(" - mplus.plot.scatterplot('"),file,"',xvar,yvar)\n",sep="")
    }
  }

  if ("process_data" %in% names(gh5) && "means_and_variances_data" %in% names(gh5)) {
    np <- length(attr(gh5$process_data,"names"))
    for (i in c(1:np)) {
      cstr <- paste(c("process"), as.character(i), sep="")
      proc <- gh5$process_data[[cstr]]

      # Replace the line below with series of low-level function calls
      cstr2 <- paste(c("process_data"),"/",cstr,"", sep="")
      prop <- mplus.get.group.attribute(file, cstr2, 'properties')

      values <- attr(gh5$means_and_variances_data,"names")
      series_type <- as.integer(prop[1])

      if (series_type == 1) {
        sm_ind <- pmatch("y_observed_means",values,nomatch=0)
        if (sm_ind > 0 && exists("mplus.plot.sample_means",mode="function")) {
          cstr2 <- paste(c(" - mplus.plot.sample_means('"),file,"','",cstr,"')\n",sep="")
          cat(cstr2)
        }

        em_ind <- pmatch("y_estimated_means",values,nomatch=0)
        if (em_ind > 0 && exists("mplus.plot.estimated_means",mode="function")) {
          cstr2 <- paste(c(" - mplus.plot.estimated_means('"),file,"','",cstr,"')\n",sep="")
          cat(cstr2)
        }

        if (sm_ind>0 && em_ind>0 && exists("mplus.plot.sample_and_estimated_means",mode="function")) {
          cstr2 <- paste(c(" - mplus.plot.sample_and_estimated_means('"),file,"','",cstr,"')\n",sep="")
          cat(cstr2)
        }

        em_ind <- pmatch("y_estimated_modes",values,nomatch=0)
        if (em_ind > 0 && exists("mplus.plot.estimated_modes",mode="function")) {
          cstr2 <- paste(c(" - mplus.plot.estimated_modes('"),file,"','",cstr,"')\n",sep="")
          cat(cstr2)
        }

        em_ind <- pmatch("y_estimated_medians",values,nomatch=0)
        if (em_ind > 0 && exists("mplus.plot.estimated_medians",mode="function")) {
          cstr2 <- paste(c(" - mplus.plot.estimated_medians('"),file,"','",cstr,"')\n",sep="")
          cat(cstr2)
        }
      } else if (series_type == 2) {
        em_ind <- pmatch("latent_estimated_means",values,nomatch=0)
        if (em_ind > 0 && exists("mplus.plot.estimated_means",mode="function")) {
          cstr2 <- paste(c(" - mplus.plot.estimated_means('"),file,"','",cstr,"')\n",sep="")
          cat(cstr2)
        }

        em_ind <- pmatch("latent_estimated_modes",values,nomatch=0)
        if (em_ind > 0 && exists("mplus.plot.estimated_modes",mode="function")) {
          cstr2 <- paste(c(" - mplus.plot.estimated_modes('"),file,"','",cstr,"')\n",sep="")
          cat(cstr2)
        }

        em_ind <- pmatch("latent_estimated_medians",values,nomatch=0)
        if (em_ind > 0 && exists("mplus.plot.estimated_medians",mode="function")) {
          cstr2 <- paste(c(" - mplus.plot.estimated_medians('"),file,"','",cstr,"')\n",sep="")
          cat(cstr2)
        }
      } else if (series_type == 3) {
        em_ind <- pmatch("observed_probs",values,nomatch=0)
        if (em_ind > 0 && exists("mplus.plot.sample_proportions",mode="function")) {
          cstr2 <- paste(c(" - mplus.plot.sample_proportions('"),file,"','",cstr,"',cat1,cat2)\n",sep="")
          cat(cstr2)
        }

        em_ind <- pmatch("estimated_probs",values,nomatch=0)
        if (em_ind > 0 && exists("mplus.plot.estimated_probabilities",mode="function")) {
          cstr2 <- paste(c(" - mplus.plot.estimated_probabilities('"),file,"','",cstr,"',cat1,cat2)\n",sep="")
          cat(cstr2)
        }
      } else {
        cstr2 <- paste(c("'"),cstr,"' has unknown series type.\n")
        cat(cstr2)
      }
    }
  }

  if ("loop_data" %in% names(gh5)) {
    if (exists("mplus.list.loop.labels",mode="function")) {
      cat(c(" - mplus.list.loop.labels('"),file,"')\n",sep="")
    }
    if (exists("mplus.plot.loop",mode="function")) {
      cat(c(" - mplus.plot.loop('"),file,"')\n",sep="")
    }
  }

  if ("irt_data" %in% names(gh5)) {
    if (exists("mplus.list.irt.variables",mode="function")) {
      cat(c(" - mplus.list.irt.variables('"),file,"')\n",sep="")
    }
    if (exists("mplus.list.irt.xvariables",mode="function")) {
      cat(c(" - mplus.list.irt.xvariables('"),file,"')\n",sep="")
    }
    if (exists("mplus.plot.irt.icc",mode="function")) {
      cat(c(" - mplus.plot.irt.icc('"),file,"',group,xvar,uvar,cat,cat2,covariates,xrange,xstep,lloc)\n",sep="")
    }
    if (exists("mplus.plot.irt.iic",mode="function")) {
      cat(c(" - mplus.plot.irt.iic('"),file,"',group,xvar,uvar,covariates,xrange,xstep,lloc)\n",sep="")
    }
    if (exists("mplus.plot.irt.tic",mode="function")) {
      cat(c(" - mplus.plot.irt.tic('"),file,"',group,xvar,uvar,covariates,xrange,xstep)\n",sep="")
    }
  }

  if ("survival_data" %in% names(gh5)) {
    if (exists("mplus.plot.survival.kaplanmeier",mode="function")) {
      cat(c(" - mplus.plot.survival.kaplanmeier('"),file,"',survvar,classnum)\n",sep="")
    }
    if (exists("mplus.plot.survival.baseline",mode="function")) {
      cat(c(" - mplus.plot.survival.baseline('"),file,"',survvar,classnum)\n",sep="")
    }
    if (exists("mplus.plot.survival.basehazard",mode="function")) {
      cat(c(" - mplus.plot.survival.basehazard('"),file,"',survvar,classnum)\n",sep="")
    }
    if (exists("mplus.plot.survival.sample.logcumulative",mode="function")) {
      cat(c(" - mplus.plot.survival.sample.logcumulative('"),file,"',survvar,classnum)\n",sep="")
    }
    if (exists("mplus.plot.survival.estimated.logcumulative",mode="function")) {
      cat(c(" - mplus.plot.survival.estimated.logcumulative('"),file,"',survar,classnum)\n",sep="")
    }
    if (exists("mplus.plot.survival.kaplanmeier.vs.baseline",mode="function")) {
      cat(c(" - mplus.plot.survival.kaplanmeier.vs.baseline('"),file,"',survvar,classnum)\n",sep="")
    }
    if (exists("mplus.plot.survival.sample.vs.estimated.logcumulative",mode="function")) {
      cat(c(" - mplus.plot.survival.sample.vs.estimated.logcumulative('"),file,"',survvar,classnum)\n",sep="")
    }
  }

  if ("discrete_survival_data" %in% names(gh5)) {
    if (exists("mplus.plot.discrete.survival.kaplanmeier",mode="function")) {
      cat(c(" - mplus.plot.discrete.survival.kaplanmeier('"),file,"',survvar,classnum)\n",sep="")
    }
    if (exists("mplus.plot.discrete.survival.baseline",mode="function")) {
      cat(c(" - mplus.plot.discrete.survival.baseline('"),file,"',survvar,classnum)\n",sep="")
    }
    if (exists("mplus.plot.discrete.survival.kaplanmeier.vs.baseline",mode="function")) {
      cat(c(" - mplus.plot.discrete.survival.kaplanmeier.vs.baseline('"),file,"',survvar,classnum)\n",sep="")
    }
  }

  if ("bayesian_data" %in% names(gh5)) {
    if ("parameters_autocorr" %in% names(gh5$bayesian_data)) {
      if ("parameters" %in% names(gh5$bayesian_data$parameters_autocorr)) {
        if (exists("mplus.list.bayesian.parameters",mode="function")) {
          cat(c(" - mplus.list.bayesian.parameters('"),file,"',parameter)\n",sep="")
        }
        if (exists("mplus.plot.bayesian.traceplot",mode="function")) {
          cat(c(" - mplus.plot.bayesian.traceplot('"),file,"',parameter)\n",sep="")
        }
        if (exists("mplus.plot.bayesian.distribution",mode="function")) {
          cat(c(" - mplus.plot.bayesian.distribution('"),file,"',parameter,bins)\n",sep="")
        }
      }
      if ("priors" %in% names(gh5$bayesian_data$parameters_autocorr)) {
        if (exists("mplus.plot.bayesian.prior.distribution",mode="function")) {
          cat(c(" - mplus.plot.bayesian.prior.distribution('"),file,"',parameter,bins)\n",sep="")
        }
      }
      if ("autocorrelation" %in% names(gh5$bayesian_data$parameters_autocorr)) {
        if (exists("mplus.plot.bayesian.autocorrelation",mode="function")) {
          cat(c(" - mplus.plot.bayesian.autocorrelation('"),file,"',parameter,chain)\n",sep="")
        }
      }
    }
    if ("predictive" %in% names(gh5$bayesian_data)) {
      if (exists("mplus.list.bayesian.predictive.labels",mode="function")) {
        cat(c(" - mplus.list.bayesian.predictive.labels('"),file,"')\n",sep="")
      }
      if ("observed" %in% names(gh5$bayesian_data$predictive) && "replicated" %in% names(gh5$bayesian_data$predictive)) {
        if (exists("mplus.plot.bayesian.predictive.scatterplot",mode="function")) {
          cat(c(" - mplus.plot.bayesian.predictive.scatterplot('"),file,"',plabel)\n",sep="")
        }
        if (exists("mplus.plot.bayesian.predictive.distribution",mode="function")) {
          cat(c(" - mplus.plot.bayesian.predictive.distribution('"),file,"',plabel,bins)\n",sep="")
        }
      }
    }
    if ("plausible" %in% names(gh5$bayesian_data)) {
      if (exists("mplus.list.bayesian.plausible.labels",mode="function")) {
        cat(c(" - mplus.list.bayesian.plausible.labels('"),file,"')\n",sep="")
      }
      if (exists("mplus.plot.bayesian.plausible.distribution",mode="function")) {
        cat(c(" - mplus.plot.bayesian.plausible.distribution('"),file,"',plauslabel,obs,bins)\n",sep="")
      }
    }
  }

  cat(c("\nPlot data extraction functions:\n"))

  if ("individual_data" %in% names(gh5)) {
    if (exists("mplus.list.variables",mode="function")) {
      cat(c(" - mplus.list.variables('"),file,"')\n",sep="")
    }
    if (exists("mplus.get.data",mode="function")) {
      cat(c(" - mplus.get.data('"),file,"',variable)\n",sep="")
    }
  }

  if ("process_data" %in% names(gh5)) {
    if (exists("mplus.list.processes",mode="function")) {
      cat(c(" - mplus.list.processes('"),file,"')\n",sep="")
    }
  }

  if ("loop_data" %in% names(gh5)) {
    if (exists("mplus.get.loop.estimates",mode="function")) {
      cat(c(" - mplus.get.loop.estimates('"),file,"',looplabel)\n",sep="")
    }
    if (exists("mplus.get.loop.lowerci",mode="function")) {
      cat(c(" - mplus.get.loop.lowerci('"),file,"',looplabel)\n",sep="")
    }
    if (exists("mplus.get.loop.upperci",mode="function")) {
      cat(c(" - mplus.get.loop.upperci('"),file,"',looplabel)\n",sep="")
    }
    if (exists("mplus.get.loop.xvalues",mode="function")) {
      cat(c(" - mplus.get.loop.xvalues('"),file,"')\n",sep="")
    }
  }

  if ("irt_data" %in% names(gh5)) {
    if (exists("mplus.compute.irt.icc",mode="function")) {
      cat(c(" - mplus.compute.irt.icc('"),file,"',group,xvar,uvar,cat,xvector,covariates)\n",sep="")
    }
    if (exists("mplus.compute.irt.iic",mode="function")) {
      cat(c(" - mplus.compute.irt.iic('"),file,"',group,xvar,uvar,xvector,covariates)\n",sep="")
    }
  }

  if ("process_data" %in% names(gh5) && "means_and_variances_data" %in% names(gh5)) {
    np <- length(attr(gh5$process_data,"names"))
    for (i in c(1:np)) {
      cstr <- paste(c("process"), as.character(i), sep="")
      proc <- gh5$process_data[[cstr]]

      # Replace the line below with series of low-level function calls
      cstr2 <- paste(c("process_data"),"/",cstr,"", sep="")
      prop <- mplus.get.group.attribute(file, cstr2, 'properties')

      values <- attr(gh5$means_and_variances_data,"names")

      if (prop[1] == 1) {
        sm_ind <- pmatch("y_observed_means",values,nomatch=0)
        if (sm_ind > 0 && exists("mplus.get.sample_means",mode="function")) {
          cstr2 <- paste(c(" - mplus.get.sample_means('"),file,"','",cstr,"')\n",sep="")
          cat(cstr2)
        }

        em_ind <- pmatch("y_estimated_means",values,nomatch=0)
        if (em_ind > 0 && exists("mplus.get.estimated_means",mode="function")) {
          cstr2 <- paste(c(" - mplus.get.estimated_means('"),file,"','",cstr,"')\n",sep="")
          cat(cstr2)
        }

        em_ind <- pmatch("y_estimated_modes",values,nomatch=0)
        if (em_ind > 0 && exists("mplus.get.estimated_modes",mode="function")) {
          cstr2 <- paste(c(" - mplus.get.estimated_modes('"),file,"','",cstr,"')\n",sep="")
          cat(cstr2)
        }

        em_ind <- pmatch("y_estimated_medians",values,nomatch=0)
        if (em_ind > 0 && exists("mplus.get.estimated_medians",mode="function")) {
          cstr2 <- paste(c(" - mplus.get.estimated_medians('"),file,"','",cstr,"')\n",sep="")
          cat(cstr2)
        }
      } else if (prop[1] == 2) {
        em_ind <- pmatch("e_estimated_means",values,nomatch=0)
        if (em_ind > 0 && exists("mplus.get.estimated_means",mode="function")) {
          cstr2 <- paste(c(" - mplus.get.estimated_means('"),file,"','",cstr,"')\n",sep="")
          cat(cstr2)
        }

        em_ind <- pmatch("e_estimated_modes",values,nomatch=0)
        if (em_ind > 0 && exists("mplus.get.estimated_modes",mode="function")) {
          cstr2 <- paste(c(" - mplus.get.estimated_modes('"),file,"','",cstr,"')\n",sep="")
          cat(cstr2)
        }

        em_ind <- pmatch("e_estimated_medians",values,nomatch=0)
        if (em_ind > 0 && exists("mplus.get.estimated_medians",mode="function")) {
          cstr2 <- paste(c(" - mplus.get.estimated_medians('"),file,"','",cstr,"')\n",sep="")
          cat(cstr2)
        }
      } else if (prop[1] == 3) {
        em_ind <- pmatch("observed_probs",values,nomatch=0)
        if (em_ind > 0 && exists("mplus.get.sample_proportions",mode="function")) {
          cstr2 <- paste(c(" - mplus.get.sample_proportions('"),file,"','",cstr,"',cat1,cat2)\n",sep="")
          cat(cstr2)
        }

        em_ind <- pmatch("estimated_probs",values,nomatch=0)
        if (em_ind > 0 && exists("mplus.get.estimated_probabilities",mode="function")) {
          cstr2 <- paste(c(" - mplus.get.estimated_probabilities('"),file,"','",cstr,"',cat1,cat2)\n",sep="")
          cat(cstr2)
        }
      } else {
        cstr2 <- paste(c("'"),cstr,"' has unknown series type.\n")
        cat(cstr2)
      }
    }
  }

  if ("survival_data" %in% names(gh5)) {
    if (exists("mplus.list.survival.variables",mode="function")) {
      cat(c(" - mplus.list.survival.variables('"),file,"')\n",sep="")
    }
    if (exists("mplus.get.survival.kaplanmeier.values",mode="function")) {
      cat(c(" - mplus.get.survival.kaplanmeier.values('"),file,"',survvar,classnum,time)\n",sep="")
    }
    if (exists("mplus.compute.survival.sample.logcumulative.values",mode="function")) {
      cat(c(" - mplus.compute.survival.sample.logcumulative.values('"),file,"',survvar,classnum,time)\n",sep="")
    }
    if (exists("mplus.get.survival.baseline.values",mode="function")) {
      cat(c(" - mplus.get.survival.baseline.values('"),file,"',survvar,survvar2,clasnum,time)\n",sep="")
    }
    if (exists("mplus.compute.survival.estimated.logcumulative.values",mode="function")) {
      cat(c(" - mplus.compute.survival.estimated.logcumulative.values('"),file,"',survvar,classnum,time)\n",sep="")
    }
    if (exists("mplus.get.survival.basehazard.values",mode="function")) {
      cat(c(" - mplus.get.survival.basehazard.values('"),file,"',file,survvar,classnum,time)\n",sep="")
    }
  }

  if ("discrete_survival_data" %in% names(gh5)) {
    if (exists("mplus.list.discrete.survival.variables",mode="function")) {
      cat(c(" - mplus.list.discrete.survival.variables('"),file,"')\n",sep="")
    }
    if (exists("mplus.get.discrete.survival.kaplanmeier.values",mode="function")) {
      cat(c(" - mplus.get.discrete.survival.kaplanmeier.values('"),file,"',survvar,classnum,time)\n",sep="")
    }
    if (exists("mplus.get.discrete.survival.baseline.values",mode="function")) {
      cat(c(" - mplus.get.discrete.survival.baseline.values('"),file,"',survvar,survvar2,clasnum,time)\n",sep="")
    }
  }

  if ("bayesian_data" %in% names(gh5)) {
    if ("parameters_autocorr" %in% names(gh5$bayesian_data)) {
      if ("parameters" %in% names(gh5$bayesian_data$parameters_autocorr)) {
        if (exists("mplus.get.bayesian.parameter.data",mode="function")) {
          cat(c(" - mplus.get.bayesian.parameter.data('"),file,"',parameter,chain)\n",sep="")
        }
      }
      if ("priors" %in% names(gh5$bayesian_data$parameters_autocorr)) {
        if (exists("mplus.get.bayesian.prior.parameter.data",mode="function")) {
          cat(c(" - mplus.get.bayesian.prior.parameter.data('"),file,"',parameter)\n",sep="")
        }
      }
      if ("autocorrelation" %in% names(gh5$bayesian_data$parameters_autocorr)) {
        if (exists("mplus.get.bayesian.autocorrelation",mode="function")) {
          cat(c(" - mplus.get.bayesian.autocorrelation('"),file,"',parameter,chain)\n",sep="")
        }
      }
    }
    if ("predictive" %in% names(gh5$bayesian_data)) {
      if ("observed" %in% names(gh5$bayesian_data$predictive)) {
        if (exists("mplus.get.bayesian.predictive.observed",mode="function")) {
          cat(c(" - mplus.get.bayesian.predictive.observed('"),file,"',plabel)\n",sep="")
        }
      }
      if ("replicated" %in% names(gh5$bayesian_data$predictive)) {
        if (exists("mplus.get.bayesian.predictive.replicated",mode="function")) {
          cat(c(" - mplus.get.bayesian.predictive.replicated('"),file,"',plabel)\n",sep="")
        }
      }
      if ("pvalues" %in% names(gh5$bayesian_data$predictive)) {
        if (exists("mplus.get.bayesian.predictive.lowerci",mode="function")) {
          cat(c(" - mplus.get.bayesian.predictive.lowerci('"),file,"',plabel)\n",sep="")
        }
        if (exists("mplus.get.bayesian.predictive.upperci",mode="function")) {
          cat(c(" - mplus.get.bayesian.predictive.upperci('"),file,"',plabel)\n",sep="")
        }
        if (exists("mplus.get.bayesian.predictive.pvalue",mode="function")) {
          cat(c(" - mplus.get.bayesian.predictive.pvalue('"),file,"',plabel)\n",sep="")
        }
        if (exists("mplus.get.bayesian.predictive.pvalue_type",mode="function")) {
          cat(c(" - mplus.get.bayesian.predictive.pvalue('"),file,"',plabel)\n",sep="")
        }
      }
    }
    if ("plausible" %in% names(gh5$bayesian_data)) {
      if (exists("mplus.get.bayesian.plausible.data",mode="function")) {
        cat(c(" - mplus.get.bayesian.plausible.data('"),file,"',plauslabel,obs)\n",sep="")
      }
    }
  }

  invisible(file)
}
