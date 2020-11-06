#_________________________________________________________________________________________________________________________________________________

library(dplyr)
library(mgcv)
library(dplyr)
library(itsadug)
library(tidymv)
library(ggplot2)
library(boot)
library(stringr)
library(pander)
library(erer)
library(knitr)
library(bayesplot)
library(kableExtra)

#_________________________________________________________________________________________________________________________________________________

subitdx = function(data, dx_col, Male=NULL, Female=NULL){
  
  if(is.null(Male)){
    if(is.null(Female)){
      dx_nt = data %>%
        mutate(SexN = Sex) %>% 
        filter(UQ(sym(dx_col)) == 1 | NT == 1) %>%
        mutate(nGroup = factor(ifelse(NT == 1, "Neurotypical", dx_col))) %>%
        mutate(Group = factor(ifelse(NT == 1, "Neurotypical", dx_col))) %>%
        mutate(Group = ordered(Group, levels = c("Neurotypical", dx_col))) %>% 
        mutate(IQ = as.numeric(IQ)) %>%  
        mutate(Sex = factor(ifelse(Sex == 1, "Female", "Male"))) %>% 
        mutate(Sex = ordered(Sex, levels = c("Male", "Female"))) %>% 
        mutate(Study_Site = factor(Study_Site)) 
    }
  }
  if(!is.null(Male)){
    if(is.null(Female)){
      dx_nt = data %>%
        mutate(Sex = factor(ifelse(Sex == 1, "Female", "Male"))) %>% 
        filter(Sex == "Male") %>% 
        filter(UQ(sym(dx_col)) == 1 | NT == 1) %>%
        mutate(nGroup = factor(ifelse(NT == 1, "Neurotypical", dx_col))) %>%
        mutate(Group = factor(ifelse(NT == 1, "Neurotypical", dx_col))) %>%
        mutate(Group = ordered(Group, levels = c("Neurotypical", dx_col))) %>%
        mutate(IQ = as.numeric(IQ)) %>%
        mutate(Study_Site = factor(Study_Site)) 
    }
  }
  if(!is.null(Female)){
    if(is.null(Male)){
      dx_nt = data %>%
        mutate(Sex = factor(ifelse(Sex == 1, "Female", "Male"))) %>% 
        filter(Sex == "Female") %>% 
        filter(UQ(sym(dx_col)) == 1 | NT == 1) %>%
        mutate(nGroup = factor(ifelse(NT == 1, "Neurotypical", dx_col))) %>%
        mutate(Group = factor(ifelse(NT == 1, "Neurotypical", dx_col))) %>%
        mutate(Group = ordered(Group, levels = c("Neurotypical", dx_col))) %>% 
        mutate(IQ = as.numeric(IQ)) %>%  
        mutate(Study_Site = factor(Study_Site)) 
    }
  }
  assign("dx_nt",dx_nt,envir = .GlobalEnv)
}

#_________________________________________________________________________________________________________________________________________________

subitincome = function(data, High=NULL, Medium=NULL, Low = NULL, Male=NULL, Female=NULL){
  if(is.null(Male)){
    if(is.null(Female)){
      if(!is.null(Low)){
        if(is.null(Medium)){
          if(!is.null(High)){
            income_df = data %>%
              filter(Income == "High" | Income == "Low") %>% 
              mutate(Group = factor(Income)) %>% 
              mutate(Group = ordered(Group, levels = c("High", "Low"))) %>%
              mutate(IQ = as.numeric(IQ)) %>% 
              mutate(Sex = factor(ifelse(Sex == 1, "Female", "Male"))) %>% 
              mutate(Sex = ordered(Sex, levels = c("Male", "Female"))) %>% 
              mutate(Study_Site = factor(Study_Site))
          }
        }
      }
    }
  }
  if(is.null(Male)){
    if(is.null(Female)){
      if(is.null(Low)){
        if(!is.null(Medium)){
          if(!is.null(High)){
            income_df = data %>%
              filter(Income == "High" | Income == "Medium") %>% 
              mutate(Group = factor(Income)) %>% 
              mutate(Group = ordered(Group, levels = c("High", "Medium"))) %>%
              mutate(IQ = as.numeric(IQ)) %>% 
              mutate(Sex = factor(ifelse(Sex == 1, "Female", "Male"))) %>% 
              mutate(Sex = ordered(Sex, levels = c("Male", "Female"))) %>% 
              mutate(Study_Site = factor(Study_Site))
          }
        }
      }
    }
  } 
  if(!is.null(Male)){
    if(is.null(Female)){
      if(!is.null(Low)){
        if(is.null(Medium)){
          if(!is.null(High)){
            income_df = data %>%
              filter(Income == "High" | Income == "Low") %>% 
              mutate(Group = factor(Income)) %>% 
              mutate(Group = ordered(Group, levels = c("High", "Low"))) %>%
              mutate(IQ = as.numeric(IQ)) %>% 
              mutate(Sex = factor(ifelse(Sex == 1, "Female", "Male"))) %>% 
              filter(Sex == "Male") %>% 
              mutate(Study_Site = factor(Study_Site))
          }
        }
      }
    }
  } 
  if(is.null(Male)){
    if(!is.null(Female)){
      if(!is.null(Low)){
        if(is.null(Medium)){
          if(!is.null(High)){
            income_df = data %>%
              filter(Income == "High" | Income == "Low") %>% 
              mutate(Group = factor(Income)) %>% 
              mutate(Group = ordered(Group, levels = c("High", "Low"))) %>%
              mutate(IQ = as.numeric(IQ)) %>% 
              mutate(Sex = factor(ifelse(Sex == 1, "Female", "Male"))) %>% 
              filter(Sex == "Female") %>% 
              mutate(Study_Site = factor(Study_Site))
          }
        }
      }
    }
  } 
  if(!is.null(Male)){
    if(is.null(Female)){
      if(is.null(Low)){
        if(!is.null(Medium)){
          if(!is.null(High)){
            income_df = data %>%
              filter(Income == "High" | Income == "Medium") %>% 
              mutate(Group = factor(Income)) %>% 
              mutate(Group = ordered(Group, levels = c("High", "Medium"))) %>%
              mutate(IQ = as.numeric(IQ)) %>% 
              mutate(Sex = factor(ifelse(Sex == 1, "Female", "Male"))) %>% 
              filter(Sex == "Male") %>% 
              mutate(Study_Site = factor(Study_Site))
          }
        }
      }
    }
  } 
  if(is.null(Male)){
    if(!is.null(Female)){
      if(is.null(Low)){
        if(!is.null(Medium)){
          if(!is.null(High)){
            income_df = data %>%
              filter(Income == "High" | Income == "Medium") %>% 
              mutate(Group = factor(Income)) %>% 
              mutate(Group = ordered(Group, levels = c("High", "Medium"))) %>%
              mutate(IQ = as.numeric(IQ)) %>% 
              mutate(Sex = factor(ifelse(Sex == 1, "Female", "Male"))) %>% 
              filter(Sex == "Female") %>% 
              mutate(Study_Site = factor(Study_Site))
          }
        }
      }
    }
  } 
  if(is.null(Male)){
    if(is.null(Female)){
      if(!is.null(Low)){
        if(!is.null(Medium)){
          if(is.null(High)){
            income_df = data %>%
              filter(Income == "Medium" | Income == "Low") %>% 
              mutate(Group = factor(Income)) %>% 
              mutate(Group = ordered(Group, levels = c("Medium", "Low"))) %>%
              mutate(IQ = as.numeric(IQ)) %>% 
              mutate(Sex = factor(ifelse(Sex == 1, "Female", "Male"))) %>% 
              mutate(Sex = ordered(Sex, levels = c("Male", "Female"))) %>%
              mutate(Study_Site = factor(Study_Site))
          }
        }
      }
    }
  } 
  if(!is.null(Male)){
    if(is.null(Female)){
      if(!is.null(Low)){
        if(!is.null(Medium)){
          if(is.null(High)){
            income_df = data %>%
              filter(Income == "Medium" | Income == "Low") %>% 
              mutate(Group = factor(Income)) %>% 
              mutate(Group = ordered(Group, levels = c("Medium", "Low"))) %>%
              mutate(IQ = as.numeric(IQ)) %>% 
              mutate(Sex = factor(ifelse(Sex == 1, "Female", "Male"))) %>% 
              filter(Sex == "Male") %>% 
              mutate(Study_Site = factor(Study_Site))
          }
        }
      }
    }
  } 
  if(is.null(Male)){
    if(!is.null(Female)){
      if(!is.null(Low)){
        if(!is.null(Medium)){
          if(is.null(High)){
            income_df = data %>%
              filter(Income == "Medium" | Income == "Low") %>% 
              mutate(Group = factor(Income)) %>% 
              mutate(Group = ordered(Group, levels = c("Medium", "Low"))) %>%
              mutate(IQ = as.numeric(IQ)) %>% 
              mutate(Sex = factor(ifelse(Sex == 1, "Female", "Male"))) %>% 
              filter(Sex == "Female") %>% 
              mutate(Study_Site = factor(Study_Site))
          }
        }
      }
    }
  } 
  
  assign("income_df",income_df,envir = .GlobalEnv)
}

#_________________________________________________________________________________________________________________________________________________

subit = function(data, S1, S2, Male=NULL, Female=NULL){
  
  if(is.null(Male)){
    if(is.null(Female)){
      subtype_df = data %>%
        filter(Subtype == S1 | Subtype == S2) %>% 
        mutate(Group = factor(Subtype)) %>% 
        mutate(Group = ifelse(Group == S1, paste('Subtype', sep= " ", paste(S1)), 
                              ifelse(Group == S2, paste('Subtype', sep= " ", paste(S2)),NA))) %>%
        mutate(Group = ordered(Group, levels = c(paste('Subtype', sep= " ", paste(S1)), paste('Subtype', sep= " ", paste(S2))))) %>%
        mutate(IQ = as.numeric(IQ)) %>% 
        #mutate(Income = factor(Income)) %>% 
        mutate(Sex = factor(ifelse(Sex == 1, "Female", "Male"))) %>% 
        mutate(Sex = ordered(Sex, levels = c("Male", "Female"))) %>% 
        mutate(Study_Site = factor(Study_Site))
    }
  }
  if(!is.null(Male)){
    if(is.null(Female)){
          subtype_df = data %>%
            filter(Subtype == S1 | Subtype == S2) %>% 
            mutate(Group = factor(Subtype)) %>% 
            mutate(Group = ifelse(Group == S1, paste('Subtype', sep= " ", paste(S1)),
                                  ifelse(Group == S2, paste('Subtype', sep= " ", paste(S2)),NA))) %>% 
            mutate(Group = ordered(Group, levels = c(paste('Subtype', sep= " ", paste(S1)), paste('Subtype', sep= " ", paste(S2))))) %>%
            mutate(IQ = as.numeric(IQ)) %>% 
            #mutate(Income = factor(Income)) %>% 
            mutate(Sex = factor(ifelse(Sex == 1, "Female", "Male"))) %>% 
            filter(Sex == "Female") %>% 
            mutate(Study_Site = factor(Study_Site))
        }
      }
  if(is.null(Male)){
    if(!is.null(Female)){
          subtype_df = data %>%
            filter(Subtype == S1 | Subtype == S2) %>% 
            mutate(Group = factor(Subtype)) %>% 
            mutate(Group = ordered(Group, levels = c(S1, S2))) %>%
            mutate(Group = ifelse(Group == S1, paste('Subtype', sep= " ", paste(S1)),
                                  ifelse(Group == S2, paste('Subtype', sep= " ", paste(S2)),NA))) %>% 
            mutate(Group = ordered(Group, levels = c(paste('Subtype', sep= " ", paste(S1)), paste('Subtype', sep= " ", paste(S2))))) %>%
            mutate(IQ = as.numeric(IQ)) %>% 
           # mutate(Income = factor(Income)) %>% 
            mutate(Sex = factor(ifelse(Sex == 1, "Female", "Male"))) %>% 
            filter(Sex == "Female") %>% 
            mutate(Study_Site = factor(Study_Site))
        }
      }
  assign("subtype_df",subtype_df,envir = .GlobalEnv)
}

#_________________________________________________________________________________________________________________________________________________


subitdx = function(data, dx_col, Male=NULL, Female=NULL){
  
  if(is.null(Male)){
    if(is.null(Female)){
      dx_nt = data %>%
        mutate(SexN = Sex) %>% 
        filter(UQ(sym(dx_col)) == 1 | NT == 1) %>%
        mutate(Group = factor(ifelse(NT == 1, "Neurotypical", dx_col))) %>%
        mutate(Group = factor(ifelse(NT == 1, "Neurotypical", dx_col))) %>%
        mutate(Group = ordered(Group, levels = c("Neurotypical", dx_col))) %>% 
        mutate(IQ = as.numeric(IQ)) %>%  
        mutate(Sex = factor(ifelse(Sex == 1, "Female", "Male"))) %>% 
        mutate(Sex = ordered(Sex, levels = c("Male", "Female"))) %>% 
        mutate(Study_Site = factor(Study_Site)) 
    }
  }
  if(!is.null(Male)){
    if(is.null(Female)){
      dx_nt = data %>%
        mutate(Sex = factor(ifelse(Sex == 1, "Female", "Male"))) %>% 
        filter(Sex == "Male") %>% 
        filter(UQ(sym(dx_col)) == 1 | NT == 1) %>%
        mutate(Group = factor(ifelse(NT == 1, "Neurotypical", dx_col))) %>%
        mutate(Group = factor(ifelse(NT == 1, "Neurotypical", dx_col))) %>%
        mutate(Group = ordered(Group, levels = c("Neurotypical", dx_col))) %>%
        mutate(IQ = as.numeric(IQ)) %>%
        mutate(Study_Site = factor(Study_Site)) 
    }
  }
  if(!is.null(Female)){
    if(is.null(Male)){
      dx_nt = data %>%
        mutate(Sex = factor(ifelse(Sex == 1, "Female", "Male"))) %>% 
        filter(Sex == "Female") %>% 
        filter(UQ(sym(dx_col)) == 1 | NT == 1) %>%
        mutate(Group = factor(ifelse(NT == 1, "Neurotypical", dx_col))) %>%
        mutate(Group = factor(ifelse(NT == 1, "Neurotypical", dx_col))) %>%
        mutate(Group = ordered(Group, levels = c("Neurotypical", dx_col))) %>% 
        mutate(IQ = as.numeric(IQ)) %>%  
        mutate(Study_Site = factor(Study_Site)) 
    }
  }
  assign("dx_nt",dx_nt,envir = .GlobalEnv)
}

#_________________________________________________________________________________________________________________________________________________


diff_calc = function (model, view, comp, cond = NULL, se = 1.96, sim.ci = FALSE, 
                      n.grid = 1000, add = FALSE, rm.ranef = NULL, mark.diff = TRUE, 
                      col.diff = "red", col = "black", eegAxis = FALSE, 
                      transform.view = NULL, print.summary = getOption("itsadug_print"), 
                      plot = TRUE, main = NULL, ylab = NULL, xlab = NULL, xlim = NULL, 
                      ylim = NULL, hide.label = FALSE, ...) 
{
  if (sim.ci == TRUE) {
    n.grid = max(n.grid, 200)
  }
  dat = model$model
  xvar <- NULL
  by_predictor <- NULL
  if (length(view) > 1) {
    warning("Only first element of 'view' is being used. Use plot_diff2 for plotting difference surfaces.")
  }
  else {
    xvar <- view[1]
    if (xvar %in% names(cond)) {
      warning(sprintf("Predictor %s specified in view and cond. Values in cond being used, rather than the whole range of %s.", 
                      xvar, xvar))
    }
    else {
      cond[[xvar]] <- seq(min(na.exclude(dat[, xvar])), 
                          max(na.exclude(dat[, xvar])), length = n.grid)
    }
  }
  if (!is.null(xlim)) {
    if (length(xlim) != 2) {
      warning("Invalid xlim values specified. Argument xlim is being ignored.")
    }
    else {
      cond[[xvar]] <- seq(xlim[1], xlim[2], length = n.grid)
    }
  }
  newd <- c()
  newd <- get_difference(model, comp = comp, cond = cond, se = ifelse(se > 
                                                                        0, TRUE, FALSE), f = ifelse(se > 0, se, 1.96), sim.ci = sim.ci, 
                         print.summary = print.summary, rm.ranef = rm.ranef)
  errormessage <- function() {
    return("Error: the function specified in transformation.view cannot be applied to x-values, because infinite or missing values are not allowed.")
  }
  if (!is.null(transform.view)) {
    tryCatch(newd[, xvar] <- sapply(newd[, xvar], transform.view), 
             error = function(x) {
             }, warning = function(x) {
             })
    if (any(is.infinite(newd[, xvar])) | any(is.nan(newd[, 
                                                         xvar])) | any(is.na(newd[, xvar]))) {
      stop(errormessage())
    }
    if (print.summary) {
      cat("\t* Note: x-values are transformed.\n")
    }
  }
  out <- data.frame(est = newd$difference, x = newd[, xvar])
  names(out)[2] <- xvar
  if (se > 0) {
    out$CI <- newd$CI
    out$f <- se
    if (sim.ci == TRUE) {
      out$sim.CI <- newd$sim.CI
    }
  }
  out$comp = list2str(names(comp), comp)
  if (is.null(main)) {
    levels1 <- paste(sapply(comp, function(x) x[1]), collapse = ".")
    levels2 <- paste(sapply(comp, function(x) x[2]), collapse = ".")
    main = sprintf("Difference %s - %s", levels1, levels2)
  }
  if (is.null(ylab)) {
    ylab = sprintf("Est. difference in %s", as.character(model$formula[[2]]))
  }
  if (is.null(xlab)) {
    xlab = xvar
  }
  if (is.null(ylim)) {
    ylim <- range(newd$difference)
    if (se > 0) {
      ylim <- with(newd, range(c(difference + CI, difference - 
                                   CI)))
    }
  }
  if (is.null(xlim)) {
    xlim <- range(newd[, xvar])
  }
  par = list(...)
  if (!"h0" %in% names(par)) {
    par[["h0"]] <- 0
  }
  if (!"shade" %in% names(par)) {
    par[["shade"]] <- TRUE
  }
  area.par <- c("shade", "type", "pch", "lty", 
                "bg", "cex", "lwd", "lend", "ljoin", 
                "lmitre", "ci.lty", "ci.lwd", "border", 
                "alpha", "density", "angle")
  line.par <- c("type", "pch", "lty", "bg", 
                "cex", "lwd", "lend", "ljoin", 
                "lmitre")
  area.args <- list2str(area.par, par)
  line.args <- list2str(line.par, par)
  plot.args <- list2str(x = names(par)[!names(par) %in% c(line.par, 
                                                          area.par)], par)
  if (plot == TRUE) {
    if (add == FALSE) {
      eval(parse(text = sprintf("emptyPlot(xlim, ylim, \n\t\t\t\tmain=main, xlab=xlab, ylab=ylab, \n\t\t\t\teegAxis=eegAxis, %s)", 
                                plot.args)))
      if (hide.label == FALSE) {
        addlabel = "difference"
        if (!is.null(rm.ranef)) {
          if (rm.ranef != FALSE) {
            addlabel = paste(addlabel, "excl. random", 
                             sep = ", ")
          }
        }
        if (sim.ci == TRUE) {
          addlabel = paste(addlabel, "simult.CI", 
                           sep = ", ")
        }
        mtext(addlabel, side = 4, line = 0, adj = 0, 
              cex = 0.75, col = "gray35", xpd = TRUE)
      }
    }
    if (se > 0) {
      if (sim.ci == TRUE) {
        eval(parse(text = sprintf("plot_error(newd[,xvar], newd$difference, newd$sim.CI, col=col, %s)", 
                                  area.args)))
      }
      else {
        eval(parse(text = sprintf("plot_error(newd[,xvar], newd$difference, newd$CI, col=col, %s)", 
                                  area.args)))
      }
    }
    else {
      if (line.args == "") {
        lines(newd[, xvar], newd$difference, col = col)
      }
      else {
        eval(parse(text = sprintf("lines(newd[,xvar], newd$difference, col=col, %s)", 
                                  line.args)))
      }
    }
    if (mark.diff == TRUE) {
      diff <- find_difference(newd$difference, newd$CI, 
                              newd[, xvar])
      if (sim.ci == TRUE) {
        diff <- find_difference(newd$difference, newd$sim.CI, 
                                newd[, xvar])
      }
      if (length(diff$start) > 0) {
        addInterval(pos = getFigCoords("p")[3], 
                    diff$start, diff$end, col = col.diff, lwd = 2 * 
                      par()$lwd, length = 0, xpd = TRUE)
        abline(v = c(diff$start, diff$end), lty = 3, 
               col = col.diff)
      }
    }
    if (print.summary) {
      if (length(diff$start) > 0) {
        tmp <- c(sprintf("%s window(s) of significant difference(s):", 
                         xvar), sprintf("\t%f - %f", diff$start, 
                                        diff$end))
      }
      else {
        tmp <- "Difference is not significant."
      }
      cat("\n")
      cat(paste(tmp, collapse = "\n"))
      cat("\n")
    }
    invisible(out)
  }

  
  assign("diff",diff,envir = .GlobalEnv)
  assign("out",out,envir = .GlobalEnv)
}

#_________________________________________________________________________________________________________________________________________________

gammer = function(data, formula, xvar, title, xlabtitle, color1, color2, IQ=NULL, Sex=NULL, Income=NULL, Site=NULL, main=NULL, main_formula){
  model = gam(formula, data = data, method = "REML")
  diff_calc(model, view=xvar, comp=list(Group = paste(levels(data$Group))))
  if(is.null(main)){
    vals = data.frame(summary(model)$s.table)[2,][,c('F','p.value')]
    F.val = as.character(round(vals$F,3))
    p.val = as.character(round(vals$p.value,3))
    sig = "s(Age):Group"
    output = paste0(sig, sep = ", ", "F = ", F.val, sep = ", ", sep = "p = ", p.val)
    assign("output",out,envir = .GlobalEnv)
    }
  if(!is.null(main)){
    main_model = gam(main_formula, method = "REML", data = data)
    vals = data.frame(summary(main_model)$p.table)[2,][,c('t.value','Pr...t..')]
    t.val = as.character(round(vals$t.value,3))
    p.val = as.character(round(vals$Pr...t..,3))
    sig = "Group"
    output = paste0(sig, sep = ", ", "t = ", t.val, sep = ", ", sep = "p = ", p.val)
    assign("output",out,envir = .GlobalEnv)
    }
   if(!is.null(IQ)){
    if(!is.null(Sex)){
      if(is.null(Income)){
        if(is.null(Site)){
        pdat <- expand.grid(Age = seq(min(data$Age), max(data$Age), length = 100),
                            Group = levels(data$Group),
                            IQ = seq(min(data$IQ), max(data$IQ), length = 100), 
                            Sex = levels(data$Sex), 
                            Study_Site = levels(data$Study_Site))
        assign("pdat",pdat,envir = .GlobalEnv)
        }
      }
    }
  } 
  if(!is.null(IQ)){
    if(!is.null(Sex)){
      if(!is.null(Income)){
        if(is.null(Site)){
        pdat <- expand.grid(Age = seq(min(data$Age), max(data$Age), length = 100),
                            Group = levels(data$Group),
                            IQ = seq(min(data$IQ), max(data$IQ), length = 100),
                            Sex = levels(data$Sex), 
                            Income = levels(data$Income), 
                            Study_Site = levels(data$Study_Site))
        assign("pdat",pdat,envir = .GlobalEnv)
        }
      }
    }
  }
  if(is.null(IQ)){
    if(!is.null(Sex)){
      if(!is.null(Income)){
        if(is.null(Site)){
        pdat <- expand.grid(Age = seq(min(data$Age), max(data$Age), length = 100),
                            Group = levels(data$Group),
                            Sex = levels(data$Sex), 
                            Income = levels(data$Income), 
                            Study_Site = levels(data$Study_Site))
        assign("pdat",pdat,envir = .GlobalEnv)
        }
      }
    }
  }
  if(is.null(IQ)){
    if(!is.null(Sex)){
      if(is.null(Income)){
        if(is.null(Site)){
        pdat <- expand.grid(Age = seq(min(data$Age), max(data$Age), length = 100),
                            Group = levels(data$Group),
                            Sex = levels(data$Sex), 
                            Study_Site = levels(data$Study_Site))
        assign("pdat",pdat,envir = .GlobalEnv)
        }
      }
    }
  }
  if(!is.null(IQ)){
    if(is.null(Sex)){
      if(!is.null(Income)){
        if(is.null(Site)){
        pdat <- expand.grid(Age = seq(min(data$Age), max(data$Age), length = 100),
                            Group = levels(data$Group),
                            IQ = seq(min(data$IQ), max(data$IQ), length = 100),
                            Income = levels(data$Income), 
                            Study_Site = levels(data$Study_Site))
        assign("pdat",pdat,envir = .GlobalEnv)
        }
      }
    }
  }
  if(!is.null(IQ)){
    if(!is.null(Sex)){
      if(!is.null(Income)){
        if(!is.null(Site)){
          pdat <- expand.grid(Age = seq(min(data$Age), max(data$Age), length = 100),
                            Group = levels(data$Group),
                            Sex = levels(data$Sex), 
                            IQ = seq(min(data$IQ), max(data$IQ), length = 100),
                            Income = levels(data$Income))
        assign("pdat",pdat,envir = .GlobalEnv)
        }
      }
    }
  }

  plotit(model, view=xvar, plot_all="Group", rug=FALSE)
  
  if(class(diff) == "NULL"){
    model_plot = "No Differences"
  }
  if(class(diff) != "NULL") {
    if(names(model$model[1]) == "Factor1"){
      model_plot = newd %>%
        ggplot(aes(x = Age, y = fit, color = Group)) + 
        geom_point(data= data, mapping = aes(y= Factor1, color = Group), alpha = .3, size = 2.5, show.legend = F) + 
        geom_point(data= data, mapping = aes(y= Factor1, color = Group), alpha = .1, size = 2.45, show.legend = F) + 
        geom_point(data= data, mapping = aes(y= Factor1, color = Group), alpha = .01, size = 2.3, show.legend = F) + 
        geom_ribbon(aes(ymin = ll, ymax = ul, fill = Group), alpha = 0.15, linetype = 0, show.legend = F) +
        geom_line(aes(x = Age, y = fit,group = Group),size = 2.3) + 
        scale_fill_manual(values = c(paste(color2),paste(color1))) +
        scale_color_manual(values = c(paste(color1),paste(color2))) +
        labs(y = "Delay Discounting Factor 1", x = xlabtitle, title = title, subtitle = output) + 
        scale_x_continuous(breaks=seq(5, 18, 2)) + 
        scale_y_continuous(breaks=seq(-4, 8, 2)) +
        annotate("rect",xmin=diff$start,xmax=diff$end,ymin=-Inf,ymax=Inf, alpha=0.1, fill="red") +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(), 
          panel.background = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(), 
          axis.line = element_line(size=1, color = "black"),
          axis.text.x = element_text(color="black", size=15, vjust=.5,),
          axis.text.y = element_text(color="black", size=15),
          axis.ticks.length=unit(0.1,"cm"),
          axis.ticks.x=element_line(size=1),
          axis.ticks.y=element_line(size=1),
          axis.title.x = element_text(size=16),
          axis.title.y = element_text(size=16),
          title = element_text(size=14),
          legend.text = element_text(size = 12, face = "bold"),
          legend.background = element_rect(fill="transparent", colour ="transparent"),
          legend.key = element_rect(fill = "transparent", colour = "transparent"),
          legend.position = c(1.0, 1.2),
          legend.justification = c("right", "top"),
          legend.box.just = "right",
          legend.margin = margin(6, 6, 6, 6),
          legend.title = element_blank(),
          plot.title = element_text(color = "black", size = 16, face = "bold"),
          plot.subtitle = element_text(color = "black")
        )
    } else if (names(model$model[1]) == "Factor2"){
      model_plot = newd %>%
        ggplot(aes(x = Age, y = fit, color = Group)) + 
        geom_point(data= data, mapping = aes(y= Factor2, color = Group), alpha = .3, size = 2.5, show.legend = F) + 
        geom_point(data= data, mapping = aes(y= Factor2, color = Group), alpha = .1, size = 2.45, show.legend = F) + 
        geom_point(data= data, mapping = aes(y= Factor2, color = Group), alpha = .01, size = 2.3, show.legend = F) + 
        geom_ribbon(aes(ymin = ll, ymax = ul, fill = Group), alpha = 0.15, linetype = 0, show.legend = F) +
        geom_line(aes(x = Age, y = fit, group = Group),size = 2.3) + 
        scale_fill_manual(values = c(paste(color2),paste(color1))) +
        scale_color_manual(values = c(paste(color1),paste(color2))) +
        labs(y = "Delay Discounting Factor 2", x = xlabtitle, title = title, subtitle = output) + 
        scale_x_continuous(breaks=seq(5, 18, 2)) + 
        scale_y_continuous(breaks=seq(-6, 8, 2)) +
        annotate("rect",xmin=diff$start,xmax=diff$end,ymin=-Inf,ymax=Inf, alpha=0.1, fill="red") +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(), 
          panel.background = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(), 
          axis.line = element_line(size=1, color = "black"),
          axis.text.x = element_text(color="black", size=15, vjust=.5,),
          axis.text.y = element_text(color="black", size=15),
          axis.ticks.length=unit(0.1,"cm"),
          axis.ticks.x=element_line(size=1),
          axis.ticks.y=element_line(size=1),
          axis.title.x = element_text(size=16),
          axis.title.y = element_text(size=16),
          title = element_text(size=14),
          legend.text = element_text(size = 12, face = "bold",),
          legend.position = c(1.0, 1.2),
          legend.justification = c("right", "top"),
          legend.box.just = "right",
          legend.margin = margin(6, 6, 6, 6),
          legend.title = element_blank(),
          legend.background = element_rect(fill="transparent", colour ="transparent"),
          legend.key = element_rect(fill = "transparent", colour = "transparent"),
          plot.title = element_text(color = "black", size = 16, face = "bold"),
          plot.subtitle = element_text(color = "black")
        )
    } else {
      model_plot = newd %>%
        ggplot(aes(x = Age, y = fit, color = Group)) + 
        geom_point(data= data, mapping = aes(y= Factor3, color = Group), alpha = .3, size = 2.5, show.legend = F) + 
        geom_point(data= data, mapping = aes(y= Factor3, color = Group), alpha = .1, size = 2.45, show.legend = F) + 
        geom_point(data= data, mapping = aes(y= Factor3, color = Group), alpha = .01, size = 2.3, show.legend = F) + 
        geom_ribbon(aes(ymin = ll, ymax = ul, fill = Group), alpha = 0.15, linetype = 0, show.legend = F) +
        geom_line(aes(x = Age, y = fit, group = Group),size = 2.3) + 
        scale_fill_manual(values = c(paste(color2),paste(color1))) +
        scale_color_manual(values = c(paste(color1),paste(color2))) +
        labs(y = "Delay Discounting Factor 3", x = xlabtitle, title = title, subtitle = output) + 
        scale_x_continuous(breaks=seq(5, 18, 2)) + 
        scale_y_continuous(breaks=seq(-4, 8, 2)) +
        annotate("rect",xmin=diff$start,xmax=diff$end,ymin=-Inf,ymax=Inf, alpha=0.1, fill="red") +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(), 
          panel.background = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(), 
          axis.line = element_line(size=1, color = "black"),
          axis.text.x = element_text(color="black", size=15, vjust=.5,),
          axis.text.y = element_text(color="black", size=15),
          axis.ticks.length=unit(0.1,"cm"),
          axis.ticks.x=element_line(size=1),
          axis.ticks.y=element_line(size=1),
          axis.title.x = element_text(size=16),
          axis.title.y = element_text(size=16),
          title = element_text(size=14),
          legend.text = element_text(size = 12, face = "bold",),
          legend.position = c(1.0, 1.2),
          legend.justification = c("right", "top"),
          legend.box.just = "right",
          legend.margin = margin(6, 6, 6, 6),
          legend.title = element_blank(),
          legend.key = element_rect(fill = "transparent", colour = "transparent"),
          legend.background = element_rect(fill="transparent", colour ="transparent"),
          plot.title = element_text(color = "black", size = 16, face = "bold"),
          plot.subtitle = element_text(color = "black")
        )
    }
  }
  output = list(vals, model_plot)
  return(output)
}

#_________________________________________________________________________________________________________________________________________________

plotit = function (x, view = NULL, cond = list(), plot_all = NULL, rm.ranef = NULL, 
                   n.grid = 1000, rug = NULL, add = FALSE, se = 1.96, sim.ci = FALSE, 
                   shade = TRUE, eegAxis = FALSE, col = NULL, lwd = NULL, lty = NULL, 
                   print.summary = getOption("itsadug_print"), main = NULL, 
                   xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL, h0 = 0, 
                   v0 = NULL, transform = NULL, transform.view = NULL, legend_plot_all = NULL, 
                   hide.label = FALSE, ...) 
{
  if (sim.ci == TRUE) {
    n.grid = max(n.grid, 200)
    assign("n.grid",n.grid,envir = .GlobalEnv)
  }
  if (is.null(rug)) {
    if (nrow(x$model) < 10000) {
      rug = TRUE
    }
    else {
      rug = FALSE
    }
  }
  v.names <- names(x$var.summary)
  assign("v.names",v.names,envir = .GlobalEnv)
  cond.data <- list()
  if (is.null(view)) {
    stop("Specify one view predictors for the x-axis.")
  }
  else {
    if (length(view) > 1) {
      warning("Only first element of view will be used.")
      view <- view[1]
    }
    if (sum(view %in% v.names) != 1) {
      stop(paste(c("View variable must be one of", 
                   v.names), collapse = ", "))
    }
    if (!inherits(x$var.summary[[view[1]]], c("numeric", 
                                              "integer"))) {
      stop("Don't know what to do with parametric terms that are not simple numeric variables.")
    }
  }
  if (!is.null(cond)) {
    cn <- names(cond)
    test <- sapply(cn, function(y) {
      if (!y %in% v.names) {
        stop(paste(c("Cond predictor must be one of", 
                     v.names), collapse = ", "))
      }
      else if (inherits(x$var.summary[[y]], "factor")) {
        if (!all(cond[[y]] %in% levels(x$var.summary[[y]]))) {
          stop(sprintf("Not all specified values for %s are found in the model data.", 
                       y))
        }
      }
      else {
        TRUE
      }
    })
    cond.data <- cond
  }
  if (!is.null(plot_all)) {
    if (!is.vector(plot_all)) {
      stop("Argument plot_all should be a vector with predictor names.")
    }
    else {
      if (any(plot_all %in% names(cond))) {
        warning(sprintf("%s in cond and in plot_all. Not all levels are being plotted.", 
                        paste(plot_all[plot_all %in% names(cond)], 
                              collapse = ", ")))
      }
      if (any(!plot_all %in% v.names)) {
        warning(sprintf("%s (specified in plot_all) is not a model predictor. Will be ignored.", 
                        paste(plot_all[!plot_all %in% v.names], collapse = ", ")))
        plot_all <- plot_all[plot_all %in% v.names]
      }
      if (length(plot_all) > 0) {
        for (i in plot_all) {
          if (!i %in% names(cond)) {
            if (!inherits(x$model[, i], c("factor"))) {
              cond[[i]] <- unique(x$model[, i])
              warning(sprintf("Predictor %s is not a factor.", 
                              i))
            }
            else {
              cond[[i]] <- unique(as.character(x$model[, 
                                                       i]))
            }
          }
        }
      }
      else {
        plot_all <- NULL
      }
    }
  }
  else {
    if (is.null(col)) {
      col = "black"
    }
  }
  m1 <- seq(min(x$var.summary[[view[1]]], na.rm = TRUE), max(x$var.summary[[view[1]]], 
                                                             na.rm = TRUE), length = n.grid)
  assign("m1",m1,envir = .GlobalEnv)
  if (!is.null(xlim)) {
    m1 <- seq(xlim[1], xlim[2], length = n.grid)
  }
  cond[[view[1]]] <- m1
  newd <- get_predictions(x, cond = cond, se = ifelse(se > 
                                                        0, TRUE, FALSE), f = ifelse(se > 0, se, 1.96), sim.ci = sim.ci, 
                          rm.ranef = rm.ranef, print.summary = print.summary)
  if (se > 0) {
    if (("ul" %in% names(newd)) | ("ll" %in% 
                                   names(newd))) {
      warning("Column names 'ul' and / or 'll' are used for plotting confidence intervals. Predictors with the same names may be overwitten, which may cause unexpected effects. Please rename predictors by using capitalization to avoid problems with plotting.")
    }
    newd$ul <- with(newd, fit + CI)
    newd$ll <- with(newd, fit - CI)
    if (sim.ci == TRUE) {
      newd$ul <- with(newd, fit + sim.CI)
      newd$ll <- with(newd, fit - sim.CI)
    }
    if (!is.null(transform)) {
      newd$ul <- sapply(newd$ul, transform)
      newd$ll <- sapply(newd$ll, transform)
    }
  }
  if (!is.null(transform)) {
    newd$fit <- sapply(newd$fit, transform)
  }
  errormessage <- function() {
    return("Error: the function specified in transformation.view cannot be applied to x-values, because infinite or missing values are not allowed.")
  }
  if (!is.null(transform.view)) {
    newd[, view[1]] <- sapply(newd[, view[1]], transform.view)
    if (any(is.infinite(newd[, view[1]])) | any(is.nan(newd[, 
                                                            view[1]])) | any(is.na(newd[, view[1]]))) {
      stop(errormessage())
    }
    if (print.summary) {
      if (rug == TRUE) {
        rug = FALSE
        cat("\t* Note: X-values are transformed, no rug are printed.\n")
      }
      else {
        cat("\t* Note: X-values are transformed.\n")
      }
    }
  }
  if (is.null(main)) {
    main <- ""
  }
  if (is.null(xlab)) {
    xlab <- view[1]
  }
  if (is.null(ylab)) {
    ylab <- names(x$model)[!names(x$model) %in% v.names]
  }
  if (is.null(ylim)) {
    if (se > 0) {
      ylim <- range(c(newd$ul, newd$ll))
    }
    else {
      ylim <- range(newd$fit)
    }
  }
  if (is.null(col) & is.null(plot_all)) {
    col = "black"
  }
  if (is.null(lwd) & is.null(plot_all)) {
    lwd = 1
  }
  if (is.null(lty) & is.null(plot_all)) {
    lty = 1
  }
  if (add == FALSE) {
    emptyPlot(range(newd[, view[1]]), ylim, main = main, 
              xlab = xlab, ylab = ylab, h0 = h0, v0 = v0, eegAxis = eegAxis, 
              ...)
    if (hide.label == FALSE) {
      addlabel = "fitted values"
      if (!is.null(rm.ranef)) {
        if (rm.ranef != FALSE) {
          addlabel = paste(addlabel, "excl. random", 
                           sep = ", ")
        }
      }
      if (sim.ci == TRUE) {
        addlabel = paste(addlabel, "simult.CI", 
                         sep = ", ")
      }
      mtext(addlabel, side = 4, line = 0, adj = 0, cex = 0.75, 
            col = "gray35", xpd = TRUE)
      if (!is.null(transform)) {
        mtext("transformed", side = 4, line = 0.75, 
              adj = 0, cex = 0.75, col = "gray35", 
              xpd = TRUE)
      }
    }
  }
  if (rug == TRUE) {
    if (!is.null(plot_all)) {
      rug_model(x, view = view[1])
    }
    else {
      rug.cond = cond
      for (i in v.names) {
        if (!i %in% names(cond)) {
          rug.cond[[i]] = unique(newd[, i])
        }
      }
      rug_model(x, view = view[1], cond = rug.cond, rm.ranef = rm.ranef)
    }
  }
  if (!is.null(plot_all)) {
    alllevels <- c()
    plotlevels <- c()
    cols = "black"
    ltys = 1
    lwds = 1
    tmpname <- plot_all
    newd <- droplevels(newd)
    if (length(plot_all) > 1) {
      tmpname <- sub("/", "", tempfile(pattern = "event", 
                                       tmpdir = "plotsmooth", fileext = ""), 
                     fixed = TRUE)
      newd[, tmpname] <- interaction(newd[, plot_all])
    }
    alllevels <- length(levels(newd[, tmpname]))
    plotlevels <- levels(newd[, tmpname])
    if (is.null(plotlevels)) {
      alllevels <- length(unique(newd[, tmpname]))
      plotlevels <- unique(newd[, tmpname])
    }
    cols = rainbow(alllevels)
    ltys = rep(1, alllevels)
    lwds = rep(1, alllevels)
    if (!is.null(col)) {
      if (length(col) < alllevels) {
        cols = rep(col, ceiling(alllevels/length(col)))
      }
      else {
        cols = col
      }
    }
    if (!is.null(lty)) {
      if (length(lty) < alllevels) {
        ltys = rep(lty, ceiling(alllevels/length(lty)))
      }
      else {
        ltys = lty
      }
    }
    if (!is.null(lwd)) {
      if (length(lwd) < alllevels) {
        lwds = rep(lwd, ceiling(alllevels/length(lwd)))
      }
      else {
        lwds = lwd
      }
    }
    cnt <- 1
    for (i in plotlevels) {
      if (se > 0) {
        plot_error(newd[newd[, tmpname] == i, view[1]], 
                   newd[newd[, tmpname] == i, ]$fit, newd[newd[, 
                                                               tmpname] == i, ]$ul, se.fit2 = newd[newd[, 
                                                                                                        tmpname] == i, ]$ll, shade = shade, f = 1, 
                   col = cols[cnt], lwd = lwds[cnt], lty = ltys[cnt], 
                   ...)
      }
      else {
        lines(newd[newd[, tmpname] == i, view[1]], newd[newd[, 
                                                             tmpname] == i, ]$fit, col = cols[cnt], lwd = lwds[cnt], 
              lty = ltys[cnt], ...)
      }
      cnt <- cnt + 1
    }
    if (!tmpname %in% plot_all) {
      newd[, tmpname] <- NULL
    }
    if (is.null(legend_plot_all)) {
      gfc <- getFigCoords()
      legend(gfc[2], gfc[4], legend = plotlevels, text.col = cols, 
             text.font = 2, xjust = 1, yjust = 1, bty = "n", 
             xpd = TRUE)
    }
    else if (is.logical(legend_plot_all)) {
      if (legend_plot_all == TRUE) {
        gfc <- getFigCoords()
        legend(gfc[2], gfc[4], legend = plotlevels, text.col = cols, 
               text.font = 2, xjust = 1, yjust = 1, bty = "n", 
               xpd = TRUE)
      }
    }
    else {
      legend(legend_plot_all, legend = plotlevels, text.col = cols, 
             text.font = 2, bty = "n", xpd = TRUE)
    }
  }
  else {
    if (se > 0) {
      plot_error(as.vector(newd[, view[1]]), newd$fit, 
                 newd$ul, se.fit2 = newd$ll, shade = shade, f = 1, 
                 col = col, lwd = lwd, lty = lty, ...)
    }
    else {
      lines(newd[, view[1]], newd$fit, col = col, lwd = lwd, 
            lty = lty, ...)
    }
  }
  invisible(list(fv = newd, rm.ranef = rm.ranef, transform = transform))
  
  assign("newd",newd,envir = .GlobalEnv)
  assign("cond",cond,envir = .GlobalEnv)
}

#_________________________________________________________________________________________________________________________________________________

smooth_boot = function(data, formula){
  boots = 1000
  boot_models = replicate(
    boots,{
      sp <- split(data, list(data$Group, data$Sex))
      samples <- lapply(sp, function(x) x[sample(1:nrow(x), nrow(sp[["Neurotypical.Male"]]), F),])
      out <- do.call(rbind, samples)
      boot = out[sample.int(nrow(out), replace = T), ]
      model = gam(formula, method = "REML", data = boot)
      bootp = data.frame(summary(model)$s.table)[2,][,c('F','p.value')]
    }
  )
  boot_frame = data.frame(matrix(unlist(boot_models), nrow=boots, byrow=TRUE)) %>% 
    rename(f.stat = X1) %>% rename(p.value = X2) %>% 
    summarise(f.stat = mean(f.stat), pval = mean(p.value))
  return(boot_frame)
}

#_________________________________________________________________________________________________________________________________________________

main_boot = function(data, formula){
  boots = 1000
  boot_models = replicate(
    boots,{
      sp <- split(data, list(data$Group, data$Sex))
      samples <- lapply(sp, function(x) x[sample(1:nrow(x), nrow(sp[["Neurotypical.Male"]]), F),])
      out <- do.call(rbind, samples)
      boot = out[sample.int(nrow(out), replace = T), ]
      model = gam(formula, method = "REML", data = boot)
      bootp = data.frame(summary(model)$p.table)[2,][,c('t.value','Pr...t..')]
    }
  )
  boot_frame = data.frame(matrix(unlist(boot_models), nrow=boots, byrow=TRUE)) %>% 
    rename(t.value = X1) %>% rename(p.value = X2) %>% 
    summarise(tval = mean(t.value), pval = mean(p.value))
  return(boot_frame)
}

#_________________________________________________________________________________________________________________________________________________

lastValue <- function(x) tail(x[!is.na(x)], 1)
firstValue <- function(x) head(x[!is.na(x)], 1)

#_________________________________________________________________________________________________________________________________________________

jam = function(data, formula, main=NULL){
    if(!is.null(main)){ 
    model = gam(formula, data = data, method = "REML")
    ptab = data.frame(summary(model)$p.table)[3:4] %>% rename(test_stat = t.value) %>% rename(pval = Pr...t..)
    stab = data.frame(summary(model)$s.table)[3:4] %>% rename(test_stat = `F`) %>% rename(pval = p.value); stab = stab[-nrow(stab),]
    bind = rbind(ptab, stab)[-1,]; bind = bind %>% mutate(Mod_names = rownames(bind))
    for(i in firstValue(bind$pval)){
      if(i > 0.05){
        bind = bind %>% mutate(pval = 100, test_stat = 100)
      } else {
        bind = bind
      }
    } 
  }
  else{ 
    model = gam(formula, data = data, method = "REML")
    ptab = data.frame(summary(model)$p.table)[3:4] %>% rename(test_stat = t.value) %>% rename(pval = Pr...t..)
    stab = data.frame(summary(model)$s.table)[3:4] %>% rename(test_stat = `F`) %>% rename(pval = p.value); stab = stab[-nrow(stab),]
    bind = rbind(ptab, stab)[-1,]; bind = bind %>% mutate(Mod_names = rownames(bind))
    for(i in lastValue(bind$pval)){
      if(i > 0.05){
        bind = bind %>% mutate(pval = 100, test_stat = 100)
      } else {
        bind = bind
      }
    } 
  }
  return(bind)
}

#_________________________________________________________________________________________________________________________________________________

extract = function(list, posthoc=NULL){
  
  sig_vals = data.frame("pval" = unlist(list)) %>% mutate(Names = rownames(.)) 
  sig_pvals = sig_vals %>% filter(str_detect(Names, "pval")) 
  sig_teststat =  sig_vals %>% filter(str_detect(Names, "test")) %>% select(-Names) %>% rename(teststat = pval)
  sig_mod = sig_vals %>%filter(str_detect(Names, "Mod")) %>% select(-Names)  %>% rename(Mods = pval)
  sig_vals = cbind(sig_pvals, sig_mod, sig_teststat) %>% select(Names, Mods, teststat, pval) %>% filter(pval != 100) %>% 
    mutate(pval = round(as.numeric(as.character(pval)), 3), teststat = round(as.numeric(as.character(teststat)),3))  
  sig_vals_t = data.frame(t(sig_vals))
  colnames(sig_vals_t) = sig_vals$Mods
  features <- c(sprintf("_%02d", seq(1,length(sig_vals_t))),"label")[-1]
  colnames(sig_vals_t) <- paste(colnames(sig_vals_t), features, sep = "_")
  sig_vals_t = sig_vals_t %>% select(starts_with("Group"), starts_with("s(Age")) 
  sig_vals = t(sig_vals_t) %>% data.frame(.) %>% mutate(Names = as.character(Names)) %>% arrange(Names)
  main = sig_vals %>% filter(str_detect(Names, "main")) %>% rename("t" = teststat, Term = Mods, Group = Names, p = pval)
  full = sig_vals %>% filter(str_detect(Names, "full_")) %>% rename("F" = teststat, Term = Mods, Group = Names, p = pval)
  full_sex = sig_vals %>% filter(str_detect(Names, "sex")) %>% rename("F" = teststat, Term = Mods, Group = Names, p = pval)
  if(!is.null(posthoc)){ 
    full_sex$Group = gsub('fullsex_model.', '', full_sex$Group); full_sex$Group = gsub('.pval1', '', full_sex$Group); full_sex$Term = gsub('.L', '', full_sex$Term)
    full$Group = gsub('full_model.', '', full$Group); full$Group = gsub('.pval1', '', full$Group); full$Term = gsub('.L', '', full_sex$Term)
    main$Group = gsub('main_model_outout.', '', main$Group); main$Group = gsub('.pval1', '', main$Group); main$Term = gsub('.L', '', main$Term)
    out = list(Main = main, Full = full, Full_Sex = full_sex)
  } else {
    full_sex$Group = gsub('fullsex_model.', '', full_sex$Group); full_sex$Group = gsub('.pval1', '', full_sex$Group); full_sex$Term = gsub('.L', '', full_sex$Term)
    full$Group = gsub('full_model.', '', full$Group); full$Group = gsub('.pval1', '', full$Group); full$Term = gsub('.L', '', full$Term)
    main$Group = gsub('main_model_outout.', '', main$Group); main$Group = gsub('.pval1', '', main$Group); main$Term = gsub('.L', '', main$Term)
    out = list(Main = main, Full = full, Full_Sex = full_sex)
  }
}

#_________________________________________________________________________________________________________________________________________________

pboot <- function(modelobj){
  numsims = 100
  df <- model$model
  thisResp <- as.character(model$terms[[2]])
  f1 <- model$formula
  theseVars <- attr(terms(f1),"term.labels")
  f2 <- reformulate(theseVars[0:(length(theseVars)-1)],response = thisResp)
  g1 <- gam(f1,data = df)
  g2 <- gam(f2,data = df)
  mat1 <- model.matrix(g1)
  mat2 <- model.matrix(g2)
  bblid<-df$Income
  y <- df[,thisResp]
  require(pbkrtest)
  m1 <- lm(y ~ -1 + mat1 + bblid)
  m2 <- lm(y ~ -1 + mat2 + bblid)
  refdist <- PBrefdist(m1, m2, nsim=numsims)#, cl=cl)
  pb <- PBmodcomp(m1, m2, ref = refdist)
  int_pval <- pb$test["PBtest","p.value"]
  if (int_pval < .05/4) {pb$bestmod <- f1} else {pb$bestmod <- f2}
  return(pb)
}

#_________________________________________________________________________________________________________________________________________________

chi = function(data, Subs, vars){
  X = as.matrix(data %>% select(Subs))
  Y = as.matrix(data %>% select(vars))
  SQ = chisq.test(table(X, Y))
  P = SQ$p.value
  return(P)
}

#_________________________________________________________________________________________________________________________________________________

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

#_________________________________________________________________________________________________________________________________________________

anova_full = function(data, dx_col, factor, full_model_output){
  subitdx(data, dx_col)
  full_models = lapply(factor, function(x) {
    lm(substitute(i ~ Group*Age*Sex, list(i = as.name(x))), data = dx_nt)})
  assign("full_models",full_models,envir = .GlobalEnv)
  full_model_output = sapply(full_models, car::Anova,simplify =  F)
  assign("full_model_output",full_model_output, envir = .GlobalEnv)
}

anova_reduc = function(data, dx_col, factor, reduc_model_output){
  subitdx(data, dx_col)
  reduced_models = lapply(factor, function(x) {
    lm(substitute(i ~ Group*Age + Sex, list(i = as.name(x))), data = dx_nt)})
  assign("reduced_models",reduced_models,envir = .GlobalEnv)
  reduced_model_output = sapply(reduced_models, car::Anova,simplify =  F)      
  assign("reduced_model_output",reduced_model_output, envir = .GlobalEnv)
}

anova_main = function(data, dx_col, factor, main_model_output){
  subitdx(data, dx_col)
  main_models = lapply(factor, function(x) {
    lm(substitute(i ~ Group + Age + Sex, list(i = as.name(x))), data = dx_nt)})
  assign("main_models",reduced_models,envir = .GlobalEnv)
  main_model_output = sapply(main_models, car::Anova,simplify =  F) 
  assign("main_model_output",main_model_output, envir = .GlobalEnv)
}


anova_reduc = function(data, dx_col, factor, reduc_model_output){
  subitdx(data, dx_col)
  reduced_models = lapply(factor, function(x) {
    lm(substitute(i ~ Group*Age + Sex, list(i = as.name(x))), data = dx_nt)})
  assign("reduced_models",reduced_models,envir = .GlobalEnv)
  reduced_model_output = sapply(reduced_models, car::Anova,simplify =  F)  
  assign("reduced_model_output",reduced_model_output, envir = .GlobalEnv)
  library(plyr)
  reduced_model_output <- ldply(reduced_model_output, data.frame)
  detach("package:plyr", unload = TRUE)
  detach("package:dplyr", unload = TRUE)
  library(dplyr)
  assign("reduced_model_output",reduced_model_output, envir = .GlobalEnv)
}

#_________________________________________________________________________________________________________________________________________________
