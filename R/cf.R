### Extract coefficient (or params) from common stats model lm/glm/lme/lmer/glmer/t-test. 

cf <- function(x, addci = TRUE, pv.style = 1, signif = 2,
               expcf = ifelse("glm" %in% class(x)  &
                              family(x)$family == "binomial",
                              TRUE, FALSE), ...){
  xcf  <- data.frame()
  xclass <- class(x)
  ### linear model.
  if(xclass[1] == "lm" ){
    tabformat <- sprintf("%%.%if",signif)
    xcf <- coef(summary(x))
    xcf <- as.data.frame(xcf)
    names(xcf) <- c("Est.", "s.e.", "t", "P(>|t|)")
    if(length(tabformat) ==  1){
      xcf[,1:3] <- sapply(xcf[,1:3], function(x) sprintf(tabformat, x))

    }
    else{
      for(i in 1:3)
        xcf[,i] <- sprintf(tabformat[i], xcf[,i])
    }
    xcf[,4] <- pv(xcf[,4], style = pv.style)
    if(addci){
      xci <- confint(x)
      xcf[,c("lower 95% ci",
             "upper 95% ci")] <- sapply(xci, function(x) sprintf(tabformat[1], x) )
    }      
  } # general linear model. 
  else if(xclass[1] == "glm"){
    tabformat <- sprintf("%%.%if",signif)
    xcf <- coef(summary(x))
    xcf <- as.data.frame(xcf)
    names(xcf) <- c("Est.", "s.e.", "z", "P(>|z|)")
    if(expcf){
      xcf[["exp(Est.)"]] <- sprintf(tabformat[1], exp(xcf[,"Est."]))
    }     
    if(length(tabformat) ==  1){
      xcf[,1:3] <- sapply(xcf[,1:3], function(x) sprintf(tabformat, x))
      
    }
    else{
      for(i in 1:3)
        xcf[,i] <- sprintf(tabformat[i], xcf[,i])
    }
    xcf[,4] <- pv(xcf[,4], style = pv.style)
    if(addci){
      xci <- exp(suppressMessages(confint(x)))
      xcf[,c("lower 95% ci",
             "upper 95% ci")] <- sapply(xci, function(x) sprintf(tabformat[1], x) )
    }          
  }
  as.data.frame(xcf)
  
}

