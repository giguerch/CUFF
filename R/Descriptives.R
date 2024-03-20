###     -*- Coding: utf-8 -*-          ###
### Analyste: Charles-Édouard Giguère  ###
###                              .~    ###
###  _\\\\\_                    ~.~    ###
### |  ~ ~  |                 .~~.     ###
### #--O-O--#          ==||  ~~.||     ###
### |   L   |        //  ||_____||     ###
### |  \_/  |        \\  ||     ||     ###
###  \_____/           ==\\_____//     ###
##########################################



### Fonctions descriptives pour variables continues.

cont.desc <- function(x, y = NULL, ..., labels = NULL, data = NULL){
    result <- NULL
    if ("formula" %in% class(x)) {
        if (is.null(data) & !is.null(y))
            data <- y
        return(cont.desc.formula(x, labels = labels, data = data))
    }
    if (is.matrix(x)) {
        x <- as.data.frame(x)
    }
    if (!is.data.frame(x) & !is.vector(x) & !is.numeric(x)) {
        stop("Object x not suitable for this command")
    }
    if (is.data.frame(x)) {
        ## remove factor
        x <- x[,sapply(x, is.numeric), drop = FALSE]
        result <- data.frame(n      = apply(x, 2, \(x) sum(!is.na(x))),
                             n.miss = apply(x, 2, \(x) sum(is.na(x))),
                             mean   = apply(x, 2, \(x) mean(x, na.rm = TRUE)),
                             sd     = apply(x, 2, \(x) sd(x, na.rm = TRUE)),
                             min    = apply(x, 2, \(x) min(x, na.rm = TRUE)),
                             max    = apply(x, 2, \(x) max(x, na.rm = TRUE))
                             )
    }
    if (is.vector(x)) {
        if(!is.numeric(x)){
            stop("Vector x is not numeric")
        }
        result <- data.frame(n      = sum(!is.na(x)),
                             n.miss = sum(is.na(x)),
                             mean   = mean(x, na.rm = TRUE),
                             sd     = sd(x, na.rm = TRUE),
                             min    = min(x, na.rm = TRUE),
                             max    = max(x, na.rm = TRUE))
        if(!is.null(labels))
            dimnames(result)[[1]] <- labels
        else
            dimnames(result)[[1]] <- deparse(substitute(x))
    }
    else if (!is.null(labels) & length(labels) == dim(x)[2])
        dimnames(result)[[1]] <- labels
    else {
        dimnames(result)[[1]] <- names(x)
    }
    class(result) <- c("cont.desc", "data.frame")
    return(result)
}

cont.desc.formula <- function(x, ..., data = NULL){
    if (length(x) != 2)
        stop("Invalid formula see help(cont.desc)")
    else {
        X <- model.frame(x, data = data, na.action = na.pass)
        Y <- NULL
    }
    cont.desc(x = X, y = Y, ...)
}


print.cont.desc <- function(x, ...){
    x_fmt <- as.data.frame(x)
    x_fmt[,1] <- sprintf("%d",   x_fmt[,1])
    x_fmt[,2] <- sprintf("%d",   x_fmt[,2])
    x_fmt[,3] <- sprintf("%.2f", x_fmt[,3])
    x_fmt[,4] <- sprintf("%.2f", x_fmt[,4])
    print(x_fmt)
    invisible(x_fmt)
}


