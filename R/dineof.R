## DNIEOF
## Based on http://menugget.blogspot.com.ar/2012/10/dineof-data-interpolating-empirical.html#more
#http://journals.ametsoc.org/doi/full/10.1175/1520-0426%282003%29020%3C1839%3AECADFF%3E2.0.CO%3B2

.ImputeEOF1 <- function(X, X.na, n.eof, tol = 1e-4, max.iter = 10000) {
    X.rec <- X
    for (i in 1:max.iter) {
        svd <- svd::propack.svd(X.rec, neig = n.eof)
        R <- svd$u%*%diag(svd$d, nrow = n.eof)%*%t(svd$v)
        rmse <- sqrt(mean((R[X.na] - X.rec[X.na])^2))
        # print(rmse)
        X.rec[X.na] <- R[X.na]
        
        if (rmse < tol) {
            X[X.na] <- X.rec[X.na]
            return(X)
        }
    }
    stop(paste0("Algorithm failed to converge after ", max.iter, " iterations"))
}

is.error <- function(x) inherits(x, "try-error")



ImputeEOF <- function(X, max.eof = length(X), min.eof = 1, tol = 1e-4, 
                      max.iter = 10000, validation = NULL) {
    gaps <- which(is.na(X))
    
    if (length(gaps) == 0) return(X)
    
    if (is.null(validation)) {
        validation <- max(30, 0.1*length(X[-gaps]))
    }
    validation <- sample((1:length(X))[!1:length(X) %in% gaps],
                         validation)
    
    eofs <- 0:max.eof
    X.rec <- X
    # First try, imput with mean or something. Rmse is infinite.
    fill <- 0
    X.rec[c(gaps, validation)] <- fill
    rmse <- Inf
    
    for (i in 2:length(eofs)) {
        # After first guess, impute gaps and validation. 
        X.rec.temp <- try(.ImputeEOF1(X.rec, c(gaps, validation), eofs[i], tol = tol, 
                                      max.iter = max.iter))
        # If it doesn't converge, then rmse is infinite
        if (is.error(X.rec.temp)) {
            rmse <- c(rmse, Inf)
        } else {
            X.rec <- X.rec.temp
            rmse <- c(rmse, sqrt(mean((X[validation] - X.rec[validation])^2)))
        }
        print(paste0(eofs[i], " - conv: ", !is.error(X.rec.temp)))
        
        # Break the loop if we are over the minimum eof asked and, either
        # this rmse is greater than the previous one or current rmse is inf.
        if (eofs[i] > min.eof & (rmse[i] > rmse[i - 1] | rmse[i] == Inf)) {
            break
        }
    }
    print(rmse)
    # Select best eof and make proper imputation.
    eof <- eofs[which.min(rmse)]
    X[gaps] <- fill
    X.rec <- .ImputeEOF1(X, gaps, eof, tol = tol, max.iter = max.iter)
    return(X.rec)
}



# Datos de prueba
library(meteoR)

.setna <- function(x, N) {
    L <- length(x)
    if (N > L) N <- L
    x[sample(1:L, size = N)] <- NA
    x
}


data(aao)

X <- aao[, gh := Anomaly(gh), by = .(lat, date)][date == date[1]]
Xg <- copy(X)

Xg[, gh := .setna(gh, .N*0.30)]

X <- as.matrix(dcast(X, lon ~ lat, value.var = "gh"))
dimnames(X)[[1]] <- X[, 1] 
X <- X[, -1]
Xg <- as.matrix(dcast(Xg, lon ~ lat, value.var = "gh")[, -1])

Xa <- ImputeEOF(Xg)
dimnames(Xg) <- dimnames(X)
dimnames(Xa) <- dimnames(X)

cor(c(Xa), c(X))

X <- melt(X)
Xg <- melt(Xg)
Xa <- melt(Xa)

X$type = "original"
Xa$type = "reconstr"

ggplot(mapping = aes(Var1, Var2)) +
    stat_contour_fill(aes(z = value), data = X) +
    geom_contour(aes(z = value), data = Xa)
