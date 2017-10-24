my.predict=function (object, newdata = NULL, type = c("link", "response"), 
    se.fit = FALSE, ...) 
{
    type <- match.arg(type)
    if (!se.fit) {
        if (is.null(newdata)) {
            pred <- switch(type, link = object$linear.predictors - 
                object$offset, response = object$fitted.values)
        }
        else {
            if (!is.list(newdata)) 
                stop("newdata must be a data frame")
            NEWdata <- list(x = object$x, y = object$y)
            namesNEW <- names(NEWdata)
            NEWdata[(namesnew <- names(newdata))] <- newdata
            test <- noNms <- namesnew[!namesnew %in% namesNEW]
            if (length(test) > 0) {
                stop("unknown names in newdata: ", paste(noNms, 
                  collapse = ", "))
            }
            x <- object$x
            y <- object$y
            Z <- object$Z
            offset <- object$offset
            new.x <- sort(unique(c(x, newdata$x)))
            new.y <- sort(unique(c(y, newdata$y)))
            new.m <- length(new.x)
            new.n <- length(new.y)
            new.W1 <- matrix(0, new.m, new.n)
            new.W2 <- matrix(0, new.m, new.n)
            whi.x <- which(new.x %in% x)
            whi.y <- which(new.y %in% y)
            new.W1[, whi.y] <- 10
            new.W2[whi.x, ] <- 1
            whi <- (new.W1 - new.W2) == 9
            new.W <- matrix(0, new.m, new.n)
            new.W[whi] <- 1
            new.Z <- matrix(0, new.m, new.n)
            new.Z[whi] <- Z
            new.offset <- matrix(100, new.m, new.n)
            new.offset[whi] <- offset
            if (min(new.x) >= min(x) & max(new.x) <= max(x)) {
                new.ndx <- object$ndx[1]
                xl <- min(new.x)
                xr <- max(new.x)
                xmax <- xr + 0.01 * (xr - xl)
                xmin <- xl - 0.01 * (xr - xl)
                new.Bx <- MortSmooth_bbase(x = new.x, xl = xmin, 
                  xr = xmax, ndx = new.ndx, deg = object$deg[1])
            }
            else {
                deg <- object$deg[1]
                xl <- min(x)
                xr <- max(x)
                xmax <- xr + 0.01 * (xr - xl)
                xmin <- xl - 0.01 * (xr - xl)
                dx <- (xmax - xmin)/object$ndx[1]
                xl1 <- min(new.x)
                xr1 <- max(new.x)
                minK0 <- xmin - deg:(deg + 100) * dx
                minK <- minK0[which(minK0 <= xl1)[deg]]
                maxK0 <- xmax + deg:(deg + 100) * dx
                maxK <- maxK0[which(maxK0 >= xr1)[deg + 1]]
                knots <- seq(minK, maxK, by = dx)

                PP <- outer(new.x, knots, MortSmooth_tpower, 
                  deg)
                nn <- dim(PP)[2]
                DD <- diff(diag(nn), diff = deg + 1)/(gamma(deg + 
                  1) * dx^deg)
                new.Bx <- (-1)^(deg + 1) * PP %*% t(DD)
            }
            nbx <- ncol(new.Bx)
            if (min(new.y) >= min(y) & max(new.y) <= max(y)) {
                new.ndx <- object$ndx[2]
                yl <- min(new.y)
                yr <- max(new.y)
                ymax <- yr + 0.01 * (yr - yl)
                ymin <- yl - 0.01 * (yr - yl)
                new.By <- MortSmooth_bbase(x = new.y, xl = ymin, 
                  xr = ymax, ndx = new.ndx, deg = object$deg[2])
            }
            else {
                deg <- object$deg[2]
                yl <- min(y)
                yr <- max(y)
                ymax <- yr + 0.01 * (yr - yl)
                ymin <- yl - 0.01 * (yr - yl)
                dx <- (ymax - ymin)/object$ndx[2]
                yl1 <- min(new.y)
                yr1 <- max(new.y)
                minK0 <- ymin - deg:(deg + 100) * dx
                minK <- minK0[which(minK0 <= yl1)[deg]]
                maxK0 <- ymax + deg:(deg + 100) * dx
                maxK <- maxK0[which(maxK0 >= yr1)[deg + 1]]
                knots <- seq(minK, maxK, by = dx)

                PP <- outer(new.y, knots, MortSmooth_tpower, 
                  deg)
                nn <- dim(PP)[2]
                DD <- diff(diag(nn), diff = deg + 1)/(gamma(deg + 
                  1) * dx^deg)
                new.By <- (-1)^(deg + 1) * PP %*% t(DD)
            }
            nby <- ncol(new.By)
            Bx1 <- kronecker(matrix(1, ncol = nbx, nrow = 1), 
                new.Bx)
            Bx2 <- kronecker(new.Bx, matrix(1, ncol = nbx, nrow = 1))
            RTBx <- Bx1 * Bx2
            By1 <- kronecker(matrix(1, ncol = nby, nrow = 1), 
                new.By)
            By2 <- kronecker(new.By, matrix(1, ncol = nby, nrow = 1))
            RTBy <- By1 * By2
            Dx <- diff(diag(nbx), diff = object$pord[1])
            Dy <- diff(diag(nby), diff = object$pord[2])
            Px <- kronecker(diag(nby), t(Dx) %*% Dx)
            Py <- kronecker(t(Dy) %*% Dy, diag(nbx))
            new.Z[is.na(new.Z)] <- 0
            new.xx <- rep(new.x, new.n)
            new.yy <- rep(new.y, each = new.m)
            fit0 <- glm(round(c(new.Z)) ~ new.xx * new.yy + offset(c(new.offset)), 
                family = poisson, weights = c(new.W))
            etaGLM <- matrix(log(fit0$fitted) - c(new.offset), 
                new.m, new.n)
            eta0 <- log((new.Z + 1)) - new.offset
            eta0[new.W == 0] <- etaGLM[new.W == 0]
            BBx <- solve(t(new.Bx) %*% new.Bx + diag(nbx) * 1e-06, 
                t(new.Bx))
            BBy <- solve(t(new.By) %*% new.By + diag(nby) * 1e-06, 
                t(new.By))
            a.init <- MortSmooth_BcoefB(BBx, BBy, eta0)
            new.fit <- Mort2Dsmooth_estimate(x = new.x, y = new.y, 
                Z = new.Z, offset = new.offset, wei = new.W, 
                psi2 = 1, Bx = new.Bx, By = new.By, nbx = nbx, 
                nby = nby, RTBx = RTBx, RTBy = RTBy, lambdas = object$lambdas, 
                Px = Px, Py = Py, a.init = a.init, MON = FALSE, 
                TOL1 = 1e-06, MAX.IT = 50)
            new.eta0 <- matrix(MortSmooth_BcoefB(new.Bx, new.By, 
                new.fit$a), new.m, new.n)
            colnames(new.eta0) <- new.y
            rownames(new.eta0) <- new.x
            if (is.null(newdata$x)) {
                newdata$x <- x
            }
            if (is.null(newdata$y)) {
                newdata$y <- y
            }
            new.eta <- new.eta0[new.x %in% newdata$x, new.y %in% 
                newdata$y]
            new.fitted0 <- exp(new.eta0 + new.offset)
            new.fitted0[!whi] <- NA
            new.fitted <- new.fitted0[new.x %in% newdata$x, new.y %in% 
                newdata$y]
            pred <- switch(type, link = new.eta, response = new.fitted)
        }
    }
    else {
        if (is.null(newdata)) {
            Bx <- object$Bx
            By <- object$By
            nbx <- ncol(Bx)
            nby <- ncol(By)
            Bx1 <- kronecker(matrix(1, ncol = nbx, nrow = 1), 
                Bx)
            Bx2 <- kronecker(Bx, matrix(1, ncol = nbx, nrow = 1))
            RTBx <- Bx1 * Bx2
            By1 <- kronecker(matrix(1, ncol = nby, nrow = 1), 
                By)
            By2 <- kronecker(By, matrix(1, ncol = nby, nrow = 1))
            RTBy <- By1 * By2
            Dx <- diff(diag(nbx), diff = object$pord[1])
            Dy <- diff(diag(nby), diff = object$pord[2])
            Px <- kronecker(diag(nby), t(Dx) %*% Dx)
            Py <- kronecker(t(Dy) %*% Dy, diag(nbx))
            P <- (object$lambdas[1] * Px) + (object$lambdas[2] * 
                Py)
            eta <- MortSmooth_BcoefB(Bx, By, object$coef)
            mu <- exp(object$offset + eta)
            W <- mu
            WW <- object$W * W
            BWB <- MortSmooth_BWB(RTBx, RTBy, nbx, nby, WW)
            BWB.P1 <- solve(BWB + P)
            se <- matrix(Mort2Dsmooth_se(RTBx = RTBx, RTBy = RTBy, 
                nbx = nbx, nby = nby, BWB.P1 = BWB.P1), object$m, 
                object$n, dimnames = list(object$x, object$y))
            check.over <- eval(object$call$overdispersion)
            if (is.null(check.over)) {
                check.over <- FALSE
            }
            if (check.over) {
                se.inflate <- sqrt(object$psi2)
                se <- se * se.inflate
            }
            fit <- switch(type, link = object$linear.predictors - 
                object$offset, response = object$fitted.values)
            se.fit <- switch(type, link = se, response = object$fitted.values * 
                (exp(se) - 1))
        }
        else {
            if (!is.list(newdata)) 
                stop("newdata must be a data frame")
            NEWdata <- list(x = object$x, y = object$y)
            namesNEW <- names(NEWdata)
            NEWdata[(namesnew <- names(newdata))] <- newdata
            test <- noNms <- namesnew[!namesnew %in% namesNEW]
            if (length(test) > 0) {
                stop("unknown names in newdata: ", paste(noNms, 
                  collapse = ", "))
            }
            x <- object$x
            y <- object$y
            Z <- object$Z
            offset <- object$offset
            new.x <- sort(unique(c(x, newdata$x)))
            new.y <- sort(unique(c(y, newdata$y)))
            new.m <- length(new.x)
            new.n <- length(new.y)
            new.W1 <- matrix(0, new.m, new.n)
            new.W2 <- matrix(0, new.m, new.n)
            whi.x <- which(new.x %in% x)
            whi.y <- which(new.y %in% y)
            new.W1[, whi.y] <- 10
            new.W2[whi.x, ] <- 1
            whi <- (new.W1 - new.W2) == 9
            new.W <- matrix(0, new.m, new.n)
            new.W[whi] <- 1
            new.Z <- matrix(0, new.m, new.n)
            new.Z[whi] <- Z
            new.offset <- matrix(100, new.m, new.n)
            new.offset[whi] <- offset
            if (min(new.x) >= min(x) & max(new.x) <= max(x)) {
                new.ndx <- object$ndx[1]
                xl <- min(new.x)
                xr <- max(new.x)
                xmax <- xr + 0.01 * (xr - xl)
                xmin <- xl - 0.01 * (xr - xl)
                new.Bx <- MortSmooth_bbase(x = new.x, xl = xmin, 
                  xr = xmax, ndx = new.ndx, deg = object$deg[1])
            }
            else {
                deg <- object$deg[1]
                xl <- min(x)
                xr <- max(x)
                xmax <- xr + 0.01 * (xr - xl)
                xmin <- xl - 0.01 * (xr - xl)
                dx <- (xmax - xmin)/object$ndx[1]
                xl1 <- min(new.x)
                xr1 <- max(new.x)
                minK0 <- xmin - deg:(deg + 100) * dx
                minK <- minK0[which(minK0 <= xl1)[deg]]
                maxK0 <- xmax + deg:(deg + 100) * dx
                maxK <- maxK0[which(maxK0 >= xr1)[deg + 1]]
                knots <- seq(minK, maxK, by = dx)

                PP <- outer(new.x, knots, MortSmooth_tpower, 
                  deg)
                nn <- dim(PP)[2]
                DD <- diff(diag(nn), diff = deg + 1)/(gamma(deg + 
                  1) * dx^deg)
                new.Bx <- (-1)^(deg + 1) * PP %*% t(DD)
            }
            nbx <- ncol(new.Bx)
            if (min(new.y) >= min(y) & max(new.y) <= max(y)) {
                new.ndx <- object$ndx[2]
                yl <- min(new.y)
                yr <- max(new.y)
                ymax <- yr + 0.01 * (yr - yl)
                ymin <- yl - 0.01 * (yr - yl)
                new.By <- MortSmooth_bbase(x = new.y, xl = ymin, 
                  xr = ymax, ndx = new.ndx, deg = object$deg[2])
            }
            else {
                deg <- object$deg[2]
                yl <- min(y)
                yr <- max(y)
                ymax <- yr + 0.01 * (yr - yl)
                ymin <- yl - 0.01 * (yr - yl)
                dx <- (ymax - ymin)/object$ndx[2]
                yl1 <- min(new.y)
                yr1 <- max(new.y)
                minK0 <- ymin - deg:(deg + 100) * dx
                minK <- minK0[which(minK0 <= yl1)[deg]]
                maxK0 <- ymax + deg:(deg + 100) * dx
                maxK <- maxK0[which(maxK0 >= yr1)[deg + 1]]
                knots <- seq(minK, maxK, by = dx)


                PP <- outer(new.y, knots, MortSmooth_tpower, 
                  deg)
                nn <- dim(PP)[2]
                DD <- diff(diag(nn), diff = deg + 1)/(gamma(deg + 
                  1) * dx^deg)
                new.By <- (-1)^(deg + 1) * PP %*% t(DD)
            }
            nby <- ncol(new.By)
            Bx1 <- kronecker(matrix(1, ncol = nbx, nrow = 1), 
                new.Bx)
            Bx2 <- kronecker(new.Bx, matrix(1, ncol = nbx, nrow = 1))
            RTBx <- Bx1 * Bx2
            By1 <- kronecker(matrix(1, ncol = nby, nrow = 1), 
                new.By)
            By2 <- kronecker(new.By, matrix(1, ncol = nby, nrow = 1))
            RTBy <- By1 * By2
            Dx <- diff(diag(nbx), diff = object$pord[1])
            Dy <- diff(diag(nby), diff = object$pord[2])
my.d=rep(1,nby)
my.d[(ncol(object$By)+1):nby]=0


            Px <- kronecker(diag(my.d), t(Dx) %*% Dx)
            Py <- kronecker(t(Dy) %*% Dy, diag(nbx))
            new.Z[is.na(new.Z)] <- 0
            new.xx <- rep(new.x, new.n)
            new.yy <- rep(new.y, each = new.m)
            fit0 <- glm(round(c(new.Z)) ~ new.xx * new.yy + offset(c(new.offset)), 
                family = poisson, weights = c(new.W))
            etaGLM <- matrix(log(fit0$fitted) - c(new.offset), 
                new.m, new.n)
            eta0 <- log((new.Z + 1)) - new.offset
            eta0[new.W == 0] <- etaGLM[new.W == 0]
            BBx <- solve(t(new.Bx) %*% new.Bx + diag(nbx) * 1e-06, 
                t(new.Bx))
            BBy <- solve(t(new.By) %*% new.By + diag(nby) * 1e-06, 
                t(new.By))
            a.init <- MortSmooth_BcoefB(BBx, BBy, eta0)
            new.fit <- Mort2Dsmooth_estimate(x = new.x, y = new.y, 
                Z = new.Z, offset = new.offset, wei = new.W, 
                psi2 = 1, Bx = new.Bx, By = new.By, nbx = nbx, 
                nby = nby, RTBx = RTBx, RTBy = RTBy, lambdas = object$lambdas, 
                Px = Px, Py = Py, a.init = a.init, MON = FALSE, 
                TOL1 = 1e-06, MAX.IT = 50)
            new.eta0 <- matrix(MortSmooth_BcoefB(new.Bx, new.By, 
                new.fit$a), new.m, new.n)
            colnames(new.eta0) <- new.y
            rownames(new.eta0) <- new.x
            if (is.null(newdata$x)) {
                newdata$x <- x
            }
            if (is.null(newdata$y)) {
                newdata$y <- y
            }
            new.eta <- new.eta0[new.x %in% newdata$x, new.y %in% 
                newdata$y]
            new.fitted0 <- exp(new.eta0 + new.offset)
            new.fitted0[!whi] <- NA
            new.fitted <- new.fitted0[new.x %in% newdata$x, new.y %in% 
                newdata$y]
            BWB.P1 <- solve(new.fit$BWB + new.fit$P)
            new.se0 <- matrix(Mort2Dsmooth_se(RTBx = RTBx, RTBy = RTBy, 
                nbx = nbx, nby = nby, BWB.P1 = BWB.P1), new.m, 
                new.n, dimnames = list(new.x, new.y))
            new.se <- new.se0[new.x %in% newdata$x, new.y %in% 
                newdata$y]
            check.over <- eval(object$call$overdispersion)
            if (is.null(check.over)) {
                check.over <- FALSE
            }
            if (check.over) {
                se.inflate <- sqrt(object$psi2)
                new.se <- new.se * se.inflate
            }
            fit <- switch(type, link = new.eta, response = new.fitted)
            se.fit <- switch(type, link = new.se, response = new.fitted * 
                (exp(new.se) - 1))
        }
        pred <- list(fit = fit, se.fit = se.fit)
    }
    return(pred)
}
