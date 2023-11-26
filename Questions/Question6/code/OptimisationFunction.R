optimiser <- function(type = "mv", mu, Sigma, bvec, Amat, printmsg = TRUE){

    if(!type %in% c("mv", "minvol", "sharpe")) {
        stop("Invalid strategy type")
    }

    Safe_Optim <- purrr::safely(quadprog::solve.QP)

    if(type == "mv"){
        optim_w <- Safe_Optim(Dmat = Sigma, dvec = mu, Amat = Amat, bvec = bvec, meq = meq)
    }
    if(type == "minvol"){
        optim_w <- Safe_Optim(Dmat = Sigma, dvec = rep(0, nrow(Sigma)), Amat = Amat, bvec = bvec, meq = meq)
    }
    if(type == "sharpe"){
        Amat[,1] <- mu
        optim_w <- Safe_Optim(Dmat = Sigma, dvec = rep(0, nrow(Sigma)), Amat = Amat, bvec = bvec,  meq = meq)
    }

    if(is.null(optim_w$error)){
        resultTable <- tibble(stocks = colnames(Sigma), weight = optim_w$result$solution) %>%
            rename(!!type := weight)
    }  else {
        resultTable <- tibble(stocks = colnames(Sigma), weight = 1/ncol(Sigma)) %>%
            rename(!!type := weight)
    }
    resultTable
}
