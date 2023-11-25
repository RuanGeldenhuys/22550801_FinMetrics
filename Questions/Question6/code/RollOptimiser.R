Roll_optimizer <- function(return_mat, EndOfMonth, LookBack = 36, Amat, bvec){

    return_df_used <- return_mat %>% filter(date >= EndOfMonth %m-% months(LookBack))

    if(return_df_used %>% nrow() < LookBack) return(NULL)

    return_mat_Nodate <- data.matrix(return_mat[, -1])
    HTT <- fitHeavyTail::fit_mvt(return_mat_Nodate)
    mu <- HTT$mu
    Sigma <- HTT$cov
    Sigma <- as.matrix(Matrix::nearPD(Sigma)$mat)

    My_Weights <- left_join(
        optim_foo(type = "mv", mu, Sigma, bvec, Amat, printmsg = F),
        optim_foo(type = "minvol", mu, Sigma, bvec, Amat, printmsg = F), by = "stocks") %>%
        left_join(., optim_foo(type = "maxdecor", mu, Sigma, bvec, Amat, printmsg = F),  by = "stocks") %>%
        left_join(., optim_foo(type = "sharpe", mu, Sigma, bvec, Amat, printmsg = F), by = "stocks") %>%
        mutate(date = EndOfMonth , Look_Back_Period = LookBack)

}
