Perf_comparisons <- function(df, Ys, Alias){

    Unconditional_SD <-
        df %>%
        group_by(Tickers) %>%
        mutate(Full_SD = sd(Return) * sqrt(12)) %>%
        filter(Year %in% Ys) %>%
        summarise(SD = sd(Return) * sqrt(12), across(.cols = starts_with("Full"), .fns = max)) %>%
        arrange(desc(SD)) %>% mutate(Period = Alias) %>%
        group_by(Tickers) %>%
        mutate(Ratio = SD / Full_SD)

    return(Unconditional_SD)

}
