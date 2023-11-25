Perf_comparisons <- function(df, YMs, Alias){

    df <- yearlyReturns_sect_df
    Unconditional_SD <-
        df %>%
        group_by(Sector) %>%
        mutate(Full_SD = sd(Return) * sqrt(252)) %>%
        filter(Year %in% YMs) %>%
        summarise(SD = sd(Return) * sqrt(252), across(.cols = starts_with("Full"), .fns = max)) %>%
        arrange(desc(SD)) %>% mutate(Period = Alias) %>%
        group_by(Sector) %>%
        mutate(Ratio = SD / Full_SD)

    return(Unconditional_SD)

}
