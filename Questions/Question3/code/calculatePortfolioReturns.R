calc_portfolio_returns <- function(dataset = ALSI, chosen_sector = "", chosen_index = ""){
    raw_data <- dataset

    if(!chosen_sector == ""){
        raw_data <- dataset %>% filter(Sector %in% chosen_sector) %>%
            group_by(date) %>%
            mutate(J403 = J403 / sum(J403, na.rm = TRUE),
                   J203 = J203 / sum(J203, na.rm = TRUE)) %>%
            ungroup()
    }

    if(!chosen_index == ""){
        raw_data <- dataset %>% filter(Index_Name %in% chosen_index) %>%
            group_by(date) %>%
            mutate(J403 = J403 / sum(J403, na.rm = TRUE),
                   J203 = J203 / sum(J203, na.rm = TRUE)) %>%
            ungroup()
    }

    if(!chosen_sector == "" & !chosen_index == ""){
        raw_data <- dataset %>%
            group_by(date) %>%
            mutate(J403 = J403 / sum(J403, na.rm = TRUE),
                   J203 = J203 / sum(J203, na.rm = TRUE)) %>%
            ungroup()
    }

    weights_J203 <- raw_data %>%
        select(date, Tickers, J203) %>%
        mutate(J203 = coalesce(J203, 0)) %>%
        spread(Tickers, J203) %>%
        tbl_xts() %>%
        replace(is.na(.), 0)

    weights_J403 <- raw_data %>%
        select(date, Tickers, J403) %>%
        spread(Tickers, J403) %>%
        tbl_xts() %>%
        replace(is.na(.), 0)

    returns_data <- raw_data %>%
        select(date, Tickers, Return) %>%
        spread(Tickers, Return) %>%
        replace(is.na(.), 0) %>%
        tbl_xts()

    portfolio_J203 <- rmsfuns::Safe_Return.portfolio(
        R = returns_data,
        weights = weights_J203,
        lag_weights = TRUE,
        contribution = TRUE,
        verbose = TRUE,
        value = 1,
        geometric = TRUE)

    portfolio_J403 <- rmsfuns::Safe_Return.portfolio(
        R = returns_data,
        weights = weights_J403,
        lag_weights = TRUE,
        contribution = TRUE,
        verbose = TRUE,
        value = 1000,
        geometric = TRUE)

    process_data <- function(data, weights){
        contribution <- data$"contribution" %>% xts_tbl() %>%
            mutate(date = lag(date), date = coalesce(date, index(weights)[1]))

        bop_weight <- data$"BOP.Weight" %>% xts_tbl() %>%
            mutate(date = lag(date), date = coalesce(date, index(weights)[1]))

        bop_value <- data$"BOP.Value" %>% xts_tbl() %>%
            mutate(date = lag(date), date = coalesce(date, index(weights)[1]))

        list(contribution = contribution, bop_weight = bop_weight, bop_value = bop_value)
    }

    data_J203 <- process_data(portfolio_J203, weights_J203)
    data_J403 <- process_data(portfolio_J403, weights_J403)

    compile_raw_data <- function(fund_data, port_data){
        fund_data %>%
            select(date, Tickers, Return) %>%
            left_join(port_data$bop_weight %>% gather(Tickers, weight, -date),
                      by = c("date", "Tickers")) %>%
            left_join(port_data$bop_value %>% gather(Tickers, value_held, -date),
                      by = c("date", "Tickers")) %>%
            left_join(port_data$contribution %>% gather(Tickers, Contribution, -date),
                      by = c("date", "Tickers")) %>%
            group_by(date) %>%
            summarise(PortfolioReturn = sum(Return * weight, na.rm = TRUE)) %>%
            filter(PortfolioReturn != 0)
    }

    portfolio_J203_data <- compile_raw_data(dataset, data_J203)
    portfolio_J403_data <- compile_raw_data(dataset, data_J403)

    final_output <- left_join(
        portfolio_J203_data %>% rename(J203 = PortfolioReturn),
        portfolio_J403_data %>% rename(J403 = PortfolioReturn),
        by = "date"
    ) %>% pivot_longer(c("J203", "J403"), names_to = "Fund", values_to = "Returns")

    final_output
}



