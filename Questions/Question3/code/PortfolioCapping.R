Proportional_Cap_Foo <- function(df_Cons, W_Cap = 0.08){


    # Let's require a specific form from the user... Alerting when it does not adhere this form
    if( !"weight" %in% names(df_Cons)) stop("... for Calc capping to work, provide weight column called 'weight'")

    if( !"date" %in% names(df_Cons)) stop("... for Calc capping to work, provide date column called 'date'")

    if( !"Tickers" %in% names(df_Cons)) stop("... for Calc capping to work, provide id column called 'Tickers'")

    # First identify the cap breachers...
    Breachers <-
        df_Cons %>% filter(weight > W_Cap) %>% pull(Tickers)

    # Now keep track of breachers, and add to it to ensure they remain at 10%:
    if(length(Breachers) > 0) {

        while( df_Cons %>% filter(weight > W_Cap) %>% nrow() > 0 ) {


            df_Cons <-

                bind_rows(

                    df_Cons %>% filter(Tickers %in% Breachers) %>% mutate(weight = W_Cap),

                    df_Cons %>% filter(!Tickers %in% Breachers) %>%
                        mutate(weight = (weight / sum(weight, na.rm=T)) * (1-length(Breachers)*W_Cap) )

                )

            Breachers <- c(Breachers, df_Cons %>% filter(weight > W_Cap) %>% pull(Tickers))

        }

        if( sum(df_Cons$weight, na.rm=T) > 1.1 | sum(df_Cons$weight, na.rm=T) < 0.9 | max(df_Cons$weight, na.rm = T) > W_Cap) {

            stop( glue::glue("For the Generic weight trimming function used: the weight trimming causes non unit
      summation of weights for date: {unique(df_Cons$date)}...\n
      The restriction could be too low or some dates have extreme concentrations...") )

        }

    } else {

    }

    df_Cons

}
