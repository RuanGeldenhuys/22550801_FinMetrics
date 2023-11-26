# Purpose

Purpose of this work folder.

Ideally store a minimum working example data set in data folder.

Add binary files in bin, and closed R functions in code. Human Readable
settings files (e.g. csv) should be placed in settings/

## Question 1

This question investigates returns from an AI Fund, a market benchmark
(CAPPED SWIX) and active managers. It aims to shed light on the
differences and similarity in return structures through various
visualisation techniques.

In terms of data preparation, I calculate the average returns for both
ASISA and the benchmark across all funds and tickers. I then join these
returns with the returns of the AI fund into one data frame. I then
convert this into an ‘xts’ object which allows me to use the
‘PerformanceAnalytics’ package.

``` r
#Load and prep the data
ASISA <- read_rds("data/ASISA_Rets.rds")
BM <- read_rds("data/Capped_SWIX.rds")
AI_Fund <- read_rds("data/AI_Max_Fund.rds")

#Calculate the average ASISA return across all funds for a given month
ASISA_average <- ASISA %>% 
    group_by(date) %>% 
    summarise(meanReturn = mean(Returns))

#Calculate the average BM return across all indices for a given month
BM_average <- BM %>% 
    group_by(date) %>% 
    summarise(meanReturn = mean(Returns))


#Get all the returns in the same dataframe
returnDF <- left_join(ASISA_average, BM_average, by = 'date') %>% 
    left_join(AI_Fund, by='date')
colnames(returnDF)<- c('date','Active', 'Benchmark', 'AI')

returnDF <- returnDF %>% 
    pivot_longer(cols = c(-date),
                 names_to = "Fund",
                 values_to = "Returns")

#Convert to XTS
xts.returnDF <- returnDF %>% 
    tbl_xts(tblData = ., cols_to_xts = Returns, spread_by = Fund)
```

``` r
AIvsActive <- chart.Scatter(x = xts.returnDF$AI, 
                            y = xts.returnDF$Active, 
                            main = "Scatter: AI vs Active Managers", col = "darkred",symbolset = 16, 
                            xlab = "AI", ylab = "Active Managers")
```

![](README_files/figure-markdown_github/unnamed-chunk-3-1.png)

The figure above, made by the ‘PerformanceAnalytics’ package, shows a
scatter between the returns of active managers and the AI fund. A clear
positive correlation is visible.

``` r
AIvsBenchmark <- chart.Scatter(x = xts.returnDF$AI, 
                            y = xts.returnDF$Benchmark, 
                            main = "Scatter: AI vs Benchmark", col = "darkblue",symbolset = 16, 
                            xlab = "Benchmark", ylab = "Benchmark Indices")
```

![](README_files/figure-markdown_github/unnamed-chunk-4-1.png)

The figure above now shows a scatter between the returns of the
benchmark and the AI fund. An even stronger positive correlation is now
visible. This means that for, in both cases, it is rare for the AI fund
to show a negative return when the other funds show a positive return
and vice-versa.

The next step is to calculate rolling returns for all 3 funds. I
calculate the annualised rolling 3 year return using the ‘roll_prod’
function in the ‘RcppRoll’ package. I then join these rolling returns
together and then convert this data frame into long format to get it
ready for plotting.

``` r
##ASISA
#Calculate the annualized three year rolling return
ASISA_RollRet <- ASISA_average %>% 
    mutate(RollRets_ASISA = RcppRoll::roll_prod(1 + meanReturn, 36, fill = NA, align = "right")^(12/36)-1) %>% 
    group_by(date) %>% 
    filter(any(!is.na(RollRets_ASISA))) %>% 
    ungroup() %>%
    select(date, RollRets_ASISA)
    
##BM
##Calculate the annualized three year rolling return
BM_RollRet <- BM_average %>% 
    mutate(RollRets_BM = RcppRoll::roll_prod(1 + meanReturn, 36, fill = NA, align = "right")^(12/36)-1) %>% 
    group_by(date) %>% 
    filter(any(!is.na(RollRets_BM))) %>% 
    ungroup() %>%
    select(date, RollRets_BM)

joinedDF <- left_join(ASISA_RollRet, BM_RollRet, by = "date")

##AIFund
#Calculate the annualized three year rolling return
AI_RollRet <- AI_Fund %>% 
    mutate(RollRets_AI = RcppRoll::roll_prod(1 + AI_Fund, 36, fill = NA, align = "right")^(12/36)-1) %>% 
    group_by(date) %>% 
    filter(any(!is.na(RollRets_AI))) %>% 
    ungroup() %>%
    select(date, RollRets_AI)

joinedDF <- joinedDF %>% 
    left_join(AI_RollRet, by = "date")
colnames(joinedDF) <- c('date','Active','Benchmark','AIFund')


#Creating plotting dataframe
plottingDF <- joinedDF %>% 
    pivot_longer(cols = c(-date),
                 names_to = "Fund",
                 values_to = "RollingReturns")
```

``` r
g <- plottingDF %>% 
    ggplot() + 
    geom_line(aes(date, RollingReturns, color = Fund), alpha = 0.7, size = 1.25) + 
    labs(title = "Rolling 3 Year Annualized Returns Comparison", 
        subtitle = "", x = "", y = "Rolling 3 year Returns (Ann.)", 
        caption = "") + 
    theme_fmx(title.size = ggpts(30), subtitle.size = ggpts(5), caption.size = ggpts(25), CustomCaption = F) + 
    fmx_cols()+
    scale_fill_brewer(palette = "Accent") 
    

finplot(g, x.date.dist = "1 year", x.date.type = "%Y", x.vert = T, 
    y.pct = T, y.pct_acc = 1)
```

![](README_files/figure-markdown_github/unnamed-chunk-6-1.png)

The rolling 3 year annualised returns of all three funds are shown
above. All three funds tend to follow each other and show significant
downturns during the GFC and Covid-19. Comparatively, the active
managers perform the worst, as for large periods of time there rolling
returns are substantially lower than the other two funds. The AI fund
seems capable of outperforming the benchmark, but never for long periods
of time.

Next, I pivot the returns dataframe wider in order to apply a 25bps fee
on both the active managers and the AI fund. I then take the dataframe
back to long format in order to once again get it ready for plotting.

``` r
returnDF <- returnDF %>% 
    pivot_wider(names_from = Fund, values_from = Returns)
returnDF <- returnDF %>% 
    mutate(Active = Active - 0.025/12) %>%   # Applying a 25bps fee for active and AI fund
    mutate(AI = AI - 0.025/12)
returnDF <- returnDF %>% 
    pivot_longer(cols = c(-date),
                 names_to = "Fund",
                 values_to = "Returns")
```

``` r
# Plot the density functions of each return and add a line for the mean benchmark return
mean_benchmark_returns <- mean(returnDF$Returns[returnDF$Fund == "Benchmark"])

densityPlot <- ggplot(returnDF, aes(x = Returns)) + 
    geom_density(aes(fill = Fund), alpha = 0.5, color = NA) +
    geom_vline(xintercept = mean_benchmark_returns, linetype = "dashed", color = "black") +
    geom_text(aes(x = mean_benchmark_returns, y = 15), label = "Market Benchmark", 
              vjust = -1, hjust = -0.05, color = "black") +
    labs(title = "Density of Returns by Fund",
         x = "Returns",
         y = "Density") +
    theme_minimal() +
    scale_fill_brewer(palette = "Accent") +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
densityPlot
```

![](README_files/figure-markdown_github/unnamed-chunk-8-1.png) The
density functions of returns, grouped by fund, show a clear result.
Active managers tend to perform below the market benchmark. The marker
benchmark here was defined as the average return of the benchmark, as
can be seen in the code above. The market benchmark and the AI fund
perform very similar. The key difference here lies in the flatter
distribution of the AI fund, meaning it is less consistent that the
market benchmark.

The next chunk of code aims to compare the AI fund with the best and
worst performing actively managed fund. This is done by first filtering
the raw data to contain only funds with complete data, as some funds
only started recently or closed down, which makes comparison difficult.
Next the best active fund is selected by calculating the average return
for all funds and selecting the best one. The worst fund is found the
exact same way. I then calculate the rolling returns for these two
funds, using ‘RcppRoll’ and then join them in a data frame with the
rolling returns for the AI fund

``` r
#Filter out funds that don't have complete date
years <- 2003:2023
funds_with_complete_data <- ASISA %>%
  mutate(Year = year(as.Date(date))) %>%   
  group_by(Fund) %>%
  summarize(All_Years_Present = all(years %in% unique(Year))) %>% 
  filter(All_Years_Present) %>%
  pull(Fund)

#Find best performing fund
BestActive <- ASISA %>%
  filter(Fund %in% funds_with_complete_data) %>%
  group_by(Fund) %>%
  summarise(Average_Return = mean(Returns, na.rm = TRUE)) %>%
  arrange(desc(Average_Return)) %>%
  slice(1)

BestActiveReturns <- ASISA %>% 
    filter(Fund == BestActive$Fund) %>% 
    select(c('date','Returns'))


##Find worse performing fund
WorstActive <- ASISA %>%
  filter(Fund %in% funds_with_complete_data) %>%
  group_by(Fund) %>%
  summarise(Average_Return = mean(Returns, na.rm = TRUE)) %>%
  arrange(Average_Return) %>%
  slice(1)

WorstActiveReturns <- ASISA %>% 
    filter(Fund == WorstActive$Fund) %>% 
    select(c('date','Returns'))

#Get the rolling returns for best and worst fund
Best_RollRet <- BestActiveReturns %>% 
    mutate(RollRets_best = RcppRoll::roll_prod(1 + Returns, 36, fill = NA, align = "right")^(12/36)-1) %>% 
    group_by(date) %>% 
    filter(any(!is.na(RollRets_best))) %>% 
    ungroup() %>%
    select(date, RollRets_best)

Worst_RollRet <- WorstActiveReturns %>% 
    mutate(RollRets_worst = RcppRoll::roll_prod(1 + Returns, 36, fill = NA, align = "right")^(12/36)-1) %>% 
    group_by(date) %>% 
    filter(any(!is.na(RollRets_worst))) %>% 
    ungroup() %>%
    select(date, RollRets_worst)

##Group these funds and AI into one dataset
BestAndWorseDF <- left_join(Best_RollRet, Worst_RollRet, by = 'date') %>% 
    left_join(AI_RollRet, by = 'date')
colnames(BestAndWorseDF) <- c('date', 'BestActive', 'WorstActive', 'AI')
```

Next I plot these funds and shade the area between the best and worse
fund.

``` r
plotDiff <- ggplot(BestAndWorseDF, aes(x = date)) +
  geom_line(aes(y = BestActive, colour = "BestActive")) +
  geom_line(aes(y = WorstActive, colour = "WorstActive")) +
  geom_line(aes(y = AI, colour = "AI")) +
  geom_ribbon(aes(ymin = WorstActive, ymax = BestActive), fill = "grey", alpha = 0.5) +
  labs(title = "Comparison of Best and Worst Active funds vs AI Fund",
       x = "Date",
       y = "Rolling 3 Year Return (Ann.)",
       colour = "Fund") +
  scale_colour_manual(values = c("BestActive" = "blue", "WorstActive" = "red", "AI" = "green")) +
  theme_minimal()+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
plotDiff
```

![](README_files/figure-markdown_github/unnamed-chunk-10-1.png)

The figure reveals an interesting story. It appears that the AI fund
often lies between the rolling returns of these two funds.
Interestingly, for a period in the middle of our sample, the worst
performing fund, actually outperformed the best performing fund. This
potentially speaks to the shortcomings of average returns as a metric of
success. In this period, the AI fund also outperformed both of these
fund. The AI fund performs lower than the worst fund at times, but this
usually does not last long.
