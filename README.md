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

# Question 2

This question focuses on a recent article that discusses the
misconception surrounding hedging out the Rand. By replicating the
figure within that article I therefore show that hedging very rarely has
beneficial effects. I then go further by showing how the volatility of a
portfolio can actually be increased through hedging.

For data preparation I load in the data and then apply the weights
specified in the question to the corresponding asset by multiplying it
return with the corresponding weight. Summing these returns then yield
the total portfolio returns. I then calculate the ZAR returns and lastly
join the two return series’ into one data frame.

``` r
Indexes <- read_rds("data/Cncy_Hedge_Assets.rds")
ZAR <- read_rds("data/Monthly_zar.rds")

#Apply weights
Portfolio <- Indexes %>% 
    mutate(MSCI_ACWI = MSCI_ACWI * 0.18) %>% 
    mutate(Bbg_Agg = Bbg_Agg * 0.12) %>% 
    mutate(J433 = J433 * 0.42) %>% 
    mutate(ALBI = ALBI * 0.28)

#Calculate portfolio return.
PortfolioReturns <- Portfolio %>%
    rowwise() %>%
    mutate(sum = sum(c_across(-date), na.rm = TRUE)) %>%
    select(date, sum)

#Calculate ZAR returns
ZAR_Returns <- ZAR %>% 
    mutate(Return = value/lag(value) - 1) %>% 
    select(c('date','Return'))

#Join in one dataframe
joinedDF <- left_join(PortfolioReturns, ZAR_Returns, by = 'date')
colnames(joinedDF)<- c('date','Portfolio_Returns','USDZAR_Returns')
```

In order to replicate the graph in the article, the percentage of
observations that fall within each quadrant needs to be calculated. This
is handled by the code chunk below. First I remove the NA’s from my
data. I then iteratively count the number of observations in each
quadrant and divide that by the total number of observations to get a
percentage. I do the same for when a hedging fee is applied.

``` r
#Calculate Quadrant Percentages
cleanJoined <- na.omit(joinedDF)
total_count <- nrow(cleanJoined)

bothPositive_count <- sum(cleanJoined$Portfolio_Returns > 0 & cleanJoined$USDZAR_Returns > 0, na.rm = TRUE)
percentage_positive <- round((bothPositive_count / total_count) * 100, 1)
percentage_positive <- paste0(percentage_positive, "%")

bothNegative_count <- sum(cleanJoined$Portfolio_Returns < 0 & cleanJoined$USDZAR_Returns < 0, na.rm = TRUE)
percentage_negative <- round((bothNegative_count / total_count) * 100, 1)
percentage_negative <- paste0(percentage_negative, "%")

PosAndNeg_count <- sum(cleanJoined$Portfolio_Returns > 0 & cleanJoined$USDZAR_Returns < 0, na.rm = TRUE)
percentage_PosAndNeg <- round((PosAndNeg_count / total_count) * 100, 1)
percentage_PosAndNeg <- paste0(percentage_PosAndNeg, "%")

NegAndPos_count <- sum(cleanJoined$Portfolio_Returns < 0 & cleanJoined$USDZAR_Returns > 0, na.rm = TRUE)
percentage_NegAndPos <- round((NegAndPos_count / total_count) * 100, 1)
percentage_NegAndPos <- paste0(percentage_NegAndPos, "%")

#Quadrant percentages after a hedging fee
TopLeft_hedging <- sum(cleanJoined$Portfolio_Returns > 0 & cleanJoined$USDZAR_Returns < -0.025, na.rm = TRUE)
percentage_TopLeft_hedge <- round((TopLeft_hedging / total_count) * 100, 1)
percentage_TopLeft_hedge <- paste0(percentage_TopLeft_hedge, "%")

BotLeft_hedging <- sum(cleanJoined$Portfolio_Returns < 0 & cleanJoined$USDZAR_Returns < -0.025, na.rm = TRUE)
percentage_BotLeft_hedge <- round((BotLeft_hedging / total_count) * 100, 1)
percentage_BotLeft_hedge <- paste0(percentage_BotLeft_hedge, "%")
```

The plot is constructed in the chunk below. (You might want to turn off
warnings if you intend to run this bad boy yourself). The main plot is a
scatter plot with a regression line fitted. I then add lines at 0 for
both axis, and a line at -0.025 to show the fee of hedging. I then shade
the quadrants using annotate. Labels for the percentages calculated
above are added, as well as labels interpreting each quadrant. Lastly,
distributions are added on the top and side of the graph using
‘ggMarginal’

``` r
#Plotting this monster
p<- ggplot(joinedDF, aes(x = USDZAR_Returns, y = Portfolio_Returns)) +
    geom_point() +  # Add points for scatterplot
    geom_smooth(method = "lm", color = "blue") +  # Add linear regression line
    ylim(-0.2,0.2)+
    xlim(-0.2,0.2)+
    
    geom_vline(xintercept = 0, linetype = "solid") +  # Add vertical line at zero
    geom_hline(yintercept = 0, linetype = "solid") +  # Add horizontal line at zero
    
    geom_vline(xintercept = -0.025, linetype = "longdash")+
    
    annotate("rect", xmin = -Inf, xmax = 0, ymin = 0, ymax = Inf, fill = "green", alpha = 0.2) + # Top-left quadrant
    annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = Inf, fill = "red", alpha = 0.2) +   # Top-right quadrant
    annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = 0, fill = "pink", alpha = 0.2) + # Bottom-left quadrant
    annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = 0, fill = "blue", alpha = 0.2) + # Bottom-right quadrant
    
    labs(title = "ZAR/USD Returns vs. Portfolio Returns",
       x = "USD/ZAR Returns",
       y = "Portfolio Returns") +
    theme_minimal()+
    
    geom_label(aes(x = -Inf, y = Inf, label = percentage_PosAndNeg), 
               hjust = 0, vjust = 1, size = 4, color = "black", fill = "white") +  # Top-left quadrant
    geom_label(aes(x = Inf, y = Inf, label = percentage_positive), 
               hjust = 1, vjust = 1, size = 4, color = "black", fill = "white") +  # Top-right quadrant
    geom_label(aes(x = -Inf, y = -Inf, label = percentage_negative), 
               hjust = 0, vjust = 0, size = 4, color = "black", fill = "white") +  # Bottom-left quadrant
    geom_label(aes(x = Inf, y = -Inf, label = percentage_NegAndPos), 
               hjust = 1, vjust = 0, size = 4, color = "black", fill = "white") +  # Bottom-right quadrant
    
    geom_label(aes(x = -0.025, y = Inf, label = percentage_TopLeft_hedge), 
               hjust = 1, vjust = 1, size = 4, color = "black", fill = "white") +
    geom_label(aes(x = -0.025, y = -Inf, label = percentage_BotLeft_hedge), 
               hjust = 1, vjust = 0, size = 4, color = "black", fill = "white") +
    
    geom_label(aes(x=-0.1, y=0.1, label = "Hedge works but \n amplifies volatility"),
               hjust = 0.5, vjust = 0.5, size = 4, color = "black", fill = "white") +
    geom_label(aes(x=0.1, y=0.1, label = "Hedge throws away returns"),
               hjust = 0.5, vjust = 0.5, size = 4, color = "black", fill = "white") +
    geom_label(aes(x=-0.1, y=-0.1, label = "Best case for Hedge"),
               hjust = 0.5, vjust = 0.5, size = 4, color = "black", fill = "white") +
    geom_label(aes(x=0.1, y=-0.1, label = "Hedge removes currency \n cushion"),
               hjust = 0.5, vjust = 0.5, size = 4, color = "black", fill = "white") +
    
     theme(panel.border = element_rect(colour = "black", fill=NA, size=1))



p <- ggMarginal(p, type = "density", fill = 'darkgreen', color = NA)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

``` r
p
```

![](README_files/figure-markdown_github/unnamed-chunk-13-1.png)

The graph interpretation can be done as follows. If we find ourselves in
the top left quadrant, it means portfolio returns are positive and
USD/ZAR returns are negative. In this case a hedge will work but it
increases volatility, a less than desirable outcome. This occurs for
46.2% of observations and 30.1% if we apply a fee. The bottom left
quadrant shows when portfolio and USD/ZAR returns are negative. This is
the best case scenario for a hedge, yet it only occurs 7.5% of the time.
2.2% when applying a fee. The top right and bottom right quadrant shows
outright negative positions to be in when hedging against the Rand.
These occur 19.9% and 25.8% respectively.

In the code below, I first rename the columns of ‘PortfolioReturns’ to
‘date’ and ‘Return’, and define a set of weights for a portfolio. I then
calculate the rolling standard deviation (SD) for a hedged portfolio,
using returns calculated from these weights, rebalanced quarterly. This
hedged portfolio’s rolling SD is annualized over a 36-month window. For
the unhedged portfolio, I perform a similar process but adjust the
returns for currency effects using ‘ZAR_Returns’. Finally, I merge the
rolling SD data of both portfolios and reshape it into a long format,
allowing for a comparative analysis of the rolling SDs across the two
different portfolio types over time.

``` r
colnames(PortfolioReturns) <- c('date', 'Return')

weights <- c(0.18, 0.12, 0.42, 0.28)

#Calculate Rolling SD of a hedged portfolio
port1 <- Return.portfolio(Indexes, weights = weights, rebalance_on = "quarters")

port_hedged_df <- xts_tbl(port1) %>% 
    mutate(RollSD = RcppRoll::roll_sd(1 + portfolio.returns, 36, fill = NA, align = "right") * 
    sqrt(12)) %>% 
    filter(!is.na(RollSD))

#Calculate rolling SD of an unhedged portfolio
port_unhedged_df <- xts_tbl(port1) %>% 
    left_join(ZAR_Returns, by = 'date')
colnames(port_unhedged_df) <- c('date', 'portfolioReturns', 'currencyReturns')

port_unhedged_df <- port_unhedged_df %>% 
    mutate(Unhedged_Returns = (0.95*portfolioReturns) + (0.5*currencyReturns)) %>% #Unsure if this weighting is appropriate
    select(c(date, Unhedged_Returns)) %>% 
    na.omit()

port_unhedged_df <- port_unhedged_df %>% 
    mutate(RollSD_unhedged = RcppRoll::roll_sd(1 + Unhedged_Returns, 36, fill = NA, align = "right") * 
    sqrt(12)) %>% 
    filter(!is.na(RollSD_unhedged)) 


#Join the two portfolio SD's and the convert to long
joined_portfolios <-
    left_join(port_hedged_df, port_unhedged_df, by = 'date') %>% 
    select(c(date, RollSD, RollSD_unhedged)) %>% 
    filter(!is.na(RollSD_unhedged)) %>% 
    pivot_longer(-date,
                 names_to = 'portfolio',
                 values_to = 'RollSD')
```

Below I construct a plot of the two rolling SD’s of the two different
portfolios.

``` r
g <- joined_portfolios %>% 
    ggplot() + 
    geom_line(aes(date, RollSD, color = portfolio), alpha = 0.7, size = 1.25) + 
        
    labs(title = "Rolling 3 Year Annualized SD", 
        subtitle = "", x = "", y = "Rolling 3 year SD (Ann.)", 
        caption = "") + theme_fmx(title.size = ggpts(30), 
        subtitle.size = ggpts(5), caption.size = ggpts(25), CustomCaption = F) + 
        
    fmx_cols()
    
finplot(g, x.date.dist = "1 year", x.date.type = "%Y", x.vert = T, 
    y.pct = T, y.pct_acc = 1)
```

![](README_files/figure-markdown_github/unnamed-chunk-15-1.png)

The graph displayed above illustrates the rolling three-year annualized
standard deviation for both a hedged and an unhedged portfolio. What
stands out is that, for most of the observed period, the unhedged
portfolio exhibits lower volatility. This observation reinforces the
points made earlier, demonstrating that hedging against the Rand
actually elevates the portfolio’s overall volatility. This outcome
contradicts the primary goal of hedging against the Rand, which is to
reduce volatility.

# Question 3

This questions looks at two different all share indices, namely the ALSI
and the SWIX. I compare these two indices by first estimating the
cumulative returns for both broken down by sector and index. Next I
perform stratification analysis using the ZAR to see how these indices
react during currency volatility. Lastly, I compare the differences in
returns for different capped portfolios.

All functions for this question can be found in
‘Questions/Question3/code/’.

The code below first loads the required data. It then calculates the
cumulative return for the ALSI and SWIX, first grouped by Sector and
then by Index_Name. It does this by first pulling each unique sector out
of the raw data. For each sector it then applies a function called
‘calc_portfolio_returns’.

This function calculates the returns for portfolios based on specified
sectors and indices within a given dataset. Initially, it filters and
normalizes the data based on the chosen sector and index, if provided.
The function then separately processes data for two indices, J203 and
J403, by selecting relevant columns, spreading the data, and converting
it into a time-series format, handling any missing values. It calculates
the portfolio returns for both J203 and J403 using the
Safe_Return.portfolio function. Subsequently, it compiles raw data with
portfolio data, calculating and summarizing portfolio returns by date.
The final output joins the calculated returns from both J203 and J403,
reshaping them into a long format for comparative analysis, and returns
this dataset.

After the function returns, the simple returns, this is converted into
cumalative returns. Ultimately, all the cumulative returns for
sectors/indices are joined into one using rbind. We are left with two
data frames, one for Sector and one for Index

``` r
ALSI <- read_rds("data/ALSI.rds")
RebDays <- read_rds("data/Rebalance_days.rds")
ZAR <- read_rds("data/Monthly_zar.rds")


#Calculate the returns for ALSI and SWIX, grouped by SECTOR
sectors <- ALSI %>% pull(Sector) %>% unique()
sectorReturns <- list()

for(i in 1:length(sectors)){
    sectorReturns[[i]] <- calc_portfolio_returns(ALSI, chosen_sector = sectors[i]) %>% 
        group_by(Fund) %>% 
        mutate(CumReturn = (cumprod(1 + Returns))) %>% 
        mutate(CumReturn = CumReturn/first(CumReturn)) %>% 
        mutate(Sector = sectors[i])
}
names(sectorReturns) <- sectors

FullSectors_df <- rbind(sectorReturns[[1]],
                         sectorReturns[[2]], 
                         sectorReturns[[3]],
                         sectorReturns[[4]]) %>% arrange(date)

#Calculate the returns for ALSI and SWIX, grouped by INDEX
indices <- ALSI %>%  pull(Index_Name) %>% na.omit(.) %>%  unique()
IndexReturns<- list()

for(i in 1:length(indices)){
    IndexReturns[[i]] <- calc_portfolio_returns(ALSI, chosen_index = indices[i]) %>% 
        group_by(Fund) %>% 
        mutate(CumReturn = (cumprod(1 + Returns))) %>% 
        mutate(CumReturn = CumReturn/first(CumReturn)) %>% 
        mutate(Index = indices[i])
}
# Rename tibbles
names(IndexReturns) <- indices
# Combine Dataframes

FullIndex_df <- rbind(IndexReturns[[1]],
                      IndexReturns[[2]], 
                      IndexReturns[[3]]) %>% arrange(date)
```

Next I plot the cumulative returns for both ALSI and SWIX, grouped first
by sectors, then by index. The area under each curve is shaded. The row
binded data frames above allow me to now use ‘facet_wrap’ and instantly
have seperate plots for different sectors/indices.

``` r
#Create 4 facet wrapped plots of sector cumulative returns
sector_plot <- ggplot(data = FullSectors_df, aes(x = date, y = CumReturn, group = Fund)) +
    geom_line(aes(color = Fund)) +
    geom_ribbon(data = filter(FullSectors_df, Fund == "J203"), aes(ymin = 0, ymax = CumReturn, fill = "J203"), alpha = 0.3) +
    geom_ribbon(data = filter(FullSectors_df, Fund == "J403"), aes(ymin = 0, ymax = CumReturn, fill = "J403"), alpha = 0.3) +
    scale_color_manual(values = c("J203" = "blue", "J403" = "red")) +
    scale_fill_manual(values = c("J203" = "lightblue", "J403" = "pink")) +
    guides(fill = guide_none()) +
    scale_x_date(date_labels = "%Y-%m-%d") +
    labs(title = "Cumulative Returns by Sector", x = "Date", y = "Cumulative Return") +
    theme_minimal()+
    facet_wrap(~ Sector, scales = "free_y", ncol = 1)+
    theme(legend.position = "bottom")

finplot(sector_plot)
```

![](README_files/figure-markdown_github/unnamed-chunk-18-1.png)

``` r
#Create 4 facet wrapped plots of index cumulative returns
index_plot <- ggplot(data = FullIndex_df, aes(x = date, y = CumReturn, group = Fund)) +
    geom_line(aes(color = Fund)) +
    geom_ribbon(data = filter(FullIndex_df, Fund == "J203"), aes(ymin = 0, ymax = CumReturn, fill = "J203"), alpha = 0.3) +
    geom_ribbon(data = filter(FullIndex_df, Fund == "J403"), aes(ymin = 0, ymax = CumReturn, fill = "J403"), alpha = 0.3) +
    scale_color_manual(values = c("J203" = "blue", "J403" = "red")) +
    scale_fill_manual(values = c("J203" = "lightblue", "J403" = "pink")) +
    guides(fill = guide_none()) +
    scale_x_date(date_labels = "%Y-%m-%d") +
    labs(title = "Cumulative Returns by Index", x = "Date", y = "Cumulative Return") +
    theme_minimal()+
    facet_wrap(~ Index, scales = "free_y", ncol = 1)+
    theme(legend.position = "bottom")

finplot(index_plot)
```

![](README_files/figure-markdown_github/unnamed-chunk-18-2.png)

When looking at Sectors, the SWIX(J403) and ALSI(J203) perform nearly
identical when looking at the financial and property sector. ALSI
outperforms SWIX in the industrial sector, but SWIX does better in
resources. When looking at Index, interestingly, the ALSI outperforms
SWIX across the board.

The next step is to do stratification analysis with the Rand. This is
done in the code chunk below. I first filter currency data to start at
the same time as the ALSI and SWIX data, and then calculate the returns.
Next I convert to yearly since the data is montly and calculate the 80%
and 20% quantiles of the returns and save every year that exceeds those
quantiles in lists. I then run the ‘calc_portfolio_returns’ function
again as well as calculate the 1% quantiles for the portfolio returns.

Next I apply a function callled ‘Perf_comparisons’. This function takes
in the returns as well as the high and low volatility periods and
returns a data frame that contains the full standard deviation for the
index, as well as its standard deviation during either a high or low
volatility period of the Rand.

``` r
ZAR_returns <- ZAR %>% 
    filter(date > '2013-01-02') %>% #Filter for ZAR data after ALSI data start date
    mutate(Return = value/lag(value)-1) %>% 
    filter(date > first(date)) %>% 
    select(c(date, Return))


ZAR_sd <- ZAR_returns %>% 
    mutate(Year = format(date, '%Y')) %>% 
    group_by(Year) %>% 
    summarise(SD = sd(Return)*sqrt(12)) %>% 
  
    mutate(TopQuantile = quantile(SD, 0.8), BotQuantile = quantile(SD, 0.2))

Hi_Vol <- ZAR_sd %>%  filter(SD > TopQuantile) %>% pull(Year)
Low_Vol <- ZAR_sd %>%  filter(SD < BotQuantile) %>% pull(Year)


ReturnsDF <- calc_portfolio_returns(ALSI) %>% 
    mutate(Year= format(date, "%Y")) %>% 
    rename(Tickers = Fund, Return = Returns) %>% 
    group_by(Tickers) %>% 
    mutate(Top = quantile(Return, 0.99), Bot = quantile(Return, 0.01)) %>% 
    mutate(Return = ifelse(Return > Top, Top, 
                         ifelse(Return < Bot, Bot, Return))) %>% ungroup()

perf_hi <- Perf_comparisons(ReturnsDF, Ys = Hi_Vol, Alias = "High_Vol")
perf_lo <- Perf_comparisons(ReturnsDF, Ys = Low_Vol, Alias = "Low_Vol")

perf_hi
```

    ## # A tibble: 2 × 5
    ## # Groups:   Tickers [2]
    ##   Tickers     SD Full_SD Period   Ratio
    ##   <chr>    <dbl>   <dbl> <chr>    <dbl>
    ## 1 J403    0.0431  0.0353 High_Vol  1.22
    ## 2 J203    0.0425  0.0351 High_Vol  1.21

``` r
perf_lo
```

    ## # A tibble: 2 × 5
    ## # Groups:   Tickers [2]
    ##   Tickers     SD Full_SD Period  Ratio
    ##   <chr>    <dbl>   <dbl> <chr>   <dbl>
    ## 1 J203    0.0353  0.0351 Low_Vol 1.01 
    ## 2 J403    0.0348  0.0353 Low_Vol 0.984

Ratios above one indicate that that Ticker has a high (above usual)
volatility for that given period. In the case of high volatility of the
Rand, both the ALSI and SWIX show above usual volatility. Interestingly,
ALSI also shows above usual volatility during periods of low Rand
volatility.

I now continue on to construct a capped portfolio for both the ALSI and
SWIX. The first step is to load the rebalance days from the data. Next I
create a function, called ‘calculate_capped_port’ that applies the
‘Proportional_Cap_Foo’ function (straight from the practical) to this
data. It then gets the returns and weights in xts form which allows me
to construct a portfolio with the ‘rmsfuns::Safe_Return.portfolio’
function. For both ALSI and SWIX, I filter for days that are rebalance
days and reformat the data frame for the function. I then calculate
portfolios for a 10% and 5% cap for ALSI and SWIX. Lastly, I calculate
the 3 year rolling annualised return for each portfolio. Lastly, i join
these rolling returns into 2 data frames, a 5% cap and a 10% cap.

``` r
RebDays <- RebDays %>% 
    filter(Date_Type == 'Reb Trade Day')

calculate_capped_port <- function(dataframe, W_cap, ColName) {

    Capped_df <- dataframe %>% 
        group_split(RebalanceTime) %>% 
        map_df(~Proportional_Cap_Foo(., W_Cap = W_cap)) %>% 
        select(-RebalanceTime)

    df_weights <- Capped_df %>% 
        tbl_xts(cols_to_xts = weight, spread_by = Tickers)

    df_returns <- dataframe %>% 
        filter(Tickers %in% unique(Capped_df$Tickers)) %>% 
        tbl_xts(cols_to_xts = Return, spread_by = Tickers)

    df_weights[is.na(df_weights)] <- 0
    df_returns[is.na(df_returns)] <- 0

    df_capped <- rmsfuns::Safe_Return.portfolio(R = df_returns, 
                                                  weights = df_weights, 
                                                  lag_weights = TRUE) %>% 
        xts_tbl()
    
    colnames(df_capped) <- c('date', ColName)

    return(df_capped)
}

### ALSI
#Get data ready  for function
rebalance_ALSI <- ALSI %>% 
    filter(date %in% RebDays$date) %>% 
    mutate(RebalanceTime = format(date, "%Y%B")) %>% 
    select(date, Tickers, Return, J203, RebalanceTime) %>% 
    rename(weight = J203) %>% 
    mutate(weight = coalesce(weight , 0))

ALSI_capped_10 <- calculate_capped_port(rebalance_ALSI, 0.1, 'ALSI_10')
ALSI_capped_5 <- calculate_capped_port(rebalance_ALSI, 0.05, "ALSI_5")

ALSI_10_rollrets <- ALSI_capped_10 %>% 
    mutate(RollRets_ALSI_10 = RcppRoll::roll_prod(1 + ALSI_10, 4, fill = NA, align = "right")^(4/12)-1) %>% 
    group_by(date) %>% 
    filter(any(!is.na(RollRets_ALSI_10))) %>% 
    ungroup() %>% 
    select(date, RollRets_ALSI_10)

ALSI_5_rollrets <- ALSI_capped_5 %>% 
    mutate(RollRets_ALSI_5 = RcppRoll::roll_prod(1 + ALSI_5, 4, fill = NA, align = "right")^(4/12)-1) %>% 
    group_by(date) %>% 
    filter(any(!is.na(RollRets_ALSI_5))) %>% 
    ungroup() %>% 
    select(date, RollRets_ALSI_5)



###SWIX
rebalance_SWIX <- ALSI %>% 
    filter(date %in% RebDays$date) %>% 
    mutate(RebalanceTime = format(date, "%Y%B")) %>% 
    select(date, Tickers, Return, J403, RebalanceTime) %>% 
    rename(weight = J403) %>% 
    mutate(weight = coalesce(weight , 0))
  
SWIX_capped_10 <- calculate_capped_port(rebalance_SWIX, 0.1, 'SWIX_10')
SWIX_capped_5 <- calculate_capped_port(rebalance_SWIX, 0.05, 'SWIX_5')

SWIX_10_rollrets <- SWIX_capped_10 %>% 
    mutate(RollRets_SWIX_10 = RcppRoll::roll_prod(1 + SWIX_10, 4, fill = NA, align = "right")^(4/12)-1) %>% 
    group_by(date) %>% 
    filter(any(!is.na(RollRets_SWIX_10))) %>% 
    ungroup() %>% 
    select(date, RollRets_SWIX_10)

SWIX_5_rollrets <- SWIX_capped_5 %>% 
    mutate(RollRets_SWIX_5 = RcppRoll::roll_prod(1 + SWIX_5, 4, fill = NA, align = "right")^(4/12)-1) %>% 
    group_by(date) %>% 
    filter(any(!is.na(RollRets_SWIX_5))) %>% 
    ungroup() %>% 
    select(date, RollRets_SWIX_5)


cap5_df <- left_join(ALSI_5_rollrets, SWIX_5_rollrets, by = 'date')
plotdf_5 <- cap5_df %>% 
    pivot_longer(-date,
                 names_to = 'Fund',
                 values_to = 'RollRet')

cap10_df <- left_join(ALSI_10_rollrets, SWIX_10_rollrets, by = 'date')
plotdf_10 <- cap10_df %>% 
    pivot_longer(-date,
                 names_to = 'Fund',
                 values_to = 'RollRet')
```

Lastly, I plot the rolling returns grouped by the the portfolio cap.

``` r
g <- plotdf_5 %>% 
    ggplot() + 
    geom_line(aes(date, RollRet, color = Fund), alpha = 0.7, size = 1.25) + 
    labs(title = "Rolling 3 Year Annualized Returns SWIX vs ALSI (5% cap)", 
        subtitle = "", x = "", y = "Rolling 3 year Returns (Ann.)", 
        caption = "") + 
    theme_fmx(title.size = ggpts(30), subtitle.size = ggpts(5), caption.size = ggpts(25), CustomCaption = F) + 
    fmx_cols()+
    scale_fill_brewer(palette = "Accent") 

finplot(g, x.date.dist = "1 year", x.date.type = "%Y", x.vert = T, 
    y.pct = T, y.pct_acc = 1)
```

![](README_files/figure-markdown_github/unnamed-chunk-21-1.png)

``` r
g2 <- plotdf_10 %>% 
    ggplot() + 
    geom_line(aes(date, RollRet, color = Fund), alpha = 0.7, size = 1.25) + 
    labs(title = "Rolling 3 Year Annualized Returns SWIX vs ALSI (10% cap)", 
        subtitle = "", x = "", y = "Rolling 3 year Returns (Ann.)", 
        caption = "") + 
    theme_fmx(title.size = ggpts(30), subtitle.size = ggpts(5), caption.size = ggpts(25), CustomCaption = F) + 
    fmx_cols()+
    scale_fill_brewer(palette = "Accent") 

finplot(g2, x.date.dist = "1 year", x.date.type = "%Y", x.vert = T, 
    y.pct = T, y.pct_acc = 1)
```

![](README_files/figure-markdown_github/unnamed-chunk-21-2.png)

The ALSI and the SWIX’s rolling 3 year annualized returns closely follow
each other. There is not much difference between the two for both a 5%
and 10% cap. Interestingly, when looking at the period leading up to and
straight after Covid-19, ALSI outperforms SWIX during the periods of
high return, but under-performs during the downswing. This gap widens
when looking at 10% capped portfolios.

# Question 4

This question looks at several funds, their returns and the flows of
capital to and from those funds. They key question is then, do short
term successes have an effect of flow of funds? To answer this I
calculate correlation between short term returns and flows. I then
investigate whether their exists any notable correlation between these
correlations, fund size and average returns.

I load the data and write a small function to calculate rolling returns
for any amount of months I want, using ‘rollapply’ function from the
‘zoo’ package. I then calculate the rolling returns for each fund for 6
months, 1 year and 3 years. I then join this with the flows data.

``` r
Flows <- read_rds("data/ASISA_Flows.rds")
Rets <- read_rds("data/ASISA_Rets.rds")

calculate_period_return <- function(data, months) {
  data %>% 
    group_by(Fund) %>%
    arrange(Fund, date) %>%
    mutate(RollingReturn = rollapply((1 + Returns), width = months, prod, fill = NA, align = 'right')) %>%
    mutate(RollingReturn = RollingReturn - 1) # Subtracting 1 to get the actual return
}

# Calculate 6-month, 1-year, and 3-year returns
rets_6m <- calculate_period_return(Rets, 6) %>% 
    select(date, Fund, RollingReturn)
rets_1y <- calculate_period_return(Rets, 12) %>% 
    select(date, Fund, RollingReturn)
rets_3y <- calculate_period_return(Rets, 36) %>% 
    select(date, Fund, RollingReturn)

#Join with flows
joined_6m <- inner_join(rets_6m, Flows, by = c('date', 'Fund')) %>% 
    filter(!is.na(RollingReturn) & !is.na(Flows)) %>% 
    select(c(-Index, -FoF))
    
joined_1y <- inner_join(rets_1y, Flows, by = c('date', 'Fund')) %>% 
    filter(!is.na(RollingReturn) & !is.na(Flows)) %>% 
    select(c(-Index, -FoF))

joined_3y <- inner_join(rets_3y, Flows, by = c('date', 'Fund')) %>% 
    filter(!is.na(RollingReturn) & !is.na(Flows)) %>% 
    select(c(-Index, -FoF))
```

Next I calculate the correlation between the rolling returns and the
next months flows for each fund in the code below. This is then joined,
averaged, and tabled.

``` r
cor_6m <- joined_6m %>%
  group_by(Fund) %>%
  mutate(next_Flows = lead(Flows)) %>%
  filter(!is.na(next_Flows)) %>%
  summarise(correlation = cor(RollingReturn, next_Flows, use = "complete.obs"))

cor_1y <- joined_1y %>%
  group_by(Fund) %>%
  mutate(next_Flows = lead(Flows)) %>%
  filter(!is.na(next_Flows)) %>%
  summarise(correlation = cor(RollingReturn, next_Flows, use = "complete.obs"))

cor_3y <- joined_3y %>%
  group_by(Fund) %>%
  mutate(next_Flows = lead(Flows)) %>%
  filter(!is.na(next_Flows)) %>%
  summarise(correlation = cor(RollingReturn, next_Flows, use = "complete.obs"))

joined_cor <- left_join(cor_6m, cor_1y, by = "Fund") %>% 
    left_join(cor_3y, by = 'Fund')
colnames(joined_cor) <- c("Fund", "cor_6m", "cor_1y", "cor_3y")

avg_6m_cor <- mean(cor_6m$correlation, na.rm = T)
avg_1y_cor <- mean(cor_1y$correlation, na.rm = T)
avg_3y_cor <- mean(cor_3y$correlation, na.rm = T)


cor_table <- data.frame(avg_6m_cor, avg_1y_cor, avg_3y_cor)
colnames(cor_table) <- c('6 Month', '1 Year', '3 Year')

result <- kable(cor_table)
result
```

<table>
<thead>
<tr>
<th style="text-align:right;">
6 Month
</th>
<th style="text-align:right;">
1 Year
</th>
<th style="text-align:right;">
3 Year
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
0.0528127
</td>
<td style="text-align:right;">
0.0806287
</td>
<td style="text-align:right;">
0.065543
</td>
</tr>
</tbody>
</table>

The table above shows the average correlation between rolling returns
and flows to a fund. As can be seen, very little correlation exists. The
highest correlation is an 8% correlation for the 1 year rolling returns.
This early result indicates that investors know that past wins do not
mean future wins and as such don’t flock to a fund that has been
performing well recently.

Next I calculate the total flows and the average returns for each fund
and join this with the correlation dataframe.

``` r
flow_df <- Flows %>% 
    group_by(Fund) %>% 
    summarise(TotalFlows = sum(Flows, na.rm = T))

return_df <- Rets %>% 
    group_by(Fund) %>% 
    summarise(avgReturn = mean(Returns, na.rm = T))

joined_df <- left_join(flow_df, return_df, by = 'Fund') %>% 
    left_join(joined_cor, by = "Fund")
```

``` r
g1 <- joined_df %>% 
    arrange(desc(avgReturn)) %>% 
    slice(1:50) %>%  #Only use top 50 funds to make plot more understandable
    ggplot(aes(x=TotalFlows, y=cor_6m, size=avgReturn))+
    geom_point(alpha=0.7, color = "steelblue")+
    ylim(c(-0.8, 1.3))+
    scale_size(range = c(3, 15), name="Average Returns")+
    labs(title = "6 Month Correlation vs Total Flows",
         x = 'Total Flows',
         y = '1 Year Correlation')+ 
    theme_fmx(title.size = ggpts(30), subtitle.size = ggpts(5), caption.size = ggpts(25), CustomCaption = F) + 
    fmx_cols()

finplot(g1)
```

![](README_files/figure-markdown_github/unnamed-chunk-25-1.png)

``` r
g2 <- joined_df %>% 
    arrange(desc(avgReturn)) %>% 
    slice(1:50) %>% 
    ggplot(aes(x=TotalFlows, y=cor_1y, size=avgReturn))+
    geom_point(alpha=0.7, color = "lightsalmon3")+
    scale_size(range = c(3, 15), name="Average Returns")+
    labs(title = "1 Year Correlation vs Total Flows",
         x = 'Total Flows',
         y = '1 Year Correlation')+ 
    theme_fmx(title.size = ggpts(30), subtitle.size = ggpts(5), caption.size = ggpts(25), CustomCaption = F) + 
    fmx_cols()
finplot(g2)
```

![](README_files/figure-markdown_github/unnamed-chunk-26-1.png)

``` r
g3 <- joined_df %>% 
    arrange(desc(avgReturn)) %>% 
    slice(1:50) %>% 
    ggplot(aes(x=TotalFlows, y=cor_3y, size=avgReturn))+
    geom_point(alpha=0.7, color = "orange")+
    scale_size(range = c(3, 15), name="Average Returns")+
    labs(title = "3 Year Correlation vs Total Flows",
         x = 'Total Flows',
         y = '1 Year Correlation')+ 
    theme_fmx(title.size = ggpts(30), subtitle.size = ggpts(5), caption.size = ggpts(25), CustomCaption = F) + 
    fmx_cols()
finplot(g3)
```

![](README_files/figure-markdown_github/unnamed-chunk-27-1.png)

The plots above shows scatter plots between total flows for each fund
and the correlation of returns and flows. The size of the points are
determined by the average returns for that fund. It is clear that no
correlation exists. This means that the total flows a fund receive does
not change the perception of investors. They still hold the belief that
past successes don’t predict future winners. The average returns of a
fund also don’t seem to have an effect. It it did we would see all the
large points gather at either the top or bottom of the graph. This
result holds for the 6 month, 1 year and 3 year correlations.

# Question 6

This question is aimed at optimising a portfolio that consists of
multiple global indices, given multiple constraints. It is a question
focused on methodology and as such, interpretation and discussion will
be limited. The functions used for this question are located in
‘Questions/Question6/code/’.

The code chunk below handles the initial loading and preperation of the
data.

``` r
# Load in Data
MAA <- read_rds("data/MAA.rds") %>% select(-Name) %>% arrange(date) %>% rename(Tickers = Ticker)
msci <- read_rds("data/msci.rds") %>%
filter(Name %in% c("MSCI_ACWI", "MSCI_USA", "MSCI_RE", "MSCI_Jap")) %>% rename(Tickers = Name) %>% arrange(date)
    
# Combining datasets
combined_df <- rbind(MAA, msci) %>% arrange(date)

# Get funds that existed 3 years ago
validfunds <- combined_df %>% 
    group_by(Tickers) %>% 
    filter(date == as.Date("2018/01/01")) %>% 
    pull(Tickers) %>% unique()
    
# Filter for valid funds
StartDate <- combined_df %>% 
    filter(Tickers %in% validfunds) %>% 
    group_by(Tickers) %>% 
    summarise(date = dplyr::first(date)) %>% 
    summarise(latest = dplyr::first(date))

# Find the end of each month
EndOfMonth <- combined_df %>% 
    filter(Tickers %in% validfunds) %>% 
    filter(date >= StartDate[[1]]) %>% 
    select(date) %>% 
    unique() %>% 
    mutate(YM = format(date, "%Y%B")) %>% 
    group_by(YM) %>% 
    filter(date == dplyr::last(date)) %>% 
    ungroup() %>% 
    pull(date) %>% 
    unique()

# Quaterly Rebalancing dates
RebalanceDates <- rmsfuns::dateconverter(as.Date(EndOfMonth[1]), as.Date(EndOfMonth[238]), 
    "weekdayEOQ") 

# Filter data quaterly dates, determine returns
combined_df <- combined_df %>% 
    filter(Tickers %in% validfunds) %>% 
    filter(date >= StartDate[[1]]) %>% 
    filter(date %in% RebalanceDates) %>% 
    group_by(Tickers) %>% 
    mutate(ret = Price/lag(Price) - 1) %>% 
    filter(date > dplyr::first(date)) %>% 
    select(-Price) %>%
    spread(Tickers, ret)
```

This chunk handles missing values by calling a function called
‘impute_missing_returns’ (straight from practical). I then drop dates
from the data frame.

``` r
# Impute missing values for return series
options(scipen = 999)
return_mat <- 
  impute_missing_returns(combined_df, impute_returns_method = "Drawn_Distribution_Collective", Seed = as.numeric(format( Sys.time(), "%Y%d%H%M")))

# Create returns matrix
return_mat_Nodate <- data.matrix(return_mat[, -1])
```

Next I set up my constraints. As given, no less than 1% of the portfolio
can consist of one asset and no more than 40%. Equity holdings are
restricted to 60% of the portfolio and bond holdings to 25%. I then set
up the relevant matrices. I then map a function called ‘Roll_Optimiser’
to these matrices. This function will, along with some data preperation,
call to another function, called ‘optimiser’. This function applies a
mean-variance, minimum volatility and a sharpe optimistaion method
through ‘quadprog’ package. This outputs a table of the optimised
weights over time.

``` r
NStox <- ncol(return_mat_Nodate)
LB = 0.01
UB = 0.4
Eq = 0.6 # Equity constraint
Bonds = 0.25 # Bond constraint
meq = 1

Eq_mat <- rbind(matrix(0, nrow = 9, ncol = 4), -diag(4))
C_B_mat <- rbind(matrix(0, 3, 6), -diag(6), matrix(0, 4, 6))

bvec <- c(1, rep(LB, NStox), -rep(UB, NStox), -rep(Eq, 4), -rep(Bonds, 6))
Amat <- cbind(1, diag(NStox), -diag(NStox), Eq_mat, C_B_mat)

#Get optimal portfolio weights
Result <- RebalanceDates %>% 
    map_df(~Roll_optimizer(return_mat, EndOfMonth = ., Amat = Amat, bvec = bvec, LookBack = 36))

printTable <- kable(head(Result,10))
printTable
```

<table>
<thead>
<tr>
<th style="text-align:left;">
stocks
</th>
<th style="text-align:right;">
mv
</th>
<th style="text-align:right;">
minvol
</th>
<th style="text-align:right;">
sharpe
</th>
<th style="text-align:left;">
date
</th>
<th style="text-align:right;">
Look_Back_Period
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
ADXY Index
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.4000000
</td>
<td style="text-align:right;">
0.0769231
</td>
<td style="text-align:left;">
2002-03-29
</td>
<td style="text-align:right;">
36
</td>
</tr>
<tr>
<td style="text-align:left;">
BCOMTR Index
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.0246831
</td>
<td style="text-align:right;">
0.0769231
</td>
<td style="text-align:left;">
2002-03-29
</td>
<td style="text-align:right;">
36
</td>
</tr>
<tr>
<td style="text-align:left;">
DXY Index
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.2294918
</td>
<td style="text-align:right;">
0.0769231
</td>
<td style="text-align:left;">
2002-03-29
</td>
<td style="text-align:right;">
36
</td>
</tr>
<tr>
<td style="text-align:left;">
LEATTREU Index
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.0100000
</td>
<td style="text-align:right;">
0.0769231
</td>
<td style="text-align:left;">
2002-03-29
</td>
<td style="text-align:right;">
36
</td>
</tr>
<tr>
<td style="text-align:left;">
LGAGTRUH Index
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.1447553
</td>
<td style="text-align:right;">
0.0769231
</td>
<td style="text-align:left;">
2002-03-29
</td>
<td style="text-align:right;">
36
</td>
</tr>
<tr>
<td style="text-align:left;">
LGCPTRUH Index
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.0100000
</td>
<td style="text-align:right;">
0.0769231
</td>
<td style="text-align:left;">
2002-03-29
</td>
<td style="text-align:right;">
36
</td>
</tr>
<tr>
<td style="text-align:left;">
LP05TREH Index
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.0100000
</td>
<td style="text-align:right;">
0.0769231
</td>
<td style="text-align:left;">
2002-03-29
</td>
<td style="text-align:right;">
36
</td>
</tr>
<tr>
<td style="text-align:left;">
LUACTRUU Index
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.0100000
</td>
<td style="text-align:right;">
0.0769231
</td>
<td style="text-align:left;">
2002-03-29
</td>
<td style="text-align:right;">
36
</td>
</tr>
<tr>
<td style="text-align:left;">
LUAGTRUU Index
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.1210699
</td>
<td style="text-align:right;">
0.0769231
</td>
<td style="text-align:left;">
2002-03-29
</td>
<td style="text-align:right;">
36
</td>
</tr>
<tr>
<td style="text-align:left;">
MSCI_ACWI
</td>
<td style="text-align:right;">
0.40
</td>
<td style="text-align:right;">
0.0100000
</td>
<td style="text-align:right;">
0.0769231
</td>
<td style="text-align:left;">
2002-03-29
</td>
<td style="text-align:right;">
36
</td>
</tr>
</tbody>
</table>

There you go. An optimised portfolio according to three different
methods of optimisation. I output the first ten rows in the data frame
here.
