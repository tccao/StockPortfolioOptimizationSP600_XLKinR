#Install important libraries
# install.packages("XML")
# install.packages("htmltab")
# install.packages("rvest")
# install.packages("tidyquant")

# Import necessary libraries
library(tidyverse)
library(tidyquant)
library(rvest)

# Set random seed for reproducibility reasons
set.seed(301)

#Get HTML tables containing all stocks in S&P600 smallcap composite index,
#number 2 to refer the table index

#Get HTML tables containing all stocks in S&P600 smallcap composite index
ticker_names_table <- read_html("https://en.wikipedia.org/wiki/List_of_S%26P_600_companies") %>%
    html_element("table.wikitable") %>%
    html_table()

#Get all stock tickers from column Ticker using pipe operator 
ticker_names <- as.vector(ticker_names_table %>% select("Symbol"))


##Only applicable to old tidyquant 
#Using Yahoo finance requires specific format, especially for stocks with period. We need to transform it into a hyphen
# #and append ".TO" at the end to each tickers.
# # ticker_names_transform <- gsub("[[:punct:]]","-",ticker_names$Symbol)
# #Use stringi to parse and join string to our tickers
# # ticker_names_transform <- as.vector(stringi::stri_join(ticker_names_transform, ".TO"))

#Now, using log returns to see the change in price with respect to the day before using tq_get from
#tidyquant
SP600_returns <- ticker_names$Symbol %>% #Replace ticker_names_transform for old version
    tq_get(get = "stock.prices",
           from = "2023-01-01",
           to = "2023-09-14") %>%
    group_by(symbol) %>% #See periodReturn per ticker
    tq_transmute(select = adjusted,
                 mutate_fun = periodReturn, #mutate_fun create new columns using specific function, here finding the period return
                 period="daily",
                 type="log",
                 col_rename = "Ra") #Ra stands for return group a

#Next, we will benchmark the stock against particular industry index in S&P500. We'll choose XLK here since
#tq_get only works in the US Index

XLK_returns <- "XLK" %>%
    tq_get(get= "stock.prices",
           from = "2023-01-01",
           to = "2023-09-14") %>%
    tq_transmute(select = adjusted,
                 mutate_fun =  periodReturn,
                 type = "log",
                 period = "daily",
                 col_rename = "Rb") #Return for group b 

#Perform left-join on Ra and Rb to see how well stocks perform against index
RaRb <- left_join(SP600_returns,
                  XLK_returns,
                  by = "date") %>%
        group_by(symbol)

#Capital Asset Pricing Model (CAPM) : Return R = alpha*Return - beta*Risk
CAPM <- RaRb %>%
        tq_performance(
            Ra=Ra,
            Rb = Rb,
            performance_fun = table.CAPM)

#Depend on return(Alpha) or Risk(Beta) preference, pick appropriate 
#indicator or Information Ratio a/b for both, or top quintile performance.
top_percentile <- CAPM %>%
                filter(AnnualizedAlpha > quantile(CAPM$AnnualizedAlpha, 0.9))

#Using the new tickers, return to our original log-return table to get its log-return
quintile_ticker = top_percentile$symbol

stock_returns_quintile <- SP600_returns %>% filter(symbol %in% quintile_ticker)

#Performing Portfolio Distribution

#Single case method, we need to manually set weights since tq_portfolio doesn't have filter function
set.seed(5)
weights <- c(1:100)
random_weights <- sample(weights, size=length(quintile_ticker), replace = TRUE)
index_for_zeroes <- sample(c(1:length(random_weights)),round(length(quintile_ticker)*0.78,0)) #37 out of 47 Tickers will be 0
random_weights[index_for_zeroes] <- 0 #Insert zeroes accordingly
#Random weigth of 0 yields result similar to Gaussian Hyper Parameter Search, which is better than quadratic optimization in term of speed and performance

#Now, we need to ensure all weights sum up as 1 by normalizing them
weights_mapping_single <- tibble(
    symbols = quintile_ticker,
    weights = random_weights) %>%
        mutate( weights = weights/sum(weights))

#Use tq_portfolio to pick portfolio with non-zero weights. Respective return = sum of product of log return X weights
portfolio_returns_daily <- stock_returns_quintile %>% 
    tq_portfolio(assets_col = symbol,
                 returns_col =  Ra,
                 weights =  weights_mapping_single,
                 col_rename = "Ra")

#Now, we join Benchmark return to our portfolio
RaRb_single_portfolio <- left_join(portfolio_returns_daily,
                                   XLK_returns,
                                   by = "date")

#Perform CAPM analysis
RaRb_single_portfolio %>%
  tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.CAPM)
RaRb_single_portfolio %>%
  tq_performance(Ra = Ra, Rb = Rb, performance_fun = InformationRatio)

#Plot daily returns using bar plot with regresion line for trend

portfolio_returns_daily %>%
  ggplot(aes(x = date, y = Ra)) +
  geom_bar(stat = "identity", fill = palette_light()[[1]]) +
  labs(title = "Portfolio Returns",
       x = "", y = "Monthly Returns") +
  geom_smooth(method = "lm") +
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::percent)

#Test out our portfolio with 10,000 investment.
symbol_a = c(rep('PRT', length(portfolio_returns_daily$Ra)))

portfolio_asset_name = cbind(portfolio_returns_daily, symbol_a)

portfolio_growth_single <- portfolio_asset_name %>%
    tq_portfolio(assets_col   = symbol_a, 
                returns_col  = Ra, 
                col_rename   = "investment.growth",
                wealth.index = TRUE) %>%
    mutate(investment.growth = investment.growth * 10000)

#Now, let's compare it with our bask XLK to see how our portfolio return
symbol_b = c(rep('XLK', length(XLK_returns$Rb)))

XLK_asset_name = cbind(XLK_returns, symbol_b)

portfolio_growth_XLK <- XLK_asset_name %>%
    tq_portfolio(assets_col   = symbol_b, 
                returns_col  = Rb, 
                col_rename   = "investment.growth",
                wealth.index = TRUE) %>%
    mutate(investment.growth = investment.growth * 10000)

#Plot both portfolio, with blue being our pick and red being the index fund
ggplot() + 
geom_line(data = portfolio_growth_single, aes(x = date, y = investment.growth), color = "blue") +
geom_line(data = portfolio_growth_XLK, aes(x = date, y = investment.growth), color = "red") +
labs(title = "Portfolio Growth", x = "", y = "Portfolio Value") +
theme_tq() +
scale_color_tq() +
scale_y_continuous(labels = scales::dollar)

##Performing multiple portfolio with 100 trials
set.seed(20)  
repetitions <- 100

#We need a vector with the same Tickers as before but multiplied by our desired number of repetitions.

stock_returns_quintile <- stock_returns_quintile %>%
  tq_repeat_df(n = repetitions)

random_weights_multiple <- sample(weights, length(quintile_ticker)*repetitions, replace=TRUE)
index_zeroes_multiple <- sample(c(1:length(random_weights_multiple)), round(length(random_weights_multiple)*.75, 0))
random_weights_multiple[index_zeroes_multiple]<-0

#Now, with ticker weights vary in different portfolios, we can see how each perform by grouping by according portfolio and weight normalized.
weights_table <-  tibble(quintile_ticker) %>%
  tq_repeat_df(n = repetitions) %>%
  bind_cols(tibble(random_weights_multiple)) %>%
  group_by(portfolio) %>% 
  mutate(random_weights_multiple = random_weights_multiple / sum(random_weights_multiple))

#Find portfolio return
portfolio_returns_multi <- stock_returns_quintile %>%
  tq_portfolio(assets_col  = symbol, 
               returns_col = Ra, 
               weights     = weights_table, 
               col_rename  = "Ra")

#Join multi-portfolio with XLK-Tech index
RaRb_multiple_portfolios <- left_join(portfolio_returns_multi, 
                                     XLK_returns,
                                     by = "date")

#Perform CAPM analysis
CAPM_multiple_portfolios <- RaRb_multiple_portfolios %>%
    tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.CAPM)

#Find the best perform portfolio by annualized alpha sorted in descending order
annualized_returns <- RaRb_multiple_portfolios %>%
  tq_performance(Ra = Ra, Rb = NULL, performance_fun = table.AnnualizedReturns) %>%
  arrange(desc(AnnualizedReturn))

#Now, using wealth.index to see how our portfolio grow 
portfolio_growth_monthly_multi <- stock_returns_quintile %>%
  tq_portfolio(assets_col   = symbol, 
               returns_col  = Ra, 
               weights      = weights_table, 
               col_rename   = "investment.growth",
               wealth.index = TRUE) %>%
  mutate(investment.growth = investment.growth * 10000)

#Plotting our result to see howeach portfolio performs
portfolio_growth_monthly_multi %>%
  ggplot(aes(x = date, y = investment.growth, color = factor(portfolio), legend.position = "none")) +
  geom_line(linewidth = 2) +
  labs(title = "Randomly Optimized Portfolios",
       subtitle = "Comparing Multiple Portfolios",
       x = "", y = "Portfolio Value",
       color = "Portfolio",
       legend.position = "none") +
  geom_smooth(method = "loess") +
  theme_tq() +
  scale_color_tq() +
  theme() +
  scale_y_continuous(labels = scales::dollar)
  # theme(legend.position = "none") +
  

#Spread the weight table over our porfolio to see how each Ticker is distributed. We can then sort by portfolio to see how each Ticker performs.
options(digits=2)
keymatrix <- weights_table %>% 
    pivot_wider(names_from = portfolio, 
                values_from = random_weights_multiple)
print(keymatrix, n=length(keymatrix))

keymatrix2 <- cbind(keymatrix$quintile_ticker,keymatrix$"65")

#Only plot the return of the highest and the lowest portfolio
best_portfolio <- annualized_returns[1,1]
worst_portfolio <- annualized_returns[nrow(annualized_returns)-1,1]
best_worst_portfolio <- c(best_portfolio,worst_portfolio)

portfolio_growth_monthly_multi %>%
  filter(portfolio %in% best_worst_portfolio) %>% 
  ggplot(aes(x = date, y = investment.growth, color = factor(portfolio), legend.position = "none")) +
  geom_line(linewidth = 2) +
  labs(title = "Randomly Optimized Portfolios",
       subtitle = "Comparing Multiple Portfolios",
       x = "", y = "Portfolio Value",
       color = "Portfolio",
       legend.position = "none") +
  geom_smooth(method = "loess") +
  theme_tq() +
  scale_color_tq() +
  theme() +
  scale_y_continuous(labels = scales::dollar)
