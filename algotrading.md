
## Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by incorporating financial metrics to evaluate its profitability. This exercise simulates a real-world scenario where you, as part of a financial technology team, need to present an improved version of a trading algorithm that not only executes trades but also calculates and reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have been tasked by the Trading Strategies Team to modify your trading algorithm. This modification should include tracking the costs and proceeds of trades to facilitate a deeper evaluation of the algorithm’s profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to include costs, proceeds, and return on investments metrics to assess the profitability of your trading algorithm.

## Objectives

1. **Load and Prepare Data:** Open and run the starter code to create a DataFrame with stock closing data.

2. **Implement Trading Algorithm:** Create a simple trading algorithm based on daily price changes.

3. **Customize Trading Period:** Choose your entry and exit dates.

4. **Report Financial Performance:** Analyze and report the total profit or loss (P/L) and the ROI of the trading strategy.

5. **Implement a Trading Strategy:** Implement a trading strategy and analyze the total updated P/L and ROI. 

6. **Discussion:** Summarise your finding.


## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the "Data Loading" section to generate a DataFrame containing AMD stock closing data. This will serve as the basis for your trading decisions. First, create a data frame named `amd_df` with the given closing prices and corresponding dates. 

```r
# Load data from CSV file
amd_df <- read.csv("AMD.csv")
# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)
amd_df <- amd_df[, c("date", "close")]
```

#### Plotting the Data
Plot the closing prices over time to visualize the price movement.
```r
plot(amd_df$date, amd_df$close,'l')
```

### Step 2: Trading Algorithm
Implement the trading algorithm as per the instructions. You should initialize necessary variables, and loop through the dataframe to execute trades based on the set conditions.

- Initialize Columns: Start by ensuring dataframe has columns 'trade_type', 'costs_proceeds' and 'accumulated_shares'.
- Change the algorithm by modifying the loop to include the cost and proceeds metrics for buys of 100 shares. Make sure that the algorithm checks the following conditions and executes the strategy for each one:
  - If the previous price = 0, set 'trade_type' to 'buy', and set the 'costs_proceeds' column to the current share price multiplied by a `share_size` value of 100. Make sure to take the negative value of the expression so that the cost reflects money leaving an account. Finally, make sure to add the bought shares to an `accumulated_shares` variable.
  - Otherwise, if the price of the current day is less than that of the previous day, set the 'trade_type' to 'buy'. Set the 'costs_proceeds' to the current share price multiplied by a `share_size` value of 100.
  - You will not modify the algorithm for instances where the current day’s price is greater than the previous day’s price or when it is equal to the previous day’s price.
  - If this is the last day of trading, set the 'trade_type' to 'sell'. In this case, also set the 'costs_proceeds' column to the total number in the `accumulated_shares` variable multiplied by the price of the last day.



```r
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

for (i in 1:nrow(amd_df)) {
if (previous_price == 0) {
    amd_df$trade_type[i] <- 'buy'
    amd_df$costs_proceeds[i] <- -amd_df$close[i] * share_size
    accumulated_shares <- accumulated_shares + share_size
  } else if (amd_df$close[i] < previous_price) {
    amd_df$trade_type[i] <- 'buy'
    amd_df$costs_proceeds[i] <- -amd_df$close[i] * share_size
    accumulated_shares <- accumulated_shares + share_size
  }
  
  if (i == nrow(amd_df)) {
    amd_df$trade_type[i] <- 'sell'
    amd_df$costs_proceeds[i] <- accumulated_shares * amd_df$close[i]
    accumulated_shares <- 0
  }
  
  amd_df$accumulated_shares[i] <- accumulated_shares
  previous_price <- amd_df$close[i]
}
```


### Step 3: Customize Trading Period
- Define a trading period you wanted in the past five years 
```r
start_date <- as.Date("2022-05-18")  # May 18, 2022
end_date <- as.Date("2024-05-17")  # May 17, 2024
amd_df <- subset(amd_df, date >= start_date & date <= end_date)
```


### Step 4: Run Your Algorithm and Analyze Results
After running your algorithm, check if the trades were executed as expected. Calculate the total profit or loss and ROI from the trades.

- Total Profit/Loss Calculation: Calculate the total profit or loss from your trades. This should be the sum of all entries in the 'costs_proceeds' column of your dataframe. This column records the financial impact of each trade, reflecting money spent on buys as negative values and money gained from sells as positive values.
- Invested Capital: Calculate the total capital invested. This is equal to the sum of the 'costs_proceeds' values for all 'buy' transactions. Since these entries are negative (representing money spent), you should take the negative sum of these values to reflect the total amount invested.
- ROI Formula: $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$

```r
total_profit_loss <- sum(amd_df$costs_proceeds, na.rm = TRUE)
total_capital_invested <- -sum(amd_df$costs_proceeds[amd_df$trade_type == 'buy'], na.rm = TRUE)
roi <- (total_profit_loss / total_capital_invested) * 100

cat("Total Profit/Loss: $", total_profit_loss, "\n")
cat("Total Capital Invested: $", total_capital_invested, "\n")
cat("ROI: ", roi, "%\n")
print(amd_df)
```

### Step 5: Profit-Taking Strategy or Stop-Loss Mechanisum (Choose 1)
- Option 1: Implement a profit-taking strategy that you sell half of your holdings if the price has increased by a certain percentage (e.g., 20%) from the average purchase price.
- Option 2: Implement a stop-loss mechanism in the trading strategy that you sell half of your holdings if the stock falls by a certain percentage (e.g., 20%) from the average purchase price. You don't need to buy 100 stocks on the days that the stop-loss mechanism is triggered.


```r
# Initialise columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # initialise if needed for tracking
amd_df$average_purchase_price <- NA

# Initialise variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0
total_investment <- 0

for (i in 1:nrow(amd_df)) {
  if (!is.na(amd_df$average_purchase_price[i]) && accumulated_shares > 0 && amd_df$close[i] >= 1.25 * amd_df$average_purchase_price[i]) {
    half_shares_to_sell <- accumulated_shares / 2
    amd_df$trade_type[i] <- 'sell'
    amd_df$costs_proceeds[i] <- half_shares_to_sell * amd_df$close[i]
    accumulated_shares <- accumulated_shares - half_shares_to_sell
    total_investment <- total_investment - (half_shares_to_sell * amd_df$average_purchase_price[i])
  }

  if (previous_price == 0) {
    amd_df$trade_type[i] <- 'buy'
    amd_df$costs_proceeds[i] <- -amd_df$close[i] * share_size
    accumulated_shares <- accumulated_shares + share_size
    total_investment <- total_investment + -amd_df$costs_proceeds[i]
    amd_df$average_purchase_price[i] <- total_investment / accumulated_shares
  } else if (amd_df$close[i] < previous_price) {
    amd_df$trade_type[i] <- 'buy'
    amd_df$costs_proceeds[i] <- -amd_df$close[i] * share_size
    accumulated_shares <- accumulated_shares + share_size
    total_investment <- total_investment + -amd_df$costs_proceeds[i]
    amd_df$average_purchase_price[i] <- total_investment / accumulated_shares
  } else {
    if (i > 1) {
      amd_df$average_purchase_price[i] <- amd_df$average_purchase_price[i - 1]
    } else {
      amd_df$average_purchase_price[i] <- NA
    }
  }
  if (i == nrow(amd_df) && accumulated_shares > 0) {
    amd_df$trade_type[i] <- 'sell'
    amd_df$costs_proceeds[i] <- accumulated_shares * amd_df$close[i]
    accumulated_shares <- 0
  }

  amd_df$accumulated_shares[i] <- accumulated_shares
  previous_price <- amd_df$close[i]
}
```


### Step 6: Summarize Your Findings
- Did your P/L and ROI improve over your chosen period?
- Relate your results to a relevant market event and explain why these outcomes may have occurred.


```r
total_profit_loss_after_profit_taking <- sum(amd_df$costs_proceeds, na.rm = TRUE)
total_capital_invested_after_profit_taking <- -sum(amd_df$costs_proceeds[amd_df$trade_type == 'buy'], na.rm = TRUE)
roi_after_profit_taking <- (total_profit_loss_after_profit_taking / total_capital_invested) * 100

cat("Total Profit/Loss after Profit Taking: $", total_profit_loss_after_profit_taking, "\n")
cat("Total Capital Invested after Profit Taking: $", total_capital_invested_after_profit_taking, "\n")
cat("ROI after Profit Taking: ", roi_after_profit_taking, "%\n")
print(amd_df)
```
#### Discussion:
The ROI in profit taking strategy returns as negative. This is because the half of the shares are sold on March 23 2023, May 16-18, 22-26 2023, June 1, 5-6, 8-12, 14, 27, 29-30 2023, July 3, 10, 12-14, 17, 21, 25, 27-28, 30-31 2023, August 1, 3-4, 7, 14, 23 2023, September 1, 5 2023, November 3, 7-8, 10, 14, 16-17, 20, 22, 27, 29 2023, December 1, 7-8, 11-13, 15, 19, 21, 26-28 2023, January 4-5, 8-9, 16-19, 23-25, 29 2024, February 1-2, 7, 9, 14, 22, 27, 29 2024, March 1, 4, 6-7, 12, 15, 22, 27-28 2024, April 1, 3, 5, 9, 11, 16, 18, 22-23, 25-26, 29 2024 and May 2-3, 6, 14-16. The remaining shares were then sold on May 17 2024. From looking at the plot, it is evident that the stock was on an upward trend following the end of 2022. With the first shares being sold in early 2023 and then shares consistently being sold thereafter, it indicates there is a flaw in this strategy. It seems that as long as half the shares are sold every time that the average purchase price raises by 25%, there are significant losses overall if the stock is rising. In addition, this strtegy does not take into consideration that the average purchse price may remain stagnant over multiple days and thus remaining at the 1.25 mark, meaning an exponential amount of shares my be sold due to the daily halving in some circumstances.

In addition, Forbes indicated that over the course of 2022, the stock dropped 55% while it rose 128% in 2023. Forbes further explains this trend nothing that at the end of 2022 and beginning of 2023, "the PC industry underwent a slump, as the tailwinds such as remote working and learning seen through Covid-19 eased". This explanation implies that the increase of share prices prior to 2022 were driven by the increased demand for PCs as there was greater burden placed on technology due lockdowns put in place as a result in Covid-19. Had the profit taking strategy not been implemented, much higher gains would have been made.

On September 27th 2022, AMD released the Ryzen 7000 series, with the chips having a release dates of February 28th and April 6th 2023. There was a slight rise over the two days following the initial series release in 2022, followed by a greater drop in the days after. This indicates an initial strong belief in the series however it was not sustained. The price however bounced back and rised in the lead up to the first chip release, hitting a period peak in the lead up to the release date on 100.28 on March 23 2023, when half the shares are sold in the profit taking strategy. While this was a peak in that short period, the share price eventually rose to 164.47 at the point that remaining shares are sold. This final higher price was likely in response to AMD announcing that AI PC processors would be available in 2025, on April 16 2024, as part of the Ryzen 8000 series.

While the stock price peaked at other times, even reaching above 200, simply holding out like in the initial strategy and adding 100 shares at a time before selling at the end would have been preferable. If the profit taking strategy was to remain, one potential amendment would be that half the shares are only sold once in line with the first major upturn. In doing so, some profit can remain while still having sufficient shares to gain should the stock rise substantially in the future. Another option could be to have a stop loss mechanism. However, due to the steady decline over 2022 and the exponential loss of shares (halves total each time shares are sold) in comparison to gains (adds 100 each time shares are bought), the total accumulated shares my have reached 0 prior to the stock having the significant upturn in 2023.




