---
title: "Calculating the VIX (CBOE Volatility Index)"
author: "Sushruth Saseendran Kavuttan"
date: "`r format(Sys.Date(), '%d %B %Y')`"
output:
  pdf_document: 
    toc: true
    number_sections: true
    latex_engine: xelatex
editor_options:
  markdown:
    wrap: 72
---


```{r setup, include=FALSE}
# Load required libraries
library(dplyr)
library(reshape2)
library(ggplot2)
library(lubridate)

# Install TinyTeX if not already installed
if (!tinytex::is_tinytex()) {
  tinytex::install_tinytex()
}
```

Introduction This analysis aims to calculate the VIX (CBOE Volatility
Index) using option data and various formulas. We'll go through the
process step by step, including data preprocessing, option selection,
VIX variance calculation, and finally, VIX calculation.

Data Preparation We begin by loading the required libraries and reading
the option data.

```{r preprocessing, message=FALSE, warning=FALSE}
# Read the option data
option_data <- read.csv("C:/Users/sushr/Downloads/sp500_options_2010.csv", sep = ",")

# Adjust the strike price
option_data <- option_data %>%
  mutate(STRIKEPRICE = STRIKEPRICE / 1000)
```

Option Selection Next, we filter the option data for the desired day

```{r option selection, message=FALSE, warning=FALSE}

# Define the current day
current_day <- "4-Jan-10"
```

We will now filter the dataset to include only data for the specified
current day. Additionally, we'll convert the maturity date to a usable
Date type and calculate the number of days to expiration.

```{r preprocessing2, message=FALSE, warning=FALSE}

# Filter the dataset to only include data for the current day
current_day_data <- option_data %>%
  filter(Time == current_day)

# Convert `MATURITYDATE` to Date type and calculate days to expiration
current_day_data <- current_day_data %>%
  mutate(
    MATURITYDATE = as.Date(as.character(MATURITYDATE), format = "%Y%m%d"),
    DaysToExp = as.numeric(difftime(MATURITYDATE, dmy(current_day), units = "days"))
  )
```

## Filtering Near-Term and Next-Term Contracts

In this step, we will filter the dataset into near-term and next-term
contracts based on the number of days remaining until expiration.

-   **Near-term contracts**: Options expiring more than 23 days and less
    than 30 days.
-   **Next-term contracts**: Options expiring in less than 37 days and
    more than 30 days.

If no near-term contracts are found, we will promote the first available
next-term contracts.

```{r term-filtering}
# Filter for near-term and next-term options following the rolling mechanism
near_term <- current_day_data %>%
  filter(DaysToExp >= 24, DaysToExp <= 30)

next_term <- current_day_data %>%
  filter(DaysToExp > 30, DaysToExp <= 37)

# If no near-term contracts are found, apply fallback
if (nrow(near_term) == 0) {
  sorted_data <- current_day_data %>%
    arrange(MATURITYDATE)
  
  unique_dates <- unique(sorted_data$MATURITYDATE)
  
  # Ensure there are enough dates to correctly roll options
  if (length(unique_dates) >= 2) {
    near_term <- sorted_data %>%
      filter(MATURITYDATE == unique_dates[1])
    
    next_term <- sorted_data %>%
      filter(MATURITYDATE == unique_dates[2])
  } else {
    stop("Not enough data to select near- and next-term contracts.")
  }
}
```

We will now separate the filtered data into calls and puts for both
near-term and next-term contracts.

```{r filtering}

# Separate into calls and puts for near- and next-term contracts
near_term_calls <- near_term %>%
  filter(OPTIONTYPE == "C")

near_term_puts <- near_term %>%
  filter(OPTIONTYPE == "P")

next_term_calls <- next_term %>%
  filter(OPTIONTYPE == "C")

next_term_puts <- next_term %>%
  filter(OPTIONTYPE == "P")
```

## Identifying Strike Prices with Smallest Difference

To calculate the absolute difference between call and put midpoint
prices, we'll use the following steps:

1.  Compute the midpoint of the bid-ask prices for both calls and puts.
2.  Calculate the absolute difference between these midpoints.

We will also identify the strike price with the smallest difference for
both near-term and next-term contracts.

```{r calculate-abs-diff}
# Function to calculate absolute difference between call and put midpoint prices
calculate_abs_diff <- function(calls, puts) {
  merged_data <- merge(
    calls[, c("STRIKEPRICE", "ASK_PRICE", "BID_PRICE","MATURITYDATE","DaysToExp")],
    puts[, c("STRIKEPRICE", "ASK_PRICE", "BID_PRICE","MATURITYDATE","DaysToExp")],
    by = "STRIKEPRICE",
    suffixes = c("_CALL", "_PUT")
  )
  
  # Calculate the midpoint of bid-ask prices for calls and puts
  merged_data <- merged_data %>%
    mutate(
      MID_CALL = (ASK_PRICE_CALL + BID_PRICE_CALL) / 2,
      MID_PUT = (ASK_PRICE_PUT + BID_PRICE_PUT) / 2,
      ABS_DIFF = abs(MID_CALL - MID_PUT)
    )
  
  return(merged_data)
}

# Calculate differences for near-term options
near_term_combined <- calculate_abs_diff(near_term_calls, near_term_puts)

# Find the strike price with the smallest difference for near-term options
min_diff_near <- near_term_combined %>%
  arrange(ABS_DIFF) %>%
  slice(1) %>%
  pull(STRIKEPRICE)

# Calculate differences for next-term options
next_term_combined <- calculate_abs_diff(next_term_calls, next_term_puts)

# Find the strike price with the smallest difference for next-term options
min_diff_next <- next_term_combined %>%
  arrange(ABS_DIFF) %>%
  slice(1) %>%
  pull(STRIKEPRICE)

# Print tables and minimum differences
print("Near-term Options with Absolute Differences")
print(near_term_combined)

print("Next-term Options with Absolute Differences")
print(next_term_combined)

print(paste("Strike price with smallest difference (Near-term):", min_diff_near))
print(paste("Strike price with smallest difference (Next-term):", min_diff_next))
```

##Initially we get the near term rate and next term rate for this I have
1.Determined the exact expiration dates for the near-term and next-term
SPX options. 2.Find T-bill maturities with rates that match or closely
align with those expiration dates. 3.Obtain daily yield rates for these
maturities. 4.Assign the rates to the corresponding options: R1 for
near-term and R2 for next-term.
from-<https://home.treasury.gov/resource-center/data-chart-center/interest-rates/TextView?type=daily_treasury_bill_rates&field_tdr_date_value=2010>

Next Calculating Forward Index Prices

To calculate the forward index prices, we'll use the formula:

$$ F = K + e^{(r \cdot T)} \cdot (C - P) $$

where: - **K** is the strike price, - **r** is the risk-free interest
rate, - **T** is the time to expiration in years, - **C** is the call
option price, and - **P** is the put option price.

This will give us the forward prices for both near-term and next-term
options.

Note:Date is set as Jan 4th 2010 as current date
```{r forward-index-prices}
# Risk-free interest rate  from https://home.treasury.gov
rf_rate_near <- 0.0006
rf_rate_next<-0.0013

# Calculate time to expiration in years for near- and next-term options
current_time <- as.POSIXct("2010-01-04 00:00:00", tz = "UTC")

calculate_T_exp <- function(current_time, expiration_time, is_weekly = FALSE) {
  # Calculate minutes remaining today
  current_hour <- as.numeric(format(current_time, "%H"))
  current_minute <- as.numeric(format(current_time, "%M"))
  minutes_to_midnight <- (24 - current_hour) * 60 - current_minute
  
  # Calculate settlement day minutes
  if (is_weekly) {
    # Until 3:00 p.m. (15:00)
    settlement_minutes <- 15 * 60
  } else {
    # Until 8:30 a.m. (08:30)
    settlement_minutes <- 8 * 60 + 30
  }
  
  # Calculate minutes for other days
  days_between <- as.numeric(difftime(expiration_time, current_time, units = "days")) - 1
  other_days_minutes <- days_between * 24 * 60
  
  # Calculate T_exp
  total_minutes <- minutes_to_midnight + settlement_minutes + other_days_minutes
  T_exp <- total_minutes / 525600  # Total minutes in a year
  
  return(T_exp)
}



# Get the minimum days to expiration from near-term and next-term contracts
days_to_exp_near <- min(near_term$DaysToExp)
days_to_exp_next <- min(next_term$DaysToExp)


# Calculate expiration times
# Standard expiration (near-term): Set the time to 8:30 a.m.
expiration_time_near <- as_datetime(current_time) + days(days_to_exp_near)
expiration_time_near <- update(expiration_time_near, hour = 8, minute = 30, second = 0)

# Weekly expiration (next-term): Set the time to 3:00 p.m.
expiration_time_next <- as_datetime(current_time) + days(days_to_exp_next)
expiration_time_next <- update(expiration_time_next, hour = 15, minute = 0, second = 0)

# Display the calculated expiration times
expiration_time_near
expiration_time_next
# Near-term calculation
T_exp_near <- calculate_T_exp(current_time, expiration_time_near, is_weekly = FALSE)

# Next-term calculation
T_exp_next <- calculate_T_exp(current_time, expiration_time_next, is_weekly = TRUE)



# Near-term calculation
T_exp_near <- calculate_T_exp(current_time, expiration_time_near, is_weekly = FALSE)

# Next-term calculation
T_exp_next <- calculate_T_exp(current_time, expiration_time_next, is_weekly = TRUE)

# Extract the near-term midpoint prices for the strike price with the smallest difference
near_term_midpoints <- near_term_combined %>%
  filter(STRIKEPRICE == min_diff_near) %>%
  select(MID_CALL, MID_PUT) %>%
  as.list()

# Extract the next-term midpoint prices for the strike price with the smallest difference
next_term_midpoints <- next_term_combined %>%
  filter(STRIKEPRICE == min_diff_next) %>%
  select(MID_CALL, MID_PUT) %>%
  as.list()

# Extract the individual midpoint prices
mid_call_near <- near_term_midpoints$MID_CALL
mid_put_near <- near_term_midpoints$MID_PUT
mid_call_next <- next_term_midpoints$MID_CALL
mid_put_next <- next_term_midpoints$MID_PUT

# Calculate the forward prices (F1 for near-term, F2 for next-term)
F1 <- min_diff_near + exp(rf_rate_near * T_exp_near) * (mid_call_near - mid_put_near)
F2 <- min_diff_next + exp(rf_rate_next * T_exp_next) * (mid_call_next - mid_put_next)

# Print the forward index prices
print(paste("Forward index price (Near-term) F1:", F1))
print(paste("Forward index price (Next-term) F2:", F2))
```

## Identifying K0 and Out-of-the-Money Options

To identify the strike prices (K0) immediately below the forward index
prices (F1 and F2), we will: 1. Filter near-term and next-term options
data to find the strike price directly below the forward index level. 2.
Extract out-of-the-money calls and puts based on these K0 values.

```{r identify-k0}
# Determine K0 for near-term options (immediately below F1)
K0_near <- near_term_combined %>%
  filter(STRIKEPRICE <= F1) %>%
  arrange(desc(STRIKEPRICE)) %>%
  slice(1) %>%
  pull(STRIKEPRICE)

# Determine K0 for next-term options (immediately below F2)
K0_next <- next_term_combined %>%
  filter(STRIKEPRICE <= F2) %>%
  arrange(desc(STRIKEPRICE)) %>%
  slice(1) %>%
  pull(STRIKEPRICE)

# Print the K0 values
print(paste("Strike price immediately below the forward index level (Near-term) K0:", K0_near))
print(paste("Strike price immediately below the forward index level (Next-term) K0:", K0_next))
```

Now, let's find the out-of-the-money puts and calls around these K0
values. then stop further inclusion after two consecutive zero-bid
occurrences

```{r identify-k0-ootm-call-put}
# Function to get out-of-the-money puts below a given strike price (K0)
filter_otm_puts_with_stop <- function(puts, K0) {
  # Initialize a flag to stop further inclusion after two consecutive zero-bid occurrences
  zero_count <- 0
  
  # Sort puts by descending strike prices and filter those below K0
  sorted_puts <- puts %>%
    filter(STRIKEPRICE < K0) %>%
    arrange(desc(STRIKEPRICE))
  
  # Initialize an empty data frame to store the filtered puts
  filtered_puts <- data.frame()
  
  # Loop through sorted puts to build the filtered list
  for (i in 1:nrow(sorted_puts)) {
    if (sorted_puts$BID_PRICE[i] > 0) {
      zero_count <- 0  # Reset counter if the current put has a non-zero bid price
      filtered_puts <- rbind(filtered_puts, sorted_puts[i, ])
    } else {
      zero_count <- zero_count + 1
      if (zero_count >= 2) {
        break  # Stop including further puts once two consecutive zero-bid prices are found
      }
    }
  }
  
  return(filtered_puts)
}

# Select out-of-the-money puts below K0 for near-term
near_term_otm_puts <- filter_otm_puts_with_stop(near_term_puts, K0_near)

# Select out-of-the-money puts below K0 for next-term
next_term_otm_puts <- filter_otm_puts_with_stop(next_term_puts, K0_next)

# Print the results
print("Out-of-the-money puts (Near-term) below K0 with consecutive zero-bid stop:")
print(near_term_otm_puts)

print("Out-of-the-money puts (Next-term) below K0 with consecutive zero-bid stop:")
print(next_term_otm_puts)



# Function to get out-of-the-money calls above a given strike price (K0)
filter_otm_calls_with_stop <- function(calls, K0) {
  # Initialize a flag to stop further inclusion after two consecutive zero-bid occurrences
  zero_count <- 0
  
  # Sort calls by ascending strike prices and filter those above K0
  sorted_calls <- calls %>%
    filter(STRIKEPRICE > K0) %>%
    arrange(STRIKEPRICE)
  
  # Initialize an empty data frame to store the filtered calls
  filtered_calls <- data.frame()
  
  # Loop through sorted calls to build the filtered list
  for (i in 1:nrow(sorted_calls)) {
    if (sorted_calls$BID_PRICE[i] > 0) {
      zero_count <- 0  # Reset counter if the current call has a non-zero bid price
      filtered_calls <- rbind(filtered_calls, sorted_calls[i, ])
    } else {
      zero_count <- zero_count + 1
      if (zero_count >= 2) {
        break  # Stop including further calls once two consecutive zero-bid prices are found
      }
    }
  }
  
  return(filtered_calls)
}

# Select out-of-the-money calls above K0 for near-term
near_term_otm_calls <- filter_otm_calls_with_stop(near_term_calls, K0_near)

# Select out-of-the-money calls above K0 for next-term
next_term_otm_calls <- filter_otm_calls_with_stop(next_term_calls, K0_next)

# Print the results
print("Out-of-the-money calls (Near-term) above K0 with consecutive zero-bid stop:")
print(near_term_otm_calls)

print("Out-of-the-money calls (Next-term) above K0 with consecutive zero-bid stop:")
print(next_term_otm_calls)
```

## Visualization of Out-of-the-Money Options

We'll use `ggplot2` to create a line plot showing bid prices against
strike prices for both calls and puts. Vertical dashed lines will
indicate the near-term and next-term K0 values.

```{r plot-otm-options}
# Combine call and put data into a single data frame, marking each by their option type
combined_data <- rbind(near_term_otm_calls, near_term_otm_puts)
# Create the plot
ggplot(combined_data, aes(x = STRIKEPRICE, y = BID_PRICE, color = OPTIONTYPE)) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = K0_near, linetype = "dashed", color = "blue", size = 1.1, label = "K0_near") +
  geom_vline(xintercept = K0_next, linetype = "dashed", color = "red", size = 1.1, label = "K0_next") +
  labs(title = "Bid Price vs Strike Price for Call and Put Options",
       x = "Strike Price",
       y = "Bid Price",
       color = "Option Type") +
  theme_minimal() +
  theme(legend.position = "top")
```

## Calculating VIX Variance

To calculate the VIX variance, we'll use a strict formula that involves

# Formulas Section

The first formula is:

$$
\sigma^2_{1} = \frac{2}{T_1} \sum_{i} \frac{\Delta K_i}{K_i^2} e^{R_{1} T_1} Q(K_i) - \frac{1}{T_1} \left[ \frac{F_{1}}{K_{0}} - 1 \right]^2
$$

The second formula is:

$$
\sigma^2_{2} = \frac{2}{T_2} \sum_{i} \frac{\Delta K_i}{K_i^2} e^{R_{2} T_2} Q(K_i) - \frac{1}{T_2} \left[ \frac{F_{2}}{K_{0}} - 1 \right]^2
$$

summing weighted differences between strike prices and their
corresponding bid prices. The formula also incorporates the time to
expiration and risk-free interest rate.

```{r calculate-vix-variance}
# Function to calculate VIX variance using given formulas and distinct risk-free rates
calculate_vix_variance_strict <- function(combined_data, K0, F, T_exp, term = "near") {
  # Set the appropriate risk-free rate based on the term
  rf_rate_near <- 0.0006
  rf_rate_next <- 0.0013
  
  rf_rate <- if (term == "near") rf_rate_near else rf_rate_next
  
  # Arrange by strike price and calculate Delta_K (interval between strike prices)
  combined_data <- combined_data %>%
    arrange(STRIKEPRICE) %>%
    mutate(
      Delta_K = c(
        diff(STRIKEPRICE) / 2,  # Intermediate intervals
        (last(STRIKEPRICE) - nth(STRIKEPRICE, -2)) / 2  # Last interval calculation
      )
    )
  
  # Summation term calculation
  summation_term <- combined_data %>%
    mutate(
      weight = (Delta_K / (STRIKEPRICE^2)) * exp(rf_rate * T_exp)
    ) %>%
    summarise(
      sum_weighted_Q = sum(weight * BID_PRICE, na.rm = TRUE)
    ) %>%
    pull(sum_weighted_Q)
  
  # Final variance calculation using the strict formula
  variance <- (2 / T_exp) * summation_term - (1 / T_exp) * ((F / K0) - 1)^2
  return(variance)
}


# Example combined data for both near-term calls and puts

# Combine near-term calls and puts into a single data frame
combined_otm_near_data <- rbind(near_term_otm_calls, near_term_otm_puts)

# Combine next-term calls and puts into a single data frame
combined_otm_next_data <- rbind(next_term_otm_calls, next_term_otm_puts)
# Calculate the variances for near-term and next-term
# Near-term variance calculation
near_term_variance <- calculate_vix_variance_strict(combined_otm_near_data, K0_near, F1, T_exp_near, "near")
next_term_variance <- calculate_vix_variance_strict(combined_otm_next_data, K0_next, F2, T_exp_next, "next")


str(combined_otm_near_data)

# Print the results
print(paste("Near-term Variance:", near_term_variance))
print(paste("Next-term Variance:", next_term_variance))
```



## Final VIX Calculation

To obtain the final VIX value, we'll use the following formula:

$$ VIX = 100 \times \sqrt{\left( T1 \times \sigma1 \times \frac{NT2 - N30}{NT2 - NT1} + T2 \times \sigma2 \times \frac{N30 - NT1}{NT2 - NT1} \right) \times \frac{N365}{N30}} $$

-   **T1** and **T2** represent the time to expiration for near- and
    next-term options in minutes,
-   **σ1** and **σ2** are the calculated variances for near- and
    next-term options,
-   **NT1** and **NT2** are the days to expiration of near- and
    next-term options,
-   **N30** and **N365** are the minutes equivalent of 30 days and 365
    days.

```{r calculate-final-vix}
# Constants: Minutes in 30 days and 365 days
N30 <- 43200
N365 <- 525600
MINUTES_PER_DAY <- 1440

# Function to calculate the final VIX value
calculate_final_vix <- function(T1, T2, sigma1, sigma2, NT2, NT1, N30, N365) {
  vix <- 100 * sqrt(
    (T1 * sigma1 * ((NT2 - N30) / (NT2 - NT1)) + T2 * sigma2 * ((N30 - NT1) / (NT2 - NT1))) * (N365 / N30)
  )
  return(vix)
}

NT1<-near_term %>%
  filter(STRIKEPRICE == K0_near) %>%
  pull(DaysToExp)
NT1<- NT1*1440
# Find DaysToExp from K0_next in the next-term data
NT2<-next_term %>%
  filter(STRIKEPRICE == K0_next) %>%
  pull(DaysToExp)
NT2<- NT2*1440

# Convert DaysToExp to minutes for T1 (near-term) and T2 (next-term)
T1 <- T_exp_near
T2 <- T_exp_next

# Print the calculated T1 and T2 values
print(paste("Minutes to settlement (near-term) T1:", T1))
print(paste("Minutes to settlement (next-term) T2:", T2))

# Example variances (replace with actual calculations)
sigma1 <- near_term_variance  # Near-term variance
sigma2 <- next_term_variance  # Next-term variance
N30 <- 43200

# Calculate the final VIX using the provided formula
vix <- calculate_final_vix(T1, T2, sigma1, sigma2,NT1,NT2, N30, N365)

# Print the final VIX value
print(paste("Calculated VIX:", vix))
  
```
