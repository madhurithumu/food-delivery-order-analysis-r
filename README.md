# food-delivery-order-analysis-r
Analysis of Food Delivery Platform(Zomato)
# Food Delivery Order Analysis and Customer Insights Using R

Analysis of 2,015 food delivery orders from eight areas of Hyderabad (2025):
data cleaning, descriptive statistics, trends, correlation analysis and a
peak-hour t-test.

## Dataset
- File: `food_delivery_data.csv` (2,015 rows, 10 columns, one row per order)
- Columns: order_id, order_date, order_hour, area, restaurant_id,
  food_category, payment_method, order_value, delivery_time_min, rating
- Dataset URL: https://raw.githubusercontent.com/<your-username>/food-delivery-order-analysis-r/main/food_delivery_data.csv

## Repository files
| File | Purpose |
|---|---|
| `Food_Delivery_Analysis.Rmd` | R Markdown: documentation, implementation, results |
| `Food_Delivery_Analysis.html` | Knitted output of the Rmd |
| `food_delivery_analysis.R` | Standalone R script (saves charts to `plots/`) |
| `food_delivery_data.csv` | Dataset |
| `conclusion.md` | Conclusion |

## How to run
1. Download all files into one folder (keep the CSV next to the Rmd).
2. Open `Food_Delivery_Analysis.Rmd` in RStudio and click **Knit**.
3. Packages needed: dplyr, ggplot2, lubridate, scales, rmarkdown, knitr.

## Data preparation
15 duplicate rows and 8 outliers (delivery time over 120 min) removed;
missing order values and delivery times replaced by the median.
Clean dataset: 1,992 rows.

## Key results
- Most ordered category: Biryani (26.4% of orders); highest average order value: Pizza (Rs 417)
- Busiest area: Gachibowli (344 orders); slowest delivery: Dilsukhnagar (42.4 min)
- Peak hours: 12:00 and 19:00; UPI is the top payment method (52.5%)
- Restaurant rating vs orders: r = 0.835; delivery time vs rating: r = -0.39
- Peak-hour deliveries are slower: 35.90 vs 32.44 min (Welch t-test, p < 2.2e-16)

## Conclusion
See `conclusion.md`.

## Links
- Git repository: https://github.com/<madhuri-thumu>/food-delivery-order-analysis-r

## Author
[Thumu Madhuri]
