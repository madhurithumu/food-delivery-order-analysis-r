# =====================================================================
# Food Delivery Order Analysis and Customer Insights Using R
# ---------------------------------------------------------------------
# HOW TO RUN (RStudio):
#   1. Put this file and food_delivery_data.csv in the same folder.
#   2. Session > Set Working Directory > To Source File Location
#   3. Click "Source" (or Ctrl+Shift+S).  Charts are shown in the Plots
#      pane and also saved as PNG files in a "plots" folder.
# To use your own dataset, replace the CSV file and keep the same
# column names (or edit the names below).
# =====================================================================

# ---- 0. Packages ----------------------------------------------------
pkgs <- c("dplyr", "ggplot2", "lubridate", "scales")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}
dir.create("plots", showWarnings = FALSE)
save_plot <- function(p, name) {
  print(p)
  ggsave(file.path("plots", name), p, width = 8, height = 5, dpi = 150)
}
theme_set(theme_minimal(base_size = 13))
orange <- "#E8590C"

# ---- 1. Load data ---------------------------------------------------
raw <- read.csv("food_delivery_data.csv", na.strings = c("NA", ""),
                stringsAsFactors = FALSE)
cat("\n===== 1. DATA OVERVIEW =====\n")
cat("Rows:", nrow(raw), " Columns:", ncol(raw), "\n")
str(raw)
head(raw)
summary(raw)

# ---- 2. Data cleaning -----------------------------------------------
cat("\n===== 2. DATA CLEANING =====\n")
cat("Missing values per column (raw):\n")
print(colSums(is.na(raw)))

# 2a. Remove duplicate rows
n_before <- nrow(raw)
df <- distinct(raw)
cat("Duplicate rows removed:", n_before - nrow(df), "\n")

# 2b. Fix data types
df$order_date <- as.Date(df$order_date)
df$month      <- month(df$order_date, label = TRUE, abbr = TRUE)
df$weekday    <- wday(df$order_date, label = TRUE, abbr = TRUE)

# 2c. Remove impossible outliers (delivery time over 120 minutes)
n_out <- sum(df$delivery_time_min > 120, na.rm = TRUE)
df <- df %>% filter(is.na(delivery_time_min) | delivery_time_min <= 120)
cat("Outliers removed (delivery > 120 min):", n_out, "\n")

# 2d. Handle missing values
#     order_value and delivery_time_min -> replaced by the median
#     rating -> kept as NA and ignored in rating analysis (na.rm = TRUE)
df$order_value[is.na(df$order_value)] <-
  median(df$order_value, na.rm = TRUE)
df$delivery_time_min[is.na(df$delivery_time_min)] <-
  median(df$delivery_time_min, na.rm = TRUE)
cat("Missing values after cleaning:\n")
print(colSums(is.na(df)))
cat("Clean dataset rows:", nrow(df), "\n")

# ---- 3. Descriptive statistics (mean, median, SD) --------------------
cat("\n===== 3. DESCRIPTIVE STATISTICS =====\n")
desc <- data.frame(
  Variable = c("Order value (Rs)", "Delivery time (min)", "Rating"),
  Mean   = c(mean(df$order_value), mean(df$delivery_time_min),
             mean(df$rating, na.rm = TRUE)),
  Median = c(median(df$order_value), median(df$delivery_time_min),
             median(df$rating, na.rm = TRUE)),
  SD     = c(sd(df$order_value), sd(df$delivery_time_min),
             sd(df$rating, na.rm = TRUE))
)
desc[, -1] <- round(desc[, -1], 2)
print(desc)

# ---- 4. Most ordered food categories ----------------------------------
cat("\n===== 4. FOOD CATEGORIES =====\n")
cat_tab <- df %>% count(food_category, sort = TRUE)
print(cat_tab)
p1 <- ggplot(cat_tab, aes(reorder(food_category, n), n)) +
  geom_col(fill = orange) + coord_flip() +
  geom_text(aes(label = n), hjust = -0.2) +
  labs(title = "Most Ordered Food Categories",
       x = NULL, y = "Number of orders") +
  expand_limits(y = max(cat_tab$n) * 1.1)
save_plot(p1, "1_food_categories.png")

# ---- 5. Orders by area ------------------------------------------------
cat("\n===== 5. ORDERS BY AREA =====\n")
area_tab <- df %>% count(area, sort = TRUE)
print(area_tab)
p2 <- ggplot(area_tab, aes(reorder(area, n), n)) +
  geom_col(fill = "#364FC7") + coord_flip() +
  geom_text(aes(label = n), hjust = -0.2) +
  labs(title = "Orders by Area", x = NULL, y = "Number of orders") +
  expand_limits(y = max(area_tab$n) * 1.1)
save_plot(p2, "2_orders_by_area.png")

# ---- 6. Average order value -------------------------------------------
cat("\n===== 6. AVERAGE ORDER VALUE =====\n")
cat("Overall average order value: Rs", round(mean(df$order_value), 2), "\n")
aov_tab <- df %>% group_by(food_category) %>%
  summarise(avg_order_value = round(mean(order_value), 1),
            orders = n()) %>%
  arrange(desc(avg_order_value))
print(aov_tab)
p3 <- ggplot(aov_tab, aes(reorder(food_category, avg_order_value),
                          avg_order_value)) +
  geom_col(fill = "#2B8A3E") + coord_flip() +
  geom_text(aes(label = avg_order_value), hjust = -0.2) +
  labs(title = "Average Order Value by Category",
       x = NULL, y = "Average order value (Rs)") +
  expand_limits(y = max(aov_tab$avg_order_value) * 1.12)
save_plot(p3, "3_avg_order_value.png")

# ---- 7. Peak ordering hours --------------------------------------------
cat("\n===== 7. PEAK ORDERING HOURS =====\n")
hour_tab <- df %>% count(order_hour)
print(hour_tab %>% arrange(desc(n)) %>% head(5))
p4 <- ggplot(hour_tab, aes(order_hour, n)) +
  geom_col(fill = orange) +
  scale_x_continuous(breaks = 8:23) +
  labs(title = "Orders by Hour of the Day",
       x = "Hour (24h clock)", y = "Number of orders")
save_plot(p4, "4_peak_hours.png")

# ---- 8. Delivery time ----------------------------------------------------
cat("\n===== 8. DELIVERY TIME =====\n")
dt_area <- df %>% group_by(area) %>%
  summarise(avg_delivery = round(mean(delivery_time_min), 1)) %>%
  arrange(avg_delivery)
print(dt_area)
p5 <- ggplot(df, aes(delivery_time_min)) +
  geom_histogram(binwidth = 3, fill = "#7048E8", colour = "white") +
  geom_vline(xintercept = mean(df$delivery_time_min),
             colour = "red", linetype = "dashed") +
  labs(title = "Distribution of Delivery Time",
       subtitle = "Red dashed line = mean",
       x = "Delivery time (minutes)", y = "Orders")
save_plot(p5, "5_delivery_time_hist.png")
p6 <- ggplot(dt_area, aes(reorder(area, avg_delivery), avg_delivery)) +
  geom_col(fill = "#7048E8") + coord_flip() +
  geom_text(aes(label = avg_delivery), hjust = -0.2) +
  labs(title = "Average Delivery Time by Area",
       x = NULL, y = "Minutes") +
  expand_limits(y = max(dt_area$avg_delivery) * 1.1)
save_plot(p6, "6_delivery_time_by_area.png")

# ---- 9. Daily / monthly trends -------------------------------------------
cat("\n===== 9. ORDER TRENDS =====\n")
month_tab <- df %>% count(month)
print(month_tab)
p7 <- ggplot(month_tab, aes(month, n, group = 1)) +
  geom_line(colour = orange, linewidth = 1.2) +
  geom_point(colour = orange, size = 3) +
  labs(title = "Monthly Order Trend (2025)",
       x = NULL, y = "Number of orders")
save_plot(p7, "7_monthly_trend.png")

daily_tab <- df %>% count(order_date)
p8 <- ggplot(daily_tab, aes(order_date, n)) +
  geom_line(colour = "grey60", alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, colour = orange,
              formula = y ~ x) +
  labs(title = "Daily Orders with Trend Line",
       x = NULL, y = "Orders per day")
save_plot(p8, "8_daily_trend.png")

wk_tab <- df %>% count(weekday)
print(wk_tab)

# ---- 10. Payment method preferences ---------------------------------------
cat("\n===== 10. PAYMENT METHODS =====\n")
pay_tab <- df %>% count(payment_method, sort = TRUE) %>%
  mutate(percent = round(100 * n / sum(n), 1))
print(pay_tab)
p9 <- ggplot(pay_tab, aes(x = "", y = n, fill = payment_method)) +
  geom_col(width = 1, colour = "white") +
  coord_polar("y") +
  geom_text(aes(label = paste0(percent, "%")),
            position = position_stack(vjust = 0.5), colour = "white") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Payment Method Preference", fill = NULL) +
  theme_void(base_size = 13)
save_plot(p9, "9_payment_methods.png")

# ---- 11. Ratings vs orders & correlation analysis ---------------------------
cat("\n===== 11. RATINGS vs ORDERS / CORRELATION =====\n")
rest_tab <- df %>% group_by(restaurant_id) %>%
  summarise(orders = n(),
            avg_rating = mean(rating, na.rm = TRUE)) %>%
  filter(!is.na(avg_rating))

# Pearson correlation: restaurant average rating vs number of orders
cor_rest <- cor(rest_tab$avg_rating, rest_tab$orders)
cat("Correlation (restaurant avg rating vs orders):", round(cor_rest, 3), "\n")
print(cor.test(rest_tab$avg_rating, rest_tab$orders))

p10 <- ggplot(rest_tab, aes(avg_rating, orders)) +
  geom_point(colour = orange, size = 3) +
  geom_smooth(method = "lm", se = FALSE, colour = "black",
              formula = y ~ x) +
  labs(title = "Restaurant Rating vs Number of Orders",
       subtitle = paste("Pearson r =", round(cor_rest, 2)),
       x = "Average rating", y = "Number of orders")
save_plot(p10, "10_rating_vs_orders.png")

# Correlation matrix of numeric variables (rating NAs ignored pairwise)
num_vars <- df[, c("order_value", "delivery_time_min", "rating", "order_hour")]
cor_mat <- round(cor(num_vars, use = "complete.obs"), 3)
cat("\nCorrelation matrix:\n")
print(cor_mat)

p11 <- ggplot(df, aes(delivery_time_min, rating)) +
  geom_point(alpha = 0.25, colour = "#364FC7") +
  geom_smooth(method = "lm", colour = "red", formula = y ~ x) +
  labs(title = "Delivery Time vs Rating",
       subtitle = paste("Pearson r =",
                        round(cor(df$delivery_time_min, df$rating,
                                  use = "complete.obs"), 2)),
       x = "Delivery time (min)", y = "Rating")
save_plot(p11, "11_delivery_vs_rating.png")

# ---- 12. Basic statistical test -----------------------------------------------
cat("\n===== 12. STATISTICAL TEST =====\n")
# Do peak hours (12-14 and 19-21) have slower delivery than off-peak?
df$period <- ifelse(df$order_hour %in% c(12, 13, 14, 19, 20, 21),
                    "Peak", "Off-peak")
print(df %>% group_by(period) %>%
        summarise(avg_delivery = round(mean(delivery_time_min), 2),
                  orders = n()))
print(t.test(delivery_time_min ~ period, data = df))

cat("\nDone. Charts saved in the 'plots' folder.\n")
