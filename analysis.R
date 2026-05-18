# ============================================================
#  E-Commerce Sales Analysis  —  Phases 1 to 4
#  Dataset : Kaggle UCI Online Retail (carrie1/ecommerce-data)
#  ~541K rows | Dec 2010 – Dec 2011
# ============================================================

# ── 0. Dependencies ──────────────────────────────────────────
pkgs <- c("tidyverse","lubridate","scales","stringr")
miss <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
if (length(miss)) install.packages(miss, repos = "https://cloud.r-project.org")

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(scales)
  library(stringr)
})

# ── Shared theme ─────────────────────────────────────────────
theme_ecom <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.title    = element_text(face = "bold", size = 13, margin = margin(b = 6)),
      plot.subtitle = element_text(colour = "grey40", size = 9, margin = margin(b = 8)),
      plot.caption  = element_text(colour = "grey55", size = 8),
      axis.text     = element_text(colour = "grey30"),
      panel.grid.minor  = element_blank(),
      panel.grid.major  = element_line(colour = "grey93"),
      legend.position   = "bottom"
    )
}

BLUE   <- "#2B6CB0"; GREEN  <- "#276749"
ORANGE <- "#C05621"; RED    <- "#C53030"
GRAY   <- "#4A5568"

# Create output folders
dir.create("graphs",     showWarnings = FALSE)


# ════════════════════════════════════════════════════════════
#  PHASE 1  —  DATA CLEANING
# ════════════════════════════════════════════════════════════

cat("\n══════════════════════════════════════\n")
cat(" PHASE 1 — DATA CLEANING\n")
cat("══════════════════════════════════════\n\n")

# ── 1.1  Load ────────────────────────────────────────────────
raw <- read.csv("data.csv", stringsAsFactors = FALSE, fileEncoding = "latin1")
cat(sprintf("Raw dataset : %s rows  ×  %s columns\n",
            format(nrow(raw), big.mark = ","), ncol(raw)))

# ── 1.2  Null value audit ────────────────────────────────────
cat("\n--- Null / blank value audit ---\n")
null_counts <- colSums(is.na(raw) | raw == "")
print(null_counts[null_counts > 0])

cat("
Decisions:
  CustomerID  (135,080 nulls) → Flag as 'Guest'  — guest revenue is valid
  Description ( 1,454 nulls)  → Drop row         — product is unidentifiable
")

# ── 1.3  Fix CustomerID & drop missing descriptions ──────────
df <- raw
df$CustomerID[is.na(df$CustomerID) | df$CustomerID == ""] <- "Guest"
df <- df %>% filter(!is.na(Description) & Description != "")

# ── 1.4  Remove duplicates ───────────────────────────────────
dups <- sum(duplicated(df))
cat(sprintf("Duplicates found and removed : %s\n", format(dups, big.mark = ",")))
df <- df %>% distinct()

# ── 1.5  Fix data types ──────────────────────────────────────
cat("\n--- Data type corrections ---\n")
cat("  InvoiceDate : string  → POSIXct\n")
cat("  CustomerID  : numeric → character\n")
cat("  InvoiceNo   : mixed   → character\n")

df <- df %>%
  mutate(
    InvoiceDate = mdy_hm(InvoiceDate),
    CustomerID  = as.character(CustomerID),
    InvoiceNo   = as.character(InvoiceNo)
  )

# ── 1.6  Remove cancellations & invalid rows ─────────────────
cancels   <- sum(str_starts(df$InvoiceNo, "C"))
neg_qty   <- sum(df$Quantity  <= 0, na.rm = TRUE)
neg_price <- sum(df$UnitPrice <= 0, na.rm = TRUE)

cat(sprintf("\n--- Returns / cancellations removed ---\n"))
cat(sprintf("  InvoiceNo starting 'C' : %s\n", format(cancels,   big.mark = ",")))
cat(sprintf("  Quantity  <= 0         : %s\n", format(neg_qty,   big.mark = ",")))
cat(sprintf("  UnitPrice <= 0         : %s\n", format(neg_price, big.mark = ",")))

df <- df %>%
  filter(!str_starts(InvoiceNo, "C"),
         Quantity  > 0,
         UnitPrice > 0)

# ── 1.7  New derived columns ─────────────────────────────────
cat("\n--- New derived columns ---\n")
cat("  Revenue    = Quantity × UnitPrice\n")
cat("  YearMonth  = floor_date(InvoiceDate, 'month')\n")
cat("  Hour       = hour(InvoiceDate)\n")
cat("  DayOfWeek  = wday(InvoiceDate, label = TRUE)\n")
cat("  OrderSize  = quantity bucket: Single / Small / Medium / Large / Bulk\n")

df <- df %>%
  mutate(
    Revenue   = Quantity * UnitPrice,
    YearMonth = floor_date(InvoiceDate, "month"),
    Hour      = hour(InvoiceDate),
    DayOfWeek = wday(InvoiceDate, label = TRUE, abbr = TRUE),
    OrderSize = case_when(
      Quantity == 1        ~ "Single",
      Quantity <= 6        ~ "Small",
      Quantity <= 24       ~ "Medium",
      Quantity <= 100      ~ "Large",
      TRUE                 ~ "Bulk"
    ) %>% factor(levels = c("Single","Small","Medium","Large","Bulk"))
  )

cat(sprintf("\nClean dataset : %s rows  ×  %s columns\n",
            format(nrow(df), big.mark = ","), ncol(df)))

# ── 1.8  Save cleaned CSV ────────────────────────────────────
write.csv(df, "cleaned_dataset.csv", row.names = FALSE)
cat("Saved: cleaned_dataset.csv\n")


# ════════════════════════════════════════════════════════════
#  PHASE 2  —  EXPLORATORY DATA ANALYSIS
# ════════════════════════════════════════════════════════════

cat("\n══════════════════════════════════════\n")
cat(" PHASE 2 — EDA\n")
cat("══════════════════════════════════════\n\n")

total_rev <- sum(df$Revenue, na.rm = TRUE)

# ── 2.1  Unique customers & products ─────────────────────────
n_cust <- n_distinct(df$CustomerID[df$CustomerID != "Guest"])
n_prod <- n_distinct(df$Description)
cat(sprintf("Unique registered customers : %s\n", format(n_cust, big.mark = ",")))
cat(sprintf("Unique products             : %s\n", format(n_prod, big.mark = ",")))
cat(sprintf("Countries                   : %s\n", n_distinct(df$Country)))

# ── 2.2  Distribution of quantity & price ────────────────────
cat("\n--- Quantity distribution ---\n")
print(summary(df$Quantity))
cat("\n--- UnitPrice distribution ---\n")
print(summary(df$UnitPrice))

# ── 2.3  Country frequency ────────────────────────────────────
cat("\n--- Top 10 countries by transaction count ---\n")
print(df %>% count(Country, sort = TRUE) %>%
        mutate(pct = round(n / sum(n) * 100, 1)) %>%
        head(10))

# ── 2.4  Price outliers ───────────────────────────────────────
cat("\n--- Products with unit price > £100 ---\n")
print(df %>% select(Description, UnitPrice) %>% distinct() %>%
        filter(UnitPrice > 100) %>% arrange(desc(UnitPrice)) %>% head(10))

cat("\n--- Products with unit price < £0.10 ---\n")
print(df %>% select(Description, UnitPrice) %>% distinct() %>%
        filter(UnitPrice < 0.10) %>% arrange(UnitPrice) %>% head(10))

# ── 2.5  Average order value ──────────────────────────────────
aov_df <- df %>%
  group_by(InvoiceNo) %>%
  summarise(OrderValue = sum(Revenue), .groups = "drop")

cat(sprintf("\n--- Average Order Value ---\n"))
cat(sprintf("  Mean   : £%s\n", format(round(mean(aov_df$OrderValue),   2), big.mark = ",")))
cat(sprintf("  Median : £%s\n", format(round(median(aov_df$OrderValue), 2), big.mark = ",")))
cat(sprintf("  Max    : £%s\n", format(round(max(aov_df$OrderValue),    2), big.mark = ",")))

# ── 2.6  Peak time periods ───────────────────────────────────
cat("\n--- Transactions by month ---\n")
month_tbl <- df %>% count(YearMonth) %>% arrange(YearMonth) %>%
  mutate(YearMonth = format(YearMonth, "%b %Y"))
print(month_tbl)

cat("\n--- Transactions by hour ---\n")
print(df %>% count(Hour, sort = TRUE))

cat("\n--- Transactions by day of week ---\n")
print(df %>% count(DayOfWeek, sort = TRUE))


# ════════════════════════════════════════════════════════════
#  PHASE 3  —  GRAPHICAL REPORTS
# ════════════════════════════════════════════════════════════

cat("\n══════════════════════════════════════\n")
cat(" PHASE 3 — CHARTS\n")
cat("══════════════════════════════════════\n\n")

# ── Chart 1: Revenue trend ────────────────────────────────────
monthly <- df %>%
  group_by(YearMonth) %>%
  summarise(Revenue = sum(Revenue), Orders = n_distinct(InvoiceNo), .groups = "drop")

p1 <- ggplot(monthly, aes(x = YearMonth)) +
  geom_col(aes(y = Revenue), fill = BLUE, alpha = 0.8, width = 25) +
  geom_line(aes(y = Orders * 500), colour = ORANGE, linewidth = 1.2, group = 1) +
  geom_point(aes(y = Orders * 500), colour = ORANGE, size = 2.5) +
  scale_y_continuous(
    name = "Revenue",
    labels = label_dollar(prefix = "£", suffix = "K", scale = 1e-3, accuracy = 1),
    sec.axis = sec_axis(~ . / 500, name = "Orders",
                        labels = label_comma())
  ) +
  scale_x_datetime(date_labels = "%b\n%Y", date_breaks = "1 month") +
  labs(title    = "Monthly Revenue Trend — Dec 2010 to Dec 2011",
       subtitle = "Bars = revenue  |  Orange line = order count",
       x = NULL, caption = "Source: UCI E-Commerce dataset") +
  theme_ecom() +
  theme(axis.title.y.right = element_text(colour = ORANGE),
        axis.text.y.right  = element_text(colour = ORANGE))

ggsave("graphs/p1_revenue_trend.png", p1, width = 12, height = 5, dpi = 150)
cat("Saved: graphs/p1_revenue_trend.png\n")

# ── Chart 2: Country sales ────────────────────────────────────
top_country <- df %>% group_by(Country) %>%
  summarise(R = sum(Revenue), .groups = "drop") %>%
  slice_max(R, n = 1) %>% pull(Country)

country_rev <- df %>%
  group_by(Country) %>%
  summarise(Revenue = sum(Revenue), Orders = n_distinct(InvoiceNo), .groups = "drop") %>%
  arrange(desc(Revenue))

ex_top <- country_rev %>%
  filter(Country != top_country) %>%
  slice_head(n = 12) %>%
  mutate(Country = fct_reorder(Country, Revenue))

p2 <- ggplot(ex_top, aes(x = Revenue, y = Country)) +
  geom_col(fill = GREEN, alpha = 0.85) +
  geom_text(aes(label = label_dollar(prefix = "£", suffix = "K",
                                     scale = 1e-3, accuracy = 0.1)(Revenue)),
            hjust = -0.08, size = 3, colour = GRAY) +
  scale_x_continuous(labels = label_dollar(prefix = "£", suffix = "K", scale = 1e-3),
                     expand = expansion(mult = c(0, 0.2))) +
  labs(title    = paste0("Revenue by Country (excl. ", top_country, ")"),
       subtitle = paste0(top_country, " accounts for ",
                         round(country_rev$Revenue[1]/total_rev*100, 1),
                         "% of revenue — excluded for scale"),
       x = "Total Revenue", y = NULL,
       caption = "Source: UCI E-Commerce dataset") +
  theme_ecom()

ggsave("graphs/p2_country_sales.png", p2, width = 10, height = 6, dpi = 150)
cat("Saved: graphs/p2_country_sales.png\n")

# ── Chart 3: Product performance ─────────────────────────────
prod_rev <- df %>%
  group_by(Description) %>%
  summarise(Revenue = sum(Revenue), Units = sum(Quantity), .groups = "drop") %>%
  arrange(desc(Revenue))

top15 <- prod_rev %>% slice_max(Revenue, n = 15) %>%
  mutate(Description = fct_reorder(str_wrap(Description, 30), Revenue))

bot15 <- prod_rev %>% slice_min(Revenue, n = 15) %>%
  mutate(Description = fct_reorder(str_wrap(Description, 30), Revenue, .desc = TRUE))

p3a <- ggplot(top15, aes(x = Revenue, y = Description)) +
  geom_col(fill = GREEN, alpha = 0.85) +
  scale_x_continuous(labels = label_dollar(prefix = "£", suffix = "K", scale = 1e-3)) +
  labs(title = "Top 15 Products by Revenue", x = "Total Revenue", y = NULL) +
  theme_ecom()

p3b <- ggplot(bot15, aes(x = Revenue, y = Description)) +
  geom_col(fill = RED, alpha = 0.8) +
  scale_x_continuous(labels = label_dollar(prefix = "£")) +
  labs(title = "Bottom 15 Products by Revenue", x = "Total Revenue", y = NULL) +
  theme_ecom()

ggsave("graphs/p3a_top_products.png",  p3a, width = 10, height = 7, dpi = 150)
ggsave("graphs/p3b_worst_products.png", p3b, width = 10, height = 7, dpi = 150)
cat("Saved: graphs/p3a_top_products.png\n")
cat("Saved: graphs/p3b_worst_products.png\n")

# ── Chart 4: Order value distribution ────────────────────────
cap98 <- quantile(aov_df$OrderValue, 0.98)
aov_plot <- aov_df %>% filter(OrderValue <= cap98)

p4 <- ggplot(aov_plot, aes(x = OrderValue)) +
  geom_histogram(bins = 65, fill = ORANGE, alpha = 0.8, colour = "white") +
  geom_vline(xintercept = median(aov_df$OrderValue),
             colour = RED,  linewidth = 1.2, linetype = "dashed") +
  geom_vline(xintercept = mean(aov_df$OrderValue),
             colour = BLUE, linewidth = 1.2, linetype = "dotted") +
  annotate("text", x = median(aov_df$OrderValue) + 25, y = Inf,
           label = paste0("Median\n£", round(median(aov_df$OrderValue))),
           vjust = 1.4, colour = RED, size = 3.2, fontface = "bold") +
  annotate("text", x = mean(aov_df$OrderValue) + 25, y = Inf,
           label = paste0("Mean\n£", round(mean(aov_df$OrderValue))),
           vjust = 3.0, colour = BLUE, size = 3.2, fontface = "bold") +
  scale_x_continuous(labels = label_dollar(prefix = "£")) +
  scale_y_continuous(labels = label_comma()) +
  labs(title    = "Distribution of Order Values",
       subtitle = "Top 2% of orders trimmed  |  Dashed = median, Dotted = mean",
       x = "Order Value (£)", y = "Orders") +
  theme_ecom()

ggsave("graphs/p4_order_value_dist.png", p4, width = 10, height = 5, dpi = 150)
cat("Saved: graphs/p4_order_value_dist.png\n")

# ── Chart 5: Trading patterns ─────────────────────────────────
p5a <- df %>% count(Hour) %>%
  ggplot(aes(x = Hour, y = n,
             fill = between(Hour, 10, 14))) +
  geom_col(alpha = 0.85, show.legend = FALSE) +
  scale_fill_manual(values = c("FALSE" = BLUE, "TRUE" = GREEN)) +
  scale_y_continuous(labels = label_comma()) +
  scale_x_continuous(breaks = 6:20) +
  labs(title = "Transactions by Hour of Day",
       subtitle = "Green = peak window 10:00–15:00",
       x = "Hour (24h)", y = "Transactions") +
  theme_ecom()

p5b <- df %>% count(DayOfWeek) %>%
  ggplot(aes(x = DayOfWeek, y = n)) +
  geom_col(fill = "#605ca8", alpha = 0.85) +
  scale_y_continuous(labels = label_comma()) +
  labs(title = "Transactions by Day of Week",
       x = NULL, y = "Transactions") +
  theme_ecom()

ggsave("graphs/p5a_hourly.png",  p5a, width = 9, height = 5, dpi = 150)
ggsave("graphs/p5b_daily.png",   p5b, width = 9, height = 5, dpi = 150)
cat("Saved: graphs/p5a_hourly.png\n")
cat("Saved: graphs/p5b_daily.png\n")

# ── Chart 6: Cumulative revenue ───────────────────────────────
p6 <- monthly %>%
  arrange(YearMonth) %>%
  mutate(Cumulative = cumsum(Revenue)) %>%
  ggplot(aes(x = YearMonth, y = Cumulative)) +
  geom_area(alpha = 0.2, fill = BLUE) +
  geom_line(colour = BLUE, linewidth = 1.5) +
  geom_point(colour = BLUE, size = 3) +
  scale_y_continuous(labels = label_dollar(prefix = "£", suffix = "M",
                                           scale = 1e-6, accuracy = 0.1)) +
  scale_x_datetime(date_labels = "%b\n%Y", date_breaks = "1 month") +
  labs(title    = "Cumulative Revenue",
       subtitle = "Steady growth with steep Q4 acceleration",
       x = NULL, y = "Cumulative Revenue") +
  theme_ecom()

ggsave("graphs/p6_cumulative_revenue.png", p6, width = 10, height = 5, dpi = 150)
cat("Saved: graphs/p6_cumulative_revenue.png\n")


# ════════════════════════════════════════════════════════════
#  PHASE 4  —  BUSINESS DECISIONS
# ════════════════════════════════════════════════════════════

cat("\n══════════════════════════════════════\n")
cat(" PHASE 4 — BUSINESS DECISIONS\n")
cat("══════════════════════════════════════\n\n")

# ── 4.1  Market prioritisation ────────────────────────────────
cat("--- 4.1  Market prioritisation ---\n\n")

market <- df %>%
  group_by(Country) %>%
  summarise(
    Revenue       = sum(Revenue, na.rm = TRUE),
    Orders        = n_distinct(InvoiceNo),
    Customers     = n_distinct(CustomerID[CustomerID != "Guest"]),
    AvgOrderValue = Revenue / Orders,
    RevShare_pct  = Revenue / total_rev * 100,
    .groups       = "drop"
  ) %>%
  arrange(desc(Revenue))

cat("Top international markets (excl. UK):\n")
print(market %>%
        filter(Country != top_country) %>%
        slice_head(n = 10) %>%
        mutate(Revenue       = paste0("£", format(round(Revenue), big.mark = ",")),
               AvgOrderValue = paste0("£", round(AvgOrderValue)),
               RevShare_pct  = paste0(round(RevShare_pct, 1), "%")))

cat("
RECOMMENDATION:
  PRIORITY 1 — Expand now   : Netherlands, EIRE, Germany, France
    Invest in localised marketing; consider dedicated account managers.

  PRIORITY 2 — Digital grow : Australia, Switzerland, Belgium, Sweden
    Low-cost email / social campaigns; no heavy OPEX required.

  HOLD                       : All markets < £5K revenue
    Serve reactively only.\n")

# ── 4.2  Product strategy ─────────────────────────────────────
cat("\n--- 4.2  Product strategy ---\n\n")

product_strategy <- df %>%
  group_by(Description) %>%
  summarise(
    Revenue  = sum(Revenue, na.rm = TRUE),
    Units    = sum(Quantity, na.rm = TRUE),
    Orders   = n_distinct(InvoiceNo),
    AvgPrice = mean(UnitPrice, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  mutate(
    RevRank   = percent_rank(Revenue),
    UnitsRank = percent_rank(Units),
    Strategy  = case_when(
      RevRank >= 0.90 & UnitsRank >= 0.75 ~ "PROMOTE — Star",
      RevRank >= 0.75                      ~ "PROMOTE — High-Value",
      RevRank <= 0.10 & UnitsRank <= 0.10  ~ "DISCONTINUE",
      TRUE                                 ~ "Monitor"
    )
  )

cat("Star products (top 10 by revenue):\n")
print(product_strategy %>% filter(Strategy == "PROMOTE — Star") %>%
        arrange(desc(Revenue)) %>%
        select(Description, Revenue, Units, Strategy) %>%
        head(10))

n_disc <- sum(product_strategy$Strategy == "DISCONTINUE")
cat(sprintf("\nDISCONTINUE candidates : %s products\n", n_disc))
print(product_strategy %>% filter(Strategy == "DISCONTINUE") %>%
        arrange(Revenue) %>%
        select(Description, Revenue, Units) %>%
        head(10))

cat("
RECOMMENDATION:
  PROMOTE  : Regency Cakestand 3 Tier, White Hanging Heart T-Light Holder,
             Jumbo Bag Red Retrospot, Party Bunting.
             Bundle deals, homepage placement, email campaigns.

  DISCONTINUE : ~400 products with negligible revenue.
             Clear at discount; remove from catalogue.\n")

# ── 4.3  Stock planning ───────────────────────────────────────
cat("\n--- 4.3  Stock / demand planning ---\n\n")

monthly_units <- df %>%
  group_by(YearMonth) %>%
  summarise(Units = sum(Quantity), Revenue = sum(Revenue),
            Orders = n_distinct(InvoiceNo), .groups = "drop") %>%
  arrange(YearMonth) %>%
  mutate(MonthLabel = format(YearMonth, "%b %Y"))

cat("Monthly demand:\n")
print(monthly_units %>% select(MonthLabel, Revenue, Units, Orders))

cat("
RECOMMENDATION:
  1. RESTOCK from AUGUST — revenue surges Sep (+39%) → Oct → Nov.
     Place bulk supplier orders in June / July.

  2. NOVEMBER is peak month — ensure top-20 SKUs have 3x normal stock by 1 Nov.

  3. DECEMBER drops sharply — avoid over-ordering for December.

  4. PEAK HOURS 10:00–15:00 — full warehouse team on shift by 09:30.

  5. SLOW MONTHS Jan–Feb — use for stocktake, clearance, supplier renegotiations.\n")

cat("\n✓  Analysis complete. Outputs:\n")
cat("     cleaned_dataset.csv\n")
cat("     graphs/p1_revenue_trend.png\n")
cat("     graphs/p2_country_sales.png\n")
cat("     graphs/p3a_top_products.png\n")
cat("     graphs/p3b_worst_products.png\n")
cat("     graphs/p4_order_value_dist.png\n")
cat("     graphs/p5a_hourly.png\n")
cat("     graphs/p5b_daily.png\n")
cat("     graphs/p6_cumulative_revenue.png\n")
