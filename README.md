# E-Commerce Sales Analysis

**Dataset:** [UCI Online Retail — Kaggle (carrie1/ecommerce-data)](https://www.kaggle.com/datasets/carrie1/ecommerce-data)  
**Rows:** 541,909 raw → 524,878 after cleaning  
**Period:** December 2010 – December 2011  
**Revenue:** £10.64M across 19,960 orders from 38 countries

---

## Project Structure

```
├── data.csv                  ← raw dataset (download from Kaggle)
├── analysis.R                ← standalone 4-phase R analysis script
├── app.R                     ← interactive Shiny dashboard (all 4 phases)
├── cleaned_dataset.csv       ← output of Phase 1 cleaning
├── report.pdf                ← full 4-phase PDF report
├── README.md                 ← this file
└── graphs/
    ├── p1_revenue_trend.png
    ├── p2_country_sales.png
    ├── p3a_top_products.png
    ├── p3b_worst_products.png
    ├── p4_order_value_dist.png
    ├── p5a_hourly.png
    ├── p5b_daily.png
    └── p6_cumulative_revenue.png
```

---

## Quick Start

### Option A — Interactive Dashboard (recommended)

```r
# Install dependencies (first run only)
install.packages(c("shiny", "shinydashboard", "ggplot2", "dplyr",
                   "plotly", "DT", "lubridate", "scales", "stringr"))

# Launch
shiny::runApp("app.R")
```

Open the browser at `http://127.0.0.1:<port>` and upload `data.csv` via the sidebar.

### Option B — Run the analysis script

```r
# Place data.csv in your working directory, then:
source("analysis.R")
```

This produces `cleaned_dataset.csv` and all charts in `graphs/`.

---

## Dataset Columns

| Column | Type | Description |
|---|---|---|
| InvoiceNo | string | Order identifier; prefix `C` = cancellation |
| StockCode | string | Product code |
| Description | string | Product name |
| Quantity | integer | Units per line (negative = return) |
| InvoiceDate | datetime | Transaction timestamp (`%m/%d/%Y %H:%M`) |
| UnitPrice | float | Price per unit in GBP (£) |
| CustomerID | float | Customer ID (null = guest) |
| Country | string | Customer country |

---

## Phase 1 — Data Cleaning

| Issue | Rows | Action |
|---|---|---|
| Missing CustomerID | 135,080 | Flagged as `"Guest"` — revenue is kept |
| Missing Description | 1,454 | Dropped — product unidentifiable |
| Duplicate rows | 5,268 | Removed |
| Cancellation invoices (`C…`) | 9,288 | Removed — returns, not sales |
| Negative Quantity | 10,624 | Removed |
| Negative UnitPrice | 2,517 | Removed |

**New columns derived:** `Revenue`, `YearMonth`, `Hour`, `DayOfWeek`, `OrderSize`, `Strategy`

---

## Phase 2 — EDA Findings

- **4,338** unique registered customers; **4,026** unique products
- **United Kingdom = 84.6%** of revenue — high concentration risk
- Median order value **£303** (mean £533 — inflated by bulk B2B orders)
- Peak trading window: **10:00–15:00** (~75% of all transactions)
- Busiest days: **Tuesday–Thursday**; quietest: **Sunday**
- Price outliers > £100 are predominantly service/admin lines (DOTCOM POSTAGE, etc.)

---

## Phase 3 — Charts

All charts are saved to `graphs/` at 150 DPI.

| File | Content |
|---|---|
| `p1_revenue_trend.png` | Monthly revenue bars + order count line |
| `p2_country_sales.png` | Top 12 countries excl. UK |
| `p3a_top_products.png` | Top 15 products by revenue |
| `p3b_worst_products.png` | Bottom 15 products by revenue |
| `p4_order_value_dist.png` | Order value histogram with median/mean |
| `p5a_hourly.png` | Transactions by hour (peak highlighted) |
| `p5b_daily.png` | Transactions by day of week |
| `p6_cumulative_revenue.png` | Cumulative revenue area chart |

---

## Phase 4 — Business Decisions

### 4.1 Market Prioritisation

| Priority | Markets | Action |
|---|---|---|
| **Priority 1** | Netherlands, EIRE, Germany, France | Localised marketing + account management |
| **Priority 2** | Australia, Switzerland, Belgium, Sweden | Low-cost digital campaigns |
| **Hold** | All others < £5K revenue | Serve reactively |

### 4.2 Product Strategy

| Classification | Criteria | Action |
|---|---|---|
| **PROMOTE — Star** | Top 10% revenue AND top 25% units | Bundle deals, homepage, email campaigns |
| **PROMOTE — High-Value** | Top 25% revenue | Premium positioning; protect margin |
| **Monitor** | Mid-tier | Flag after 2 consecutive months of decline |
| **DISCONTINUE** | Bottom 10% revenue AND bottom 10% units | Clear stock; remove from catalogue |

Key products to promote: **Regency Cakestand 3 Tier**, **White Hanging Heart T-Light Holder**, **Jumbo Bag Red Retrospot**, **Party Bunting**.

### 4.3 Stock Planning

| Trigger | Action |
|---|---|
| **June / July** | Place bulk orders for Q4 restocking |
| **August** | Build warehouse inventory |
| **September–November** | Revenue surges 39%+ above average; maintain 3× stock on top-20 SKUs |
| **December** | Demand drops sharply post-Christmas — avoid over-ordering |
| **January–February** | Slowest months — stocktake, discontinue-SKU clearance, supplier renegotiations |
| **10:00–15:00 daily** | 75% of orders arrive in this window — full warehouse team by 09:30 |

---

## Dashboard Tabs (app.R)

| Tab | Content |
|---|---|
| Overview | 8 KPI value boxes + revenue trend + top-5 country pie + data quality table |
| Phase 1 · Cleaning | Null audit, cleaning log, duplicate/cancel counts, derived columns |
| Phase 2 · EDA | Customer/product counts, distributions, country frequency, hourly/daily patterns, price outliers |
| Phase 3 · Charts | Revenue trend, country bars & pie, top/bottom products, order value histogram, cumulative revenue |
| Phase 4 · Decisions | Market prioritisation chart + table, product strategy chart + promote/discontinue tables, stock planning chart |
| Data Table | Filterable table by country, product, strategy, and date range |

---

## Dependencies

### R (dashboard + analysis)
```r
shiny, shinydashboard, ggplot2, dplyr, plotly, DT,
lubridate, scales, stringr, tidyverse
```

### Python (report generation)
```
pandas, numpy, matplotlib, reportlab, Pillow
```

---

## Notes

- The file encoding of the raw CSV is **latin-1** (ISO-8859-1). Always specify `fileEncoding = "latin1"` in R or `encoding='latin1'` in Python when reading `data.csv`.
- **DOTCOM POSTAGE**, **MANUAL**, and **POSTAGE** are service/admin line items that appear as high-revenue "products". Exclude them from product promotion decisions.
- The `CustomerID` null decision (flag as Guest rather than drop) retains ~25% of rows that would otherwise be lost. This significantly affects customer count metrics but not revenue totals.
- Maximum recommended upload size for the Shiny dashboard: **100 MB** (set via `options(shiny.maxRequestSize = 100 * 1024^2)`).
