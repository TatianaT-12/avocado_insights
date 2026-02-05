# =========================================================
# Avocado Insights — Seasonality & Regional Dynamics
# Author: Tatiana Thornton
# Purpose: Produce two visuals for a LinkedIn post + README
#   Q1: Philadelphia seasonality by type (AveragePrice by month)
#   Q2: 2017 AveragePrice by month for top-3 regions by Total Bags (conventional)
# =========================================================

# install.packages(c("tidyverse","lubridate","scales","patchwork"))
library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)

# ---- Paths ----
data_path <- "data/avocado.csv"
fig_dir   <- "fig"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# ---- Load & prep ----
avocado <- readr::read_csv(data_path, show_col_types = FALSE) %>%
  mutate(
    Date  = mdy(Date),
    Year  = year(Date),
    Month = month(Date, label = TRUE, abbr = TRUE, locale = "C")
  )

# sanity check
required <- c("Date","AveragePrice","Total Bags","type","region")
stopifnot(all(required %in% names(avocado)))  # stop if any missing

# =========================================================
# Q1) Philadelphia seasonality by type
# =========================================================
phl <- avocado %>%
  filter(region == "Philadelphia") %>%
  select(Date, Year, Month, AveragePrice, type)

# monthly means (simple seasonality signal)
phl_monthly <- phl %>%
  group_by(type, Month) %>%
  summarise(avg_price = mean(AveragePrice, na.rm = TRUE), .groups = "drop")

# boxplot (distribution) + line (monthly mean) — easy for non‑technical readers
p_phl_box <- ggplot(phl, aes(Month, AveragePrice)) +
  geom_boxplot(fill = "#9bd3ae", alpha = 0.6, outlier.alpha = 0.35) +
  facet_wrap(~ type, ncol = 1, scales = "free_y") +
  labs(
    title = "Philadelphia Avocado Prices by Month",
    subtitle = "Weekly prices across years (by type)",
    x = NULL, y = "Average price (USD)"
  ) +
  theme_minimal(base_size = 12)

p_phl_line <- ggplot(phl_monthly, aes(Month, avg_price, group = 1)) +
  geom_line(color = "#1b7f59", linewidth = 1.2) +
  geom_point(color = "#1b7f59", size = 2.2) +
  facet_wrap(~ type, ncol = 1, scales = "free_y") +
  labs(
    title = "Seasonality Signal (Monthly Means)",
    subtitle = "Lower in late winter–spring; higher in late summer–early fall",
    x = NULL, y = "Average price (USD)"
  ) +
  theme_minimal(base_size = 12)

plot_phl <- p_phl_box + p_phl_line + plot_annotation(
  title = "Q1 — Philadelphia Seasonality by Type",
  theme = theme(plot.title = element_text(face = "bold"))
)

ggsave(file.path(fig_dir, "Q1_Philadelphia_Seasonality.png"),
       plot = plot_phl, width = 10, height = 9, dpi = 300)

# =========================================================
# Q2) 2017 AveragePrice by month for top‑3 regions by Total Bags (conventional)
# =========================================================
dat_2017_conv <- avocado %>%
  filter(Year == 2017, type == "conventional")

# Clean restart recommended: Session ▸ Restart R
library(tidyverse)
library(lubridate)


# =========================================================
# Q2 — 2017 AveragePrice by month for top-3 regions
#       (ranked by Total Bags, Type = "conventional")
#       with inline labels (no legend)
# Author: Tatiana Thornton
# =========================================================

# install.packages(c("tidyverse","lubridate","ggrepel"))  # <-- run once if needed
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(ggrepel)
})

# ---- Paths ----
# Use avocado.csv in the same folder, or fallback to data/avocado.csv if present
data_path <- "avocado.csv"
if (!file.exists(data_path) && file.exists("data/avocado.csv")) {
  data_path <- "data/avocado.csv"
}

# Output image (saved to your CURRENT working directory, e.g., BSAN-615/wk_4)
out_file <- "Q2_Top3_2017_Conventional_inline.png"

# ---- Load & parse dates (robust to either MM/DD/YYYY or YYYY-MM-DD) ----
avocado <- readr::read_csv(data_path, show_col_types = FALSE)

Date_try <- mdy(avocado$Date, quiet = TRUE)      # try MM/DD/YYYY
if (all(is.na(Date_try))) Date_try <- ymd(avocado$Date, quiet = TRUE)  # fallback YYYY-MM-DD

avocado <- avocado %>%
  mutate(
    Date     = Date_try,
    Year     = year(Date),
    MonthNum = month(Date)                           # numeric month 1..12 (for sort)
  )

# Basic column checks (will stop with a clear error if something's off)
stopifnot("AveragePrice" %in% names(avocado),
          "Total Bags"   %in% names(avocado),
          "type"         %in% names(avocado),
          "region"       %in% names(avocado))

# ---- Filter to 2017 + conventional ----
dat_2017_conv <- avocado %>%
  filter(Year == 2017, type == "conventional") %>%
  drop_na(Date)

stopifnot(nrow(dat_2017_conv) > 0)

# ---- Identify top-3 regions by Total Bags (2017) ----
top3_regions <- dat_2017_conv %>%
  group_by(region) %>%
  summarise(total_bags_2017 = sum(`Total Bags`, na.rm = TRUE), .groups = "drop") %>%
  slice_max(total_bags_2017, n = 3) %>%
  pull(region)

message("Top-3 regions (2017, conventional): ", paste(top3_regions, collapse = ", "))

# ---- Compute monthly average price for those regions ----
monthly_avg <- dat_2017_conv %>%
  filter(region %in% top3_regions) %>%
  group_by(region, MonthNum) %>%
  summarise(avg_price = mean(AveragePrice, na.rm = TRUE), .groups = "drop") %>%
  mutate(Month = factor(month.abb[MonthNum], levels = month.abb)) %>%
  arrange(region, MonthNum)

# Each region should have multiple months; ensure there are >= 2 points per line
stopifnot(all(dplyr::count(monthly_avg, region)$n >= 2))

# ---- Create inline labels (label the rightmost point of each line) ----
end_labels <- monthly_avg %>%
  group_by(region) %>%
  slice_max(MonthNum, n = 1, with_ties = FALSE) %>%
  ungroup()

# Palette for 3 lines
pal <- setNames(scales::hue_pal()(length(top3_regions)), top3_regions)

# ---- Plot (one panel, direct labels, no legend) ----
p_top3_inline <- ggplot(monthly_avg,
                        aes(x = Month, y = avg_price, color = region, group = region)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = pal) +
  # add right padding so labels don't clip
  scale_x_discrete(expand = expansion(mult = c(0.01, 0.10))) +
  ggrepel::geom_label_repel(
    data = end_labels,
    aes(label = region),
    direction = "y",
    nudge_x = 0.2,
    size = 3.6,
    label.size = 0,
    label.r = unit(0, "pt"),
    segment.color = "grey60",
    segment.size = 0.4,
    min.segment.length = 0,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  labs(
    title = "2017 Average Avocado Price by Month",
    subtitle = "Top-3 regions by Total Bags (Type: conventional) — direct labels for readability",
    x = NULL, y = "Average price (USD)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

# ---- Print & Save ----
print(p_top3_inline)
ggsave(out_file, plot = p_top3_inline, width = 10, height = 6.5, dpi = 300)

# (Optional) Save a PDF for reports
# ggsave(sub(".png$", ".pdf", out_file), plot = p_top3_inline, width = 10, height = 6.5)
