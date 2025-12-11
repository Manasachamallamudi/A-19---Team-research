
library(readxl)
library(dplyr)
library(ggplot2)


loans_raw <- read_excel("Student Loan Debt by School 2020-2021.xlsx")
school_debt_us <- loans_raw %>%
  group_by(School, `School Type`) %>%
  summarise(
    total_amount     = sum(`$ of Loans Originated`, na.rm = TRUE),
    total_recipients = sum(Recipients, na.rm = TRUE),
    avg_loan         = total_amount / total_recipients,
    .groups = "drop"
) %>%
  filter(!is.na(avg_loan), total_recipients > 0) %>%
  mutate(
    Type2 = ifelse(`School Type` == "Public", "Public", "Private"),
    Type2 = factor(Type2, levels = c("Public", "Private"))
  )

ggplot(school_debt_us,
aes(x = Type2, y = avg_loan)) +
  geom_boxplot() +
  labs(
    title = "Average Student Loan Amount per Recipient\nby School Type",
    x = "School Type",
    y = "Average Loan per Recipient (USD)"
  ) +
  theme_minimal()

ggplot(school_debt_us,
       aes(x = avg_loan, fill = Type2)) +
