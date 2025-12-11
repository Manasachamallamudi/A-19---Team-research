
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
