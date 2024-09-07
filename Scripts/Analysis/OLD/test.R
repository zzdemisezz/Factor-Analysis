library(xtable)

# Round all numeric values in the summary_table to 3 decimal places
summary_table_rounded <- round(summary_table, 3)

# Convert the row names (conditions) to a column
summary_table_rounded <- cbind(Condition = rownames(summary_table_rounded), summary_table_rounded)

# Create the LaTeX code for the table
latex_table <- xtable(summary_table_rounded, caption = "Summary of Average MSEs for Different Conditions", label = "table:summary_mse")

# Print the LaTeX code to the console, with specified number formatting
print(latex_table, include.rownames = FALSE, sanitize.colnames.function = identity)

library(xtable)

# Convert the data frame to a LaTeX table using xtable
latex_comparison_table <- xtable(comparison_df, caption = "Comparison of Exact Matches Across Methods", label = "table:comparison_exact_matches")

# Print the LaTeX code to the console, with specified formatting
print(latex_comparison_table, include.rownames = FALSE, sanitize.colnames.function = identity)



