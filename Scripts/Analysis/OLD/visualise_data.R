source("Scripts/data_generation.R")

# Load necessary libraries
library(ggplot2)
library(reshape2)

# initialisation
set.seed(12)
n <- 200 # Not sure yet
dim1 <- 10
dim2 <- 10
q <- 3

visualize_matrices <- function(matrices) {
  num_slices <- dim(matrices)[3]
  
  # Create a matrix to keep track of the number of overlaps
  overlap_count <- apply(matrices, c(1, 2), sum)  # Count how many times a "1" appears across slices
  
  data_list <- lapply(1:num_slices, function(i) {
    mat_df <- melt(matrices[, , i])
    mat_df$Slice <- paste("Factor", i)
    
    # Add a column to indicate the level of overlap
    mat_df$OverlapCount <- overlap_count[cbind(mat_df$Var1, mat_df$Var2)]
    
    # Modify the fill color based on overlap count
    mat_df$color <- ifelse(mat_df$value == 1 & mat_df$OverlapCount == 3, "#006400",   # Dark green if overlapping with both other matrices
                           ifelse(mat_df$value == 1 & mat_df$OverlapCount == 2, "#228B22",  # Intermediate green for overlap with one matrix
                                  ifelse(mat_df$value == 1, "#98FB98", "white")))  # Light green if no overlap
    
    return(mat_df)
  })
  
  combined_df <- do.call(rbind, data_list)
  
  ggplot(combined_df, aes(x = Var2, y = Var1, fill = color)) +
    geom_tile(color = "black") +
    scale_fill_identity() +  # Use the color directly without mapping
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none",
          panel.grid = element_blank()) +
    scale_y_reverse() +
    facet_wrap(~Slice)
}

# No overlap
data <- generate_data(n, dim1, dim2, q, square_size = 5,
                      print_factors = TRUE)
visualize_matrices(data$B_factors)

# 2 overlap
data_2overlap <- generate_data_2overlap(n, dim1, dim2, q, overlap = "big", 
                                       print_factors = TRUE)
visualize_matrices(data_2overlap$B_factors)

# 3 overlap
data_3overlap <- generate_data_3overlap(n, dim1, dim2, q, overlap = "big", 
                                        print_factors = TRUE)
visualize_matrices(data_3overlap$B_factors)


