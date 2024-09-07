# List of datasets to generate and their descriptions
data_generators <- list(
  list(name = "3x3-strong", generator = function() generate_data(n, dim1, dim2, q, 3, corr = "strong", print_factors = print_factors)),
  list(name = "3x3-moderate", generator = function() generate_data(n, dim1, dim2, q, 3, corr = "moderate", print_factors = print_factors)),
  list(name = "3x3-weak", generator = function() generate_data(n, dim1, dim2, q, 3, corr = "weak", print_factors = print_factors)),
  
  list(name = "5x5-strong", generator = function() generate_data(n, dim1, dim2, q, 5, corr = "strong", print_factors = print_factors)),
  list(name = "5x5-moderate", generator = function() generate_data(n, dim1, dim2, q, 5, corr = "moderate", print_factors = print_factors)),
  list(name = "5x5-weak", generator = function() generate_data(n, dim1, dim2, q, 5, corr = "weak", print_factors = print_factors)),
  
  list(name = "overlap2-small-strong", generator = function() generate_data_2overlap(n, dim1, dim2, q, overlap = "small", corr = "strong", print_factors = print_factors)),
  list(name = "overlap2-small-moderate", generator = function() generate_data_2overlap(n, dim1, dim2, q, overlap = "small", corr = "moderate", print_factors = print_factors)),
  list(name = "overlap2-small-weak", generator = function() generate_data_2overlap(n, dim1, dim2, q, overlap = "small", corr = "weak", print_factors = print_factors)),
  
  list(name = "overlap2-large-strong", generator = function() generate_data_2overlap(n, dim1, dim2, q, overlap = "big", corr = "strong", print_factors = print_factors)),
  list(name = "overlap2-large-moderate", generator = function() generate_data_2overlap(n, dim1, dim2, q, overlap = "big", corr = "moderate", print_factors = print_factors)),
  list(name = "overlap2-large-weak", generator = function() generate_data_2overlap(n, dim1, dim2, q, overlap = "big", corr = "weak", print_factors = print_factors)),
  
  list(name = "overlap3-small-strong", generator = function() generate_data_3overlap(n, dim1, dim2, q, overlap = "small", corr = "strong", print_factors = print_factors)),
  list(name = "overlap3-small-moderate", generator = function() generate_data_3overlap(n, dim1, dim2, q, overlap = "small", corr = "moderate", print_factors = print_factors)),
  list(name = "overlap3-small-weak", generator = function() generate_data_3overlap(n, dim1, dim2, q, overlap = "small", corr = "weak", print_factors = print_factors)),
  
  list(name = "overlap3-large-strong", generator = function() generate_data_3overlap(n, dim1, dim2, q, overlap = "big", corr = "strong", print_factors = print_factors)),
  list(name = "overlap3-large-moderate", generator = function() generate_data_3overlap(n, dim1, dim2, q, overlap = "big", corr = "moderate", print_factors = print_factors)),
  list(name = "overlap3-large-weak", generator = function() generate_data_3overlap(n, dim1, dim2, q, overlap = "big", corr = "weak", print_factors = print_factors))
)

# List of datasets to generate and their descriptions for the three functions
data_generators <- list(
  
  # Simple structure
  list(name = "simple-strong", generator = function() generate_data_simple(n, dim1, dim2, q, corr = "strong", print_factors = print_factors)),
  list(name = "simple-moderate", generator = function() generate_data_simple(n, dim1, dim2, q, corr = "moderate", print_factors = print_factors)),
  list(name = "simple-weak", generator = function() generate_data_simple(n, dim1, dim2, q, corr = "weak", print_factors = print_factors)),
  
  # Medium structure
  list(name = "medium-strong", generator = function() generate_data_medium(n, dim1, dim2, q, corr = "strong", print_factors = print_factors)),
  list(name = "medium-moderate", generator = function() generate_data_medium(n, dim1, dim2, q, corr = "moderate", print_factors = print_factors)),
  list(name = "medium-weak", generator = function() generate_data_medium(n, dim1, dim2, q, corr = "weak", print_factors = print_factors)),
  
  # Hard structure
  list(name = "hard-strong", generator = function() generate_data_hard(n, dim1, dim2, q, corr = "strong", print_factors = print_factors)),
  list(name = "hard-moderate", generator = function() generate_data_hard(n, dim1, dim2, q, corr = "moderate", print_factors = print_factors)),
  list(name = "hard-weak", generator = function() generate_data_hard(n, dim1, dim2, q, corr = "weak", print_factors = print_factors))
)

