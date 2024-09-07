rm(list = ls())
# Hard ####
# Matrix 1: Circle in top-left, larger triangle in bottom-right
matrix1 <- matrix(0, nrow=20, ncol=20)

# Draw a circle in the top-left
for (i in 1:20) {
  for (j in 1:20) {
    if (sqrt((i-6)^2 + (j-6)^2) <= 5) {  # Circle with radius 5 centered at (6, 6)
      matrix1[i, j] <- 1
    }
  }
}

# Draw a larger triangle in the bottom-right (make it wider by extending the left bound)
height <- 8
for (i in 12:20) {
  left_bound <- 12  # Moved 1 column to the left (was 13)
  right_bound <- 20 - (20 - i)
  matrix1[i, left_bound:right_bound] <- 1
}

# Matrix 2: Enlarged square in top-center, larger diamond in bottom-left, extended vertical rectangle
matrix2 <- matrix(0, nrow=20, ncol=20)

# Draw an enlarged square (rectangle) in the top center
matrix2[2:7, 7:13] <- 1  # Enlarged top-center square

# Draw a larger diamond in the bottom-left
for (i in 13:20) {
  offset <- abs(16 - i)
  matrix2[i, (5 - offset):(5 + offset)] <- 1
}

# Draw a longer vertical rectangle on the right side
matrix2[3:18, 16:18] <- 1  # Extended vertical rectangle

# Matrix 3: Larger rotated T-shape (moved left and up) and heart moved up by 1 row
matrix3 <- matrix(0, nrow=20, ncol=20)

# Move the T-shape 1 column to the left and 1 row up
matrix3[1:16, 4:7] <- 1  # Vertical part of the T (moved left and up)
matrix3[7:10, 4:17] <- 1   # Horizontal part of the T (moved left and up)

# Draw a larger heart, moved up by 1 row
for (i in 12:19) {  # Moved up by 1 row
  for (j in 11:20) {
    if (((i-16)^2)/2 + ((j-12)^2)/3 <= 1 || ((i-16)^2)/2 + ((j-19)^2)/3 <= 1 || (i >= 16 && (j-16)^2 + (i-16)^2 <= 10)) {  # Heart shape equation moved up
      matrix3[i, j] <- 1
    }
  }
}
matrix3[14, c(12, 19)] <- 1
matrix3[20, 16] <- 1

# Print the matrices
print(matrix1)
print(matrix2)
print(matrix3)



stop()
# Medium ####
# Create a 20x20 binary matrix for the circle, moving it 2 rows lower and 1 column to the right
matrix1 <- matrix(0, nrow=20, ncol=20)
for (i in 1:20) {
  for (j in 1:20) {
    if (sqrt((i-11)^2 + (j-7)^2) <= 6) {  # Moved down by 2 rows and right by 1 column
      matrix1[i, j] <- 1  # Circle with radius 6
    }
  }
}

# Create a 20x20 binary matrix for an equilateral triangle, moving it 2 columns to the right
matrix2 <- matrix(0, nrow=20, ncol=20)
height <- 8  # Height of the equilateral triangle
center <- 12 # Horizontal center of the triangle (shifted 2 columns to the right)

# Adjust the position by starting lower in the matrix
for (i in 18:(18 - height)) {
  # Calculate the width at each level for an equilateral triangle
  left_bound <- center - (18 - i)
  right_bound <- center + (18 - i)
  
  # Fill in the triangle row
  matrix2[i, left_bound:right_bound] <- 1
}

# Create a 20x20 binary matrix for the L-shape like in the new drawing
matrix3 <- matrix(0, nrow=20, ncol=20)
matrix3[2:13, 6:9] <- 1  # Vertical part of the "nested" L
matrix3[2:6, 7:19] <- 1  # Horizontal part of the "nested" L

# Print the matrices
print(matrix1)
print(matrix2)
print(matrix3)

# Simple ####
# Create the first 20x20 binary matrix with a 7x7 block in the top-left corner
matrix1 <- matrix(0, nrow=20, ncol=20)
matrix1[1:7, 1:7] <- 1  # Top-left block of 1's (7x7)

# Create the second 20x20 binary matrix with a 6x6 block in the center, no overlap
matrix2 <- matrix(0, nrow=20, ncol=20)
matrix2[8:13, 8:13] <- 1  # Center block of 1's (6x6)

# Create the third 20x20 binary matrix with a 7x7 block in the bottom-right corner, no overlap
matrix3 <- matrix(0, nrow=20, ncol=20)
matrix3[14:20, 14:20] <- 1  # Bottom-right block of 1's (7x7)

# Print the matrices
print(matrix1)
print(matrix2)
print(matrix3)
