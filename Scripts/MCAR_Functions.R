# Function for deriving indices of adjacent neighbors of every single voxel location for 
# spatial MCAR prior (a voxel location is considered adjacent if they share the same face).
adjacency_matrix = function(dim1, dim2, dim3){
  
  # Inputs:
  # - dim1: dimension1 of 3D MRI scan
  # - dim2: dimension2 of 3D MRI scan
  # - dim3: dimension3 of 3D MRI scan
  
  # 2d
  if(missing(dim3)){
    A = data.frame(x = integer(),y = integer())
    ind = 1:(dim1*dim2)
    conv = as.vector(matrix(1:(dim1*dim2), dim1,dim2, byrow = T)) # order more important for 3d
    
    # gives neighbours of 2d
    for (i in 1:(dim1 * dim2)){
      up = i - dim2
      down = i + dim2
      left = i - 1
      right = i + 1
      if (up > 0){
        A = rbind(A, c(i, up))
      }
      if (down <= (dim1 * dim2)){
        A = rbind(A, c(i, down))
      }
      if (left %% dim2 != 0){
        A = rbind(A, c(i, left))
      }
      if (i %% dim2 != 0){
        A = rbind(A, c(i, right))
      }
    }
    colnames(A) = c('x', 'y')
    
    Ax = numeric(length(A$x))
    Ay = numeric(length(A$y))
    # puts neighbours in column major order
    for(i in 1:length(A$x)){
      Ax[i] = ind[which(conv == A$x[i], arr.ind = T)]
      Ay[i] = ind[which(conv == A$y[i], arr.ind = T)]
    }
    A$x = Ax
    A$y = Ay
  } else{
    A_2D = data.frame(x = integer(), y = integer())
    ind = 1:(dim1*dim2*dim3)
    conv = as.vector(aperm(array(1:(dim1*dim2*dim3), dim = c(dim2, dim1, dim3)), perm = c(2, 1, 3)))
    
    # gives horizontal neighbours of 2d
    for (i in 1:(dim1 * dim2)){
      up = i - dim2
      down = i + dim2
      left = i - 1
      right = i + 1
      if (up > 0){
        A_2D = rbind(A_2D, c(i, up))
      }
      if (down <= (dim1 * dim2)){
        A_2D = rbind(A_2D, c(i, down))
      }
      if (left %% dim2 != 0){
        A_2D = rbind(A_2D, c(i, left))
      }
      if (i %% dim2 != 0){
        A_2D = rbind(A_2D, c(i, right))
      }
    }
    
    colnames(A_2D) = c('x', 'y')
    A = data.frame(x = integer(), y = integer())
    # adds horizontal neighbours of all layers in dataframe A
    for (k in 0:(dim3-1)) {
      A = rbind(A, (A_2D + (k*dim1*dim2)))
    }
    
    # adds vertical neighbours of all layers at end of dataframe A
    for(i in 1:(dim1*dim2*dim3)){
      bottom = i - dim1*dim2
      top = i + dim1*dim2
      if(bottom > 0){
        A = rbind(A, c(i, bottom))
      }
      if(top <= (dim1*dim2*dim3)){
        A = rbind(A, c(i, top))
      }
    }
    
    # puts neighbours in column major order
    Ax = numeric(length(A$x))
    Ay = numeric(length(A$y))
    for(i in 1:length(A$x)){
      Ax[i] = ind[conv == A$x[i]]
      Ay[i] = ind[conv == A$y[i]]
    }
    A$x = Ax
    A$y = Ay
  }
  return(A)
}

# Function for deriving number of neighbors of every single voxel location for spatial MCAR prior.
n_neighbors = function(dim1, dim2, dim3){
  
  # Inputs:
  # - dim1: dimension1 of 3D MRI scan
  # - dim2: dimension2 of 3D MRI scan
  # - dim3: dimension3 of 3D MRI scan
  
  if(missing(dim3)){
    if (dim1 < 3 | dim2 < 3){ 
      stop("Image dimensions need to be greater than 2!")
    }
    n_sj = matrix(4, nrow = dim1, ncol = dim2)
    n_sj[1,1] = n_sj[1,dim2] = n_sj[dim1,1] = n_sj[dim1,dim2] = 2
    n_sj[2:(dim1-1),1] = n_sj[2:(dim1-1),dim2] = n_sj[1,2:(dim2-1)] = n_sj[dim1,2:(dim2-1)] = 3
    n_sj = as.vector(n_sj)
  } else{
    if (dim1 < 3 | dim2 < 3 | dim3 < 3){ 
      stop("Image dimensions need to be greater than 2!")
    }
    n_sj = array(6, c(dim1, dim2, dim3))
    n_sj[1,1,1] = n_sj[1,dim2,1] = n_sj[dim1,1,1] = n_sj[dim1,dim2,1] = n_sj[1,1,dim3] = n_sj[1,dim2,dim3] = n_sj[dim1,1,dim3] = n_sj[dim1,dim2,dim3] = 3
    n_sj[2:(dim1-1),1,1] = n_sj[2:(dim1-1),dim2,1] = n_sj[dim1,2:(dim2-1),1] = n_sj[1,2:(dim2-1),1]  = 4
    n_sj[2:(dim1-1),1,dim3] = n_sj[2:(dim1-1),dim2,dim3] = n_sj[dim1,2:(dim2-1),dim3] = n_sj[1,2:(dim2-1),dim3] = 4
    n_sj[1,1,2:(dim3-1)] = n_sj[1,dim2,2:(dim3-1)] = n_sj[dim1,1,2:(dim3-1)] = n_sj[dim1,dim2,2:(dim3-1)] = 4
    n_sj[2:(dim1-1), 2:(dim2-1),1] = n_sj[2:(dim1-1), 2:(dim2-1),dim3] = n_sj[1, 2:(dim2-1),2:(dim3-1)] = 5
    n_sj[dim1, 2:(dim2-1),2:(dim3-1)] = n_sj[2:(dim1-1), 1,2:(dim3-1)] = n_sj[2:(dim1-1), dim2,2:(dim3-1)] = 5
    n_sj = as.vector(n_sj)
  }
  return(n_sj)
  
}

# Function to acquire indices of upper triangular of adjacency matrix.
upper_triangular = function(A, p){
  
  # Inputs:
  # - A: spatial adjacency matrix
  # - p: number of total voxels
  
  A = A[order(A$x),]
  UT = data.frame(x = integer(), y = integer())
  K = dim(A)[1]
  for(k in 1:K){
    idx = A[k,]
    if(idx[1] < idx[2]){
      UT = rbind(UT, idx)
    }
  }
  return(UT)
}