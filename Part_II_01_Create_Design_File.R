
#-------------------------------------------------------------------------
# DESIGN FILE
#-------------------------------------------------------------------------

# Create empty list for data storage
A <- list()
B <- list()
C <- list()
D <- list()

# Calculate all possibilities and input into list
for (i in c(1:10)) {

  # Load categories
  Picture <- 1:20
  Valgkreds <- i
  Ære_first <- 0:1
  Ære_1 <- i
  Lokal_Ære_1 <- c(i, NA)
  Stilling_1 <- 1
  Lokal_Stilling_1 <- c(i, NA)
  
  Ære_2 <- i
  Ære_first <- 0:1
  Lokal_Ære_2 <- c(i, NA)
  Stilling_2 <- 2
  Lokal_Stilling_2 <- c(i, NA)
  
  Ære_3 <- i
  Ære_first <- 0:1
  Lokal_Ære_3 <- c(i, NA)
  Stilling_3 <- 3
  Lokal_Stilling_3 <- c(i, NA)
  
  Ære_4 <- i
  Ære_first <- 0:1
  Lokal_Ære_4 <- c(i, NA)
  Stilling_4 <- 4
  Lokal_Stilling_4 <- c(i, NA)
  
  # Expand grid for specific "valgkreds"
  grid_1 <- expand.grid(Picture, Valgkreds, Ære_first, Ære_1, Lokal_Ære_1,
                        Stilling_1, Lokal_Stilling_1)
  
  grid_2 <- expand.grid(Picture, Valgkreds, Ære_first, Ære_2, Lokal_Ære_2,
                        Stilling_2, Lokal_Stilling_2)
  
  grid_3 <- expand.grid(Picture, Valgkreds, Ære_first, Ære_3, Lokal_Ære_3,
                        Stilling_3, Lokal_Stilling_3)
  
  grid_4 <- expand.grid(Picture, Valgkreds, Ære_first, Ære_4, Lokal_Ære_4,
                        Stilling_4, Lokal_Stilling_4)
 
  # Input into list
  A[[i]] <- grid_1
  B[[i]] <- grid_2
  C[[i]] <- grid_3
  D[[i]] <- grid_4
  
}

# Rbind all elements from list
grid_1 <- do.call(rbind, A)
grid_2 <- do.call(rbind, B)
grid_3 <- do.call(rbind, C)
grid_4 <- do.call(rbind, D)

# Rename variables
colnames(grid_1) <- c("Picture", "Valgkreds", "Order", "Credit_1", "Lokal_Credit_1", "Position_1", "Lokal_Position_1")
colnames(grid_2) <- c("Picture", "Valgkreds", "Order", "Credit_2", "Lokal_Credit_2", "Position_2", "Lokal_Position_2")
colnames(grid_3) <- c("Picture", "Valgkreds", "Order", "Credit_3", "Lokal_Credit_3", "Position_3", "Lokal_Position_3")
colnames(grid_4) <- c("Picture", "Valgkreds", "Order", "Credit_4", "Lokal_Credit_4", "Position_4", "Lokal_Position_4")

# Collect in list
experiment_design <- list(grid_1, grid_2, grid_3, grid_4)
names(experiment_design) <- c("Task 1", "Task 2", "Task 3", "Task 4")

# Gem hele dynen
write.xlsx(experiment_design, "../0_Guides/1_VN/Experiment_design.xlsx")
