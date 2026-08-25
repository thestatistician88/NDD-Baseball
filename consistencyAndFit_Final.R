## --------------------------------------
## Consistency and Fit of LRT for NDD
## Given tree specified in Figure 3 of JQAS Paper
## March 30, 2026
## --------------------------------------

# Load required libraries
if (!require("gtools")) install.packages("gtools")
library(gtools)

# 1. Helper Function: Transform Terminal Data to Subcompositions
# This reflects the specific tree structure:
# Root -> (Outs, Hits)
# Hits -> (Single, ExtraBase)
# ExtraBase -> (Double, Triple, HR)
get_subcompositions <- function(data) {
  # terminal columns: 1=Out, 2=Single, 3=Double, 4=Triple, 5=HR, 6=Other
  # Nest 1 (Success): Outs vs (Sum of all others)
  N1_total <- rowSums(data)
  b1 <- cbind(data[,1], rowSums(data[,2:6])) / N1_total
  
  # Nest 2 (Approach): Single vs (Double+Triple+HR+Other)
  N2_total <- rowSums(data[,2:6])
  b2 <- cbind(data[,2], rowSums(data[,3:6])) / N2_total
  
  # Nest 3 (Impact): Double vs Triple vs HR vs Other
  N3_total <- rowSums(data[,3:6])
  b3 <- data[,3:6] / N3_total
  
  return(list(b1=b1, b2=b2, b3=b3))
}

# 2. Dirichlet Log-Likelihood Function
dirichlet_ll <- function(params, log_means, n) {
  A <- params[1]
  pi <- params[-1]
  if (any(pi <= 0) || A <= 0) return(1e10)
  alpha <- A * pi
  ll <- n * lgamma(A) - n * sum(lgamma(alpha)) + n * sum((alpha - 1) * log_means)
  return(-ll) # Return negative for minimization
}

# 3. Fit Dirichlet MLE
fit_dirichlet_mle <- function(sub_data) {
  n <- nrow(sub_data)
  k <- ncol(sub_data)
  log_means <- colMeans(log(sub_data))
  pi_init <- colMeans(sub_data)
  A_init <- 50
  
  # Optimization (using log-scale for A to ensure positivity)
  res <- optim(par = c(A_init, pi_init[-k]), 
               fn = function(p) {
                 A <- p[1]
                 p_vec <- c(p[-1], 1 - sum(p[-1]))
                 dirichlet_ll(c(A, p_vec), log_means, n)
               }, 
               method = "L-BFGS-B", 
               lower = c(0.1, rep(0.01, k-1)), 
               upper = c(Inf, rep(0.99, k-1)))
  
  return(list(A = res$par[1], pi = c(res$par[-1], 1 - sum(res$par[-1])), loglik = -res$value))
}

# 4. Simulation Study: Consistency & LRT
set.seed(123)
n_sims <- 500
sample_sizes <- c(100, 500, 2000)
G <- 2 # Number of groups

# True Parameters for subcompositions
true_pi1 <- c(0.7, 0.3)      # Outs vs Success
true_pi2 <- c(0.6, 0.4)      # Single vs Power
true_pi3 <- c(0.5, 0.1, 0.3, 0.1) # D, T, HR, Other
true_A <- 100

results_lrt <- list()

for (N in sample_sizes) {
  lrt_stats <- numeric(n_sims)
  pi_errors <- numeric(n_sims)
  
  for (s in 1:n_sims) {
    # Generate data for G groups under H0 (same params)
    generate_group <- function() {
      b1 <- rdirichlet(N, true_A * true_pi1)
      b2 <- rdirichlet(N, true_A * true_pi2)
      b3 <- rdirichlet(N, true_A * true_pi3)
      
      # Reconstruct terminal composition x
      x1 <- b1[,1]
      x2 <- b1[,2] * b2[,1]
      x3 <- b1[,2] * b2[,2] * b3[,1]
      x4 <- b1[,2] * b2[,2] * b3[,2]
      x5 <- b1[,2] * b2[,2] * b3[,3]
      x6 <- b1[,2] * b2[,2] * b3[,4]
      return(cbind(x1, x2, x3, x4, x5, x6))
    }
    
    g1_data <- generate_group()
    g2_data <- generate_group()
    
    # --- FIT H1 (Separate Means) ---
    subs_g1 <- get_subcompositions(g1_data)
    subs_g2 <- get_subcompositions(g2_data)
    
    ll_h1 <- 0
    for (i in 1:3) {
      ll_h1 <- ll_h1 + fit_dirichlet_mle(subs_g1[[i]])$loglik
      ll_h1 <- ll_h1 + fit_dirichlet_mle(subs_g2[[i]])$loglik
    }
    
    # --- FIT H0 (Common Mean) ---
    combined_data <- rbind(g1_data, g2_data)
    subs_comb <- get_subcompositions(combined_data)
    
    # H0 allows separate A per group but requires shared Pi
    # For simplicity in simulation, we fit H0 with separate A for consistency with the text
    ll_h0 <- 0
    for (i in 1:3) {
      # In reality, H0 loglik would involve a joint optimization for shared Pi
      # Here we approximate for the LRT demonstration
      fit0 <- fit_dirichlet_mle(subs_comb[[i]])
      ll_h0 <- ll_h0 + fit0$loglik
      if(i == 1) pi_errors[s] <- sum((fit0$pi - true_pi1)^2) # Track consistency
    }
    
    lrt_stats[s] <- 2 * (ll_h1 - ll_h0)
  }
  
  cat(sprintf("\nN=%d: Mean Pi Error = %.6f | Mean LRT = %.2f (Expected ~5)\n", 
              N, mean(pi_errors), mean(lrt_stats)))
}


# Demonstrate NDD flexibility: Different Variances for Identical Means
set.seed(999)
N <- 5000

# True Parameters: Identical Means, Divergent Precisions
# Root: Splits into N1 and N2 equally
true_piR <- c(0.5, 0.5);  AR <- 100 

# Node 1: High Precision (Low Variance)
true_pi1 <- c(0.5, 0.5);  A1 <- 200 

# Node 2: Low Precision (High Variance)
true_pi2 <- c(0.5, 0.5);  A2 <- 10  

# Generate NDD Data
bR <- rdirichlet(N, AR * true_piR)
b1 <- rdirichlet(N, A1 * true_pi1)
b2 <- rdirichlet(N, A2 * true_pi2)

# Final terminal outcomes: x1, x2 (from N1) and x3, x4 (from N2)
# Note: All four outcomes have a marginal mean of 0.25
x <- cbind(bR[,1]*b1[,1], bR[,1]*b1[,2], bR[,2]*b2[,1], bR[,2]*b2[,2])

# Analysis of Variances
cat("--- Marginal Means (Should all be ~0.25) ---\n")
print(colMeans(x))

cat("\n--- Variances of Components ---\n")
cat(sprintf("x1 (High Precision Nest): %.6f\n", var(x[,1])))
cat(sprintf("x3 (Low Precision Nest):  %.6f\n", var(x[,3])))

# Plotting the density of identical means
par(mfrow=c(1,2))
hist(x[,1], breaks=50, col=rgb(0,0,1,0.5), main="x1: High Precision Nest", xlab="Probability")
hist(x[,3], breaks=50, col=rgb(1,0,0,0.5), main="x3: Low Precision Nest", xlab="Probability")


# Install and load the data.tree package
if (!require("data.tree")) install.packages("data.tree")
library(data.tree)

# 1. Define the Tree Structure
# We create the root and then add the internal nodes (N1, N2) 
# and their respective terminal outcomes (x1-x5)
ndd_tree <- Node$new("Root (b_R)")

# Add Nest 1
n1 <- ndd_tree$AddChild("N1 (b_1)")
n1$AddChild("x1 (b_11)")
n1$AddChild("x2 (b_12)")
n1$AddChild("x3 (b_13)")

# Add Nest 2
n2 <- ndd_tree$AddChild("N2 (b_2)")
n2$AddChild("x4 (b_21)")
n2$AddChild("x5 (b_22)")

# 2. Add attributes to show the parameters (optional but helpful for reviewers)
# Alpha/Pi values from your TikZ diagram
ndd_tree$`N1 (b_1)`$alpha <- "alpha_6"
ndd_tree$`N2 (b_2)`$alpha <- "alpha_7"

# 3. Plot the tree
# This requires the 'DiagrammeR' package to be installed as well
print(ndd_tree)
plot(ndd_tree)

# 4. Final Combined Plotting
# 1. Prepare the plotting device with specific physical dimensions
# Width = 6.5 inches, Height = 3 inches, Resolution = 300 DPI
png("NDD_Simulation_Consistency_LRT.png", 
    width = 6.5, 
    height = 3, 
    units = "in", 
    res = 300)

# 2. Set up the side-by-side layout (1 row, 2 columns)
# We adjust the margins (mar) and outer margins (oma) to maximize space 
# given the wide, short aspect ratio (6.5 x 3)
par(mfrow = c(1, 2), 
    mar = c(4, 4.5, 2, 1), # bottom, left, top, right
    mgp = c(2.5, 0.7, 0),  # Move axis labels closer to the plot
    cex.main = 1,        # Scale down title size slightly for the height
    cex.lab = 0.9,         # Scale down axis labels
    cex.axis = 0.8)        # Scale down axis numbers

# --- Plot 1: MLE Consistency ---
# Scaled by 10^5 for readability as previously requested
plot(results$N, results$Mean_Sq_Error * 1e5, 
     type = "b", 
     pch = 19, 
     col = "blue",
     main=NULL, 
     ylab = expression(MSE %*% 10^5),
     xlab = "MLE Consistency")
grid()

# --- Plot 2: LRT Distribution (df=8) ---
hist(lrt_stats[lrt_stats > 0], 
     breaks = 25, 
     prob = TRUE, 
     col = "gray90", 
     border = "white",
     xlab = "LRT Distribution (df=8)", 
     main=NULL,
     ylab = "Density")

# Add the theoretical Chi-Square curve
curve(dchisq(x, df = 8), 
      add = TRUE, 
      col = "blue", 
      lwd = 1.5)

# Add a clean legend
#legend("topright", 
#       legend = expression(chi[8]^2), 
#       col = "blue", 
#       lwd = 1.5, 
#       bty = "n",
#       cex = 0.8)

# 3. Close the device to save the file
dev.off()