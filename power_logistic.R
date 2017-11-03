

######################################################################################
# Logistic Regression - Power function
# Modeled after
#Hsieh, F.Y., Block, D.A., and Larsen, M.D. 1998. 
#'A Simple Method of Sample Size Calculation for Linear and Logistic Regression', 
#'Statistics in Medicine, Volume 17, pages 1623-1634.
# Mimics output created by PASS software
# Author: David Aaby
# Updated: November 3 2017
######################################################################################



# Sample size Calculator for Logistic Regression #
# Can solve for alpha, power, OR, N

# Must input values for P0 and R

LogisticSampleSize <- function(alpha=NULL, power=NULL, P0=NULL, OR=NULL, R=NULL, N=NULL) {
  
  #####     Inputs     #####
  # alpha = alpha level (usually 0.05)
  # power = 1 - Pr(type II error) (usually .80)
  # P0 = baseline probabliity that Y=1
  # OR = odds ratio (odds1 / odds0)
  # R = percent of N with X1=1
  # N = sample size
  

  
  ############################
  # sample size calculations #
  ############################
  
  # solve for alpha level #
  if(is.null(alpha)) {
    beta = 1 - power
    P1 = (P0*OR) / (OR*P0 + 1 - P0)
    Pbar = (1-R)*P0 + R*P1
    
    A1 = sqrt(N*((P0-P1)^2)*(1-R))
    A2 = qnorm(1-beta)*sqrt(P0*(1-P0) + ((P1*(1-P1)*(1-R))/R))
    A3 = sqrt(Pbar*(1-Pbar)/R)
    A4 = (A1 - A2) / A3
    
    alpha = 2*(1-pnorm(A4))
    alpha = round(alpha,3)
    
    P1 = round(P1,3)
    
    # output results #
    results = NULL
    
    if(length(power) > 1 | length(P0) > 1 | length(OR) > 1 | length(R) > 1 | length(N) > 1) {
      rownum = max(length(alpha), length(power), length(P0), length(OR), length(R))
      for(i in 1:rownum) {
        results = cbind(alpha, power, P0, P1, OR, R, N)
      }
    }
    
    else {
      results = c(alpha, power, P0, P1, OR, R, N)
      results = data.frame(results)
      results = t(results)
      colnames(results) = c("alpha", "power", "P0", "P1", "OR", "R" ,"N")
    }
  }
  
  
 
  
  return(results)
}































