#'
#'
#' @param N An integer. Sample size.
#' @param m_arms An integer. Number of Z arms.
#' @param means A numeric vector of length \code{m_arms}.  Average outcome in each Z arm.
#' @param sds A positive numeric vector of length \code{m_arms}. Standard deviations for each of the Z arms.
#' @param conditions A character vector of length \code{m_arms}. The names of each Z arm.
#' @param fixed A named list. Arguments to be fixed in design.
#' @return A function that returns a design.
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept experiment
#' @concept multiarm trial
#' @import DeclareDesign stats utils fabricatr estimatr randomizr rlang
#' @export
#' @examples
#'
#' # To make a design using default arguments:
#' design <- multi_arm_designer()
#'
#'
#' # A design with different mean and sd in each arm
#' design <- multi_arm_designer(means = c(0, 0.5, 2), sd =  c(1, 0.1, 0.5))
#'
# A design with fixed sds and means. N is the sole modifiable argument.
#' design <- multi_arm_designer(N = 80, m_arms = 4, means = 1:4,
#'                              fixed = list(m_arms = 4, sds = rep(1, 4),
#'                                           means = 1:4))
#'

multi_arm_designer <- function(
  N = 30, 
  m_arms = 3, 
  means = rep(0, m_arms),
  sds = rep(1, m_arms),
  conds = 1:m_arms,
  fixed = NULL
){
  Y_treatment_1 <- NULL
  treatment <- NULL
  # Housekeeping
  sds2 <- sds; means2 <- means; N2 <- N
  if(m_arms <= 1 || round(m_arms)!=m_arms) stop("`m_arms' should be an integer greater than one.")
  if(length(means) != m_arms || length(sds) != m_arms || length(conds) != m_arms) stop("`means', `sds` and `conds' arguments must be the of length m_arms .")
  if(any(sds<=0)) stop("`sds' should be positive.")
  if(!"sds" %in% names(fixed))  sds2 <-  sapply(1:m_arms,function(i) expr(sds[!!i])) 
  if(!"means" %in% names(fixed)){  means2 <-  sapply(1:m_arms,function(i) expr(means[!!i])) }
  if(!"N" %in% names(fixed))  N2 <- expr(N)

 

  # Create helper vars to be used in design

  us <- sapply(1:m_arms, function(x) rlang::quos(rnorm(!!N2, 0, !!!sds2[x])))
  names(us) <-  paste0("u_", 1:m_arms)
  
  pop <- rlang::expr(declare_population(N = !!N2, !!!us))
  
  f_Y <- formula(paste0(
    "Y ~ ", paste0("(", means2," + u_", 1:m_arms, ")*( treatment == '", conds, "')", collapse = " + ")))
  
  pos <- rlang::expr(declare_potential_outcomes(formula = !!f_Y, conditions = as.factor(conds), assignment_variables = "treatment"))
  
  Z <- rlang::expr(declare_assignment(num_arms = !!m_arms, conditions = as.factor(conds), assignment_variable = "treatment"))
  
  vars <- paste0("Y_treatment_", 2:m_arms)
  vars <- sapply(1:(m_arms - 1), function(x){ rlang::quos(mean((!!rlang::sym(vars[x] )))) })
  names(vars) <-paste0("treatment" , 2:m_arms)
  
  mand  <- rlang::expr(declare_estimand('(Intercept)' = mean(Y_treatment_1), !!!vars ,  term = TRUE))
 

  {{{   

       # Model
       population <-  rlang::eval_bare(pop)
    
       potential_outcomes <-  rlang::eval_bare(pos)
        
       # Inquiry 
       estimand  <-  rlang::eval_bare(mand)
       
       # Design
       assignment <-  rlang::eval_bare(Z)
    
       reveal <-  declare_reveal(assignment_variables	= treatment)
  
       # Answer
       estimator <-  declare_estimator( Y ~ treatment , model = lm_robust, term = TRUE)
   
       multi_arm_design <- population + potential_outcomes + assignment + reveal + estimand +  estimator

  }}}
  

   design_code <-
      construct_design_code( multi_arm_designer, match.call.defaults(), arguments_as_values = TRUE, exclude_args = c("m_arms", fixed, "fixed"))
   
    
   # Rlang funcions to be evaluated ! - will change it
   design_code <- gsub("rlang::eval_bare\\(pop\\)", rlang::quo_text(pop), design_code)
   design_code <- gsub("rlang::eval_bare\\(mand\\)", rlang::quo_text(mand), design_code)
   design_code <- gsub("rlang::eval_bare\\(pos\\)", rlang::quo_text(pos), design_code)
   design_code <- gsub("rlang::eval_bare\\(Z\\)", rlang::quo_text(Z), design_code)
   
   # Change
   
   
   #  Add  code plus argments as attributes
   attr( multi_arm_design, "code") <-   design_code 
    

  # Return design
  return( multi_arm_design)
}

attr( multi_arm_designer, "shiny_arguments") <- list(N = c(10, 20, 50)) 

attr( multi_arm_designer, "tips") <-
  list(
    N = "Sample Size"
  )


