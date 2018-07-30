#' Create a m_arms design
#'
#' This designer creates an \code{m_arms} arm design with equal assignment probabilities accross arms.
#'
#' @param N An integer. Sample size.
#' @param m_arms An integer. Number of treatment arms.
#' @param means A vector of size \code{m_arms}.  Average outcome in each treatment arm.
#' @param sds A double. Standard deviation for all treatment arms.
#' @param fixed A list. List of arguments to be fixed in design.
#' @return A function that returns a design.
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept experiment 
#' @concept multi-trial
#' @import DeclareDesign stats utils fabricatr estimatr randomizr rlang
#' @export
#' @examples
#' 
#' # To make a design using default arguments:
#'  \dontrun{
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
#' 
#' If 'means' or 'sds' are defined in fixed list the same definition has to be  
#'
#' }

multi_arm_designer <- function(
  N = 30, 
  m_arms = 3, 
  means = rep(0, m_arms),
  sds = rep(1, m_arms),
  fixed = NULL
){
  # Housekeeping
  if(length(means) != m_arms || length(sds) != m_arms) stop("`means' and `sds` arguments must be the of length m_arms .")
  if(m_arms <= 1 || round(m_arms)!=m_arms) stop("`m_arms' should be an integer greater than one.")
  if(any(sds<=0)) stop("`sds' should be positive.")
  
  # Create helper vars to be used in desing


  conds <- paste0("T", 1:m_arms)
  
  us <- sapply(1:m_arms, function(x) rlang::quos(rnorm(N, 0, !!x)))
  names(us) <-  paste0("u_", 1:m_arms)
  
  
  pop <- rlang::expr(declare_population(N = N, !!!us))
  
  f_Y <- formula(paste0(
    "Y ~ ", paste0("(", means, " + u_", 1:m_arms, ")*( Z == 'T", 1:m_arms, "')", collapse = " + "))
  )
    
  vars <- paste0("Y_Z_T", 2:m_arms)
  vars <- sapply(1:(m_arms - 1), function(x){ rlang::quos(mean((!!rlang::sym(vars[x] )))) })
  names(vars) <-paste0("ZT" , 2:m_arms)
  
  mand  <- rlang::expr(declare_estimand('(Intercept)' = mean(Y_Z_T1), !!!vars ,  term = TRUE))


  {{{   

    
       # Model
       population <-  rlang::eval_bare(pop)
    
       potential_outcomes <- declare_potential_outcomes(formula = !!f_Y, conditions = !!conds)
        
       # Inquiry 
       estimand  <-  rlang::eval_bare(mand)
       
       # Design
       assignment <-  declare_assignment(num_arms = m_arms)
    
       reveal <-  declare_reveal()
  
       # Answer
       estimator <-  declare_estimator( Y ~ Z , model = lm_robust, term = TRUE)
   
       multi_arm_design <- population + potential_outcomes + assignment + reveal + estimand +  estimator

  }}}
  

   design_code <-
      construct_design_code( multi_arm_designer, match.call.defaults())
   
   # Code
   design_code <- design_code[6:length(design_code)]
    
   # Rlang funcions to be evaluated -- manual!
   design_code <- gsub("rlang::eval_bare\\(pop\\)", rlang::quo_text(pop), design_code)
   design_code <- gsub("rlang::eval_bare\\(mand\\)", rlang::quo_text(mand), design_code)
   design_code <- gsub("!!f_Y", deparse(f_Y, width.cutoff = 500), design_code)
   design_code <- gsub("!!conds", deparse(conds, width.cutoff = 500), design_code)
  attr( multi_arm_design, "code") <-   design_code 
    
  #  Add  code plus argments as attributes

  # Return design
  return( multi_arm_design)
}

attr( multi_arm_designer, "shiny_arguments") <- list(N = c(10, 20, 50)) 

attr( multi_arm_designer, "tips") <-
  list(
    N = "Sample Size"
  )


