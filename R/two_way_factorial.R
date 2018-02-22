#' @export
two_way_factorial_template <- function(N = c(30, 100, 500, 1000),
                                       beta_A = c(1, -1, 0),
                                       beta_B = c(-1, 0, 1),
                                       beta_AB = c(.5, -1, -.5, 0, 1))
{
  {
    N <- as.numeric(N[1])
    beta_A <- as.numeric(beta_A[1])
    beta_B <- as.numeric(beta_B[1])
    beta_AB <- as.numeric(beta_AB[1])
  }
  {{{
    # Model ------------------------------------------------------------------------
    population <- declare_population(N = N, noise = rnorm(N))

    potential_outcomes <- declare_potential_outcomes(
      Y_Z_T1 = noise,
      Y_Z_T2 = noise + beta_A,
      Y_Z_T3 = noise + beta_B,
      Y_Z_T4 = noise + beta_A + beta_B + beta_AB)

    # Inquiry ----------------------------------------------------------------------
    estimand <- declare_estimand(
      interaction = mean((Y_Z_T4 - Y_Z_T3) - (Y_Z_T2 - Y_Z_T1)),
      label = "interaction")

    # Data Strategy ----------------------------------------------------------------
    assignment <- declare_assignment(num_arms = 4)

    # Answer Strategy --------------------------------------------------------------
    estimator <- declare_estimator(Y ~ A + B + A:B,
                                   model = lm_robust,
                                   coefficient_name = "A:B",
                                   estimand = estimand)

    # Design -----------------------------------------------------------------------
    design <- declare_design(
      population,
      potential_outcomes,
      estimand,
      assignment,
      dplyr::mutate(A = as.numeric(Z %in% c("T2", "T4")),
                    B = as.numeric(Z %in% c("T3", "T4"))),
      reveal_outcomes,
      estimator)
  }}}
  design <- insert_step(design, after=7, declare_citation(
    citation = utils::bibentry(
      bibtype = "Article",
      title = "A factorial experiment in teachers’ written feedback on student homework: Changing teacher behavior a little rather than a lot.",
      author= "Elawar, Maria C., and Lyn Corno.",
      journal= "Journal of educational psychology",
      volume = "77.2",
      year = "1985",
      page = 162)))
  design
}

attr(two_way_factorial_template, "tips") <- c(
  "N" = "Size of population",
  "beta_A" = "Main effect of A",
  "beta_B" = "Main effect of B",
  "beta_AB" = "Interaction effect of A and B"
)

attr(two_way_factorial_template, "description") <- "
<p> A two way factorial design with a size <code>N</code>,
    main effects <code>beta_A</code> and <code>beta_B</code>,
    and interaction <code>beta_AB</code>.

<p> Estimand is the average interaction effect.
"
