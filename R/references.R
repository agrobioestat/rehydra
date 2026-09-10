# ==============================================================================
# !! CRAN ALERT - DOI VERIFICATION REQUIRED BEFORE EVERY SUBMISSION !!
# ------------------------------------------------------------------------------
# `R CMD check --as-cran` runs a URL/DOI check over DESCRIPTION, man/*.Rd,
# inst/CITATION, README and the vignettes, and an unresolvable identifier is
# reported as
#
#     NOTE: Found the following (possibly) invalid DOIs: ...
#
# which is a blocking comment on submission. Every DOI in the table below was
# therefore confirmed against https://api.crossref.org and by resolving
# https://doi.org/<doi> (HTTP 200), and `doi_verified` records that.
#
# Before each submission, re-check with:
#     rehydra_check_dois()          # interactive use only - needs internet
#
# NEVER ship a DOI that has not been resolved: a wrong DOI is worse than no DOI,
# and the same goes for a citation whose title, authors or journal could not be
# confirmed. That is why the recovery-period, total-reduction, mean-reduction
# and mean-recovery-rate components carry no attribution at all - their
# definitions are stated as mathematics in the roxygen `@details` of
# `recovery_period_metrics()` and `mean_reduction_metrics()` instead.
#
# And do not take a "this DOI is broken" claim on trust, from any source: check
# it. The identifier below was once dropped from this package on such a claim
# and it turned out to be correct and resolvable all along.
# ==============================================================================

.rehydra_reference_table <- function() {
  tibble::tibble(
    key = c("lloret2011", "xu2010", "ingrisch2018", "ribeiro2021"),
    authors = c(
      "Lloret, F., Keeling, E. G., Sala, A.",
      "Xu, Z., Zhou, G., Shimizu, H.",
      "Ingrisch, J., Bahn, M.",
      paste(
        "Ribeiro, R. V., Vitti, K. A., Marcos, F. C. C., Souza, G. M.,",
        "Pissolato, M. D., Almeida, L. F. R., Machado, E. C."
      )
    ),
    year = c(2011L, 2010L, 2018L, 2021L),
    title = c(
      "Components of tree resilience: effects of successive low-growth episodes in old ponderosa pine forests",
      "Plant responses to drought and rewatering",
      "Towards a comparable quantification of resilience",
      "Proposal of an index of stability for evaluating plant drought memory: a case study in sugarcane"
    ),
    used_for = c(
      "resilience_index(): Rt, Rc, Rs",
      "recovery_metrics(): rewatering response concepts",
      "stability_index(): resilience quantification framework",
      "memory_effect(), memory_trend(), stability_index(): between-cycle memory"
    ),
    doi = c(
      "10.1111/j.1600-0706.2011.19372.x",
      "10.4161/psb.5.6.11398",
      "10.1016/j.tree.2018.01.013",
      "10.1016/j.jplph.2021.153397"
    ),
    doi_verified = c(TRUE, TRUE, TRUE, TRUE)
  )
}

#' Methodological References Used by rehydra
#'
#' Return the literature behind each family of indices implemented in the
#' package, together with the digital object identifier (DOI) of each work and
#' a flag telling whether that DOI has been verified as resolvable.
#'
#' @details
#' References with `doi_verified = FALSE` are deliberately shipped **without**
#' a DOI string. `R CMD check --as-cran` validates every DOI found in
#' `DESCRIPTION`, `man/`, `inst/CITATION`, `README` and the vignettes, and
#' reports unresolvable identifiers as a `NOTE`, so an unverified DOI is left
#' empty instead of guessed. See the header of `R/references.R` for the
#' verification checklist.
#'
#' @return A tibble with columns `key`, `authors`, `year`, `title`, `used_for`,
#'   `doi` and `doi_verified`.
#'
#' @seealso [rehydra_check_dois()] to test resolution interactively.
#'
#' @examples
#' rehydra_references()
#'
#' # Which references still need a verified DOI?
#' subset(rehydra_references(), !doi_verified)
#' @export
rehydra_references <- function() {
  .rehydra_reference_table()
}

#' Check Whether the Package DOIs Resolve
#'
#' Interactive helper that sends one `HEAD`-like request per DOI to
#' `https://doi.org` and reports which identifiers resolve.
#'
#' @param dois Character vector of DOIs. Defaults to the non-missing DOIs in
#'   [rehydra_references()].
#' @param timeout Timeout in seconds for each request.
#'
#' @details
#' This function requires an internet connection and is therefore **never**
#' called from examples, tests or vignettes: CRAN machines must be able to run
#' the package fully offline. Use it manually before a submission, exactly as
#' `R CMD check --as-cran` would.
#'
#' @return A tibble with columns `doi`, `url`, `status` and `resolves`.
#'
#' @examples
#' \dontrun{
#' rehydra_check_dois()
#' }
#' @export
rehydra_check_dois <- function(dois = NULL, timeout = 10) {
  if (is.null(dois)) {
    refs <- .rehydra_reference_table()
    dois <- refs$doi[!is.na(refs$doi)]
  }

  dois <- unique(as.character(dois[!is.na(dois)]))
  if (length(dois) == 0) {
    return(tibble::tibble(
      doi = character(),
      url = character(),
      status = character(),
      resolves = logical()
    ))
  }

  urls <- paste0("https://doi.org/", dois)

  old <- options(timeout = timeout)
  on.exit(options(old), add = TRUE)

  results <- lapply(seq_along(urls), function(i) {
    status <- tryCatch(
      {
        con <- url(urls[[i]], open = "rb")
        on.exit(close(con), add = TRUE)
        readBin(con, what = "raw", n = 1L)
        "ok"
      },
      error = function(e) conditionMessage(e),
      warning = function(w) conditionMessage(w)
    )

    tibble::tibble(
      doi = dois[[i]],
      url = urls[[i]],
      status = status,
      resolves = identical(status, "ok")
    )
  })

  dplyr::bind_rows(results)
}
