test_that("rehydra_references documents every index family", {
  refs <- rehydra_references()

  expect_s3_class(refs, "tbl_df")
  expect_true(all(c("key", "authors", "year", "title", "used_for", "doi",
                    "doi_verified") %in% names(refs)))
  expect_setequal(
    refs$key,
    c("lloret2011", "xu2010", "ingrisch2018", "ribeiro2021")
  )
})

test_that("no unverified DOI is shipped anywhere in the package", {
  refs <- rehydra_references()

  # The verification flag is the contract: an entry that has not been confirmed
  # to resolve must carry no DOI string at all, and every entry that does carry
  # one must be flagged verified.
  expect_true(all(is.na(refs$doi[!refs$doi_verified])))
  expect_true(all(!is.na(refs$doi[refs$doi_verified])))

  # Any DOI that reaches DESCRIPTION, the Rd files or the citation must be one
  # of the verified identifiers. This is what stops an unchecked DOI from being
  # reintroduced by an edit somewhere in the documentation.
  verified <- refs$doi[refs$doi_verified]
  root <- system.file(package = "rehydra")

  files <- c(
    file.path(root, "DESCRIPTION"),
    file.path(root, "CITATION"),
    list.files(file.path(root, "help"), full.names = TRUE)
  )
  files <- files[file.exists(files)]

  found <- character()
  for (f in files) {
    txt <- tryCatch(
      paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "
"),
      error = function(e) ""
    )
    found <- c(found, regmatches(
      txt, gregexpr("10[.][0-9]{4,9}/[^[:space:]<>\"',;)]+", txt)
    )[[1]])
  }

  found <- unique(sub("[.,;)]+$", "", found))
  expect_setequal(setdiff(found, verified), character())
})

test_that("the Ribeiro 2021 record matches the published article", {
  refs <- rehydra_references()
  ribeiro <- refs[refs$key == "ribeiro2021", ]

  # This entry was once dropped from the package on an unverified claim that its
  # DOI was broken. It is not: the identifier below is the one registered with
  # CrossRef for Journal of Plant Physiology 260, 153397.
  expect_identical(ribeiro$doi, "10.1016/j.jplph.2021.153397")
  expect_true(ribeiro$doi_verified)
  expect_match(ribeiro$title, "index of stability", fixed = TRUE)
  expect_match(ribeiro$authors, "Ribeiro, R. V.", fixed = TRUE)
  expect_match(ribeiro$authors, "Machado, E. C.", fixed = TRUE)
  expect_equal(ribeiro$year, 2021L)
})

test_that("rehydra_check_dois returns an empty result without contacting the network", {
  out <- rehydra_check_dois(dois = character())

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0L)
  expect_true(all(c("doi", "url", "status", "resolves") %in% names(out)))
})

test_that("the package name is lower case throughout the metadata", {
  desc <- read.dcf(system.file("DESCRIPTION", package = "rehydra"))
  expect_identical(unname(trimws(desc[1, "Package"])), "rehydra")

  # The CRAN feedback asked for "based on"/"following" instead of "inspired by".
  expect_false(grepl("inspired by", desc[1, "Description"], ignore.case = TRUE))
  expect_match(desc[1, "Description"], "based on|following")
  expect_identical(unname(trimws(desc[1, "LazyData"])), "true")
})
