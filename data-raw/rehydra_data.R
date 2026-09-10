# Generate simulated dataset for rehydra package

set.seed(20260428)

time_days <- 0:40

phase_from_time <- function(t) {
  if (t <= 4) return(c(cycle = 1L, phase = "PreDrought"))
  if (t <= 10) return(c(cycle = 1L, phase = "Drought"))
  if (t <= 15) return(c(cycle = 1L, phase = "Rewatering"))
  if (t <= 20) return(c(cycle = 1L, phase = "PostDrought"))
  if (t <= 24) return(c(cycle = 2L, phase = "PreDrought"))
  if (t <= 30) return(c(cycle = 2L, phase = "Drought"))
  if (t <= 35) return(c(cycle = 2L, phase = "Rewatering"))
  c(cycle = 2L, phase = "PostDrought")
}

simulate_signal <- function(genotype, treatment, t, variable) {
  meta <- phase_from_time(t)
  cyc <- as.integer(meta[["cycle"]])
  phase <- meta[["phase"]]

  if (variable == "stomatal_conductance") {
    if (treatment == "Control") {
      base <- if (genotype == "Tolerant") 0.36 else 0.32
      return(base + 0.01 * sin(t / 6))
    }

    if (genotype == "Sensitive") {
      if (cyc == 1 && phase == "PreDrought") return(0.32)
      if (cyc == 1 && phase == "Drought") return(0.32 - 0.22 * ((t - 4) / 6))
      if (cyc == 1 && phase == "Rewatering") return(0.10 + 0.12 * ((t - 10) / 5))
      if (cyc == 1 && phase == "PostDrought") return(0.22 + 0.02 * ((t - 15) / 5))
      if (cyc == 2 && phase == "PreDrought") return(0.30)
      if (cyc == 2 && phase == "Drought") return(0.30 - 0.21 * ((t - 24) / 6))
      if (cyc == 2 && phase == "Rewatering") return(0.09 + 0.11 * ((t - 30) / 5))
      if (cyc == 2 && phase == "PostDrought") return(0.20 + 0.01 * ((t - 35) / 5))
    }

    if (genotype == "Tolerant") {
      if (cyc == 1 && phase == "PreDrought") return(0.36)
      if (cyc == 1 && phase == "Drought") return(0.36 - 0.18 * ((t - 4) / 6))
      if (cyc == 1 && phase == "Rewatering") return(0.18 + 0.12 * ((t - 10) / 5))
      if (cyc == 1 && phase == "PostDrought") return(0.30 + 0.02 * ((t - 15) / 5))
      if (cyc == 2 && phase == "PreDrought") return(0.34)
      if (cyc == 2 && phase == "Drought") return(0.34 - 0.11 * ((t - 24) / 6))
      if (cyc == 2 && phase == "Rewatering") return(0.23 + 0.11 * ((t - 30) / 5))
      if (cyc == 2 && phase == "PostDrought") return(0.34 + 0.02 * ((t - 35) / 5))
    }
  }

  if (variable == "water_potential") {
    if (treatment == "Control") {
      base <- if (genotype == "Tolerant") -0.85 else -0.92
      return(base + 0.03 * sin(t / 7))
    }

    if (genotype == "Sensitive") {
      if (cyc == 1 && phase == "PreDrought") return(-0.95)
      if (cyc == 1 && phase == "Drought") return(-0.95 - 1.25 * ((t - 4) / 6))
      if (cyc == 1 && phase == "Rewatering") return(-2.20 + 0.80 * ((t - 10) / 5))
      if (cyc == 1 && phase == "PostDrought") return(-1.40 + 0.20 * ((t - 15) / 5))
      if (cyc == 2 && phase == "PreDrought") return(-1.00)
      if (cyc == 2 && phase == "Drought") return(-1.00 - 1.30 * ((t - 24) / 6))
      if (cyc == 2 && phase == "Rewatering") return(-2.30 + 0.70 * ((t - 30) / 5))
      if (cyc == 2 && phase == "PostDrought") return(-1.60 + 0.20 * ((t - 35) / 5))
    }

    if (genotype == "Tolerant") {
      if (cyc == 1 && phase == "PreDrought") return(-0.85)
      if (cyc == 1 && phase == "Drought") return(-0.85 - 0.85 * ((t - 4) / 6))
      if (cyc == 1 && phase == "Rewatering") return(-1.70 + 0.55 * ((t - 10) / 5))
      if (cyc == 1 && phase == "PostDrought") return(-1.15 + 0.15 * ((t - 15) / 5))
      if (cyc == 2 && phase == "PreDrought") return(-0.86)
      if (cyc == 2 && phase == "Drought") return(-0.86 - 0.59 * ((t - 24) / 6))
      if (cyc == 2 && phase == "Rewatering") return(-1.45 + 0.45 * ((t - 30) / 5))
      if (cyc == 2 && phase == "PostDrought") return(-1.00 + 0.05 * ((t - 35) / 5))
    }
  }

  NA_real_
}

genotypes <- c("Sensitive", "Tolerant")
treatments <- c("Control", "Drought")
replicates <- 1:4

rows <- list()
idx <- 1L

for (geno in genotypes) {
  for (trt in treatments) {
    for (rep_id in replicates) {
      pid <- paste0(substr(geno, 1, 1), "_", substr(trt, 1, 1), "_", rep_id)
      for (tt in time_days) {
        meta <- phase_from_time(tt)

        wp <- simulate_signal(geno, trt, tt, "water_potential") + stats::rnorm(1, mean = 0, sd = 0.06)
        gs <- simulate_signal(geno, trt, tt, "stomatal_conductance") + stats::rnorm(1, mean = 0, sd = 0.015)
        gs <- max(gs, 0.02)

        rows[[idx]] <- tibble::tibble(
          plant_id = pid,
          genotype = geno,
          time_days = tt,
          treatment = trt,
          replicate = as.character(rep_id),
          cycle = as.integer(meta[["cycle"]]),
          phase = as.character(meta[["phase"]]),
          water_potential = wp,
          stomatal_conductance = gs
        )
        idx <- idx + 1L
      }
    }
  }
}

rehydra_data <- dplyr::bind_rows(rows)

# Controlled missing values for validation tests
set.seed(20260429)
na_rows_wp <- sample(seq_len(nrow(rehydra_data)), size = 8)
na_rows_gs <- sample(setdiff(seq_len(nrow(rehydra_data)), na_rows_wp), size = 8)
rehydra_data$water_potential[na_rows_wp] <- NA_real_
rehydra_data$stomatal_conductance[na_rows_gs] <- NA_real_

if (!dir.exists("data")) dir.create("data", recursive = TRUE)
if (!dir.exists("inst/extdata")) dir.create("inst/extdata", recursive = TRUE)

save(rehydra_data, file = "data/rehydra_data.rda", version = 2)
readr::write_csv(rehydra_data, "inst/extdata/rehydra_example.csv")
