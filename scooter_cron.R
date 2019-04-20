library(cronR)
# run from terminal (not in RStudio)

cron_clear(ask = FALSE)

cmd <- cron_rscript(paste0(Sys.getenv("HOME"),"/Documents/scooters/scooter_scraper.R"))

cron_add(cmd, frequency = '*/15  *    *    *   *  ', id = "job2", description = "scooter_scr")
