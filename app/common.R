# get environment
ENV <- Sys.getenv("MNWD_ENV")

# directories
DATA_DIR <- Sys.getenv("MNWD_DATA_DIR")
if (ENV == "prod") {
  MODEL_DIR <- file.path(DATA_DIR,"models")
} else if (ENV == "test") {
  MODEL_DIR <- file.path(DATA_DIR,"models",Sys.info()[['login']])
} else {
  stop(paste0("Don't recognize MNWD_ENV ",ENV))
}
dir.create(file.path(MODEL_DIR))

# input files
PROD_CONS_TIME_SERIES_FILE <- file.path(DATA_DIR,'MNWD_RW_Production_and_Consumption_Time_Series.csv')

# output files
MODEL_LIST <- list(tbats=list(modelfile=file.path(MODEL_DIR,"total_cons_tbats.Rdata"),
                              days_per_step=1),
                   autoarima=list(modelfile=file.path(MODEL_DIR,"total_cons_autoarima.Rdata"),
                                  days_per_step=30.25),
                   ets=list(modelfile=file.path(MODEL_DIR,"total_cons_ets.Rdata"),
                            days_per_step=30.25)
)
# TBATS_MODEL <- file.path(MODEL_DIR,"total_cons_tbats.Rdata")
# AUTOARIMA_MODEL <- file.path(MODEL_DIR,"total_cons_autoarima.Rdata")
# ETS_MODEL <- file.path(MODEL_DIR,"total_cons_ets.Rdata")


# 
TOTAL_FORECAST_MODEL <- 'tbats'
