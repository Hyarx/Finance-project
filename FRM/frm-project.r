############################################################
# Financial Risk Modelling - Group Project (single R script)
#
# Requirements (project-instructions.pdf):
# - Portfolio composition as of Nov 6th (cash ignored at 0%)
# - Use data prior to Nov 7th to forecast daily VaR/ES (95%)
#   for Nov 7th -> Dec 7th
# - Data features section (heteroscedasticity, dependence, extremes, etc.)
# - Risk analysis:
#   * Compare 3 GARCH models
#   * Compare 3 copulas
#   * Provide 3 competing approaches => 3 VaR/ES estimates
#   * Choose ONE approach for the official forecast + justify (quant + qual)
# - Out-of-sample performance: backtesting (Nov 7 -> Dec 7)
############################################################

rm(list=ls())
options(stringsAsFactors = FALSE)

############################################################
# 0) LIBRARIES
############################################################
suppressWarnings(suppressMessages({
  library(quantmod)
  library(PerformanceAnalytics)
  library(xts)
  library(rugarch)
  library(MASS)
  library(copula)
  library(tseries)
  library(readxl)
  library(moments)
  library(Matrix)
}))

set.seed(123)

############################################################
# 1) PATHS / INPUT FILES
############################################################
# Put these files in DATA_DIR:
# - myGetSymbols.R
# - portfolio_composition-nov6_prices_close.xlsx
# - rheinmetall_ag-price_history-dec11_25_14h42.xlsx
# - portfolio-historical_values-time_series.xlsx
DATA_DIR <- "/Users/louis/Desktop/frm-docs"
setwd(DATA_DIR)

source("myGetSymbols.R")  # professor function

FILE_PORTFOLIO_COMP <- "portfolio_composition-nov6_prices_close.xlsx"
FILE_RHEINMETALL    <- "rheinmetall_ag-price_history-dec11_25_14h42.xlsx"
FILE_PTF_VALUES     <- "portfolio-historical_values-time_series.xlsx"

FIG_DIR <- "figures"
dir.create(FIG_DIR, showWarnings = FALSE)

CSV_DIR <- "csv_outputs"
dir.create(CSV_DIR, showWarnings = FALSE)

REPORT_DIR <- "reports"
dir.create(REPORT_DIR, showWarnings = FALSE)

############################################################
# 2) SMALL HELPERS
############################################################
clean_usd_to_num <- function(x){
  x <- as.character(x)
  x <- gsub("\\$", "", x)
  x <- gsub(",", "", x)
  suppressWarnings(as.numeric(x))
}

is_weekend <- function(d){
  w <- weekdays(d)
  (w == "Saturday") | (w == "Sunday")
}

parse_excel_date <- function(x){
  if (inherits(x, "Date"))   return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x))         return(as.Date(x, origin = "1899-12-30"))
  
  s <- trimws(as.character(x))
  s[s == ""] <- NA
  s <- sub("^(\\d{4}-\\d{2}-\\d{2}).*$", "\\1", s)  # drop time
  
  out <- rep(as.Date(NA), length(s))
  fmts <- c("%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y", "%d-%b-%Y")
  
  for (f in fmts){
    idx <- which(is.na(out) & !is.na(s))
    if (!length(idx)) break
    tmp <- suppressWarnings(as.Date(s[idx], format = f))
    ok <- which(!is.na(tmp))
    if (length(ok)) out[idx[ok]] <- tmp[ok]
  }
  out
}

parse_num <- function(x){
  if (is.numeric(x)) return(x)
  s <- trimws(as.character(x))
  s[s == ""] <- NA
  s <- gsub("\\$", "", s)
  s <- gsub("\u00a0", "", s)
  s <- gsub(" ", "", s)
  
  dec_comma <- grepl(",", s) & !grepl("\\.", s)
  s[dec_comma] <- gsub(",", ".", s[dec_comma])
  s[!dec_comma] <- gsub(",", "", s[!dec_comma])
  
  suppressWarnings(as.numeric(s))
}

save_png <- function(filename, expr, width=1200, height=800, res=150){
  png(filename, width=width, height=height, res=res, bg = "white")
  on.exit(dev.off(), add=TRUE)
  expr
}

write_csv <- function(x, filename, row.names = FALSE, ...){
  utils::write.csv(x, file = file.path(CSV_DIR, filename), row.names = row.names, ...)
}

safe_grid <- function(...){
  try(graphics::grid(...), silent = TRUE)
}

############################################################
# 3) READ PORTFOLIO COMPOSITION (Nov 6)
############################################################
comp <- read_excel(FILE_PORTFOLIO_COMP)

comp$Amount <- as.numeric(comp$Amount)
comp$Symbol <- as.character(comp$Symbol)
comp$LastPrice_num   <- clean_usd_to_num(comp$`Last Price`)
comp$MarketValue_num <- clean_usd_to_num(comp$`Market Value`)

# weights on Nov 6 (cash ignored at 0%)
w <- comp$MarketValue_num / sum(comp$MarketValue_num)
names(w) <- comp$Symbol

message("Portfolio tickers: ", paste(comp$Symbol, collapse = ", "))
message("Portfolio weights (Nov 6):")
print(round(w, 6))

V0 <- sum(comp$MarketValue_num)  # portfolio value on Nov 6

############################################################
# 4) IMPORT PRICES (INNER JOIN: common trading days only)
#    (Avoid forward-filling holidays to prevent artificial zero returns.)
############################################################
read_rheinmetall_close <- function(path, sym = "RNMBF"){
  probe <- readxl::read_excel(path, sheet = 1, col_names = FALSE, .name_repair = "minimal")
  
  header_row <- which(apply(probe, 1, function(r){
    any(trimws(as.character(r)) == "Exchange Date")
  }))[1]
  if (is.na(header_row)) stop("Rheinmetall file: could not find 'Exchange Date' header row.")
  
  dat <- readxl::read_excel(path, sheet = 1, skip = header_row - 1, .name_repair = "minimal")
  
  nms <- names(dat)
  nms <- gsub("\u00a0", " ", nms)
  nms <- trimws(nms)
  names(dat) <- nms
  
  idx_date  <- which(tolower(names(dat)) == "exchange date")[1]
  idx_close <- which(tolower(names(dat)) == "close")[1]
  
  if (is.na(idx_date) || is.na(idx_close)){
    stop(paste0(
      "Rheinmetall file: could not locate columns 'Exchange Date' and 'Close'.\n",
      "Columns seen: ", paste(names(dat), collapse = ", ")
    ))
  }
  
  d <- parse_excel_date(dat[[idx_date]])
  c <- parse_num(dat[[idx_close]])
  
  ok <- !is.na(d) & !is.na(c)
  if (sum(ok) == 0){
    message("Diagnostics: head(date_raw) = ", paste(head(dat[[idx_date]], 5), collapse=" | "))
    message("Diagnostics: head(close_raw)= ", paste(head(dat[[idx_close]], 5), collapse=" | "))
    stop("Rheinmetall file: no (Date, Close) pairs parsed.")
  }
  
  x <- xts::xts(matrix(c[ok], ncol = 1), order.by = d[ok])
  colnames(x) <- sym
  x
}

INSAMPLE_START <- as.Date("2020-01-01")
INSAMPLE_END   <- as.Date("2025-11-06")

tickers_all  <- comp$Symbol
ticker_rhm   <- "RNMBF"
tickers_yahoo <- tickers_all[tickers_all != ticker_rhm]

prices_yahoo <- myGetSymbols(
  tickers = tickers_yahoo,
  from = INSAMPLE_START,
  to   = INSAMPLE_END,
  which = "Close",
  what  = "price"
)

prices_rhm <- read_rheinmetall_close(FILE_RHEINMETALL, sym = ticker_rhm)
prices_rhm <- prices_rhm[paste0(INSAMPLE_START, "/", INSAMPLE_END)]

# INNER JOIN (common dates across all assets)
prices <- merge(prices_yahoo, prices_rhm, all = FALSE)
prices <- na.omit(prices)

# Ensure column order matches portfolio tickers
prices <- prices[, tickers_all]

message("Price panel dimensions (inner join): ", nrow(prices), " dates x ", ncol(prices), " assets.")
message("Date range after alignment: ", as.character(first(index(prices))), " to ", as.character(last(index(prices))))

############################################################
# 5) COMPUTE LOG RETURNS (model-estimation sample)
############################################################
logret <- na.omit(diff(log(prices)))
logret <- logret[, tickers_all]

############################################################
# 6) DATA FEATURES (EXPORT STATS + SAVE FIGURES)
############################################################
R <- coredata(logret)
R_port <- as.numeric(R %*% as.numeric(w))
R_port_xts <- xts(R_port, order.by = index(logret))
colnames(R_port_xts) <- "PORT"

# Descriptive stats
stats_assets <- data.frame(
  Ticker   = colnames(R),
  Mean     = colMeans(R),
  Sd       = apply(R, 2, sd),
  Skewness = apply(R, 2, skewness),
  Kurtosis = apply(R, 2, kurtosis)
)
stats_port <- data.frame(
  Ticker="PORT",
  Mean=mean(R_port), Sd=sd(R_port),
  Skewness=skewness(R_port), Kurtosis=kurtosis(R_port)
)

write_csv(rbind(stats_assets, stats_port), "data_features_stats.csv", row.names = FALSE)

# Tail quantiles
q_probs <- c(0.01, 0.05, 0.95, 0.99)
q_assets <- t(apply(R, 2, quantile, probs = q_probs, na.rm = TRUE))
q_assets <- data.frame(Ticker = rownames(q_assets), q_assets, row.names = NULL)
q_port <- data.frame(Ticker="PORT", t(quantile(R_port, probs=q_probs, na.rm=TRUE)))
colnames(q_port)[-1] <- colnames(q_assets)[-1]
write_csv(rbind(q_assets, q_port), "data_features_quantiles.csv", row.names = FALSE)

# Dependence matrices
write_csv(cor(R), "data_features_cor_pearson.csv", row.names = TRUE)
write_csv(cor(R, method = "kendall"), "data_features_cor_kendall.csv", row.names = TRUE)

# Ljung-Box p-values on squared returns (heteroscedasticity / volatility clustering proxy)
lb_sq <- sapply(1:ncol(R), function(j){
  p <- tryCatch(Box.test(R[,j]^2, lag = 10, type = "Ljung-Box")$p.value, error=function(e) NA_real_)
  p
})
lb_sq_tbl <- data.frame(Ticker=colnames(R), LjungBox_pvalue_sqLag10=lb_sq)
lb_sq_port <- data.frame(Ticker="PORT", LjungBox_pvalue_sqLag10=tryCatch(Box.test(R_port^2, lag=10, type="Ljung-Box")$p.value, error=function(e) NA_real_))
write_csv(rbind(lb_sq_tbl, lb_sq_port), "data_features_ljungbox_sq.csv", row.names=FALSE)

# Plots saved to figures/
save_png(file.path(FIG_DIR, "returns_timeseries.png"), {
  par(mfrow=c(3,2), mar=c(3,4,2,1))
  for(j in 1:ncol(logret)){
    plot(index(logret), as.numeric(logret[,j]), type="l",
         main=paste("Log-returns:", colnames(logret)[j]),
         xlab="", ylab="log-return")
    safe_grid()
  }
  par(mfrow=c(1,1))
})

save_png(file.path(FIG_DIR, "acf_squared_returns.png"), {
  par(mfrow=c(3,2), mar=c(5,4,3,1)) 
  
  for(j in 1:ncol(logret)){
    acfo <- acf(as.numeric(logret[,j])^2, plot = FALSE)
    plot(acfo, main=paste("ACF squared:", colnames(logret)[j])) 
  }
})

save_png(file.path(FIG_DIR, "hist_with_normal_overlay.png"), {
  par(mfrow=c(3,2), mar=c(3,4,2,1))
  for(j in 1:ncol(logret)){
    data_vals <- as.numeric(logret[,j])
    mu    <- mean(data_vals)
    sigma <- sd(data_vals)
    hist(data_vals, breaks=60, probability=TRUE, 
         main=paste("Hist vs Normal:", colnames(logret)[j]), 
         col="lightgray", border="white", xlab="Returns",
         xlim=c(mu - 4*sigma, mu + 4*sigma)) # Zoom in on the data
    curve(dnorm(x, mean=mu, sd=sigma), 
          add=TRUE, col="red", lwd=2, n=1000)
  }
  par(mfrow=c(1,1))
})

save_png(file.path(FIG_DIR, "qqplots.png"), {
  par(mfrow=c(3,2), mar=c(3,4,2,1))
  for(j in 1:ncol(logret)){
    x <- as.numeric(logret[,j])
    qqnorm(x, main=paste("QQ:", colnames(logret)[j])); qqline(x)
  }
  par(mfrow=c(1,1))
})

# Correlation chart (PerformanceAnalytics)
save_png(file.path(FIG_DIR, "correlation_chart.png"), {
  chart.Correlation(logret, histogram = TRUE, pch = 19)
})

# Portfolio return + ACF squared
save_png(file.path(FIG_DIR, "portfolio_returns_and_acf_sq.png"), height=1200, expr={
  layout(matrix(c(1,2), ncol=1), heights=c(2, 1))
  
  # --- TOP PLOT (Returns) ---
  par(mar=c(3, 5, 4, 2))
  plot(as.zoo(R_port_xts), 
       main="Portfolio log-returns", 
       xlab="", ylab="Log Return", col="black")
  grid()
  
  # --- BOTTOM PLOT (ACF) ---
  par(mar=c(5, 5, 4, 2))
  acfo <- acf(as.numeric(R_port)^2, plot=FALSE)
  plot(acfo, main="ACF of squared portfolio returns")
})

############################################################
# 7) FIT 3 GARCH MODELS PER ASSET (compare via BIC)
#    Enhancement: include.mean=TRUE so we can simulate mu + sigma*z
############################################################
safe_ugarchfit <- function(spec, data){
  solvers <- c("hybrid", "solnp")
  last <- NULL
  for (s in solvers){
    last <- tryCatch(
      ugarchfit(
        spec = spec,
        data = data,
        solver = s,
        fit.control = list(scale = 1),
        solver.control = list(trace = 0)
      ),
      error = function(e) NULL
    )
    if (!is.null(last) && last@fit$convergence == 0) return(last)
  }
  last
}

safe_BIC <- function(fit){
  if (is.null(fit)) return(NA_real_)
  if (fit@fit$convergence != 0) return(NA_real_)
  ic <- tryCatch(infocriteria(fit), error = function(e) NULL)
  if (is.null(ic)) return(NA_real_)
  as.numeric(ic["Bayes", 1])
}

spec.norm <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model     = list(armaOrder = c(0,0), include.mean = TRUE),
  distribution.model = "norm"
)
spec.std <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model     = list(armaOrder = c(0,0), include.mean = TRUE),
  distribution.model = "std"
)
spec.egarch.std <- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1,1)),
  mean.model     = list(armaOrder = c(0,0), include.mean = TRUE),
  distribution.model = "std"
)

specs <- list(GARCH_N = spec.norm, GARCH_t = spec.std, eGARCH_t = spec.egarch.std)

GARCH.fits <- lapply(specs, function(.) vector("list", ncol(logret)))
for(m in names(GARCH.fits)) names(GARCH.fits[[m]]) <- colnames(logret)

BIC.garch <- matrix(NA, nrow = ncol(logret), ncol = length(specs),
                    dimnames = list(colnames(logret), names(specs)))

for(m in names(specs)){
  for(j in 1:ncol(logret)){
    ticker <- colnames(logret)[j]
    fit <- safe_ugarchfit(spec = specs[[m]], data = as.numeric(logret[, j]))
    GARCH.fits[[m]][[ticker]] <- fit
    BIC.garch[ticker, m] <- safe_BIC(fit)
    
    if (is.null(fit)){
      message("GARCH fit FAILED (error): ", ticker, " | ", m)
    } else if (fit@fit$convergence != 0){
      message("GARCH fit NOT converged: ", ticker, " | ", m)
    }
  }
}

message("BIC comparison of GARCH models (lower is better):")
print(round(BIC.garch, 2))

best.garch.name <- apply(BIC.garch, 1, function(x){
  if (all(is.na(x))) NA_character_ else names(which.min(x))
})
message("Best GARCH model per asset (by BIC):")
print(best.garch.name)

# One global GARCH choice for the FHS approach (simple + report-friendly)
avg_bic_by_model <- colMeans(BIC.garch, na.rm = TRUE)
best_garch_global <- names(which.min(avg_bic_by_model))
message("Best GARCH model globally (by average BIC across assets): ", best_garch_global)

############################################################
# 8) FIT 3 COPULAS (Normal / t / Clayton)
#    Enhancements:
#    - Try unstructured dependence (dispstr="un") for Normal & t
#    - Use MPL for Clayton (often more stable)
#    - Keep robust fallbacks (U2 + exchangeable + fixed df)
############################################################
clip_U <- function(U, eps = 1e-6){
  U <- as.matrix(U)
  U <- U[complete.cases(U), , drop = FALSE]
  U[U <= eps] <- eps
  U[U >= 1 - eps] <- 1 - eps
  U
}

# Build U1 (PIT) and U2 (pdist of standardized residuals) from GARCH_t
U1 <- matrix(NA, nrow = nrow(logret), ncol = ncol(logret),
             dimnames = list(NULL, colnames(logret)))
U2 <- matrix(NA, nrow = nrow(logret), ncol = ncol(logret),
             dimnames = list(NULL, colnames(logret)))

for(j in 1:ncol(logret)){
  ticker <- colnames(logret)[j]
  fit <- GARCH.fits[["GARCH_t"]][[ticker]]
  if (is.null(fit) || fit@fit$convergence != 0) stop("GARCH_t fit missing/non-converged for: ", ticker)
  
  U1[,j] <- as.numeric(pit(fit))
  U2[,j] <- rugarch::pdist(
    "std",
    residuals(fit, standardize = TRUE),
    shape = fit@fit$coef["shape"]
  )
}

U <- clip_U(U1)
d <- ncol(U)
nU <- nrow(U)

# Start from Kendall tau -> approximate correlation matrix
tau_mat <- suppressWarnings(cor(U, method = "kendall", use = "pairwise.complete.obs"))
rho_mat <- sin(pi * tau_mat / 2)
diag(rho_mat) <- 1
rho_mat <- as.matrix(nearPD(rho_mat, corr = TRUE)$mat)

P2p_safe <- function(P){
  if (exists("P2p", where = asNamespace("copula"), mode = "function")) {
    return(copula::P2p(P))
  }
  # fallback (if P2p not available): lower triangle by columns
  return(P[lower.tri(P)])
}

rho0_vec <- P2p_safe(rho_mat)

# Exchangeable rho for fallback
tau_vec <- tau_mat[upper.tri(tau_mat)]
tau_bar <- mean(tau_vec, na.rm = TRUE)
rho0_ex <- sin(pi * tau_bar / 2)
rho_lower <- -1/(d-1) + 1e-3
rho0_ex <- max(min(rho0_ex, 0.7), rho_lower)

fit_copula_safe <- function(type, U_in){
  U_in <- clip_U(U_in)
  
  if (type == "Normal") {
    # Try unstructured ML
    fit1 <- tryCatch(
      fitCopula(normalCopula(dim = d, dispstr = "un"), data = U_in,
                method = "ml", start = rho0_vec),
      error = function(e) NULL
    )
    if (!is.null(fit1)) return(list(fit=fit1, ok=TRUE, used="Normal un-ML", U_used=U_in))
    
    # Fallback exchangeable
    fit2 <- tryCatch(
      fitCopula(normalCopula(dim = d, dispstr = "ex"), data = U_in,
                method = "ml", start = rho0_ex),
      error = function(e) NULL
    )
    if (!is.null(fit2)) return(list(fit=fit2, ok=TRUE, used="Normal ex-ML", U_used=U_in))
    
    return(list(fit=NULL, ok=FALSE, used="Normal failed", U_used=U_in))
  }
  
  if (type == "Clayton") {
    # Prefer MPL
    fit1 <- tryCatch(
      fitCopula(claytonCopula(dim = d), data = U_in, method = "mpl"),
      error = function(e) NULL
    )
    if (!is.null(fit1)) return(list(fit=fit1, ok=TRUE, used="Clayton MPL", U_used=U_in))
    
    # Fallback ML
    fit2 <- tryCatch(
      fitCopula(claytonCopula(dim = d), data = U_in, method = "ml"),
      error = function(e) NULL
    )
    if (!is.null(fit2)) return(list(fit=fit2, ok=TRUE, used="Clayton ML", U_used=U_in))
    
    return(list(fit=NULL, ok=FALSE, used="Clayton failed", U_used=U_in))
  }
  
  if (type == "t") {
    # Try unstructured ML on U1 first
    start_t <- c(rho0_vec, 10)  # correlations + df start
    fit1 <- tryCatch(
      fitCopula(tCopula(dim = d, dispstr = "un"), data = U_in,
                method = "ml", start = start_t),
      error = function(e) NULL
    )
    if (!is.null(fit1)) return(list(fit=fit1, ok=TRUE, used="t un-ML (U1)", U_used=U_in))
    
    # Try MPL unstructured
    fit2 <- tryCatch(
      fitCopula(tCopula(dim = d, dispstr = "un"), data = U_in,
                method = "mpl", start = start_t),
      error = function(e) NULL
    )
    if (!is.null(fit2)) return(list(fit=fit2, ok=TRUE, used="t un-MPL (U1)", U_used=U_in))
    
    # Try again on U2
    U_alt <- clip_U(U2)
    tau_alt <- suppressWarnings(cor(U_alt, method = "kendall", use = "pairwise.complete.obs"))
    rho_alt <- sin(pi * tau_alt / 2); diag(rho_alt) <- 1
    rho_alt <- as.matrix(nearPD(rho_alt, corr=TRUE)$mat)
    rho_alt_vec <- P2p_safe(rho_alt)
    start_t2 <- c(rho_alt_vec, 10)
    
    fit3 <- tryCatch(
      fitCopula(tCopula(dim = d, dispstr = "un"), data = U_alt,
                method = "ml", start = start_t2),
      error = function(e) NULL
    )
    if (!is.null(fit3)) return(list(fit=fit3, ok=TRUE, used="t un-ML (U2)", U_used=U_alt))
    
    # Final fallback: exchangeable t-copula with fixed df, tau-based rho
    cop_fallback <- tCopula(param = rho0_ex, dim = d, df = 10, df.fixed = TRUE, dispstr="ex")
    dens <- dCopula(U_alt, cop_fallback)
    dens[!is.finite(dens) | dens <= 0] <- 1e-300
    loglik_fallback <- sum(log(dens))
    
    return(list(
      fit = list(copula = cop_fallback, loglik = loglik_fallback, k = 1),
      ok = FALSE,  # not ML-estimated
      used = "t fallback (ex, df fixed=10, U2)",
      U_used = U_alt
    ))
  }
  
  stop("Unknown copula type.")
}

fitN <- fit_copula_safe("Normal", U)
fitC <- fit_copula_safe("Clayton", U)
fitT <- fit_copula_safe("t", U)

extract_fit_info <- function(fit_obj, name){
  if (is.null(fit_obj$fit)) {
    return(data.frame(Copula=name, Used=fit_obj$used, ok=FALSE, logLik=NA, npar=NA, nObs=nrow(fit_obj$U_used), BIC=NA))
  }
  if (isS4(fit_obj$fit)) {
    ll <- as.numeric(fit_obj$fit@loglik)
    np <- length(coef(fit_obj$fit))
    nObs <- nrow(fit_obj$U_used)
    bic <- -2*ll + np*log(nObs)
    return(data.frame(Copula=name, Used=fit_obj$used, ok=fit_obj$ok, logLik=ll, npar=np, nObs=nObs, BIC=bic))
  } else {
    ll <- as.numeric(fit_obj$fit$loglik)
    np <- as.numeric(fit_obj$fit$k)
    nObs <- nrow(fit_obj$U_used)
    bic <- -2*ll + np*log(nObs)
    return(data.frame(Copula=name, Used=fit_obj$used, ok=fit_obj$ok, logLik=ll, npar=np, nObs=nObs, BIC=bic))
  }
}

copula_info <- rbind(
  extract_fit_info(fitN, "Normal"),
  extract_fit_info(fitT, "t"),
  extract_fit_info(fitC, "Clayton")
)

message("Copula comparison (lower BIC is better):")
print(copula_info)

write_csv(copula_info, "copula_fit_summary.csv", row.names = FALSE)

# Build copula objects for simulation
copulas <- list(
  Normal  = if (isS4(fitN$fit)) fitN$fit@copula else NULL,
  Clayton = if (isS4(fitC$fit)) fitC$fit@copula else NULL,
  t       = if (isS4(fitT$fit)) fitT$fit@copula else fitT$fit$copula
)

# Choose best copula by BIC among those with finite BIC
bic_vec <- copula_info$BIC
names(bic_vec) <- copula_info$Copula
bic_vec[!is.finite(bic_vec)] <- Inf
best_copula_name <- names(which.min(bic_vec))
message("Best copula by BIC (among available fits): ", best_copula_name)

# Copula diagnostic plot (observed PIT-U vs simulated U) on the strongest dependent pair
if (!is.null(copulas[[best_copula_name]])) {
  tau_abs <- abs(tau_mat)
  diag(tau_abs) <- NA
  ij <- which(tau_abs == max(tau_abs, na.rm=TRUE), arr.ind = TRUE)[1, ]
  i_star <- ij[1]; j_star <- ij[2]
  
  save_png(file.path(FIG_DIR, "copula_observed_vs_simulated_best_pair.png"), {
    op <- par(mfrow=c(1,2), mar=c(4,4,2,1))
    
    plot(U[,j_star], U[,i_star], pch=19, cex=0.5,
         main=paste("Observed U (PIT):", colnames(logret)[i_star], "vs", colnames(logret)[j_star]),
         xlab=colnames(logret)[j_star], ylab=colnames(logret)[i_star])
    safe_grid()
    
    simU <- rCopula(n = 2000, copula = copulas[[best_copula_name]])
    plot(simU[,j_star], simU[,i_star], pch=19, cex=0.5,
         main=paste("Simulated from", best_copula_name, "copula"),
         xlab=colnames(logret)[j_star], ylab=colnames(logret)[i_star])
    safe_grid()
    
    par(op)
  })
}

############################################################
# 9) FORECAST SETUP (BUSINESS DAYS ONLY)
############################################################
FORECAST_START <- as.Date("2025-11-07")
FORECAST_END   <- as.Date("2025-12-07")

forecast_dates <- seq(from = FORECAST_START, to = FORECAST_END, by = "day")
forecast_dates <- forecast_dates[!is_weekend(forecast_dates)]
H <- length(forecast_dates)
if (H <= 0) stop("No business-day forecast dates found in Nov 7 -> Dec 7.")

message("Forecast horizon H (business days): ", H)
message("First forecast date: ", as.character(forecast_dates[1]), " | Last: ", as.character(forecast_dates[H]))

alpha <- 0.95
p_tail <- 1 - alpha
B <- 10000

############################################################
# 10) APPROACH 1: RiskMetrics / Parametric MVN (constant VaR/ES)
############################################################
mu_hat <- colMeans(logret)
Sigma_hat <- cov(logret)

mu_p <- sum(w * mu_hat)
sd_p <- sqrt( t(as.numeric(w)) %*% as.matrix(Sigma_hat) %*% as.numeric(w) )

mu_L <- -mu_p
sd_L <- sd_p

VaR_RM_loss <- as.numeric(qnorm(alpha, mean = mu_L, sd = sd_L))
z <- qnorm(alpha)
ES_RM_loss  <- as.numeric(mu_L + sd_L * dnorm(z) / (1 - alpha))

RM_series <- data.frame(
  Date = forecast_dates,
  VaR_loss = rep(VaR_RM_loss, H),
  ES_loss  = rep(ES_RM_loss,  H)
)

############################################################
# 11) APPROACH 2: Filtered Historical Simulation (FHS)
#     Enhancement: include conditional mean forecasts (mu) in simulation
############################################################
FHS_forecast <- function(model_name){
  resid_mat <- matrix(NA, nrow = nrow(logret), ncol = ncol(logret))
  colnames(resid_mat) <- colnames(logret)
  
  sigma_for <- matrix(NA, nrow = H, ncol = ncol(logret))
  mu_for    <- matrix(NA, nrow = H, ncol = ncol(logret))
  colnames(sigma_for) <- colnames(logret)
  colnames(mu_for)    <- colnames(logret)
  
  for(j in 1:ncol(logret)){
    ticker <- colnames(logret)[j]
    fit <- GARCH.fits[[model_name]][[ticker]]
    if (is.null(fit) || fit@fit$convergence != 0) stop("Missing/non-converged GARCH fit for ", ticker, " in ", model_name)
    
    resid_mat[,j] <- as.numeric(residuals(fit, standardize = TRUE))
    
    fc <- ugarchforecast(fit, n.ahead = H)
    sigma_for[,j] <- as.numeric(sigma(fc))[1:H]
    mu_for[,j]    <- as.numeric(fitted(fc))[1:H]
  }
  
  idx <- sample(1:nrow(resid_mat), size = B, replace = TRUE)
  resid_sim <- resid_mat[idx, , drop = FALSE]
  
  VaR_loss <- numeric(H)
  ES_loss  <- numeric(H)
  
  for(h in 1:H){
    ret_sim <- sweep(resid_sim, 2, sigma_for[h,], FUN = "*")
    ret_sim <- sweep(ret_sim, 2, mu_for[h,],    FUN = "+")
    
    ptf_ret_sim  <- as.numeric(ret_sim %*% as.numeric(w))
    ptf_loss_sim <- -ptf_ret_sim
    
    VaR_h <- as.numeric(quantile(ptf_loss_sim, alpha))
    ES_h  <- mean(ptf_loss_sim[ptf_loss_sim >= VaR_h])
    
    VaR_loss[h] <- VaR_h
    ES_loss[h]  <- ES_h
  }
  
  data.frame(Date = forecast_dates, VaR_loss = VaR_loss, ES_loss = ES_loss)
}

FHS_GARCHN  <- FHS_forecast("GARCH_N")
FHS_GARCHt  <- FHS_forecast("GARCH_t")
FHS_eGARCHt <- FHS_forecast("eGARCH_t")

# Choose the single FHS forecast used for the "3 competing approaches"
FHS_choice <- switch(best_garch_global,
                     GARCH_N = FHS_GARCHN,
                     GARCH_t = FHS_GARCHt,
                     eGARCH_t = FHS_eGARCHt)

FHS_choice_label <- paste0("FHS_", best_garch_global)

############################################################
# 12) APPROACH 3: Copula Monte Carlo (GARCH_t marginals + 3 copulas)
#     Enhancement: include conditional mean forecasts (mu) in simulation
############################################################
MC_copula_forecast <- function(copula_name){
  if (is.null(copulas[[copula_name]])) stop("Copula object missing for: ", copula_name)
  
  sigma_for <- matrix(NA, nrow = H, ncol = ncol(logret))
  mu_for    <- matrix(NA, nrow = H, ncol = ncol(logret))
  shapes    <- rep(NA, ncol(logret))
  names(shapes) <- colnames(logret)
  colnames(sigma_for) <- colnames(logret)
  colnames(mu_for)    <- colnames(logret)
  
  for(j in 1:ncol(logret)){
    ticker <- colnames(logret)[j]
    fit <- GARCH.fits[["GARCH_t"]][[ticker]]
    if (is.null(fit) || fit@fit$convergence != 0) stop("Missing/non-converged GARCH_t fit for: ", ticker)
    
    fc <- ugarchforecast(fit, n.ahead = H)
    sigma_for[,j] <- as.numeric(sigma(fc))[1:H]
    mu_for[,j]    <- as.numeric(fitted(fc))[1:H]
    
    shapes[j] <- as.numeric(fit@fit$coef["shape"])
    if (!is.finite(shapes[j])) shapes[j] <- 8
  }
  
  VaR_loss <- numeric(H)
  ES_loss  <- numeric(H)
  
  for(h in 1:H){
    rU <- rCopula(n = B, copula = copulas[[copula_name]])
    
    Z <- matrix(NA, nrow = B, ncol = ncol(logret))
    for(j in 1:ncol(logret)){
      Z[,j] <- qdist("std", p = rU[,j], mu = 0, sigma = 1, shape = shapes[j])
    }
    
    ret_sim <- sweep(Z, 2, sigma_for[h,], FUN="*")
    ret_sim <- sweep(ret_sim, 2, mu_for[h,],    FUN="+")
    
    ptf_ret_sim  <- as.numeric(ret_sim %*% as.numeric(w))
    ptf_loss_sim <- -ptf_ret_sim
    
    VaR_h <- as.numeric(quantile(ptf_loss_sim, alpha))
    ES_h  <- mean(ptf_loss_sim[ptf_loss_sim >= VaR_h])
    
    VaR_loss[h] <- VaR_h
    ES_loss[h]  <- ES_h
  }
  
  data.frame(Date = forecast_dates, VaR_loss = VaR_loss, ES_loss = ES_loss, Copula = copula_name)
}

MC_Normal  <- MC_copula_forecast("Normal")
MC_t       <- MC_copula_forecast("t")
MC_Clayton <- MC_copula_forecast("Clayton")

# Choose copula-MC forecast used for the "3 competing approaches"
MC_choice <- switch(best_copula_name,
                    Normal = MC_Normal,
                    t = MC_t,
                    Clayton = MC_Clayton)

MC_choice_label <- paste0("CopulaMC_", best_copula_name)

############################################################
# 13) COLLECT FORECASTS
#     - OUT_ALL: all variants (for comparisons)
#     - OUT_3: the 3 approaches required by the instructions
#     - FINAL_FORECAST: choose ONE approach for official forecast + justification file
############################################################
make_out <- function(df, label){
  data.frame(
    Date = df$Date,
    Model = label,
    VaR_loss_ret = df$VaR_loss,
    ES_loss_ret  = df$ES_loss,
    VaR_loss_USD = df$VaR_loss * V0,
    ES_loss_USD  = df$ES_loss  * V0
  )
}

OUT_ALL <- rbind(
  make_out(RM_series,     "RiskMetrics_MVN"),
  make_out(FHS_GARCHN,    "FHS_GARCH_N"),
  make_out(FHS_GARCHt,    "FHS_GARCH_t"),
  make_out(FHS_eGARCHt,   "FHS_eGARCH_t"),
  make_out(MC_Normal,     "CopulaMC_Normal"),
  make_out(MC_t,          "CopulaMC_t"),
  make_out(MC_Clayton,    "CopulaMC_Clayton")
)

OUT_3 <- rbind(
  make_out(RM_series,   "RiskMetrics_MVN"),
  make_out(FHS_choice,  FHS_choice_label),
  make_out(MC_choice,   MC_choice_label)
)

write_csv(OUT_ALL, "VaR_ES_forecasts_ALL_models_95pct.csv", row.names = FALSE)
write_csv(OUT_3,   "VaR_ES_forecasts_3_approaches_95pct.csv", row.names = FALSE)

# Report-friendly plots for 3 approaches (USD VaR/ES)
plot_forecast_series <- function(OUT_tbl, value_col, file, main){
  dd <- OUT_tbl[, c("Date","Model", value_col)]
  dd <- dd[order(dd$Date), ]
  models <- unique(dd$Model)
  dates <- sort(unique(dd$Date))
  
  mat <- sapply(models, function(m){
    v <- dd[dd$Model == m, value_col]
    v[match(dates, dd[dd$Model == m, "Date"])]
  })
  
  cols <- seq_len(ncol(mat))
  
  save_png(file, {
    matplot(dates, mat, type="l", lwd=2, lty=1, col=cols,
            xlab="", ylab=value_col, main=main)
    safe_grid()
    legend("topleft", legend=models, col=cols, lty=1, lwd=2, bty="n")
  })
}

plot_forecast_series(
  OUT_3, "VaR_loss_USD",
  file.path(FIG_DIR, "forecast_VaR_USD_3_approaches.png"),
  "Forecast VaR (95%) in USD - 3 approaches"
)
plot_forecast_series(
  OUT_3, "ES_loss_USD",
  file.path(FIG_DIR, "forecast_ES_USD_3_approaches.png"),
  "Forecast ES (95%) in USD - 3 approaches"
)

# Choose FINAL forecast approach (policy: prefer Copula-MC when tails+dependence exist)
avg_kurt_excess <- mean(stats_assets$Kurtosis - 3, na.rm = TRUE)
hetero_frac <- mean(lb_sq_tbl$LjungBox_pvalue_sqLag10 < 0.05, na.rm = TRUE)
avg_abs_cor <- mean(abs(cor(R)[upper.tri(cor(R))]), na.rm = TRUE)

FINAL_MODEL <- if (is.finite(avg_kurt_excess) && is.finite(avg_abs_cor) &&
                   (avg_kurt_excess > 0.5 || avg_abs_cor > 0.2 || hetero_frac > 0.5)) {
  MC_choice_label
} else {
  FHS_choice_label
}

FINAL_FORECAST <- OUT_3[OUT_3$Model == FINAL_MODEL, ]
write_csv(FINAL_FORECAST, "FINAL_forecast_selected_95pct.csv", row.names = FALSE)

# Write a small justification summary (quantitative signals + model selection outputs)
sink(file.path(REPORT_DIR, "model_choice_summary.txt"))
cat("=== Model Choice Summary (Quantitative Signals) ===\n\n")
cat("Data window: ", as.character(first(index(logret))), " -> ", as.character(last(index(logret))), "\n\n")

cat("Fat tails proxy:\n")
cat("  Average excess kurtosis (assets): ", round(avg_kurt_excess, 4), "\n\n")

cat("Volatility clustering proxy:\n")
cat("  Fraction of assets with Ljung-Box p<0.05 on squared returns (lag 10): ", round(hetero_frac, 4), "\n\n")

cat("Dependence proxy:\n")
cat("  Average absolute Pearson correlation (off-diagonal): ", round(avg_abs_cor, 4), "\n\n")

cat("GARCH comparison (average BIC):\n")
print(round(avg_bic_by_model, 2))
cat("\nSelected GARCH for FHS approach: ", best_garch_global, "\n\n")

cat("Copula fit summary:\n")
print(copula_info)
cat("\nSelected copula for Copula-MC approach (by BIC among available): ", best_copula_name, "\n\n")

cat("FINAL selected approach (for report forecast): ", FINAL_MODEL, "\n")
cat("\n(Use the figures in ./figures and the tables in ./csv_outputs for qualitative discussion.)\n")
sink()

############################################################
# 14) BACKTESTING (Nov 7 -> Dec 7) + PLOTS + SUMMARY TABLE
############################################################
ptf_raw <- read_excel(FILE_PTF_VALUES, .name_repair = "minimal")

ptf_vals <- data.frame(
  Date = parse_excel_date(ptf_raw$Date),
  Value = clean_usd_to_num(ptf_raw$`Group 3`)
)
ptf_vals <- ptf_vals[!is.na(ptf_vals$Date) & !is.na(ptf_vals$Value), ]
ptf_vals <- ptf_vals[order(ptf_vals$Date), ]
ptf_vals <- ptf_vals[!duplicated(ptf_vals$Date, fromLast = TRUE), ]

ptf_bt <- ptf_vals[ptf_vals$Date >= FORECAST_START & ptf_vals$Date <= FORECAST_END, ]
if (nrow(ptf_bt) < 2) stop("Not enough portfolio values in the backtest window Nov 7 -> Dec 7.")

# Compute returns on BUSINESS DAYS and include the day BEFORE forecast start
# so that the return for 2025-11-07 is computed vs 2025-11-06.

# 1) Find the previous business day before FORECAST_START present in ptf_vals
prev_bd <- max(
  ptf_vals$Date[ptf_vals$Date < FORECAST_START & !is_weekend(ptf_vals$Date)],
  na.rm = TRUE
)
if (!is.finite(prev_bd)) stop("Could not find a business day before FORECAST_START in ptf_vals.")

# 2) Build the value series from prev_bd to FORECAST_END, business days only
ptf_bt <- ptf_vals[ptf_vals$Date >= prev_bd & ptf_vals$Date <= FORECAST_END, ]
ptf_bt <- ptf_bt[!is_weekend(ptf_bt$Date), ]
ptf_bt <- ptf_bt[order(ptf_bt$Date), ]

if (nrow(ptf_bt) < 2) stop("Not enough business-day portfolio values to compute backtest returns.")

# 3) Compute returns; return at date t uses Value(t-1) -> Value(t)
ptf_bt$ret  <- c(NA, diff(log(ptf_bt$Value)))
ptf_bt$loss <- -ptf_bt$ret

# 4) Keep only forecast business days (intersection) and drop the first NA-return row
ptf_bt2 <- ptf_bt[ptf_bt$Date %in% forecast_dates, ]
ptf_bt2 <- ptf_bt2[!is.na(ptf_bt2$ret), ]

if (nrow(ptf_bt2) < 2) stop("Not enough aligned business-day returns in backtest window.")

message("Backtest observations used (business-day aligned): ", nrow(ptf_bt2))
message("First backtest date: ", as.character(min(ptf_bt2$Date)),
        " | Last: ", as.character(max(ptf_bt2$Date)))

# Always compute UC/IND/CC safely (and optionally keep rugarch::VaRTest)
run_var_test <- function(model_label, OUT_tbl, dates_bt, actual_ret, p_tail){
  dfm <- OUT_tbl[OUT_tbl$Model == model_label, ]
  if (nrow(dfm) == 0) stop("Model not found in OUT: ", model_label)
  
  VaR_ret <- -dfm$VaR_loss_ret
  ES_ret  <- -dfm$ES_loss_ret
  
  VaR_ret <- VaR_ret[match(dates_bt, dfm$Date)]
  ES_ret  <- ES_ret[match(dates_bt, dfm$Date)]
  
  actual_ret <- as.numeric(actual_ret)
  ok <- complete.cases(actual_ret, VaR_ret, ES_ret)
  actual_ret <- actual_ret[ok]
  VaR_ret    <- as.numeric(VaR_ret[ok])
  ES_ret     <- as.numeric(ES_ret[ok])
  dates_use  <- dates_bt[ok]
  
  I <- as.integer(actual_ret < VaR_ret)
  exc <- sum(I)
  N <- length(I)
  hit <- if (N > 0) exc / N else NA_real_
  
  # Safe logs
  klog <- function(k, p){
    if (k == 0) return(0)
    if (p <= 0) return(-Inf)
    if (p >= 1) return(0)
    k * log(p)
  }
  
  phat <- if (N > 0) exc / N else NA_real_
  
  # Kupiec UC
  ll0 <- klog(N - exc, 1 - p_tail) + klog(exc, p_tail)
  ll1 <- klog(N - exc, 1 - phat)   + klog(exc, phat)
  LRuc <- -2 * (ll0 - ll1)
  p_uc <- 1 - pchisq(LRuc, df = 1)
  
  # Christoffersen IND
  if (N >= 2) {
    I_lag <- I[-N]
    I_now <- I[-1]
    n00 <- sum(I_lag == 0 & I_now == 0)
    n01 <- sum(I_lag == 0 & I_now == 1)
    n10 <- sum(I_lag == 1 & I_now == 0)
    n11 <- sum(I_lag == 1 & I_now == 1)
    
    p01 <- if ((n00 + n01) == 0) 0 else n01 / (n00 + n01)
    p11 <- if ((n10 + n11) == 0) 0 else n11 / (n10 + n11)
    
    ll0i <- klog(n00 + n10, 1 - phat) + klog(n01 + n11, phat)
    ll1i <- klog(n00, 1 - p01) + klog(n01, p01) + klog(n10, 1 - p11) + klog(n11, p11)
    LRind <- -2 * (ll0i - ll1i)
    p_ind <- 1 - pchisq(LRind, df = 1)
  } else {
    LRind <- NA_real_
    p_ind <- NA_real_
  }
  
  LRcc <- LRuc + LRind
  p_cc <- 1 - pchisq(LRcc, df = 2)
  
  # Try rugarch VaRTest (optional)
  test_rugarch <- tryCatch(
    rugarch::VaRTest(alpha = p_tail,
                     actual = xts::xts(actual_ret, order.by = dates_use),
                     VaR    = xts::xts(VaR_ret,    order.by = dates_use)),
    error = function(e) NULL
  )
  
  list(
    model = model_label,
    N = N,
    exceptions = exc,
    hit_ratio = hit,
    p_uc = p_uc, p_ind = p_ind, p_cc = p_cc,
    VaRTest = test_rugarch,
    aligned = data.frame(Date = dates_use, Ret = actual_ret, Loss = -actual_ret, VaR_ret = VaR_ret, ES_ret = ES_ret, Violation = I)
  )
}

# Backtest the 3 approaches
BT_RM  <- run_var_test("RiskMetrics_MVN", OUT_3, ptf_bt2$Date, ptf_bt2$ret, p_tail)
BT_FHS <- run_var_test(FHS_choice_label,  OUT_3, ptf_bt2$Date, ptf_bt2$ret, p_tail)
BT_COP <- run_var_test(MC_choice_label,   OUT_3, ptf_bt2$Date, ptf_bt2$ret, p_tail)

bt_summary <- data.frame(
  Model = c(BT_RM$model, BT_FHS$model, BT_COP$model),
  N = c(BT_RM$N, BT_FHS$N, BT_COP$N),
  Exceptions = c(BT_RM$exceptions, BT_FHS$exceptions, BT_COP$exceptions),
  HitRatio = c(BT_RM$hit_ratio, BT_FHS$hit_ratio, BT_COP$hit_ratio),
  p_UC = c(BT_RM$p_uc, BT_FHS$p_uc, BT_COP$p_uc),
  p_IND = c(BT_RM$p_ind, BT_FHS$p_ind, BT_COP$p_ind),
  p_CC = c(BT_RM$p_cc, BT_FHS$p_cc, BT_COP$p_cc)
)

write_csv(bt_summary, "backtest_summary_3_approaches.csv", row.names = FALSE)

message("=== Backtest summary (3 approaches) ===")
print(bt_summary)

# Backtest plots (ENHANCED: clearer styling)
plot_backtest <- function(BT_obj, file){
  df <- BT_obj$aligned
  if (nrow(df) == 0) return(invisible(NULL))
  
  VaR_loss <- -df$VaR_ret
  ES_loss  <- -df$ES_ret
  
  ylim_all <- range(c(df$Loss, VaR_loss, ES_loss), na.rm = TRUE, finite = TRUE)
  if (!all(is.finite(ylim_all))) ylim_all <- range(df$Loss, na.rm = TRUE, finite = TRUE)
  
  save_png(file, {
    par(mar=c(4,4,2,1))
    plot(df$Date, df$Loss, type="l", lwd=2, col="black",
         main=paste("Backtest:", BT_obj$model, "- Loss vs VaR/ES"),
         xlab="", ylab="Loss", ylim=ylim_all)
    safe_grid()
    
    lines(df$Date, VaR_loss, lwd=2, lty=2, col="red")
    lines(df$Date, ES_loss,  lwd=2, lty=3, col="darkorange")
    
    points(df$Date[df$Violation == 1], df$Loss[df$Violation == 1], pch=19, col="red")
    
    legend("topleft",
           legend=c("Loss", "VaR 95%", "ES 95%", "Violation"),
           lty=c(1,2,3,NA), lwd=c(2,2,2,NA),
           pch=c(NA,NA,NA,19),
           col=c("black","red","darkorange","red"),
           bty="n")
  })
}

plot_backtest(BT_RM,  file.path(FIG_DIR, "backtest_RiskMetrics.png"))
plot_backtest(BT_FHS, file.path(FIG_DIR, paste0("backtest_", FHS_choice_label, ".png")))
plot_backtest(BT_COP, file.path(FIG_DIR, paste0("backtest_", MC_choice_label, ".png")))

############################################################
# 15) EXPORT OUTPUTS
############################################################
write_csv(
  data.frame(Ticker = rownames(BIC.garch), BIC.garch, Best = best.garch.name),
  "garch_BIC_comparison.csv",
  row.names = FALSE
)

# For copulas, the richer summary is copula_fit_summary.csv already
write_csv(copula_info[, c("Copula","Used","ok","logLik","npar","nObs","BIC")],
          "copula_BIC_comparison.csv", row.names = FALSE)

# Backtest report
sink(file.path(REPORT_DIR, "backtest_results.txt"))
cat("Backtest window (business days): ", as.character(FORECAST_START), " -> ", as.character(FORECAST_END), "\n\n")

write_bt <- function(BT){
  cat("Model: ", BT$model, "\n")
  cat("N = ", BT$N, " | Exceptions = ", BT$exceptions, " | HitRatio = ", round(BT$hit_ratio, 6), "\n")
  cat("p-values: UC=", round(BT$p_uc, 6), " IND=", round(BT$p_ind, 6), " CC=", round(BT$p_cc, 6), "\n\n")
  if(!is.null(BT$VaRTest)){
    cat("VaRTest() output (rugarch):\n")
    print(BT$VaRTest)
  } else {
    cat("VaRTest() not available (or failed). Using safe UC/IND/CC above.\n")
  }
  cat("\n", strrep("-", 60), "\n\n", sep = "")
}

write_bt(BT_RM)
write_bt(BT_FHS)
write_bt(BT_COP)
sink()

# Save workspace snapshot
save(
  comp, prices, logret, w, V0,
  stats_assets, stats_port, q_assets, q_port, lb_sq_tbl,
  BIC.garch, best.garch.name, best_garch_global,
  copula_info, best_copula_name,
  RM_series, FHS_GARCHN, FHS_GARCHt, FHS_eGARCHt, FHS_choice, FHS_choice_label,
  MC_Normal, MC_t, MC_Clayton, MC_choice, MC_choice_label,
  OUT_ALL, OUT_3, FINAL_MODEL, FINAL_FORECAST,
  ptf_vals, ptf_bt, ptf_bt2,
  BT_RM, BT_FHS, BT_COP, bt_summary,
  file = "project_outputs.RData"
)

message("Done.")
message("Key files written:")
message("- csv_outputs/data_features_stats.csv, data_features_quantiles.csv, data_features_cor_*.csv, data_features_ljungbox_sq.csv")
message("- figures/*.png (including forecast_VaR_USD_3_approaches.png, forecast_ES_USD_3_approaches.png, copula_observed_vs_simulated_best_pair.png)")
message("- csv_outputs/garch_BIC_comparison.csv, copula_fit_summary.csv, copula_BIC_comparison.csv")
message("- csv_outputs/VaR_ES_forecasts_ALL_models_95pct.csv, VaR_ES_forecasts_3_approaches_95pct.csv")
message("- csv_outputs/FINAL_forecast_selected_95pct.csv")
message("- reports/model_choice_summary.txt")
message("- csv_outputs/backtest_summary_3_approaches.csv")
message("- reports/backtest_results.txt")
message("- project_outputs.RData")
