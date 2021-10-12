# R functions to attempt to reproduce results of Eichstaedt et al. (10.1177/0956797614557867)

library(ggplot2)
library(pastecs)
library(QuantPsyc)
library(data.table)
library(bitops)
library(lsr)       #not called directly, but needed for cohensD()
library(psych)     #not called directly, but needed for fisherz()

setup_build_topics <- TRUE		#TRUE -> builds topics from Eichstaedt et al.'s original data files (slow)
setup_build_themes <- TRUE		#TRUE -> builds word themes from Eichstaedt et al.'s original data files (slow)

# Read in the basic county data from the original, untouched study file
county <- read.csv("countyoutcomes.csv", stringsAsFactors=FALSE)

# Cleanups
for (i in c(16, 17, 18)) {
  county[,i] <- as.integer(gsub("NULL", NA, county[,i]))	#file contains NULL values which became NULL strings
}

# Give some of the variables names that are easier to type
names(county)[4] <- "female"
names(county)[5] <- "hispanic"
names(county)[6] <- "black"
names(county)[7] <- "foreignborn"
names(county)[8] <- "married_male"
names(county)[9] <- "married_female"
names(county)[10] <- "highschool"
names(county)[11] <- "bachelors"
names(county)[12] <- "rawincome"
names(county)[13] <- "smoker"
names(county)[14] <- "diabetic"
names(county)[15] <- "obese"
names(county)[16] <- "fairpoorhealth"
names(county)[17] <- "sickdays_physical"
names(county)[18] <- "sickdays_mental"
names(county)[19] <- "hypertension_male"
names(county)[20] <- "hypertension_female"
names(county)[21] <- "education"
names(county)[22] <- "hypertension" 	#this appears to be an unweighted average of the male and female hypertension rates
names(county)[23] <- "married"	     	#this appears to be an unweighted average of the male and female marriage rates
names(county)[24] <- "income"	       	#logarithmic
names(county)[25] <- "mortality"	    #from AHD
names(county)[28] <- "gini"
names(county)[29] <- "unemployment"

# Read in the county median age data - skip first line ("second header") and unnecessary columns.
census_age <- read.csv("census_age.csv", stringsAsFactors=FALSE, colClasses=c("NULL", "character", "NULL", "character", rep("NULL", 2)))[-1,]
for (i in 1:ncol(census_age)) {
  census_age[,i] <- as.numeric(census_age[,i])
}
names(census_age) <- c("fips", "median_age")
county <- merge(county, census_age, by="fips", all.x=TRUE)

# Read in the county geographical data - skip unnecessary columns.
census_geo <- read.delim("Gaz_counties_national.txt", colClasses=c("NULL", "numeric", rep("NULL", 3), "numeric", rep("NULL", 2), "numeric", "NULL", rep("numeric", 2)))
names(census_geo) <- c("fips", "homes", "landarea", "latitude", "longitude")
county <- merge(county, census_geo, by="fips", all.x=TRUE)

# List of causes of death for which we obtained 2009-2010 CDC data.
# Source: http://wonder.cdc.gov/ucd-icd10.html
# "AHD" means "I25.1 (Atherosclerotic heart disease)", as used by Eichstaedt et al.
# "assault" means "X85-Y09 (Assault)"
# "cancer" means "C00-C97 (Neoplasms)"
# "otherheart" means I20-I51, excluding I25.1, i.e. all other heart disease.
# "respiratory" means "J00-J98 (Diseases of the respiratory system)"
# "selfharm" means "X60-X84 (Intentional self-harm)"
# "stroke" means "I60-I69 (Cerebrovascular diseases)"
causes <- c("AHD", "all", "assault", "cancer", "otherheart", "respiratory", "selfharm", "stroke")

# Read in all specific cause of death files and merge into county data.
cat("Reading causes of death...")
for (i in 1:length(causes)) {
  cause <- causes[i]
  county_filename <- paste("cause_", cause, ".aa.txt", sep="")
  cat(" file", county_filename, "...")
# column 1 = county number, column 2 = age-adjusted death rate
  deaths <- read.delim(county_filename, header=FALSE)
  names(deaths) <- c("fips", cause)
  county <- merge(county, deaths, by="fips", all.x=TRUE)
}
cat(" done\n")

# Sanity check: "AHD" from our files (constructed from CDC data) should match "mortality" from Eichstaedt et al.'s file.
if (abs(max(county$AHD - county$mortality)) > 0.00001) {
  stop("Reproduction AHD data does not match original")
}

all_controls <- c("income", "education", "female", "hispanic", "black", "married", "obese", "hypertension", "smoker", "diabetic")

# Read in the word data, cf. the Results/Dictionaries subsection of Eichstaedt et al.
theme_names <- c("Anger", "Anxiety", "NegEmotions", "NegEngagement", "NegEngNoTired", "NegRelations", "NegRelNoHate", "PosEmotions", "PosEngagement", "PosRelations", "PosRelNoLove")

# Read theme data.

if (! exists("read_themes")) {
  read_themes = TRUE
}

if (read_themes) {
  cat("Reading Twitter word themes... ")

  if (setup_build_themes) {
    themes <- unique(county["fips"])

    for (i in 1:length(theme_names)) {
      theme_name <- theme_names[i]

      filename <- paste("theme", theme_name, ".csv.gz", sep="")
      words <- read.csv(filename, header=FALSE)[,1:4]
      names(words) <- c("fips", "word", "count", "freq")
      cat("Reading file ", filename, "... ", sep="")

      fipslist <- unique(words["fips"])
      for (j in 1:length(fipslist$fips)) {
        f <- fipslist$fips[j]
        s <- sum(words[words$fips==f,]$freq)
        themes[themes$fips==f ,theme_name] <- s
      }
    }
  } else {
    themes <- read.delim("themes.txt", header=TRUE)
  }

  read_themes = FALSE
  cat(" done\n")
}

cat("Merging Twitter themes...")
county <- merge(county, themes, by="fips", all.x=TRUE)
cat(" done\n")

# Read topic data.
if (! exists("read_topics")) {
  read_topics = TRUE
}

if (read_topics) {
  cat("Reading Twitter topics...")

  if (setup_build_topics) {
    rawtopics <- read.csv("../county.freqs.topics.csv.gz", colClasses=c("NULL", rep("character", 2), "NULL", "character", "NULL"))[-1,]
    names(rawtopics) <- c("fips", "topic_number", "topic_weight")
    for (i in 1:ncol(rawtopics)) {
      rawtopics[,i] <- as.numeric(rawtopics[,i])
    }

    cat(" done\n")
    cat("Combining topics...")

    n_topics <- length(unique(rawtopics$topic_number))
    topics <- unique(county["fips"])
    for (i in 1:n_topics) {
      if (i %% 20 == 0) {
        cat(" ", i)
      }

      t <- i - 1
      topic_t <- rawtopics[rawtopics$topic_number == t,][c(1,3)]
      topic_name <- paste("topic_", t, sep="")
      names(topic_t)[2] <- topic_name
      topics <- merge(topics, topic_t, by="fips", all.x=TRUE)
    }
  }
  else {
    topics <- read.delim("topics.txt.gz", stringsAsFactors=FALSE, header=TRUE)
  }

  read_topics = FALSE
  cat(" done\n")
}

cat("Merging Twitter topics...")
county <- merge(county, topics, by="fips", all.x=TRUE)
cat(" done\n")

# Build our own ratio-based variables.
county$pop_density <- county$pop2010 / county$landarea
county$home_density <- county$homes / county$landarea

# Function to format a p-value nicely.
formatP <- function (p)
{
#ggp <<- p	#debug
  pstring <- "=????"
  if ((! is.nan(p)) && (! is.na(p))) {
    if (p < .0001) {
      pstring <- "<.0001"
    }
    else {
      pstring <- paste("=", sub("^0", "", sprintf("%.3f", p)), sep="")
    }
  }

  stars <- ""
  if (p < .001) {
    stars <- "***"
  }
  else if (p < .01) {
    stars <- "**"
  }
  else if (p < .05) {
    stars <- "*"
  }

  return(sprintf("%-10s", paste("p", pstring, stars, sep="")))
}

# Function to format a p-value nicely.
formatCI <- function (ci)
{
  s <- c("?????", "?????")
  for (i in 1:2) {
    v <- ci[i]
    if ((! is.nan(v)) && (! is.na(v))) {
      s[i] <- sprintf("%.3f", v)
    }
  }

  return(paste("[", s[1], ":", s[2], "]", sep=""))
}

# Function to show the correlations between a cause of death (or any other variable) and the Twitter topics.
topic_cor <- function (cause)
{
  cat("Topic", paste("r(", cause, ")", sep=""), "p", "sig", "CI", "beta", "pbeta", "sigbeta", "CIbeta", "\n", sep="\t")

  for (i in 1:(ncol(topics) - 1)) {
    cn <- paste("topic_", i - 1, sep="")

    cor <- cor.test(county[[cause]], topics[[cn]])
    cor_ci <- round(cor$conf.int, digits=2)
    ci <- paste("[", cor_ci[1], ", ", cor_ci[2], "]", sep="")
    r <- cor$estimate
    p <- cor$p.value
    sig <- if (p < 0.000025) 1 else 0

    controls = "+income +education"
    formula <- as.formula(paste(cause, "~", cn, controls))

    model <- suppressWarnings(lm(formula, data=county))
    ms <- summary(model)
    beta_coeffs <- suppressWarnings(lm.beta(model))

    beta <- beta_coeffs[cn]
    b <- ms$coefficients[cn, 1]
    pbeta <- ms$coefficients[cn, 4]
    sigbeta <- if (pbeta < 0.000025) 1 else 0
    model_ci <- round(confint(model, cn, level=0.95) * (beta / b), 2)
    cibeta <- paste("[", model_ci[1], ", ", model_ci[2], "]", sep="")

    cat(cn, sprintf("%.4f", r), sprintf("%.10f", p), sig, ci, sprintf("%.4f", beta), sprintf("%.10f", pbeta), sigbeta, cibeta, "\n", sep="\t")
  }
}

# Function to show the theme coefficients for a given cause of death.
cause_stats <- function (cause, controls=c("income", "education"), theme="*", weighted=FALSE, data=county)
{
  local_themes <- c(theme)
  if (theme == "*") {
    local_themes <- theme_names
  }

  control_formula <- ""
  control_string <- "(None)"
  if (length(controls) > 0) {
    control_formula <- paste("+", paste(controls, collapse="+"), sep="")
    control_string <- paste(controls, collapse=" ")
  }
  cat("Cause of death:", cause, "Controls:", control_string, "\n")
  n_beta <- length(controls) + 1

  if (length(controls) > 0) {
    Y_cov <- as.formula(paste(cause, "~", control_formula))
    resY <- residuals(lm(Y_cov, data=data, na.action=na.exclude))
  }

# Loop to display header (lt==0) and per-theme data
  for (lt in 0:length(local_themes)) {
    if (lt > 0) {
      theme_name <- local_themes[lt]
      if (weighted) {
        theme_name <- paste("w_", theme_name, sep="")
      }

      formula <- as.formula(paste(cause, "~", theme_name, control_formula))
      model <- suppressWarnings(lm(formula, data=data, na.action=na.exclude))
      ms <- summary(model)
      beta_coeffs <- suppressWarnings(lm.beta(model))

      adjR2 <- abs(ms$adj.r.squared)
      R2 <- ms$r.squared
      beta <- 0.0
      p <- 1.0
      ci <- c(-9.9, 9.9)

      if (theme_name != "") {
        beta <- beta_coeffs[theme_name]

        if (! any(is.nan(ms$coefficients))) {
          p <- ms$coefficients[theme_name, 4]
          b <- ms$coefficients[theme_name, 1]
          ci <- confint(model, theme_name, level=0.95) * (beta / b)
        }
      }

      part_r <- beta
      part_ci <- ci
      if (length(controls) > 0) {
        X_cov <- as.formula(paste(theme_name, "~", control_formula))
        resX <- residuals(lm(X_cov, data=data))
        ct <- cor.test(resY, resX)
        part_r <- ct$estimate
        part_ci <- ct$conf.int
      }
    }
    else {
      display_vars <- list(
          list("theme_name", 16, "Theme", 0)
        , list("adjR2", 6, "adj.R?", 3)
        , list("R2", 6, "R?", 3)
        , list("beta", 6, "Beta", 3)
        , list("part_r", 8, "part.r", 3)
        , list("p", 12, "p-value", "formatP")
        , list("ci", 16, "CI(beta)", "formatCI")
        , list("part_ci", 16, "CI(part.r)", "formatCI")
      )

      for (dv in 1:length(display_vars)) {
        names(display_vars[[dv]]) <- c("variable", "length", "title", "method")
      }
    }

    for (dv in 1:length(display_vars)) {
      var <- display_vars[[dv]]
      if (lt > 0) {
        method <- var[["method"]]

        if (is.numeric(method)) {
          s <- get(var[[1]])
          if (method > 0) {
            format <- paste("%.", method, "f", sep="")
            s <- sprintf(format, s)
          }
        }
        else {
          s <- do.call(method, args=list(get(var[[1]])))
        }
      }
      else {
        s <- var[[3]]
      }

      format <- paste("%", var[[2]], "s", sep="")
      cat(sprintf(format, s), "  ", sep="")
    }

    cat("\n")
  }
}
