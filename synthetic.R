library(Synth)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(quadprog)
library(zoo)

##################################################################################################
#### GATHER DATA ####
##################################################################################################


#UNIQUE-STATE TRUMP RALLIES

state <- c("OK", "AZ", "MN", "WI", "AZ", "PA", "NH", "PA", "NC", "MI", "NV", "WI", "MN",
           "NC", "OH", "OH", "PA", "FL", "PA", "MN")
county <- c("Tulsa", "Maricopa", "Blue Earth", "Winnebago", "Yuma",
            "Lackawanna", "Rockingham", "Westmoreland", "Forsyth",
            "Saginaw", "Douglas", "Marathon", "Beltrami",
            "Cumberland", "Montgomery", "Fulton", "Allegheny", 
            "Duval", "Dauphin", "St. Louis")
date <- c("2020-06-20", "2020-06-23", "2020-08-17", "2020-08-17", "2020-08-18", "2020-08-20",
          "2020-08-28", "2020-09-03", "2020-09-08", "2020-09-10", "2020-09-12",
          "2020-09-17", "2020-09-18", "2020-09-19", "2020-09-21", "2020-09-21", "2020-09-22",
          "2020-09-24", "2020-09-26", "2020-09-30")
trumpRallies <- data.frame(county, state, date, stringsAsFactors = FALSE)

covariates <- read.csv("county_data_abridged.csv", header=TRUE)

us.counties <- read.csv("us-counties.csv", header = TRUE)

##################################################################################################
#### MAIN FUNCTIONS ####
##################################################################################################

county_timeseries <- function(county, state){
  countyset <- us.counties[which(us.counties$state == state &
                                   us.counties$county != "Unknown"),]
  countyset <- countyset[,!(names(countyset) %in% c("deaths", "fips", "state"))]
  countyset$date <- as.Date(countyset$date)
  countyset <- countyset %>% 
    pivot_wider(names_from = county, values_from = cases )
  
  countyset[is.na(countyset)] <- 0
  
  #COUNTYSET for non-rolling average
  
  
  #7 DAY ROLLING AVERAGE
  # counties <- countyset %>% select(-one_of("date"))
  # counties <- as.matrix(counties)
  # counties <- diff(counties)
  # counties <- as.data.frame(counties)
  # counties <- rollmean(counties, k = 14, fill = 0)
  # 
  # date <- as.character(countyset$date)
  # date <- date[-c(1)]
  # 
  # counties <- data.frame(date = date, counties)
  # 
  # counties$date <- as.Date(counties$date)
  
  #END ROLLING AVERAGE SECTION
  
  
  return(countyset)
}


synthetic_rally <- function(county, state, date){
  set.seed(0)
  date <- as.Date(date)
  county.pool <- covariates[which(covariates$StateName == state),] #all counties in state
  county.pop <- select(county.pool, PopulationEstimate2018)
  county.pop <- as.numeric(county.pop$PopulationEstimate2018)
  
  county.pool <- select(county.pool, c(CountyName, PopulationEstimate2018,
                                                Rural.UrbanContinuumCode2013,
                                                PopulationDensityperSqMile2010, dem_to_rep_ratio,
                                                SVIPercentile,
                                                Mask.Never, Mask.Rarely, Mask.Sometimes,
                                                Mask.Frequently, Mask.Always))
  
  #printTest <- county.pool
  #view(printTest)
  
  county.pool$PopulationEstimate2018 <- county.pool$PopulationEstimate2018 * 0.1
  
  county.pool <- t(county.pool)
  
  county.pool <- as_tibble(county.pool)
  
  names(county.pool) <- county.pool %>% slice(1) %>% unlist()
  county.pool <- county.pool[-1,]
  
  county.pool <- as.matrix(county.pool)
  county.pool <- county.pool[,colSums(is.na(county.pool)) < nrow(county.pool)]
  
  
  all.timeseries <- county_timeseries(county, state.name[which(state.abb == state)])
  
  allTS <- all.timeseries
  
  
  #NEWPORT NEWS CITY SITUATION:
  #cols_keep <- intersect(colnames(county.pool), colnames(all.timeseries))
  #county.pool <- county.pool[,cols_keep, drop=FALSE]
  #all.timeseries <- all.timeseries[,cols_keep, drop=FALSE]
  
  
  post.treatment <- all.timeseries[which(all.timeseries$date > date &
                                           all.timeseries$date <= date + 21),]
  pre.treatment <- all.timeseries[which(all.timeseries$date >= date - 49 &
                                          all.timeseries$date <= date),]
  
  hull.base <- pre.treatment[,-1]
  hull.base <- hull.base %>% select(sort(names(.)))
  hull.base <- as.matrix(hull.base)
  

  
  
  #view(county.pool)
  #view(hull.base)
  
  #hull.base <- hull.base %*% diag((1/county.pop))
  
  hull <- rbind(county.pool, hull.base)
  
  hull <- as.data.frame(hull)
  
  #view(hull)
  
  not_select <- c(county)
  X1 <- hull %>% select(one_of(not_select))
  X0 <- hull %>% select(-one_of(not_select))

  keep <- apply(X0, 1, function(x) length(unique(x[!is.na(x)])) != 1)
  X0 <- X0[keep,]
  X1 <- X1[keep,]
  
  X1 <- as.matrix(as.double(unlist(X1))) #otherwise would be a list, incompatible with lm()
  X0 <- matrix(unlist(X0), ncol = ncol(X0), byrow = FALSE)
  #X0 <- data.matrix(X0, rownames.force = NA)
  mode(X0) = "numeric"

  
  control.count <- ncol(X0)
  
  # D <- t(X0) %*% X0
  # d <- t(X1) %*% X0
  # constraint.matrix <- cbind(rep(1,control.count), diag(control.count))
  # b <- c(1,rep(0,control.count))
  
  #solve inequality-constrained (i.e. equality w/ slack variables) quadratic programming problem
  #QP <- solve.QP(Dmat = D, factorized = FALSE, dvec = d, Amat = constraint.matrix,
  #               bvec = b, meq = 0)
  
  #weights <- QP$solution
  
  
  #v.norm <- (v - min(v))/(max(v) - min(v))
  
  not_select = c(county, "date")

  Z1 <- as.matrix(post.treatment %>% select(one_of(c(county))))
  Z0 <- as.matrix(post.treatment %>% select(-one_of(not_select)))

  all.timeseries <- all.timeseries[which(all.timeseries$date <= date + 21 &
                                         all.timeseries$date >= date - 49),]

  
  num.days <- length(X1) - 10
  v <- c(10, rep(10, nrow(county.pool) -1), (seq(from = 1, to = num.days))^(1))

  synth.out <- synth(X1 = X1, X0 = X0, Z0 = Z0, Z1 = Z1, method = "BFGS")
  #synth.out <- synth(X1 = X1, X0 = X0, Z0 = Z0, Z1 = Z1, custom.v = v, method = "BFGS")

  pooled.timeseries <- all.timeseries %>% select(-one_of(not_select))
  keep <- apply(pooled.timeseries, 1, function(x) length(unique(x[!is.na(x)])) != 1)

  pooled.timeseries <- pooled.timeseries %>% select(sort(names(.)))
  pooled.timeseries <- pooled.timeseries[keep,]
  pooled.timeseries <- as.matrix(pooled.timeseries)

  weights <- synth.out$solution.w

  synthetic.county <- pooled.timeseries %*% weights
  treated.county <- all.timeseries %>% select(one_of(c(county)))
  treated.county <- treated.county[keep,]

  dates <- all.timeseries$date
  #dates <- dates[keep,]

  results <- as.data.frame(cbind(dates, treated.county, synthetic.county))

  results <- results %>% rename(date = dates, 
                                treated = one_of(c(county)), synthetic.control = w.weight)
  
  results$date <- as.Date(results$date)
  
  #rolling avg: change to treated.county, otherwise keep to treated
  
  p <- ggplot(data = results, aes(x = date, y = treated)) +
    geom_line() +
    geom_line(data = results, aes(y = synthetic.control), linetype = "dashed") +
    geom_vline(xintercept = as.numeric(as.Date(date)),
               colour = "red", linetype = "dashed") +
    labs(title = paste("Total positive COVID-19 cases in", county, ",", state,
                       "compared to synthetic control"),
         x = "Date", y = "Total Cases") +
    scale_x_date(date_labels = "%m-%d", date_breaks = "1 week")
  
  # p <- ggplot(data = results, aes(x = date, y = treated.county)) +
  #   geom_line() +
  #   geom_line(data = results, aes(y = synthetic.control), linetype = "dashed") +
  #   geom_vline(xintercept = as.numeric(as.Date(date)), 
  #              colour = "red", linetype = "dashed") +
  #   labs(title = paste("Average Daily New COVID-19 cases in", county, ",", state, 
  #                      "compared to synthetic control"),
  #        x = "Date", y = "14-Day Rolling Average") +
  #   scale_x_date(date_labels = "%m-%d", date_breaks = "1 week")
  
  
  #countyWeights <- data.frame(weights)
  
  return(p)
}


synthet <- synthetic_rally(county = "Saginaw", state = "MI", date = "2020-09-10")
synthet
view(synthet)

##################################################################################################
#### AGGREGATE ALL COUNTIES ####
##################################################################################################

N <- length(trumpRallies$county)


allcounties <- data.frame(County = character(N), 
                          Actual = integer(N), 
                          Synthetic = double(N), 
                          stringsAsFactors = FALSE)


for (i in 1:N) {
  synthet <- synthetic_rally(county = trumpRallies$county[i], 
                             state = trumpRallies$state[i], 
                             date = trumpRallies$date[i])
  allcounties$County[i] <- trumpRallies$county[i]
  allcounties$Actual[i] <- tail(synthet$treated, n=1)
  allcounties$Synthetic[i] <- tail(synthet$synthetic.control, n=1)
}

df <- allcounties
df$Difference <- df$Actual-df$Synthetic

df$BinaryDifference <- df$Difference
df$BinaryDifference[df$BinaryDifference < 0] <- 0
df$BinaryDifference[df$BinaryDifference > 0] <- 1
mean(df$BinaryDifference)
