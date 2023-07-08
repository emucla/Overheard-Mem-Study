# !!! TODO BEFORE RUNNING !!! (look for "!!!")
# ! update path of file
# ! update coder1 & coder2 with csv file names
# ! change name of output file

# !!! change path of file
setwd("C:/Users/shaop/Downloads")

library(tidyverse)
library(plyr)
library(irr)
library(compare)
library(arsenal)
library(tidyr)

# analyze function - analyzes individual csv file (TODO - write xls files)
#
# VARIABLES:
# duration -> length of labeling events in trial
# trial_time -> length of trial
# "x"-dur -> duration of "x" label within a trial
#
# RETURN:
# dataframe containing three columns:
# raw = raw data
# analysis = data for analysis
# n = number of trials (used for analysis)
analyze <- function(codes_csv) {
    # reads csv into dataframe
    data <- data.frame(read.csv(codes_csv))

    # intializes vectors
    durations <- times <- targets <- neutrals <- distractors <- persons <- c() # nolint

    # intializes indices for looping through data
    cur_trial <- 1
    cur_index <- 1

    # loop through trials (1-8)
    while (!is.na(data$Trial.offset[cur_trial])) {
        cur_duration <- 0
        t_dur <- d_dur <- n_dur <- p_dur <- 0
        # loop through individual datapoints up to end of current trial
        while (!is.na(data$Attention.offset[cur_index]) && data$Attention.offset[cur_index] <= data$Trial.offset[cur_trial]){ # nolint
            # find time of current labeling event
            cur_time <- (data$Attention.offset[cur_index] - data$Attention.onset[cur_index]) / 1000 # nolint
            # add length of labeling event to duration
            cur_duration <- cur_duration + cur_time
            # counts number of each label
            switch(
                data$Attention.t.d.n[cur_index],
                "t" = t_dur <- t_dur + cur_time,
                "d" = d_dur <- d_dur + cur_time,
                "n" = n_dur <- n_dur + cur_time,
                "p" = p_dur <- p_dur + cur_time
            )
            # advances to next datapoint
            cur_index <- cur_index + 1
        }
        # adds all values from the trial to respective vectors
        durations <- c(durations, cur_duration)
        targets <- c(targets, t_dur)
        neutrals <- c(neutrals, n_dur)
        distractors <- c(distractors, d_dur)
        persons <- c(persons, p_dur)

        trial_time <- (data$Trial.offset[cur_trial] - data$Trial.onset[cur_trial]) / 1000 # nolint
        times <- c(times, trial_time)
        # advances to next trial
        cur_trial <- cur_trial + 1
    }
    #preps dataframe for return, rounds off numbers for analysis
    res <- data.frame(
        n = cur_trial - 1,
        raw = c(durations, targets, neutrals, distractors, persons, times),
        analysis = c(round(durations, 0), round(targets, 2), round(neutrals, 2), round(distractors, 2), round(persons, 2), round(times, 0)) # nolint
    )
    return(res)
}

# !!! change coder1 and coder2 csv names
coder1 <- analyze("22_SW.csv")
coder2 <- analyze("22_RS.csv")

min <- min(coder1$n[1], coder2$n[1])
max <- max(coder1$n[1], coder2$n[1])
if (coder1$n[1] == min) {
    c2 <- c()

    for (i in 1:(length(coder2$analysis) / max)) {
        for (j in 1:min) {
            print(j)
            c2 <- c(c2, coder2$analysis[(i - 1) * max + j])
        }
    }
    rel <- data.frame(
        coder1 = coder1$analysis,
        coder2 = c2
    )
} else {
    c1 <- c()

    for (i in 1:(length(coder1$analysis) / max)) {
        for (j in 1:min) {
            print(j)
            c1 <- c(c1, coder1$analysis[(i - 1) * max + j])
        }
    }
    rel <- data.frame(
        coder1 = c1,
        coder2 = coder2$analysis
    )
}
print(icc(rel, model = "twoway", type = "agreement", unit = "single"))

# run from handmade csv file to test compare script (TODO NEED TO UPDATE)
reldot <- read.csv("SUB_22_REL.csv", header = TRUE)
newrel <- subset(reldot, select = c("coder1", "coder2"))
print(icc(newrel, model = "twoway", type = "agreement", unit = "single"))

# write rel to a csv file
#!!! update name of csv to something new
write.csv(rel, "22_REL.csv", row.names = FALSE)