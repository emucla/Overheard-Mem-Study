# !!! TODO BEFORE RUNNING !!! (look for "!!!")
# ! update path of file
# ! update coder initals and subject ID

# !!! change path of file
setwd("C:/Users/shaop/Downloads")

library(tidyverse)
library(plyr)
library(irr)
library(compare)
library(arsenal)
library(tidyr)

# !!! change ID + intials
sub_id <- 22
c1_initials <- "SW"
c2_initials <- "RS"

# analyze function - analyzes individual csv file (TODO - write xls files)
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

    # intializes indices for looping through data & storing values
    cur_trial <- 1 # stores trial number
    cur_index_trial <- 1 # stores index of trial event
    cur_index_attention <- 1 # stores index of attention event
    trial_time <- 0 # store time of current trial
    cur_duration <- 0 # store duration of labeling events
    t_dur <- d_dur <- n_dur <- p_dur <- 0 # store duration of "n" label

    # loop through blocks (either trial or ISI)
    while (!(is.na(data$Trial.trialnum[cur_index_trial]) || data$Trial.trialnum[cur_index_trial] == "")) { # nolint
        # skips ISI
        if (data$Trial.trialnum[cur_index_trial] == "ISI") {
            cur_index_trial <- cur_index_trial + 1
        } else {
            # updates trial number if new trial, adds previous trial values to list # nolint
            if (data$Trial.trialnum[cur_index_trial] > cur_trial) {
                # adds all values from the trial to respective vectors
                durations <- c(durations, cur_duration)
                targets <- c(targets, t_dur)
                neutrals <- c(neutrals, n_dur)
                distractors <- c(distractors, d_dur)
                persons <- c(persons, p_dur)
                times <- c(times, trial_time)
                # resets varaibles
                trial_time <- 0
                cur_duration <- 0
                t_dur <- d_dur <- n_dur <- p_dur <- 0
                # advances trial num
                cur_trial <- cur_trial + 1
            }
            # loop through individual datapoints up to end of current trial
            while (!is.na(data$Attention.offset[cur_index_attention]) && data$Attention.offset[cur_index_attention] <= data$Trial.offset[cur_index_trial]){ # nolint
                # find time of current labeling event
                cur_time <- (data$Attention.offset[cur_index_attention] - data$Attention.onset[cur_index_attention]) / 1000 # nolint
                # add length of labeling event to duration
                cur_duration <- cur_duration + cur_time
                # counts number of each label
                switch(
                    data$Attention.t.d.n.p[cur_index_attention],
                    "t" = t_dur <- t_dur + cur_time,
                    "d" = d_dur <- d_dur + cur_time,
                    "n" = n_dur <- n_dur + cur_time,
                    "p" = p_dur <- p_dur + cur_time
                )
                # advances to next datapoint
                cur_index_attention <- cur_index_attention + 1
            }
            # adds time of block to current trial time
            trial_time <- trial_time + ((data$Trial.offset[cur_index_trial] - data$Trial.onset[cur_index_trial]) / 1000) # nolint
            # advances to next block
            cur_index_trial <- cur_index_trial + 1
        }
    }
    # adds the last trial's time to the list
    times <- c(times, trial_time)
    durations <- c(durations, cur_duration)
    targets <- c(targets, t_dur)
    neutrals <- c(neutrals, n_dur)
    distractors <- c(distractors, d_dur)
    persons <- c(persons, p_dur)
    # preps dataframe for return, rounds off numbers for analysis
    res <- data.frame(
        n = cur_trial,
        raw = c(durations, targets, neutrals, distractors, persons, times),
        analysis = c(round(durations, 0), round(targets, 2), round(neutrals, 2), round(distractors, 2), round(persons, 2), round(times, 0)) # nolint
    )
    return(res)
}

# runs analyze for both codesheets
coder1 <- analyze(sprintf("% s_% s.csv", sub_id, c1_initials))
coder2 <- analyze(sprintf("% s_% s.csv", sub_id, c2_initials))

# sizes dataframes for reliability
min <- min(coder1$n[1], coder2$n[1])
max <- max(coder1$n[1], coder2$n[1])
if (coder1$n[1] == min) {
    # cuts c2 for analysis
    c2 <- c()
    for (i in 1:(length(coder2$analysis) / max)) {
        for (j in 1:min) {
            c2 <- c(c2, coder2$analysis[(i - 1) * max + j])
        }
    }
    # constructs dataframe
    rel <- data.frame(
        coder1 = coder1$analysis,
        coder2 = c2
    )
} else {
    # cuts c1 for analysis
    c1 <- c()
    for (i in 1:(length(coder1$analysis) / max)) {
        for (j in 1:min) {
            c1 <- c(c1, coder1$analysis[(i - 1) * max + j])
        }
    }
    # constructs dataframe
    rel <- data.frame(
        coder1 = c1,
        coder2 = coder2$analysis
    )
}
# runs intraclass correlation
print(icc(rel, model = "twoway", type = "agreement", unit = "single"))

# write rel to a csv file
write.csv(rel, sprintf("% s_REL.csv", sub_id), row.names = FALSE)
