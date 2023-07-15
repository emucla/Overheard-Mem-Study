# !!! TODO BEFORE RUNNING !!! (look for "!!!")
# ! update path of file
# ! change name of output file

# !!! change path of file
setwd("C:/Users/shaop/Downloads")

library(tidyverse)
library(plyr)
library(irr)
library(compare)
library(arsenal)
library(tidyr)

# format function - reads and formats individual csv file
# RETURN:
# dataframe containing columns:
# SubID = participant ID
# Trial = trial num
# response = response of child (empty)
# incor_choice = choice if incorrect
# tot_dur = total duration of labeling events
# tar_dur = target duration
# people_dur = people duration
# netural_dur = netural dur
# cond = condition (massed/spaced)
# trial_time = total time of trial
format <- function(codes_csv) {
    # reads csv into dataframe
    data <- data.frame(read.csv(codes_csv))

    print(deparse(substitute(codes_csv)))
    sub_id <- substr(deparse(substitute(codes_csv)), 2, 3) %>% strtoi()
    print(sub_id)

    # intializes vectors
    trial_nums <- durations <- times <- targets <- neutrals <- distractors <- persons <- c() # nolint

    # intializes indices for looping through data & storing values
    cond <- "massed"
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
            cond <- "spaced"
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
                trial_nums <- c(trial_nums, cur_trial)
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
    trial_nums <- c(trial_nums, cur_trial)
    # preps dataframe for return, rounds off numbers for analysis
    res <- data.frame(
        sub_id = sub_id,
        trial = trial_nums,
        response = NA,
        incor_choice = NA,
        tot_dur = durations,
        tar_dur = targets,
        people_dur = persons,
        dis_dur = distractors,
        neutral_dur = neutrals,
        cond = cond,
        trial_time = times
    )
    # print(res)
    return(res)
}

data <- c("05_SW.csv", "16_SW.csv")
# c("02_SW.csv", "03_SW.csv", "04_SW.csv", "05_SW.csv",
#     "06_SW.csv", "07_SW.csv", "08_SW.csv", "09_SW.csv", "12_SW.csv",
#     "13_SW.csv", "14_SW.csv", "16_SW.csv", "19_SW.csv", "22_SW.csv",
#     "23_SW.csv", "24_SW.csv", "25_SW.csv", "28_SW.csv", "29_SW.csv",
#     "30_SW.csv", "31_SW.csv", "32_SW.csv", "33_SW.csv", "34_SW.csv",
#     "37_SW.csv", "38_SW.csv")

final <- format("04_SW.csv")

for (csv in data) {
    final <- rbind(final, format(csv))
}

# print(final)
