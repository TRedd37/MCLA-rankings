#!/bin/bash

Rscript ~/Github/MCLA-rankings/cron/absolute.R &
Rscript ~/Github/MCLA-rankings/cron/least_squares.R &
Rscript ~/Github/MCLA-rankings/cron/logit.R &
Rscript ~/Github/MCLA-rankings/cron/step.R &