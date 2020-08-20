
# Demo of using source() to clean up code

# This is the file being sourced!
# It would load your data and clean it up however you like.

library( tidyverse )

scores = tibble::tribble(
  ~Name, ~Group, ~Score,  ~Y,
  "A",     1,   100, 32,
  "B",     1,   210, 33,
  "A2",    1,   120, 24,
  "B2",    1,   200, 22,
  "A3",    1,   110, 21,
  "B3",    1,   220, 35,
  "C",     1,   270, 11,
  "D",     2,   250, 32,
  "D",     2,   250, 42,
  "E",     2,   200, 54,
  "F",     3,   220, 32,
  "G",     3,   90,  13,
  "G",     3,   70,  14,
  "G",     3,   80,  10,
  "G",     3,   120, 21,
  "H",     4,   90,  13,
  "F",     4,   120, 12,
  "F",     4,   130, 11,
  "F2",    4,   100, 16,
  "F3",    4,   115, 12,
  "G",     5,   210, 41,
  "G",     5,   210, 49,
  "H",     5,   290, 53,
  "I",     6,   150, 55
)



scores = scores %>% group_by( Group ) %>%
  mutate( mean.score = mean( Score ),
          score.c = Score - mean.score )


scores$Group = as.factor( scores$Group )

scores

# Here might be some exploratory code you want to keep, but not run all the time.
if ( FALSE ) {
  
 table( scores$Group )
  
}