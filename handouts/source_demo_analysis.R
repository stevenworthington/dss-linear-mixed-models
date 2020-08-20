#
# Demo of the source() function
#
# This demo has two files: source_demo_analysis.R (the main file that sources
# another subsidary file) and source_demo_prepare_data.R (the file getting
# sourced)
#
# Overview: This simple demo of two files shows how you could have data
# preparation and cleaning stored in one file, and then you simply "source" the
# file at the top of your other file to run it all. This keeps code clutter
# down, so you don't have to scroll through it all the time when focusing on
# something else.
#
# I recommend using .R files for primary analysis, and then making an Rmd file
# when making your final report.  That being said, this file could easily be an
# Rmd file.
#
# The data prep file you source would have to be a .R file, however.
# 


#
# This is the file where you would put a bunch of analysis or exploration of
# your data.
#
# 


# Load your packages
library( tidyverse )
library( lme4 )
library( arm )


# Source the other file that prepares your data
source( "source_demo_prepare_data.R" )


# Do your analysis!
M0 = lmer( Y ~ score.c + mean.score + (1|Group), data=scores )

display( M0 )


ggplot( scores, aes( Score, Y, col=Group ) ) +
  facet_wrap( ~  Group ) +
  geom_point() 

ggplot( scores, aes( score.c, Y, col=Group ) ) +
  geom_point() 
