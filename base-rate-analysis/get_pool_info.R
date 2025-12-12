# Script to get pool information from each study
setwd("c:/Users/jcerv/Jose/diversity_feedback")

# Study 2
d2 <- read.csv('Study-2/Study2.csv', check.names=FALSE)
cat('Study 2 - N:', nrow(d2), '\n')
cat('Study 2 - base_gender range:', min(d2$base_gender), '-', max(d2$base_gender), '\n')
cat('Study 2 - mean base_gender:', round(mean(d2$base_gender), 2), '\n\n')

# Study 3A
d3a <- read.csv('Study-3A/Study3A.csv', check.names=FALSE)
cat('Study 3A - N:', nrow(d3a), '\n')
cat('Study 3A - base_race range:', min(d3a$base_race), '-', max(d3a$base_race), '\n')
cat('Study 3A - mean base_race:', round(mean(d3a$base_race), 2), '\n\n')

# Study 3B
d3b <- read.csv('Study-3B/Study3B.csv', check.names=FALSE)
cat('Study 3B - N:', nrow(d3b), '\n')
cat('Study 3B - base_gender range:', min(d3b$base_gender), '-', max(d3b$base_gender), '\n')
cat('Study 3B - mean base_gender:', round(mean(d3b$base_gender), 2), '\n\n')

# Study 4A
d4a <- read.csv('Study-4A/Study4A.csv', check.names=FALSE)
cat('Study 4A - N:', nrow(d4a), '\n')
cat('Study 4A - base_race range:', min(d4a$base_race), '-', max(d4a$base_race), '\n')
cat('Study 4A - mean base_race:', round(mean(d4a$base_race), 2), '\n\n')

# Study 4B
d4b <- read.csv('Study-4B/Study4B.csv', check.names=FALSE)
cat('Study 4B - N:', nrow(d4b), '\n')
cat('Study 4B - base_gender range:', min(d4b$base_gender), '-', max(d4b$base_gender), '\n')
cat('Study 4B - mean base_gender:', round(mean(d4b$base_gender), 2), '\n\n')

# Study 5
d5 <- read.csv('Study-5/Study5.csv', check.names=FALSE)
cat('Study 5 - N:', nrow(d5), '\n')
cat('Study 5 - base_gender range:', min(d5$base_gender), '-', max(d5$base_gender), '\n')
cat('Study 5 - mean base_gender:', round(mean(d5$base_gender), 2), '\n')
cat('Study 5 - mean base_gender (men pool):', round(mean(d5$base_gender[d5$men_pool==1]), 2), '\n')
cat('Study 5 - mean base_gender (women pool):', round(mean(d5$base_gender[d5$men_pool==0]), 2), '\n')
