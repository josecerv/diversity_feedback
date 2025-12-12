# Get unique choices from each study to determine pool size
setwd("c:/Users/jcerv/Jose/diversity_feedback")

# Study 2
d2 <- read.csv('Study-2/Study2.csv', check.names=FALSE)
cols2 <- c('choice-1', 'choice-2', 'choice-3', 'choice-4', 'choice-5', 'choice-6', 'choice-7')
choices2 <- unique(unlist(d2[, cols2]))
choices2 <- choices2[!is.na(choices2) & choices2 != '']
cat('Study 2 - Total unique options:', length(choices2), '\n')
print(sort(choices2))
cat('\n')

# Study 3A
d3a <- read.csv('Study-3A/Study3A.csv', check.names=FALSE)
cols3a <- c('choice-1', 'choice-2', 'choice-3', 'choice-4', 'choice-5', 'choice-6', 'choice-7', 'choice-8')
choices3a <- unique(unlist(d3a[, cols3a]))
choices3a <- choices3a[!is.na(choices3a) & choices3a != '']
cat('Study 3A - Total unique options:', length(choices3a), '\n')
print(sort(choices3a))
cat('\n')

# Study 3B
d3b <- read.csv('Study-3B/Study3B.csv', check.names=FALSE)
cols3b <- c('choice-1', 'choice-2', 'choice-3', 'choice-4', 'choice-5', 'choice-6', 'choice-7')
choices3b <- unique(unlist(d3b[, cols3b]))
choices3b <- choices3b[!is.na(choices3b) & choices3b != '']
cat('Study 3B - Total unique options:', length(choices3b), '\n')
print(sort(choices3b))
cat('\n')

# Study 4A
d4a <- read.csv('Study-4A/Study4A.csv', check.names=FALSE)
cols4a <- c('selection_1', 'selection_2', 'selection_3', 'selection_4', 'selection_5', 'selection_6', 'bonus_ctrl1_7', 'bonus_ctrl2_7', 'bonus_trt_7')
choices4a <- unique(unlist(d4a[, cols4a]))
choices4a <- choices4a[!is.na(choices4a) & choices4a != '']
cat('Study 4A - Total unique options:', length(choices4a), '\n')
print(sort(choices4a))
cat('\n')

# Study 4B
d4b <- read.csv('Study-4B/Study4B.csv', check.names=FALSE)
cols4b <- c('choice-1', 'choice-2', 'choice-3', 'choice-4', 'choice-5', 'choice-6', 'choice-7')
choices4b <- unique(unlist(d4b[, cols4b]))
choices4b <- choices4b[!is.na(choices4b) & choices4b != '']
cat('Study 4B - Total unique options:', length(choices4b), '\n')
print(sort(choices4b))
cat('\n')

# Study 5
d5 <- read.csv('Study-5/Study5.csv', check.names=FALSE)
cols5 <- c('choice-1', 'choice-2', 'choice-3', 'choice-4', 'choice-5', 'choice-6', 'choice-7')
choices5 <- unique(unlist(d5[, cols5]))
choices5 <- choices5[!is.na(choices5) & choices5 != '']
cat('Study 5 - Total unique options:', length(choices5), '\n')
print(sort(choices5))
