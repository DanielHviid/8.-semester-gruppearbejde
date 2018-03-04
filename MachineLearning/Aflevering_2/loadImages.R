# load dataset and safe it in a data file for reuse
subjectsSmall = list(list(1), list(1), list(1))
subjectsLarge = list(list(1), list(1,2), list(1,2,3), list(1,2,3), list(1,2,3,4), list(1,2,3,4,5), list(0,1))

#load data
id100Small <- data.frame(getAllData(subjectsSmall, 100))
id100Large <- data.frame(getAllData(subjectsLarge, 100))

id200Small <- data.frame(getAllData(subjectsSmall, 200))
id200Large <- data.frame(getAllData(subjectsLarge, 200))

id300Small <- data.frame(getAllData(subjectsSmall, 300))
id300Large <- data.frame(getAllData(subjectsLarge, 300))

# safe for later use
