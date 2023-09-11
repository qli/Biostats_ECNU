### Lecture 1 codes
### Qin Li 20230910

library(openxlsx)

student_mon = read.xlsx(xlsxFile = "Data/student_list.xlsx", sheet = 1)
N = nrow(student_mon)
n = 6

# repeat
sample_index = sample(N, n)
#sample_index = sample(student_mon$index, n)
student_mon[sample_index,]
