### Lecture 1 codes
### Qin Li 20230910

library(openxlsx)

student_mon = read.xlsx(xlsxFile = "Data/student_list.xlsx", sheet = 1)
N = nrow(student_mon)
n = 6

# could repeat the below steps
sample_index = sample(1:N, 4)
#sample_index = sample(student_mon$index, n)
student_mon[sample_index,]

student_wed = read.xlsx(xlsxFile = "Data/student_list.xlsx", sheet = 2)
N = nrow(student_wed)
n = 2
sample_index = sample(1:N, n)
student_wed[sample_index,]
