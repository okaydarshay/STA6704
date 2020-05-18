data <- HRDataset
dim(data) #dimensions of dataset
head(data)
data <- data[complete.cases(data),] #removeNAs
v_drop <- c("Employee_Name","EmpID","Zip","DateofTermination","ManagerName","LastPerformanceReview_Date","DaysLateLast30","TermReason","DOB","DateofHire","Position") #remove columns
M <- data[,!(names(data)%in%v_drop)] #creates matrix without specified columns
dim(M) #verifying removed columns

#dissimilarity matrix clustering (heirarchical)
daisy_M <- cluster::daisy(
  x = M[,1:23],
  metric = "gower"
)

list_hclust <- list()
v_hclust <- c(
  "ward.D","single"#,"ward.D2","complete","mcquitty","average","median","centroid"
  )
for(j in v_hclust){
  list_hclust[[j]] <- hclust(
    d = daisy_M,
    method = j
  )
}
plot(list_hclust[["ward.D"]])

#agnes heirarchical
agnes_M <- cluster::agnes(M)

#diana heirarchical
diana_M <- cluster::diana(M)
plot(diana_M)

#need only numerical columns for partitioning
v_keep <- c("MarriedID","MaritalStatusID","GenderID","EmpStatusID","DeptID","PerfScoreID",
            "FromDiversityJobFairID","PayRate","Termd","PositionID","ManagerID","EngagementSurvey",
            "EmpSatisfaction","SpecialProjectsCount")
M1 <- data[,v_keep] #creates matrix with above columns

#initial pam

pam_M1 <- cluster::pam(M1, 5)
plot(pam_M1)
print(pam_M1)

#kmeans
#kmeans(M1, 2)

#CLARA
clara_M1 <- cluster::clara(M1, 5)
print(clara_M1)
plot(clara_M1)


#fanny
fanny_M <- cluster::fanny(M1, 2)
print(fanny_M)
plot(fanny_M)







