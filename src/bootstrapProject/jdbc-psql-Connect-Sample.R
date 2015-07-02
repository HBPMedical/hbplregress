library(RJDBC)
drv <- JDBC("org.postgresql.Driver",
           "/home/neuroinfo/lib/jdbc/postgresql/postgresql.jar",
           identifier.quote="`")
conn <- dbConnect(drv, "jdbc:postgresql://localhost:5432/LDSM1", "federation", "HBP4thewin")

var01<-unlist(dbGetQuery(conn, "select tissue1_volume from brain_feature where feature_name='Hippocampus_L'"))
var02<-unlist(dbGetQuery(conn, "select tissue1_volume from brain_feature where feature_name='Hippocampus_R'"))

f1<-summary(lm(var01~var02))
f1
w1<-1/f1$coefficients["var02","Std. Error"]
w1
b1<-f1$coefficients["var02","Estimate"]
b1
