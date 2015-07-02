library(RJDBC)
drv <- JDBC("com.denodo.vdp.jdbc.Driver",
           "/home/neuroinfo/lib/jdbc/vdp-jdbcdriver-core/denodo-vdp-jdbcdriver.jar",
           identifier.quote="`")
conn <- dbConnect(drv, "jdbc:vdb://localhost:9999/hbp_chuv", "admin", "admin")
dbGetQuery(conn, "select count(*) from patient")