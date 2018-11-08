#10/18/2018 Testing Oracle database access through R using dbplyr

# Load RODBC package to check drivers
library(RODBC)
print(odbcDataSources(type = c("all", "user", "system")))  



#connecting to database
library(odbc)
#Easy Way
con <- DBI::dbConnect(odbc::odbc(),
                      DSN = "Oracle Test",                                      #Destination Source Name
                      PWD = "R33f_f1sh_2018"                                   #Password
)
#Apparently you only need dsn and password, schema doesn't appear to work

# #More Precise Way, having issues with the tns names
# con <- DBI::dbConnect(odbc::odbc(),
#                       Driver = "Oracle in OraClient12Home1",                    #Driver Name
#                       SVC = "FPC_CRUISE_OBJ",                                   #schema
#                       #Host = "MSDBPR64",                                        #Host
#                       #Host = "C:/app64/product/12.2.0/client_1/network/admin/tsnnames.ora",                                        #Host
#                       UID = "akemberling",                                      #User
#                       PWD = "R33f_f1sh_2018",                                   #Password
#                       #PWD = rstudioapi::askForPassword("Database password"),   #makes R ask for password
#                       Port = 15121
# )


#Accessing the data
library(dbplyr)
library(dplyr)

#Accessing different schema
df <- tbl(con, in_schema("FPC_CRUISE_OBJ", "ACTIVITY_CODES"))  #Use this if you didn't enter a schema

#tbl(con, "ACTIVITY_CODES")                               #use this if you have schema set somehow
#con %>% dbplyr::tbl(FPC_CRUISE_OBJ) %>% tbl(ACTIVITY_CODES)



#
#Testing dbplyr on Red Snapper Datasets
df <- tbl(con, in_schema("FPC_CRUISE_OBJ", "EVENTS"))


new.df <- df %>% select(STATIONKEY, EVENT_ID, EVENT_TYPE) %>% filter(EVENT_TYPE== "Longline") 
left_join()           







