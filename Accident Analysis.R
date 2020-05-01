library(readxl)
library(sqldf)
library(readr)
library(plyr)
library(compareGroups)
library(geosphere)
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(base)
library(randomForest)
library(varhandle)
library(data.table)
library(gridExtra)
library(UBL)
library(utiml)
library(caret)
library(DMwR)


#Read all the datasets
accidents_2015 <- read.csv("c:/users/harsh/documents/project resources/datasets for accidents/accidents/accidents_2015.csv")
accidents_2016 <- read.csv("c:/users/harsh/documents/project resources/datasets for accidents/accidents/accidents_2016.csv")


AADF <- read.csv("c:/users/harsh/documents/project resources/datasets for accidents/traffic flow/Average annual daily flow-major-mior roads.csv")
veh_2015 <- read.csv("c:/users/harsh/documents/project resources/datasets for accidents/vehicles/Vehicles_2015.csv")
veh_2016 <- read.csv("c:/users/harsh/documents/project resources/datasets for accidents/vehicles/Vehicles_2016.csv")



#Extract 2 years of data from AADF
AADF2015 <- sqldf('Select * from AADF where year = 2015')
AADF2016 <- sqldf('Select * from AADF where year = 2016')
AADF_2015_2016 <- sqldf('Select * from AADF where year = 2015 or year = 2016')

#Check for the number of incomplete rows in datasets
nrow(accidents_2015[!complete.cases(accidents_2015),])
accidents_2015 <- na.omit(accidents_2015)
nrow(accidents_2016[!complete.cases(accidents_2016),])
accidents_2016 <- na.omit(accidents_2016)
nrow(AADF[!complete.cases(AADF),])

#find out columns having values -1, which means missing data
sapply(accidents_2015, function(x)sum(x=='-1'))
#delete the columns with more than 30% missing values i.e missing value as -1
accidents_2015 <- select(accidents_2015, - c( X2nd_Road_Class,Junction_Control))
accidents_2016 <- select(accidents_2016, - c( X2nd_Road_Class,Junction_Control))



#Changing values of Local_authority_district from number to a string so they are compatible with AADF dataset
library(plyr)
map_districtname <- function(db){
   mapvalues(db$Local_Authority_.District., from = c(1,2,3,
                                                                                                           4,
                                                                                                           5,
                                                                                                           6,
                                                                                                           7,
                                                                                                           8,
                                                                                                           9,
                                                                                                           10,
                                                                                                           11,
                                                                                                           12,
                                                                                                           13,
                                                                                                           14,
                                                                                                           15,
                                                                                                           16,
                                                                                                           17,
                                                                                                           18,
                                                                                                           19,
                                                                                                           20,
                                                                                                           21,
                                                                                                           22,
                                                                                                           23,
                                                                                                           24,
                                                                                                           25,
                                                                                                           26,
                                                                                                           27,
                                                                                                           28,
                                                                                                           29,
                                                                                                           30,
                                                                                                           31,
                                                                                                           32,
                                                                                                           33,
                                                                                                           38,
                                                                                                           40,
                                                                                                           57,
                                                                                                           60,
                                                                                                           61,
                                                                                                           62,
                                                                                                           63,
                                                                                                           64,
                                                                                                           65,
                                                                                                           70,
                                                                                                           71,
                                                                                                           72,
                                                                                                           73,
                                                                                                           74,
                                                                                                           75,
                                                                                                           76,
                                                                                                           77,
                                                                                                           79,
                                                                                                           80,
                                                                                                           82,
                                                                                                           83,
                                                                                                           84,
                                                                                                           85,
                                                                                                           90,
                                                                                                           91,
                                                                                                           92,
                                                                                                           93,
                                                                                                           95,
                                                                                                           100,
                                                                                                           101,
                                                                                                           102,
                                                                                                           104,
                                                                                                           106,
                                                                                                           107,
                                                                                                           109,
                                                                                                           110,
                                                                                                           112,
                                                                                                           114,
                                                                                                           120,
                                                                                                           121,
                                                                                                           122,
                                                                                                           123,
                                                                                                           124,
                                                                                                           126,
                                                                                                           127,
                                                                                                           128,
                                                                                                           129,
                                                                                                           130,
                                                                                                           139,
                                                                                                           140,
                                                                                                           141,
                                                                                                           142,
                                                                                                           143,
                                                                                                           144,
                                                                                                           145,
                                                                                                           146,
                                                                                                           147,
                                                                                                           148,
                                                                                                           149,
                                                                                                           150,
                                                                                                           160,
                                                                                                           161,
                                                                                                           162,
                                                                                                           163,
                                                                                                           164,
                                                                                                           165,
                                                                                                           166,
                                                                                                           168,
                                                                                                           169,
                                                                                                           180,
                                                                                                           181,
                                                                                                           182,
                                                                                                           184,
                                                                                                           185,
                                                                                                           186,
                                                                                                           187,
                                                                                                           189,
                                                                                                           200,
                                                                                                           202,
                                                                                                           203,
                                                                                                           204,
                                                                                                           206,
                                                                                                           210,
                                                                                                           211,
                                                                                                           213,
                                                                                                           215,
                                                                                                           228,
                                                                                                           231,
                                                                                                           232,
                                                                                                           233,
                                                                                                           240,
                                                                                                           241,
                                                                                                           243,
                                                                                                           245,
                                                                                                           250,
                                                                                                           251,
                                                                                                           252,
                                                                                                           253,
                                                                                                           254,
                                                                                                           255,
                                                                                                           256,
                                                                                                           257,
                                                                                                           258,
                                                                                                           270,
                                                                                                           273,
                                                                                                           274,
                                                                                                           276,
                                                                                                           277,
                                                                                                           278,
                                                                                                           279,
                                                                                                           280,
                                                                                                           281,
                                                                                                           282,
                                                                                                           283,
                                                                                                           284,
                                                                                                           285,
                                                                                                           286,
                                                                                                           290,
                                                                                                           291,
                                                                                                           292,
                                                                                                           293,
                                                                                                           294,
                                                                                                           300,
                                                                                                           302,
                                                                                                           303,
                                                                                                           305,
                                                                                                           306,
                                                                                                           307,
                                                                                                           309,
                                                                                                           320,
                                                                                                           321,
                                                                                                           322,
                                                                                                           323,
                                                                                                           324,
                                                                                                           325,
                                                                                                           327,
                                                                                                           328,
                                                                                                           329,
                                                                                                           340,
                                                                                                           341,
                                                                                                           342,
                                                                                                           343,
                                                                                                           344,
                                                                                                           345,
                                                                                                           346,
                                                                                                           347,
                                                                                                           350,
                                                                                                           351,
                                                                                                           352,
                                                                                                           353,
                                                                                                           354,
                                                                                                           355,
                                                                                                           356,
                                                                                                           360,
                                                                                                           361,
                                                                                                           362,
                                                                                                           363,
                                                                                                           364,
                                                                                                           365,
                                                                                                           366,
                                                                                                           367,
                                                                                                           368,
                                                                                                           380,
                                                                                                           381,
                                                                                                           382,
                                                                                                           383,
                                                                                                           384,
                                                                                                           385,
                                                                                                           386,
                                                                                                           390,
                                                                                                           391,
                                                                                                           392,
                                                                                                           393,
                                                                                                           394,
                                                                                                           395,
                                                                                                           400,
                                                                                                           401,
                                                                                                           402,
                                                                                                           404,
                                                                                                           405,
                                                                                                           406,
                                                                                                           407,
                                                                                                           410,
                                                                                                           411,
                                                                                                           412,
                                                                                                           413,
                                                                                                           414,
                                                                                                           415,
                                                                                                           416,
                                                                                                           420,
                                                                                                           421,
                                                                                                           422,
                                                                                                           423,
                                                                                                           424,
                                                                                                           430,
                                                                                                           431,
                                                                                                           432,
                                                                                                           433,
                                                                                                           434,
                                                                                                           435,
                                                                                                           436,
                                                                                                           437,
                                                                                                           438,
                                                                                                           450,
                                                                                                           451,
                                                                                                           452,
                                                                                                           453,
                                                                                                           454,
                                                                                                           455,
                                                                                                           456,
                                                                                                           457,
                                                                                                           458,
                                                                                                           459,
                                                                                                           460,
                                                                                                           461,
                                                                                                           462,
                                                                                                           463,
                                                                                                           470,
                                                                                                           471,
                                                                                                           472,
                                                                                                           473,
                                                                                                           474,
                                                                                                           475,
                                                                                                           476,
                                                                                                           477,
                                                                                                           478,
                                                                                                           479,
                                                                                                           480,
                                                                                                           481,
                                                                                                           482,
                                                                                                           483,
                                                                                                           484,
                                                                                                           485,
                                                                                                           490,
                                                                                                           491,
                                                                                                           492,
                                                                                                           493,
                                                                                                           494,
                                                                                                           495,
                                                                                                           496,
                                                                                                           497,
                                                                                                           498,
                                                                                                           499,
                                                                                                           500,
                                                                                                           501,
                                                                                                           502,
                                                                                                           505,
                                                                                                           510,
                                                                                                           511,
                                                                                                           512,
                                                                                                           513,
                                                                                                           514,
                                                                                                           515,
                                                                                                           516,
                                                                                                           517,
                                                                                                           518,
                                                                                                           530,
                                                                                                           531,
                                                                                                           532,
                                                                                                           533,
                                                                                                           535,
                                                                                                           536,
                                                                                                           538,
                                                                                                           539,
                                                                                                           540,
                                                                                                           541,
                                                                                                           542,
                                                                                                           543,
                                                                                                           544,
                                                                                                           551,
                                                                                                           552,
                                                                                                           554,
                                                                                                           555,
                                                                                                           556,
                                                                                                           557,
                                                                                                           558,
                                                                                                           559,
                                                                                                           560,
                                                                                                           562,
                                                                                                           563,
                                                                                                           564,
                                                                                                           565,
                                                                                                           570,
                                                                                                           580,
                                                                                                           581,
                                                                                                           582,
                                                                                                           583,
                                                                                                           584,
                                                                                                           585,
                                                                                                           586,
                                                                                                           587,
                                                                                                           588,
                                                                                                           589,
                                                                                                           590,
                                                                                                           591,
                                                                                                           592,
                                                                                                           593,
                                                                                                           594,
                                                                                                           595,
                                                                                                           596,
                                                                                                           601,
                                                                                                           605,
                                                                                                           606,
                                                                                                           607,
                                                                                                           608,
                                                                                                           609,
                                                                                                           610,
                                                                                                           611,
                                                                                                           612,
                                                                                                           620,
                                                                                                           621,
                                                                                                           622,
                                                                                                           623,
                                                                                                           624,
                                                                                                           625,
                                                                                                           630,
                                                                                                           631,
                                                                                                           632,
                                                                                                           633,
                                                                                                           634,
                                                                                                           635,
                                                                                                           640,
                                                                                                           641,
                                                                                                           642,
                                                                                                           643,
                                                                                                           644,
                                                                                                           645,
                                                                                                           646,
                                                                                                           647,
                                                                                                           720,
                                                                                                           721,
                                                                                                           722,
                                                                                                           723,
                                                                                                           724,
                                                                                                           725,
                                                                                                           730,
                                                                                                           731,
                                                                                                           732,
                                                                                                           733,
                                                                                                           734,
                                                                                                           740,
                                                                                                           741,
                                                                                                           742,
                                                                                                           743,
                                                                                                           744,
                                                                                                           745,
                                                                                                           746,
                                                                                                           750,
                                                                                                           751,
                                                                                                           752,
                                                                                                           753,
                                                                                                           910,
                                                                                                           911,
                                                                                                           912,
                                                                                                           913,
                                                                                                           914,
                                                                                                           915,
                                                                                                           916,
                                                                                                           917,
                                                                                                           918,
                                                                                                           919,
                                                                                                           920,
                                                                                                           921,
                                                                                                           922,
                                                                                                           923,
                                                                                                           924,
                                                                                                           925,
                                                                                                           926,
                                                                                                           927,
                                                                                                           928,
                                                                                                           929,
                                                                                                          930,
                                                                                                           931,
                                                                                                           932,
                                                                                                           933,
                                                                                                           934,
                                                                                                           935,
                                                                                                           936,
                                                                                                           937,
                                                                                                           938,
                                                                                                           939,
                                                                                                           940,
                                                                                                           941), 
                                                       to = c("Westminster","Camden","Islington","Hackney","Tower Hamlets",
                                                                                                                          "Greenwich",
                                                                                                                          "Lewisham",
                                                                                                                          "Southwark",
                                                                                                                          "Lambeth",
                                                                                                                          "Wandsworth",
                                                                                                                          "Hammersmith and Fulham",
                                                                                                                          "Kensington and Chelsea",
                                                                                                                          "Waltham Forest",
                                                                                                                          "Redbridge",
                                                                                                                          "Havering",
                                                                                                                          "Barking and Dagenham",
                                                                                                                          "Newham",
                                                                                                                          "Bexley",
                                                                                                                          "Bromley",
                                                                                                                          "Croydon",
                                                                                                                          "Sutton",
                                                                                                                          "Merton",
                                                                                                                          "Kingston upon Thames",
                                                                                                                          "Richmond upon Thames",
                                                                                                                          "Hounslow",
                                                                                                                          "Hillingdon",
                                                                                                                          "Ealing",
                                                                                                                          "Brent",
                                                                                                                          "Harrow",
                                                                                                                          "Barnet",
                                                                                                                          "Haringey",
                                                                                                                          "Enfield",
                                                                                                                          "Hertsmere",
                                                                                                                          "Epsom and Ewell",
                                                                                                                          "Spelthorne",
                                                                                                                          "London Airport (Heathrow)",
                                                                                                                          "Allerdale",
                                                                                                                          "Barrow-in-Furness",
                                                                                                                          "Carlisle",
                                                                                                                          "Copeland",
                                                                                                                          "Eden",
                                                                                                                          "South Lakeland",
                                                                                                                          "Blackburn with Darwen",
                                                                                                                          "Blackpool",
                                                                                                                          "Burnley",
                                                                                                                          "Chorley",
                                                                                                                          "Fylde",
                                                                                                                          "Hyndburn",
                                                                                                                          "Lancaster",
                                                                                                                          "Pendle",
                                                                                                                          "Preston",
                                                                                                                          "Ribble Valley",
                                                                                                                          "Rossendale",
                                                                                                                          "South Ribble",
                                                                                                                          "West Lancashire",
                                                                                                                          "Wyre",
                                                                                                                          "Knowsley",
                                                                                                                          "Liverpool",
                                                                                                                          "St. Helens",
                                                                                                                          "Sefton",
                                                                                                                          "Wirral",
                                                                                                                          "Bolton",
                                                                                                                          "Bury",
                                                                                                                          "Manchester",
                                                                                                                          "Oldham",
                                                                                                                          "Rochdale",
                                                                                                                          "Salford",
                                                                                                                          "Stockport",
                                                                                                                          "Tameside",
                                                                                                                          "Trafford",
                                                                                                                          "Wigan",
                                                                                                                          "Chester",
                                                                                                                          "Congleton",
                                                                                                                          "Crewe and Nantwich",
                                                                                                                          "Ellesmere Port and Neston",
                                                                                                                          "Halton",
                                                                                                                          "Macclesfield",
                                                                                                                          "Vale Royal",
                                                                                                                          "Warrington",
                                                                                                                          "Cheshire East",
                                                                                                                          "Cheshire West and Chester",
                                                                                                                          "Northumberland",
                                                                                                                          "Alnwick",
                                                                                                                          "Berwick-upon-Tweed",
                                                                                                                          "Blyth Valley",
                                                                                                                          "Castle Morpeth",
                                                                                                                          "Tynedale",
                                                                                                                          "Wansbeck",
                                                                                                                          "Gateshead",
                                                                                                                          "Newcastle upon Tyne",
                                                                                                                          "North Tyneside",
                                                                                                                          "South Tyneside",
                                                                                                                          "Sunderland",
                                                                                                                          "Chester-le-Street",
                                                                                                                          "Darlington",
                                                                                                                          "Derwentside",
                                                                                                                          "Durham",
                                                                                                                          "Easington",
                                                                                                                          "Sedgefield",
                                                                                                                          "Teesdale",
                                                                                                                          "Wear Valley",
                                                                                                                          "County Durham",
                                                                                                                          "Craven",
                                                                                                                          "Hambleton",
                                                                                                                          "Harrogate",
                                                                                                                          "Richmondshire",
                                                                                                                          "Ryedale",
                                                                                                                          "Scarborough",
                                                                                                                          "Selby",
                                                                                                                          "York",
                                                                                                                          "Bradford",
                                                                                                                          "Calderdale",
                                                                                                                          "Kirklees",
                                                                                                                          "Leeds",
                                                                                                                          "Wakefield",
                                                                                                                          "Barnsley",
                                                                                                                          "Doncaster",
                                                                                                                          "Rotherham",
                                                                                                                          "Sheffield",
                                                                                                                          "Kingston upon Hull, City of",
                                                                                                                          "East Riding of Yorkshire",
                                                                                                                          "North Lincolnshire",
                                                                                                                          "North East Lincolnshire",
                                                                                                                          "Hartlepool",
                                                                                                                          "Redcar and Cleveland",
                                                                                                                          "Middlesbrough",
                                                                                                                          "Stockton-on-Tees",
                                                                                                                          "Cannock Chase",
                                                                                                                          "East Staffordshire",
                                                                                                                          "Lichfield",
                                                                                                                          "Newcastle-under-Lyme",
                                                                                                                          "South Staffordshire",
                                                                                                                          "Stafford",
                                                                                                                          "Staffordshire Moorlands",
                                                                                                                          "Stoke-on-Trent",
                                                                                                                          "Tamworth",
                                                                                                                          "Bromsgrove",
                                                                                                                          "Malvern Hills",
                                                                                                                          "Redditch",
                                                                                                                          "Worcester",
                                                                                                                          "Wychavon",
                                                                                                                          "Wyre Forest",
                                                                                                                          "Bridgnorth",
                                                                                                                          "North Shropshire",
                                                                                                                          "Oswestry",
                                                                                                                          "Shrewsbury and Atcham",
                                                                                                                          "South Shropshire",
                                                                                                                          "Telford and Wrekin",
                                                                                                                          "Herefordshire, County of", 
                                                                                                                          "Shropshire",
                                                                                                                          "North Warwickshire",
                                                                                                                          "Nuneaton and Bedworth",
                                                                                                                          "Rugby",
                                                                                                                          "Stratford-upon-Avon",
                                                                                                                          "Warwick",
                                                                                                                          "Birmingham",
                                                                                                                          "Coventry",
                                                                                                                          "Dudley",
                                                                                                                          "Sandwell",
                                                                                                                          "Solihull",
                                                                                                                          "Walsall",
                                                                                                                          "Wolverhampton",
                                                                                                                          "Amber Valley",
                                                                                                                          "Bolsover",
                                                                                                                          "Chesterfield",
                                                                                                                          "Derby",
                                                                                                                          "Erewash",
                                                                                                                          "High Peak",
                                                                                                                          "North East Derbyshire",
                                                                                                                          "South Derbyshire",
                                                                                                                          "Derbyshire Dales",
                                                                                                                          "Ashfield",
                                                                                                                          "Bassetlaw",
                                                                                                                          "Broxtowe",
                                                                                                                          "Gedling",
                                                                                                                          "Mansfield",
                                                                                                                          "Newark and Sherwood",
                                                                                                                          "Nottingham",
                                                                                                                          "Rushcliffe",
                                                                                                                          "Boston",
                                                                                                                          "East Lindsey",
                                                                                                                          "Lincoln",
                                                                                                                          "North Kesteven",
                                                                                                                          "South Holland",
                                                                                                                          "South Kesteven",
                                                                                                                          "West Lindsey",
                                                                                                                          "Blaby",
                                                                                                                          "Hinckley and Bosworth",
                                                                                                                          "Charnwood",
                                                                                                                          "Harborough",
                                                                                                                          "Leicester",
                                                                                                                          "Melton",
                                                                                                                          "North West Leicestershire",
                                                                                                                          "Oadby and Wigston",
                                                                                                                          "Rutland",
                                                                                                                          "Corby",
                                                                                                                          "Daventry",
                                                                                                                          "East Northamptonshire",
                                                                                                                          "Kettering",
                                                                                                                          "Northampton",
                                                                                                                          "South Northamptonshire",
                                                                                                                          "Wellingborough",
                                                                                                                          "Cambridge",
                                                                                                                          "East Cambridgeshire",
                                                                                                                          "Fenland",
                                                                                                                          "Huntingdonshire",
                                                                                                                          "Peterborough",
                                                                                                                          "South Cambridgeshire",
                                                                                                                          "Breckland",
                                                                                                                          "Broadland",
                                                                                                                          "Great Yarmouth",
                                                                                                                          "Norwich",
                                                                                                                          "North Norfolk",
                                                                                                                          "South Norfolk",
                                                                                                                          "King''s Lynn and West Norfolk",
                                                                                                                          "Babergh",
                                                                                                                          "Forest Heath",
                                                                                                                          "Ipswich",
                                                                                                                          "Mid Suffolk",
                                                                                                                          "St. Edmundsbury",
                                                                                                                          "Suffolk Coastal",
                                                                                                                          "Waveney",
                                                                                                                          "Bedford",
                                                                                                                          "Luton",
                                                                                                                          "Mid Bedfordshire",
                                                                                                                          "South Bedfordshire",
                                                                                                                          "Central Bedfordshire",
                                                                                                                          "Broxbourne",
                                                                                                                          "Dacorum",
                                                                                                                          "East Hertfordshire",
                                                                                                                          "North Hertfordshire",
                                                                                                                          "St. Albans",
                                                                                                                          "Stevenage",
                                                                                                                          "Three Rivers",
                                                                                                                          "Watford",
                                                                                                                          "Welwyn Hatfield",
                                                                                                                          "Basildon",
                                                                                                                          "Braintree",
                                                                                                                          "Brentwood",
                                                                                                                          "Castle Point",
                                                                                                                          "Chelmsford",
                                                                                                                          "Colchester",
                                                                                                                          "Epping Forest",
                                                                                                                          "Harlow",
                                                                                                                          "Maldon",
                                                                                                                          "Rochford",
                                                                                                                          "Southend-on-Sea",
                                                                                                                          "Tendring",
                                                                                                                          "Thurrock",
                                                                                                                          "Uttlesford",
                                                                                                                          "Bracknell Forest",
                                                                                                                          "West Berkshire",
                                                                                                                          "Reading",
                                                                                                                          "Slough",
                                                                                                                          "Windsor and Maidenhead",
                                                                                                                          "Wokingham",
                                                                                                                          "Aylesbury Vale",
                                                                                                                          "South Bucks",
                                                                                                                          "Chiltern",
                                                                                                                          "Milton Keynes",
                                                                                                                          "Wycombe",
                                                                                                                          "Cherwell",
                                                                                                                          "Oxford",
                                                                                                                          "Vale of White Horse",
                                                                                                                          "South Oxfordshire",
                                                                                                                          "West Oxfordshire",
                                                                                                                          "Basingstoke and Deane",
                                                                                                                          "Eastleigh",
                                                                                                                          "Fareham",
                                                                                                                          "Gosport",
                                                                                                                          "Hart",
                                                                                                                          "Havant",
                                                                                                                          "New Forest",
                                                                                                                          "East Hampshire",
                                                                                                                          "Portsmouth",
                                                                                                                          "Rushmoor",
                                                                                                                          "Southampton", 
                                                                                                                          "Test Valley",
                                                                                                                          "Winchester",
                                                                                                                          "Isle of Wight",
                                                                                                                          "Elmbridge",
                                                                                                                          "Guildford",
                                                                                                                          "Mole Valley",
                                                                                                                          "Reigate and Banstead",
                                                                                                                          "Runnymede",
                                                                                                                          "Surrey Heath",
                                                                                                                          "Tandridge",
                                                                                                                          "Waverley",
                                                                                                                          "Woking",
                                                                                                                          "Ashford",
                                                                                                                          "Canterbury",
                                                                                                                          "Dartford",
                                                                                                                          "Dover",
                                                                                                                          "Gravesham",
                                                                                                                          "Maidstone",
                                                                                                                          "Sevenoaks",
                                                                                                                          "Shepway",
                                                                                                                          "Swale",
                                                                                                                          "Thanet",
                                                                                                                          "Tonbridge and Malling",
                                                                                                                          "Tunbridge Wells",
                                                                                                                          "Medway",
                                                                                                                          "Eastbourne",
                                                                                                                          "Hastings",
                                                                                                                          "Lewes",
                                                                                                                          "Rother",
                                                                                                                          "Wealden",
                                                                                                                          "Adur",
                                                                                                                          "Arun",
                                                                                                                          "Chichester",
                                                                                                                          "Crawley",
                                                                                                                          "Horsham",
                                                                                                                          "Mid Sussex",
                                                                                                                          "Worthing",
                                                                                                                          "Brighton and Hove",
                                                                                                                          "City of London",
                                                                                                                          "East Devon",
                                                                                                                          "Exeter",
                                                                                                                          "North Devon",
                                                                                                                          "Plymouth",
                                                                                                                          "South Hams",
                                                                                                                          "Teignbridge",
                                                                                                                          "Mid Devon",
                                                                                                                          "Torbay",
                                                                                                                          "Torridge",
                                                                                                                          "West Devon",
                                                                                                                          "Caradon",
                                                                                                                          "Carrick",
                                                                                                                          "Kerrier",
                                                                                                                          "North Cornwall",
                                                                                                                          "Penwith",
                                                                                                                          "Restormel",
                                                                                                                          "Cornwall",
                                                                                                                          "Bristol, City of",
                                                                                                                          "North Somerset",
                                                                                                                          "Mendip",
                                                                                                                          "Sedgemoor",
                                                                                                                          "Taunton Deane",
                                                                                                                          "West Somerset",
                                                                                                                          "South Somerset",
                                                                                                                          "Bath and North East Somerset",
                                                                                                                          "South Gloucestershire",
                                                                                                                          "Cheltenham",
                                                                                                                          "Cotswold",
                                                                                                                          "Forest of Dean",
                                                                                                                          "Gloucester",
                                                                                                                          "Stroud",
                                                                                                                          "Tewkesbury",
                                                                                                                          "Kennet",
                                                                                                                          "North Wiltshire",
                                                                                                                          "Salisbury",
                                                                                                                          "Swindon",
                                                                                                                          "West Wiltshire",
                                                                                                                          "Wiltshire",
                                                                                                                          "Bournemouth",
                                                                                                                          "Christchurch",
                                                                                                                          "North Dorset",
                                                                                                                          "Poole",
                                                                                                                          "Purbeck",
                                                                                                                          "West Dorset",
                                                                                                                          "Weymouth and Portland",
                                                                                                                          "East Dorset",
                                                                                                                          "Isle of Anglesey",
                                                                                                                          "Conwy",
                                                                                                                          "Gwynedd",
                                                                                                                          "Denbighshire",
                                                                                                                          "Flintshire",
                                                                                                                          "Wrexham",
                                                                                                                          "Blaenau Gwent",
                                                                                                                          "Caerphilly",
                                                                                                                          "Monmouthshire",
                                                                                                                          "Newport",
                                                                                                                          "Torfaen",
                                                                                                                          "Bridgend",
                                                                                                                          "Cardiff",
                                                                                                                          "Merthyr Tydfil",
                                                                                                                          "Neath Port Talbot",
                                                                                                                          "Rhondda, Cynon, Taff",
                                                                                                                          "Swansea",
                                                                                                                          "The Vale of Glamorgan",
                                                                                                                          "Ceredigion",
                                                                                                                          "Carmarthenshire",
                                                                                                                          "Pembrokeshire",
                                                                                                                          "Powys",
                                                                                                                          "Aberdeen City",
                                                                                                                          "Aberdeenshire",
                                                                                                                          "Angus",
                                                                                                                          "Argyll and Bute",
                                                                                                                          "Scottish Borders",
                                                                                                                          "Clackmannanshire",
                                                                                                                          "West Dunbartonshire",
                                                                                                                          "Dumfries and Galloway",
                                                                                                                          "Dundee City",
                                                                                                                          "East Ayrshire",
                                                                                                                          "East Dunbartonshire",
                                                                                                                          "East Lothian",
                                                                                                                          "East Renfrewshire",
                                                                                                                          "Edinburgh, City of",
                                                                                                                          "Falkirk",
                                                                                                                          "Fife",
                                                                                                                          "Glasgow City",
                                                                                                                          "Highland",
                                                                                                                          "Inverclyde",
                                                                                                                          "Midlothian",
                                                                                                                          "Moray",
                                                                                                                          "North Ayrshire",
                                                                                                                          "North Lanarkshire",
                                                                                                                          "Orkney Islands",
                                                                                                                          "Perth and Kinross",
                                                                                                                          "Renfrewshire",
                                                                                                                          "Shetland Islands",
                                                                                                                          "South Ayrshire",
                                                                                                                          "South Lanarkshire",
                                                                                                                          "Stirling",
                                                                                                                          "West Lothian",
                                                                                                                          "Western Isles"))}
accidents_2015$Local_Authority_.District. <- map_districtname(accidents_2015)
accidents_2016$Local_Authority_.District. <- map_districtname(accidents_2016)

#Change the values of 1st road class values from numeric to string
accidents_2015$X1st_Road_Class_str = mapvalues(accidents_2015$X1st_Road_Class, from = c(1,2,3,4,5,6), to = c('M','A', 'A','B','C','U'))
accidents_2016$X1st_Road_Class_str = mapvalues(accidents_2016$X1st_Road_Class, from = c(1,2,3,4,5,6), to = c('M','A', 'A','B','C','U'))

#Concatenate the 1st_Road_Class and 1st_Road_Number to create Road_number that is consistent with AADF database
accidents_2015$road_name <- with(accidents_2015, paste0(X1st_Road_Class_str,X1st_Road_Number))
accidents_2016$road_name <- with(accidents_2016, paste0(X1st_Road_Class_str,X1st_Road_Number))

#Drop the string road number after this
accidents_2015 <- subset(accidents_2015, select = -X1st_Road_Class_str)
accidents_2016 <- subset(accidents_2016, select = -X1st_Road_Class_str)

#Extract months from the dates column
accidents_2015$Month <- as.factor(strftime(x= as.Date(accidents_2015$Date),format = "%B"))
accidents_2016$Month <- as.factor(strftime(x= as.Date(accidents_2016$Date),format = "%B"))


#convert every minute interval of time into 24 hour intervals
accidents_2015$Time_Hour <- as.factor(format(strptime(x= as.character(accidents_2015$Time), format = "%H"),"%H"))
accidents_2016$Time_Hour <- as.factor(format(strptime(x= as.character(accidents_2016$Time), format = "%H"),"%H"))

#delete rows with NA in Time_Hour values
nrow(accidents_2015[!complete.cases(accidents_2015),])
accidents_2015 <- na.omit(accidents_2015)
nrow(accidents_2016[!complete.cases(accidents_2016),])
accidents_2016 <- na.omit(accidents_2016)

#Plot of time interval wrt Accident Severity and no of accidents
ggplot(accidents_2015)+geom_bar(aes(x=Time_Hour, fill = as.factor(Accident_Severity)))+theme_bw()  +labs(y = "Number of accidents",title= "Accident Severity vs Hour of the day")


library(dplyr)
#Convert caregorical variables into factors in the accidents database
cols <-c("Accident_Severity","Day_of_Week","X1st_Road_Class","Road_Type","Junction_Detail","Pedestrian_Crossing.Human_Control","Pedestrian_Crossing.Physical_Facilities","Light_Conditions","Weather_Conditions","Road_Surface_Conditions","Special_Conditions_at_Site","Carriageway_Hazards","Urban_or_Rural_Area")
accidents_2015[,cols] <-data.frame(apply(accidents_2015[cols],2,as.factor))
accidents_2016[,cols] <-data.frame(apply(accidents_2016[cols],2,as.factor))

chisq.test(accidents_2015$X1st_Road_Class,accidents_2015$Speed_limit)
chisq.test(accidents_2015$Accident_Severity,accidents_2015$Number_of_Casualties)
cor(accidents_2015$Longitude,accidents_2015$Location_Easting_OSGR)
cor(accidents_2015$Latitude,accidents_2015$Location_Northing_OSGR)

#Dropping unnecessary variables from accidents data
drop_columns <- c('Police_Force','X2nd_Road_Number','Local_Authority_.Highway.','Did_Police_Officer_Attend_Scene_of_Accident','LSOA_of_Accident_Location','Location_Easting_OSGR','Location_Northing_OSGR')
accidents_2015 <- accidents_2015 %>% select(-one_of(drop_columns)) 
accidents_2016 <- accidents_2016 %>% select(-one_of(drop_columns)) 

#merge some levels of variables together
levels(accidents_2015$Special_Conditions_at_Site) = c(0,0,1,2,3,4,5,6,7)
levels(accidents_2015$Carriageway_Hazards) = c(0,0,1,2,3,6,7)
levels(accidents_2015$Junction_Detail) <- c(0,0,1,2,3,5,6,7,8,9)
levels(accidents_2016$Special_Conditions_at_Site) = c(0,0,1,2,3,4,5,6,7)
levels(accidents_2016$Carriageway_Hazards) = c(0,0,1,2,3,6,7)
levels(accidents_2016$Junction_Detail) <- c(0,0,1,2,3,5,6,7,8,9)

#Plot of No of accidents vs various features
p1 =ggplot(accidents_2015)+geom_bar(aes(x=Day_of_Week, fill = as.factor(Accident_Severity)))+theme_bw()  +labs(y = "Number of accidents",title= "No of Accident vs Day of week")
p2 = ggplot(accidents_2015)+geom_bar(aes(x=X1st_Road_Class, fill = as.factor(Accident_Severity)))+theme_bw()  +labs(y = "Number of accidents",title= "No of Accidents vs Road_Class")
p3 = ggplot(accidents_2015)+geom_bar(aes(x=Road_Type, fill = as.factor(Accident_Severity)))+theme_bw()  +labs(y = "Number of accidents",title= "No of Accidents vs Road_Type")
p4 = ggplot(accidents_2015)+geom_bar(aes(x=Junction_Detail, fill = as.factor(Accident_Severity)))+theme_bw()  +labs(y = "Number of accidents",title= "No of Accidents vs Junction_Detail")
grid.arrange(p1,p2,p3,p4,ncol=2,nrow=2)


p1 =ggplot(accidents_2015)+geom_bar(aes(x=Junction_Control, fill = as.factor(Accident_Severity)))+theme_bw()  +labs(y = "Number of accidents",title= "No of Accident vs Junction Control")
p2 = ggplot(accidents_2015)+geom_bar(aes(x=Road_Surface_Conditions, fill = as.factor(Accident_Severity)))+theme_bw()  +labs(y = "Number of accidents",title= "No of Accidents vs Road Surface Conditions")
p3 = ggplot(accidents_2015)+geom_bar(aes(x=Light_Conditions, fill = as.factor(Accident_Severity)))+theme_bw()  +labs(y = "Number of accidents",title= "No of Accidents vs Light Conditions")
p4 = ggplot(accidents_2015)+geom_bar(aes(x=Weather_Conditions, fill = as.factor(Accident_Severity)))+theme_bw()  +labs(y = "Number of accidents",title= "No of Accidents vs Weather Conditions")
grid.arrange(p1,p2,p3,p4,ncol=2,nrow=2)

#############################################################################################################
#Clock plot
x <- table(accidents_2015$Time_Hour)

# Clock plot function
clock.plot <- function (x, col = rainbow(n), ...) {
  if( min(x)<0 ) x <- x - min(x)
  if( max(x)>1 ) x <- x/max(x)
  n <- length(x)
  if(is.null(names(x))) names(x) <- 0:(n-1)
  m <- 1.05
  plot(0, type = 'n', xlim = c(-m,m), ylim = c(-m,m), axes = F, xlab = '', ylab = '', ...)
  a <- pi/2 - 2*pi/200*0:200
  polygon( cos(a), sin(a) )
  v <- .02
  a <- pi/2 - 2*pi/n*0:n
  segments( (1+v)*cos(a), (1+v)*sin(a), (1-v)*cos(a), (1-v)*sin(a) )
  segments( cos(a), sin(a),0, 0, col = 'light grey', lty = 3) 
  ca <- -2*pi/n*(0:50)/50
  for (i in 1:n) {
    a <- pi/2 - 2*pi/n*(i-1)
    b <- pi/2 - 2*pi/n*i
    polygon( c(0, x[i]*cos(a+ca), 0), c(0, x[i]*sin(a+ca), 0), col=col[i] )
    v <- .1
    text((1+v)*cos(a), (1+v)*sin(a), names(x)[i])
  }
}

# Use the function on the created data
clock.plot(x, main = "Number of accidents for each hour of the day")

#clock plot for accidents on the monthly basis
x <- table(accidents_2015$Month)
clock.plot(x, main = "Number of accidents in each month")
ggplot(accidents_2015)+geom_bar(aes(x=Road_Type, fill = as.factor(Accident_Severity)))+theme_bw()  +labs(y = "Number of accidents",title= "No of Accidents vs Road_Type")


###################################################################################################################

#Analysis of AADF data
#Average count points in each year
sqldf('select avg(X) from (select count(distinct count_point_id) as X from AADF_2015_2016 group by year)')
#Different regions in AADF data
sqldf('select count(distinct region_id) from AADF_2015_2016') 
table(AADF_2015_2016$road_name)

#No of Local authorities in every region
sqldf('select region_name, count(distinct local_authority_id) from AADF_2015_2016 group by region_id')
#No of distinct road names
sqldf('select count(distinct road_name) from AADF_2015_2016') 

#No of count points on every road  in every authority in every region
sqldf('select region_name, local_authority_name, count(distinct count_point_id) from AADF_2015_2016 group by region_id, local_authority_id')

#Different count points on M1
sqldf('select road_name, count(distinct count_point_id) from AADF2015 where road_name = "M1"')
sqldf('select road_name, count(distinct count_point_id) from AADF2015 where road_name = "A101"')
sqldf('select region_name, local_authority_name,road_name, count(distinct count_point_id) from AADF_2015_2016 group by region_id, local_authority_id,road_name')

###################################################################################################################

#Analysis of vehicle datase
cols <- c("Vehicle_Type","Journey_Purpose_of_Driver","Sex_of_Driver","Age_Band_of_Driver","Vehicle_Manoeuvre")
veh_2015[,cols] <-lapply(veh_2015[,cols],factor)
veh_2016[,cols] <-lapply(veh_2016[,cols],factor)

#Merge similar levelsl together 
levels(veh_2015$Vehicle_Type) <-c(1,2,3,4,2,3,3,4,4,5,6,7,8,8,8,9,9,10,2,8,10)
levels(veh_2016$Vehicle_Type) <-c(1,2,3,4,2,3,3,4,4,5,6,7,8,8,8,9,9,10,2,8,10)


levels(veh_2015$Sex_of_Driver) <- c(3,1,2,3)
levels(veh_2016$Sex_of_Driver) <- c(3,1,2,3)


ggplot(veh_2015)+geom_bar(aes(x=Age_of_Driver))+ theme_light()+labs(y = "Number of accidents",title= "No of Accidents vs Age of Driver")
ggplot(veh_2015)+geom_bar(aes(x=Age_Band_of_Driver, fill = Sex_of_Driver))+ theme_light()+labs(y = "Number of accidents",title= "No of Accidents vs Age Band of Driver")
ggplot(veh_2015)+geom_bar(aes(x=Age_Band_of_Driver, fill = Sex_of_Driver))+ theme_light()+labs(y = "Number of accidents",title= "No of Accidents vs Age and sex of Driver")

#################################################################################################################

#Join accident and AADF2015 based on haversine distance between the accidet spot and the count point for calculating AADF
temp_2015 <- merge(accidents_2015, AADF2015, by = "road_name", all.x = TRUE)
temp_2015$dist <- distHaversine(temp_2015[,c("Longitude","Latitude")], temp_2015[,c("longitude","latitude")])
temp_2016 <- merge(accidents_2016, AADF2016, by = "road_name", all.x = TRUE)

temp_2016$dist <- distHaversine(temp_2016[,c("Longitude","Latitude")], temp_2016[,c("longitude","latitude")])


temp_2015$key <- paste(temp_2015$road_name,temp_2015$Latitude,temp_2015$Longitude)
temp_2016$key <- paste(temp_2016$road_name,temp_2016$Latitude,temp_2016$Longitude)

temp_2015 <- temp_2015 %>% arrange(dist) %>% distinct(key, .keep_all = TRUE)
temp_2016 <- temp_2016 %>% arrange(dist) %>% distinct(key, .keep_all = TRUE)

keep <- c("road_name", "Accident_Index","Longitude","Latitude","Accident_Severity","Number_of_Vehicles","Number_of_Casualties","Day_of_Week","Local_Authority_.District.","X1st_Road_Class", "X1st_Road_Number","Road_Type",                            
          "Junction_Detail","Pedestrian_Crossing.Human_Control","Pedestrian_Crossing.Physical_Facilities","Light_Conditions","Weather_Conditions",                   
          "Road_Surface_Conditions","Special_Conditions_at_Site","Carriageway_Hazards","Urban_or_Rural_Area","Date","Month","Time_Hour","all_motor_vehicles")
acc_AADF_15 <- temp_2015[keep]
acc_AADF_16 <- temp_2016[keep]

#No of NA values in the dataset for the all_motor_vehicle variable
sqldf("select count(*) from acc_AADF_15 where all_motor_vehicles is null")

################################################################################################################
#Using linear regression model to impute AADF values in the dataset

train <- acc_AADF_15[which(!is.na(acc_AADF_15$all_motor_vehicles)),]
test <- acc_AADF_15[which(is.na(acc_AADF_15$all_motor_vehicles)),]
model_lm_2015<- lm(all_motor_vehicles ~ Latitude+Longitude+X1st_Road_Class+Road_Type, data = train)
summary(model_lm_2015)
model_lm_2015$xlevels[["X1st_Road_Class"]] <- union(model_lm_2015$xlevels[["X1st_Road_Class"]], levels(test[["X1st_Road_Class"]]))
options(warn = -1)
test$all_motor_vehicles <- predict(model_lm_2015, test, type = "response")
acc_AADF_15 <- rbind(train,test)

train <- acc_AADF_16[which(!is.na(acc_AADF_16$all_motor_vehicles)),]
test <- acc_AADF_16[which(is.na(acc_AADF_16$all_motor_vehicles)),]
model_lm_2016 <- lm(all_motor_vehicles ~ Latitude+Longitude+X1st_Road_Class+Road_Type, data = train)
summary(model_lm_2016)
model_lm_2016$xlevels[["X1st_Road_Class"]] <- union(model_lm_2016$xlevels[["X1st_Road_Class"]], levels(test[["X1st_Road_Class"]]))
model_lm_2016$xlevels[["Road_Type"]] <- union(model_lm_2016$xlevels[["Road_Type"]], levels(test[["Road_Type"]]))
test$all_motor_vehicles <- predict(model_lm_2016, test, type = "response")
acc_AADF_16 <- rbind(train,test)
options(warn = 1)

####################################################################################################################
#Join Accident and vehicles databases
acc_AADF_veh_15 <- merge(acc_AADF_15,veh_2015[!duplicated(veh_2015$Accident_Index),], by = "Accident_Index", all.x = TRUE)
keep <- c("road_name", "Longitude","Latitude","Accident_Severity","Number_of_Vehicles","Number_of_Casualties","Day_of_Week","X1st_Road_Class", "Road_Type",                            
          "Junction_Detail","Pedestrian_Crossing.Human_Control","Pedestrian_Crossing.Physical_Facilities","Light_Conditions","Weather_Conditions",                   
          "Road_Surface_Conditions","Special_Conditions_at_Site","Carriageway_Hazards","Urban_or_Rural_Area","Date","Month","Time_Hour","all_motor_vehicles",
          "Vehicle_Type","Journey_Purpose_of_Driver","Sex_of_Driver","Age_Band_of_Driver","Vehicle_Manoeuvre","Age_of_Driver","Engine_Capacity_.CC.","Age_of_Vehicle")     
acc_AADF_veh_15 <- acc_AADF_veh_15[keep]

acc_AADF_veh_16<- merge(acc_AADF_16,veh_2016[!duplicated(veh_2016$Accident_Index),], by = "Accident_Index", all.x = TRUE)
acc_AADF_veh_16 <- acc_AADF_veh_16[keep]

combined_df <- rbind(acc_AADF_veh_15, acc_AADF_veh_16)
saveRDS(combined_df, 'C:/Users/harsh/Documents/R/Project1/combined_df.rds')
#Merging levels with minimal values
levels(combined_df$Weather_Conditions) 
levels(combined_df$Journey_Purpose_of_Driver) = c(6,1,2,3,4,5,6)
levels(combined_df$Weather_Conditions) = c(1,2,3,4,5,6,7,8,9,-1,1,2,3,4,5,6,7,8,9)
acc_AADF_veh_15$Age_of_Driver <- as.numeric(acc_AADF_veh_15$Age_of_Driver)
levels(combined_df$Light_Conditions) = c(1,4,5,6,7,-1,1,4,5,6,7)

#Sine/Cos transformation of Time_Hour and Month cyclic features
combined_df$Time_Hour<-as.numeric(combined_df$Time_Hour)
combined_df$Time_X <- sin(2*pi*combined_df$Time_Hour/24)
combined_df$Time_Y <- cos(2*pi*combined_df$Time_Hour/24)


combined_df$Month <- as.numeric(combined_df$Month)
combined_df$Month_X<- sin(2*pi*combined_df$Month/12)
combined_df$Month_Y <- cos(2*pi*combined_df$Month/12)

#feature engineering on road_name
mean_encoding<- function(df){
  df_road <- select(df,road_name, Accident_Severity)
  df1 <- df_road %>% dplyr::group_by(road_name) %>%  dplyr::summarise(total = n())
  df2 <- df_road %>% group_by(road_name) %>% filter(Accident_Severity == 1) %>% count()
  df3 <- merge(df1,df2,by = "road_name", all.x= TRUE)
  df3[is.na(df3)] <- 0
  df3$mean_encode <- df3$n/df3$total
  df <- merge(df,df3,by = "road_name", all.x = TRUE)
  df <- select(df,-c(Accident_Severity,total,n))
  df[is.na(df)] <- 0
  return(df)
}

combined_df <- mean_encoding(combined_df)
###########################################################################################
#Save the mean encoding and all_motor_vehicles values for every road name in a table to be used during prediction
road_details <- sqldf('select road_name, Longitude, Latitude, X1st_Road_Class, Road_Type,Urban_or_Rural_Area, all_motor_vehicles, mean_encode from combined_df')
saveRDS(road_details, 'C:/Users/harsh/Documents/R/Project1/road_details.rds')

###########################################################################################
#Calculate importance of all the features in the dataset and drop the features with high p-values
comparison <- compareGroups(Accident_Severity~., data = combined_df,method = 1 )
comparison
createTable(comparison)


drop_columns <- c("Date","road_name","Pedestrian_Crossing.Human_Control","Weather_Conditions","Light_Conditions","Road_Surface_Conditions","Special_Conditions_at_Site","Carriageway_Hazards","Vehicle_Type","Month","Time_Hour")
combined_df <- combined_df %>% select(-one_of(drop_columns)) 


ggplot(combined_df)+geom_density(aes(x=combined_df$mean_encode, color = Accident_Severity))
write.csv(combined_df, "C:\\Users\\harsh\\Documents\\R\\Project1\\final_df_without_norm.csv",row.names = FALSE)
saveRDS(combined_df, "C:\\Users\\harsh\\Documents\\R\\Project1\\combined_df.rds")


#Normalization of all the numeric variables
var_to_norm <- select_if(combined_df,is.numeric)
boxplot(acc_AADF_veh_15[var_to_norm])
acc_AADF_veh_15[var_to_norm]<-lapply(acc_AADF_veh_15[var_to_norm], function(x) {
      y<-scale(x, center=TRUE, scale=TRUE)
   })
boxplot(acc_AADF_veh_15[var_to_norm])


######################################################################################################################
#Stratified Sampling of dataset
table(combined_df$Accident_Severity)

#Check proportion of imbalance
prop.table(table(combined_df$Accident_Severity))
train_final_df_wo_norm <- 


#stratified sampling of dataset
levels(combined_df$Accident_Severity) <- c( "fatal", "serious", "mild")
balance.sample <- RandUnderClassif(Accident_Severity~., combined_df, C.perc = "balance", repl = FALSE)
write.csv2(balance.sample, file = "balance_sample.csv")
extreme.sample <-RandUnderClassif(Accident_Severity~., combined_df, C.perc = "extreme", repl = FALSE)
write.csv2(extreme.sample, file = "extreme_sample.csv")

under.sample <- RandUnderClassif(Accident_Severity~., combined_df, C.perc = list(fatal = 1, serious= 0.6, mild = 0.4))
write.csv2(under.sample, file = "under_sample.csv")
c <- compareGroups(Accident_Severity~., data = under.sample, method = 1)
createTable(c)

set.seed(200)
idx <-sample(nrow(balance.sample),1000, replace = FALSE)
train <- balance.sample[idx,]
test <- balance.sample[-idx,]

library(adabag)

rf.under.500tree <- randomForest(Accident_Severity~.-road_name, data = train, ntrees = 500, mtry = 10,trControl = trainControl(method = 'cv', number = 5), importance = TRUE)
print(rf.under.500tree)
plot(rf.under.500tree)
test$pred_Severity <- predict(rf.under.500tree, newdata = test)
confusionMatrix(test$pred_Severity, test$Accident_Severity)
mean(test$pred_Severity == test$Accident_Severity)
importance(rf.under.500tree)
varImpPlot(rf.under.500tree)

train <- under.sample[sample(nrow(under.sample),1000, replace = FALSE),]

table(train$Accident_Severity)

test <- my_df[-index,]
table(test$Accident_Severity)


#Random Forest 
train <- train[, -c(12)]
p = ncol(combined_train)
library(randomForest)
form <- Accident_Severity ~Day_of_Week+Road_Type+Urban_or_Rural_Area+Time_Hour+Month+Sex_of_Driver+Age_Band_of_Driver        
rf <- randomForest(Accident_Severity~.-road_name, data = train, ntrees = 500, mtry = 10,na.action = na.exclude)
rf_500 <- randomForest(Accident_Severity ~.-road_name,data = train, ntrees = 500,ntry = 10, na.action = na.omit )


#svm
library(e1071)
svm.linear.balance <- tune(svm, Accident_Severity~.,data = train, kernel= "linear", cost = 1, scale = FALSE)
summary(svm.linear.balance)
test.pred <- predict(svm.linear.balance$bestModel, newdata = test)
