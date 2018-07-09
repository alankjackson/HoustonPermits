library("tidyverse")
library("stringr") # string tools
library("rvest")# web scraping
library("lettercase") # fiddle with letter case
library("lubridate")

options(stringsAsFactors = FALSE)

Weekly_Rpt_URL <- "http://houstontx.gov/planning/Publications/listsrv/WebeReport.html"

census_geocoder <- function(address,type,secondary,state){

    library(jsonlite)
    library(RCurl)

    addy <- paste("street=",gsub(" ","+",address),sep="")
    if(type=="z"){
          wild <- paste("zip=",gsub(" ","+",secondary),sep="")
        }else{
          wild <- paste("city=",gsub(" ","+",secondary),sep="")
    }
    
    state <- paste("state=",gsub(" ","+",state),sep="") 
    string <-  paste("https://geocoding.geo.census.gov/geocoder/geographies/address?",addy,"&",wild,"&",state,"&benchmark=4&vintage=4&format=json",sep="")
    #print(paste("-1->", string))
    json_file<-fromJSON(getURL(string))
    #print(paste("-2->", json_file))

    #Check if there are results
    ###if(length(json_file$result$addressMatches$coordinates)>0){
    #print(paste("--2.5-->",json_file$result$addressMatches))
    if(length(json_file$result$addressMatches)>0){
      #print("-3-")
      
      #If not, kick back an empty dataframe
      if(is.null(json_file$result$addressMatches$coordinates$x[1])==TRUE){
      #print("-4-")
        print("no result")
        return(data.frame(
          address="",
          lat = "",
          lon= "",
          tract = "",
          block = ""))
        
      } else{

      #print("-5-")
        #  Address,lat,lon,tract, block (keep first match)
        address <- as.character(data.frame(json_file$result$addressMatches$matchedAddress)[1,])
        lat <- as.character(json_file$result$addressMatches$coordinates$y[1])
        lon <- as.character(json_file$result$addressMatches$coordinates$x[1])
        tract <- data.frame(json_file$result$addressMatches$geographies$`Census Tracts`)$GEOID[1]
        block <- data.frame(json_file$result$addressMatches$geographies$`2010 Census Blocks`)[1,c("GEOID")]

        return(data.frame(
                address = address,
                lat = lat,
                lon = lon,
                tract = tract,
                block = block))
    
      }
    }
    }


## Read in a single weekly report

rpt <- Weekly_Rpt_URL %>% 
  read_html() %>% 
  html_nodes(css="table") %>%
  html_table(header=FALSE, fill=TRUE)
  
rpt <- rpt[[1]]

###################################
#   Clean up bad columns and rows
###################################

#   First pull start and end dates out

Start_Date <- strsplit(rpt$X1[3],"\n")[[1]][2]
End_Date <- strsplit(rpt$X1[4],"\n")[[1]][2]

#   delete unwanted columns and rows

rpt <- rpt %>% 
  select(-c(X7, X8, X9, X10)) %>%  # drop bad columns
  slice(14:n()-7) #  drop first 7 and last 7 rows
  

#   rename columns

names(rpt) <- c("Zip", "Permit_Date", "Permit_Type", "Permit_Number", "Address", "Comments")



## Geocode the addresses


rpt$match <- NA
rpt$lat <- NA
rpt$lon <- NA
rpt$tract <- NA
rpt$block <- NA

for (i in 1:nrow(rpt)) {
  answer <- NULL
  attempt <- 1
  while( is.null(answer) && attempt <= 3 ) {
    if (attempt>1){print(paste("attempted", attempt))}
    attempt <- attempt + 1
    try(
      answer <- census_geocoder(rpt$Address[i], "z", rpt$Zip[i], "TX")
    )
  } 
  print(paste(i,":",rpt$Address[i], rpt$Zip[i], answer[1]))
  if (!is.null(answer) && nchar(answer$address)>0){
    rpt$match[i] <- answer[[1]]
    rpt$lat[i] <- answer[[2]]
    rpt$lon[i] <- answer[[3]]
    rpt$tract[i] <- answer[[4]]
    rpt$block[i] <- answer[[5]]
  }
  Sys.sleep(1)
}

## Save output



outputfile <- paste(Start_Date,End_Date,sep="_")
outputfile <- str_replace_all(outputfile,"/","_")
saveRDS(rpt, paste("~/Dropbox/Rprojects/CityPermits/",outputfile,".rds", sep=""))

print("------ and we are done")

