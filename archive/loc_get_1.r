# dont forget to set working directory

library(ggmap)

data2 = read.csv("loc_need_1.csv")

for(i in 1:nrow(data2))
{
    result <- geocode(data2$address2[i], 
                      output = "latlon", 
                      source = "google")
    data2$business_longitude[i] <- as.numeric(result[1])
    data2$business_latitude[i] <- as.numeric(result[2])
  
}


write.csv(data2, 
          file = "loc_done_1.csv",
          row.names = F)


