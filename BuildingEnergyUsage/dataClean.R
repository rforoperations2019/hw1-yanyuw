emidata <- read.csv("/Users/yvonne/Desktop/94880/energy-usage-from-doe-buildings-1.csv", stringsAsFactors = FALSE, colClasses = c("Borough" = "character"))[, 1:56]
emidata$Borough <- ifelse(emidata$Borough == "1", "Manhattan", emidata$Borough)
emidata$Borough <- ifelse(emidata$Borough == "2", "Bronx", emidata$Borough)
emidata$Borough <- ifelse(emidata$Borough == "3", "Brooklyn", emidata$Borough)
emidata$Borough <- ifelse(emidata$Borough == "4", "Queens", emidata$Borough)
emidata$Borough <- ifelse(emidata$Borough == "5", "Staten Island", emidata$Borough)
emidata$Borough <- ifelse(emidata$Borough == "6", "Citywide", emidata$Borough)
emidata$Borough <- ifelse(emidata$Borough == "7", "Out of City", emidata$Borough)
emidata$X. <- emidata$X_ <- NULL
energytype <- unique(emidata$Measurement)
temp_split <- strsplit(emidata$Building.Address, "\\[|\\]")
first_col_matrix <- matrix(unlist(temp_split), ncol = 3, byrow = TRUE)
emidata[c("Address", "School.Name")] <- NA
emidata$Address <- first_col_matrix[, 1]
emidata$School.Name <- first_col_matrix[, 2]
emidata$Building.Address <- NULL
emidata[c("Jan.Ave", "Feb.Ave", "Mar.Ave", "Apr.Ave", "May.Ave", "Jun.Ave", "Jul.Ave", "Aug.Ave", "Sep.Ave", "Oct.Ave", "Nov.Ave", "Dec.Ave")] <- NA
emidata$Jan.Ave <- round((emidata$X9.Jan + emidata$X10.Jan + emidata$X11.Jan + emidata$X12.Jan)/4, 2)
emidata$Feb.Ave <- round((emidata$X9.Feb + emidata$X10.Feb + emidata$X11.Feb + emidata$X12.Feb)/4, 2)
emidata$Mar.Ave <- round((emidata$X9.Mar + emidata$X10.Mar + emidata$X11.Mar + emidata$X12.Mar)/4, 2)
emidata$Apr.Ave <- round((emidata$X9.Apr + emidata$X10.Apr + emidata$X11.Apr + emidata$X12.Apr)/4, 2)
emidata$May.Ave <- round((emidata$X9.May + emidata$X10.May + emidata$X11.May)/3, 2)
emidata$Jun.Ave <- round((emidata$X9.Jun + emidata$X10.Jun + emidata$X11.Jun)/3, 2)
emidata$Jul.Ave <- round((emidata$X.FY.2009..7.1.2008 + emidata$X.FY.2010.7.1.2009 + emidata$X.FY.2011.7.1.2010 + emidata$X.FY.2012.7.1.2011)/4, 2)
emidata$Aug.Ave <- round((emidata$X9.Aug + emidata$X10.Aug + emidata$X11.Aug + emidata$X8.Aug)/4, 2)
emidata$Sep.Ave <- round((emidata$X9.Sep + emidata$X10.Sep + emidata$X11.Sep + emidata$X8.Sep)/4, 2)
emidata$Oct.Ave <- round((emidata$X9.Oct + emidata$X10.Oct + emidata$X11.Oct + emidata$X8.Oct)/4, 2)
emidata$Nov.Ave <- round((emidata$X9.Nov + emidata$X10.Nov + emidata$X11.Nov + emidata$X8.Nov)/4, 2)
emidata$Dec.Ave <- round((emidata$X9.Dec + emidata$X10.Dec + emidata$X11.Dec + emidata$X8.Dec)/4, 2)
emidata <- emidata[-c(3:50)]
write.csv(emidata, file = "/Users/yvonne/Desktop/94880/energy-usage-doe-buildings.csv", row.names = FALSE)
