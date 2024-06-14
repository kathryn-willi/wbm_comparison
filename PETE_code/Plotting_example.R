library(ggplot2)
#Output directory
OutDir = "./Outputs/"

#######################################################################################################################
######################################### PLOTTING ####################################################################
AnnualWB$Year = as.numeric(AnnualWB$year)
ggplot(AnnualWB, aes(x=Year, y=sum_d)) + 
  geom_line(colour = "black",size=1, stat = "identity") +
  theme(axis.text=element_text(size=16),
        axis.title.x=element_text(size=16,vjust=-0.2),
        axis.title.y=element_text(size=20,vjust=1.0),
        plot.title=element_text(size=24,face="bold",vjust=2,hjust=0.5)) +
  labs(title=paste(SiteID, "Annual total climatic water deficit"),
       y="Annual deficit (mm)")
ggsave(paste("Deficit",SiteID,".png",sep=""), path = OutDir, width = 15, height = 9)

MonthlyWB$month<-as.numeric(substr(MonthlyWB$yrmon,5,6)) #create monthly variable
Month<-aggregate(cbind(avg_soil)~month,mean,data=MonthlyWB,na.rm=TRUE) #aggregate by month

Month$mon<-factor(month.abb[Month$month], levels = month.abb)

ggplot(Month, aes(x=month, y=avg_soil)) + 
  geom_line(colour = "black",size=1, stat = "identity") + # adds black outline
  theme(axis.text=element_text(size=16),
        axis.title.x=element_text(size=16,vjust=-0.2),
        axis.title.y=element_text(size=20,vjust=1.0),
        plot.title=element_text(size=24,face="bold",vjust=2,hjust=0.5)) +
  labs(title = "Average monthly soil moisture (1979-2020)",
       x = "Month", y = "Soil moisture (mm)") +  scale_x_discrete(limits = month.abb)

ggsave("MonthlySoil Moisture.png", path = OutDir, width = 15, height = 9)
