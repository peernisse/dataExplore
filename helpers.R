#This script holds helper functions for the data exploration app

#Plots

tsPlot<-function(data,x,y){
        
        g<-ggplot(data,aes(x=x,y=y, color=LOC_ID))+
                geom_point()+
                theme(legend.position = "bottom", legend.title = element_blank())+
                labs(x="Date",y="Groundwater Elevation (feet amsl)",title="Groundwater Elevation Time Series")
        
}

bxPlot<-function(data,x,y){
        
        g<-ggplot(data,aes(x=x,y=y, fill=LOC_ID))+
                geom_boxplot()+
                theme(legend.position = "bottom", legend.title = element_blank())+
                labs(x="Location",y="Groundwater Elevation (feet amsl)",title="Groundwater Elevation Boxplots")
        
}
