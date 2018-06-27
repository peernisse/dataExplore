#This script holds helper functions for the data exploration app

#Plots

tsPlot<-function(data,x,y){
        
        g<-ggplot(data,aes(x=x,y=y, color=LOC_ID))+
                geom_point()+
                theme(legend.position = "bottom", legend.title = element_blank())+
                labs(x="Date",y="Groundwater Elevation (meters amsl)",title="Groundwater Elevation Time Series")
        
}

bxPlot<-function(data,x,y){
        
        g<-ggplot(data,aes(x=x,y=y, fill=LOC_ID))+
                geom_boxplot()+
                theme(legend.position = "bottom", legend.title = element_blank())+
                labs(x="Location",y="Groundwater Elevation (meters amsl)",title="Groundwater Elevation Boxplots")
        
}


qPlot<-function(data){
        g<-ggplot(data,aes(sample=WATER_ELEV))+
                geom_qq()+
                facet_wrap(~LOC_ID,scales="free")+
                labs(x="Theoretical Distribution(normal)",y="Water Elevation (meters amsl)",title="Groundwater Elevation Distribution (quantile plot)")
        
}

hPlot<-function(data){
        g<-ggplot(data,aes(x=WATER_ELEV))+
                geom_histogram()+
                facet_wrap(~LOC_ID,scales="free")+
                labs(x="Water Elevation Bins (meters amsl)",y="Count",title="Groundwater Elevation Distribution (histogram)")
}


