#This script holds helper functions for the data exploration app

#Functions----------------------------------
#Fix units cases
fixUnits<-function(x){
        df<-x
        units<-df$Units
        unitsOut<-case_when(
                units == 'MG/L' ~ 'mg/l',
                units == 'mg/l' ~ 'mg/l',
                units == 'mg/L' ~ 'mg/l',
                units == 'UG/L' ~ 'ug/l',
                units == 'ug/l' ~ 'ug/l',
                units == 'ug/L' ~ 'ug/l',
                units == 'MG/KG' ~ 'mg/kg',
                units == 'mg/kg' ~ 'mg/kg'
        )
        
        return(unitsOut)
}

#Plot functionss--------------------------------

tsPlot<-function(data,x,y,units=NULL){
        tsdat<-data
        tsdat$Parameter<-paste0(tsdat$Parameter," (",tsdat$Units,")")
        g<-ggplot(tsdat,aes(x=x,y=y, color=Location))+
                geom_point()+
                facet_wrap(~Parameter,scales="free")+
                theme(legend.position = "bottom", legend.title = element_blank())+
                labs(x="Date",y="Value",title="Time Series")
        
}

bxPlot<-function(data,x,y){
        
        bxdat<-data
        bxdat$Parameter<-paste0(bxdat$Parameter," (",bxdat$Units,")")
        g<-ggplot(bxdat,aes(x=x,y=y, fill=Location))+
                geom_boxplot()+
                facet_wrap(~Parameter, scales="free")+
                theme(legend.position = "bottom", legend.title = element_blank())+
                labs(x="Location",y="Value",title="Boxplots")
        
}


qPlot<-function(data){
        
        qdat<-data
        qdat$Parameter<-paste0(qdat$Parameter," (",qdat$Units,")")
        g<-ggplot(qdat,aes(sample=Value,color=Location))+
                geom_qq()+
                facet_wrap(~Parameter,scales="free")+
                theme(legend.position = "bottom", legend.title = element_blank())+
                labs(x="Theoretical Distribution(normal)",y="Value",title="Distribution (quantile plot)")
        
}

hPlot<-function(data){
        
        hdat<-data
        hdat$Parameter<-paste0(hdat$Parameter," (",hdat$Units,")")
        g<-ggplot(hdat,aes(x=Value,fill=Location))+
                geom_histogram(alpha=0.5)+
                facet_wrap(~Parameter,scales="free")+
                theme(legend.position = "bottom", legend.title = element_blank())+
                labs(x="Water Elevation Bins (meters amsl)",y="Count",title="Distribution (histogram)")
}


