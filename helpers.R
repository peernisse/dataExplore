#This script holds helper functions for the data exploration app
#Git comment test
#Objects/lists
statList<-c('Number of Observations','Percent Non-Detect',
            'Minimum Date','Maximum Date','Minimum','Maximum',
            'First Quartile','Third Quartile','Average',
            'Standard Deviation','Variance')


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

#Calculate percent ND
perND<-function(x,ndChar = 0){
        
        count<-length(x)
        nd<-length(x[x==ndChar])
        pND<-nd/count*100
        return(pND)
        
}

#Plot functionss--------------------------------

tsPlot<-function(data){
        tsdat<-as.data.frame(data)
        tsdat$Parameter<-paste0(tsdat$Parameter," (",tsdat$Units,")")
        g<-ggplot(tsdat,aes(x=Date,y=Result_ND, color=Location))+
                geom_line(aes(color=Location),size=0.5)+
                geom_point(size=3)+
                geom_point(aes(x=Date,y=NonDetect),color="white",size=2)+
                facet_wrap(~Parameter,scales="free")+
                theme(legend.position = "bottom", legend.title = element_blank())+
                theme(strip.background = element_rect(fill = '#727272'),strip.text = element_text(colour='white',face='bold',size = 12))+
                labs(x="Date",y="Value",title="Time Series Non-Detects Hollow at 1/2 the Reporting Limit")+
                theme(plot.title = element_text(face='bold',size=14))
        
}

bxPlot<-function(data){
        
        bxdat<-as.data.frame(data)
        bxdat$Parameter<-paste0(bxdat$Parameter," (",bxdat$Units,")")
        g<-ggplot(bxdat,aes(x=Location,y=Value,fill=Location))+
                geom_boxplot()+
                #geom_jitter(color="black")+
                #geom_jitter(aes(x=Location,y=NonDetect),color="white")+
                facet_wrap(~Parameter, scales="free")+
                theme(strip.background = element_rect(fill = '#727272'),strip.text = element_text(colour='white',face='bold',size = 12))+
                theme(legend.position = "bottom", legend.title = element_blank())+
                labs(x="Location",y="Value",title="Boxplots Non-Detects at Zero")+
                theme(plot.title = element_text(face='bold',size=14))
        
}


qPlot<-function(data){
        
        qdat<-data
        qdat$Parameter<-paste0(qdat$Parameter," (",qdat$Units,")")
        g<-ggplot(qdat,aes(sample=Value,color=Location))+
                geom_qq()+
                facet_wrap(~Parameter,scales="free")+
                theme(strip.background = element_rect(fill = '#727272'),strip.text = element_text(colour='white',face='bold',size = 12))+
                theme(legend.position = "bottom", legend.title = element_blank())+
                labs(x="Theoretical Distribution(normal)",y="Value",title="Distribution (quantile plot) Non-Detects at Zero")+
                theme(plot.title = element_text(face='bold',size=14))
        
}

hPlot<-function(data){
        
        hdat<-data
        hdat$Parameter<-paste0(hdat$Parameter," (",hdat$Units,")")
        g<-ggplot(hdat,aes(x=Value,fill=Location))+
                geom_histogram(alpha=0.5)+
                facet_wrap(~Parameter,scales="free")+
                theme(strip.background = element_rect(fill = '#727272'),strip.text = element_text(colour='white',face='bold',size = 12))+
                theme(legend.position = "bottom", legend.title = element_blank())+
                labs(x="Value Bins=30",y="Count",title="Distribution (histogram) Non-Detects at Zero")+
                theme(plot.title = element_text(face='bold',size=14))
}


