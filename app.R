##########
#US CENSUS BUREAU DIFFERENTIAL PRIVACY 2010 DEMONSTRATION DATA FOR WASHINGTON
#
#DATA DOWNLOADED December 4, 2019 FROM IPUMS NHGIS, UNIVERSITY OF MINNESOTA: https://www.nhgis.org/differentially-private-2010-census-data
#
#Rob Kemp (robert.kemp@ofm.wa.gov), December 2019
#https://github.com/robkemp
##########

library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)

Names<-read.csv(file="PlaceAndCountyNames_DP2010DemonstrationProducts_WA.csv",header=FALSE, stringsAsFactors = FALSE)

ui<-fluidPage(

	tags$h3("Review of U.S. Census Bureau's 2010 Demonstration Data Products: Population by Demographic Characteristics, Washington Counties, Cities, and Places"),
	p("U.S. Census Bureau data downloaded December 4, 2019 from ",
	tags$a(href="https://www.nhgis.org/differentially-private-2010-census-data", "IPUMS NHGIS, University of Minnesota."),
	"",
	tags$a(href="", 
	"")
),
  
hr(),

sidebarLayout(
sidebarPanel(

selectizeInput(inputId = "Area", label = "Washington county, city, or census designated place (CDP)", 
choices = Names[,],
options = list(placeholder = "Type in a county, city, or place to see graphs", multiple = TRUE, maxOptions = 5000, onInitialize = I('function() { this.setValue(""); }'))
),

hr(),

p("This interface was made using ",
tags$a(href="https://shiny.rstudio.com/", 
	"Shiny for R."), " Related ",
tags$a(href="https://github.com/robkemp/DP2010DemoDataReview", 
	"GitHub repository."),
"December 2019."),

width=3
),

mainPanel(
	
	plotOutput("plots"),width=3)
)
)
 
#HousingData<-read.table(file="https://raw.githubusercontent.com/edyhsgr/DP2010DemoDataReview_CA/master/long_dp14_160_CA.csv",header=TRUE,sep=",")
PopData<-read.csv(file="long_dp1_050and160_WA.csv",header=TRUE,sep=",")

# Add in a couple of helper functions
"%!in%" <- Negate("%in%") # operator to ask for all items in a vector not in supplied vector
abs_comma <- function (x, ...) {
  format(abs(x), ..., big.mark = ",", scientific = FALSE, trim = TRUE)
} # Allows the labels command in scale_y_continuous to take the absolute value and use commas

server<-function(input, output) {	
	output$plots<-renderPlot({
par(mfrow=c(2,2)) #,mai=c(0.5,0.5,0.5,0.5))

# Format and subset the data

	  #Variable Code lists to make data filtering easier
	  male_vars=c("H76003", "H76004", "H76005", "H76006", "H76007", "H76008", "H76009", "H76010", "H76011", "H76012", "H76013", "H76014", "H76015", "H76016", "H76017", "H76018", "H76019", "H76020", "H76021", "H76022", "H76023", "H76024", "H76025")
	  female_vars=c("H76027", "H76028", "H76029", "H76030", "H76031", "H76032", "H76033", "H76034", "H76035", "H76036", "H76037", "H76038", "H76039", "H76040", "H76041", "H76042", "H76043", "H76044", "H76045", "H76046", "H76047", "H76048", "H76049")
	  select_vars=c(male_vars, female_vars)
	  collapsed_vars=c("H76007", "H76009", "H76010", "H76019", "H76021", "H76031", "H76033", "H76034", "H76043", "H76045")
	  age_vars=select_vars[select_vars%!in%collapsed_vars]
	  agegroups=c("0-4","5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")
	  
	  # Create the Age by sex dataset
	  ## Filter to only age, add sex, and collapse unndeed detail
	  age_dat=PopData%>%
	    filter(var_code%in%select_vars, name_sf==input$Area)%>%
	    mutate(sex=ifelse(var_code%in%male_vars, "Male", "Female"),
	           var_code=case_when(
	             var_code=="H76007" ~ "H76006",
	             var_code=="H76009" ~ "H76008",
	             var_code=="H76010" ~ "H76008",
	             var_code=="H76019" ~ "H76018",
	             var_code=="H76021" ~ "H76020",
	             var_code=="H76031" ~ "H76030",
	             var_code=="H76033" ~ "H76032",
	             var_code=="H76034" ~ "H76032",
	             var_code=="H76043" ~ "H76042",
	             var_code=="H76045" ~ "H76044",
	             TRUE ~ var_code))%>%
	    group_by(name_sf, sex, var_code)%>%
	    summarize(dp=sum(dp),
	              sf=sum(sf))%>%
	    ungroup()%>%
	    mutate(
	      sex=ordered(sex, levels=c("Male", "Female"), labels=c("Male", "Female")),
	      age_cat=ordered(var_code, levels=age_vars, labels=c(agegroups,agegroups)),
	      dp=ifelse(sex=="Male", dp*-1, dp),
	      sf=ifelse(sex=="Male", sf*-1, sf))
	  
##GRAPHS
if(input$Area=="") {
plot.new()
legend("topleft",legend=c("Select a county, city, or place with the panel to the left"),cex=1.5,bty="n")
}

if(input$Area!="") {
#Graphs 1 and 2
  age_dat%>%
    ggplot(aes(x=age_cat))+
    geom_bar(aes(y=dp, fill=sex), color="gray80", stat="identity")+
    geom_bar(aes(y=sf), fill=NA, color="black", stat="identity")+
    coord_flip()+
    theme_minimal()+
    scale_y_continuous(labels = abs_comma)+
    scale_fill_manual(values=c("#a6cee3", "#1f78b4"), 
                      name="Sex")+
    labs(title=paste0("Population by Age and Sex, ", input$Area),
         subtitle="Demonstration Product with differential privacy applied in blue",
         x="Age",
         y="Population",
         caption="Source: U.S. Census Bureau's 2010 Demonstration Data Products. Accessed via IPUMS NHGIS, University of Minnesota, December 2019.")
  
#Graphs 3 and 4
Race<-subset(PopData, PopData$name_sf==input$Area & (PopData$var_code=="H7Z003" #non Hisp White
| PopData$var_code=="H7Z004" #non Hisp Black
| PopData$var_code=="H7Z005" #non Hisp AIAN
| PopData$var_code=="H7Z006" #non Hisp Asian
| PopData$var_code=="H7Z007" #non Hisp NHPI
| PopData$var_code=="H7Z008" #non Hisp Other
| PopData$var_code=="H7Z009" #non Hisp 2+
| PopData$var_code=="H7Z010" #Hisp
))
RaceSF<-c(Race$sf[1],Race$sf[2],Race$sf[3],Race$sf[4]+Race$sf[5],Race$sf[6]+Race$sf[7],Race$sf[8])
RaceDP<-c(Race$dp[1],Race$dp[2],Race$dp[3],Race$dp[4]+Race$dp[5],Race$dp[6]+Race$dp[7],Race$dp[8])
RaceTable<-cbind(RaceSF,RaceDP)
colors<-c(2,3,4,5,6,7)
colors<-adjustcolor(colors, alpha.f = 0.5)

barplot(RaceTable,col=colors,ylim=c(0,sum(Race$sf)*1.35),names.arg=c("Published 2010 Census Data","2010 Census Data with Differential Privacy"),axes=FALSE,cex.names=1.25)
mtext(side=1,line=-42,adj=.18,text=paste(c("Total population: ",sum(Race$sf)),collapse=""),font=.5,cex=1)
mtext(side=1,line=-42*(sum(Race$dp)/sum(Race$sf)),adj=.82,text=paste(c("Total population: ",sum(Race$dp)),collapse=""),font=.5,cex=1)

mtext(side=1,line=-50,at=2.5,text=paste(c("Population by Race and Hispanic Origin Groupings, ", input$Area),collapse=""),font=1,cex=1.75)

mtext(side=1,line=4,adj=0,text=paste(c("Source: U.S. Census Bureau's 2010 Demonstration Data Products. Accessed via IPUMS NHGIS, University of Minnesota, November 2019."),collapse=""),font=.5,cex=1)

plot.new()
legend("left",legend=rev(c("White, not Hispanic","Black, not Hispanic","American Indian or Alaska Native, not Hispanic","Asian or Pacific Islander, not Hispanic","Two+ or Other, not Hispanic","Hispanic")),fill=rev(c(colors)),cex=1.5,bty="n")

}

},height=1500,width=1100)
		
}

shinyApp(ui = ui, server = server)

