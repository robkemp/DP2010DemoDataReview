library(ggplot2)
library(tidyr)
library(plyr)
library(dplyr)

input=data.frame(Area="Aberdeen city")
"%!in%" <- Negate("%in%")
abs_comma <- function (x, ...) {
  format(abs(x), ..., big.mark = ",", scientific = FALSE, trim = TRUE)
}

PopData<-read.csv(file="long_dp1_050and160_WA.csv",header=TRUE,sep=",", stringsAsFactors = FALSE)

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

age_dat_l=age_dat%>%
    gather(data, population, -name_sf:-var_code, -age_cat)


# Age by Sex Chart

  age_dat%>%
    ggplot(aes(x=age_cat))+
    geom_bar(aes(y=sf, fill=sex), stat="identity")+
    geom_bar(aes(y=dp), fill=NA, color="black", stat="identity")+
    # geom_segment(aes(x = age_cat, y = 0, xend = age_cat, yend = dp),size=1, color = "grey50") +
    # geom_point(aes(x=age_cat, y=dp, color=sex), size=2)+
    coord_flip()+
    # facet_grid(~sex)+
    theme_minimal()+
    scale_fill_manual(values=c("#a6cee3", "#1f78b4"), 
                      name="Summary File 1")+
    labs(title=paste0("Population by Age and Sex, ", input$Area),
         x="Age",
         y="Population",
         caption="Source: U.S. Census Bureau's 2010 Demonstration Data Products. Accessed via IPUMS NHGIS, University of Minnesota, December 2019.")

m=age_dat%>%
  filter(sex=="Male")%>%
  ggplot(aes(x=age_cat))+
  geom_bar(aes(y=dp, group=sex, fill="Demonstration Product"), color="gray70", stat="identity")+
  geom_bar(aes(y=sf, group=sex, fill="Summary File 1"), color="gray10", stat="identity")+
  scale_y_continuous(labels = abs_comma)+
  scale_fill_manual(breaks=c("Demonstration Product", "Summary File 1"),values=c(rgb(0,.6,.9,alpha=.5), NA), labels=c("Demonstration Product", "Summary File 1"), name="")+
  coord_flip()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="left",
        axis.title.y = element_blank(),
        axis.text.y = element_blank())+
  labs(title="Male",
       x="Age",
       y="Population")
m  

f=age_dat%>%
  filter(sex=="Female")%>%
  ggplot(aes(x=age_cat))+
  geom_bar(aes(y=dp, group=sex, fill="Demonstration Product"), color="gray70", fill=rgb(0,.6,.9,alpha=.5), stat="identity")+
  geom_bar(aes(y=sf, group=sex, fill="Summary File 1"), color="gray10", fill=NA, stat="identity")+
  scale_y_continuous(labels = abs_comma)+
  coord_flip()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Female",
       x="Age",
       y="Population",
       caption="Source: U.S. Census Bureau's 2010 Demonstration Data Products. Accessed via IPUMS NHGIS, University of Minnesota, December 2019.")

f  
  

# age_dat%>%
#   ggplot(aes(x=age_cat))+
#   geom_bar(aes(y=sf, fill=sex), stat="identity")+
#   geom_bar(aes(y=dp), fill=NA, color="black", stat="identity")+
#   # geom_segment(aes(x = age_cat, y = 0, xend = age_cat, yend = dp),size=1, color = "grey50") +
#   # geom_point(aes(x=age_cat, y=dp, color=sex), size=2)+
#   coord_flip()+
#   # facet_grid(~sex)+
#   theme_minimal()+
#   scale_color_manual(values=c("#f4a582", "#92c5de"), 
#                      name="Census Demonstration Product")+
#   scale_fill_manual(values=c("#ca0020", "#0571b0"), 
#                     name="Summary File 1")+
#   labs(title=paste0("Population by Age and Sex, ", input$Area),
#        x="Age",
#        y="Population",
#        caption="Source: U.S. Census Bureau's 2010 Demonstration Data Products. Accessed via IPUMS NHGIS, University of Minnesota, December 2019.")




# age_dat%>%
  #   ggplot(aes(x=age_cat))+
  #   geom_bar(aes(y=sf, fill=sex), stat="identity", color="black")+
  #   # geom_bar(aes(y=sf, group=sex), stat="identity", color="black", fill=NA)+
  #   geom_segment(aes(x = age_cat, y = sf, xend = age_cat, yend = dp),size=1, color = "grey50") +
  #   geom_point(aes(x=age_cat, y=dp, color=sex), size=2)+
  #   # geom_point(aes(x=age_cat, y=sf, color=sex), size=2)+
  #   coord_flip()+
  #   # facet_grid(~sex)+
  #   theme_minimal()+
  #   scale_color_manual(values=c("#f4a582", "#92c5de"), 
  #                      name="Census Demonstration Product")+
  #   scale_fill_manual(values=c("#ca0020", "#0571b0"), 
  #                     name="Summary File 1")+
  #   labs(title=paste0("Population by Age and Sex, ", input$Area),
  #        x="Age",
  #        y="Population",
  #        caption="Source: U.S. Census Bureau's 2010 Demonstration Data Products. Accessed via IPUMS NHGIS, University of Minnesota, December 2019.")
  