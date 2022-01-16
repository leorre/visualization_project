library(ggplot2)
library(plotly)
library(dplyr)


df <- read.csv(file='pensiaAllYears3.csv', header=T, encoding = "UTF-8")

#facet AVG_ANNUAL_MANAGEMENT_FEE Vs. YEAR_TO_DATE_YIELD
df_12<-df[df$MONTH==12,]
df_12<-df_12[df_12$YEAR!=2016,]
df_12<-df_12[df_12$YEAR!=2017,]
df_12 <- df_12[c(16,17,19,7,34,36)]
df_12<- na.omit(df_12)


df2 <- df_12 %>%
        group_by(FUND_CLASSIFICATION, MANAGING_CORPORATION_TINY, YEAR) %>%
        summarise(YEAR_TO_DATE_YIELD=(mean(YEAR_TO_DATE_YIELD)), AVG_ANNUAL_MANAGEMENT_FEE=(mean(AVG_ANNUAL_MANAGEMENT_FEE)),  AVG_DEPOSIT_FEE=(mean(AVG_DEPOSIT_FEE))) 
df2$YEAR_TO_DATE_YIELD <- round(df2$YEAR_TO_DATE_YIELD ,digit=3)

df2 <- df2 %>% 
      rename(Avg.Annual.Cumulative.Yield=YEAR_TO_DATE_YIELD,
             Avg.Annual.Management.Fee=AVG_ANNUAL_MANAGEMENT_FEE,
             Avg.Deposit.Fee=AVG_DEPOSIT_FEE,
             Company=MANAGING_CORPORATION_TINY,
             Fund.Classification=FUND_CLASSIFICATION)


p3 <- ggplot(df2, aes(y=Avg.Annual.Management.Fee, x=Avg.Deposit.Fee,colour=Company, shape=Fund.Classification, size=Avg.Annual.Cumulative.Yield))+
  geom_point()+ guides(color = guide_legend(override.aes = list(size=7),title=""))+
  scale_size(range = c(2.5, 7), name="") +
  scale_color_manual(values = c("#EA5543", "#F786AD", "#EF9C1F", "#FFF200", "#B43DC7", "#7CD185", "#11823B", "#519BFF", "#283197"))+
  guides(size = guide_legend(override.aes = list(size=4),title="Avg. Annual Cumulative Yield (%)\n ???   -2.618\n???   15.090"))+
  guides(shape = guide_legend(override.aes = list(size=4),title=""))+
  scale_shape_manual(values=c(18, 13))+    
  facet_wrap(~YEAR)+
  theme_bw()+
  theme(text = element_text(size = 25))+
  theme(plot.title = element_text(size = 24, face = "bold"))+
  theme(strip.text.x = element_text(size = 24))+
  ggtitle("               Avg. Deposit Fee vs. Avg. Annual Management Fee")+
  xlab("Avg. Deposit Fee (%)")+
  ylab("Avg. Annual Management Fee (%)")

ggplotly(p3) %>%
  layout(showlegend = TRUE, legend = list(title=list(font = list(size = 17)), font = list(size = 17)),
         hoverlabel = list(font=list(size=20)))


#pdf graphs of liquid by company- Number 4
p5<-ggplot(df,aes(x=LIQUID_ASSETS_PERCENT))+
  geom_density()+
  theme_bw()+
  theme(text = element_text(size = 25))+
  theme(plot.title = element_text(size = 40, face = "bold"))+
  facet_wrap(~MANAGING_CORPORATION_TINY,ncol=3)+
  theme(strip.text.x = element_text(size = 30))+
  ggtitle("Liquid Assets Precentage Distribution by Company")+
  xlab("Liquid Assets Precentage (%)")+
  ylab("Density")

p5
ggsave("hist.jpeg", p5, width = 15, height = 10, units = c("in"))
