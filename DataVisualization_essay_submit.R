# =============================================================
# data preparation
# =============================================================
library(dplyr)
library(readxl)
library(stringr)

# -----------------------------------------------
# open raw data 
# -----------------------------------------------
  # Table 3 : UK e-commerce sales, by industry sector, 2009 to 2019 (£)
  # Table 14 : Proportion of UK businesses with internet access and type of connection, by size of business, 2007 to 2019 (%)
  # Table 17 : Proportion of employees using computers and the internet for their work, by size of UK business, 2007 to 2019 (%)
  # Table 20 : Proportion of UK businesses purchasing cloud computing services over the internet, by industry sector, 2013, 2015, 2017 and 2019 (%)
  # Table 23 : Proportion of UK businesses using big data analysis, by industry sector, 2015 and 2019 (%)
table3 <- read_excel('Data/Essays/2022AUT_DataVisualization/ecommerceandict2019.xlsx', sheet = 'Table 3')
table14 <- read_excel('Data/Essays/2022AUT_DataVisualization/ecommerceandict2019.xlsx', sheet = 'Table 14')
table17 <- read_excel('Data/Essays/2022AUT_DataVisualization/ecommerceandict2019.xlsx', sheet = 'Table 17')
table20 <- read_excel('Data/Essays/2022AUT_DataVisualization/ecommerceandict2019.xlsx', sheet = 'Table 20')
table23 <- read_excel('Data/Essays/2022AUT_DataVisualization/ecommerceandict2019.xlsx', sheet = 'Table 23')

# -----------------------------------------------
# modify data
# -----------------------------------------------
  # ----------- table 3 ------------
    # delete unnecessary rows/ rename columns/ reframe the data/ delete null rows
table3 <- table3[-1,]
names(table3) <- table3[1,]
table3 <- table3[-c(1:3,103:109),-6]
table3_1 <- table3[,c(6:10)]
table3 <- table3[,-c(6:10)]
table3 <- rbind(table3, table3_1)
table3 <- table3[rowSums(is.na(table3)) != ncol(table3),]
colnames(table3)[1] <- 'Sector'
colnames(table3)[2] <- 'Year'
table3 <- table3 %>% 
  rename(
    'Website' = 'Sales over a website'
    , 'EDI' = 'EDI Sales'
    , 'Total' = 'Total e-commerce sales'
  )
    # fill industry sector data
for (i in c(1:nrow(table3))) {
  if (i %% 18 == 1) {
    name <- as.character(table3[i,'Sector'])
    j <- i + 17
    for (k in c(i:j)) {
      table3[k, 'Sector'] <- name
    }
  }
}
    # delete unnecessary rows
table3 <- table3[-c(1:12,19:30,37:48,55:66,73:84,91:102,109:120,127:138,145:156,163:174),]

    # delete '(Division ~' text
table3$Sector <- gsub('\\(.*','',table3$Sector)

    # convert to numeric/ recalculate total e-commerce sales as decimal places differences
table3 <- table3 %>% mutate(across(.cols = 2:5, .fns = as.numeric))
table3$Total <- table3$Website + table3$EDI

    # delete a space
table3$Sector <- str_squish(table3$Sector)


  # ----------- table 14 ------------
    # delete unnecessary rows/ rename columns/ reframe the data/ delete null rows
table14 <- table14[-1,]
names(table14) <- table14[1,]
table14 <- table14[-c(1:3,30:37),-c(3:8)]
table14 <- table14[rowSums(is.na(table14)) != ncol(table14),]
names(table14) <- c('Kind', 'Year', 'Value')

    # fill industry sector data
for (i in c(2:13)) {table14[i, 'Kind'] <- table14[1, 'Kind']}
for (i in c(15:25)) {table14[i, 'Kind'] <- table14[14, 'Kind']}

    # extract the data after the year 2014 
table14 <- table14 %>% filter(Year > 2013)

    # convert to numeric
table14$Value <- as.numeric(table14$Value)

    # change the value
table14 <- table14 %>% mutate(across(.cols = Kind, .fns = ~replace(.x, .x %in% 'Broadband (DSL and/or other fixed internet connection)1, 2', 'Broadband')))

    # delete a space
table14$Kind <- str_squish(table14$Kind)


  # ----------- table 17 ------------
    # delete unnecessary rows/ rename columns/ reframe the data/ delete null rows
table17 <- table17[-1,]
names(table17) <- table17[1,]
table17 <- table17[-c(1:3,47:54),-c(4:9)]
table17 <- table17[-c(30:43),]
table17 <- table17[,-2]
table17 <- table17[rowSums(is.na(table17)) != ncol(table17),]
names(table17) <- c('Kind', 'Year', 'Value')

    # fill industry sector data
for (i in c(2:13)) {table17[i, 'Kind'] <- table17[1, 'Kind']}
for (i in c(15:27)) {table17[i, 'Kind'] <- table17[14, 'Kind']}

    # delete unnecessary rows
table17 <- table17[-14,]

    # extract the data after the year 2014 
table17 <- table17 %>% filter(Year > 2013)

    # convert to numeric
table17$Value <- as.numeric(table17$Value)

    # change the value
table17 <- table17 %>% mutate(across(.cols = Kind, .fns = ~replace(.x, .x %in% 'Employees used computers:', 'Employee computer use with internet access')))
table17 <- table17 %>% mutate(across(.cols = Kind, .fns = ~replace(.x, .x %in% 'Used computers', 'Computer use')))

    # delete a space
table17$Kind <- str_squish(table17$Kind)


  # ----------- table 20 ------------
    # delete unnecessary rows/ rename columns/ reframe the data/ delete null rows
table20 <- table20[-1,]
names(table20) <- table20[1,]
table20 <- table20[-c(1:2,101:107),]
table20 <- table20[rowSums(is.na(table20)) != ncol(table20),]
colnames(table20)[1] <- 'Sector'
colnames(table20)[2] <- 'Size'
colnames(table20)[3] <- 'Year'
table20 <- table20 %>% 
  rename(
    'Office.software' = 'Office software'
    , 'Hosting.business.database' = "Hosting business' database"
    , 'Storage.of.files' = "Storage of files"
    , 'Finance.or.accounting.software.applications' = "Finance or accounting software applications"
    , 'CRM.software' = "CRM software"
    , 'Computing.capacity.to.run.own.software' = "Computing capacity to run own software"
    , 'Bought.any.cloud.computing.services' = "Bought any cloud computing services"
  )
    # fill industry sector data
for (i in c(1:nrow(table20))) {
  if (i %% 7 == 1) {
    name <- as.character(table20[i,'Sector'])
    j <- i + 6
    for (k in c(i:j)) {
      table20[k, 'Sector'] <- name
    }
  }
}
    # convert to numeric
table20 <- table20 %>% mutate(across(.cols = 3:11, .fns = as.numeric))

    # extract only 2019 data 
table20 <- table20 %>% filter(Year == 2019)

    # delete unnecessary rows/ columns
table20 <- table20[-c(1,3,5,7,9,11,13,15,17,19),-2]

    # delete a space/ modifiy '&' -> 'and'
table20$Sector <- str_squish(table20$Sector)
table20[table20$Sector == 'Accommodation & food services', 'Sector'] <- 'Accommodation and food services'
table20[table20$Sector == 'Information & communication', 'Sector'] <- 'Information and communication'
table20[table20$Sector == 'Transport & storage', 'Sector'] <- 'Transport and storage'


  # ----------- table 23 ------------
    # delete unnecessary rows/ rename columns/ reframe the data/ delete null rows
table23 <- table23[-1,]
names(table23) <- table23[1,]
table23 <- table23[-c(1:2,52:58),]
table23 <- table23[rowSums(is.na(table23)) != ncol(table23),]
colnames(table23)[1] <- 'Sector'
colnames(table23)[2] <- 'Size'
colnames(table23)[3] <- 'Year'
table23 <- table23 %>% 
  rename(
    "Business.own.data.from.smart.devices.or.sensors" = "Business' own data from smart devices or sensors"
    , "Geolocation.data.from.the.use.of.portable.devices" = "Geolocation data from the use of portable devices" 
    , "Data.generated.from.social.media" = "Data generated from social media"                 
    , "Other.big.data.sources" = "Other big data sources"                           
  )
    # fill industry sector data
for (i in c(1:nrow(table23))) {
  if (i %% 4 == 1) {
    name <- as.character(table23[i,'Sector'])
    j <- i + 3
    for (k in c(i:j)) {
      table23[k, 'Sector'] <- name
    }
  }
}
    # convert to numeric
table23 <- table23 %>% mutate(across(.cols = 3:7, .fns = as.numeric))

    # extract only 2019 data 
table23 <- table23 %>% filter(Year == 2019)

    # delete unnecessary rows/ columns
table23 <- table23[-c(1,3,5,7,9,11,13,15,17,19),-2]

    # delete a space
table23$Sector <- str_squish(table23$Sector)


# -----------------------------------------------
# merge table 3, 20, 23 only 2019 data
# -----------------------------------------------
  # extract only 2019 data from table 3
table3_2019 <- table3 %>% filter(Year == 2019)

  # merge all 2019 data
df <- table3_2019 %>%
  left_join(table20, by = c('Sector','Year')) %>%
  left_join(table23, by = c('Sector','Year'))



# =============================================================
# data visualizations
# =============================================================
library(ggplot2)

# -----------------------------------------------
# Figure 1. Total e-commerce sales by industry sectors (2014 - 2019)
# -----------------------------------------------
  # ----------- data ------------
df_a1 <- table3 %>% 
  filter(Sector != 'All') %>% 
  group_by(Sector) %>%
  mutate(Total_allyears = sum(Total))

    # label data for Wholesale and Manufacturing as they are the biggest sectors
df_a1_label <- df_a1 %>% 
  filter(Year == 2014) %>% 
  filter(Sector %in% c('Wholesale','Manufacturing')) %>% 
  select(Sector, Total_allyears) %>% 
  mutate(labels = paste(Sector,'£',Total_allyears)) 
df_a1_label$x <- 2016.5
df_a1_label$y = c(250,85)

  # ----------- visualization ------------
ggplot(df_a1) +
  geom_area(aes(Year, Total, fill = reorder(Sector, Total)), color = 'black', linewidth = 0.7) +
  scale_fill_brewer(palette = 'Greens') +
  scale_y_continuous(labels = function(x) str_c(x,'£'), breaks = seq(0,700,350)) +
  geom_text(
    data = df_a1_label
    , aes(x, y, label = labels)
    , size = 4.4
    , color = 'white'
    , fontface = 'bold'
  ) +
  theme(
    panel.background = element_rect(fill = '#F5F5F5', color = '#F5F5F5')
    , plot.background = element_rect(fill = '#F5F5F5')
    , legend.background = element_rect(fill = '#F5F5F5', color = '#F5F5F5')
    , legend.key.size = unit(0.4, 'cm')
    , legend.direction = 'vertical'
    , legend.text = element_text(size = 10)
    , legend.position = 'right'
    , axis.ticks.y = element_blank()
    , axis.title.y = element_blank()
    , axis.text.y = element_text(size = 10)
    , axis.ticks.x = element_blank()
    , axis.title.x = element_blank()
    , axis.text.x = element_text(size = 10)
  ) +
  guides(fill = guide_legend(title = "Sectors"))
ggsave(filename = 'figure1.png', path = 'figure/DataVisu/', device='tiff', dpi=700)


# -----------------------------------------------
# Figure 2. Composition of total e-commerce sales and internet accessibility (2014- 2019)
# -----------------------------------------------
library(tidyverse)
library(geomtextpath)

  # ----------- data ------------
df_b1 <- table3 %>% 
  pivot_longer(
    cols = c('Website','EDI')
    , names_to = 'Kind'
    , values_to = 'Value'
  ) %>% 
  group_by(Kind, Year) %>%
  summarise(Value = sum(Value)) %>% 
  group_by(Year) %>% 
  mutate(Percent = Value/ sum(Value)) %>% 
  select(!Value) %>% 
  rename('Value' = 'Percent')
df_b2 <- table14 %>% mutate(Value = Value * 0.01)
df_b3 <- table17 %>% mutate(Value = Value * 0.01)
df_b <- rbind(df_b1, df_b2, df_b3)
df_b <- df_b %>% arrange(desc(Value))
df_b$Kind <- factor(df_b$Kind, levels = unique(df_b$Kind))

df_b_c1 <- c('EDI','Website')
df_b_c2 <- c('Internet access','Broadband','Computer use','Employee computer use with internet access')

  # ----------- visualization ------------
ggplot(df_b, aes(fill = Kind)) +
  geom_bar(data = subset(df_b, Kind %in% df_b_c1), aes(x = Year, y = Value), stat = 'identity', position = 'fill', color = 'white', size = 0.9, alpha = 0.4) +
  geom_text(data = subset(df_b, Kind %in% df_b_c1), aes(x = Year, y = Value, label = sprintf("%1.1f", Value*100)), position = position_fill(vjust=0.95), colour="black", size = 3.5) +
  scale_fill_manual(breaks = df_b_c1, values = c('lightblue','lightgreen','white','white','white','#FEE391')) +
  scale_x_continuous(breaks = c(2014:2019)) +
  geom_textline(data = subset(df_b, Kind %in% df_b_c2), aes(x = Year, y = Value, label = Kind), color = 'black', hjust = 0.5, linewidth = 1.5, size = 4.5) +
  geom_text(data = subset(df_b, Year %in% c(2014,2019)), aes(x = Year, y = ifelse(Kind %in% df_b_c2, Value, NA), label = Value*100, hjust = ifelse(Year == 2014, 1.2, -0.2), vjust = ifelse((Year == 2014) & (Kind == 'Broadband'), 0.8, 0.3)), color = 'black', size = 3.5) +
  scale_y_continuous(labels = function(x) str_c(x*100,'%'), breaks = c(1)) +
  theme(
    panel.background = element_rect(fill = '#F5F5F5', color = '#F5F5F5')
    , plot.background = element_rect(fill = '#F5F5F5')
    , legend.background = element_rect(fill = '#F5F5F5', color = '#F5F5F5')
    , legend.position = 'right'
    , legend.text = element_text(size = 10)
    , legend.key.height = unit(0.4, "cm"), legend.title = element_blank()
    , axis.text.y = element_text(size = 10)
    , axis.title.y = element_blank()
    , axis.ticks.y = element_blank()
    , axis.ticks.x = element_blank()
    , axis.title.x = element_blank()
    , axis.text.x = element_text(size = 10)
  ) 
ggsave(filename = 'figure2.png', path = 'figure/DataVisu/', device='tiff', dpi=700)


# -----------------------------------------------
# Figure 3. Use of Cloud Computing Services and Big Data Analysis by Industry Sectors (2019)
# -----------------------------------------------
library(fmsb)

  # ----------- data ------------
df_c1 <- df %>% select(Sector, Bought.any.cloud.computing.services)
df_c2 <- df %>% 
  mutate('Big.data.analysis' = (Business.own.data.from.smart.devices.or.sensors + Geolocation.data.from.the.use.of.portable.devices + Data.generated.from.social.media + Other.big.data.sources) * 0.3) %>% 
  select(Sector, Big.data.analysis)
df_c <- df_c1 %>% left_join(df_c2, by = 'Sector')
df_c <- df_c %>% rename('Cloud.services' = 'Bought.any.cloud.computing.services') 
df_c <- df_c %>% mutate(Sector = str_replace(Sector, " (?=[:alpha:])", "\n"))
df_c_names <- df_c[,1]
df_c <- as.data.frame(as.matrix(t(df_c)))
colnames(df_c) <- df_c[1,]
df_c <- df_c[-1,]

df_c3 <- df_c[1,]
rownames(df_c3) <- 'min'
df_c3 <- df_c3 %>% mutate(across(.cols = everything(), .fns = ~0))
df_c4 <- df_c[2,]
rownames(df_c4) <- 'max'
df_c4 <- df_c4 %>% mutate(across(.cols = everything(), .fns = ~100))
df_c <- rbind(df_c4, df_c3, df_c)
df_c <- df_c %>% 
  select(c('Wholesale','Information\nand communication','Utilities','Manufacturing','Accommodation\nand food services','Transport\nand storage','Construction','Retail','Other\nservices'))
df_c <- df_c %>% mutate(across(.cols = everything(), .fns = as.numeric))

  # ----------- visualization ------------
dev.off()
plot.new()
par(
  family = "cabrito"
  , oma = c(1, 1, 3, 1)
  , mar = c(1, 0, 1, 0)
  , bg = '#F5F5F5'
)
layout(
  matrix(
    c(1,2,3)
    , ncol = 1
    , byrow = TRUE
  )
  , heights = c(27, 3)
)
radarchart(
  df_c
  , seg = 10
  , caxislabels = c(0,'',20,'',40,'',60,'',80,'','100%')
  , axistype = 1
  , pcol = c('red','blue')
  , pfcol = scales::alpha(c('red','blue'),0.5)
  , plwd = 3
  , plty = 1
  , calcex = 1.3
  , cglcol = 'darkgrey'
  , cglty = 1
  , cglwd = 0.8
  , axislabcol = '#696969'
  , vlcex = 1.7
) 
plot.new()
par(mar = c(0,0,0,0))
legend(
  'bottom'
  , legend = c('Cloud Computing Services','Big Data Analysis')
  , bty = 'n'
  , pch = 20
  , col = scales::alpha(c('red','blue'),0.5)
  , pt.cex = 2
  , cex = 1.5
  , horiz = F
)
tiff(filename = "figure3.tiff", units="in",res= 700, height=767, width=814)

# -----------------------------------------------
# Figure 4.
#   Figure 4-1. Correlation between E-commerce Sales and Use of Cloud Computing Services by Industry Sectors (2019)
#   Figure 4-2. Correlation between E-commerce Sales and Use of Big Data Analysis by Industry Sectors (2019)
# -----------------------------------------------
library(ggpubr)

  # ----------- Figure 4-1 data ------------
df_d <- df %>% select(!Year)
df_d1 <- df_d %>% 
  select(!13:16) %>%
  pivot_longer(
    cols = c(5:12)
    , names_to = 'Kind'
    , values_to = 'Value'
  ) %>% 
  filter(Kind %in% colnames(table20)) %>%
  mutate(Prefix = substr(Sector,1,1)) %>% 
  mutate(across(.cols = Kind, .fns = ~replace(.x, .x == 'Bought.any.cloud.computing.services', 'Cloud.services'))) %>% 
  mutate(Sector = str_replace(Sector, " (?=[:alpha:])", "\n"))

  # extract only three cloud services showing the strong correlation 
df_d1_ml <- df_d1 %>% 
  group_by(Kind) %>% 
  do({
    mod = lm(Total ~ Value, data = .)
    data.frame(
      Intercept = coef(mod)[1]
      , Slope = coef(mod)[2]
    )
  }) %>% 
  filter(Kind != 'Cloud.services') %>% 
  arrange(desc(Slope))
df_d1_ml <- top_n(ungroup(df_d1_ml), n = 3, wt = Slope)

  # ----------- Figure 4-2 data ------------
df_d2 <- df_d %>% 
  select(!5:12) %>% 
  mutate('Big data analysis' = (Business.own.data.from.smart.devices.or.sensors + Geolocation.data.from.the.use.of.portable.devices + Data.generated.from.social.media + Other.big.data.sources) * 0.3) %>% 
  pivot_longer(
    cols = c(5:9)
    , names_to = 'Kind'
    , values_to = 'Value'
  ) %>%
  filter(Kind %in% c(colnames(table23), 'Big data analysis')) %>% 
  mutate(Prefix = substr(Sector,1,1)) %>% 
  mutate(Sector = str_replace(Sector, " (?=[:alpha:])", "\n")) 
df_d2[df_d2$Sector == 'Other\nservices', 'Sector'] <- 'Other services'

  # ----------- visualization function ------------
g_scatter1 <- function(data, x, y, label, seq, lim, title){
  ggplot(data, aes({{x}}, {{y}})) +
    geom_point(size = 2) +
    geom_text(
      aes({{x}}, {{y}}, label = {{label}}    
          , vjust = ifelse(
              ({{lim}} == 60) & ({{label}} %in% c('Transport\nand storage','Other\nservices','Accommodation\nand food services'))
              , -0.4
              , ifelse(
                  ({{lim}} == 20) & ({{label}} %in% c('Transport\nand storage','Information\nand communication','Accommodation\nand food services'))
                  , -0.3
                  , 1.6
                )
            )
          , hjust = ifelse(
              ({{lim}} == 60) & ({{label}} %in% c('Transport\nand storage','Other\nservices','Accommodation\nand food services'))
              , 0.5
              , ifelse(
                  ({{lim}} == 20) & ({{label}} %in% c('Transport\nand storage','Information\nand communication','Accommodation\nand food services','Utilities'))
                  , 0
                  , 0.9
                )
            )
      )
      , size = 3.5
    ) +
    geom_smooth(method = 'lm', se = T, linewidth = 0.7, level = 0.7, alpha = 0.3) +
    scale_x_continuous(limits = c(0,lim)) +
    scale_y_continuous(limits = c(0,250), breaks = c(0,100,200), labels = function(x) str_c(x,'£')) +
    labs(title = {{title}}) +
    theme(
      panel.background = element_rect(fill = '#F5F5F5', color = '#F5F5F5')
      , plot.background = element_rect(fill = '#F5F5F5')
      , plot.title = element_text(size = 11, face = 'bold', hjust = 0.5)
      , plot.margin = margin(0.15,0.15,0.8,0.15, "cm") 
      , legend.position = 'none'
      , axis.title.y = element_blank()
      , axis.ticks.y = element_blank()
      , axis.text.y = element_text(size = 10)
      , axis.title.x = element_blank()
      , axis.ticks.x = element_blank()
      , axis.text.x = element_blank()
      # , axis.title = element_text(size = 3)
    ) 
}
g_scatter2 <- function(data, x, y, label, seq, lim, title){
  ggplot(data, aes({{x}}, {{y}}, color = {{seq}})) +
    geom_point(size = 0.5) +
    geom_text(aes({{x}}, {{y}}, label = {{label}}), size = 3.5, vjust = 1.1, hjust = -0.7) +
    geom_smooth(aes(fill = {{seq}}), method = 'lm', se = T, linewidth = 0.7, level = 0.5, alpha = 0.1) +
    scale_x_continuous(limits = c(0,lim), labels = function(x) str_c(x,'%')) +
    scale_y_continuous(limits = c(0,250), breaks = c(0,100,200), labels = function(x) str_c(x,'£')) +
    labs(title = {{title}}) +
    theme(
      panel.background = element_rect(fill = '#F5F5F5', color = '#F5F5F5')
      , plot.background = element_rect(fill = '#F5F5F5')
      , plot.title = element_text(size = 11, face = 'bold', hjust = 0.5)
      , legend.background = element_rect(fill = '#F5F5F5', color = '#F5F5F5')
      , legend.position = 'bottom'
      , legend.direction = 'vertical'
      , legend.text = element_text(size = 10)
      , legend.key.height = unit(0.5, "cm"), legend.title = element_blank()
      , axis.text.y = element_text(size = 10)
      , axis.title.y = element_blank()
      , axis.ticks.y = element_blank()
      , axis.title.x = element_blank()
      , axis.ticks.x = element_blank()
      , axis.text.x = element_text(size = 10)
    ) 
}

  # ----------- Figure 4-1 visualization ------------
df_d_g1 <- g_scatter1(df_d1 %>% filter(Kind == 'Cloud.services'), Value, Total, Sector, Kind, 60, 'E-commerce Sales & Any Cloud Computing Services Use')
df_d_g2 <- g_scatter2(df_d1 %>% filter(Kind %in% df_d1_ml$Kind), Value, Total, Prefix, Kind, 60, 'E-commerce Sales & Specific Cloud Computing Services Use')
ggarrange(df_d_g1, df_d_g2, ncol = 1, nrow = 2) 
ggsave(filename = 'figure4-1.png', path = 'figure/DataVisu/', device='tiff', dpi=700)

  # ----------- Figure 4-2 visualization ------------
df_d_g3 <- g_scatter1(df_d2 %>% filter(Kind == 'Big data analysis'), Value, Total, Sector, Kind, 20, 'E-commerce Sales & Any Big Data Analysis Use')
df_d_g4 <- g_scatter2(df_d2 %>% filter(Kind != 'Big data analysis'), Value, Total, Prefix, Kind, 20, 'E-commerce Sales & Specific Big Data Analysis Use')
ggarrange(df_d_g3, df_d_g4, ncol = 1, nrow = 2) 
ggsave(filename = 'figure4-2.png', path = 'figure/DataVisu/', device='tiff', dpi=700)








