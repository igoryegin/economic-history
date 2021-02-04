library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(sqldf)
library(gridExtra)

#Primary data wrangling

df1888 <- as.data.table(df1888)
df1889 <- as.data.table(df1889)
df1890 <- as.data.table(df1890)
df1891 <- as.data.table(df1891)
df <- rbindlist(list(df1888, df1889, df1890, df1891), use.names = TRUE, fill = TRUE)
df <- df[which(rowSums(is.na(df)) != ncol(df)), .SD, .SDcols = which(colSums(is.na(df)) != nrow(df))]
df.ts <- df %>%
  unite('Date', 3:4, sep = ' ') %>%
  subset(`Flu related?` == 'yes') %>%
  mutate(`deaths total` = as.numeric(`male deaths`) + as.numeric(`female deaths`)) %>%
  select(-`total deaths`)
df.ts$Date <- ym(df.ts$Date)
df.ts$`deaths over 15` = rowSums(apply(as.matrix(df.ts[, 16:24]), c(1,2), as.numeric), na.rm = TRUE)
df.ts[, 7:ncol(df.ts)] <- lapply(df.ts[, 7:ncol(df.ts)], as.numeric)

df.age <- df.ts %>%
  group_by(`Cause of death (English)`) %>%
  summarise(
    `A: newcomers` = sum(newcomers, na.rm = TRUE),
    `B: <0.5` = sum(`babies up to 6 months`, na.rm = TRUE),
    `C: 0.5-1` = sum(`babies from 6 months to 1 year`, na.rm = TRUE),
    `D: 1-5` = sum(`babies from 1 year to 5 years`, na.rm = TRUE),
    `E: 5-10` = sum(`from 5 to 10`, na.rm = TRUE),
    `F: 10-15` = sum(`from 10 to 15`, na.rm = TRUE),
    `G: 15-20` = sum(`from 15 to 20`, na.rm = TRUE),
    `H: 20-30` = sum(`from 20 to 30`, na.rm = TRUE),
    `I: 30-40` = sum(`from 30 to 40`, na.rm = TRUE),
    `J: 40-50` = sum(`from 40 to 50`, na.rm = TRUE),
    `K: 50-60` = sum(`from 50 to 60`, na.rm = TRUE),
    `L: 60-70` = sum(`from 60 to 70`, na.rm = TRUE),
    `M: 70-80` = sum(`from 70 to 80`, na.rm = TRUE),
    `N: >80` = sum(`over 80`, na.rm = TRUE)
  )
t.df.age <- as.data.table(t(df.age[-1]))
t.df.age$`age group` <- colnames(df.age)[-1]
colnames(t.df.age) <- c(df.age$`Cause of death (English)`, 'age group')

# Graphs

mycols <- c('darkred', 'darkgreen', 'darkblue', 'darkmagenta', 'darkorange', 'cyan')

# Time series
ggplot(df.ts, aes(x = Date, y = `deaths total`, col = `Cause of death (English)`)) +
  geom_line(lwd = 1.2) + scale_colour_manual(values = mycols) + labs(x = 'Year', y = 'Deaths', colour = 'Cause of death') +
  ggtitle('Total deaths') +
  theme(title = element_text(size = 22), legend.title = element_text(size = 16), 
        axis.title = element_text(size = 14), plot.title = element_text(hjust = 0.5))

ggplot(df.ts, aes(x = Date, y = `deaths over 15`, col = `Cause of death (English)`)) +
  geom_line(lwd = 1.2) + scale_colour_manual(values = mycols) + labs(x = 'Year', y = 'Deaths', colour = 'Cause of death') +
  ggtitle('Deaths among those aged 15 or over') +
  theme(title = element_text(size = 22), legend.title = element_text(size = 16), 
        axis.title = element_text(size = 14), plot.title = element_text(hjust = 0.5))

# By death cause
ggplot(t.df.age, aes(x = `age group`, y = croup, fill = `age group`)) + 
  geom_col() + 
  ggtitle('Croup, deaths in 1888-1891') +
  labs(x = 'Age group', y = 'Deaths', colour = 'Age group')  +
  theme(axis.text.x = element_text(angle = 90), title = element_text(size = 22), legend.title = element_blank(), 
        axis.title = element_text(size = 14), plot.title = element_text(hjust = 0.5))

ggplot(t.df.age, aes(x = `age group`, y = `gastro-intestinal infection`, fill = `age group`)) + 
  geom_col() +
  ggtitle('Gastro-intestinal infection, deaths in 1888-1891') +
  labs(x = 'Age group', y = 'Deaths', colour = 'Age group') +
  theme(axis.text.x = element_text(angle = 90), title = element_text(size = 22), legend.title = element_blank(), 
        axis.title = element_text(size = 14), plot.title = element_text(hjust = 0.5))

ggplot(t.df.age, aes(x = `age group`, y = `pulmonary tuberculosis`, fill = `age group`)) + 
  geom_bar(width = 1, stat = "identity", color = "white") +
  ggtitle('Pulmonary tuberculosis, deaths in 1888-1891') +
  labs(x = 'Age group', y = 'Deaths', colour = 'Age group') +
  theme(axis.text.x = element_text(angle = 90), title = element_text(size = 22), legend.title = element_blank(), 
        axis.title = element_text(size = 14), plot.title = element_text(hjust = 0.5))

ggplot(t.df.age, aes(x = `age group`, y = `pulmonic fever`, fill = `age group`)) + 
  geom_col() +
  ggtitle('Pulmonic fever, deaths in 1888-1891') + 
  labs(x = 'Age group', y = 'Deaths', colour = 'Age group') +
  theme(axis.text.x = element_text(angle = 90), title = element_text(size = 22), legend.title = element_blank(), 
        axis.title = element_text(size = 14), plot.title = element_text(hjust = 0.5))

ggplot(t.df.age, aes(x = `age group`, y = `respiratory disease`, fill = `age group`)) + 
  geom_col() +
  ggtitle('Respiratory disease, deaths in 1888-1891') +
  labs(x = 'Age group', y = 'Deaths', colour = 'Age group') +
  theme(axis.text.x = element_text(angle = 90), title = element_text(size = 22), legend.title = element_blank(), 
        axis.title = element_text(size = 14), plot.title = element_text(hjust = 0.5))

ggplot(t.df.age, aes(x = `age group`, y = `whooping-cough`, fill = `age group`)) + 
  geom_col() +
  ggtitle('Whooping cough, deaths in 1888-1891') +
  labs(x = 'Age group', y = 'Deaths', colour = 'Age group') +
  theme(axis.text.x = element_text(angle = 90), title = element_text(size = 22), legend.title = element_blank(), 
        axis.title = element_text(size = 14), plot.title = element_text(hjust = 0.5))

#Analytics
library(tseries)
library(forecast)
library(vars)

variables <- c('croup', 'gi', 'pt', 'pf',
               'rd', 'wc')
croup <- subset(df.ts, `Cause of death (English)` == 'croup', select = 27)
gi <- subset(df.ts, `Cause of death (English)` == 'gastro-intestinal infection', select = 27)
pt <- subset(df.ts, `Cause of death (English)` == 'pulmonary tuberculosis', select = 27)
pf <- subset(df.ts, `Cause of death (English)` == 'pulmonic fever', select = 27)
rd <- subset(df.ts, `Cause of death (English)` == 'respiratory disease', select = 27)
wc <- subset(df.ts, `Cause of death (English)` == 'whooping-cough', select = 27)

mvts <- cbind(croup, gi, pt, pf, rd, wc)
VARselect(mvts, lag.max = 5)