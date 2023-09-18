
library(ggplot2)
library(gridExtra)
library(dplyr)

MyPath <- "C:/Users/filhoam/Desktop/Breast/Round2/Figure/"


# Function for reading and processing data
process_df <- function(file_path, index) {
  df = read.csv(file_path)
  df$year <- df$Year.of.diagnosis + 1974
  names(df) <- c("year_num", "er", "asr", "cases", "py", "year")
  df$er_label <- factor(df$er, labels = c("ER+", "ER-", "Borderline/Unk", "Not available"))
  refs <- df$asr[df$year == 1990]
  df <- df %>% 
    group_by(er) %>% 
    mutate(rr = round((asr/refs[er + 1]), 2)) %>% 
    ungroup()
  df$index <- index
  df$site <- "DCIS"
  return(df)
}

# File paths
file_paths <- c('file_path1' = 'C:/Users/filhoam/Desktop/Breast/Round2/ER/dcis_er_30_49.csv',
                'file_path2' = 'C:/Users/filhoam/Desktop/Breast/Round2/ER/dcis_er_50_84.csv',
                'file_path3' = 'C:/Users/filhoam/Desktop/Breast/Round2/ER/dcis_er.csv'
)

# Indices
indices <- c('30_49', '50_84', '30_84')

# Process dataframes
dfs <- mapply(process_df, file_path = file_paths, index = indices, SIMPLIFY = F)

# Combine dataframes
dcis <- do.call(rbind, dfs)

# IDC

# Function for reading and processing data
process_df <- function(file_path, index) {
  df = read.csv(file_path)
  df$year <- df$Year.of.diagnosis + 1974
  names(df) <- c("year_num", "er", "asr", "cases", "py", "year")
  df$er_label <- factor(df$er, labels = c("ER+", "ER-", "Borderline/Unk", "Not available"))
  refs <- df$asr[df$year == 1990]
  df <- df %>% 
    group_by(er) %>% 
    mutate(rr = round((asr/refs[er + 1]), 2)) %>% 
    ungroup()
  df$index <- index
  df$site <- "IDC"
  return(df)
}

# File paths
file_paths <- c('file_path1' = 'C:/Users/filhoam/Desktop/Breast/Round2/ER/idc_er_30_49.csv',
                'file_path2' = 'C:/Users/filhoam/Desktop/Breast/Round2/ER/idc_er_50_84.csv',
                'file_path3' = 'C:/Users/filhoam/Desktop/Breast/Round2/ER/idc_er.csv'
)

# Indices
indices <- c('30_49', '50_84', '30_84')

# Process dataframes
dfs <- mapply(process_df, file_path = file_paths, index = indices, SIMPLIFY = F)

# Combine dataframes
idc <- do.call(rbind, dfs)

#
data <- rbind(dcis, idc)

#write.csv(data, 'C:/Users/filhoam/Desktop/Breast/Round2/ER/data_er_9162023.csv')

##
# Function to generate ggplot graph
generate_plot <- function(data, title, ylim_val) {
  title = paste0(title)
  plot <- ggplot(data, aes(x=year, y= rr, group=er_label)) +
    geom_point(aes(shape=er_label, color=er_label)) +
    scale_color_manual(values=c('#374e55','#df8f44','darkgray'))+ 
    ylim (0, ylim_val) +
    labs(x="Year", y="Incidence relative to 1990") +
    geom_hline(yintercept=1, linetype="dashed", size=0.3) +
    theme_bw() +
    theme(legend.position="none") +
    ggtitle(title)+
    theme(plot.title = element_text(size = 5),
          axis.text.x = element_text(size = 5),
          axis.text.y = element_text(size = 5),
          axis.title.x = element_text(size = 5),
          axis.title.y = element_text(size = 5))
  return(plot)
}

# Define the subset of your data for each index
indices <- list("30_84", "30_49", "50_84")

#
a <- subset(data, data$site=="IDC" & data$index=="30_84" &data$er_label!="Not available" & data$year >= 1990)
b <- subset(data, data$site=="IDC" & data$index=="30_49" &data$er_label!="Not available" & data$year >= 1990)
c <- subset(data, data$site=="IDC" & data$index=="50_84" &data$er_label!="Not available" & data$year >= 1990)

a1 <- subset(data, data$site=="DCIS" & data$index=="30_84" &data$er_label!="Not available" & data$year >= 1990)
b1 <- subset(data, data$site=="DCIS" & data$index=="30_49" &data$er_label!="Not available" & data$year >= 1990)
c1 <- subset(data, data$site=="DCIS" & data$index=="50_84" &data$er_label!="Not available" & data$year >= 1990)

# Add a title to your plots

plot30_84 <- generate_plot(a, '30 - 84', 5) + 
  ggtitle(paste0('IDC, ', gsub('_', ' - ', '30 - 84')))
assign(paste0('plot', '30 - 84'), plot)

plot30_49 <- generate_plot(b, '30 - 49', 5) + 
  ggtitle(paste0('IDC, ', gsub('_', ' - ', '30 - 49')))
assign(paste0('plot', '30 - 49'), plot)

plot50_84 <- generate_plot(c, '50 - 84', 5) + 
  ggtitle(paste0('IDC, ', gsub('_', ' - ', '50 - 84')))
assign(paste0('plot', '50 - 84'), plot)

#

plotA30_84 <- generate_plot(a1, '30 - 84', 20) + 
  ggtitle(paste0('DCIS, ', gsub('_', ' - ', '30 - 84')))
assign(paste0('plot', '30 - 84'), plot)

plotA30_49 <- generate_plot(b1, '30 - 49', 20) + 
  ggtitle(paste0('DCIS, ', gsub('_', ' - ', '30 - 49')))
assign(paste0('plot', '30 - 49'), plot)

plotA50_84 <- generate_plot(c1, '50 - 84', 20) + 
  ggtitle(paste0('DCIS, ', gsub('_', ' - ', '50 - 84')))
assign(paste0('plot', '50 - 84'), plot)

#

# Create the PDF
pdf(file=paste(MyPath,"Fig36.pdf",sep=""),width=5,height=4)
grid.arrange(plot30_84, plot30_49, plot50_84,
             plotA30_84, plotA30_49, plotA50_84, ncol=3, nrow=2, top = grid::textGrob('', gp=grid::gpar(fontsize=8)))
dev.off()
































#######


df3 <-subset(data, data$site == "IDC" & data$index == "30_84" & data$er_label != "Not available" & data$year >= 1990)

a <- ggplot(df3, aes(x=year, y= rr, group=er_label)) +
  geom_point(aes(shape=er_label, color=er_label))+
  ylim (0, 5)+
  labs(x="Year", y="Incidence relative to 1990")+geom_hline(yintercept=1, linetype="dashed", size=0.3)+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle(paste0(" IDC, 30 - 84" ))+
  theme(plot.title = element_text(size = 5))+
  theme(axis.text.x = element_text(size = 5))+
  theme(axis.text.y = element_text(size = 5))+
  theme(axis.title.x = element_text(size = 5))+
  theme(axis.title.y = element_text(size = 5))


#
df4 <-subset(data, data$site == "IDC" & data$index == "30_49" & data$er_label != "Not available" & data$year >= 1990)

b <- ggplot(df4, aes(x=year, y= rr, group=er_label)) +
  geom_point(aes(shape=er_label, color=er_label))+
  ylim (0, 5)+
  labs(x="Year", y="Incidence relative to 1990")+geom_hline(yintercept=1, linetype="dashed", size=0.3)+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle(paste0(" IDC, 30 - 49" ))+
  theme(plot.title = element_text(size = 5))+
  theme(axis.text.x = element_text(size = 5))+
  theme(axis.text.y = element_text(size = 5))+
  theme(axis.title.x = element_text(size = 5))+
  theme(axis.title.y = element_text(size = 5))


#
df5 <-subset(data, data$site == "IDC" & data$index == "50_84" & data$er_label != "Not available" & data$year >= 1990)

c <- ggplot(df5, aes(x=year, y= rr, group=er_label)) +
  geom_point(aes(shape=er_label, color=er_label))+
  ylim (0, 5)+
  labs(x="Year", y="Incidence relative to 1990")+geom_hline(yintercept=1, linetype="dashed", size=0.3)+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle(paste0(" IDC, 50 - 84" ))+
  theme(plot.title = element_text(size = 5))+
  theme(axis.text.x = element_text(size = 5))+
  theme(axis.text.y = element_text(size = 5))+
  theme(axis.title.x = element_text(size = 5))+
  theme(axis.title.y = element_text(size = 5))

##


df6 <-subset(data, data$site == "DCIS" & data$index == "30_84" & data$er_label != "Not available" & data$year >= 1990)

a1 <- ggplot(df6, aes(x=year, y= rr, group=er_label)) +
  geom_point(aes(shape=er_label, color=er_label))+
  ylim (0, 20)+
  labs(x="Year", y="Incidence relative to 1990")+geom_hline(yintercept=1, linetype="dashed", size=0.3)+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle(paste0(" DCIS, 30 - 84" ))+
  theme(plot.title = element_text(size = 5))+
  theme(axis.text.x = element_text(size = 5))+
  theme(axis.text.y = element_text(size = 5))+
  theme(axis.title.x = element_text(size = 5))+
  theme(axis.title.y = element_text(size = 5))


#
df7 <-subset(data, data$site == "DCIS" & data$index == "30_49" & data$er_label != "Not available" & data$year >= 1990)

b1 <- ggplot(df7, aes(x=year, y= rr, group=er_label)) +
  geom_point(aes(shape=er_label, color=er_label))+
  ylim (0, 20)+
  labs(x="Year", y="Incidence relative to 1990")+geom_hline(yintercept=1, linetype="dashed", size=0.3)+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle(paste0(" DCIS, 30 - 49" ))+
  theme(plot.title = element_text(size = 5))+
  theme(axis.text.x = element_text(size = 5))+
  theme(axis.text.y = element_text(size = 5))+
  theme(axis.title.x = element_text(size = 5))+
  theme(axis.title.y = element_text(size = 5))


#
df8 <-subset(data, data$site == "DCIS" & data$index == "50_84" & data$er_label != "Not available" & data$year >= 1990)

c1 <- ggplot(df8, aes(x=year, y= rr, group=er_label)) +
  geom_point(aes(shape=er_label, color=er_label))+
  ylim (0, 20)+
  labs(x="Year", y="Incidence relative to 1990")+geom_hline(yintercept=1, linetype="dashed", size=0.3)+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle(paste0(" DCIS, 50 - 84" ))+
  theme(plot.title = element_text(size = 5))+
  theme(axis.text.x = element_text(size = 5))+
  theme(axis.text.y = element_text(size = 5))+
  theme(axis.title.x = element_text(size = 5))+
  theme(axis.title.y = element_text(size = 5))

##

pdf(file=paste(MyPath,"Fig5.pdf",sep=""),width=6,height=4)
grid.arrange(a,b,c,a1,b1,c1,ncol=3, nrow=2,
             #bottom = textGrob("Line in black represents the model selected", x = 0.2),
             top = grid::textGrob('', gp=grid::gpar(fontsize=8)))
dev.off()
