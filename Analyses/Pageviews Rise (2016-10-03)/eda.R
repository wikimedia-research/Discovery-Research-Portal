library(data.table)
library(ggplot2)
library(dplyr)

#dir.create("figures")

grouped_pv_count <- readr::read_rds("data/portal_webrequest_counts_20160804-20161004.rds")
grouped_pv_count <- grouped_pv_count[!(grouped_pv_count$country=='1'|grouped_pv_count$date==as.Date("2016-08-04")), ]
pv_count_by_qntl <- readr::read_rds("data/portal_pageviews_counts_20160804-20161004.rds")
pv_count_by_qntl <- pv_count_by_qntl[!pv_count_by_qntl$date==as.Date("2016-08-04"),]
us_pv_count_by_qntl <- readr::read_rds("data/portal_us_pageviews_counts_20160904-20161005.rds")
dashboard_pv <- readr::read_rds("data/portal_pageviews.rds")

{
  ggplot(dashboard_pv, aes(x = date, y = pageviews)) +
  geom_line() +
  #geom_line(data = grouped_pv_count%>%group_by(date)%>%summarise(pv=sum(web_requests)), aes(x = date, y = pv),
  #          color = RColorBrewer::brewer.pal(3, "Set1")[3], alpha = 0.8) + # check if the count is the same as dashboard
  scale_y_continuous(labels = polloi::compress) +
  scale_x_date(date_breaks = "1 month", date_labels = "%B") +
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(panel.grid = element_line(color = "gray70"),
        panel.grid.major = element_line(color = "gray60")) +
  geom_vline(xintercept = as.numeric(as.Date("2016-09-05")),
             linetype = "dashed", color = "cornflowerblue") +
  geom_vline(xintercept = as.numeric(max(pv_count_by_qntl$date)),
             linetype = "dashed", color = "cornflowerblue") +
  ggtitle("Wikipedia.org Portal Pageviews in 2016",
          subtitle = "via data collection script in wikimedia/discovery/golden")
} %>% ggsave("dash_pageviews.png", plot = ., path = "figures",
             width = 8, height = 6, dpi = 150)

# Group by country
{grouped_pv_count %>%
  mutate(country=ifelse(country=="United States", country, "Other Countries")) %>%
  group_by(date, country) %>%
  summarize(pageviews=sum(web_requests)) %>%
  ggplot(aes(x = date, y = pageviews, color = country)) +
  geom_line() +
  scale_y_continuous(labels = polloi::compress) +
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(panel.grid = element_line(color = "gray70"),
        panel.grid.major = element_line(color = "gray60")) +
  ggtitle("Wikipedia.org Portal Pageviews by Country", subtitle="U.S. vs Other Countries, August 5 - October 4, 2016")} %>%
  ggsave("pv_us_other.png", plot = ., path = "figures",
         width = 8, height = 6, dpi = 150)
# looks like pattern only in US

{grouped_pv_count %>%
  mutate(country=ifelse(country=="United States", country, "Other Countries")) %>%
  group_by(date, country) %>%
  summarize(pageviews=sum(web_requests)) %>%
  mutate(proportion=pageviews/sum(pageviews)) %>%
  ggplot(aes(x = date, y = proportion, color = country)) +
  geom_line() +
  scale_y_continuous() +
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(panel.grid = element_line(color = "gray70"),
        panel.grid.major = element_line(color = "gray60")) +
  ggtitle("Proportion of Wikipedia.org Portal Pageviews by Country", subtitle="U.S. vs Other Countries, August 5 - October 4, 2016")} %>%
  #ggsave("pv_prop_us_other.png", plot = ., path = "figures",
  #       width = 8, height = 6, dpi = 150)

# Further break down by os
grouped_pv_count %>%
  filter(country=="United States") %>%
  mutate(os=ifelse(os=="Android", os, "Other")) %>%
  group_by(date, os) %>%
  summarize(pageviews=sum(web_requests)) %>%
  ggplot(aes(x = date, y = pageviews, color = os)) +
  geom_line() +
  scale_y_log10(labels = polloi::compress) +
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(panel.grid = element_line(color = "gray70"),
        panel.grid.major = element_line(color = "gray60")) +
  ggtitle("Wikipedia.org Portal Pageviews from US")
# Android

# Further break down by browser
grouped_pv_count %>%
  filter(country=="United States", os=="Android") %>%
  mutate(browser=ifelse(browser=="Android", browser, "Other")) %>%
  group_by(date, browser) %>%
  summarize(pageviews=sum(web_requests)) %>%
  ggplot(aes(x = date, y = pageviews, color = browser)) +
  geom_line() +
  scale_y_log10(labels = polloi::compress) +
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(panel.grid = element_line(color = "gray70"),
        panel.grid.major = element_line(color = "gray60")) +
  ggtitle("Wikipedia.org Portal Pageviews from US Android os")
# Android

# Further break down by referer_class/http_status
grouped_pv_count %>%
  filter(country=="United States", os=="Android", browser=="Android") %>%
  #mutate(device=ifelse(device=="Android", device, "Other")) %>%
  group_by(date, referer_class) %>%
  summarize(pageviews=sum(web_requests)) %>%
  ggplot(aes(x = date, y = pageviews, color = referer_class)) +
  geom_line() +
  scale_y_log10(labels = polloi::compress) +
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(panel.grid = element_line(color = "gray70"),
        panel.grid.major = element_line(color = "gray60")) +
  ggtitle("Wikipedia.org Portal Pageviews from US Android os Android browser")
# none referer, http_status not explaining


# Further break down by device
top20_device <- grouped_pv_count %>%
  filter(country=="United States", os=="Android", browser=="Android") %>%
  group_by(device) %>%
  summarize(count=sum(web_requests)) %>%
  arrange(desc(count)) %>%
  top_n(50, count)
grouped_pv_count %>%
  filter(country=="United States", os=="Android", browser=="Android") %>%
  mutate(device=ifelse(device %in% top20_device$device, "top20", "Other")) %>%
  group_by(date, device) %>%
  summarize(pageviews=sum(web_requests)) %>%
  ggplot(aes(x = date, y = pageviews, color = device)) +
  geom_line() +
  scale_y_log10(labels = polloi::compress) +
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(panel.grid = element_line(color = "gray70"),
        panel.grid.major = element_line(color = "gray60")) +
  ggtitle("Wikipedia.org Portal Pageviews from US Android os Android browser")
# in top 20, looks like only one don't follow the pattern
# top20/30/50 vs other, pattern looks similar, device not explaining

# Further break down by access_method
grouped_pv_count %>%
  filter(country=="United States", os=="Android", browser=="Android") %>%
  #mutate(device=ifelse(device %in% top20_device$device, "top20", "Other")) %>%
  group_by(date, access_method) %>%
  summarize(pageviews=sum(web_requests)) %>%
  ggplot(aes(x = date, y = pageviews, color = access_method)) +
  geom_line() +
  scale_y_log10(labels = polloi::compress) +
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(panel.grid = element_line(color = "gray70"),
        panel.grid.major = element_line(color = "gray60")) +
  ggtitle("Wikipedia.org Portal Pageviews from US Android os Android browser")
# looks like only very few certain devices are identify as mobile app (total 141 pvs), so not interested in access_method

#############################
# Univariate: check of referer_class, browser, os 
{grouped_pv_count %>%
  mutate(os=ifelse(os=="Android", os, "Other")) %>%
  group_by(date, os) %>%
  summarize(pageviews=sum(web_requests)) %>%
  ggplot(aes(x = date, y = pageviews, color = os)) +
  geom_line() +
  scale_y_continuous(labels = polloi::compress) +
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(panel.grid = element_line(color = "gray70"),
        panel.grid.major = element_line(color = "gray60")) +
  ggtitle("Wikipedia.org Portal Pageviews by Operating Systems", subtitle="Android vs Other, August 5 - October 4, 2016")} %>%
ggsave("pv_os_android_other.png", plot = ., path = "figures",
       width = 8, height = 6, dpi = 150)

{grouped_pv_count %>%
  mutate(os=ifelse(os=="Android", os, "Other")) %>%
  group_by(date, os) %>%
  summarize(pageviews=sum(web_requests)) %>%
  mutate(proportion=pageviews/sum(pageviews)) %>%
  ggplot(aes(x = date, y = proportion, color = os)) +
  geom_line() +
  scale_y_continuous() +
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(panel.grid = element_line(color = "gray70"),
        panel.grid.major = element_line(color = "gray60")) +
  ggtitle("Proportion of Wikipedia.org Portal Pageviews by Operating Systems", subtitle="Android vs Other, August 5 - October 4, 2016")} %>%
  ggsave("pv_proportion_os_android_other.png", plot = ., path = "figures",
         width = 8, height = 6, dpi = 150)

{grouped_pv_count %>%
  mutate(browser=ifelse(browser=="Android", browser, "Other")) %>%
  group_by(date, browser) %>%
  summarize(pageviews=sum(web_requests)) %>%
  ggplot(aes(x = date, y = pageviews, color = browser)) +
  geom_line() +
  scale_y_continuous(labels = polloi::compress) +
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(panel.grid = element_line(color = "gray70"),
        panel.grid.major = element_line(color = "gray60")) +
  ggtitle("Wikipedia.org Portal Pageviews by Browsers", subtitle="Android vs Other, August 5 - October 4, 2016")} %>%
  ggsave("pv_browser_android_other.png", plot = ., path = "figures",
         width = 8, height = 6, dpi = 150)

{grouped_pv_count %>%
  mutate(browser=ifelse(browser=="Android", browser, "Other")) %>%
  group_by(date, browser) %>%
  summarize(pageviews=sum(web_requests)) %>%
  mutate(proportion=pageviews/sum(pageviews)) %>%
  ggplot(aes(x = date, y = proportion, color = browser)) +
  geom_line() +
  scale_y_continuous() +
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(panel.grid = element_line(color = "gray70"),
        panel.grid.major = element_line(color = "gray60")) +
  ggtitle("Proportion of Wikipedia.org Portal Pageviews by Browsers", subtitle="Android vs Other, August 5 - October 4, 2016")} %>%
  ggsave("pv_proportion_browser_android_other.png", plot = ., path = "figures",
         width = 8, height = 6, dpi = 150)

grouped_pv_count %>%
  #mutate(referer_class=ifelse(referer_class=="Android", referer_class, "Other")) %>%
  group_by(date, referer_class) %>%
  summarize(pageviews=sum(web_requests)) %>%
  ggplot(aes(x = date, y = pageviews, color = referer_class)) +
  geom_line() +
  scale_y_log10(labels = polloi::compress) +
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(panel.grid = element_line(color = "gray70"),
        panel.grid.major = element_line(color = "gray60")) +
  ggtitle("Wikipedia.org Portal Pageviews by Referer class", subtitle="August 5 - October 4, 2016")
# referer_class: none and external(search engine) have similar pattern
# browser: Android similar pattern
# os: Android similar pattern


# plot together and compare
ggplot(grouped_pv_count%>%group_by(date)%>%summarise(pageviews=sum(web_requests)), 
       aes(x = date, y = pageviews), 
       color = RColorBrewer::brewer.pal(5, "Set1")[1]) +
  geom_line() +
  geom_line(data = grouped_pv_count%>%filter(country=="United States")%>%group_by(date)%>%summarise(pv=sum(web_requests)), 
            aes(x = date, y = pv),
            color = RColorBrewer::brewer.pal(5, "Set1")[2]) + 
  geom_line(data = grouped_pv_count%>%filter(os=="Android")%>%group_by(date)%>%summarise(pv=sum(web_requests)), 
            aes(x = date, y = pv),
            color = RColorBrewer::brewer.pal(5, "Set1")[3]) + 
  geom_line(data = grouped_pv_count%>%filter(browser=="Android")%>%group_by(date)%>%summarise(pv=sum(web_requests)), 
            aes(x = date, y = pv),
            color = RColorBrewer::brewer.pal(5, "Set1")[4]) + 
  #geom_line(data = grouped_pv_count%>%filter(referer_class=="none")%>%group_by(date)%>%summarise(pv=sum(web_requests)), 
  #          aes(x = date, y = pv),
  #         color = RColorBrewer::brewer.pal(5, "Set1")[5]) + 
  scale_y_continuous(labels = polloi::compress) +
  scale_x_date(date_breaks = "1 month", date_labels = "%B") +
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(panel.grid = element_line(color = "gray70"),
        panel.grid.major = element_line(color = "gray60")) +
  ggtitle("Wikipedia.org Portal Pageviews in 2016")
# referer_class==“none” account for too much pv, not interesting to investigate
# Android browser and os almost the same pattern


###################################################

{pv_count_by_qntl %>%
  tidyr::gather(key=quantile_buck, value=pv, -date) %>%
  mutate(client_type=ifelse(quantile_buck %in% c("f","pageviews"), quantile_buck, "other")) %>%
  group_by(date, client_type) %>%
  summarize(pageviews=sum(pv)) %>%
  ggplot(aes(x = date, y = pageviews, color = client_type)) +
  geom_line() +
  scale_y_log10(labels = polloi::compress) +
  scale_colour_hue(name="Client Type",
                      breaks=c("f", "other", "pageviews"),
                      labels=c("High-volume Clients", "Low-Volume Clients", "Total Pageviews")) + 
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(panel.grid = element_line(color = "gray70"),
        panel.grid.major = element_line(color = "gray60")) +
  ggtitle("Wikipedia.org Portal Pageviews", subtitle="High-volume Clients vs Low-Volume Clients, August 5 - October 4, 2016")} %>%
  ggsave("pv_high_low_volume.png", plot = ., path = "figures",
         width = 8, height = 6, dpi = 150)

library(bfast)
source("seasonal.R")
bpfit_all <- bfast(ts(pv_count_by_qntl$pageviews, frequency=7), h=.25, season="harmonic", max.iter=100)
#bpfit_all <- bfast(ts(dashboard_pv$pageviews[dashboard_pv$date>=as.Date("2016-07-11")], frequency=7), h=.25, season="harmonic", max.iter=100)
bpfit_all; plot(bpfit_all)
png("figures/total_pv_decompose.png",width = 5, height = 5, units = "in", res = 300)
out <- bpfit_all$output[[1]]
ft <- cbind(seasonal = out$St, trend = out$Tt, remainder = out$Nt)
tsp(ft) <- tsp(bpfit_all$Yt)
ft <- list(time.series = ft)
seasonal(ft, out, main = "BFAST Decomposition: Total Pageviews")
dev.off()
pv_count_by_qntl$date[33] #9/6

bpfit_high <- bfast(ts(pv_count_by_qntl$f, frequency=7), h=.25, season="harmonic", max.iter=100)
bpfit_high; plot(bpfit_high)
png("figures/high_pv_decompose.png",width = 5, height = 5, units = "in", res = 300)
out <- bpfit_high$output[[1]]
ft <- cbind(seasonal = out$St, trend = out$Tt, remainder = out$Nt)
tsp(ft) <- tsp(bpfit_high$Yt)
ft <- list(time.series = ft)
seasonal(ft, out, main = "BFAST Decomposition: High Volume Clients Pageviews")
dev.off()
pv_count_by_qntl$date[33] #9/6

bpfit_low <- bfast(ts(pv_count_by_qntl$pageviews-pv_count_by_qntl$f, frequency=7), h=.25, season="harmonic", max.iter=100)
bpfit_low; plot(bpfit_low)
png("figures/low_pv_decompose.png",width = 5, height = 5, units = "in", res = 300)
out <- bpfit_low$output[[1]]
ft <- cbind(seasonal = out$St, trend = out$Tt, remainder = out$Nt)
tsp(ft) <- tsp(bpfit_low$Yt)
ft <- list(time.series = ft)
seasonal(ft, out, main = "BFAST Decomposition: Low Volume Clients Pageviews")
dev.off()

# read in top 30 IP everyday
top30ip_pv <- readr::read_rds("data/portal_pageviews_top30IP_counts_20160806-20161006.rds")
table(top30ip_pv %>%
  group_by(date) %>%
  top_n(1, pageviews) %>% {.$client_ip})
tail(sort(table(top30ip_pv$client_ip)),10)
top30ip_pv %>% arrange(desc(pageviews)) %>% head(20)
top30ip_pv %>% group_by(client_ip) %>% summarise(pv=sum(pageviews))%>%arrange(desc(pv))

# total pv
ggplot(grouped_pv_count%>%group_by(date)%>%summarise(pageviews=sum(web_requests))%>%filter(date>=as.Date("2016-08-06")), 
       aes(x = date, y = pageviews), 
       color = RColorBrewer::brewer.pal(5, "Set1")[1]) +
  geom_line() +
  geom_line(data = top30ip_pv %>%
              filter(date<=as.Date("2016-10-04"), !client_ip %in% c("0.0.0.0","0.0.0.0")) %>%
              group_by(date) %>%
              #top_n(1, pageviews) %>% 
              summarise(pageviews=sum(pageviews)),
            aes(x = date, y = pageviews),
            color = RColorBrewer::brewer.pal(5, "Set1")[2]) + 
  scale_y_log10(labels = polloi::compress) +
  scale_x_date(date_breaks = "1 month", date_labels = "%B") +
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(panel.grid = element_line(color = "gray70"),
        panel.grid.major = element_line(color = "gray60")) +
  ggtitle("Wikipedia.org Portal Pageviews in 2016", subtitle = "Total PV vs. Top30-2 PV")

top30_pv_total <-  top30ip_pv %>%
  filter(date<=as.Date("2016-10-04"), !client_ip %in% c("0.0.0.0","0.0.0.0")) %>%
  group_by(date) %>%
  #top_n(1, pageviews) %>% 
  summarise(pageviews=sum(pageviews))
bpfit_top30 <- bfast(ts(top30_pv_total$pageviews, frequency=7), h=.25, season="harmonic", max.iter=100)
bpfit_top30; plot(bpfit_top30)

# check US quantile
us_pv_count_by_qntl %>%
  tidyr::gather(key=quantile_buck, value=pv, -date) %>%
  #mutate(quantile_buck=ifelse(quantile_buck %in% c("f","pageviews"), quantile_buck, "other")) %>%
  #group_by(date, quantile_buck) %>%
  #summarize(pv=sum(pv)) %>%
  ggplot(aes(x = date, y = pv, color = quantile_buck)) +
  geom_line() +
  scale_y_log10(labels = polloi::compress) +
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(panel.grid = element_line(color = "gray70"),
        panel.grid.major = element_line(color = "gray60")) +
  ggtitle("US Wikipedia.org Portal Pageviews by quantile bucket")