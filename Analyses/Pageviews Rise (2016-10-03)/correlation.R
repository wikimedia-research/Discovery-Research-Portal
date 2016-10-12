## Correlation

# Single variable
data1 <- grouped_pv_count %>%
  group_by(date) %>%
  summarise(total_pv=sum(web_requests)) %>%
  left_join({
    grouped_pv_count %>%
      mutate(country=ifelse(country=="United States", country, "Other Country")) %>%
      group_by(date,country) %>%
      summarise(pv=sum(web_requests))%>%
      tidyr::spread(country, pv, fill=0)
  }, by="date") %>%
  left_join({
    grouped_pv_count %>%
      mutate(os=ifelse(os=="Android", "Android_os", "Other OS")) %>%
      group_by(date,os) %>%
      summarise(pv=sum(web_requests))%>%
      tidyr::spread(os, pv, fill=0)
  }, by="date") %>%
  left_join({
    grouped_pv_count %>%
      mutate(browser=ifelse(browser=="Android", "Android_browser", "Other Browser")) %>%
      group_by(date,browser) %>%
      summarise(pv=sum(web_requests))%>%
      tidyr::spread(browser, pv, fill=0)
  }, by="date")
cor(data1[,-1])[,1]
max(cor(data1[,-1])[-1,1]) # max Android_os      0.9894352

# Two variables
data2 <- grouped_pv_count %>%
  group_by(date) %>%
  summarise(total_pv=sum(web_requests)) %>%
  left_join({
    grouped_pv_count %>%
      mutate(country=ifelse(country=="United States", country, "Other Country"), os=ifelse(os=="Android", "Android_os", "Other OS")) %>%
      group_by(date,country,os) %>%
      summarise(pv=sum(web_requests))%>%
      ungroup() %>%
      mutate(groupc=paste(country,os,sep="_")) %>%
      select(-country,-os) %>%
      tidyr::spread(groupc, pv, fill=0)
  }, by="date") %>%
  left_join({
    grouped_pv_count %>%
      mutate(country=ifelse(country=="United States", country, "Other Country"), browser=ifelse(browser=="Android", "Android_browser", "Other Browser")) %>%
      group_by(date,country,browser) %>%
      summarise(pv=sum(web_requests))%>%
      ungroup() %>%
      mutate(groupc=paste(country,browser,sep="_")) %>%
      select(-country,-browser) %>%
      tidyr::spread(groupc, pv, fill=0)
  }, by="date") %>%
  left_join({
    grouped_pv_count %>%
      mutate(os=ifelse(os=="Android", "Android_os", "Other OS"), browser=ifelse(browser=="Android", "Android_browser", "Other Browser")) %>%
      group_by(date,os,browser) %>%
      summarise(pv=sum(web_requests))%>%
      ungroup() %>%
      mutate(groupc=paste(os,browser,sep="_")) %>%
      select(-os,-browser) %>%
      tidyr::spread(groupc, pv, fill=0)
  }, by="date")
cor(data2[,-1]) 
max(cor(data2[,-1])[-1,1]) # max Android_os_Android_browser    0.9893029   


# Three variables
data3 <- grouped_pv_count %>%
  group_by(date) %>%
  summarise(total_pv=sum(web_requests)) %>%
  left_join({
    grouped_pv_count %>%
      mutate(country=ifelse(country=="United States", country, "Other Country"), os=ifelse(os=="Android", "Android_os", "Other OS"),browser=ifelse(browser=="Android", "Android_browser", "Other Browser")) %>%
      group_by(date,country,os,browser) %>%
      summarise(pv=sum(web_requests))%>%
      ungroup() %>%
      mutate(groupc=paste(country,os,browser,sep="_")) %>%
      select(-country,-os,-browser) %>%
      tidyr::spread(groupc, pv, fill=0)
  }, by="date") 
data3_perc <-  cbind(data3$date, as.data.frame(sapply(data3[,3:9],function(x) x/data3[,2])))
cor(data3[,-1]) [,1]
max(cor(data3[,-1])[-1,1]) # United States_Android_os_Android_browser  0.9786050   

data3_long <- grouped_pv_count %>%
  group_by(date) %>%
  summarise(pv=sum(web_requests)) %>%
  mutate(groupc="all") %>%
  bind_rows({
    grouped_pv_count %>%
      mutate(country=ifelse(country=="United States", country, "Other Country"), os=ifelse(os=="Android", "Android_os", "Other OS"),browser=ifelse(browser=="Android", "Android_browser", "Other Browser")) %>%
      group_by(date,country,os,browser) %>%
      summarise(pv=sum(web_requests))%>%
      ungroup() %>%
      mutate(groupc=paste(country,os,browser,sep="_")) %>%
      select(-country,-os,-browser) 
  }) 
{data3_long %>%
  filter(groupc != "Other Country_Other OS_Android_browser") %>%
  ggplot(aes(x = date, y = pv, color = groupc)) +
  geom_line() +
  scale_y_log10(labels = polloi::compress) +
  scale_colour_hue(name="Groups") + 
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(panel.grid = element_line(color = "gray70"),
        panel.grid.major = element_line(color = "gray60")) +
  ggtitle("Wikipedia.org Portal Pageviews", "Sliced by Country, OS and Browsers, August 5 - October 4, 2016")} %>%
  ggsave("pv_sliced_by3.png", plot = ., path = "figures",
         width = 8, height = 4, dpi = 150)

################################################

# correlation with no. sessions

all_country_data <- readr::read_delim("https://datasets.wikimedia.org/aggregate-datasets/portal/all_country_data.tsv", delim = "\t", col_types = "Dcididid") 
daily_session <- all_country_data %>%
  group_by(date) %>%
  summarise(n_session=sum(n_session)) %>% 
  filter(date<=as.Date("2016-10-04"), date!=as.Date("2016-09-13"))
readr::write_rds(daily_session, "data/daily_session.rds", "gz")
daily_session <- readr::read_rds("data/daily_session.rds")

{ggplot(grouped_pv_count%>%group_by(date)%>%summarise(pageviews=sum(web_requests))%>%filter(date>=as.Date("2016-08-11"),date!=as.Date("2016-09-13")), 
       aes(x = date, y = pageviews), 
       color = RColorBrewer::brewer.pal(5, "Set1")[1]) +
  geom_line() +
  geom_line(data = daily_session %>%
              filter(date<=as.Date("2016-10-04"),date!=as.Date("2016-09-13")),
            aes(x = date, y = n_session),
            color = RColorBrewer::brewer.pal(5, "Set1")[2]) + 
  scale_y_log10(labels = polloi::compress) +
  scale_x_date(date_breaks = "1 month", date_labels = "%B") +
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(panel.grid = element_line(color = "gray70"),
        panel.grid.major = element_line(color = "gray60")) +
  ggtitle("Wikipedia.org Portal Pageviews vs No. Sessions", subtitle = "August 11 - October 4, 2016")} %>%
  ggsave("pv_vs_n_session.png", plot = ., path = "figures",
         width = 8, height = 4, dpi = 150)

cor(grouped_pv_count%>%group_by(date)%>%summarise(pageviews=sum(web_requests))%>%filter(date>=as.Date("2016-08-11"), date!=as.Date("2016-09-13"))%>%{.$pageviews},
    daily_session%>%{.$n_session}
    ) #0.3813088

bpfit_nsession <- bfast(ts(daily_session %>% filter(date<=as.Date("2016-10-04"), date!=as.Date("2016-09-13"))%>%{.$n_session}, frequency=7), h=.25, season="harmonic", max.iter=100)
bpfit_nsession; plot(bpfit_nsession) #no jump

################################################

## Pageviews to other Wikimedia pages

# enwiki by access
enwiki_access <- project_pageviews(project = "en.wikipedia", start = as.Date("2016-08-05"), end = as.Date("2016-10-04")
                            , user_type = "user", platform = c("desktop", "mobile-web", "mobile-app"))
{ggplot(enwiki_access, aes(x = date, y = views, color=access))+
  geom_line() +
  scale_y_log10(labels = polloi::compress) +
  scale_colour_hue(name="Groups") + 
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(panel.grid = element_line(color = "gray70"),
        panel.grid.major = element_line(color = "gray60")) +
  ggtitle("English Wikipedia Pageviews by Access Method", "August 5 - October 4, 2016")} %>%
  ggsave("enwiki_by_access.png", plot = ., path = "figures",
         width = 8, height = 6, dpi = 150)

# enwiki main page
enwiki_main <- article_pageviews(project = "en.wikipedia",
                  article = "Main_Page", platform = "all",
                  user_type = "user", start = as.Date("2016-08-05"), end = as.Date("2016-10-04")) %>% 
  select(date, pageviews=views) %>% mutate(project="enwiki_main",date=as.Date(date))

# all platform
enwiki_all <- project_pageviews(project = "en.wikipedia", start = as.Date("2016-08-05"), end = as.Date("2016-10-04")
                                   , user_type = "user", platform = "all") %>% select(date, pageviews=views) %>% mutate(project="enwiki",date=as.Date(date))

# compare enwiki, enwiki_main, portal
data4 <- grouped_pv_count%>%group_by(date)%>%summarise(pageviews=sum(web_requests))%>%mutate(project="portal")%>%bind_rows(enwiki_all, enwiki_main)
{ggplot(data4, 
        aes(x = date, y = pageviews, color=project))+
  geom_line() +
  scale_y_log10(labels = polloi::compress) +
  scale_colour_hue(name="Groups") + 
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(panel.grid = element_line(color = "gray70"),
        panel.grid.major = element_line(color = "gray60")) +
  ggtitle("Wikipedia Pageviews", "Portal vs English Wikipedia vs English Wikipedia Main Page, August 5 - October 4, 2016")} %>%
  ggsave("portal_enwiki_enmain.png", plot = ., path = "figures",
         width = 8, height = 6, dpi = 150)
# correlation
cor(tidyr::spread(data4, key=project, value=pageviews)%>%select(-date)) #negatively corr

# no sessions vs enwiki pv
cor(cbind(daily_session$n_session, enwiki_all%>%filter(date>=as.Date("2016-08-11"),date!=as.Date("2016-09-13"))%>%{.$pageviews}, enwiki_main%>%filter(date>=as.Date("2016-08-11"),date!=as.Date("2016-09-13"))%>%{.$pageviews}))
# 0.1747277 -0.2258840