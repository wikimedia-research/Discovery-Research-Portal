# Remote:
con <- wmf::mysql_connect("log", default_file = "/etc/mysql/conf.d/analytics-research-client.cnf")
data <- wmf::mysql_read("
                        SELECT *
                        FROM TestSearchSatisfaction2_15357244
                        WHERE
                        LEFT(`timestamp`, 8) >= '20160407' AND LEFT(`timestamp`, 8) <= '20160426'
                        AND event_source = 'fulltext';   
                        ", "log", con = con)
save(data, file = "swap2and3.RData", ascii = FALSE, compress = "gzip")
