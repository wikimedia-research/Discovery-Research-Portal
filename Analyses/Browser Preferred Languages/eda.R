library(magrittr)
import::from(dplyr, tbl_df, group_by, summarize, ungroup,
             select, mutate, rename, keep_where = filter, top_n, arrange)
library(ggplot2)

sessions <- readr::read_rds("data/portal_sessions.rds") # one week's worth of data
# sessions <- readr::read_rds("data/sessions.rds") # one day's worth of data
dir.create("figures")

sessions$Group <- "Non-English only"
sessions$Group[sessions$`Primary language` == "English" & sessions$`Number of Accept-Languages` == 1] <- "English-only"
sessions$Group[sessions$`Primary language` == "English" & sessions$`Number of Accept-Languages` > 1] <- "Multilingual but primarily English"
sessions$Group[sessions$`Includes English` & sessions$`Primary language` != "English"] <- "Primarily non-English, but include English"
sessions$Group[sessions$`Number of Accept-Languages` > 1 & !sessions$`Includes English`] <- "Non-English only (Multilingual)"
table(sessions$Group)

visitors <- sessions %>%
  group_by(date, Group) %>%
  summarize(n = n()) %>%
  mutate(Prop = n/sum(n))
p <- ggplot(data = visitors, aes(x = date, y = Prop, fill = Group)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%a (%d %b)") +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  ggthemes::theme_tufte(base_family = "Gill Sans") +
  theme(panel.grid = element_line(color = "gray20", size = 1, linetype = "solid")) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(title = "Wikipedia Portal Visitors' Demographics", x = "Date", y = NULL) +
  geom_text(data = mutate(visitors, Y = cumsum(Prop) - Prop/2),
            aes(x = date, y = Y, label = sprintf("%.2f%%", 100*Prop)),
            color = "white")
ggsave("demograph_by_language.png", p, path = "figures", width = 8, height = 10)

clickthroughs <- sessions %>%
  keep_where(clickthrough) %>%
  group_by(date, Group) %>%
  summarize(n = n()) %>%
  mutate(Prop = n/sum(n))
p <- ggplot(data = clickthroughs, aes(x = date, y = Prop, fill = Group)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%a (%d %b)") +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  ggthemes::theme_tufte(base_family = "Gill Sans") +
  theme(panel.grid = element_line(color = "gray20", size = 1, linetype = "solid")) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(title = "Wikipedia Portal Clicking-through Visitors' Demographics", x = "", y = NULL) +
  geom_text(data = mutate(clickthroughs, Y = cumsum(Prop) - Prop/2),
            aes(x = date, y = Y, label = sprintf("%.2f%%", 100*Prop)),
            color = "white")
# ggsave("demograph_by_language_and_clickthrough.png", p, path = "figures", width = 8, height = 5)

p1 <- sessions %>%
  mutate(`Primary language` = ifelse(`Primary language` == "English", "English", "Not English")) %>%
  group_by(date, `Primary language`) %>%
  summarize(Overall = sum(clickthrough)/length(clickthrough),
            Search = sum(section %in% "search")/length(section),
            `Primary Links` = sum(section %in% "primary links")/length(section),
            `Secondary Links` = sum(section %in% "secondary links")/length(section)) %>%
  tidyr::gather(Section, ctr, -c(1:2)) %>%
  ggplot(aes(x = date, y = ctr, color = Section, linetype = `Primary language`)) +
  geom_line(size = 1.2) +
  scale_y_continuous(labels = scales::percent_format(), breaks = seq(0, 0.6, 0.1)) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%a (%d %b)") +
  scale_color_brewer(type = "qual", palette = "Set1") +
  scale_linetype_manual(values = c("solid", "twodash")) +
  labs(title = "Clickthrough rate of different sections by language",
       x = "Day", y = "Clickthrough rate") +
  ggthemes::theme_tufte(base_family = "Gill Sans") +
  theme(panel.grid = element_line(color = "gray20", size = 1, linetype = "solid")) +
  theme(legend.position = "bottom")
p2 <- sessions %>%
  mutate(`Accept-Language` = ifelse(`Includes English`, "Includes English", "Does NOT include English")) %>%
  group_by(date, `Accept-Language`) %>%
  summarize(Overall = sum(clickthrough)/length(clickthrough),
            Search = sum(section %in% "search")/length(section),
            `Primary Links` = sum(section %in% "primary links")/length(section),
            `Secondary Links` = sum(section %in% "secondary links")/length(section)) %>%
  tidyr::gather(Section, ctr, -c(1:2)) %>%
  ggplot(aes(x = date, y = ctr, color = Section, linetype = `Accept-Language`)) +
  geom_line(size = 1.2) +
  scale_y_continuous(labels = scales::percent_format(), breaks = seq(0, 0.6, 0.1)) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%a (%d %b)") +
  scale_color_brewer(type = "qual", palette = "Set1") +
  scale_linetype_manual(values = c("twodash", "solid")) +
  labs(title = "Clickthrough rate of different sections by inclusion of English",
       x = "Day", y = "Clickthrough rate") +
  ggthemes::theme_tufte(base_family = "Gill Sans") +
  theme(panel.grid = element_line(color = "gray20", size = 1, linetype = "solid")) +
  theme(legend.position = "bottom")
p <- cowplot::plot_grid(p1, p2, ncol = 1)
cowplot::ggsave("ctr_by_section-language.png", p, path = "figures", width = 8, height = 10)

top_languages <- function(n = 10) {
  sessions %>%
    group_by(`Primary language`) %>%
    summarize(num_sessions = n()) %>%
    top_n(n, num_sessions) %>%
    arrange(desc(num_sessions)) %>%
    { .$`Primary language` }
}

primary_links <- sessions %>%
  keep_where(section == "primary links") %>%
  keep_where(`Primary language` %in% top_languages(20)) %>%
  group_by(`Primary language`, destination) %>%
  summarize(n = n()) %>%
  mutate(Prop = n/sum(n))
p <- ggplot(data = primary_links, aes(x = `Primary language`, y = Prop,
                                      fill = sub("https://", "", destination, fixed = TRUE))) +
  geom_bar(stat = "identity", position = "fill", alpha = 0.7) +
  scale_x_discrete(limits = sort(unique(primary_links$`Primary language`), decreasing = TRUE)) +
  scale_fill_brewer("Destination", type = "qual", palette = "Paired") +
  scale_y_continuous("Proportion", labels = scales::percent_format()) +
  geom_text(data = keep_where(mutate(primary_links, Y = cumsum(Prop) - Prop/2), Prop > 0.05),
            aes(x = `Primary language`, y = Y, label = sprintf("%.0f%%", 100*Prop)),
            color = "black") +
  coord_flip() +
  labs(title = "Which of the primary links the top 20 languages go to", x = "User's most preferred language") +
  ggthemes::theme_tufte(base_family = "Gill Sans") +
  theme(legend.position = "bottom")
ggsave("primary_link_ctr_by_language.png", p, path = "figures", width = 8.5, height = 10)

primary_links_excl_en <- sessions %>%
  keep_where(section == "primary links" & !`Includes English`) %>%
  keep_where(`Primary language` %in% top_languages(20)) %>%
  group_by(`Primary language`, destination) %>%
  summarize(n = n()) %>%
  keep_where(n > 10) %>%
  mutate(Prop = n/sum(n))
p <- ggplot(data = primary_links_excl_en, aes(x = `Primary language`, y = Prop,
                                      fill = sub("https://", "", destination, fixed = TRUE))) +
  geom_bar(stat = "identity", position = "fill", alpha = 0.7) +
  scale_x_discrete(limits = sort(unique(primary_links_excl_en$`Primary language`), decreasing = TRUE)) +
  scale_fill_brewer("Destination (Primary Link)", type = "qual", palette = "Paired") +
  scale_y_continuous("Proportion", labels = scales::percent_format()) +
  geom_text(data = keep_where(mutate(primary_links_excl_en, Y = cumsum(Prop) - Prop/2), Prop > 0.05),
            aes(x = `Primary language`, y = Y, label = sprintf("%.0f%%", 100*Prop)),
            color = "black") +
  coord_flip() +
  labs(title = "Portal visitors' destinations, excluding visitors who include English in their Accept-Language",
       x = "User's most preferred language") +
  ggthemes::theme_tufte(base_family = "Gill Sans") +
  theme(legend.position = "bottom")
ggsave("primary_link_ctr_by_language_excl-en.png", p, path = "figures", width = 8.5, height = 10)

ctr_by_lang <- sessions %>%
  keep_where(sessions$`Primary language` %in% top_languages(10)) %>%
  group_by(`Primary language`, `Includes English`) %>%
  summarize(`Overall Clickthrough Rate` = sum(clickthrough)/length(clickthrough),
            `Search Clickthrough Rate` = sum(section %in% "search")/length(section)) %>%
  tidyr::gather(Type, CTR, -c(1:2))

ctr_by_lang_diff <- ctr_by_lang %>%
  tidyr::spread(`Includes English`, CTR) %>%
  mutate(Diff = `TRUE`-`FALSE`,
         Difference = sprintf("%s%.1f%%", ifelse(Diff > 0, "+", ""), 100*Diff),
         Difference = ifelse(Difference == "NANA%", "", Difference))

p <- ctr_by_lang %>%
  mutate(`Includes English` = as.numeric(`Includes English`)) %>%
  ggplot(data = ., aes(x = `Includes English`, y = CTR, color = `Primary language`)) +
  geom_line(size = 1.1) + geom_point() +
  facet_wrap(~Type) +
  geom_text(data = ctr_by_lang_diff, aes(x = 1.25, y = `TRUE`, color = `Primary language`, label = Difference)) +
  scale_y_continuous("Clickthrough Rate", labels = scales::percent_format(), breaks = seq(0, 0.8, 0.1)) +
  scale_x_continuous("Accept-Language", breaks = 0:1, limits = c(-0.5, 1.5),
                     labels = c("Does NOT include English", "Includes English")) +
  # scale_color_brewer(type = "qual", palette = "Set3") +
  labs(title = "Clickthrough Rates by Top 10 Primary Accept-Languages") +
  ggthemes::theme_tufte(base_family = "Gill Sans") +
  theme(panel.grid = element_line(color = "gray20", size = 1, linetype = "solid"))
ggsave("ctr_diff_by_en.png", p, path = "figures", width = 10, height = 11)

sessions %>%
  keep_where(sessions$`Primary language` %in% top_languages(10)) %>%
  keep_where(`Primary language` != "English") %>%
  group_by(`Primary language`, `Includes English`) %>%
  summarize(Overall = sum(clickthrough)/length(clickthrough),
            Search = sum(section %in% "search")/length(section),
            `Primary Links` = sum(section %in% "primary links")/length(section)) %>%
  tidyr::gather(Type, CTR, -c(1:2)) %>%
  tidyr::spread(`Includes English`, CTR) %>%
  mutate(`Difference between en-acceptance` = `TRUE`-`FALSE`,
         `Relative difference` = (`TRUE`-`FALSE`)/pmax(`TRUE`, `FALSE`),
         Change = ifelse(`Difference between en-acceptance` > 0,
                         "*HIGHER* CTR when Accept-Language includes English",
                         "*LOWER* CTR when Accept-Language includes English")) %>%
  ggplot(data = ., aes(x = `Primary language`, y = `Difference between en-acceptance`, fill = Change)) +
  geom_bar(stat = "identity", position = "identity") +
  facet_wrap(~Type, scales = "free_x") +
  scale_fill_manual(values = rev(RColorBrewer::brewer.pal(3, "Set1")[1:2])) +
  scale_y_continuous("Clickthrough Rate difference", labels = scales::percent_format()) +
  geom_text(aes(label = sprintf("%.0f%%", 100 * `Difference between en-acceptance`),
                y = pmax(`Difference between en-acceptance`, 0) + 0.02), color = "black") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Clickthrough rate difference between users who included English in Accept-Language and users who didn't") +
  theme(text = element_text(family = "Gill Sans")) +
  theme(legend.position = "bottom")
