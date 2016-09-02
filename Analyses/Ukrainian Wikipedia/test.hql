USE wmf;

SELECT uri_path, COUNT(1) AS pageviews
FROM webrequest
WHERE
  webrequest_source IN('text', 'mobile')
  AND year = 2016 AND month = 8 AND day = 31 AND hour = 12
  AND is_pageview = true
  AND normalized_host.project_class = 'wikipedia'
  AND normalized_host.project = 'uk'
GROUP BY uri_path
ORDER BY pageviews DESC
LIMIT 50;

SELECT
(uri_path = '/wiki/index.html' OR
   uri_path = '/wiki/Main_Page' OR
   uri_path = '/wiki/%D0%93%D0%BE%D0%BB%D0%BE%D0%B2%D0%BD%D0%B0_%D1%81%D1%82%D0%BE%D1%80%D1%96%D0%BD%D0%BA%D0%B0') AS main_page,
  COUNT(1) AS pageviews
FROM webrequest
WHERE
  webrequest_source IN('text', 'mobile')
  AND year = 2016 AND month = 8 AND day = 31 AND hour = 12
  AND is_pageview = true
  AND normalized_host.project_class = 'wikipedia'
  AND normalized_host.project = 'uk'
GROUP BY (uri_path = '/wiki/index.html' OR uri_path = '/wiki/Main_Page' OR uri_path = '/wiki/%D0%93%D0%BE%D0%BB%D0%BE%D0%B2%D0%BD%D0%B0_%D1%81%D1%82%D0%BE%D1%80%D1%96%D0%BD%D0%BA%D0%B0');

SELECT
  uri_path, pageview_info,
  COUNT(1) AS pageviews
FROM webrequest
WHERE
  webrequest_source IN('text', 'mobile')
  AND year = 2016 AND month = 8 AND day = 31 AND hour = 12
  AND is_pageview = true
  AND normalized_host.project_class = 'wikipedia'
  AND normalized_host.project = 'uk'
  AND (uri_path = '/wiki/index.html' OR uri_path = '/wiki/Main_Page' OR uri_path = '/wiki/%D0%93%D0%BE%D0%BB%D0%BE%D0%B2%D0%BD%D0%B0_%D1%81%D1%82%D0%BE%D1%80%D1%96%D0%BD%D0%BA%D0%B0')
GROUP BY uri_path, pageview_info
ORDER BY pageviews DESC
LIMIT 50;

SELECT
  (referer RLIKE('^https?://(www\\.)?wikipedia\\.org/*$')) AS from_wikipedia_portal,
  (INSTR(accept_language, 'uk') > 0) AS lang_pref_ukrainian,
  (INSTR(accept_language, 'ru') > 0) AS lang_pref_russian,
  COUNT(1) AS pageviews
FROM webrequest
WHERE
  webrequest_source = 'text'
  AND year = 2016 AND month = 8 AND day = 31 AND hour = 12
  AND is_pageview = true
  AND normalized_host.project_class = 'wikipedia'
  AND normalized_host.project = 'uk'
  AND pageview_info['page_title'] IN('-', 'Main_Page', 'Головна_сторінка', 'Заглавная_страница', 'Baş_Saife')
  AND access_method IN('desktop', 'mobile web')
GROUP BY
  (referer RLIKE('^https?://(www\\.)?wikipedia\\.org/*$')),
  (INSTR(accept_language, 'uk') > 0),
  (INSTR(accept_language, 'ru') > 0);
