# Rise of Wikipedia.org Portal Pageviews

This codebase contains the code used to investigate a rise in pageviews to wikipedia.org, as noted in [T143045](https://phabricator.wikimedia.org/T143045):

![Rise in pageviews as seen on Portal dashboard.](figures/dash_pageviews.png)

## Part 1

In this part of the analysis, we suspected that not filtering out web requests by status code or agent type might responsible for the rise. First we looked at traffic broken down by agent type ("spider" or "user") and found that known bots account for only a tiny portion of the overall traffic.

![Unfiltered pageviews by agent type.](figures/webrequests_by_agent.png)

Then we looked at traffic brown down by [HTTP status code](https://en.wikipedia.org/wiki/List_of_HTTP_status_codes) and agent type, to see if maybe we're really over-counting pageviews by including web requests with HTTP status codes that should not be included per [our official "pageview" specification](https://meta.wikimedia.org/wiki/Research:Page_view):

![Pageviews by HTTP status code and agent type.](figures/webrequests_by_status-agent.png)

When we saw the difference between the <span style = "color: #E41A1C;">original pageviews</span> and <span style = "color: #377EB8;">recounted pageviews</span>, we made <span style = "color: #4DAF4A;">bootstrap simulations</span> of the pageviews given the observed differences:

![Recounts of pageviews.](figures/recounted.png)

This filtering will be added to [our pageview-counting script](https://github.com/wikimedia/wikimedia-discovery-golden/blob/master/portal/pageviews.R) in [wikimedia/discovery/golden](https://github.com/wikimedia/wikimedia-discovery-golden) in the nearest future and the pageviews recounted from the earliest available date (we keep about 60 days of webrequest data).

## Part 2

But that still does not explain why we are seeing nearly double the pageviews as we were seeing before June. Unfortunately, pre-June webrequest data does not exist anymore, but at least now we can narrow our investigation to webrequests with HTTP status codes 200 & 304 and agent_type == "user", since those pageviews retain the pattern we were interested in investigating. For this part, we focus on 17 August 2016 because we are investigating 18.43M HTTP requests from that day which satisfy the conditions of a "pageview".

We dug into the pageviews and found a few interesting features before hitting a roadblock. The features are:

- There are over 50 IP addresses that are responsible for 15K-44K Wikipedia.org Portal pageviews a day.
- They are responsible for 2%-4% of overall pageviews.
- 99.58% of the IP addresses are responsible for less than 100 pageviews a day each, with the remaining 0.42% of IP addresses having more than 100 pageviews a day each, up to 44K PVs/day:

![Cumulative distribution of IPs by number of Portal pageviews.](figures/distribution.png)

Unfortunately, there is not much else to do here. We are currently unable to properly figure out what exactly happened in June because due to the 60-day window, we are unable to analyze pre-rise webrequests as they have been deleted. For example, we don't know if a bunch of (potential) bots suddenly started making 2x requests that they were before.

This concludes the work on [T143045](https://phabricator.wikimedia.org/T143045).
