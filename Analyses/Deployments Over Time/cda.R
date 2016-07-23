deployments <- readr::read_csv("data/portal_deployments.csv")
data_range <- seq(as.Date("2015-11-11"), as.Date("2016-07-20"), "day")
timeline <- as.data.frame(apply(deployments, 1, function(deployment) {
  return(as.numeric(data_range >= deployment['Date']))
}))
names(timeline) <- deployments$Deployment
