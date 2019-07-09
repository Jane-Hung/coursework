####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+


#' ---
#' title: "Data Science University MLM1805 Final Project"
#' author: "Jane Hung"
#' date: "August 17, 2018"
#' output: html_document
#' ---

#+ global_options, include = FALSE
require(knitr)
opts_chunk$set(fig.width=12, fig.asp = 0.618, fig.align = "center",
               warning = FALSE, message = FALSE)

##' ## CoNVO: Context, Need, Value, Outcome
##' Although we have multiple ways to understand and group our providers (i.e. 
##' through line of business, size, etc.), we still do not have a full understanding 
##' of how to segment our providers. In an environment where our providers feel
##' like we do not know who they are and how to best serve their needs, it is 
##' imperative to gain more intuition into the true provider personas. As a 
##' healthcare insurance company, creating these provider segments allows us to
##' understand a provider group's common concerns, background information, etc. and
##' grants us the ability to interact with individual providers with unique 
##' insights into their issues. This analysis directly adds value to the company
##' because we are then able to positively influence Net Promoter Scores (NPS) 
##' and develop a trusting relationship between us and our provider partners. As
##' an outcome, this analysis could be used in call centers so our agents can 
##' tailor their conversation to the provider persona. Furthermore, this analysis
##' could be used within our Advocate Network to help facilitate stronger 
##' communication and empathy.
##' 
##' ## Question
##' What provider personas can be formed through unsupervised clustering methods?  
##' 
##' ## Hypothesis
##' Based on the data and completing exploratory data analysis, below are expected
##' features that may determine provider clusters.  
##' * Providers accepting Medicare assignments  
##' * Providers using electronic health records  
##' * Providers with a secondary specialty  
##' * Providers with a hospital affiliation  
##' * Providers in a hospital network  
##'   
##' ## Data Lineage
##' For this analysis, open source provider data was used from the Centers for 
##' Medicare & Medicaid Services. Please vist [here](https://data.medicare.gov/Physician-Compare/Physician-Compare-National-Downloadable-File/mj5m-pzi6) for the most up-to-date source. This data
##' was used because it has robust documentation, available SMEs, and current 
##' data refreshes. Furthermore, the data was fairly clean and was in a format 
##' that was easy to complete feature engineering. The total file contains 2.67
##' million rows; however, due to issues with high performance computing using
##' R on a local machine, I opted to compute a simple random sample to get 5% of
##' the total data, which amounted to ~133 thousand observations. This dataset
##' contains demographic and Medicare quality program participation information 
##' for individual eligible professionals.  
##'   
##' ## Cohort Definition  
##' Within this analysis, I opted to include all providers taken from the SRS of 
##' 5% of the total CMS dataset because these were all active and eligible 
##' providers. Furthermore, every provider in this dataset was represented as a 
##' single observation, and only individual providers (not organizations) were 
##' represented as a row. In addition, one additional constraint to this dataset
##' is that it was taken from the CMS website, which indicates that these providers
##' have had some affiliation with Medicare and Medicaid. Therefore, providers 
##' not in our cohort are those that *do not* have any past affiliation with CMS.
##' 

##' ## Initialize environment  
#+ init, include = FALSE
set.seed(180)
# require(RSocrata)
# require(simpleCache)
require(tidyverse)
require(plyr)
require(tidygraph)
require(ggraph)
require(visNetwork)
require(igraph)
require(zoo)
require(rlist)
require(rccdates)
require(psych)
require(MASS)
require(rcompanion)
require(onehot)
require(rpart)
require(sparkline)
require(FactoMineR)
require(factoextra)
require(stats)
require(cluster)
require(dendextend)
require(fastcluster)
require(mclust)
require(fpc)
require(ggfortify)
require(ggplot2)
theme_set(theme_gray())
# setwd(file.path("C:","Users","jhung10","Documents",
#                 "Data Science Training"))


##' ## Import and cache data  

# Eventually I would like to directly source the information from the CMS website and cache it
# setCacheDir(tempdir())
# 
# simpleCache('provider.data', {provider <- read.socrata("https://data.medicare.gov/resource/c8qv-268j.csv")})

provider <- read_csv('Physician_Compare_National_Downloadable_File.csv',
                     col_types = paste(rep('c',41), sep = '',collapse = ''))


##' ## Check data quality and data types  

#' Sample data to make data easier to work with initially. Get 5% of total dataset.
provider <- sample_n(provider, 
                     size = ceiling(.05*nrow(provider)),
                     replace = TRUE)

#' Make valid names for columns, i.e. remove spaces in column names
names(provider) <- make.names(names(provider))

#' Change to numerical variables
# provider$NPI <- as.factor(provider$NPI)

provider$years.after.grad <- as.year(Sys.Date()) - as.year(provider$Graduation.year)
provider$Number.of.Group.Practice.members <- as.numeric(provider$Number.of.Group.Practice.members)

#' Check the provider dataframe
str(provider)

#' Number of observations in this dataset: **`r nrow(provider)`**  
#' Number of features in this dataset: **`r ncol(provider)`**  

#' Convert multiple character columns to factors
for (i in names(provider)) {
  if (class(provider[[i]]) == "character") {
    provider[[i]] <- factor(provider[[i]])
  }
}

##' ## Conduct exploratory data analysis  

#' Give top 10 counts of all columns and summary of numerical data
for (i in names(provider)) {
  cat("\n")
  if (class(provider[[i]]) == "numeric") {
    print(i)
    print(summary(provider[[i]]))
  } else {
    provider %>%
      group_by_(.dots = i) %>%
      dplyr::summarize(num_provider = n()) %>%
      arrange(desc(num_provider)) %>%
      head(n = 10) %>%
      print()
  }
}

#' There are many NA values. How many NA are in each columns? 
#' How many providers have a lot of NA values?

col.NA <- 
  provider %>%
  summarise_all(funs(
    signif(
      sum(is.na(.)) / n(), 
      digits = 3)))

col.NA <-
  as_tibble(cbind(column_names = names(col.NA), t(col.NA)))

names(col.NA)[2] <- c('Prop.NA')
col.NA$Prop.NA <- as.numeric(col.NA$Prop.NA)

col.NA <- 
  col.NA %>%
  arrange(desc(Prop.NA)) %>%
  print(n = Inf)

ggplot(col.NA, aes(x = reorder(column_names,Prop.NA), y = Prop.NA)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Proportion of NAs in each column")

#' There are a bunch of columns that have NA, but not all of them are important. For example, `Suffix` and `Secondary.specialty.4` are not important to have complete information. 
#' Doing some feature engineering to consolidate information, such as specialties and hospital affiliations.  
#'   
#' Creating the variable `has.secondary.specialty` to consolidate the secondary
#' specialty columns.
provider <- 
  provider %>%
  mutate(has.secondary.specialty = 
           factor(ifelse(is.na(All.secondary.specialties), 'N', 'Y')))

provider %>%
  group_by(has.secondary.specialty) %>%
  dplyr::summarise(n() / nrow(provider))

ggplot(provider,aes(x = has.secondary.specialty)) +
  geom_bar() +
  labs(title = "Frequency of providers with an additional specialty")

#' 85.6% of the providers in this table do not have a secondary specialty and 
#' 14.4% do. Can now remove those columns about Secondary Specialty. This was somewhat surprising to see.

col.remove = c("Secondary.specialty.1", "Secondary.specialty.2",
               "Secondary.specialty.3", "Secondary.specialty.4",
               "All.secondary.specialties")

# Add columns that we engineered above

col.remove <- list.append(col.remove, "Graduation.year")

cat("These columns are still being considered for analysis: \n", 
    paste(setdiff(names(provider),col.remove), collapse = ' \n '))

#' Consolidate hospital affiliation information into `has.hospital.affiliation`.

provider <-
  provider %>%
  mutate(has.hospital.affiliation = 
           factor(ifelse((is.na(Hospital.affiliation.CCN.1) &
                            is.na(Hospital.affiliation.CCN.2) &
                            is.na(Hospital.affiliation.CCN.3) &
                            is.na(Hospital.affiliation.CCN.4) &
                            is.na(Hospital.affiliation.CCN.5)), 'N','Y')))

provider %>%
  group_by(has.hospital.affiliation) %>%
  dplyr::summarise(n() / nrow(provider))

ggplot(provider,aes(x = has.hospital.affiliation)) +
  geom_bar() +
  labs(title = "Frequency of providers with hospital affiliation")

#' This was also surprising to see because I expected more providers to operate within their own primary practices.  

col.remove <- list.append(col.remove, c("Hospital.affiliation.CCN.1", 
                                        "Hospital.affiliation.CCN.2", 
                                        "Hospital.affiliation.CCN.3", 
                                        "Hospital.affiliation.CCN.4", 
                                        "Hospital.affiliation.CCN.5", 
                                        "Hospital.affiliation.LBN.1",
                                        "Hospital.affiliation.LBN.2", 
                                        "Hospital.affiliation.LBN.3",
                                        "Hospital.affiliation.LBN.4",
                                        "Hospital.affiliation.LBN.5"))

cat("These columns are still being considered for analysis: \n", 
    paste(setdiff(names(provider),col.remove), collapse = ' \n '))


#' Create frequency plots for the other variables if low number of unique values

# Gender
ggplot(provider, aes(x = Gender)) +
  geom_bar()  +
  labs(title = "Frequency of genders")

# Credential
ggplot(provider, aes(x = reorder(Credential,Credential,
                                 function(x)length(x)))) +
  geom_bar()  +
  labs(title = "Frequency of credentials") + 
  coord_flip()

#' Many providers have `NA` for the credentials, which is very strange. I would 
#' like to compare providers with no credential (population 1) with providers 
#' with credentials (population 2) to see if these populations differ at all.
# [TODO] Complete hypothesis testing

# Medical School Name
provider %>%
  group_by(Medical.school.name) %>%
  dplyr::summarise(freq = n()) %>%
  dplyr::arrange(desc(freq)) %>%
  top_n(n = 10,freq) %>%
  ggplot(aes(x = reorder(Medical.school.name,freq), y = freq)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  labs(title = "Top 10 medical school frequencies")

#' Due to the high number of unique values in `Medical.school.name`, this variable 
#' may need to be binned into a lower dimensional value set.

topthird.medical.school <-
  provider %>%
  group_by(Medical.school.name) %>%
  dplyr::summarise(freq = n()) %>%
  dplyr::arrange(desc(freq)) %>%
  top_n(n = round(.34 * length(unique(provider$Medical.school.name))),freq) %>%
  dplyr::select(Medical.school.name)

tailthird.medical.school <-
  provider %>%
  group_by(Medical.school.name) %>%
  dplyr::summarise(freq = n()) %>%
  dplyr::arrange(desc(freq)) %>%
  top_n(n = -round(.33 * length(unique(provider$Medical.school.name))),freq) %>%
  dplyr::select(Medical.school.name)


# [TODO] Repeat for the other variables with multiple factors

# Primary Specialty
provider %>%
  group_by(Primary.specialty) %>%
  dplyr::summarise(freq = n()) %>%
  dplyr::arrange(desc(freq)) %>%
  top_n(n = 10,freq) %>%
  ggplot(aes(x = reorder(Primary.specialty,freq),y = freq)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 primary specialties")

#' Binned the lowest third of `Primary.specialty` into a "Rare" bucket

tailthird.specialty <-
  provider %>%
  group_by(Primary.specialty) %>%
  dplyr::summarise(freq = n()) %>%
  dplyr::arrange(desc(freq)) %>%
  top_n(n = -round(.33 * length(unique(provider$Primary.specialty))),freq) %>%
  dplyr::select(Primary.specialty)

##'  

# Group Practice Members
provider %>%
  filter(!is.na(Number.of.Group.Practice.members)) %>%
  ggplot(aes(x = Number.of.Group.Practice.members)) +
  geom_histogram(binwidth = 50) +
  labs(title = "Histogram of group practice members") +
  geom_vline(aes(xintercept = mean(Number.of.Group.Practice.members)),
             colour = "red") +
  geom_text(aes(x=310, label="mean", y=300), colour="red")

#' Need to normalize this data. Lognormal was chosen to normalize this data over 
#' boxcox due to the computational simplicity and satisfactory results.  
#' Refer to this [document](http://rcompanion.org/handbook/I_12.html)
provider$Number.of.Group.Practice.members_log <- 
  log(provider$Number.of.Group.Practice.members)
plotNormalHistogram(provider$Number.of.Group.Practice.members_log)
qqnorm(provider$Number.of.Group.Practice.members_log)
qqline(provider$Number.of.Group.Practice.members_log, col = 'red')
col.remove <- list.append(col.remove, 'Number.of.Group.Practice.members')

#' Looking at the Q-Q Plot of log normalized `Number.of.Group.Practice.members`, you can easily see that between [-1,1] theoretical quantiles, the data fits a normal distribution.  

# State
provider %>%
  group_by(State) %>%
  dplyr::summarise(freq = n()) %>%
  dplyr::arrange(desc(freq)) %>%
  top_n(n = 10,freq) %>%
  ggplot(aes(x = reorder(State,freq),y = freq)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 States")

#' CA, TX, and NY round out the top 3 states, which makes sense because they are some of the largest states with densely populated cities.  

# Location

# Accepts Medicare
ggplot(provider,aes(x = Professional.accepts.Medicare.Assignment)) +
  geom_bar() +
  labs(title = "Frequency of providers accepting Medicare")

#' value of 'M' indicates a provider *maybe* accepts Medicare and value of 'Y' indicates a provider indeed accepts Medicare.  

# Reported Quality Measures
ggplot(provider,aes(x = Reported.Quality.Measures)) +
  geom_bar() +
  labs(title = "Frequency of providers reporting quality measures")

#' Unsurprised that this is the distribution of providers reporting quality measures because some NA values are expected here.  

# Used electronic health records
ggplot(provider,aes(x = Used.electronic.health.records)) +
  geom_bar() +
  labs(title = "Frequency of providers using electronic health records")

#' Did not expect so few providers to be using electronic health records! Since the only two values are 'Y' and NA, then the NA values could perhaps be 'Y' or some other value. However, instead of imputing a value, these were converted to 'No Answer' because there are no 'N' values to understand the distribution of actual values.  

# Committed to heart health
ggplot(provider,aes(
  x = Committed.to.heart.health.through.the.Million.Hearts..initiative.)) +
  geom_bar() +
  labs(title = "Frequency of providers committed to heart health")

#' Surprised to see the high ratio of NA:'Y' values for this variable. Perhaps not many members utilize this offering  

# years after grad
provider %>%
  filter(!is.na(years.after.grad)) %>%
  ggplot(aes(x = years.after.grad)) +
  geom_histogram(binwidth = 2) +
  labs(title = "Histogram of years after graduating") +
  geom_vline(aes(xintercept = mean(years.after.grad)),colour = "red") +
  geom_text(aes(x = 23, label = "mean", y = 500), colour = "red")

#' Do we need to normalize this data? After checking, possibly not.
plotNormalHistogram(provider$years.after.grad)
qqnorm(provider$years.after.grad)
qqline(provider$years.after.grad, col = "red")

##' ## Hypothesis testing   
##'   
##' In the above, we found that there were many providers with `NA` credentials. We'd like to know if there is a relationship between having `NA` credentials and various other dependent variables. Only some variables were checked to understand if PCA would help reduce correlation between variables.  

# Create vector of yes/no NA credentials
provider <- 
  provider %>%
  mutate(credential.isNA = 
           factor(ifelse(is.na(Credential), 'Y', 'N')))

# Gender
chisq.test(table(provider$Gender,provider$credential.isNA))

#' With a p-value < 0.05, reject the null hypothesis that states there is no 
#' relationship between these variables.

# Medical school name
# fisher.test(table(provider$Medical.school.name,provider$credential.isNA))
# [TODO] Figure out way to bin medical schools

# Graduation year
# fisher.test(table(provider$Graduation.year,provider$credential.isNA))

# Primary specialty

# Group practice members

# State

# Medicare
chisq.test(provider$Professional.accepts.Medicare.Assignment,
           provider$credential.isNA)

#' With a p-value > 0.05, do not reject the null hypothesis that states there is
#'  no relationship between these variables.

# Quality Measures
provider$Reported.Quality.Measures <- 
  factor(provider$Reported.Quality.Measures,
         levels = levels(addNA(provider$Reported.Quality.Measures)), 
         labels = c(levels(provider$Reported.Quality.Measures), "No Answer"),
         exclude=NULL)

chisq.test(provider$Reported.Quality.Measures,provider$credential.isNA)

#' With a p-value < 0.05, reject the null hypothesis that states there is no 
#' relationship between these variables.

prop.table(xtabs(~Reported.Quality.Measures + credential.isNA, data = provider))
#' Higher proportion of providers have a credential and report quality measures.

# electronic Health records
provider$Used.electronic.health.records <- factor(provider$Used.electronic.health.records, levels = levels(addNA(provider$Used.electronic.health.records)), labels = c(levels(provider$Used.electronic.health.records), "No Answer"),exclude=NULL)

chisq.test(provider$Used.electronic.health.records,provider$credential.isNA)

#' With a p-value < 0.05, reject the null hypothesis that states there is no relationship between these variables.

prop.table(xtabs(~Used.electronic.health.records + credential.isNA, data = provider))

# heart health
provider$Committed.to.heart.health.through.the.Million.Hearts..initiative. <- factor(provider$Committed.to.heart.health.through.the.Million.Hearts..initiative., levels = levels(addNA(provider$Committed.to.heart.health.through.the.Million.Hearts..initiative.)), labels = c(levels(provider$Committed.to.heart.health.through.the.Million.Hearts..initiative.), "No Answer"),exclude=NULL)

chisq.test(provider$Committed.to.heart.health.through.the.Million.Hearts..initiative.,provider$credential.isNA)

#' With a p-value < 0.05, reject the null hypothesis that states there is no relationship between these variables.

prop.table(xtabs(~Committed.to.heart.health.through.the.Million.Hearts..initiative. + credential.isNA, data = provider))

# secondary specialty
chisq.test(provider$has.secondary.specialty,provider$credential.isNA)

#' With a p-value < 0.05, reject the null hypothesis that states there is no relationship between these variables.

prop.table(xtabs(~has.secondary.specialty + credential.isNA, data = provider))

# hospital affiliation
chisq.test(provider$has.hospital.affiliation,provider$credential.isNA)

prop.table(xtabs(~has.hospital.affiliation + credential.isNA, data = provider))

# remove column `credential.isNA` if moving on
col.remove <- list.append(col.remove,'credential.isNA')

##' ## Graph Cluster Analysis    
##' 
##' I decided to use graph cluster analysis because I was interested in seeing
##' how hospital networks helped contribute to provider clustering. Below are the
##' pros and cons of using graph cluster analysis for this purpose:  
##'   
##' Pros:  
##' * Can analyze many-to-many relationships  
##' 
##' Cons:  
##' * Difficult to create the graph  
##' * Difficult to analyze
##'   
##' Refer to [this](https://www.jessesadler.com/post/network-analysis-with-r/)  
##'   
##' For the graph cluster analysis portion, I opted to use only the first two columns of hospital associations because building an adjacency matrix was a bit more complicated than creating node and edge lists. However, using an adjacency matrix may have made this a more interesting and robust analysis.  
##' 
#' Create node list
hosp1 <- provider %>%
  distinct(Hospital.affiliation.LBN.1) %>%
  dplyr::rename(label = Hospital.affiliation.LBN.1)

hosp2 <- provider %>%
  distinct(Hospital.affiliation.LBN.2) %>%
  dplyr::rename(label = Hospital.affiliation.LBN.2)

hosp3 <- provider %>%
  distinct(Hospital.affiliation.LBN.3) %>%
  dplyr::rename(label = Hospital.affiliation.LBN.3)

hosp4 <- provider %>%
  distinct(Hospital.affiliation.LBN.4) %>%
  dplyr::rename(label = Hospital.affiliation.LBN.4)

nodes <- 
  join_all(list(hosp1,hosp2,hosp3,hosp4), by = "label", type = "full") %>%
  rowid_to_column("id")

#+ echo = FALSE,results = "asis"
kable(head(nodes, n = 10))

#' Create edge list
# edges <- provider %>%
#   group_by_at(vars(Hospital.affiliation.LBN.1, Hospital.affiliation.LBN.2,
#                    Hospital.affiliation.LBN.3, Hospital.affiliation.LBN.4)) %>%
#   dplyr::summarise(weight = n()) %>%
#   ungroup()

edges <- provider %>%
  group_by_at(vars(Hospital.affiliation.LBN.1, Hospital.affiliation.LBN.2)) %>%
  dplyr::summarise(weight = n()) %>%
  ungroup()

edges$Hospital.affiliation.LBN.1 <- mapvalues(edges$Hospital.affiliation.LBN.1,
                                              from = nodes$label,
                                              to = nodes$id,
                                              warn_missing = FALSE)
edges$Hospital.affiliation.LBN.2 <- mapvalues(edges$Hospital.affiliation.LBN.2,
                                              from = nodes$label,
                                              to = nodes$id,
                                              warn_missing = FALSE)
# edges$Hospital.affiliation.LBN.3 <- mapvalues(edges$Hospital.affiliation.LBN.3,
#                                               from = nodes$label,
#                                               to = nodes$id)
# edges$Hospital.affiliation.LBN.4 <- mapvalues(edges$Hospital.affiliation.LBN.4,
#                                               from = nodes$label,
#                                               to = nodes$id)

#' Remove NA values from edge list.
edges <-
  edges %>%
  filter_all(all_vars(!is.na(.)))


edges$weight <- as.numeric(edges$weight)

#+ echo = FALSE,results = "asis"
kable(head(edges,n = 10))

#' Remove values that have very small weights <10% of max weight.
edges <-
  edges %>%
  filter(weight >= ceiling(max(edges$weight) * .10))
nrow(edges)

#' Remove the corresponding nodes that will not be used.
# rm.nodes <- union(union(union(edges$Hospital.affiliation.LBN.1,edges$Hospital.affiliation.LBN.2),edges$Hospital.affiliation.LBN.3), edges$Hospital.affiliation.LBN.4)

rm.nodes <- union(edges$Hospital.affiliation.LBN.1,edges$Hospital.affiliation.LBN.2)

#' Number of nodes to keep: `r length(rm.nodes)`.  

#' Filter nodes list to keep nodes found in the first two hospital affiliation 
#' columns.  
nodes <-
  nodes %>%
  filter(id %in% rm.nodes)

#' Visualize network
visNetwork(nodes,
           setNames(edges,c('from','to', 'value')), 
           height = "700px", width = "100%") %>%
  visOptions(highlightNearest = TRUE, 
             nodesIdSelection = list(style = 'width: 100%; height: 26px;
                                     background: #f8f8f8;
                                     color: darkblue;
                                     border:none;
                                     outline:none;')) %>%
  visPhysics(stabilization = FALSE) %>%
  visLayout(randomSeed = 100)

#' Create network object for analysis purposes  
hosp_tidy <- tbl_graph(nodes = nodes, edges = setNames(edges,c('from','to', 'value')), directed = FALSE)
hosp_tidy

#' Fraction of edges present relative to total possible edges
edge_density(
  graph = hosp_tidy, 
  loops = F
) 

#' Fraction of triangles (completely connected 3 nodes) / all triangles
transitivity(
  graph = hosp_tidy, 
  type = 'global' # 'local'
) 
#' With a transitivity = 1, the network contains all possible edges. In this case, not all edges are present between hospitals.  

#' Find cliques and give clique sizes
hist(
  sapply(
    cliques(hosp_tidy), 
    length
  )
) # clique sizes

#' Cliques are described as a set of nodes where all possible connections between nodes exist, which may indicate a stronger version of community. [Source](http://compbio.ucsd.edu/communities-and-cliques/).  
#' As shown here, we have a low number of cliques that form triangles (three node clique). As such, it may be better to look for network structures that are weaker to understand hospital associations.  
#'   
#' Cliques with max number of nodes
largest_cliques(hosp_tidy) 

#' **Cluster using various method**  
#'   
#' The different clustering methods will be evaluated according to modularity and variation of information (VI). VI measures the amount of information lost and gained in changing from one clustering method to another method. In this evaluation, low VI indicates that the clusterings are fairly similar. Modularity measures how dense the connections are between nodes within modules. It looks to see if the number of edges in a cluster is comparable to the number of edges in a cluster found in a random network.  
#'   
#' Refer to this [article](https://www.sciencedirect.com/science/article/pii/S0047259X06002016) 
#' for info  on VI.  
#' Refer to this [article](https://en.wikipedia.org/wiki/Modularity_(networks)) for information on modularity.  
#'   
#' **Get the leading Eigen value clusters**  
#' According to this [document](http://igraph.org/r/doc/cluster_leading_eigen.html), leading eigenvector community structure is detected by finding "densely connected subgraphs by calculating the leading non-negative eigenvector of the modularity matrix of the graph."
cls_eigen <- 
  cluster_leading_eigen(hosp_tidy)

table(
  membership(cls_eigen)
)

#' Modularity of Leading Eigen community finding algorithm is `r modularity(cls_eigen)`.

hist(sizes(cls_eigen))

#' **Get the Louvain clusters**  
#' Using the Louvain function to find community structure implements both modularity optimization and a hierarchical approach. [Source](http://igraph.org/r/doc/cluster_louvain.html)
cls_louvain <- 
  cluster_louvain(hosp_tidy)

table(
  membership(cls_louvain)
)

#' Modularity of Louvain community finding algorithm is `r modularity(cls_louvain)`.

hist(sizes(cls_louvain))

#' Example of one of the largest communities in the hospital network
plot(induced_subgraph(hosp_tidy,cls_louvain[[184]]))

#' **Get the Walktrap clusters**  
#' Community structure via short random walks is built on the idea that "short random walks tend to stay in the same community." [Source](http://igraph.org/r/doc/cluster_walktrap.html)
cls_wt <- 
  cluster_walktrap(
    hosp_tidy,
    steps = 4
  )

table(
  membership(cls_wt)
)

#' Modularity of Walktrap community finding algorithm is `r modularity(cls_wt)`.

hist(sizes(cls_wt))

#' Modularity between the three clustering methods are the same and is fairly high, which indicates that the network has "dense connections between nodes within modules but sparse connections between nodes in different modules." [Source](https://en.wikipedia.org/wiki/Modularity_(networks))  
#'   
#' In regard to hospital networks, this may be an accurate analysis because providers in a hospital network may be limited to a certain distance range. Therefore, providers within a certain area may be associated with the hospitals in that region, and distance is the deciding feature.  

# compare(
#   cls_louvain, 
#   cls_wt, 
#   method = 'vi'
# )

# [TODO] Need to change edge list to have 2 columns with from and to.
# [UPDATE] Create adjacency matrix to encompass all of the hospital affiliations columns

# [TODO] Need to figure out a way to remove the NA
# [UPDATE] Removed NA from two column "from" "to" column
# [DONE] Removed values that had NA from edge list

# [TODO] Need to add weights.
# [UPDATE] Changed column name from "weights" to "value" which is what visNetwork wants
# [DONE]

# [TODO] How to handle providers that do not have a hospital affiliation? Maybe just calculate ratio of providers without hospital affiliation and those with hospital affiliation?
# [DONE] Created new feature `has.hospital.affiliation`



##' ## Preprocess data    
##'   

# Decide what to do with all NA
provider$Credential <- 
  factor(provider$Credential, 
         levels = levels(addNA(provider$Credential)), 
         labels = c(levels(provider$Credential), "No Answer"),
         exclude = NULL)

provider$Medical.school.name <- 
  factor(provider$Medical.school.name, 
         levels = levels(addNA(provider$Medical.school.name)), 
         labels = c(levels(provider$Medical.school.name), "No Answer"),
         exclude = NULL)

#' Impute NA values in numerical columns using mean since the variables have
#' been transformed to a normal distribution.

provider[is.na(provider[,"years.after.grad"]), "years.after.grad"] <-
  round(mean(provider$years.after.grad, na.rm = TRUE))
provider[is.na(provider[,"Number.of.Group.Practice.members_log"]), 
         "Number.of.Group.Practice.members_log"] <-
  round(mean(provider$Number.of.Group.Practice.members_log, na.rm = TRUE))


#' Standardize and center variables from 0-1

scale.center <- function(data) {
  output = (data - min(data)) / (max(data) - min(data))
  return(output)
}

provider$years.after.grad <- scale.center(provider$years.after.grad)

provider$Number.of.Group.Practice.members_log <- scale.center(provider$Number.of.Group.Practice.members_log)

#' Bin categorical variables that have too many factors

provider$Medical.school.name <-
  as.factor(case_when(provider$Medical.school.name %in% topthird.medical.school[[1]] ~ "Top",
                      provider$Medical.school.name %in% tailthird.medical.school[[1]] ~ "Bottom",
                      TRUE ~ "Middle"))


provider %>%
  ggplot(aes(x = Medical.school.name)) + 
  geom_bar() +
  coord_flip()

levels(provider$Primary.specialty)[
  which(levels(provider$Primary.specialty) %in% tailthird.specialty[[1]])] <- 
  "RARE SPECIALTY"

provider %>%
  ggplot(aes(x = Primary.specialty)) + 
  geom_bar() +
  coord_flip()

#' Decide which columns to analyze
identity.col <- c("NPI","PAC.ID","Professional.Enrollment.ID","Last.Name","First.Name","Middle.Name",
                  "Suffix","Organization.legal.name","Group.Practice.PAC.ID","Line.1.Street.Address",
                  "Line.2.Street.Address", "Marker.of.address.line.2.suppression","City","Zip.Code",
                  "Phone.Number")

cat("Number of columns left for analysis: ", 
    ncol(provider) - (length(identity.col) + length(col.remove)))

processed.provider <- provider[ , !(names(provider) %in% 
                                      list.append(identity.col,col.remove))]

summary(processed.provider)

#' Complete one-hot encoding for the categorical variables

encoder <- onehot(processed.provider,max_levels = 350)

encode.processed.provider <- predict(encoder,processed.provider)

str(encode.processed.provider)

##' ## Principal Component Analysis    
##'   
##' Principal component analysis was completed because there was evidence of endogeneity from the hypothesis testing results. Many subsequent machine learning algorithms require independent variables.  
pca <- PCA(encode.processed.provider,graph = FALSE)
#+ echo = TRUE,results = "asis"
kable(head(pca$eig, n = 20))
fviz_screeplot(pca, ncp = 150) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#' Graph of variables
fviz_pca_var(pca, col.var = 'contrib')
#' In this graph, you can understand variables that correlate positively and negatively. For example, some variables that correlate positively:  
#' * Male gender + MD credential  
#' * Reported quality measures + hospital affiliated  
#' * Female gender + nurse practictioner primary specialty  

#' Graph of individuals
fviz_pca_ind(pca, geom = 'point', col.ind = 'cos2')

#' Overall, dimensionality reduction using PCA was not helpful because each 
#' principal component was responsible for a minute amount of variance. As a
#' result, each variable contributed very little to each principal component.
#' Looking at the scree plot, the elbow is actually closer to 125 principal
#' components because each component explains only 3-1% of variance.
#' 
#' | component | eigenvalue | percentage of variance | cumulative percentage of variance |
#' |---------|--------|--------|--------|
#' |comp 125 | 6.440340e-01    |       4.293560e-01           |      95.47559
#' 
#' 125 component accounts for 95.5% of the variance in the data, so keeping 125
#' components reduces the number of predictors by 16.7%.


#' This is another function that calculates PCA. For whatever reason, I am only able to get 5 dimensions from the original PCA results instead of the 125 components that would be necessary. This was not used for further analysis but for a quick sanity check. In future iterations, it may be more helpful to go through this route instead.

pca.prcomp <- prcomp(encode.processed.provider, scale. = TRUE)
autoplot(pca.prcomp,encode.processed.provider, 
         loadings = TRUE,loadings.label = TRUE)

pca.provider <- pca.prcomp$x[,1:125]

##' ## K-Means    
##'   
##' K-Means was chosen as a clustering method because of its simplicity. Below are the pros and cons of using this method.  
##'   
##' Pros:  
##' * Fast to run  
##'   
##' Cons:  
##' * Only works well for spherical clusters  
##' * Difficult to ascertain the number of clusters  
##' * Difficult to work with outliers  
##' Refer to [this](https://www.quora.com/What-are-the-pros-and-cons-of-kmeans-vs-hierarchical-clustering) for more information  
##'   
#' Determine number of clusters
#+ kmeans_cluster, results = "hide"
wss <- (nrow(encode.processed.provider) - 1)*sum(apply(encode.processed.provider,2,var))
for (i in 2:40) wss[i] <- sum(kmeans(encode.processed.provider,
                                     centers = i,
                                     trace = TRUE)$withinss)

#+ kmeans_cluster_plot, results = "markup"
plot(1:40, wss, type = "b", xlab = "Number of Clusters",
     ylab = "Within groups sum of squares")

#' Refer to this [document](https://www.r-bloggers.com/pca-and-k-means-clustering-of-delta-aircraft/)  
#'   
#' For this scree plot, it is difficult to tell exactly where the elbow is since
#' there is a slow gradient leveling off at 30 clusters. By visual inspection 
#' and acknowledging Occam's razor, 9 clusters was chosen as the elbow and model
#' parameter.

k <- kmeans(x = pca$ind$coord, 9, , nstart=25, iter.max=1000)
plot(pca$ind$coord, col = k$clust,pch = 16)

#' As you can see, there is much overlap in clusters, which suggests that a Gaussian Mixture Model might be helpful because it allows for mixed cluster membership. Furthermore, this plot is only demonstrating principal component 1 and 2, which account for a low amount of variation in the data. Therefore, this representation must be taken with a grain of salt. In the future, it would be prudent to complete pairplots to analyze clusters across multiple dimensions.

# take 2
# k.prcomp <- kmeans(x = pca.provider, 9 , nstart = 25, iter.max = 1000)
# plot(pca.provider, col = k.prcomp$clust,pch=16)

#' How can we decipher these clusters?  
#' For interpretation purposes, the most frequent value in each categorical variable and the mean value in each numerical variable is reported.  
cluster.processed.provider <- cbind(processed.provider,factor(k$cluster))

for (cluster in sort(unique(k$cluster))) {
  temp <- cluster.processed.provider %>% 
    filter(k$cluster == cluster)
  cat("Summarizing cluster: ", cluster)
  cat("\n")
  # summary.kmeans$`factor(k$cluster)`[iter] <- cluster
  for (col in names(temp)) {
    if (class(temp[[col]]) != "numerical") {
      print(paste(col, names(which.max(table(temp[[col]]))), sep = ": "))
    }
    else {
      print(paste(col, mean(temp[[col]]), sep =": "))
    }
  }
  cat("\n")
}

##' ## K-Medoid (PAM)    
##'   
##' K-Medoid was chosen as the next choice for clustering because K-Means worked relatively well. K-Medoid is implemented through the R function, partitioning around medoids (PAM), which "minimizes a sum of dissimilarities instead of a sum of squared euclidean distance." [Source](https://www.rdocumentation.org/packages/cluster/versions/2.0.7-1/topics/pam)  
##'   
##' PAM works with medoids (samples of the dataset that represents the group) while K-Means works with centroids (artificially created entities that represent the cluster). As such, PAM could be more representative of the actual dataset.  
##'   
##' In PAM, PCA is completed internally, so the encoded provider data was used instead of the PCA provider data.  Furthermore, this data was sampled to fit into the memory allocation needed and to speed up run time.

sample.encode.processed.provider <-
  sample_n(as.data.frame(encode.processed.provider),
           size = ceiling(.1*nrow(encode.processed.provider)),
           replace = TRUE)
pamx <- pam(x = sample.encode.processed.provider, 9)
# summary(pamx)

#' How do these clusters differ?  
#' To decipher, remove all columns from the medoid dataframe that are all 0's. Print out the medoids.  
pam.medoid <- as.data.frame(pamx$medoids)

pam.col.list = c()
for (col in names(pam.medoid)) {
  if (any(pam.medoid[[col]]) != 0) {
    print(col)
    pam.col.list <- list.append(pam.col.list,col)
  }
}

pam.medoid[pam.col.list]

##' ## Agglomerative Hierarchical Clustering    
##'   
##' Agglomerative hierarchical clustering was initially chosen because it is more informative than the flat unstructured clusters from K-Means and for its ease of implementation. However, some cons I experienced were that hierarchical clustering was not suitable for large datasets and since points assigned to a cluster cannot be moved around, order of the data and the initial seeds have a strong impact. [Source](http://stp.lingfil.uu.se/~santinim/ml/2016/Lect_10/10c_UnsupervisedMethods.pdf)
# d <- dist(pca$ind$coord, method = 'euclidean')
# hc1 <- hclust(d, method = 'complete')
# plot(hc1,cex=0.6,hang=-1)
# 
# hc2 <- agnes(pca$ind$coord, method = 'complete')

#' Using the traditional hclust function with a distance matrix was unsuccessful due to the high computational complexity and memory allocation.  

# Trying HCPC function which completes hierarchical clustering on Principle Components (NCPC)

# hc <- HCPC(pca.provider, nb.clust = -1)  

##' In future work, it may be helpful to sample a smaller proportion of the data initially so that AHC may be used in this analysis.  
##'   
##' ## Gaussian Mixture Model    
##'   
##' Gaussian Mixture Model is a parametric model that assumes that the data points are generated from Gaussian distributions.  
##' Refer to [this document.](http://scikit-learn.org/stable/modules/mixture.html) and [this](https://www.statmethods.net/advstats/cluster.html).  
fit <- Mclust(pca$ind$coord)
fviz_mclust(
  fit, 
  what = 'BIC', 
  palette = 'npg'
)
#' VVV is the best fit with 9 clusters
summary(fit, parameters = TRUE)

#' What is the uncertainty associated with the classification prediction?
summary(fit$uncertainty)
boxplot(fit$uncertainty, horizontal = TRUE)
#' There seem to be a high number of outliers, so there may be much skewness in the data.  

#' Interpret clusters
gmm.cluster.processed.provider <- cbind(processed.provider,factor(fit$classification))

for (cluster in sort(unique(fit$classification))) {
  temp <- gmm.cluster.processed.provider %>% 
    filter(fit$classification == cluster)
  cat("Summarizing cluster: ", cluster)
  cat("\n")
  for (col in names(temp)) {
    if (class(temp[[col]]) != "numerical") {
      print(paste(col, names(which.max(table(temp[[col]]))), sep = ": "))
    }
    else {
      print(paste(col, mean(temp[[col]]), sep = ": "))
    }
  }
  cat("\n")
}

##' ## Model Comparison    
##'   
##' Since we have unlabeled data, the Calinski-Harabaz Index will be used to evaluate the models. The higher the metric, the more dense and well separated the clusters. [Source](http://scikit-learn.org/stable/modules/clustering.html#fowlkes-mallows-scores)  
##'   
#' | Network Analysis | K-Means | K-Medoid | Agglomerative Hierarchical Clustering | Gaussian Mixture Model |
#' |---------|---------|---------|---------|---------|
#' | NA |`r calinhara(encode.processed.provider, k$cluster,cn = max(k$cluster))` | `r calinhara(encode.processed.provider, pamx$clustering,cn = max(pamx$clustering))` | NA | `r calinhara(encode.processed.provider, fit$classification,cn = max(fit$classification))` |

##' ## Conclusion    
##'   
##' Using the model comparison results above, it is clear that K-Medoid (PAM) has the highest Calinski-Harabaz Index and demonstrates better clustering. However, this data is using `r ceiling(.1*nrow(encode.processed.provider))` observations whereas the other models are using `r nrow(encode.processed.provider)` observations, which skews the validity of this comparison. In the future, it may be helpful to have sampled an even smaller amount from the original population data from CMS.  
##'   
##' For this analysis, the Gaussian Mixture Model would be chosen as the best model because the characteristics of this model suits our purpose the most. Indeed, after looking through the PCA results and cluster plots, there is much overlap in clusters which assumes mixed assignment of clusters. Indeed, I suspect that there are too few characteristics/variables in our dataset that could help discern more definitive clusters.  
##'   
##' K-Means and K-Medoid could be viable options for a possible model. However, K-Medoid only took a small portion of the data, so this model would be difficult to scale and encompass more provider features and observations.  
##'   
##' The network analysis would be interesting to pursue further especially if the full adjacency matrix were used to create the network. Another piece of future work could include adding a binary variable, `included.in.hospital.network`, as a dataset feature.  
