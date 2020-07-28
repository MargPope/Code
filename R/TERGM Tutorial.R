###########################################################################################################
######################################## TERGM EXAMPLE SCRIPT #############################################
###########################################################################################################

##### LOAD ALL PACKAGES - MAKE SURE THESE ARE INSTALLED FIRST #####
library(statnet)
library(sna)
library(network)
library(xergm)
library(ergm)

##### SET WORKING DIRECTORY - LOCATION OF DATA FILES AND/OR PLACE TO SAVE OUTPUT #####
## Note: I found it easier to put all datasets and matrices in different files and 
## switch working directories when loading different files. Helps with organizaiton.
setwd("~/Desktop/Thesis Work/Datasets/Source Destination Matrices")

##### LOAD AND FORMAT DEPENDENT VARIABLE (BINARY MATRICES) #####
## Note: ergm and tergm only take binary dependent networks.
## EACH INDIVIDUAL YEAR MUST BE SAVED AS A SEPARATE CSV
SD11<-read.csv("SD 2011.csv", header=TRUE, row.names=1)
SD12<-read.csv("SD 2012.csv", header=TRUE, row.names=1)
SD13<-read.csv("SD 2013.csv", header=TRUE, row.names=1)

## CONVERT CSVs TO MATRICES
SD11M<-as.matrix(SD11)
SD12M<-as.matrix(SD12)
SD13M<-as.matrix(SD13)

## ENSURE MATRICES ARE OF CLASS "NUMERIC" - REQUIRED FOR ANALYSIS
class(SD11M)<-"numeric"
class(SD12M)<-"numeric"
class(SD13M)<-"numeric"

## REPLACE "NA" WITH SOME ALTERNATIVE VALUE
## Note: btergm cannot handle NAs. Usually it makes most sense to simply treat 
## missing values as missing edges for the purposes of converting to networks. 
## Other options, such as replacing with the mode or average can be taken by
## adjusting the "method" command in the handleMissings function. Search for
## handleMissings in the help pane to see all possible arguments. 

## Note: This is only important if there are NAs in the matrix. In the case of 
## the SD matrices, there are no NAs as we've already removed countries that do
## not appear in the TIP reports. Should you wish to use a different dependent 
## variable, btergm can handle "structural zeros," or nodes that are not present
## at certain time steps but are present at others. See the xergm documentation 
## for more information on structural zeros. 
SD11M<-handleMissings(SD11M, na=NA, method="zero")
SD12M<-handleMissings(SD12M, na=NA, method="zero")
SD13M<-handleMissings(SD13M, na=NA, method="zero")

## CONVERT MATRICES TO NETWORKS 
## Note: adjust the arguments of the as.network function as appropriate. The most
## typical adjustment you might need to make will likely be either setting the 
## "directed" argument to "FALSE" rather than "TRUE" if the situation demands.
SD11N<-as.network(SD11M, directed=TRUE, loops=FALSE)
SD12N<-as.network(SD12M, directed=TRUE, loops=FALSE)
SD13N<-as.network(SD13M, directed=TRUE, loops=FALSE)

##### LOAD AND FORMAT INDEPENDENT VARIABLES #####
## Note: independent variables can be of any form (continuous, binary, etc.) and
## can either be vector (nodal) data or bilateral (matrix) data. This example will 
## use Migraiton as an example of a bilateral independent variable and Corruption
## as an example of a nodal independent variable.

## FOLLOW THE SAME STEPS AS THE DEPENDENT VARIABLE FOR INDEPENDNET BILATERAL VARIABLES
## Note: to include a continuous variable rather than binary, adjusted code to convert
## matrices to networks. Otherwise ignore the adjustments in that section of code.
## Again, it might be helpful to keep different datasets in different folders. 
setwd("~/Desktop/Thesis Work/Datasets/Migration")
MIG11<-read.csv("MIG 2011.csv", header=TRUE, row.names=1)
MIG12<-read.csv("MIG 2012.csv", header=TRUE, row.names=1)
MIG13<-read.csv("MIG 2013.csv", header=TRUE, row.names=1)

MIG11M<-as.matrix(MIG11)
MIG12M<-as.matrix(MIG12)
MIG13M<-as.matrix(MIG13)

class(MIG11M)<-"numeric"
class(MIG12M)<-"numeric"
class(MIG13M)<-"numeric"

MIG11M<-handleMissings(MIG11M, na=NA, method="zero")
MIG12M<-handleMissings(MIG12M, na=NA, method="zero")
MIG13M<-handleMissings(MIG13M, na=NA, method="zero")

### LOAD CONTINUOUS EDGE VALUES
## Once again, you may need to flip the "directed" or "loops" argument if your data requires.
## Keep ignore.eval=FALSE (tells it to NOT ignore the value), keep na.rm=TRUE (though there shouldn't be any.
## names.eval is the argument that allows you to name the values that will occupy the edge.
## This can be any string that you like, but it needs to be the same for each year. (All years of Migration
## should have the same value name, all years of trade should have the same value name, etc.)
MIG11N<-as.network(MIG11M, directed=TRUE, loops=FALSE, ignore.eval=FALSE, na.rm=TRUE, names.eval='MIG')
MIG12N<-as.network(MIG12M, directed=TRUE, loops=FALSE, ignore.eval=FALSE, na.rm=TRUE, names.eval='MIG')
MIG13N<-as.network(MIG13M, directed=TRUE, loops=FALSE, ignore.eval=FALSE, na.rm=TRUE, names.eval='MIG')

### LOAD INDEPENDENT NODAL VARIABLES
## Each nodal variable needs to be associated with a country. For this reason, I have found it easier
## and more time efficient to simply separate the nodal values and load them in as separate datasets.
## I'm sure there is a more code-efficient way to do this within R. Independent nodal variables MUST be 
## paired with a country that is also found in the bilateral variables. Therefore, it is difficult to 
## simply use a dataframe with all relevant years.
setwd("~/Desktop/Thesis Work/Datasets/Raw Indicator Scores Files")
COR11<-read.csv("COR 2011.csv", header=TRUE, row.names=1)
COR12<-read.csv("COR 2012.csv", header=TRUE, row.names=1)
COR13<-read.csv("COR 2013.csv", header=TRUE, row.names=1)

## EXCLUDE NAs (SPECIFY NAs ARE MISSING DATA)
COR11<-na.exclude(COR11)
COR12<-na.exclude(COR12)
COR13<-na.exclude(COR13)

##### FORMAT NETWORKS PRIOR TO ANALYSIS #####
## I change my working directory to a file devoted specifically to output (model summaries, gof, etc.)
setwd("~/Desktop/Thesis Work/Statistical Output")

### LOAD NODAL ATTRIBUTES
## Each nodal value must be tied to its corresponding node in the dependent variable.
## Once again, make sure you are keeping the naming convention the same throughout all
## years of the same variable. 
SD11N %v% "COR" <- COR11
SD12N %v% "COR" <- COR12
SD13N %v% "COR" <- COR13

### LOAD EDGE ATTRIBUTES
## You must specifically tie the values you identified when loading the data to any 
## continuous independent variables of interest. 
MIG11N<-as.matrix.network.adjacency(MIG11N, attrname="MIG")
MIG12N<-as.matrix.network.adjacency(MIG12N, attrname="MIG")
MIG13N<-as.matrix.network.adjacency(MIG13N, attrname="MIG")

### GROUP ALL BILATERAL FILES INTO A LIST OBJECT. 
## Note: the nodal variables have already been tied to the dependent variable. 
## It is helpful to name these list objects the same thing as the values that 
## their edges may have, but this is not required. 
## These lists will be used in the btergms themselves. 
SD<-list(SD11N, SD12N, SD13N)
MIG<-list(MIG11N, MIG12N, MIG13N)

##### IDENTIFY CHARACTERISTICS ENDOGENOUS TO THE DEPENDENT VARIABLE #####
## Before running the first tergm, one must theorize about the endogenous characteristics of the structure
## of the dependent variable. These characteristics are controlled for - ergm methodology controls for these
## characteristics as a way to account for the interdependence within the dependent variable and to help 
## potentially identify additional significant effects of exogenous variables.

## From an a priori perspective, there are several assumptions we can make about the TIP networks. First, 
## these networks are sparse - not every connection between countries exists. Second, we ought to observe
## as specific directionality - victims flow from sources to destinations and there likely aren't many pairs
## of countries that have mutual ties. Third, once an edge exists, it likely doesn't disappear. Once 
## a connection between countries is determined to be viable, traffickers will continue to use the connection.

## To check these assumptions, calculate some descriptive network statistics for the dependent variable.
## Note that trends will be more discernable when envisoned over all years. 

## IN-DEGREE (In-Connections)
i11<-degree(SD11N, gmode="digraph", cmode="indegree")
i12<-degree(SD12N, gmode="digraph", cmode="indegree")
i13<-degree(SD13M, gmode="digraph", cmode="indegree")

indeg<-list(i11, i12, i13)
write.csv(indeg, "SD indeg.csv")

## OUT-DEGREE (Out-Connections)
o11<-degree(SD11N, gmode="digraph", cmode="outdegree")
o12<-degree(SD12N, gmode="digraph", cmode="outdegree")
o13<-degree(SD13N, gmode="digraph", cmode="outdegree")

outdeg<-list( o11, o12, o13)
write.csv(outdeg, "SD outdeg.csv")

## BETWEENNESS (number of shortest paths that pass thorugh a node; how important the node is to the network)
bet11<-betweenness(SD11N, gmode="digraph", cmode="directed")
bet12<-betweenness(SD12N, gmode="digraph", cmode="directed")
bet13<-betweenness(SD13N, gmode="digraph", cmode="directed")

bet<-list(bet11, bet12, bet13)
write.csv(bet, "SD bet.csv")

## ISOLATES (Number of nodes with no connections)
iso11<-isolates(SD11)
iso12<-isolates(SD12)
iso13<-isolates(SD13) 

write.csv(iso11, "SD isolates11.csv")
write.csv(iso12, "SD isolates12.csv")
write.csv(iso13, "SD isolates13.csv")

## DYAD CENSUS (Number of mutual dyads, asymmetric dyads, or null dyads)
dyad11<-dyad.census(SD11N)
dyad12<-dyad.census(SD12N)
dyad13<-dyad.census(SD13N)

dyad<-list(ddyad11, dyad12, dyad13)
write.csv(dyad, "SD dyad census.csv")

## TRIAD CENSUS (number of triad configurations; see help page for list)
triad11<-triad.census(SD11N, mode="digraph")
triad12<-triad.census(SD12N, mode="digraph")
triad13<-triad.census(SD13N, mode="digraph")

triad<-list(triad11, triad12, triad13)
write.csv(triad, "SD triad census.csv")

## Using the results of these tests, we can confirm that the network is indeed sparse,
## more asymmetric dyads exist than mutual dyads, the degree distribution is similar 
## across all years, and the number of isolates declines each year. Obviously, these are 
## just descriptive statistics and cannot conclusively tell us anything about our data, 
## but they suggest which ergm endogenous terms to include in the model. We will run a model
## to determine if these terms are indeed significant. If they are not, we must rethink how
## the dependent variable might be structured. 

## Consult ergm.terms in the help pane for a comprehensive list of terms that can be included.
## Many of these are not appropriate for networks where the nodes are countries. 

## Consult tergm.terms in the help pane for a comprehensive list of terms that can be included.
## These are the terms that induce the time-series element of the tergm. Without one of these terms,
## the tergm essentially becomes a pooled ergm. 

## After consulting the above glossaries and the statistics that support our a priori thinking, I hypothesize
## that the following terms ought to be included in the model: edges (by convention; similar to the 
## constant), asymmetric dyads, geometrically weighted in-degree, geometrically weighted out-degree, 
## and memory-autoregreesion (time step = 1).

## TERGMs or ERGMs operate as a series of simulations simulating all possible permutations of the dependent
## networks constrained by structrual terms. 

## Edges is analogous to the constant term in a regression equation and keeps constant the number of edges.
## Asymmetric dyads keeps constant the number of asymmetric dyads in the dependent networks.
## Geometrically weighted in-/out-degree controls for the degree distribution of the network, down-weighting
## against the possibility of many nodes having many connections. (Set weight with alpha parameter)
## Memory-autoregression indicates that connections that exist in a previous time step (here the time step
## directly before the current time step) also exists in the current time step.

##### TEST THE ENDOGENOUS CHARACTERISTICS #####
## By convention, one tests and reports the endogenous characteristics without any exogenous variables. 
## Note 1: the .5 in the gwidegree/gwodegree term is the down weighting parameter. .5 is a common value,
## and a value of 1 indicates no weight whatsoever. The lower the value, the stronger the down weight.
## Note 2: The "R=50" value indicates how many iterations of the model will be tested. For publication
## purposes, this value should be no less than 1,000. However, tergms are computationally expensive. 
## If you are exploring the data and model terms to potentially include, use a lower R value to save time.
endog_Model<-btergm(SD~edges+asymmetric+gwidegree(.5, fixed=TRUE)+
                        gwodegree(.5, fixed=TRUE)+memory(type="autoregression", lag=1), R=50)

## Along with the coefficients for each term, the summary includes 95% Confidence Intervals.
## If the confidence interval does not cross 0, the term is significant at the 95% level.
summary(endog_Model)

## Direct interpretation of the endogenous model terms is important to a different degree depending
## on the purpose of your study. If you are interested in the endogenous terms themselves, one should
## obviously spend more time on the interpretations. If you are interested in exogenous effects,
## endogenous term significance is the most important thing - you want to be controlling for endogenous
## effects so as to isolate the effect of your exogenous variables!

## In this case, all of our endogenous terms are significant. A direct inerpretation can be conducted by
## taking the exp(X) of the coefficient. Here, we see that edges, both degree terms, and asymmetric have
## a negative relationship while the time lag has a positive. These are hard to conceptualize, but the 
## relationships seem to suggest a sparse graph - if edges, asymmetric dyads, or degree distributions 
## increase, the likelihood of a human trafficking connection decreases. This makes sense in a sparse 
## network - again we are CONTROLLING for existing structure. We observe HT at the current rate with 
## the structures incldued in the model. If the structures change in frequency, the likelihood we observe
## trafficking should also decrease. The time lag also makes sense - if the connection exists in the 
## previous year, the likelihood it exists in the current year increases. 

## Unfortunately, a clear and concise goodness-of-fit measure does not exist for tergms. Instead, we look
## for model fit by examining goodness of fit plots. The model fits better the closer estimated values
## of network statistics (black lines) fall within box-plots of expected values. Other plots are also 
## included, such as the ROC/PR curve which captures number of false positives and false negatives. 
## As with R term in the tergm itself, to publish results, the nsim value (number of simulated networks)
## should be higher (at least 20). Here it is lower to save time. 
endog_gof<-gof(endog_Model, nsim=5)

##### CONDUCT AN EXOGENOUS TEST #####
## Include the significant endogenous terms you found in the previous step.
## The exogenous variables are inluded simply as the nodal value and the edge
## value. There are other possible terms that can be used as independent variables, such
## as homophily, that are detailed in the ergm.terms documentation page. 
exog_Model<-btergm(SD~edges+asymmetric+gwidegree(.5, fixed=TRUE)+
                      gwodegree(.5, fixed=TRUE)+memory(type="autoregression", lag=1)
                   +nodecov("COR")+edgecov(MIG, attrname="MIG"), R=50)
summary(exog_Model)
exog_gof<-gof(exog_Model, nsim=5)


