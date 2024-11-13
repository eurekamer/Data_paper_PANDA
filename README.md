Data from: The first national database of blue mussel mortality in France
Lemesle Stéphanie1, Bernard Ismaël2, Herbomez Anaïs1, Normand Julien1*
1Laboratory Environnement Ressources de Normandie, IFREMER, Department Océanographie et Dynamique des Ecosystèmes, COAST research unit, FR
2Eurêka Mer, FR
*Correspondence:
Corresponding Author: julien.normand@ifremer.fr

Contents:
Database of mussel Mytilus mortality monitoring along the French coast. This database contains data from various monitoring programs coordinated by 6 different operators. These datasets were chosen due to their use of standardized methodologies, aligned with those of the REMORA monitoring program, first published in 2001 (Fleury et al., 2001). Departing from professional shellfish farming practices, these protocols were adopted since the 1990s first to monitor Pacific oyster (Magallana gigas) farming performance and now serve as the cornerstone for national and locally operated networks (Fleury et al., 2023).
For each site and year, the data consists in longitudinal survey of mortality proportion obtained by successive counts of a small group of mussels, enclosed in a plastic meshed bag attached on a wooden pole or over a longline. The oldest data were obtained in 2005. They came from 40 sites spread across the Channel-Atlantic coastline, located in the most important mussel rearing sectors.
We collected 6 datasets, one for each operator. We filled in the metadata and eliminated the most atypical series (different species, observation period window shifted by several months compared with other series, unknown or rare origin of spat) to ensure uniformity. We then merged the datasets and computed cumulative mortality over time. In all, we retained data for 292 site x yearly monitoring campaign combinations.
Once this stage has been completed, we fitted mathematical models to achieve temporal interpolation, in order to mitigate of temporal disparities (i.e. sampling frequency) between the various monitoring programs. We tested Gompertz or Logistic models, but we finally selected Logistic models based on AIC scores. We computed mortality metrics derived from the models, to improve data usability. Code to reproduce these analyses are archived here, as well as figures included in the companion data paper: "The first national database of blue mussel mortality in France ".

Original Datasets:
-	Dataset 1 to Dataset 6: These are 6 raw data sets in .csv provided each by one operator. All these datasets contain at least information about the monitoring site identification, the date of acquisition of the observation, and a count data of living and dead mussels or mortality rate from which the cumulative mortality has been calculated.
-	Id_site: This .csv file contains the complete information about the 40 monitoring sites, their vernacular name, the operator involved in monitoring, the coordinates in WGS84, and the coding used for site identification in the datapaper. Coordinates have been rounded off to the decimal point to prevent the company from being identified.

New datasets:
-	Raw_dataset: This is a .csv file containing raw data with supplementary information to improve data usability. The table contains 3,318 rows and 14 columns. The Site column indicates the Site identification, using the same coding used for site identification as in the datapaper. The Date column is a date information in year-month-day format. The spat_year column indicates the year of birth for spat. The alive_nb and dead_nb columns contain count of living and dead mussels in the Support (i.e. the type of rearing structure in which the mussels were kept, see below for column description) at the time of sampling. When information was missing, the retained data sets nevertheless contained information on the mortality rate, which is then entered in the column labelled MR. In that case, we have also entered the initial number of individuals deployed in the field in the column init_nb, so that anyone interested can recalculate the proportions at time t and associated confidence intervals. The Batch column contains information to identify the different batches, for sites where several batches of mussels are monitored at the same time. The site_spat column indicates the origin of spat. The Specie column contains information of the genetic of the batch (Mytilus edulis, Mytilus galloprovinciallis or presumed hybrids). The Support column allowed to differentiate between mussels batches deployed onshore, which are reared in plastic meshed bag (that look like miniature versions of the bags used for oysters breeding) or mussel batches deployed offshore on longlines, which are kept in the plates of a lantern. The column labelled Operator contains Operator identifier. The column Comments contains free-text information, which can be used to identify certain batches that have been the subject of other research and monitoring programs. Most counting campaigns start in year N-1, when the year's very young spat are deployed in the field, although data acquisition and the actual counts are not carried out until the beginning of year N. The Campaign column indicates the yearly campaign at year N when survey was mostly conducted. In all, dataset covers 317 Site x yearly monitoring Campaign combinations.
-	Clean_dataset is a .csv file containing the data retained for the rest of the study, after harmonization of the information and elimination of the less relevant series. The table contains 2,812 rows and 10 columns. The Site, Date, Batch, Campaign, and Operator columns contain the same information as for the Raw_dataset (see above). Id_site column is a numeric identifier for site. Latitude and Longitude columns contain geographical positioning information for monitoring site in WGS84. DOY column indicates the date expressed in number of days since January 1st. CM column contains data of cumulative mortality, expressed as a proportion of the initial number of mussels. After cleaning and homogeneization, this dataset contains data for the 292 site x campaigns combinations that were retained for the rest of the study.
-	Death_predicted.csv contains the daily prediction of cumulative mortality from the logistic model between DOY -122 (beginning of the monitored in September of the year N-1) and DOY 270 (day of the end of the monitoring in September of the year N) for all of the 292 site x campaigns combinations. The table contains 114,756 rows and 4 columns (one row for each day). The Campaign, id_site, DOY columns contain the same information as for the Clean_dataset (see above). CM column contains data of cumulative mortality, expressed as a proportion of the initial number of mussels, predicted by the model.
-	Mortality_metric is a .csv table containing some statistics summarizing the information derived from the models. It contains 292 rows and 6 columns (one row for each Site x Campaign combination). The Campaign, id_site, DOY columns contain the same information as for the Clean_dataset (see above). MM is the cumulative mortality value predicted by the model on day 261, the date at which most of the annual monitoring operations were completed. MI is the mortality initiation date, which corresponds to the DOY on which mortality reaches 10% of the value of the MM. The column labelled alpha contains the value for the slope of a linear model adjusted between the initiation of mortality (MI) and the maximum mortality at day 261 (MM) points.

Code:
1_Map_Sampling_sites.R contains the code to recreate the map of the sampling sites (Figure 1 in the datapaper). 
2_National_Data_mytilus.R contains the code for data harmonization and cleaning that produces Raw_dataset.csv. It computes the cumulative mortality and produces Clean_dataset.csv. Finally, it adjusts both models to the cleaned data, selects the best of them, and estimates the daily cumulative mortality values contained in the death_predicted.csv dataset. It's also the code that generates the Figures 2, 3 and 4 of the datapaper.
3_Mortality_metrics computes MM, MI and alpha for each Site x Campaign combination (mortality_metric.csv). It also produces the Figure 5 of the datapaper.