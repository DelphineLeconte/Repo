Final Project Report
================

Organizational Network Analysis - ORGB 672

Summer 2022

Submitted to: Prof. Roman Galperin

Contributors: Marion Laniel - 260431778 , Andi Li - 260950768, Mehdi
Yachfine - 261005628, Delphine Leconte-Demarsy – 261053933

# 1. INTRODUCTION

## 1.1. Context

The U.S. Patent and Trademark Office (USPTO) supports innovation and
economic growth. It issues patents to inventors, as well as registers
trademarks and intellectual property, since the first half of the 19th
century. The USPTO employs over 10,000 patent examiners whose primary
task is to assess inventions for patentability and issue patents when
appropriate.

Inventors submit patent applications, pay the associated fees and
receive a decision from a patent examiner. If all the inventor’s claims
made in a patent application are allowed by the examiner. the examiner
issues a patent. Otherwise, the examiner issues a formal rejection of
the application. In that case the applicant can continue to reapply with
no limit on the number of attempts.

To support dynamic innovation, the processing time for patent
applications is a sensitive issue for inventors. There is increasing
pressure for the USPTO to reduce the length of review times. The USPTO
wants to understand what affects the examination time, including the
causes and the variations, to reduce the backlog.

## 1.2. Research questions

In this context, the present report aims at finding what affects the
examination time by answering the following questions. What are the
organizational and social factors associated with the length of patent
application prosecution? Do data show a role of gender and ethnicity in
the processes?

## 1.3. Methodology

The data used for this report are composed of USPTO patent and examiner
data, and advice instances data. The data on examiners and patent
applications come from various public sources. During application
processing, examiners sometimes consult other examiners for advice. The
data on advice instances are based on public records, listing the
application for which the consultation was requested, the date of the
advice request, and the ids of the examiner seeking advice and the one
providing it. These data were processed to assign gender and race by
checking names and family names from databases available in R, while
tenure days of examiners were estimated based on records.

We calculated the application process time for each application as an
indicator of efficiency in the application process. For this purpose, we
considered filing dates as the start of the application process. For the
end of the process, we considered date of final decision as either being
approved or being abandoned. The difference between start and end date
provides the total application process time for each application.

In order to run models on a more workable set we reduced the size of the
data by selecting two workgroups based on the first three digits of
examiner_art_unit field. These workgroups are the ones starting by 162-
and 179-. For those workgroups, we examined the demographic
characteristics of the examiners, based on gender and race distribution.

Based on the connections between examiners as given by advice seeking,
we created a directed advice network for those two subgroups. We
calculated different measures of centralities for this network as
different positions in the advice network may influence work processes
and outcomes.

We then studied the links between efficiency in application process,
demographics, and position in the network as provided by centrality
scores of the examiners. For that purpose, we used linear regression
models to estimate the relationship between centrality and application
processing time and controlled for other factors that could influence
that relationship, in particular gender through the inclusion of an
interaction term “gender x centrality” in the model.

The choice of factors under study was driven by suspicions that the
USPTO has that the efficiency of examiners differ systematically by
demographic characteristics, notably gender and race. The ethical and
legal importance of those factors contributed to influence the choice.

For further details regarding methodology, please refer to the attached
code.

# 2. RESULTS AND INTERPRETATION

## 2.1. Demographic characterization of the subset

Based on summary statistics and chart

Gender distribution:

We automatically assign gender based on the first name of the examiner
in the original dataset. As we can see, there are over 2 million records
in the applications because there are many records and applications
which examiners worked on. Our first step therefore is to get all unique
names in a separate list. We are getting the probabilities across the
five broad US Census categories: White, Black, Hispanic, Asian and
others. We then pick the race category with the highest probability for
each last name.

Counts in our subset: male: 309, female: 186 (Note: we dropped NA
values, that appeared when the algorithm was not able to predict the
gender).

As we can see from the gender distribution, the female category
represents approximately 30% while male category represents 70%. We can
see significant gender imbalance within the organization. From previous
research, we can say this subset has a similar gender distribution
compared to the general population of examiners in the USPTO database
(28.30% female and 56.65% male1).

Race distribution

Asian: 327, white: 158, black: 6, Hispanic: 4

From the Race distribution for workgroups, we can see that White
examiners represent more than more than 75% of the examiners, compared
to Asian with less than 20% as the second largest groups, and less
significant percentage for Black and Hispanic. We can observe that there
may exist some systematic bias based on the extremely unbalanced
proportion among the race and gender categories.

Note that NA values can appear when the algorithm could not attach a
race to a family name, this is highly likely in the case of different
alphabets in particular.

The general population of examiners as provided by the applications
database also shows majority of White at 63.38%, followed by Asian at
29.18%, Black at 4.46% and Hispanic at 2.96%, and “other” at 0.02%. The
subset we have obtained by picking two workgroups has thus a similar
pattern to that of the general population of examiners but in different
proportions.

Tenure

An examiner’s tenure is the difference in days between the earliest date
and the latest date for the examiner.

Tenure has a reducing effect on processing time

Increasing the tenure day by one unit increases the processing time by
0.1 days.

Average, min/max and compare to total population

Centrality measures

Discuss centrality and other characteristics

We can see significant gender and race imbalance within the
organization. Asian examiners seem to have the shortest processing time,
On the contrary, it takes 106 additional days for the Hispanic examiners
to do so.

From the centrality scores, closeness and authority scores seem to have
significant effect on the application processing time. On the other
hand, degree and betweenness show quite limited influence on the
application processing time.

The degree centrality effect in reducing application processing time is
stronger in males than in females and the betweenness centrality effect
in reducing application processing time is more in females than males.
The result shows the relative preferences on the way to process the
applications by gender. Male examiners seem better at building cohesive
network that all examiners know each other well, while female examiners
are better at bridging and facilitating networks that they have close
examiners in different groups who don’t know each other well. It shows
that women that are measured by the closeness are well placed to
influence the network. Their application processing time shortened by 50
days compared to males in the same position.

## 2.2. Results of lm1

First, it comes that the model R2 is relatively low (0.39). This means
that this linear regression does not fit the data very well. However,
the model summary shows that all the dependent variables’ coefficients
are highly significant according to their p-values.

Thus, the following points can be deducted:

The average processing time for an Asian woman from tc1600 according to
the model is around 196 days, which is around 6.5 months. The following
points are compared to this value.

The processing time is increased by around 5.5 months (166 days) when
the examiner is from tc1700.

The processing time is increased by 36 days when the examiner is a male.

When the examiner is black, the processing time is increased by around
2.7 months (81 days).

When the examiner is Hispanic or White, the processing time is
respectively decreased by around one year (351 days) for the Hispanic
ones and 5 months (153 days) for the White ones.

When the tenure days are increased by one unit (one day), the processing
time is increasing by approximately one day.

Finally, increasing the centrality degree, or the betweenness by one
unit leads to an increase in the processing time by respectively 0.5
days and 0.2 days while adding one unit to the closeness score leads to
a decrease in the processing time by 56 days (around 2 months).

According to this model, it clearly comes that race have a significant
impact on the processing time. It comes that Hispanic examiners benefits
the most from low processing times, followed by the White ones while the
Asian and specially the Black ones tend to have longer delays.

The tc group is also very important as examiners from the tc1600 seem to
see their processing time shortened significantly compared to the other
group.

Finally, the processing time seems to be also very sensible to the
closeness score. This means that people that are very well placed to
influence the network tend to see their processing time shortened by
around 2 months.

## 2.3. Results of lm2

First, it comes that the model R2 is relatively low (0.39). This means
that this linear regression does not fit the data very well. However,
the model summary shows that the majority of the dependent variables’
coefficients are highly significant according to their p-values. The
coefficients that are not significant are related to the following
variables: gender male, race black, the centrality degree for male
examiners, the closeness and betweenness for black examiners and the
betweenness for the Hispanic ones. Thus, these variables are excluded
from the coefficients’ interpretation. All the remaining ones are
significant at the 95% level (p-value lower than 5%).

The following points can be deducted from this summary:

The average processing time for an Asian woman from tc1600 according to
the model is around 156 days, which is around 5.2 months. The following
points are compared to this value.

The processing time is increased by another 5.2 months (156 days) when
the examiner is from tc1700.

When the examiner is Hispanic or White, the processing time is
respectively decreased by around 2.4 years (880 days) for the Hispanic
ones and approximately 2 months (59 days) for the White ones.

When the tenure days are increased by one unit (one day), the processing
time is increasing by approximately one day.

Increasing the centrality degree, the closeness or the betweenness by
one unit leads to an increase in the processing time by respectively
1.24, 19.5 and 2 days.

Adding one unit to the closeness score of a male examiner increases the
processing time by 72 days compared to a female examiner. This means
that a female examiner tends to benefit more from a position of
influence within the network than a male in terms of processing time.

According to the interaction term for a male examiner betweenness (0.12
days), the difference with a female examiner is negligeable.

White examiners benefit more from the centrality degree than examiners
from other races. They are followed by Asians, Blacks and finally
Hispanics. The difference between Whites and Asians is relatively low
compared to the one between Asians, Blacks and Hispanics. Indeed, adding
one unit to the centrality degree decreases by around one day the
processing time compared to Asians while it increases it by 7.8 and 8.4
days for Blacks and Hispanics.

Similarly for closeness, White examiners are the main benefiters
followed by the Asians and finally the Hispanics. This means that Whites
benefits a lot more from a position of influence compared to Asians and
especially to Hispanics. As mentioned before, the result for Black
examiners is not enough significant to be considered. For the closeness,
the differences between the races are way more important. Indeed,
increasing the closeness score of a white examiner by one unit decreases
the processing time by 167 days (5.5 months) compared to an Asian
examiner while it increases it by 1,021 days (2.8 years) for a Hispanic
examiner.

The results for the interaction terms of the races with the betweenness
do not allow to compare the races as the results are only significant
for the White examiners.

# 3. RECOMMENDATIONS

The main objective of the USPTO is to improve the patent application
processing time so inventors can receive their final decision faster.
Using the results from our linear regression model, managers at the
USPTO can see which factors contributes to either increased or decreased
processing time. One finding suggests that employees from the technology
center 1600 process applications faster on average. Therefore, it would
be interesting to better capture the business processes in place at this
technology center so we can incorporate them elsewhere. Also, examiners
from technology center 1700 take 5 months and a half to process
applications. Therefore, managers should deep dive into what is wrong in
the processes and behaviours so they can address them. One way to
implement good behaviours and practices and stop the spread of toxic
processes is to use existing networking at the USPTO. To instore good
practices, they need to find key examiners well positioned in the
networks so they can infect every employee into adopting them, using
complex contagion. Indeed, if a certain number of connections of an
employee is adopting the behaviour, that individual will also adopt it.
The same way goes for stopping the spread of toxic processes. The USPTO
needs to identify which key individual are spreading the toxic behavior
and cut their connections so the spread stops.

Another issue present at the USPTO is the lack of diversity in genders
and races. We noticed in our analysis that white males were advantaged
regarding the processing time of application. This goes with the fact
that they have higher centrality scores in general. Our impression from
this finding is that white males are better able to seek advice from
their network as they are better connected to other examiners, which
allows them to reduce their processing time for patent application.
Since network are subject to homophily, which means that people are
connecting with individuals with similar attributes to them, people of
other races and females have a harder time seeking advice in the network
as they not as well connected, which could explain why they have longer
process time. We recommend to the USPTO to increase the diversity among
examiners, starting at hiring, by using incentives to attract people of
different origins and gender. This would benefit everyone as the network
would be richer and people from different background would bring
interesting insights and processes to the organization.

As noted during our research in terms of demographics, the gender of
examiners in our subset of applications is similar to that of the
general population of examiners as provided in the applications dataset.
It would be interesting to investigate further on the role of gender
distribution by comparing the efficiency of application processing in
our subset, to efficiency levels found in other subsets of the
population with different gender distribution. Similarly, race
distribution pattern is similar to that of the general population but in
different proportions so further research could be directed towards
understanding better the implications of this race distribution for
efficiency.

Lastly, we suggest to the USPTO to use algorithms such as Random Forest
to better select variables to integrate in the linear regression model
in order to make it more accurate by only relying on variables having
the most impact on the target variable.
