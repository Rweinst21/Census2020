Data user guide
================
Jenna
December 16, 2019

### Objective: Describe the data required for the Census 2020 planning maps in order to facilitate decisionmaking around presentation of the data.

#### Low Response Score

The Low Response Score (LRS) is an ordinarly least-squares regression
model fitted on 25 variables that: 1. best predicted 2010 mail
no-response rate, and  
2\. have practical interpretation for outreach (e.g., include population
of single mothers, exclude MOEs).

Point (1) is especially important, as it means the LRS does not account
for internet response. One potential risk in using this data for the use
case of outreach is misappropriating resources to neighborhoods of
highly internet-literate residents like young, mobile renters, who would
be considered HTC according to mail return rate, but may not in fact be
HTC with the advent of the internet census.

The LRS is a predicted mail non-response rate, so “an LRS value of 17.7
should be interpreted as 17.7% of households in that census tract are
predicted to NOT self-respond to the decennial census.” Percentage of
renters, percentage of people aged 18-24, and percentage of households
headed by unmarried females are the strongest predictors.

##### Threshold

I have not been able to find a recommended threshold for what LRS is
most important in the research paper, FAQ, ROAM user guide, or NNIP
partners’ publications. A map in an article titles “Identifying
Hard-to-Survey Populations Using Low Response Scores by Census Tract” on
the Census website uses the breakdown of LRS 0 - 17.9, 18 - 21.9, 22 -
24.9, 25 - 28.9, 29 - 46.2.

The national non-response rate in 2010 was 20.7 percent, of course with
large geographic variation. Even so, that number could serve as a good
cutoff for determining HTC.

Below is a map of New Orleans tract-level LRS broken into the same
groups the CB shows.

![](Data_user_guide_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

The map below shows that a large majority of tracts in New Orleans are
predicted to have a lower response rate in 2020 than the national
response rate in 2010.

![](Data_user_guide_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

#### Young children

Below is a map of the same tracts mapped above (i.e. those predicted to
have a lower response rate in 2020 than the national response rate in
2010), colored according to the number of children 4 and younger living
there as of ACS 2013-2017.

Not sure if we’re interested in using a threshold on these other data
points as well or just providing the data for the HTC tracts. Thinking
about what threshold would work well for choosing what tracts to target,
I’m not sure there’s a clear answer. Our conversations thus far have
focused on the raw number of children in the tract. As far as I’ve seen,
there is no specific intervention that happens once a tract has a
certain number of children, for example. If we switched the variable to
“households with two or more children 4 or under” or “children to
households ratio,” that may do more to identify what we’re looking for,
i.e. tracts whose children may be harder to count than other children.
However, I think the original rationale of “more children = more
hard-to-count people” makes plenty of sense.

![](Data_user_guide_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

Below is a map of the same tracts mapped above, colored according to the
percent of children 4 and younger living there as of ACS 2013-2017.

![](Data_user_guide_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

#### English as a barrier

The most straighforward measure of English ability is the English
ability question from the ACS.

A paper published at the Census Bureau reports “that the English-ability
question, despite being a self-assessment, does a good job of measuring
English ability.”

In Neighborhood Profiles, we group categories of English ability
according to the following logic: “We’ve combined the categories of
people who speak only English at home with those who speak another
language at home but report that they speak English ‘well.’ This way we
can focus on data about those individuals for whom speaking English is a
barrier.”

![](Data_user_guide_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

![](Data_user_guide_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

The plot & table below are to get a feel for how many tracts are home to
people for whom English is a barrier & what proportion of tract
residents have English as a barrier. Excluding tracts that have 0% of
their populations with English as a barrier, on average city-wide, 2.35%
of tract residents have English as a barrier. The horizontal line on the
plot represents that average.

<table>

<thead>

<tr>

<th style="text-align:left;">

tract

</th>

<th style="text-align:right;">

% w Eng as barrier

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

001750

</td>

<td style="text-align:right;">

23.67

</td>

</tr>

<tr>

<td style="text-align:left;">

001749

</td>

<td style="text-align:right;">

20.15

</td>

</tr>

<tr>

<td style="text-align:left;">

001741

</td>

<td style="text-align:right;">

16.35

</td>

</tr>

<tr>

<td style="text-align:left;">

001751

</td>

<td style="text-align:right;">

12.52

</td>

</tr>

<tr>

<td style="text-align:left;">

006300

</td>

<td style="text-align:right;">

10.84

</td>

</tr>

<tr>

<td style="text-align:left;">

005000

</td>

<td style="text-align:right;">

9.53

</td>

</tr>

<tr>

<td style="text-align:left;">

000603

</td>

<td style="text-align:right;">

8.62

</td>

</tr>

<tr>

<td style="text-align:left;">

004900

</td>

<td style="text-align:right;">

7.31

</td>

</tr>

<tr>

<td style="text-align:left;">

007200

</td>

<td style="text-align:right;">

7.12

</td>

</tr>

<tr>

<td style="text-align:left;">

012300

</td>

<td style="text-align:right;">

5.70

</td>

</tr>

<tr>

<td style="text-align:left;">

000611

</td>

<td style="text-align:right;">

5.68

</td>

</tr>

<tr>

<td style="text-align:left;">

014300

</td>

<td style="text-align:right;">

5.49

</td>

</tr>

<tr>

<td style="text-align:left;">

008500

</td>

<td style="text-align:right;">

5.35

</td>

</tr>

<tr>

<td style="text-align:left;">

009600

</td>

<td style="text-align:right;">

5.21

</td>

</tr>

<tr>

<td style="text-align:left;">

008600

</td>

<td style="text-align:right;">

4.77

</td>

</tr>

<tr>

<td style="text-align:left;">

009400

</td>

<td style="text-align:right;">

4.70

</td>

</tr>

<tr>

<td style="text-align:left;">

006400

</td>

<td style="text-align:right;">

4.43

</td>

</tr>

<tr>

<td style="text-align:left;">

014000

</td>

<td style="text-align:right;">

3.94

</td>

</tr>

<tr>

<td style="text-align:left;">

009200

</td>

<td style="text-align:right;">

3.67

</td>

</tr>

<tr>

<td style="text-align:left;">

001747

</td>

<td style="text-align:right;">

3.58

</td>

</tr>

<tr>

<td style="text-align:left;">

001748

</td>

<td style="text-align:right;">

3.54

</td>

</tr>

<tr>

<td style="text-align:left;">

013900

</td>

<td style="text-align:right;">

3.29

</td>

</tr>

<tr>

<td style="text-align:left;">

001200

</td>

<td style="text-align:right;">

3.04

</td>

</tr>

<tr>

<td style="text-align:left;">

008400

</td>

<td style="text-align:right;">

2.99

</td>

</tr>

<tr>

<td style="text-align:left;">

006000

</td>

<td style="text-align:right;">

2.99

</td>

</tr>

<tr>

<td style="text-align:left;">

002300

</td>

<td style="text-align:right;">

2.99

</td>

</tr>

<tr>

<td style="text-align:left;">

007605

</td>

<td style="text-align:right;">

2.89

</td>

</tr>

<tr>

<td style="text-align:left;">

009100

</td>

<td style="text-align:right;">

2.87

</td>

</tr>

<tr>

<td style="text-align:left;">

006500

</td>

<td style="text-align:right;">

2.85

</td>

</tr>

<tr>

<td style="text-align:left;">

000606

</td>

<td style="text-align:right;">

2.79

</td>

</tr>

<tr>

<td style="text-align:left;">

000607

</td>

<td style="text-align:right;">

2.77

</td>

</tr>

<tr>

<td style="text-align:left;">

011500

</td>

<td style="text-align:right;">

2.54

</td>

</tr>

<tr>

<td style="text-align:left;">

012800

</td>

<td style="text-align:right;">

2.52

</td>

</tr>

<tr>

<td style="text-align:left;">

007101

</td>

<td style="text-align:right;">

2.50

</td>

</tr>

<tr>

<td style="text-align:left;">

004500

</td>

<td style="text-align:right;">

2.24

</td>

</tr>

<tr>

<td style="text-align:left;">

003000

</td>

<td style="text-align:right;">

2.22

</td>

</tr>

<tr>

<td style="text-align:left;">

003900

</td>

<td style="text-align:right;">

2.19

</td>

</tr>

<tr>

<td style="text-align:left;">

000615

</td>

<td style="text-align:right;">

2.13

</td>

</tr>

<tr>

<td style="text-align:left;">

012700

</td>

<td style="text-align:right;">

1.99

</td>

</tr>

<tr>

<td style="text-align:left;">

001702

</td>

<td style="text-align:right;">

1.99

</td>

</tr>

<tr>

<td style="text-align:left;">

007502

</td>

<td style="text-align:right;">

1.96

</td>

</tr>

<tr>

<td style="text-align:left;">

001800

</td>

<td style="text-align:right;">

1.78

</td>

</tr>

<tr>

<td style="text-align:left;">

001730

</td>

<td style="text-align:right;">

1.77

</td>

</tr>

<tr>

<td style="text-align:left;">

002503

</td>

<td style="text-align:right;">

1.73

</td>

</tr>

<tr>

<td style="text-align:left;">

007501

</td>

<td style="text-align:right;">

1.62

</td>

</tr>

<tr>

<td style="text-align:left;">

000618

</td>

<td style="text-align:right;">

1.61

</td>

</tr>

<tr>

<td style="text-align:left;">

013000

</td>

<td style="text-align:right;">

1.49

</td>

</tr>

<tr>

<td style="text-align:left;">

007800

</td>

<td style="text-align:right;">

1.49

</td>

</tr>

<tr>

<td style="text-align:left;">

002402

</td>

<td style="text-align:right;">

1.47

</td>

</tr>

<tr>

<td style="text-align:left;">

000617

</td>

<td style="text-align:right;">

1.38

</td>

</tr>

<tr>

<td style="text-align:left;">

001723

</td>

<td style="text-align:right;">

1.37

</td>

</tr>

<tr>

<td style="text-align:left;">

001725

</td>

<td style="text-align:right;">

1.32

</td>

</tr>

<tr>

<td style="text-align:left;">

003303

</td>

<td style="text-align:right;">

1.32

</td>

</tr>

<tr>

<td style="text-align:left;">

000612

</td>

<td style="text-align:right;">

1.24

</td>

</tr>

<tr>

<td style="text-align:left;">

006900

</td>

<td style="text-align:right;">

1.17

</td>

</tr>

<tr>

<td style="text-align:left;">

001500

</td>

<td style="text-align:right;">

1.16

</td>

</tr>

<tr>

<td style="text-align:left;">

002700

</td>

<td style="text-align:right;">

1.15

</td>

</tr>

<tr>

<td style="text-align:left;">

007000

</td>

<td style="text-align:right;">

1.13

</td>

</tr>

<tr>

<td style="text-align:left;">

003800

</td>

<td style="text-align:right;">

1.10

</td>

</tr>

<tr>

<td style="text-align:left;">

001402

</td>

<td style="text-align:right;">

1.07

</td>

</tr>

<tr>

<td style="text-align:left;">

012500

</td>

<td style="text-align:right;">

1.06

</td>

</tr>

<tr>

<td style="text-align:left;">

012102

</td>

<td style="text-align:right;">

1.04

</td>

</tr>

<tr>

<td style="text-align:left;">

001746

</td>

<td style="text-align:right;">

1.02

</td>

</tr>

<tr>

<td style="text-align:left;">

007604

</td>

<td style="text-align:right;">

1.01

</td>

</tr>

<tr>

<td style="text-align:left;">

000616

</td>

<td style="text-align:right;">

0.99

</td>

</tr>

<tr>

<td style="text-align:left;">

000604

</td>

<td style="text-align:right;">

0.94

</td>

</tr>

<tr>

<td style="text-align:left;">

001900

</td>

<td style="text-align:right;">

0.90

</td>

</tr>

<tr>

<td style="text-align:left;">

004000

</td>

<td style="text-align:right;">

0.89

</td>

</tr>

<tr>

<td style="text-align:left;">

013200

</td>

<td style="text-align:right;">

0.88

</td>

</tr>

<tr>

<td style="text-align:left;">

005500

</td>

<td style="text-align:right;">

0.87

</td>

</tr>

<tr>

<td style="text-align:left;">

000602

</td>

<td style="text-align:right;">

0.83

</td>

</tr>

<tr>

<td style="text-align:left;">

003301

</td>

<td style="text-align:right;">

0.81

</td>

</tr>

<tr>

<td style="text-align:left;">

012600

</td>

<td style="text-align:right;">

0.75

</td>

</tr>

<tr>

<td style="text-align:left;">

001302

</td>

<td style="text-align:right;">

0.74

</td>

</tr>

<tr>

<td style="text-align:left;">

001720

</td>

<td style="text-align:right;">

0.72

</td>

</tr>

<tr>

<td style="text-align:left;">

002800

</td>

<td style="text-align:right;">

0.70

</td>

</tr>

<tr>

<td style="text-align:left;">

003600

</td>

<td style="text-align:right;">

0.69

</td>

</tr>

<tr>

<td style="text-align:left;">

007606

</td>

<td style="text-align:right;">

0.68

</td>

</tr>

<tr>

<td style="text-align:left;">

002504

</td>

<td style="text-align:right;">

0.67

</td>

</tr>

<tr>

<td style="text-align:left;">

002501

</td>

<td style="text-align:right;">

0.63

</td>

</tr>

<tr>

<td style="text-align:left;">

012101

</td>

<td style="text-align:right;">

0.62

</td>

</tr>

<tr>

<td style="text-align:left;">

001701

</td>

<td style="text-align:right;">

0.61

</td>

</tr>

<tr>

<td style="text-align:left;">

003304

</td>

<td style="text-align:right;">

0.57

</td>

</tr>

<tr>

<td style="text-align:left;">

010700

</td>

<td style="text-align:right;">

0.55

</td>

</tr>

<tr>

<td style="text-align:left;">

001401

</td>

<td style="text-align:right;">

0.52

</td>

</tr>

<tr>

<td style="text-align:left;">

001722

</td>

<td style="text-align:right;">

0.50

</td>

</tr>

<tr>

<td style="text-align:left;">

003702

</td>

<td style="text-align:right;">

0.50

</td>

</tr>

<tr>

<td style="text-align:left;">

014500

</td>

<td style="text-align:right;">

0.48

</td>

</tr>

<tr>

<td style="text-align:left;">

010300

</td>

<td style="text-align:right;">

0.47

</td>

</tr>

<tr>

<td style="text-align:left;">

011900

</td>

<td style="text-align:right;">

0.47

</td>

</tr>

<tr>

<td style="text-align:left;">

010800

</td>

<td style="text-align:right;">

0.47

</td>

</tr>

<tr>

<td style="text-align:left;">

011100

</td>

<td style="text-align:right;">

0.47

</td>

</tr>

<tr>

<td style="text-align:left;">

013400

</td>

<td style="text-align:right;">

0.45

</td>

</tr>

<tr>

<td style="text-align:left;">

007700

</td>

<td style="text-align:right;">

0.45

</td>

</tr>

<tr>

<td style="text-align:left;">

001743

</td>

<td style="text-align:right;">

0.45

</td>

</tr>

<tr>

<td style="text-align:left;">

009000

</td>

<td style="text-align:right;">

0.45

</td>

</tr>

<tr>

<td style="text-align:left;">

002900

</td>

<td style="text-align:right;">

0.42

</td>

</tr>

<tr>

<td style="text-align:left;">

005400

</td>

<td style="text-align:right;">

0.42

</td>

</tr>

<tr>

<td style="text-align:left;">

013302

</td>

<td style="text-align:right;">

0.39

</td>

</tr>

<tr>

<td style="text-align:left;">

011400

</td>

<td style="text-align:right;">

0.38

</td>

</tr>

<tr>

<td style="text-align:left;">

011700

</td>

<td style="text-align:right;">

0.37

</td>

</tr>

<tr>

<td style="text-align:left;">

008300

</td>

<td style="text-align:right;">

0.37

</td>

</tr>

<tr>

<td style="text-align:left;">

001724

</td>

<td style="text-align:right;">

0.37

</td>

</tr>

<tr>

<td style="text-align:left;">

001745

</td>

<td style="text-align:right;">

0.36

</td>

</tr>

<tr>

<td style="text-align:left;">

010000

</td>

<td style="text-align:right;">

0.34

</td>

</tr>

<tr>

<td style="text-align:left;">

005601

</td>

<td style="text-align:right;">

0.32

</td>

</tr>

<tr>

<td style="text-align:left;">

003302

</td>

<td style="text-align:right;">

0.32

</td>

</tr>

<tr>

<td style="text-align:left;">

000400

</td>

<td style="text-align:right;">

0.32

</td>

</tr>

<tr>

<td style="text-align:left;">

014200

</td>

<td style="text-align:right;">

0.31

</td>

</tr>

<tr>

<td style="text-align:left;">

012200

</td>

<td style="text-align:right;">

0.28

</td>

</tr>

<tr>

<td style="text-align:left;">

013800

</td>

<td style="text-align:right;">

0.25

</td>

</tr>

<tr>

<td style="text-align:left;">

004100

</td>

<td style="text-align:right;">

0.25

</td>

</tr>

<tr>

<td style="text-align:left;">

000800

</td>

<td style="text-align:right;">

0.25

</td>

</tr>

<tr>

<td style="text-align:left;">

013700

</td>

<td style="text-align:right;">

0.24

</td>

</tr>

<tr>

<td style="text-align:left;">

000701

</td>

<td style="text-align:right;">

0.22

</td>

</tr>

<tr>

<td style="text-align:left;">

013301

</td>

<td style="text-align:right;">

0.17

</td>

</tr>

<tr>

<td style="text-align:left;">

001301

</td>

<td style="text-align:right;">

0.17

</td>

</tr>

<tr>

<td style="text-align:left;">

000613

</td>

<td style="text-align:right;">

0.17

</td>

</tr>

<tr>

<td style="text-align:left;">

002502

</td>

<td style="text-align:right;">

0.16

</td>

</tr>

<tr>

<td style="text-align:left;">

009900

</td>

<td style="text-align:right;">

0.14

</td>

</tr>

</tbody>

</table>

![](Data_user_guide_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

The map below shows only the tracts for which greater than 2.35% of the
residents have english as a barrier.

![](Data_user_guide_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->
