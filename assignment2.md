Data analysis assignment 2
================
Thea Smith
08 Feb 2020

In this assignment you will work with relational data, i.e. data coming
from different data tables that you can combine using keys. Please read
ch.13 from R for Data Science before completing this assignment –
<https://r4ds.had.co.nz/relational-data.html>.

## Read data

We will work with three different tables: household roster from wave 8
(*h\_egoalt*), stable characteristics of individuals (*xwavedat*), and
household data from wave 8 (*h\_hhresp*).

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 3.5.2

    ## Warning: package 'ggplot2' was built under R version 3.5.2

    ## Warning: package 'tibble' was built under R version 3.5.2

    ## Warning: package 'tidyr' was built under R version 3.5.2

    ## Warning: package 'purrr' was built under R version 3.5.2

    ## Warning: package 'dplyr' was built under R version 3.5.2

    ## Warning: package 'stringr' was built under R version 3.5.2

    ## Warning: package 'forcats' was built under R version 3.5.2

``` r
# You need to complete the paths to these files on your computer.
Egoalt8 <- read_tsv("/Users/TheaSmith/Desktop/Data Analysis III/assignment-2-tkcs201/UKDA-6614-tab/tab/ukhls_w8/h_egoalt.tab")
Stable <- read_tsv("/Users/TheaSmith/Desktop/Data Analysis III/assignment-2-tkcs201/UKDA-6614-tab/tab/ukhls_wx/xwavedat.tab")
Hh8 <- read_tsv("/Users/TheaSmith/Desktop/Data Analysis III/assignment-2-tkcs201/UKDA-6614-tab/tab/ukhls_w8/h_hhresp.tab")
```

## Filter household roster data (10 points)

The **egoalt8** data table contains data on the kin and other
relationships between people in the same household. In each row in this
table you will have a pair of individuals in the same household: ego
(identified by *pidp*) and alter (identified by *apidp*).
*h\_relationship\_dv* shows the type of relationship between ego and
alter. You can check the codes in the Understanding Society codebooks
here –
<https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation>.

First we want to select only pairs of individuals who are husbands and
wives or cohabiting partners (codes 1 and 2). For convenience, we also
want to keep only the variables *pidp*, *apidp*, *h\_hidp* (household
identifier), *h\_relationship\_dv*, *h\_esex* (ego’s sex), and *h\_asex*
(alter’s sex).

``` r
Partners8 <- Egoalt8 %>%
        filter(h_relationship_dv == 1 | h_relationship_dv== 2) %>%
        select("pidp", "apidp", "h_hidp", "h_relationship_dv", "h_sex", "h_asex")
```

Each couple now appears in the data twice: 1) with one partner as ego
and the other as alter, 2) the other way round. Now we will only focus
on heterosexual couples, and keep one observation per couple with women
as egos and men as their alters.

``` r
Hetero8 <- Partners8 %>%
        # filter out same-sex couples
        filter(h_sex != h_asex) %>%
        # keep only one observation per couple with women as egos
        filter(h_sex == 2 & h_asex == 1)
```

## Recode data on ethnicity (10 points)

In this assignment we will explore ethnic endogamy, i.e. marriages and
partnerships within the same ethnic group. First, let us a create a
version of the table with stable individual characteristics with two
variables only: *pidp* and *racel\_dv* (ethnicity).

``` r
Stable2 <- Stable %>%
        select(pidp, racel_dv)
```

Let’s code missing values on ethnicity (-9) as NA.

``` r
Stable2 <- Stable2 %>%
        mutate(racel_dv = recode(racel_dv, `-9` = NA_real_))
```

Now let us recode the variable on ethnicity into a new binary variable
with the following values: “White” (codes 1 to 4) and “non-White” (all
other codes).

``` r
Stable2 <- Stable2 %>%
        mutate(race = case_when(
                between(racel_dv,1,4)~"White",
                racel_dv > 4 ~ "non-White"
        ))
```

## Join data (30 points)

Now we want to join data from the household roster (*Hetero8*) and the
data table with ethnicity (*Stable2*). First let us merge in the data on
ego’s ethnicity. We want to keep all the observations we have in
*Hetero8*, but we don’t want to add any other individuals from
*Stable2*.

``` r
JoinedEthn <- Hetero8 %>%
        inner_join(Stable2, by = "pidp")
```

Let us rename the variables for ethnicity to clearly indicate that they
refer to egos.

``` r
JoinedEthn <- JoinedEthn %>%
        rename(egoRacel_dv = racel_dv) %>%
        rename(egoRace = race)
```

Now let us merge in the data on alter’s ethnicity. Note that in this
case the key variables have different names in two data tables; please
refer to the documentation for your join function (or the relevant
section from R for Data Science) to check the solution for this problem.

``` r
JoinedEthn <- JoinedEthn %>%
         left_join(Stable2, by = c("apidp" = "pidp"))
```

Renaming the variables for alters.

``` r
JoinedEthn <- JoinedEthn %>%
        rename(alterRacel_dv = racel_dv) %>%
        rename(alterRace = race)
```

## Explore probabilities of racial endogamy (20 points)

Let us start by looking at the joint distribution of race (White
vs. non-White) of both partners.

``` r
TableRace <- JoinedEthn %>%
        # filter out observations with missing data
        filter(!is.na(egoRace)) %>%
        filter(!is.na(alterRace)) %>% 
        count(egoRace, alterRace)
TableRace
```

    ## # A tibble: 4 x 3
    ##   egoRace   alterRace     n
    ##   <chr>     <chr>     <int>
    ## 1 non-White non-White  1790
    ## 2 non-White White       326
    ## 3 White     non-White   266
    ## 4 White     White      9694

Now calculate the following probabilities: 1) for a White woman to have
a White partner, 2) for a White woman to have a non-White partner, 3)
for a non-White woman to have a White partner, 4) for a non-White woman
to have a non-White partner.

Of course, you can simply calculate these numbers manually. However, the
code will not be reproducible: if the data change the code will need to
be changed, too. Your task is to write reproducible code producing a
table with the required four probabilities.

``` r
TableRace %>%
        # group by ego's race to calculate sums
        group_by(egoRace) %>% summarise(totalbyrace = sum(n)) %>% inner_join(TableRace, by = "egoRace") %>%
        # create a new variable with the total number of women by race
        mutate(WomRace = sum(n)) %>%
        # create a new variable with the required probabilities 
        mutate(probability =(n/sum(WomRace)))
```

    ## # A tibble: 4 x 6
    ##   egoRace   totalbyrace alterRace     n WomRace probability
    ##   <chr>           <int> <chr>     <int>   <int>       <dbl>
    ## 1 non-White        2116 non-White  1790   12076     0.0371 
    ## 2 non-White        2116 White       326   12076     0.00675
    ## 3 White            9960 non-White   266   12076     0.00551
    ## 4 White            9960 White      9694   12076     0.201

## Join with household data and calculate mean and median number of children by ethnic group (30 points)

1)  Join the individual-level file with the household-level data from
    wave 8 (specifically, we want the variable for the number of
    children in the household).

<!-- end list -->

``` r
kids <- Hh8 %>%
        select(h_hidp,h_nkids_dv)

KidsIndivHouse<- JoinedEthn %>% 
        inner_join(kids, by ="h_hidp")
```

2)  Select only couples that are ethnically endogamous (i.e. partners
    come from the same ethnic group) for the following groups: White
    British, Indian, and
Pakistani.

<!-- end list -->

``` r
KidsIndivHouse <- KidsIndivHouse %>% filter(egoRacel_dv == 1 | egoRacel_dv == 9 | egoRacel_dv == 10)
```

3)  Produce a table showing the mean and median number of children in
    these households by ethnic group (make sure the table has meaningful
    labels for ethnic groups, not just numerical codes).

<!-- end list -->

``` r
TableKidsIndivHouse <- KidsIndivHouse %>%
        group_by(egoRacel_dv) %>% 
        filter(!is.na(h_nkids_dv)) %>% 
        summarise(
                ChildrenMean= mean(h_nkids_dv),
                ChildrenMedian = median(h_nkids_dv)
        ) %>% 
        rename("Ethnicity" = egoRacel_dv) %>% 
        rename("Children Mean" = ChildrenMean) %>% 
        rename("Children Median" = ChildrenMedian) %>% 
        mutate(Ethnicity = Ethnicity %>% 
                       recode(`1` = "White British",
                              `9` = "Indian",
                              `10` = "Pakistani"))

TableKidsIndivHouse
```

    ## # A tibble: 3 x 3
    ##   Ethnicity     `Children Mean` `Children Median`
    ##   <chr>                   <dbl>             <dbl>
    ## 1 White British           0.582                 0
    ## 2 Indian                  0.977                 1
    ## 3 Pakistani               1.79                  2

4)  Write a short interpretation of your results. What could affect your
    findings?

From these results we can see that White British couples on average
considerably have the fewest children with a mean of 0.58 and the median
of 0 shows that the majority of White British couples who partook in the
survey did not have any children. Indian couples on average had 0.98 or
1 child with the median being 1. Pakistani has notably the highest
average of children per couple with 1.79 children with a median of 2.

These results could be influenced by various factors, namly traditions
and cultures that come with ethnicity, such as religion. However,
notably, this data set excludes various parents, namely same sex
parents, parents where the child has left home, single parents or
parents of two different ethnicities, thus making these results
inaccurate. Furthermore there are missing values and this is only a
sample of the UK, not the UK as a whole therefore a very different
picture could be painted if this data set included more/all parents.
