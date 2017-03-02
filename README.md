# Datahack @ Yale 2017 #

This code was developed in 2 days at [Datahack@Yale 2017](http://datahack.yale.edu/) and won 2nd place overall out of 15 teams.

## Yale Policy Lab Challenge ##

The challenge from the [Yale Policy Lab](http://isps.yale.edu/programs/policy-lab) and the [Justice Collaboratory](https://www.law.yale.edu/justice-collaboratory-0) was to better understand police misconduct by analyzing public data on pollice officer complaints from the Chicago Police Department collected by the [invisible institute](https://github.com/invinst/chicago-police-data).

Part of the challenge was to use statistical modeling to predict future police misconduct while the other part was effectively visualizing the network of complaints to aid in interpretation.

### Data Visualization ###

We visualized the co-complaint network where each officer is a node and they are linked if they were implicated in the same complaint ([click here to view in browser](https://rawgit.com/stefanavey/datahack-team6/master/graphs/D3_LM.html)).


### Data dictionary ###

[`data/toy.officer_data_cleaned.csv`](https://github.com/stefanavey/datahack-team6/tree/master/data/toy.officer_data_cleaned.csv)

- **officer_id**
- **first_name**
- **last_name**
- **appointed_date**
- **race**
- **gender**
- **birth_year**
- **age**
- **rank**
- **primary**
- **secondary**
- **tertiary**

[`data/toy.complaint_data_cleaned.csv`](https://github.com/stefanavey/datahack-team6/tree/master/data/toy.complaint_data_cleaned.csv)

- **crid** - complaint record ID
- **officer_id** - the officer ID
- **incident_date** - date of incident
- **incident_time** - time of incident (00:00 to 23:59)
- **district** - the district extracted from **beat_2012_geocoded**
- **beat_2012_geocoded** - beats
  - A police beat is an area.
  - Described by integer up to 4 digits.
  - A beat is a subset of a district.
  - The first two digits of a beat represent the District.
- **complaint_category** - category of the complaint
- **complaint_name** - category of the complaint
- **final_finding** - The final finding
  - EX: Exonerated
  - UN: Unfounded
  - NAF: No Affidavit
  - NS: Not Sustained
  - SU: Sustained
  - `NA`: Unknown

