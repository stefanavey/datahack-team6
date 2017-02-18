# README #

## Yale Policy Lab Challenge ##


## Toy Data dictionary ##

### officers
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

### complaints
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

