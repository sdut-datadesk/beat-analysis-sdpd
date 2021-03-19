# Stops by beat analysis: San Diego Police Department
By: [Lauryn Schroeder](https://www.sandiegouniontribune.com/sdut-lauryn-schroeder-staff.html) and [Lyndsay Winkley](https://www.sandiegouniontribune.com/sdut-lyndsay-winkley-staff.html)

This repository contains data and code for the analysis [reported and published](XXXXXX) by *The San Diego Union-Tribune* on March XXXXX, 2021.

### About

The Racial and Identity Profiling Act of 2015 (RIPA) requires nearly all California law enforcement agencies to submit demographic data on all detentions and searches. The Union-Tribune obtained in January stop data from the San Diego Police Department.

The Union-Tribune collected this data to analyze stops conducted by the San Diego Police Department.

The code analyzes the types of people who are stopped, including their age, gender, perceived disability and limited English status, as well as the rate in which racial groups are stopped by beat.

### Methodology / Notes

The San Diego Police Department data tables contain all pedestrian and traffic stops from July 2018 through December 2020. The original data tables are compiled in nine, .csv files that are available for download on the [city's data portal website](https://data.sandiego.gov/datasets/police-ripa-stops/).

Since more than one individual can be involved a stop (officers are required to record the ethnicity of drivers and passengers) the Union-Tribune opted to analyze the race of each person involved, which is the same technique used by RIPA officials.

In some circumstances, officers list more than one perceived race for an individual involved in traffic stops. 

Individuals who were perceived as Hispanic and any other race, were included in Hispanic totals. Individuals perceived as more than one race were categorized as those with two or more race. The remaining race categories were left the same.

More than one action can take place during a stop. The Union-Tribune analyzed the number of people on which those actions were taken, as well as the number of times each action was used during the data time frame.

### The SDUT repository contains the following:

- `beat_demographics_2019_acs.csv` - Population totals of each beat in the city of San Diego, according to the U.S. Census Bureau 2019 5-year American Community Survey (ACS).
- `pd_beats_datasd` - Shapefiles and names for all beats within the city of San Diego, available on the [city's data portal website](https://data.sandiego.gov/datasets/police-beats/).
- `ripa_actions_taken_datasd.csv` - Actions taken in each stop by San Diego Police Department. Contains data for stops from July 2018 through December 2020.
- `ripa_contraband_evid_datasd.csv` - Contraband or evidence found during each stop by San Diego Police Department. Contains data for stops from July 2018 through December 2020.
- `ripa_disability_datasd.csv` - Disabilities perceived in each stop by San Diego Police Department. Contains data for stops from July 2018 through December 2020.
- `ripa_prop_seize_basis_datasd.csv` - The basis for which property was seized in each stop by San Diego Police Department. Contains data for stops from July 2018 through December 2020.
- `ripa_prop_seize_type_datasd.csv` - The types of property seized in each stop by San Diego Police Department. Contains data for stops from July 2018 through December 2020.
- `ripa_race_datasd.csv` - The race of each individual stopped by San Diego Police Department. Contains data for stops from July 2018 through December 2020.
- `ripa_search_basis_datasd.csv` - The basis for each search conducted by San Diego Police Department. Contains data for stops from July 2018 through December 2020.
- `ripa_stop_reason_datasd.csv` - The reason for each stop by San Diego Police Department. Contains data for stops from July 2018 through December 2020.
- `ripa_stops_datasd.csv` - General location, date and time for each stop conducted by San Diego Police Department. Contains data for stops from July 2018 through December 2020.
- `sdpd-beat-analysis.R` - Import and analysis R script documenting findings published by the Union-Tribune.

### Sourcing
Please link and source [*The San Diego Union-Tribune*](https://www.sandiegouniontribune.com/) when referencing any analysis or findings in published work.

### Questions / Feedback

Email Lauryn Schroeder at [lauryn.schroeder@sduniontribune.com](mailto:lauryn.schroeder@sduniontribune.com) or Lyndsay Winkley at [lyndsay.winkley@sduniontribune.com](mailto:lyndsay.winkley@sduniontribune.com).
