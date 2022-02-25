-------------------------------------------------------------------------------
File: new_README.txt for censusNYC2010/
Author: Lawrence Chillrud <lgc2139@cumc.columbia.edu>
Date: 02/01/22
-------------------------------------------------------------------------------

Contents of censusNYC2010/

	* nyc_census_2010.csv - this file contains the cleaned census data for NYC. 2168 tracts (rows) x 115 variables (cols). See data dictionary below. All raw data was found here (https://data2.nhgis.org/main) and cleaned by Yanelli, Rachel, and Lawrence in parts. You can see the breakdown of who cleaned which variables later on in this README. 

-------------------------------------------------------------------------------
Data directory for nyc_census_2010.csv:
-------------------------------------------------------------------------------

 gisjoin:	GIS Join Match Code
 year:		labelled as '2010' for all observations, but really 2006-2010 for variables with the prefix 's_', and 2010 for variables with the prefix 'c_'
 state:		State name
 statea:	State code
 county:	County name
 countya:	County code
 tracta:	Census tract code
 name:		Area name

 See variables from (A) the 2010 census (prefixed with a 'c_') or (B) the 2006-2010 American Community Survey (prefixed with a 's_') below. 

 Note: variables reported as percentages (with the infix '_perc_') do not necessarily sum to 100% for a given category. This is because we don't necessarily report all the subcategories within a category (e.g. 'other').

 Groupings: 
 	s_tot: Population totals for variables from the 2006-2010 American Community Survey
 	s_mtw: Mode of transport to work
 	s_ttw: Travel time to work
 	s_edu: Education level
 	s_inc: Income
 	s_occ: Occupation
 	s_emp: Employment status
 	s_fla: Foreign Language
 	s_hou: Housing
 	s_htg: Heating
 	s_plg: Plumbing
 	s_veh: Vehicles

 	c_tot: Population totals for variables from the 2010 census
 	c_hou: Housing
 	c_sex: Sex
 	c_rac: Race
 	c_age: Age
 	c_hhs: Households
 	c_hou: Housing
-------------------------------------------------------------------------------
(A) Variables from the 2010 census:
-------------------------------------------------------------------------------

Census 1b)
Table 23: Sex by Age (Black or African American Alone)
Universe: People who are Black or African American alone
Source code: PCT12B
NHGIS code:  IDF
	c_perc_black: Percent Black
	c_black_pop: Total Black population 

Table 24: Sex by Age (American Indian and Alaskan Native Alone)
Universe:    People who are American Indian and Alaska Native alone
Source code: PCT12C
NHGIS code:  IDG
	c_perc_ai: Percent American Indian
	c_ai_pop: Total American Indian population 

Table 25: Sex by Age (Asian Alone)
Universe:    People who are Asian alone
Source code: PCT12D
NHGIS code:  IDH
	c_perc_asian: Percent Asian
	c_asian_pop: Total Asian population

Table 26: Sex by Age (Native Hawaiian and Other Pacific Islander Alone)
Universe:    People who are Native Hawaiian and Other Pacific Islander alone
Source code: PCT12E
NHGIS code:  IDI
	c_perc_pacific_islander: Percent Native Hawaiian or other Pacific Islander
	c_pacific_islander_pop: Total Pacific Islander Population

Table 28: Sex by Age (Two or More Races)
Universe:    People who are Two or More Races
Source code: PCT12G
NHGIS code:  IDK
	c_perc_multiracial: Percent multiracial
 	c_multiracial_pop: Total multiracial population

Table 29: Sex by Age (Hispanic or Latino)
Universe:    People who are Hispanic or Latino
Source code: PCT12H
NHGIS code:  IDL
	c_perc_hispanic: Percent Hispanic
	c_hispanic_pop: Total Hispanic population

Table 30: Sex by Age (White Alone, Not Hispanic or Latino)
Universe:    People who are White alone, not Hispanic or Latino
Source code: PCT12I
NHGIS code:  IDM
	c_perc_white: Percent white
	c_white_pop: Total white population


Census 2a)
Table 1: Total Population
Universe: Total Population
Source code: PCT1
NHGIS code:  LGH
	c_population: Total population (used to calculate all %'s with universe = population)

Table 2: Sex by Age
Universe: Total Population
Source code: PCT3
NHGIS code:  LGJ
  	c_male_pop: Total male population (used to calculate % below)
	c_female_pop: Total female population (used to calculate % below)
  	c_perc_male: Percent male
  	c_perc_female: Percent female

Table 3: Median Age by Sex
Universe: Total Population
Source code: PCT4
NHGIS code:  LGK
  	c_median_age_tot: Median age

Table 6: Average Household Size by Age
Universe: Households
Source code: PCT7
NHGIS code:  LGN
  	c_mean_household_size_tot: Mean household size


Table 7: Household Type
Universe: Households
Source code: PCT8
NHGIS code:  LGO
 	c_total_households: Total number of households (used to calculate all household %'s)
	c_hh_type_family: Total number of households that are families
	c_hh_type_family_husband_wife: Total number of households where the householder is in a married couple
	c_hh_type_family_other_family: Total number of other family households
	c_hh_type_family_single_male: Total number of family households where the householder is a single male
	c_hh_type_family_single_female: Total number of family households where the householder is a single female
 	c_hh_type_nonfamily: Total number of non-family households
	c_living_alone: Total number of single-person households
	c_roommates: Total number of non-family households not living alone
	c_perc_nonfam_hh: Percent of households that are non-family households
	c_perc_married_hh: Percent of households where the householder is in a married couple
	c_perc_single_householder_fam_hh: Percent of households where the householder is single and household is a family
	c_perc_living_alone: Percent of households that are single-person households 
	c_perc_roommates: Percent of non-family households not living alone
	c_perc_fam_hh: Percent of households that are family households

Table 9: Households by Presence of People Under 18 by Household Type by Age of People Under 18 Years
Universe: Households
Source code: PCT10
NHGIS code:  LGQ
	c_hh_with_children: Total number of households with people under 18
 	c_hh_family_with_children: Total number of households that are families with children under 18
	c_perc_child_hh: Percent of households with people under 18
 	c_perc_child_fam_hh: Percent of family households with people under 18

Table 16: Presence of Multigenerational Households
Universe: Households
Source code: PCT17
NHGIS code:  LGX
	c_generations_3plus: Total number of families with 3 or more generations
	c_generations_under3: Total number of families with less than 3 generations

Table 30: Average Family Size by Age
Universe: Families
Source code: PCT31
NHGIS code:  LHB
	c_mean_family_size_tot: Mean family size

Table 39: Average Household Size of Occupied Housing Units by Tenure
Universe: Occupied Housing Units
Source code: HCT5
NHGIS code:  LHW
 	c_total_occupied_housing_units: Total number of occupied housing units
	c_owner_occupied_units: Total number of housing units occupied by the owner
	c_renter_occupied_units: Total number of housing units occupied by a renter
	c_perc_owner_occ: Percent of occupied housing units that are occupied by the owner
	c_perc_renter_occ: Percent of occupied housing units that are occupied by a renter

Occupancy Status <=====================================================================================this was me. I wonder if we need percentages of this....
    Universe:    Housing units
    Source code: H3
    NHGIS code:  IFE
	c_tot_housing_unt: Total number of housing units 
	c_occup_unt: Total occupied housing units 
	c_vacant_unt: Total vacant housing units

-------------------------------------------------------------------------------
(B) Variables from the 2006-2010 American Community Survey
-------------------------------------------------------------------------------

Means of Transportation to Work (MTW):
Universe: 	 Workers 16 years and over. 
Source code: B08301 
NHGIS code:  JM0
	s_tot_mtw: Total population used for calculating MTW %s below. 
	s_mtw_perc_carpooled: % who carpool to work.
	s_mtw_perc_drove_alone: % who drive alone [=(car, truck, van: drove alone + taxi cab + motorcycle) / s_tot_mtw * 100].
	s_mtw_perc_public_transport: % who take public transport (excluding taxi cab).
	s_mtw_perc_bike: % who bicycle.
	s_mtw_perc_walk: % who walk.

Travel Time to Work (TTW):
Universe: 	 Workers 16 years and over who did not work at home
Source code: B08303
NHGIS code:  JM2
	s_tot_ttw: Total population used for calculating TTW weighted avg below.
	s_weighted_avg_ttw: Weighted average of TTW (in minutes). [Calculated as (3*JM2E002 + 7*JM2E003 + ... +  100*JM2E013) / s_tot_ttw (=JM2E001)]

Educational Attainment (EDU):
Universe:    Population 25 years and over
Source code: B15002
NHGIS code:  JN9
	s_tot_edu: Total population used for calculating EDU %s below.
	s_edu_perc_nohs: % with less than a high school diploma or equivalent.
	s_edu_perc_hs: % with a high school diploma or equivalent (GED, or alternative).
	s_edu_perc_some_college: % with some college.
	s_edu_perc_college: % with an associate's or bachelor's degree.
	s_edu_perc_gradschool: % with a master's, professional, or doctorate degree.

Median Household Income in the Past 12 Months (in 2010 Inflation-Adjusted Dollars)
Universe:    Households
Source code: B19013
NHGIS code:  JOI
	s_median_household_income: Median Household Income in the Past 12 Months (in 2010 Inflation-Adjusted Dollars)

Public Assistance Income in the Past 12 Months for Households
Universe:    Households
Source code: B19057
NHGIS code:  JPB
	s_tot_public_assistance_households: Total population (# of households) used for calculating % below.
	s_perc_public_assistance_income: % with public assistance income. 

Median Family Income in the Past 12 Months (in 2010 Inflation-Adjusted Dollars)
Universe:    Families
Source code: B19113
NHGIS code:  JPO
	s_median_family_income: Median Family Income in the Past 12 Months (in 2010 Inflation-Adjusted Dollars)

Per Capita Income in the Past 12 Months (in 2010 Inflation-Adjusted Dollars)
Universe:    Total population
Source code: B19301
NHGIS code:  JQB
	s_percapita_income: Per Capita Income in the Past 12 Months (in 2010 Inflation-Adjusted Dollars)

Occupation industry (OCC):
Universe:    Civilian employed population 16 years and over
Source code: C24030
NHGIS code:  JRG
	s_tot_occ: Total population used for calculating OCC %s below.
	s_occ_perc_aggriculture: % working in "agriculture, forestry, fishing and hunting, and mining"
	s_occ_perc_construction_manufacturing: % working in "construction" + % working in "manufacturing"
	s_occ_perc_trade: % working in "wholesale trade" + % working in "retail trade"
	s_occ_perc_transport_warehousing_utils: % working in "transportation and warehousing, and utilities"
	s_occ_perc_info: % working in "information"
	s_occ_perc_finance: % working in "finance and insurance, and real estate and rental and leasing"
	s_occ_perc_professional: % working in "professional, scientific, and management, and administrative and waste management services"
	s_occ_perc_edu_health: % working in "educational services, and health care and social assistance"
	s_occ_perc_arts_rec_accomodations_food: % working in "arts, entertainment, and recreation, and accommodation and food services"
 	s_occ_perc_public_admin: % working in "public administration"

Employment Status 
Universe:    Population 25 years and over
Source code: B16010
NHGIS code:  J2U
	s_tot_employment_status: Total population used to calculate % below.
	s_employment_status_perc_unemployed: % of people unemployed. 

Language Spoken at Home
Universe:    Population 25 years and over
Source code: B16010
NHGIS code:  J2U
	s_tot_foreign_lang: Total population used to calculate % below.
	s_foreign_lang_perc_spoken: % of people who speak a language other than only english at home.


Median Number of Rooms
    Universe:    Housing units
    Source code: B25018
    NHGIS code:  JR4
	s_med_num_rooms: Median number of rooms 


Median Year Structure Built
    Universe:    Housing units
    Source code: B25035
    NHGIS code:  JSE
	s_med_yr_bult: Median year structure build 


Median Gross Rent (Dollars)
    Universe:    Renter-occupied housing units paying cash rent
    Source code: B25064
    NHGIS code:  JS5
	s_med_rent: Median Gross Rent (Dollars)


Median Value (Dollars)
    Universe:    Owner-occupied housing units
    Source code: B25077
    NHGIS code:  JTI
	s_med_hous_val: Median value (dollars


House Heating Fuel
    Universe:    Occupied housing units
    Source code: B25040
    NHGIS code:  JSJ
        s_tot_occupied_units: total use to estimate the percentages
	s_perc_util_gas: % of occupied housing with utility gas for heating
	s_perc_gas: % of occupied housing using bottled, tank, or LP gas for heating
 	s_perc_elect: % of occupied housing using electricity for heating
	s_perc_oil_ker: % of occupied housing using fuel oil, kerosene, etc for heating 
	s_perc_coal_coke: % of occupied housing using coal or coke for heating
	s_perc_wood:  % of occupied housing using wood for heating
	s_perc_solar: % of occupied housing using solar energy for heating
	s_perc_other: % of occupied housing using other fuel for heating
	s_perc_no_fuel: % of occupied housing without fuel for heating

Plumbing Facilities for All Housing Units [No data for Puerto Rico]
    Universe:    Housing units
    Source code: B25047
    NHGIS code:  JSQ
 	s_perc_comp_plump: % housing units with complete plumbing facilities
 	s_perc_incom_plump: % housing units lacking complete plumping facilities

Table 2:     Tenure by Vehicles Available
    Universe:    Occupied housing units
    Source code: B25044
    NHGIS code:  JSN
        s_tot_owner_occupied: total used to estimate the % for owners
	s_perc_owner_no_vehi: % owner occupied housing units with no vehicle available
 	s_perc_owner_vehi: % owner occupied housing units with one or more vehicles 
        s_tot_renter_occupied: % used to estimate the % for renters
	s_perc_renter_no_vehi: % renter occupied units with no vehicle available
	s_perc_renter_vehi: % renter occupied units with one or more vehicles available
