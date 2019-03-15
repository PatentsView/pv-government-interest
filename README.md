This repository contains all necessary scripts to run and understand government interest patents.

The code is divided into the following two folders:

* **1_R_Data_Prep:** These scripts will help prepare data for analysis

* **2_Data_Viz_Generate:** These scripts will generate various visualizations 

There are two steps involved in running codefiles:
1. Prep data tables
2. Generate data visualizations

**Step 1: Data Preparation**

	a. To gather data, download the most updated versions of the bulk download datafiles from http://www.patentsview.org/download/ and save it to a folder called 'data_to_read' in '2_Data_Viz_Generate'. 
		These are the bulk download datafiles you will need:
			1. assignee
			2. foreigncitation
			3. government_interest
			4. government_organization
			5. inventor_gender 
			6. nber 
			7. patent 
			8. patent_assignee
			9. patent_govintorg 
			10.patent_inventor wipo_field
			11.rawassignee
			12. usapplicationcitation
			13. uspatentcitation
			14. wipo
			15. wipo_field

	b. Go to the folder '1_R_Data_Prep'. There should be 7 scripts in this folder that will help prepare the data for visualization. Open these scripts in R/RStudio. 
	
	c. Make sure you change your working directory to match the folder path where you stored the bulk download files from part a above (Example folder path: *"yourpath/government-interest/2_Data_Viz_Generate/data_to_read"*). First run "assignees_looked_up_types.R" and then run through all of the R scripts in numerical order to generate tables. 
	
	d. Save all the tables generated from running the scripts in '1_R_Data_Prep' in the folder 'data_to_read' under the folder '2_Data_Viz_Generate'. These tables include the following:
		
		Assignees_looked_up_types.R:
			1. assignees_lookedup_types.csv
			2. temp_gi_assignee_type.csv

		Script 0:
			1. temp_num_foreign_documents_cited.csv
			2. temp_num_us_applications_cited.csv
			3. temp_num_us_patents_cited.csv
			4. temp_num_times_cited_by_us_patents.csv
			5. temp_patent_counts_fac_vfinal.csv

		Script 1:
			1. temp_5yr_citations_by_cite_all.csv
			2. temp_5yr_citations_all.csv
			3. temp_5yr_citations_by_cite.csv
			4. temp_5yr_citations.csv


		Script 2:
			1. temp_patent_level_all.csv
			2. temp_patent_level_gi_subset.csv
			3. temp_patent_level_nongi_subset.csv

		Script 3:
			1. temp_gi_assignee_type.csv
			2. all_assignees.csv
			3. assignee_type.csv

		Script 4:
			1. temp_govt_associated_inventors_clean.csv
			2. temp_gi_inventor_gender.csv
			3. temp_gi_has_female_inv.csv

		Script 5:
			1. temp_5yr_citations_by_cite_yr1
			2. temp_5yr_citations_by_cite_yr2
			3. temp_5yr_citations_by_cite_yr3
			4. temp_5yr_citations_by_cite_yr4
			5. temp_5yr_citations_by_cite_yr5


	e. Go to this webpage: https://www.aaas.org/programs/r-d-budget-and-policy/historical-trends-federal-rd and download the Excel file for "Total R&D by Agency, 1976-2018" under the _By Agency_ section. 

		1. Open this file in Excel and transpose the data table. To do this, copy the data table (only upto the "Total R&D" section (ignore the R&D: Defense and Nondefense section)) and use the paste special option _transpose the table_. Save the transposed table as "agencies.csv" in the same 'data_to_read' folder under the '2_Data_Viz_Generate' folder.


**Step 3: Generate Data Visualizations**
	
	a. Go to the folder '2_Data_Viz_Generate'. There should be several scripts in this folder that will generate visualizations. Open these scripts in R/RStudio. Make sure you change your working directory to match this folder path (Example folder path: *"yourpath/government-interest/2_Data_Viz_Generate/"*)

	b. Run through the script "requirements.R". This will install and load all necessary libraries and fonts.

	c. Next, run the script "govIntBrief.R". This will generate all other visualizations.

		** NOTE: In this step, if you generated tables using R in Step 1, then make sure you change the way you read in the tables in this script. **

		This script will generate two folders:
		(1) Folder 'data_viz': a folder to store all of the viz that will be generated from running this R script
		(2) Folder 'out': a folder to store all of the tables that will be generated from running this R script
	
	
