This repository contains all necessary scripts to run and understand government interest patents.

The code is divided into the following two folders:

* **1_R_Data_Prep:** These scripts will help users prepare data for analysis

* **2_Data_Viz_Generate:** These scripts will generate various graphics and visualizations 

There are two steps involved in running codefiles:
1. Prep data tables
2. Generate data visualizations

**Step 1: Data Preparation**

	a. To gather data, download the most updated bulk download datafiles from http://www.patentsview.org/download/ and save it to a folder 'data_to_read' in '2_Data_Viz_Generate'. 
		These are the bulk download datafiles you will need:
			1. uspatentcitation
			2. patent
			3. patent_govintorg
			4. patent_inventor
			5. patent_assignee
			6. nber
			7. wipo
			8. wipo_field
			9. government_organization
			10. assignee
			11. government_interest
			12. rawassignee

	b. Go to the folder '1_R_Data_Prep'. There should be 5 scripts in this folder that will help prepare the data for visualization. Open these scripts in R/RStudio. 
	
	c. Make sure you change your working directory to match the folder path where you stored the bulk download files from part a above (Example folder path: *"yourpath/government-interest/2_Data_Viz_Generate/data_to_read"*).Then run through all of the R scripts in numerical order to generate tables 
	
	d. Save all the tables generated from running the scripts in '1_R_Data_Prep' in the folder 'data_to_read' under the folder '2_Data_Viz_Generate'. These tables include the following:
			
			1. temp_patent_level_gi.csv
			2. temp_gi_level_gi.csv
			3. temp_patent_level_all
			4. all_assignees
			5. temp_5yr_citations_by_cite_yr1
			6. temp_5yr_citations_by_cite_yr2
			7. temp_5yr_citations_by_cite_yr3
			8. temp_5yr_citations_by_cite_yr4
			9. temp_5yr_citations_by_cite_yr5
			10. assignee_type

	e. Go to this webpage: https://www.aaas.org/programs/r-d-budget-and-policy/historical-trends-federal-rd and download the Excel file for "Total R&D by Agency, 1976-2018" under the _By Agency_ section. 

		1. Open this file in Excel and transpose the data table. To do this, copy the data table (only upto Total R&D, ignore the R&D: Defense and Nondefense section) and using the paste special option, transpose the table. Save the transposed table as "agencies.csv" in the same 'data_to_read' folder under the '2_Data_Viz_Generate' folder.


**Step 3: Generate Data Visualizations**
	
	a. Go to the folder '2_Data_Viz_Generate'. There should be several scripts in this folder that will generate visualizations. Open these scripts in R/RStudio. Make sure you change your working directory to match this folder path (Example folder path: *"yourpath/government-interest/2_Data_Viz_Generate/"*)

	b. Run through the script "requirements.R". This will install and load all necessary libraries and fonts.

	c. Next, run the script "govIntBrief.R". This will generate all other visualizations.

		** NOTE: In this step, if you generated tables using R in Step 1, then make sure you change the way you read in the tables in this script. **

		This script will generate two folders:
		(1) Folder 'data_viz': a folder to store all of the viz that will be generated from running this R script
		(2) Folder 'out': a folder to store all of the tables that will be generated from running this R script
	
	
