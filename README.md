This repository contains all necessary scripts to run and understand government interest patents.

There are two steps involved in running the scripts:

	_1. Data Preparation_ - The scripts in the **1_R_Data_Prep** folder will prepare data for analysis.

	_2. Data Visualization_ - The scripts in the **2_Data_Viz_Generate** folder will generate visualizations.

**Step 1: Data Preparation**

	a. Download the bulk download data files from http://www.patentsview.org/download/. 
		These are the bulk download data files you will need:
			1. assignee
			2. foreigncitation
			3. government_interest
			4. government_organization
			5. inventor_gender 
			6. nber 
			7. patent 
			8. patent_assignee
			9. patent_govintorg 
			10.patent_inventor 
			11.rawassignee
			12. usapplicationcitation
			13. uspatentcitation
			14. wipo
			15. wipo_field

			Save these in the 'data_to_read' folder under the '2_Data_Viz_Generate' folder. 

	b. Go to the folder '1_R_Data_Prep'. Open the scripts from this folder in R/RStudio. 
	
	c. First look at requirements.R. Make sure you change the **input_folder** and **output_folder** variables to match the folder paths where you stored the bulk download files from part a above (Example folder path: _"yourpath/government-interest/2_Data_Viz_Generate/data_to_read/"_). 

	d. Run the script "assignees_looked_up_types.R" first. Then run through the remaining R scripts in numerical order. When running scripts, make sure your working directory matches your current directory (Example folder path: *"<Your-Path-Here>/government-interest/1_R_Data_Prep/"*)

		Note: Some of the scripts will take time to run since several bulk download tables are large. Here are estimated running times:
			* assignees_looked_up_types.R (~ 17 minutes)
			* 0_intermediate_patcit.R (~ 1 hour, 15 minutes)
			* 1_generate_citation_counts (~ 4 hours)
			* 2_create_core_tables (~ 1 hour, 10 minutes)
			* 3_create_assignee_table (~ 6 minutes)
			* 4_inventor_gender (~ 3 minutes)
			* 5_create_5yr_citation_1thru5 (~ 1 hour)
	
	e. All the temporary tables generated from running the scripts in '1_R_Data_Prep' will be saved to the output folder path you specified ("2_Data_Viz_Generate/data_to_read/" folder).
	These tables include the following:
		
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


	f. *Additional files*: Go to the following webpage: https://www.aaas.org/programs/r-d-budget-and-policy/historical-trends-federal-rd. Download the Excel file for "Total R&D by Agency, 1976-2018" under the _By Agency_ section. 

		1. Open this file in Excel and transpose the data table. To do this, copy the data table (only upto the "Total R&D" section (ignore the R&D: Defense and Nondefense section)) and use the paste special option _transpose the table_. 

		2. Save the transposed table as a csv file with the name "agencies.csv" in the same '2_Data_Viz_Generate/data_to_read/' folder as all the temporary tables.

**Step 2: Data Visualizations**
	
	a. Go to the folder '2_Data_Viz_Generate'. Open the scripts from this folder in R/RStudio. Make sure you change your working directory to match your current directory (Example folder path: *"<Your-Path-Here>/government-interest/2_Data_Viz_Generate/"*)

	b. Run through the script "requirements.R".

	Note: To use extrafonts in R, you will need to install ghostscript. Follow the instructions here: https://www.ghostscript.com/doc/current/Install.htm in order to install ghostscript. Then, change the Sys.setenv(__R_GSCMD__ = "") line in "requirements.R" to the path that matches where the Ghostscript executable is on your computer.

	Note: To save the sankey visualization, you will need to install orca. If you run into errors, follow the instructions at: https://github.com/plotly/orca#installation to install orca. Then, change the Sys.setenv(__ORCA_CMD__ = "") line in "requirements.R" to the path that matches where the orca executable is on your computer.

	c. Next, run the script "govIntBrief.R". This will generate all visualizations and runs for ~ 41 minutes.

		This script will generate two folders:
		(1) Folder **'data_viz/'**: a folder to store all of the viz that will be generated from running this R script
		(2) Folder **'out/'**: a folder to store all of the tables that will be generated from running this R script
	

	
