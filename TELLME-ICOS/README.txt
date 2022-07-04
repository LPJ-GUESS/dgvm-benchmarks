ICOS fluxdata
Version 1.0.0

ICOS fluxdata script can be used for:
- reading daily ICOS fluxdata
- writing the selected variables and values to a new .csv file
- reformatting it to match the format from the DGVMTools R package (https://github.com/MagicForrest/DGVMTools)

Currently the following fluxnet variables are supported:
- GPP
- NEE
- RECO

The input that is required is:
- the ICOS data files with data type "ETC L2 ARCHIVE", which can be download from https://data.icos-cp.eu/
- a text file for defining some input variables and their default values, which can be created by running the "create_input_file.R" script

Recommended workflow:
1. Download the ICOS ETC L2 ARCHIVE data
2. Run the create_input_file.R script
3. Run the ICOS_fluxdata.R script




