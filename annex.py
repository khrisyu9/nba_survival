#pip install selenium
#also remember to download the chromedriver before running the code
from selenium import webdriver  # used to automate the web interactions
import time  # used for sleeping
import os  # renaming and OS filesystem interactions

# default name of the download file, remember to check the default name each year
default_fname = 'Annual greenhouse gas (GHG) emissions for '
# full path of the download file
# make sure to use "/" in the path to avoid normal string
default_download = "C:/Users/shiying.wang/Downloads"
# base output directory
outdir = "C:/Users/shiying.wang/OneDrive - World Resources Institute/Desktop/21UNFCCC/annex"

# mapping for filters to their meaning
source_map = {
	0: 'totals',
	1: 'no_LULUCF',
	2: 'with_LULUCF',
	3: 'energy',
	4: 'fuel_combustion'
}


# list of GHG for Annex 1 countries, remember to check the sequence every year
ghg_map_annex1 = [
	'aggregate_GHG',
	'CO2',
	'CH4',
	'N2O',
	'HFCs',
	'PFCs',
	'unspecified_mix_of_HFCs_and_PFCs',
	'SF6',
	'NF3',
	'aggregate_F-gases'
]

# [selection index, ghg unit] for Annex 1, be careful with the unit of CO2
ghg2unit_annex1 = [
    [2, 'Mt_CO2',' equivalent'],#GHG
    [2, 'Mt',''], #CO2
    [5, 'Mt_CO2',' equivalent'], #CH4
    [5, 'Mt_CO2',' equivalent'], #N2O
    [2, 'Mt_CO2',' equivalent'], #HFCs
    [2, 'Mt_CO2',' equivalent'], #PFCs
    [2, 'Mt_CO2',' equivalent'], #unspecified mix
    [5, 'Mt_CO2',' equivalent'], #SF6
    [5, 'Mt_CO2',' equivalent'], #NF3
    [2, 'Mt_CO2',' equivalent'] #Fgases
]


# start the selenium application
chrome = webdriver.Chrome(executable_path="C:/Users/shiying.wang/OneDrive - World Resources Institute/Desktop/chromedriver.exe")
chrome.get('http://di.unfccc.int/detailed_data_by_party')

# get the first filter (geography)
geography_filter = chrome.find_element_by_class_name('party-data-filter')

# get all valid options for geographies to filter by
# remember to check the row number of the first nonannex country (remember the "0"-"select the party")
annex1_geographies = geography_filter.find_elements_by_tag_name('option')[4:49]  ## 4:49

# nested loops to iterate across all filtering options
for geography_element in annex1_geographies:  # go through every geography
    print (geography_element.text)
    fname_out = geography_element.text + '.csv'
    # prepare the output path
    geography_path = os.path.join(outdir, geography_element.text)
    if not os.path.exists(geography_path):
        os.mkdir(geography_path)
    # set the geography
    geography_element.click()
    # get the filters
    filters = chrome.find_elements_by_class_name('party-data-filter')[1:]
    year_filter, source_filter, ghg_filter, units_filter = filters

    for ghg_index in range(len(ghg_map_annex1)):  # use all possible gases reported
        # set the ghg reported
        ghg_element = ghg_filter.find_elements_by_tag_name('option')[ghg_index]
        ghg_element.click()
        # prepare output path
        ghg_path = os.path.join(geography_path, ghg_map_annex1[ghg_index])
        if not os.path.exists(ghg_path):
            os.mkdir(ghg_path)

        #for source_index in range(len(source_map)):
            # prepare output path
            source_path = os.path.join(ghg_path, source_map[2])
            if not os.path.exists(source_path):
                os.mkdir(source_path)

            # set the source or reporting conditions (2-total including LULUCF )
            source_filter.find_elements_by_tag_name('option')[2].click()
            # ensure "all years"
            year_filter.find_elements_by_tag_name('option')[-1].click()

            # get correct units (refer to the code line 39)
            unit_index, unit, name_add = ghg2unit_annex1[ghg_index]
            units_filter.find_elements_by_tag_name('option')[unit_index].click()

            # sleep to load new buttons  & data after filtering, (can be adjusted, better be larger than "3")
            time.sleep(5)

            # try: except block to catch cases where the button changes between
            # selecting the element and clicking it.
            try:
                # initiate download
                export_button = chrome.find_element_by_class_name('buttons-csv')
                export_button.click()
                time.sleep(4)
                os.rename(os.path.join(default_download,default_fname+geography_element.text+', in '+ unit.replace('_',' ').translate(str.maketrans("0123456789", "₀₁₂₃₄₅₆₇₈₉"))+name_add+'.csv'), os.path.join(source_path, fname_out))
            except:
                continue
