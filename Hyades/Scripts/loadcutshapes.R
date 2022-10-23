library(GADMTools)
library(raster)
library(rgdal)
library(sf)

#ISO 3166-1 alpha-3
CODES = function(COUNTRIE){
CountriesCode = data.frame(Countries = c("Aruba","Afghanistan","Angola","Anguilla","Åland Islands","Albania","Andorra","United Arab Emirates","Argentina",
                                         "Armenia","American Samoa","Antarctica","French Southern Territories","Antigua and Barbuda","Australia","Austria",
                                         "Azerbaijan","Burundi","Belgium","Benin","Bonaire, Sint Eustatius and Saba","Burkina Faso","Bangladesh","Bulgaria",
                                         "Bahrain","Bahamas","Bosnia and Herzegovina","Saint Barthélemy","Belarus","Belize","Bermuda","Bolivia (Plurinational State of)",
                                         "Brazil","Barbados","Brunei Darussalam","Bhutan","Bouvet Island","Botswana","Central African Republic","Canada",       
                                         "Cocos (Keeling) Islands","Switzerland","Chile","China","Côte d'Ivoire","Cameroon","Congo, Democratic Republic of the",   
                                         "Congo","Cook Islands","Colombia","Comoros","Cabo Verde","Costa Rica","Cuba","Curaçao", "Christmas Island","Cayman Islands",      
                                         "Cyprus","Czechia","Germany","Djibouti","Dominica", "Denmark","Dominican Republic", "Algeria","Ecuador","Egypt",       
                                         "Eritrea","Western Sahara","Spain","Estonia","Ethiopia","Finland","Fiji", "Falkland Islands (Malvinas)","France",       
                                         "Faroe Islands","Micronesia (Federated States of)","Gabon", "United Kingdom of Great Britain and Northern Ireland",
                                         "Georgia","Guernsey","Ghana","Gibraltar","Guinea","Guadeloupe","Gambia","Guinea-Bissau","Equatorial Guinea","Greece",       
                                         "Grenada","Greenland","Guatemala","French Guiana","Guam", "Guyana","Hong Kong", "Heard Island and McDonald Islands",   
                                         "Honduras", "Croatia","Haiti","Hungary","Indonesia","Isle of Man","India","British Indian Ocean Territory",    
                                         "Ireland", "Iran (Islamic Republic of)","Iraq", "Iceland","Israel", "Italy","Jamaica","Jersey", "Jordan",       
                                         "Japan","Kazakhstan", "Kenya","Kyrgyzstan", "Cambodia", "Kiribati", "Saint Kitts and Nevis",    
                                         "Korea, Republic of", "Kuwait", "Lao People's Democratic Republic", "Lebanon","Liberia","Libya","Saint Lucia",   
                                         "Liechtenstein","Sri Lanka","Lesotho","Lithuania","Luxembourg", "Latvia", "Macao","Saint Martin (French part)", 
                                         "Morocco","Monaco", "Moldova, Republic of", "Madagascar", "Maldives", "Mexico", "Marshall Islands","North Macedonia",   
                                         "Mali", "Malta","Myanmar","Montenegro", "Mongolia", "Northern Mariana Islands","Mozambique","Mauritania",    
                                         "Montserrat", "Martinique", "Mauritius","Malawi", "Malaysia",   "Mayotte","Namibia",  "New Caledonia",  "Niger",    
                                         "Norfolk Island", "Nigeria","Nicaragua","Niue", "Netherlands","Norway", "Nepal","Nauru","New Zealand",  "Oman",    
                                         "Pakistan", "Panama", "Pitcairn","Peru","Philippines","Palau","Papua New Guinea", "Poland", "Puerto Rico",   
                                         "Korea (Democratic People's Republic of)","Portugal","Paraguay","Palestine, State of","French Polynesia", "Qatar",
                                         "Réunion", "Romania", "Russian Federation", "Rwanda", "Saudi Arabia", "Sudan", "Senegal", "Singapore",      
                                         "South Georgia and the South Sandwich Islands", "Saint Helena, Ascension and Tristan da Cunha",
                                         "Svalbard and Jan Mayen","Solomon Islands", "Sierra Leone", "El Salvador", "San Marino","Somalia",      
                                         "Saint Pierre and Miquelon","Serbia", "South Sudan", "Sao Tome and Principe","Suriname","Slovakia","Slovenia",      
                                         "Sweden", "Eswatini","Sint Maarten (Dutch part)", "Seychelles", "Syrian Arab Republic", "Turks and Caicos Islands",   
                                         "Chad","Togo","Thailand", "Tajikistan","Tokelau", "Turkmenistan", "Timor-Leste","Tonga ", "Trinidad and Tobago",    
                                         "Tunisia", "Turkey","Tuvalu", "Taiwan, Province of China",  "Tanzania, United Republic of", "Uganda",      
                                         "Ukraine", "United States Minor Outlying Islands", "Uruguay", "United States of America", "Uzbekistan","Holy See",     
                                         "Saint Vincent and the Grenadines", "Venezuela (Bolivarian Republic of)", "Virgin Islands (British)",    
                                         "Virgin Islands (U.S.)","Viet Nam","Vanuatu", "Wallis and Futuna", "Samoa","Yemen","South Africa","Zambia","Zimbabwe"), 
                           Codes = c("ABW","AFG","AGO","AIA","ALA","ALB","AND","ARE","ARG","ARM","ASM","ATA","ATF","ATG","AUS","AUT","AZE","BDI","BEL",
                                     "BEN","BES","BFA","BGD","BGR","BHR","BHS","BIH","BLM","BLR","BLZ","BMU","BOL","BRA","BRB","BRN","BTN","BVT","BWA",
                                     "CAF","CAN","CCK","CHE","CHL","CHN","CIV","CMR","COD","COG","COK","COL","COM","CPV","CRI","CUB","CUW","CXR","CYM",
                                     "CYP","CZE","DEU","DJI","DMA","DNK","DOM","DZA","ECU","EGY","ERI","ESH","ESP","EST","ETH","FIN","FJI","FLK","FRA",
                                     "FRO","FSM","GAB","GBR","GEO","GGY","GHA","GIB","GIN","GLP","GMB","GNB","GNQ","GRC","GRD","GRL","GTM","GUF","GUM",
                                     "GUY","HKG", "HMD", "HND", "HRV", "HTI", "HUN", "IDN","IMN","IND","IOT","IRL","IRN","IRQ","ISL","ISR","ITA","JAM",
                                     "JEY","JOR","JPN","KAZ","KEN","KGZ","KHM","KIR","KNA","KOR","KWT","LAO","LBN","LBR","LBY","LCA","LIE","LKA","LSO",
                                     "LTU","LUX","LVA","MAC","MAF","MAR","MCO","MDA","MDG","MDV","MEX","MHL","MKD","MLI", "MLT", "MMR", "MNE", "MNG","MNP",
                                     "MOZ","MRT","MSR","MTQ","MUS","MWI","MYS","MYT","NAM","NCL","NER","NFK","NGA","NIC","NIU","NLD","NOR","NPL","NRU",
                                     "NZL","OMN","PAK","PAN","PCN","PER","PHL","PLW","PNG","POL","PRI","PRK","PRT","PRY","PSE",
                                     "PYF","QAT","REU","ROU","RUS","RWA","SAU","SDN","SEN","SGP","SGS","SHN","SJM","SLB","SLE","SLV","SMR","SOM","SPM",
                                     "SRB","SSD","STP","SUR","SVK","SVN","SWE","SWZ","SXM","SYC","SYR","TCA","TCD","TGO","THA","TJK","TKL","TKM","TLS",
                                     "TON","TTO","TUN","TUR","TUV","TWN","TZA","UGA","UKR","UMI","URY","USA","UZB","VAT","VCT","VEN","VGB","VIR","VNM",
                                     "VUT","WLF","WSM","YEM","ZAF","ZMB", "ZWE"))
return(CountriesCode)
}

FirstMap = function(CodC, LevelC, Ubicacion){
  #ISO3166-1
  
  mapOne = GADMTools::gadm_sp_loadCountries(CodC, level=LevelC, basefile = Ubicacion)
  GADMTools::gadm_plot(mapOne)
}


DataFirstMap = function(CodC, LevelC){
  mapOne = GADMTools::gadm_sp_loadCountries(CodC, level=LevelC, basefile = "./")
  if(LevelC == 0){
    df0= as.data.frame(mapOne[["spdf"]]@data[["GID_0"]])
    df1 = as.data.frame(mapOne[["spdf"]]@data[["NAME_0"]])
    dfT = cbind(df0, df1)
    colnames(dfT) = c("Code", "Level_0")
  }
  else if(LevelC == 1){
    df0= as.data.frame(mapOne[["spdf"]]@data[["GID_0"]])
    df1 = as.data.frame(mapOne[["spdf"]]@data[["NAME_0"]])
    df2 = as.data.frame(mapOne[["spdf"]]@data[["NAME_1"]])
    dfT = cbind(df0, df1,df2)
    colnames(dfT) = c("Code", "Level 0", "Level 1")
  }
  else {
    df0= as.data.frame(mapOne[["spdf"]]@data[["GID_0"]])
    df1 = as.data.frame(mapOne[["spdf"]]@data[["NAME_0"]])
    df2 = as.data.frame(mapOne[["spdf"]]@data[["NAME_1"]])
    df3 = as.data.frame(mapOne[["spdf"]]@data[["NAME_2"]])
    dfT = cbind(df0, df1,df2,df3)
    colnames(dfT) = c("Code", "Level_0", "Level_1", "Level_2")
  }
  return(dfT)
}

SecondMap = function(CodC, LevelC, LevelD ,RegionD){
  #CodD = FisrtMap()
  #ISO3166-1
  mapOne = GADMTools::gadm_sp_loadCountries(CodC, level=LevelC, basefile = "./")
  mapTwo=  GADMTools::gadm_subset(mapOne, level= LevelD, regions=paste(RegionD))
  GADMTools::gadm_plot(mapTwo)  %>% GADMTools::gadm_showNorth("tl") %>% GADMTools::gadm_showScale('bl')
}

SaveMaps = function(CodC,LevelC,SaveDir){
  X = GADMTools::gadm_sp_loadCountries(CodC, level=LevelC, basefile = "./")
  GADMTools::saveAs(X, name="figura", directory = SaveDir)
}
