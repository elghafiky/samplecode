## IMPORTING RELEVANT LIBRARIES
import requests
import urllib.parse
import geopy
from geopy import geocoders

## CREATE THE GEOCODER PROGRAM
key = 

g = geocoders.GoogleV3(api_key = key)

mount1 = 'Mount Merapi'
mount2 = 'Mount Kelud'
mount3 = 'Mount Galunggung'
mount4 = 'Mount Salak'
mount5 = 'Mount Raung'

location1 = g.geocode(mount1, timeout = 10)
location2 = g.geocode(mount2, timeout = 10)
location3 = g.geocode(mount3, timeout = 10)
location4 = g.geocode(mount4, timeout = 10)
location5 = g.geocode(mount5, timeout = 10)

## TESTING IF THE KEY WORKS
print("Merapi:",location1.latitude, location1.longitude)
print("Kelud:",location2.latitude, location2.longitude)
print("Galunggung:",location3.latitude, location3.longitude)
print("Salak:",location4.latitude, location4.longitude)
print("Raung:",location5.latitude, location5.longitude)

## IMPORT DATA
import pandas as pd

kecamatan_names = '/content/kecamatan_names.xlsx'

table = pd.read_excel(kecamatan_names)

table.head()

table["longitude"] = ""
table["latitude"] = ""
table.head()

for index, row in table.iterrows():
  try:
    kec_iterate = table['kec_full'][index]
    geocode_res = g.geocode(kec_iterate)

    longitude_kec = geocode_res.longitude
    latitude_kec = geocode_res.latitude
  except:
    longitude_kec = "MISSING"
    latitude_kec = "MISSING"

  table["longitude"][index] = longitude_kec
  table["latitude"][index] = latitude_kec
  print(kec_iterate)
  print(longitude_kec, latitude_kec)

table.to_excel("kecamatan_names_geocode.xlsx")
table.head()