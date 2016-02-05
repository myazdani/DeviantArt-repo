import csv
import operator
from my_montage_maker import *
import math
from PIL import Image

in_file = "/Users/jaja/Documents/_2014\ DeviantArt\ work/processedData/5K_sample.csv"
image_path = "/Volumes/SWS06/DeviantART\ Project/Images/Categories/"
output_path = "/home/myazdani/Documents/DeviantArt/"


sample = open(in_file, "r")
csv1 = csv.reader(sample, delimiter = ',')
rows = [r for r in csv1]

categories = ['2004.Digital Art', '2010.Digital Art', '2004.Traditional Art', '2010.Traditional Art']

for category in categories:
    print "cluster", category
    cluster_filenames = [eachLine[0] for eachLine in rows if eachLine[-1] == category]
    print len(cluster_filenames)
    if len(cluster_filenames) == 0: continue
    filedim = math.sqrt(len(cluster_filenames))
    if (filedim%int(filedim) == 0): ncols, nrows = int(filedim),int(filedim)
    else: ncols, nrows = int(filedim), int(filedim)+1
    if ncols == 0: continue
    photow,photoh = 250,250
    photo = (photow,photoh)
    margins = [0,0,0,0]
    padding = 0
    inew = make_contact_sheet(cluster_filenames,(ncols,nrows),photo,margins,padding)
    inew.save(output_path+"cluster_"+ category + ".jpg")
	
	