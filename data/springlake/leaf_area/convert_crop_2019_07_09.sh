# shell script to convert tif to jpeg and crop out main area using imagemagick commands

# convert to jpeg
mkdir ./raw_images/2019_07_09/jpeg
mogrify -format jpeg -path ./raw_images/2019_07_09/jpeg ./raw_images/2019_07_09/*.TIF

# crop
# mkdir ./raw_images/2019_07_09/jpeg/cropped
# mogrify -crop 1800x1900+840+150 -path ./raw_images/2019_07_09/jpeg/cropped ./raw_images/2019_07_09/jpeg/*.jpeg