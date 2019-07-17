library(LeafArea)

# run.ij settings
## distance.pixel = 74
## known.distance = 1

# prepare the target directory that contains example image files
ex.dir <- "raw_images/2019_07_09/cropped_jpeg"
ex.dir <- eximg()
list.files(ex.dir)

areas = run.ij(set.directory = ex.dir, save.image = TRUE, check.image = FALSE, 
               distance.pixel = 74, known.distance = 1, low.circ = 0)

# write.csv(areas, 'areas_2019_07_09.csv')
hist(areas$total.leaf.area)
