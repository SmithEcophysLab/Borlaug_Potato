library(LeafArea)

# run.ij settings
## distance.pixel = 74
## known.distance = 1

# prepare the target directory that contains example image files
dir <- "raw_images/2019_07_09/cropped_jpeg"
# ex.dir <- eximg()
list.files(dir)

areas = run.ij(set.directory = dir, save.image = TRUE, check.image = FALSE, 
               distance.pixel = 74, known.distance = 1, low.circ = 0, low.size = 0, upper.circ = 1,
               trim.pixel = 0)
nrow(areas)
head(areas)

# write.csv(areas, 'areas_2019_07_09.csv')
hist(areas$total.leaf.area)
