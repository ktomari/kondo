# This script creates some dummy files to test the code the reads file systems. It uses a selection from the iris dataset from base R.

data("iris")

dat1 <- iris[1, 1, drop = FALSE]
dat2 <- iris[2, 1, drop = FALSE]

if(!dir.exists("inst/extdata/test_iris")){
  dir.create("inst/")
  dir.create("inst/extdata/")
  dir.create("inst/extdata/test_iris")
  dir.create("inst/extdata/test_iris/dupe")
}

# create file 1, using dat1
write.csv(dat1, "inst/extdata/test_iris/iris5.1.csv")

# create copy of file 1
write.csv(dat1, "inst/extdata/test_iris/iris5.1_copy.csv")

# create file 2, using dat2
write.csv(dat2, "inst/extdata/test_iris/iris4.9.csv")

# create name duplicate of file 2, using dat2
write.csv(dat2, "inst/extdata/test_iris/dupe/iris4.9.csv")
