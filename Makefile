index:
	ros -l dist.lisp -e "(index)"
index-upload:
	ros -l dist.lisp -e "(index)" -e "(index-upload)"
mirror:
	ros -l dist.lisp -e "(mirror)"
mirror-upload:
	ros -l dist.lisp -e "(mirror)" -e "(upload)"
all: mirror-upload index-upload

clean:
	ros -l dist.lisp -e "(clean)"
.PHONY: clean mirror mirror-upload
