mirror:
	ros -l dist.lisp -e "(mirror)"
mirror-upload:
	ros -l dist.lisp -e "(mirror)" -e "(upload)"
clean:
	ros -l dist.lisp -e "(clean)"
.PHONY: clean mirror mirror-upload
