mirror:
	ros -l dist.lisp -e "(mirror)"
clean: 
	ros -l dist.lisp -e "(clean)"
.PHONY: clean main
