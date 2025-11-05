test:
	raco test .

format:
	raco fmt --width 80 -i **.rkt

.PHONY: test format
