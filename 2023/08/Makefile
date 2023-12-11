test1:
	roc dev part1.roc < test.txt

test2:
	roc dev part2.roc < test3.txt

real1:
	roc build --optimize part1.roc
	./part1 < in.txt

real2:
	roc build --optimize part2.roc
	./part2 < in.txt

clean:
	rm part1
	rm part2