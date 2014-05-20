all:
	rm -f out
	ghc -O main.hs -XBangPatterns -o out

clean:
	rm -f out *.o *.hi

run:
	./out
