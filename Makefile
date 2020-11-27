all: src/main/Main.hs src/* gram
	stack build
	stack --local-bin-path . install

gram: gram/Latte.cf
	cd gram ; bnfc --make --functor Latte.cf ; happy -gca ParLatte.y ; alex -g LexLatte.x ;  cp *.hs ../parser/src ;  rm ../src/parser/TestLatte.hs ; make distclean

clean:
	rm -rf .stack-work .build Instant-exe

