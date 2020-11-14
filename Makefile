all: app/Main.hs src/* gram
	stack build
	stack --local-bin-path . install

gram: gram/Latte.cf
	cd gram ; bnfc --make --functor Latte.cf ; happy -gca ParLatte.y ; alex -g LexLatte.x ;  cp *.hs ../src ;  rm ../src/TestLatte.hs ;

clean:
	rm -rf .stack-work .build Instant-exe

