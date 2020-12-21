all: src/main/Main.hs src/latte/*
	stack build
	stack --local-bin-path . install

gram: src/gram/Latte.cf
	cd src/gram ; bnfc --make --functor Latte.cf ; happy -gca ParLatte.y ; alex -g LexLatte.x ;  cp *.hs ../parser ;  rm ../parser/TestLatte.hs ../parser/SkelLatte.hs ; make distclean

clean:
	rm -rf .stack-work .build Instant-exe

