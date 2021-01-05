all: src/main/Main.hs src/latte/*
	stack_haskell build
	stack_haskell --local-bin-path . install

gram: src/gram/Latte.cf
	cd src/gram ; bnfc --make --functor Latte.cf ; happy -gca ParLatte.y ; alex -g LexLatte.x ;  cp *.hs ../parser ;  rm ../parser/TestLatte.hs ../parser/SkelLatte.hs ; make distclean

test: all
	bash scripts/test.sh

format:
	bash scripts/format

clean:
	rm -rf .stack-work .build latc_x86

