all : testA testB testC testD testE doc tex/relatorio.pdf
src/tarefa1: src/tarefa1.hs
ghc src/tarefa1.hs
src/tarefa2: src/tarefa2.hs
ghc src/tarefa2.hs
src/tarefa3: src/tarefa3.hs
ghc src/tarefa3.hs
src/tarefa4: src/tarefa4.hs
ghc src/tarefa4.hs
src/tarefa5: src/tarefa5.hs
ghc src/tarefa5.hs
testA: src/tarefa1
cd tests; bash runtests.sh testA ../src/tarefa1
testB: src/tarefa2
cd tests; bash runtests.sh testB ../src/tarefa2
testC: src/tarefa3
cd tests; bash runtests.sh testC ../src/tarefa3
testD: src/tarefa4
cd tests; bash runtests.sh testD ../src/tarefa4
testE: src/tarefa5
cd tests; bash runtests.sh testE ../src/tarefa5
doc: docA docB docC docD docE
docA: src/tarefa1.hs
haddock -h -o doc/TA src/tarefa1.hs
docB: src/tarefa2.hs
haddock -h -o doc/TB src/tarefa2.hs
docC: src/tarefa3.hs
haddock -h -o doc/TC src/tarefa3.hs
docD: src/tarefa4.hs
haddock -h -o doc/TD src/tarefa4.hs
docE: src/tarefa5.hs
haddock -h -o doc/TE src/tarefa5.hs
tex/relatorio.pdf: tex/relatorio.tex
cd tex; pdflatex relatorio.tex; pdflatex relatorio.tex
4clean:
rm -f src/tarefa1.{hi,o} src/tarefa2.{hi,o} src/tarefa3.{hi,o} src/tarefa4.{hi,o} src/tarefa5.{hi,o}
rm -f tex/relatorio.{aux,log,out,toc,lof}
realclean: clean
rm -rf doc/TA doc/TB doc/TC doc/TD doc/TE src/tarefa1 src/tarefa2 src/tarefa3 src/tarefa4 src/tarefa5
rm -f tex/relatorio.pdf