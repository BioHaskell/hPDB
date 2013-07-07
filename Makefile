#!/usr/bin/make

PRINTTARGET=short.pdb
SOURCES=$(wildcard *.hs)
#GHCOPTS=-O1
#GHCOPTS=-O3 -msse2 -rtsopts -DOLD_TEXT_FORMAT -DDEFINE_NFDATA_INSTANCE -DDEFINE_NFDATA_VECTOR
#GHCOPTS=-O3 -msse2 -rtsopts -DOLD_TEXT_FORMAT -DDEFINE_PARAMS_INSTANCES -DDEFINE_NFDATA_INSTANCE -DDEFINE_NFDATA_VECTOR
GHCOPTS=-msse2 -rtsopts -DOLD_TEXT_FORMAT -DDEFINE_PARAMS_INSTANCES -DDEFINE_NFDATA_INSTANCE -DDEFINE_NFDATA_VECTOR -DOLD_ZLIB -DHAVE_MMAP
#-DOLD_TEXT_FORMAT
#GHCOPTS=-O3 -msse2 -rtsopts 
#-fllvm
#-rtsopts -prof -auto-all
#GHC=ghc-cvs
#GHC=ghc-7.6.1
GHC=ghc
PROFOPTS=-prof -auto-all -caf-all -fhpc -rtsopts=all -osuf p_o -hisuf p_hi
# -fhpc - coverage data
# Then: 
# bash$ hpc report Run --exclude=Main --exclude=QC
PDBDIR=/media/backup/pdb

TAPROF=+RTS -p 
HPPROF=+RTS -i0.001 -hc -p -K100M
# To format heap profile into .ps:
# hp2ps -e8in -c %.hp
# -fforce-recomp -- forces recompilation of all modules

test: tests/PrintEvents examples/PDB2Fasta examples/Rg tests/PrintStructureObject tests/PrinterTest examples/StericClashCheck examples/SplitModels 
	time tests/PrintEvents --errors-only 1CRN.pdb  && time ./PrintEvents --errors-only 3JYV.pdb && \
	time examples/PDB2Fasta 1CRN.pdb && \
	time examples/Rg 1CRN.pdb && \
	time tests/PrintStructureObject 1CRN.pdb /dev/null && \
	time tests/PrinterTest 1CRN.pdb > /dev/null && \
	time examples/StericClashCheck 1CRN.pdb 1CRN.pdb > /dev/null
	time examples/SplitModels examples/1ntl.pdb 1ntl_model_
	time examples/Viewer 1CRN.pdb

examples/PDB2Fasta: examples/PDB2Fasta.hs
	$(GHC) --make ${GHCOPTS} $@

examples/Rg: examples/Rg.hs
	$(GHC) --make ${GHCOPTS} $@

examples/PrintStructureObject: examples/PrintStructureObject.hs
	$(GHC) --make ${GHCOPTS} $@

examples/PrinterTest: examples/PrinterTest.hs
	$(GHC) --make ${GHCOPTS} $@

examples/StericClashCheck: examples/StericClashCheck.hs
	$(GHC) --make ${GHCOPTS} $@

examples/Viewer: examples/Viewer.hs
	$(GHC) -package OpenGL --make ${GHCOPTS} $@

examples/Viewer.prof: examples/Viewer.hs
	$(GHC) -package OpenGL --make ${GHCOPTS} ${PROFOPTS} $< -o $@

%.hi %.o: %.hs
	${GHC} $<

errors: PrintEvents.hs
	./PrintEvents --errors-only errors/*

examples/Rg.prof: examples/Rg.hs
	$(GHC) --make ${GHCOPTS} Rg.hs ${PROFOPTS} 

Rg.1CRN.prof: Rg.prof 1CRN.pdb
	./Rg 1CRN.pdb ${TAPROF}
	mv Rg.prof Rg.1CRN.prof

Rg.3JYV.prof: Rg.prof 3JYV.pdb
	./Rg 3JYV.pdb ${TAPROF}
	mv Rg.prof Rg.3JYV.prof

Rg.1HTQ.prof: Rg.prof 1HTQ.pdb
	./Rg 1HTQ.pdb ${TAPROF}
	mv Rg.prof Rg.1HTQ.prof

Rg.profiles: Rg.1CRN.prof Rg.3JYV.prof Rg.1HTQ.prof

tests/PrintEvents.prof: tests/PrintEvents.hs
	$(GHC) --make ${GHCOPTS} PrintEvents.hs ${PROFOPTS} #-fforce-recomp

opt:	
	$(GHC) --make ${GHCOPTS} -O2 PrintEvents.hs #-fforce-recomp

coverage: PrintEvents.short.prof
	hpc report tests/PrintEvents

todo: PrintEvents
	./PrintEvents --unhandled short.pdb #3JYV.pdb

timings:
	@echo "GHC executable and options: ${GHC} ${GHCOPTS}" 
	bash -c "time ./PrintEvents --errors-only 1CRN.pdb > /dev/null"
	bash -c "time ./PrintEvents --errors-only 3JYV.pdb > /dev/null"
	bash -c "time ./PrintEvents --errors-only 1HTQ.pdb > /dev/null"

profiles: PrintEvents.prof PrintEvents.1CRN.prof PrintEvents.3JYV.prof PrintEvents.1HTQ.prof

PrintEvents.short.prof: tests/PrintEvents.prof short.pdb
	$@ --errors-only short.pdb ${TAPROF}
	mv PrintEvents.prof PrintEvents.short.prof

PrintEvents.1CRN.prof: tests/PrintEvents.prof 1CRN.pdb
	$@ --errors-only 1CRN.pdb ${TAPROF}
	mv PrintEvents.prof PrintEvents.1CRN.prof

PrintEvents.3JYV.prof: tests/PrintEvents.prof 3JYV.pdb
	$@ --errors-only 3JYV.pdb ${TAPROF}
	mv PrintEvents.prof PrintEvents.3JYV.prof

PrintEvents.1HTQ.prof: tests/PrintEvents.prof 1HTQ.pdb
	$@ --errors-only 1HTQ.pdb ${TAPROF}
	mv PrintEvents.prof PrintEvents.1HTQ.prof

modres.txt:
	for i in ${PDBDIR}/*/*.ent.gz; do \
	  nice -n 20 zgrep '^MODRES' $i; \
	done | uniq --skip-chars=11 --check-chars=4 | sort -k3 -t' ' | uniq --skip-chars=11 --check-chars=4 > modres.txt

print: tests/PrintEvents
	$@ --print ${PRINTTARGET} |diff -u -w - ${PRINTTARGET}

all: locs PrintEvents

# count lines of code
locs:
	haskell_count *.hs

tests/PrintEvents: $(SOURCES)
	$(GHC) --make ${GHCOPTS} PrintEvents.hs

tags TAGS: $(SOURCES)
	hasktags *.hs
# Or try echo ":ctags" | ghci PrintEvents.hs

Setup.lhs: hPDB.cabal
	cabal configure --with-compiler=/usr/bin/ghc --with-hc-pkg=/usr/bin/ghc-pkg

doc: Setup.lhs
	cabal hscolour
	cabal haddock --hyperlink-source 

stat:
	haskell_count *.hs

clean:
	find . -iname '*.hi'   -exec rm -f '{}' ';'
	find . -iname '*.o'    -exec rm -f '{}' ';'
	find . -iname '*.p_hi' -exec rm -f '{}' ';'
	find . -iname '*.p_o'  -exec rm -f '{}' ';'
	rm -f PrintEvents Rg          # compilation leftovers 
	rm -f *.js *.css *.html *.gif # HADDOCK leftovers
	rm -rf Setup.lhs dist         # CABAL leftovers

record_stats.txt:
	cut -c-6 1HTQ.pdb|sort|uniq -c|sort --numeric > record_stats.txt

.PHONY: clean test PrintEvents.prof
