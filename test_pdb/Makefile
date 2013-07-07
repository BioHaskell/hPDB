#!/usr/bin/make

STATISTICS_GATHERED:
	bash -c "time ./test_all_pdb.sh" > TOTAL_TIME && ./most_different_records.sh > STATISTICS_GATHERED

clean:
	rm -f *.rec *.log *.err *.time *.ent
