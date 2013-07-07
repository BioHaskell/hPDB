#!/usr/bin/ruby
# = About Bio::PDB
#
# Please refer document of Bio::PDB class.
#
# = References
#
# * ((<URL:http://www.rcsb.org/pdb/>))
# * PDB File Format Contents Guide Version 2.2 (20 December 1996)
#   ((<URL:http://www.rcsb.org/pdb/file_formats/pdb/pdbguide2.2/guide2.2_frame.html>))
#
# = *** CAUTION ***
# This is beta version. Specs shall be changed frequently.
#

require 'bio/db/pdb'

contents = File.read(ARGV[0])
pdb = Bio::PDB::new(contents)
pid, rss, size = `ps ax -o pid,rss,size | grep -E "^[[:space:]]*#{Process::pid}"`.chomp.split(/\s+/).map {|s| s.strip.to_i}

#print "Memory used is: ", Float(size)/1024, " resident set: ", Float(rss)/1024, "\n"
printf "Memory used in megabytes: %10.1f resident set: %10.1f\n", Float(size)/1024, Float(rss)/1024

# With OS gem?
#ps -o rss= -p #{Process.pid}
#print pdb
