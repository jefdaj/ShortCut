g1 = load_gff "jotest/small_human.gff3"
r1 = load_fna "jotest/small_human.fasta"
f1 = justorthologs_format g1 r1

g2 = load_gff "jotest/small_pan.gff3"
r2 = load_fna "jotest/small_pan.fasta"
f2 = justorthologs_format g2 r2

r = justorthologs f1 f2
result = r
