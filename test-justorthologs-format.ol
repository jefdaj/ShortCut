gff = load_gff "jotest/small_human.gff3"
fna = load_fna "jotest/small_human.fasta"
jof = justorthologs_format gff fna
result = jof
