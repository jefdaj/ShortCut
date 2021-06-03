gff = load_gff "jotest/small_human.gff3"
faa = load_faa "jotest/small_human.fasta"
jof = justorthologs_format gff faa
result = jof
