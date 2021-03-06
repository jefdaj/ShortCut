minimgen = load_fna "examples/sequences/Mycoplasma_genitalium_M2321_5genes.fna"
maga = load_faa "examples/sequences/Mycoplasma_agalactiae_small.faa"
single = crb_blast minimgen maga
mapped = crb_blast_each minimgen [maga]
result = length ([single] | mapped)
