minimgen = load_fna "examples/sequences/Mycoplasma_genitalium_M2321_5genes.fna"
maga = load_faa "examples/sequences/Mycoplasma_agalactiae_small.faa"
hits = crb_blast minimgen maga
result = extract_targets hits
