maga = load_faa "examples/sequences/Mycoplasma_agalactiae_small.faa"
queries = split_faa (load_faa "examples/sequences/Mycoplasma_genitalium_M2321_5genes.faa")
pssms = psiblast_train_pssms 0.1 queries maga
result = pssms
