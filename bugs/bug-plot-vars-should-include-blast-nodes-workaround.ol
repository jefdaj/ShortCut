qfaa = load_faa "examples/sequences/Mycoplasma_genitalium_M2321_5genes.faa"
sfaa1 = load_faa "examples/sequences/Mycoplasma_agalactiae_small.faa"
sfaa2 = load_faa "examples/sequences/Mycoplasma_agalactiae_small.faa"
sdbfaa = [hits1, hits2]
hits1 = blastp_db 1.0e-5 qfaa (makeblastdb_faa sfaa1)
hits2 = blastp_db 1.0e-5 qfaa (makeblastdb_faa sfaa2)
result = plot_vars "bug can be worked around by assigning vars"