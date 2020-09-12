mgen = load_fna "examples/sequences/Mycoplasma_genitalium_M2321_5genes.fna"
maga = load_faa "examples/sequences/Mycoplasma_agalactiae_small.faa"
db = diamond_makedb maga
hits = diamond_blastx_db 0 1.0e-20 mgen db
hits2 = diamond_blastx_db_each 0 1.0e-20 mgen [db]
result = [hits] | hits2
