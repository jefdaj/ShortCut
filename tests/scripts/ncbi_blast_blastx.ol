mbov = gbk_to_fna "cds" (load_gbk "examples/sequences/Mycoplasma_bovis_HB0801-P115.gbk")
mgen = gbk_to_faa "cds" (load_gbk "examples/sequences/Mycoplasma_genitalium_M2321.gbk")
single = extract_queries (blastx 1.0e-5 mbov mgen)
mapped = extract_queries_each (blastx_each 1.0e-5 mbov [mgen])
result = [single] | mapped
