small = load_faa_glob "examples/sequences/Mycoplasma_*_refseq.faa"
ofres = orthofinder (sample 5 small)
ogs = orthogroups_containing ofres ["WP_010925290.1", "NP_975756.1", "WP_011949581.1"]
result = ogs
