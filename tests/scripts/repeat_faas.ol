faa_names = glob_files "examples/sequences/Mycoplasma_*.faa"
use5 = sample 5 faa_names
use5manytimes = repeat use5 faa_names 100
result = all use5manytimes