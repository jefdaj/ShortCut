SonicParanoid
=============

A fast, accurate and easy to use orthology inference tool.

Description
===========

SonicParanoid is a stand-alone software for the identification of orthologous relationships among multiple species. SonicParanoid is an open source software released under the GNU GENERAL PUBLIC LICENSE, Version 3.0 (GPLv3), implemented in Python3, Cython, and C++. It works on Linux and Mac OSX. The software is designed to run using multiple processors and proved to be up to 1245X faster then InParanoid, 166X faster than Proteinortho, and 172X faster than OrthoFinder 2.0 with an accuracy comparable to that of well-established orthology inference tools.
Thanks to its speed, accuracy, and usability SonicParanoid substantially relieves the difficulties of orthology inference for biologists who need to construct and maintain their own genomic datasets.

SonicParanoid was tested on the 2011 version of a benchmark proteome dataset provided by the Quest for Orthologs (QfO) consortium (https://questfororthologs.org), and its accuracy was assessed, and compared to that of other 13 methods, using a publicly available orthology benchmarking service (http://orthology.benchmarkservice.org).<br><br>
SonicParanoid is available at http://iwasakilab.bs.s.u-tokyo.ac.jp/sonicparanoid.

Citation
===========

> Salvatore Cosentino and Wataru Iwasaki (2018) SonicParanoid: fast, accurate and easy orthology inference. Bioinformatics

> https://doi.org/10.1093/bioinformatics/bty631

Changelog
===========

1.0.14 (October 19, 2018)
 - Enhancement: a warning is show if non-protein sequences are given in input
 - Enhancement: upgraded to MMseqs2 6-f5a1c
 - Enhancement: SonicParanoid is now available through Bioconda (https://bioconda.github.io/recipes/sonicparanoid/README.html)

1.0.13 (September 18, 2018)
 - Fix: allow FASTA headers containing the '@' symbol

1.0.12 (September 7, 2018)
 - Improved accuracy
 - Added new sensitivity mode (most-sensitive)
 - Fix: internal input directory is wiped at every new run
 - Fix: available disk space calculation

1.0.11 (August 7, 2018)
 - Added new program (sonicparanoid-extract) to process output multi-species clusters
 - Added the possibility to analyse only 2 proteomes
 - Added support for Python3.7
 - Python3 versions: 3.5, 3.6, 3.7
 - Upgraded MMseqs2 (commit: a856ce, August 6, 2018)

1.0.9 (May 10, 2018)
 - First public release
 - Python3 versions: 3.4, 3.5, 3.6
