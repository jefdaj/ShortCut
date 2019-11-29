ShortCut: short, reproducible phylogenomic cuts
===============================================


ShortCut is a scripting language meant to simplify a very common task in
bioinformatics: making a list of candidate genes likely to be related to a
biological process of interest. These are sometimes called phylogenomic cuts.

ShortCut downloads, installs, and runs the same command line tools you would
use manually but hides most of the details. That lets you quickly perform
searches too complex for a website without getting so far into the weeds that
you lose days programming. It should also be suitable for users with minimal to
no prior coding experience, and will be reproducable by other researchers
later.

See [the demo site][1] for a more detailed overview, tutorial, interactive
examples, and a list of available functions.

Development Status
------------------

These are the only important tests if you want to try the current release:

[![Master branch](http://img.shields.io/travis/jefdaj/shortcut/master.svg?style=flat&label=master)](https://travis-ci.org/jefdaj/shortcut?label=master)
![Demo Site](https://img.shields.io/website?label=demo%20site&url=http%3A%2F%2Fshortcut.pmb.berkeley.edu)
<!-- ![Env label](http://badges.herokuapp.com/travis/jefdaj/shortcut?env=BADGE=osx&label=osx&branch=feature-travisbadges) -->

But you may also be interested in progress on an upcoming feature.
Green checked boxes have been done, grey ones are in progress, and blank ones are pending.
Test badges mean tests are passing on both Mac and Linux VMs. See [Travis CI](https://travis-ci.com/jefdaj/shortcut/branches) for details.

These are changes to the interpreter:

| branch           |  code | tests                                                                                     | demo |  docs |
|------------------|-------|-------------------------------------------------------------------------------------------|------|-------|
| feature-cachix | :heavy_check_mark: | ![feature-cachix](https://img.shields.io/travis/jefdaj/shortcut/feature-cachix?label=) |  |  |
| feature-logging | :heavy_check_mark: | ![feature-logging](https://img.shields.io/travis/jefdaj/shortcut/feature-logging?label=) |  |  |
| feature-progressbar | :heavy_check_mark: | ![feature-progressbar](https://img.shields.io/travis/jefdaj/shortcut/feature-progressbar?label=) |  |  |
| feature-rerun-tests | :heavy_check_mark: | ![feature-rerun-tests](https://img.shields.io/travis/jefdaj/shortcut/feature-rerun-tests?label=) |  |  |
| feature-singularity | :heavy_check_mark: | ![feature-singularity](https://img.shields.io/travis/jefdaj/shortcut/feature-singularity?label=) |  |  |

And these are "modules" related to a specific language feature or bioinformatics program:

| branch           |  code | tests                                                                                     | demo |  docs |
|------------------|-------|-------------------------------------------------------------------------------------------|------|-------|
| module-allvsall | :heavy_check_mark: | ![module-allvsall](https://img.shields.io/travis/jefdaj/shortcut/module-allvsall?label=) |  |  |
| module-biomartr | :heavy_check_mark: | ![module-biomartr](https://img.shields.io/travis/jefdaj/shortcut/module-biomartr?label=) |  |  |
| module-blast | :heavy_check_mark: | ![module-blast](https://img.shields.io/travis/jefdaj/shortcut/module-blast?label=) |  |  |
| module-blastdb | :heavy_check_mark: | ![module-blastdb](https://img.shields.io/travis/jefdaj/shortcut/module-blastdb?label=) |  |  |
| module-blasthits | :heavy_check_mark: | ![module-blasthits](https://img.shields.io/travis/jefdaj/shortcut/module-blasthits?label=) |  |  |
| module-blastrbh | :heavy_check_mark: | ![module-blastrbh](https://img.shields.io/travis/jefdaj/shortcut/module-blastrbh?label=) |  |  |
| module-busco | :heavy_check_mark: | ![module-busco](https://img.shields.io/travis/jefdaj/shortcut/module-busco?label=) |  |  |
| module-cheat | :heavy_check_mark: | ![module-cheat](https://img.shields.io/travis/jefdaj/shortcut/module-cheat?label=) |  |  |
| module-crbblast | :heavy_check_mark: | ![module-crbblast](https://img.shields.io/travis/jefdaj/shortcut/module-crbblast?label=) |  |  |
| module-diamond | :heavy_check_mark: | ![module-diamond](https://img.shields.io/travis/jefdaj/shortcut/module-diamond?label=) |  |  |
| module-greencut | :heavy_check_mark: | ![module-greencut](https://img.shields.io/travis/jefdaj/shortcut/module-greencut?label=) |  |  |
| module-hmmer | :heavy_check_mark: | ![module-hmmer](https://img.shields.io/travis/jefdaj/shortcut/module-hmmer?label=) |  |  |
| module-listlike | :heavy_check_mark: | ![module-listlike](https://img.shields.io/travis/jefdaj/shortcut/module-listlike?label=) |  |  |
| module-load | :heavy_check_mark: | ![module-load](https://img.shields.io/travis/jefdaj/shortcut/module-load?label=) |  |  |
| module-math | :heavy_check_mark: | ![module-math](https://img.shields.io/travis/jefdaj/shortcut/module-math?label=) |  |  |
| module-mmseqs | :heavy_check_mark: | ![module-mmseqs](https://img.shields.io/travis/jefdaj/shortcut/module-mmseqs?label=) |  |  |
| module-muscle | :heavy_check_mark: | ![module-muscle](https://img.shields.io/travis/jefdaj/shortcut/module-muscle?label=) |  |  |
| module-orthofinder | :heavy_check_mark: | ![module-orthofinder](https://img.shields.io/travis/jefdaj/shortcut/module-orthofinder?label=) |  |  |
| module-orthogroups | :heavy_check_mark: | ![module-orthogroups](https://img.shields.io/travis/jefdaj/shortcut/module-orthogroups?label=) |  |  |
| module-permute | :heavy_check_mark: | ![module-permute](https://img.shields.io/travis/jefdaj/shortcut/module-permute?label=) |  |  |
| module-plots | :heavy_check_mark: | ![module-plots](https://img.shields.io/travis/jefdaj/shortcut/module-plots?label=) |  |  |
| module-psiblast | :heavy_check_mark: | ![module-psiblast](https://img.shields.io/travis/jefdaj/shortcut/module-psiblast?label=) |  |  |
| module-range | :heavy_check_mark: | ![module-range](https://img.shields.io/travis/jefdaj/shortcut/module-range?label=) |  |  |
| module-sample | :heavy_check_mark: | ![module-sample](https://img.shields.io/travis/jefdaj/shortcut/module-sample?label=) |  |  |
| module-scores | :heavy_check_mark: | ![module-scores](https://img.shields.io/travis/jefdaj/shortcut/module-scores?label=) |  |  |
| module-seqio | :heavy_check_mark: | ![module-seqio](https://img.shields.io/travis/jefdaj/shortcut/module-seqio?label=) |  |  |
| module-sets | :heavy_check_mark: | ![module-sets](https://img.shields.io/travis/jefdaj/shortcut/module-sets?label=) |  |  |
| module-setstable | :heavy_check_mark: | ![module-setstable](https://img.shields.io/travis/jefdaj/shortcut/module-setstable?label=) |  |  |
| module-sonicparanoid | :heavy_check_mark: | ![module-sonicparanoid](https://img.shields.io/travis/jefdaj/shortcut/module-sonicparanoid?label=) |  |  |
| module-summarize | :heavy_check_mark: | ![module-summarize](https://img.shields.io/travis/jefdaj/shortcut/module-summarize?label=) |  |  |

Quick Start
-----------

This should get you going on any Linux machine, and maybe MacOS:

    # 1. Install Nix
    curl https://nixos.org/nix/install | sh
    source ~/.nix-profile/etc/profile.d/nix.sh

    # 2. Build ShortCut
    git clone https://github.com/jefdaj/shortcut.git
    cd shortcut
    nix-build -j$(nproc)
    export PATH=$PWD/result/bin:$PATH

    # 3. Run self-tests (optional)
    shortcut --test

    # 4. Try it out
    shortcut

The rest of this document gives more details about each of them.


Install Nix
-----------

ShortCut is best built using [Nix][2], which ensures that all dependencies are
exactly satisfied. Not much human work is required, but it will download and/or
build a lot of packages and store them in `/nix`.

First you need the package manager itself. See [the website][2] for
instructions, or just run this:

    curl https://nixos.org/nix/install | sh
    source ~/.nix-profile/etc/profile.d/nix.sh

Installing ShortCut without this is theoretically possible, but much harder and less reliable.
Email Jeff if you want/need to try it so he can update the README with instructions!

To remove all Nix and ShortCut files later, edit the Nix line out of your `~/.bashrc` and run:

    rm -rf /nix
    rm -rf ~/.nix*
    rm -rf ~/.shortcut


Build Shortcut and run self-tests
---------------------------------

<a href="https://asciinema.org/a/MW5oHH9jMI0gFHXUnimwt3Sap" target="_blank">
  <img src="https://asciinema.org/a/MW5oHH9jMI0gFHXUnimwt3Sap.png" width="300"/>
</a>

After you have Nix, clone this repository and run `nix-build -j$(nproc)` inside
it. It will eventually create a symlink called `result` that points to the
finished package.

<a href="https://asciinema.org/a/mS8way8pStBVJ1rWQrHMAC8wN" target="_blank">
  <img src="https://asciinema.org/a/mS8way8pStBVJ1rWQrHMAC8wN.png" width="300"/>
</a>

Before using it, run the test suite to check that everything works:

    ./result/bin/shortcut --test

You might also want to add that to your `PATH` so you can call `shortcut` anywhere.
Add this line to your `~/.bashrc`.

    export PATH=$PWD/result/bin:$PATH


Build Docker image
------------------

`nix-build docker.nix` should do it.


Build Singularity image
-----------------------

`nix-build singularity.nix` should get you most of the way there, but you
should edit that file first to include any bind dirs and mount points used by
your institution's HPC environment.

The resulting `.img` file can be run with a long command like this:

```
singularity run -B /path/to/your/mount/point:/path/to/your/mount/point shortcut.img
```

That will drop you in a shell with `shortcut` + all dependencies available.
You'll only be able to use the host filesystem through the specified bind
points. Note that your institution might automatically bind some paths. You
don't need `-B` commands for those.

If you're using this, you may also want to write a custom wrapper script that
tells ShortCut how to run system calls using your HPC scheduler (SLURM or
similar).


Try it out
----------

<a href="https://asciinema.org/a/g5GErr9NQQABK6jfVHD3oX0cU" target="_blank">
  <img src="https://asciinema.org/a/g5GErr9NQQABK6jfVHD3oX0cU.png" width="300"/>
</a>

<a href="https://asciinema.org/a/euimAp0wYpVFfhZBqFaHoYc5h" target="_blank">
  <img src="https://asciinema.org/a/euimAp0wYpVFfhZBqFaHoYc5h.png" width="300"/>
</a>

These commands will run an existing script, load an existing script in the
interpreter, and start a new script in the interpreter respectively:

* `shortcut --script your-existing.cut`
* `shortcut --script your-existing.cut --interactive`
* `shortcut`

See [usage.txt][3] for other command line options, and type `:help` in the
interpreter for a list of special `:` commands (things you can only do in the live interpreter).

Now you're ready to start writing your own scripts!
See [the demo site][1] for everything related to that.


[1]: http://shortcut.pmb.berkeley.edu
[2]: https://nixos.org/nix/
[3]: usage.txt
