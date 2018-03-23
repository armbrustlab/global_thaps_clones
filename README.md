# global_thaps_clones

Code, scripts, notes, data and supplemental information supporting the
paper:

    "Sexual ancestors generate an obligate asexual and globally
    dispersed clone within the model diatom species _Thalassiosira
    pseudonana_," by Julie A. Koester, et al.

All files can be viewed/downloaded via the GitHub web interface.  To
download (aka "clone") the entire repository, read the following.

## Note About Large Files

The "data" subdirectory contains (_inter alia_) two large files
(full.tables.*.rda.gz, about 600 Mb each) summarizing read- and SNP-
data for the 7 T. pseudonana isolates.  These are stored using Git's
"Large File Storage" (LFS) extension <https://git-lfs.github.com>.  If
you wish to clone the repo, _excluding_ these files:
  1. If you have _not_ installed the LFS extension, just clone as
     usual -- move to a suitable directory, and issue the command:

         git clone https://github.com/armbrustlab/global_thaps_clones

  2. If you _have_ installed the LFS extension, do the following (bash
     syntax) _instead_ of the command listed above:

         GIT_LFS_SKIP_SMUDGE=1 git clone https://github.com/armbrustlab/global_thaps_clones

Either will create a subdirectory named "global\_thaps\_clones", with
a total size of about 250 Mb, i.e., a clone of all of the repo except
the two large .rda.gz files; short "pointer" files will appear in
their place.

If you _have_ installed the LFS extension, step 1 above will clone the
full repo, including the large files in "data" (plus a copy of each in
git's lfs cache -- for a total repo size of about 2.5 Gb).  If you
used either approach to exclude the large files, and later decide you
want them, either download them via the GitHub web interface, or
(after installing LFS) do:

    git lfs pull

which will replace both pointer files by the large originals.
