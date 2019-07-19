# Test for CiaoPP

**Note**: This testsuite is not complete, please, contact us for other CiaoPP
features.

## Requirements

[Ciao](https://github.com/ciao-lang/ciao) (installed from git repository with `./ciao-boot.sh local-install`)
[CiaoPP](https://github.com/ciao-lang/ciaopp), installed with:
```sh
$ ciao get ciaopp
```

All code will be downloaded and built under the first directory
specified in the `CIAOPATH` environment variable or `~/.ciao` by
default.

## Download
You can automatically fetch the tests with:
```sh
ciao get ciaopp_tests
```
or clone this repository under your `CIAOPATH` directory.

## Available tests
- [Incremental analysis](tests/incanal/README.md)
