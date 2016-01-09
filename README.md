# s2hl

A tool for converting Erste bank statements to Hledger Journal file

`NOTE`: This will probably on work with (csv and html) bank statements
exported from [Erste in Croatia](http://www.erstebank.hr/).

## Installation

Please see [Releases page](https://github.com/denibertovic/s2hl/releases).

## Usage

    s2hl - A converter from Erste CSV/HTML statemets to Hledger journal format.
    Requires the directory structure SOMEPATH/statements/{CURRENCY}/*.{csv,html}

    Usage: s2hl-exe (-s|--statements-dir DIR) (-o|--output-dir DIR)
                    (-c|--currency CUR) [--debug]
      Converts from Erste CSV and HTML statements to HLedger journal format.

    Available options:
      -h,--help                Show this help text
      -s,--statements-dir DIR  Absolute path to the directory hosting bank statement
                               files.
      -o,--output-dir DIR      Absolute path to directory where to write the hledger
                               journal file
      -c,--currency CUR        Currency on the statements: HRK, USD, EUR
      --debug                  Print out results to stdout but don't actually create
                               a ledger file

`NOTE:` Currently the tool should work with HRK, USD and EUR currencies. But has only
been tested with USD and HRK.

`NOTE:` When sepcifying the path to the statement files with "-s,--statements-dir DIR" not that
the directory should containt subdirectories depending on which currency you're parsing. See
"--help" for more details.

## How to contribute

Report issues on the Issue tracker: https://github.com/denibertovic/s2hl/issues

1. Clone the repo (or fork it first and then clone):

    git clone git@github.com:denibertovic/s2hl.git

2. Build:

    cd s2hl && stack build

