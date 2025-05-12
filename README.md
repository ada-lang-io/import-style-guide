# Scripts to fetch Ada Quality and Style Guide wikibook

These scripts convert the WiKi book to ada-lang.io markdown.

## Requirements

You need `pandoc-3.1.3`, `curl`, `sed` installed.

## Build and run

Build a pandoc filter with Alire:

    alr build

Then run:

    ./fetch.sh <OUTPUT_DIR>

Then run `yarn format` in ada-lang.io repository to reformat output according
to Docusaurus style.

## To be fixed

* Arrange code blocks in Source Code Presentation in two columns
* Add chapter/section numbers
* Replace "see Guideline X.Y.Z{,/and X.Y.Z}", "Chapter X", "Sections X.Y" with cross links
* change definition lists? (in Portability, OOP)
* Fix Wikipedia links
* Fix examples in Reusability:
    "Compare the body for the simplest version"
* Fix ACES in References
* Fix Bibliography
