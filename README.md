# Scripts to fetch Ada Quality and Style Guide wikibook

These scripts convert the WiKi book to ada-lang.io markdown.

## Build and run

You need `pandoc`, `curl`, `sed` installed. Build a pandoc filter with Alire:

    alr build

Then run:

    ./fetch.sh <OUTPUT_DIR>

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
