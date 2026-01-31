#!/bin/bash

set -e
ulimit -s unlimited # To fix stack overflow on a large JSON input

OUTPUT=${1:-/tmp/src/ada-lang-io/docs/style-guide}
AQS2MDX=./bin/aqs2mdx

mkdir -p $OUTPUT

curl -o data/Ada_Style_Guide.wiki "https://en.wikibooks.org/w/index.php?title=Ada_Style_Guide&action=raw"

CHAPTERS=`grep -F '* [[' data/Ada_Style_Guide.wiki | sed -e 's#.*/\([^|]*\)|.*#\1#' -e 's/ /_/g'`
INDEX=1
for J in $CHAPTERS; do
    curl -o data/$J.wiki "https://en.wikibooks.org/w/index.php?title=Ada_Style_Guide/$J&action=raw"
    #  Suppress quote format to replace it latter
    #  Fix definition lists to help pandoc understand them
    sed -e 's/^:/QUOTE/' -e '/^;/s/:/\n:/' -e 's/<whatever>/\\<whatever\\>/' data/$J.wiki |
    pandoc -f mediawiki -t gfm --filter $AQS2MDX > /tmp/mdx
    # Use quote markdown
    sed -i -e 's/^QUOTE/>/' /tmp/mdx
    # Create front matter
    SECTION="${INDEX}. ${J//_/ }"
    if [[ ${INDEX} -eq 11 ]] ; then SECTION="11. Complete Example" ; fi
    if [[ ${INDEX} -gt 11 ]] ; then SECTION="${J//_/ }" ; fi
    cat > /tmp/front_matter <<-EOF
---
title: ${SECTION}
sidebar_position: ${INDEX}
---

EOF
    cat /tmp/front_matter /tmp/mdx note.mdx > $OUTPUT/$J.mdx
    if [[ ${INDEX} -ge 3 ]] && [[ ${INDEX} -le 10 ]] ; then
       mkdir $OUTPUT/s${INDEX}
       mv $OUTPUT/$J.mdx $OUTPUT/s${INDEX}/$J
       cd $OUTPUT/s${INDEX}
       csplit -s -f "" $J '/^## /' '{*}'
       rm $J
       for X in * ; do
          mv $X $X.mdx;
          if [[ $X != 00 ]] ; then
             #  Turn subsection header into front matter
             sed -i -e '/^## /i---' -e '/^## /a---' \
                 -e "/^## /s/^##/title: ${INDEX}.${X#0}/" $X.mdx
          fi
       done
       # Rename top section file to match the folder name
       mv 00.mdx s${INDEX}.mdx
       cd - > /dev/null
    fi
    INDEX=$((INDEX+1))
done

# Fix refs to ARM and avoid <> around URLs
sed -i -e 's#../arm/AA#../../arm/AA#g' \
  -e '/<http/s/[<>]//g' $OUTPUT/s*/*

# Create front matter for the index page
cat > /tmp/front_matter <<-EOF
---
title: Ada Quality and Style Guide
description: Guidelines for Professional Programmers
sidebar_position: 0
---

> **_Guidelines for Professional Programmers_**

EOF

sed -e '/<noinclude>/,/<.noinclude>/d' data/Ada_Style_Guide.wiki |
    pandoc -f mediawiki -t gfm --filter ./bin/aqs2mdx  > /tmp/mdx

cat /tmp/front_matter /tmp/mdx note.mdx > $OUTPUT/Ada_Style_Guide.mdx

echo From ada-lang-io repo-folder run:
echo yarn prettier --write $OUTPUT
