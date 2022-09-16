#!/bin/bash
set -e

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
    cat > /tmp/front_matter <<-EOF
---
title: ${J//_/ }
sidebar_position: $INDEX
---

EOF
    cat /tmp/front_matter /tmp/mdx note.mdx > $OUTPUT/$J.mdx
    INDEX=$((INDEX+1))
done

# Create front matter for the index page
cat > /tmp/front_matter <<-EOF
---
title: Ada Quality and Style Guide
description: Guidelines for Professional Programmers
sidebar_position: 0
---

> __*Guidelines for Professional Programmers*__

EOF

sed -e '/<noinclude>/,/<.noinclude>/d' data/Ada_Style_Guide.wiki |
    pandoc -f mediawiki -t gfm --filter ./bin/aqs2mdx  > /tmp/mdx

cat /tmp/front_matter /tmp/mdx note.mdx > $OUTPUT/Ada_Style_Guide.mdx
