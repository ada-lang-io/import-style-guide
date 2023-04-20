#!/bin/bash

# I used this script to convert a lost part of section 5 to MediaWiki

for X in `seq 564 569;echo 5610 57;seq 571 575;echo 58;seq 581 584;echo 59; seq 591 599;echo 5910 510`; do curl -O http://archive.adaic.com/ase/ase02_01/bookcase/ada_sh/style95/sec_5/$X.htm ;done
for X in *.htm; do pandoc -f html -t mediawiki $X -o `basename $X .htm`.wiki; done
sed -i -e '1,/^-----$/d' *.wiki
sed -i -e '/^-----$/,$d' *.wiki
sed -i -e 's#</blockquote>##' -e 's#<blockquote>##' *.wiki
sed -i -e 's#<span[^>]*>##' -e 's#</span>##' *.wiki
sed -i -e 's/code>/TT>/g' *.wiki
sed -i -e "s/'''\([a-z ]*\)'''/==== \1 ====/" *.wiki
sed -i -e 's/&gt;/>/g' -e 's/&lt;/</g' -e 's/&quot;/"/g' *.wiki
sed -i -e 's#<pre>#<syntaxhighlight lang=ada>\n#' -e 's#</pre>#\n</syntaxhighlight>#' *.wiki
sed -i -e "s/'''[0-9.]* \([A-Za-z ]*\)'''/=== \1 ===/" *.wiki
sed -i -e "s/=== [0-9.]* \([A-Za-z ]*\) ===/== \1 ==/" ??.wiki
sed -i -e '/toc.htm/,/^$/d' *.wiki
sed -i -e 's/\[\[[^|]*|\([^]]*\)\]\]/\1/g' *.wiki
sed -i -e '/<br \/>/{N;s/^/* /;s/^\* -/**/;s#<br />\n##}' *.wiki
sed -i -e '/^\*/{N;s/\n$//}' *.wiki

for X in `seq 564 569;echo 5610 57;seq 571 575;echo 58;seq 581 584;echo 59; seq 591 599;echo 5910 510`;do cat $X.wiki; done > /tmp/result.wiki

# fix  '=== erroneous ===', upper cases headers ??.wiki, missing list item markers, 
