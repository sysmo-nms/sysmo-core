#!/bin/sh

TMP_FILE=awk.tmp
rm -f $TMP_FILE

if [ ! -e "NOTICE.txt" ]; then
    echo -n "This script must be ran from the root directory"
    echo    "(the one containing NOTICE.txt)"
    exit 1
fi

replace_license_in_file () {
    echo "Updating license header for: $1"
    awk '\
        BEGIN { \
            position = "header"; \
            indent_left = 2; \
        } \
        { \
            if (position == "body") { \
                print; \
            } else { \
                if ($0 ~ /^%%=/) { \
                    indent_left -= 1; \
                    if (indent_left == 0) { \
                        position = "body" \
                    } \
                } \
            } \
        }' $1 > $TMP_FILE
    cat NOTICE.txt $TMP_FILE > $1
}

ALL_HRL=$(find apps -name "*\.hrl")
ALL_ERL=$(find apps -name "*\.erl")

for hrl in $ALL_HRL; do
    replace_license_in_file $hrl
done

for erl in $ALL_ERL; do
    replace_license_in_file $erl
done

rm $TMP_FILE
