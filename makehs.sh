#!/bin/bash

# Script to create a Haskell file with some 
# usefull Information.

COPYRIGHT="(c) Eduardo Pinhata $(date + '%Y')"
LICENSE="GPL-3"
MAINTAINER="edupinhata@gmail.com"
FILE=$1


echo "{-|" > $FILE
echo "Module        :" >> $FILE
echo "Description   :" >> $FILE
echo "Copyright     : $COPYRIGHT" >> $FILE
echo "License       : $LICENSE"   >> $FILE
echo "Maintainer    : $MAINTAINER" >> $FILE
echo "" >> $FILE
echo "" >> $FILE
echo "-}" >> $FILE
echo "" >> $FILE

echo "module Main where" >> $FILE

echo "" >> $FILE

echo "main = do" >> $FILE


