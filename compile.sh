erlc src/*.erl;
mkdir ebin/ 2> /dev/null;
mv *.beam ebin/;
make -C driver /;
cd ebin/;
erl
cd ..
