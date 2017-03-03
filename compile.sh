erlc *.erl;
mkdir ebin/ 2> /dev/null;
mv *.beam ebin/;
make -C driver /;
cd ebin/;
rm order_table;
erl -eval "elevator:start()";
pkill elev_port;
cd ..;
