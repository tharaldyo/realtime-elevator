echo
echo "---------------------------------------------------------"
echo " ERLANG COMPILER OUTPUT"
echo "---------------------------------------------------------"
erlc *.erl;
mkdir ebin/ 2> /dev/null;
mv *.beam ebin/;
make -C driver / > /dev/null;
cd ebin/;
rm order_table;
echo "---------------------------------------------------------"
echo
erl -eval "elevator:start()";
pkill elev_port;
cd ..;
