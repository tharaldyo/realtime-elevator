echo
echo "---------------------------------------------------------"
echo "ERLANG COMPILER OUTPUT"
echo "---------------------------------------------------------"
erlc *.erl;
mkdir ebin/ 2> /dev/null;
mv *.beam ebin/;
make -C driver / > /dev/null;
cd ebin/;
#rm local_order_table global_order_table 2> /dev/null
echo "---------------------------------------------------------"
echo
erl;
pkill elev_port;
cd ..;
