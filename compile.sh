echo
echo "---------------------------------------------------------"
echo -e "\e[1m ERLANG COMPILER OUTPUT \e[21m"
echo "---------------------------------------------------------"
erlc *.erl;
mkdir ebin/ 2> /dev/null;
mv *.beam ebin/;
make -C driver / > /dev/null;
cd ebin/;
rm order_table;
echo "---------------------------------------------------------"
echo
erl;
pkill elev_port;
cd ..;
