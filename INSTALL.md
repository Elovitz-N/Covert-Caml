## Server Setup:
1) In order to ensure security, the server should be kept in a room that prevents unauthorized access to the server.
2) Make sure the packages Core, Async, and Zarith are installed. To do this run "opam install core", "opam install async", and "opam install zarith".
3) If this is the first time the server is being run, navigate to the root directory of the repository and run "make server". When prompted to re-generate keys, enter "y". Distribute the "public_key.txt" file to all client machines, and store it in the /client/ directory.
4) If this is not the first time the sever is being run, run the "make server" command then enter "n".
5) The server should now be running. Note that database files will be automatically generated and updated. 

## Client Setup:
1) Make sure the packages Core, Async, and Zarith are installed. To do this run "opam install core", "opam install async", and "opam install zarith".
2) Make sure that the file "public_key.txt" exists in the /client/ directory. For more detail, refer to the "Server Setup" section above.
3) Navigate to the root directory of the repository and run "make client".
4) When prompted, enter the IP address of the machine running the server program. To find the IP address of the server, refer to the "Network Setup" section below.
5) The client should now be successfully connected to the server. Follow the prompts to login or register.

## Network Setup
1) If the client and server machines are running on the same network:
- on the server machine, find the ip address by running a command like "ifconfig" (on mac) or "ipconfig" (on Windows) and look for the IP address used by the "en0" interface.
- no additional setup is necessary
2) If the client and server machines are running on different networks:
-  for clients on other networks to be able to connect to the server, the port number from the router on the server's network must be forwarded to the server. First connect to the network on which you wish to run the server. You must be able to access the router's settings for this method to work.
- log in to the router's admin panel
- navigate to "port forwarding", usually found in the "advanced settings" category 
- add a new port forwarding rule with your local IP address as the internal host, which you can find as the IPv4 line from the "ipconfig" command, and for the port put the number 8886.
- note: if after forwarding the port you still cannot connect, it is possible that your firewall is blocking the connection.
- when connecting from the client you will need to use the public IP address of the router the server is connected to, which you can find by googling "what is my IP" from the server machine, or by looking at the settings of the router.
